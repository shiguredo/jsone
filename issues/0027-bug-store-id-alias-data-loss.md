# スキーマストアの `$id` エイリアス管理で登録が失われるのを修正する

- Created: 2026-11-14
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-store-id-alias-data-loss
- Polished: {YYYY-MM-DD}

## 目的

`jsone_schema_store` は登録キーと `$id` エイリアスを独立した `persistent_term` キーで管理しているため、再登録・削除・共有 `$id`・`load_schemas/2` の組み合わせで、無関係な登録が消えたり、古いスキーマが残ったりする。登録と削除の意味を一貫させ、データ損失をなくす。

## 現状

`src/jsone_schema_store.erl` の `add/2` は `schema_keys/2` で作ったキーを個別に `persistent_term:put/2` するだけで、以前のエイリアスを消さない。`delete/1` は現在のスキーマからキーを再計算して `erase/1` するため、canonical key と alias の対応や参照関係を持っていない。`jsone_schema:load_schemas/2` はファイルごとに `register_schemas/1` を呼び、既存キーとの衝突を検査しない。

実測した挙動:

```erlang
%% 1. alias と同じ名前の canonical key を登録すると、del_schema/1 で無関係な登録が消える
A = #{<<"$id">> => <<"X">>, <<"type">> => <<"integer">>},
B = #{<<"type">> => <<"boolean">>},
ok = jsone_schema:add_schema(<<"A">>, A),
ok = jsone_schema:add_schema(<<"X">>, B),
ok = jsone_schema:del_schema(<<"A">>),
jsone_schema:get_schema(<<"X">>).
%% => {error, not_found}（B の登録まで消える）
```

```erlang
%% 2. 同じキーを別の $id で再登録すると、古い $id が古いスキーマを返し続ける
S1 = #{<<"$id">> => <<"http://example.com/one">>, <<"type">> => <<"integer">>},
S2 = #{<<"$id">> => <<"http://example.com/two">>, <<"type">> => <<"string">>},
ok = jsone_schema:add_schema(<<"k">>, S1),
ok = jsone_schema:add_schema(<<"k">>, S2),
jsone_schema:get_schema(<<"http://example.com/one">>).
%% => {ok, S1}（古いスキーマが残る）
```

```erlang
%% 3. 共有 $id の片方を削除すると、もう片方の alias も消える
ok = jsone_schema:add_schema(<<"k1">>, S1),
ok = jsone_schema:add_schema(<<"k2">>, S1),
ok = jsone_schema:del_schema(<<"k1">>),
jsone_schema:get_schema(<<"http://example.com/one">>).
%% => {error, not_found}（k2 は残っているのに alias が消える）
```

さらに、`load_schemas/2` で読み込むファイルの `$id` が利用者の既存キーと同じ場合、その既存キーを上書きする。`list_schemas/0` は alias も独立した登録として返すため、同じスキーマが複数キーで現れる。

## 設計方針

- 第一候補は、`persistent_term` ストアと `validate_key/2,3` / `add_schema/2,3` / `load_schemas/1,2` をコア API から外すこと。未リリース機能なので、`schemas` の明示マップに寄せれば状態管理そのものを削除できる。
- ストアを残す場合は、canonical entry に `{Schema, CanonicalKey, Aliases}` を持たせ、alias は canonical key への参照として管理する。再登録時は旧 alias を削除し、削除時は他の canonical key が同じ alias を参照していない場合だけ削除する。
- `add_schema/2` の明示的な上書きと `load_schemas/2` の自動登録は別ポリシーにする。`load_schemas/2` は全キー・全エイリアスの衝突を登録前に検査し、衝突したら `{error, {File, {schema_id_conflict, URI}}}` を返す。
- `list_schemas/0` は「利用者が登録したキーだけ」を返すか、「エイリアスを含む」かを決めて README に明記する。
- `persistent_term:put/2` は更新時に全体へ影響するため、登録・削除の原子性と並行性の前提を README に書く。

## 完了条件

- 上記 1〜3 の再現で、無関係な登録が消えず、古い alias が残らず、共有 alias が他の canonical key の存在中は維持される。
- `del_schema/1` を canonical key と alias のどちらから呼んでも、同じ登録が消える。共有 alias の場合は、参照している登録がなくなるまで維持される。
- `load_schemas/2` が既存キーを `$id` 経由で上書きしない。衝突時はファイルと URI を含むエラーを返す。
- `list_schemas/0` の返り値の意味がテストと README で一致する。
- ストアを削除する方針の場合は、API 削除と README / CHANGES の更新が完了している。
- `xref` / `dialyzer` / `eunit` / `proper` が通る。

## 解決方法

{未着手}
