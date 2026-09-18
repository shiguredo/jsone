# 文字列以外の $ref が無視されて参照先が検証されない

- Created: 2026-09-16
- Completed: 2026-09-18
- Branch: feature/fix-non-string-ref-ignored
- Polished: 2026-09-18

## 目的

`{"$ref": 5}` のように URI 参照でない `$ref` を書いたスキーマで、`$ref` キーワードが無視される問題を修正する。`$ref` 以外にデータを検証するキーワードを持たないスキーマ（`definitions` など）は、どのデータでも成功する状態になる。`$schema` は方言ゲートとして `check_value/3` より先に評価されるため、未対応の `$schema` を併記した場合はこの限りではない。

タイポが「検証していないのに成功する」失敗モードになる。他のキーワード（`required` / `pattern` / `properties` など）の型不正を schema エラーにしている方針とも不整合。

## 現状

`src/jsone_schema_validator.erl` の `check_value/3` は `#{?REF := Reference} when is_binary(Reference)` のガードを外れると `check_keywords/3` に落ち、`check_keyword_value/5` の catch-all 節が `$ref` を無視する。

再現（実測）:

```erlang
jsone_schema:validate(#{<<"$ref">> => 5}, <<"anything">>).
%% {ok, <<"anything">>}

%% 非文字列であれば null / true / [<<"#">>] なども同じ
%% 兄弟キーワードは無視されず評価され続ける
jsone_schema:validate(#{<<"$ref">> => 5, <<"type">> => <<"string">>}, 42).
%% {error, [#{kind => data, error => wrong_type, ...}]}
```

文字列の `$ref` は、URI 参照として不正な値でも現行どおり schema エラーになる（実測: `#{<<"$ref">> => <<":::">>}` は `{error, [#{kind => schema, error => {schema_not_found, <<":::">>}}]}`）。

仕様の根拠:

- draft-06 core §8「The value of the "$ref" property MUST be a URI Reference.」
- 同梱のメタスキーマ `test/meta-schemas/draft-06.json` の `"$ref": {"type": "string", "format": "uri-reference"}`

テストスイートに該当ケースが無いため 702 ケース通過では検出できない。

## 設計方針

- 検査は `check_value/3` の分岐に足す。`?REF` が存在して binary でなければ `check_keywords/3` に落とさず schema エラーを返す。`check_keyword_value/5` に `?REF` の節を足す形にはしない。`check_keywords/3` はスキーマの全キーワードを fold するため、兄弟キーワードを評価し続けてしまう。分岐の順序は「binary な `$ref` → `id` → 非 binary な `$ref` → `check_keywords/3`」とする。0004 が入れた `id` の検出（`wrong_draft6_id_tag`）は非 binary な `$ref` の場合も優先されるため、`id` の検査を非 binary な `$ref` の検査より前に置く
- 兄弟キーワードは評価しない。draft-06 core §8 の「All other properties in a "$ref" object MUST be ignored.」に合わせ、`$ref` の有無だけで分岐する。`{"$ref": 5, "type": "string"}` は `type` を評価せず schema エラーになる
- エラー理由は `?schema_invalid` を使う。`pattern` / `properties` / `patternProperties` / `dependencies` と同じくキーワード値の型不正は `schema_invalid` に揃えており、`jsone_schema.hrl` に専用の reason マクロを増やさない
- URI 参照としての構文検査は追加しない。binary の `$ref` は現行どおり `jsone_schema_state:resolve_ref/2` に委ね、解決できなければ `schema_not_found` の schema エラーになる。`format` の `uri-reference` 検査とは目的が別であり、この issue では扱わない
- `$ref` と `id` の併記は別 issue（0004）の担当であり、この issue の完了条件には含めない。0004 が `$ref` の分岐を `is_binary/1` でガードするため、`{"$ref": 5, "id": "legacy"}` はこの issue の実装後も `id` の検出（`wrong_draft6_id_tag`）が優先される。`schema_invalid` にはならない。実装順は 0004 を先にする

## 完了条件

- `{"$ref": 5}` が `{error, [#{kind := schema, error := schema_invalid, ...}]}` を返す
- `null` / `true` / `[<<"#">>]` のような他の非文字列の `$ref` も同じ schema エラーになる
- `{"$ref": 5, "type": "string"}` に `42` を与えても data エラーではなく同じ schema エラーになる（兄弟キーワードを評価しない）
- 回帰テストが `test/jsone_schema_tests.erl` に追加されている
- 正常な `$ref`（文字列）の挙動が変わっていない。`test/JSON-Schema-Test-Suite/tests/draft6` の 702 ケースと `test/prop_jsone_schema.erl` の全プロパティが引き続き通る
- `./rebar3 xref` / `./rebar3 dialyzer` / `./rebar3 as test eunit` / `./rebar3 as test proper` が通る

## 解決方法

`check_value/3` の分岐を 4 節にし、`$ref` が文字列でない場合は URI 参照にならないため schema エラーにするようにした。

- 分岐の順序は「binary な `$ref` → `id` → 非 binary な `$ref` → `check_keywords/3`」とした。binary な `$ref` がある場合は参照先の検証に進み兄弟キーワードを評価しない。`id` がある場合は 0004 の確定どおり `wrong_draft6_id_tag` を優先する。`$ref` が文字列でなければ `schema_invalid` の schema エラーにし、兄弟キーワードは評価しない
- エラー理由は既存の `?schema_invalid` を使い、`jsone_schema.hrl` に専用の理由は増やしていない。URI 参照としての構文検査も追加していない
- `test/jsone_schema_tests.erl` に `non_string_ref_test/0` を追加した（`5` / `5.0` / `null` / `true` / `false` / 配列 / map、`$id` の併記、空文字列の `$ref`、兄弟キーワードの `type` / `required` / `enum` / `maxItems`、正常な `$ref` の維持）

既知の制限: `not` / `anyOf` / `oneOf` / `allOf` の分岐内にある schema エラーは `run_subschema/3` が分岐の失敗として扱うため、`{"not": {"$ref": 5}}` は `{ok, ...}` になる。原因は分岐内の schema エラーの扱いという既存の設計にあり、この issue の対象外として別 issue で扱う。

`./rebar3 xref` / `./rebar3 dialyzer` / `./rebar3 as test eunit` / `./rebar3 as test proper` が通り、eunit は 750 件、PropEr は 15 件すべて通過した。`test/JSON-Schema-Test-Suite/tests/draft6` の 702 ケースも引き続き通る。
