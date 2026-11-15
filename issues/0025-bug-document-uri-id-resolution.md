# ドキュメント URI と `$id` の基準 URI 解決を一貫させる

- Created: 2026-11-14
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-document-uri-id-resolution
- Polished: {YYYY-MM-DD}

## 目的

取得 URI、ルート `$id`、埋め込みリソースの `$id`、`validate_key/2,3` のキーが混在すると、同じ文書の別スキーマで検証されたり、参照先が別の文書に変わったりする。draft-06 core §9.1 / §9.2 と RFC 3986 に沿って、文書 URI と基準 URI の決定規則を一貫させる。

## 現状

`src/jsone_schema_state.erl` の `document_uri/2`、`resolve_pointer/4`、`lookup_resource_identifier/3`、`compose_base_uri/2` が、取得 URI・ルート `$id`・埋め込みリソース URI を個別に扱っている。このため次の誤動作がある。

### 1. 取得 URI とルート `$id` が異なる場合

`schemas` のキーが `http://example.com/retrieved.json`、返された文書のルート `$id` が `http://example.com/a/root.json` の場合、`retrieved.json#/definitions/t` から入った参照先の相対 `$ref` が取得 URI 基準で解決される。

```erlang
Doc = #{
  <<"$id">> => <<"http://example.com/a/root.json">>,
  <<"definitions">> => #{<<"t">> => #{<<"$ref">> => <<"sub.json">>}}
},
Options = #{
  schemas => #{
    <<"http://example.com/retrieved.json">> => Doc,
    <<"http://example.com/a/sub.json">> => #{<<"type">> => <<"integer">>},
    <<"http://example.com/sub.json">> => #{<<"type">> => <<"string">>}
  }
},
Outer = #{<<"$ref">> => <<"http://example.com/retrieved.json#/definitions/t">>},
jsone_schema:validate(Outer, 1, Options).
%% => {error, ...}（正しくは a/sub.json の integer に解決され {ok, 1}）
jsone_schema:validate(Outer, <<"x">>, Options).
%% => {ok, <<"x">>}（正しくは error）
```

### 2. 埋め込みリソースの相対 `$id` と空フラグメント `#`

埋め込みリソースの `$id` が相対パスの場合、`$ref: "models/user.json#"` で `$id` が二重適用され、`models/models/user.json` を探す。

```erlang
Schema = #{
  <<"$id">> => <<"http://example.com/root.json">>,
  <<"definitions">> => #{
    <<"user">> => #{
      <<"$id">> => <<"models/user.json">>,
      <<"definitions">> => #{<<"leaf">> => #{<<"type">> => <<"integer">>}},
      <<"properties">> => #{
        <<"self">> => #{<<"$ref">> => <<"#">>},
        <<"leaf">> => #{<<"$ref">> => <<"#/definitions/leaf">>}
      }
    }
  },
  <<"properties">> => #{<<"user">> => #{<<"$ref">> => <<"#/definitions/user">>}}
},
jsone_schema:validate(Schema, #{<<"user">> => #{<<"self">> => #{<<"leaf">> => 1}}}).
%% => schema_not_found: http://example.com/models/models/user.json
%%    `#` を外した参照では成功する
```

### 3. `validate_key/2,3` のキーにフラグメントがある場合

`u` に integer のスキーマ、`u#foo` に string のスキーマを登録すると、`validate_key(<<"u#foo">>, ...)` が `u` のスキーマで検証される。

```erlang
S1 = #{
  <<"definitions">> => #{<<"a">> => #{<<"type">> => <<"integer">>}},
  <<"properties">> => #{<<"v">> => #{<<"$ref">> => <<"#/definitions/a">>}}
},
S2 = S1#{<<"definitions">> := #{<<"a">> => #{<<"type">> => <<"string">>}}},
ok = jsone_schema:add_schema(<<"u">>, S1),
ok = jsone_schema:add_schema(<<"u#foo">>, S2),
jsone_schema:validate_key(<<"u#foo">>, #{<<"v">> => 1}).
%% => {ok, ...}（u の integer スキーマで検証される。正しくは string なので error）
```

### 4. 未知キーワード内の `$id` が基準 URI に適用される

`compose_base_uri/2` は JSON Pointer の訪問値のうち、任意の map の `$id` を schema とみなして base を進める。一方、`jsone_schema_index/5` は未知キーワード内の `$id` を索引しない。この不整合により、未知キーワード内の `$ref` が意図しない base で解決される。

```erlang
Schema = #{
  <<"$id">> => <<"https://example.com/root.json">>,
  <<"x">> => #{
    <<"$id">> => <<"https://evil.example/other.json">>,
    <<"y">> => #{<<"$ref">> => <<"z.json">>}
  },
  <<"properties">> => #{<<"p">> => #{<<"$ref">> => <<"#/x/y">>}}
},
Options = #{
  schemas => #{
    <<"https://example.com/z.json">> => #{<<"type">> => <<"integer">>},
    <<"https://evil.example/z.json">> => #{<<"type">> => <<"string">>}
  }
},
jsone_schema:validate(Schema, #{<<"p">> => 1}, Options).
%% => 未知キーワードの $id により evil.example/z.json（string）が使われる
```

### 5. `$id` の JSON Pointer フラグメントが JSON Pointer 解決を乗っ取る

`$id: "#/definitions/a"` を持つサブスキーマがあると、`$ref: "#/definitions/a"` が JSON Pointer の本来の先ではなく、その `$id` のスキーマに解決される。draft-06 core §5 は「JSON Pointer 構文に一致するフラグメントは JSON Pointer として解釈する」としている。

## 設計方針

- スキーマ位置ごとに「取得 URI」「ルート `$id`」「その位置の絶対 URI」「所属ドキュメント URI」をコンパイル時に確定するレジストリを導入する。
- 文書 URI は `jsone_schema_uri:split_fragment/1` でフラグメントを除去して扱う。`validate_key/2,3` のキーも同じ規則にする。
- `$ref` のフラグメントが空文字列または `/` で始まる場合は、`$id` 索引より先に JSON Pointer として解決する。
- `$id` の解決は schema 位置だけで行う。未知キーワード内の `$id` は索引にも base にも使わない。
- `$id` のフラグメント付きの複雑な形は draft-06 core §9.2 の CREF2 で未定義であるため、サポートする形を限定し、それ以外は `schema_invalid` / `schema_unsupported` として拒否する方針を決める。
- 既存の `jsone_schema_state` の `base_uri` / `document_uri` / `root_schema` / `index` の書き換え・復元を減らし、データ検証中に基準 URI を変えない構造にする。

## 完了条件

- 上記 1〜5 の再現がすべて正しい結果になる。
- 1 のケースで、`schemas` と `schema_loader` の両方が同じ結果になる。
- 2 のケースで、`models/user.json#` と `models/user.json` の両方が同じ `user` リソースに解決される。
- 3 のケースで、`u#foo` が `u` のスキーマで検証されない。`u#foo` を拒否する方針なら、`add_schema/2` と `validate_key/2` で明確なエラーになる。
- 4 のケースで、未知キーワード内の `$id` が base に影響しない。
- 5 のケースで、JSON Pointer 構文のフラグメントが優先される。`$id` の JSON Pointer フラグメントを拒否する方針なら、その旨が README に書かれている。
- `xref` / `dialyzer` / `eunit` / `proper` と公式スイート 702 件が通る。
- 回帰テストは、成功時に `{ok, _}`、失敗時に `kind => data` / `kind => schema` の区別まで検査する。

## 解決方法

{未着手}
