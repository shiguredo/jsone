# $id アンカーと埋め込みリソースを跨ぐ $ref が解決できない

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-id-anchor-ref-resolution
- Polished: {YYYY-MM-DD}

## 目的

draft 6 の `$id`（プレーンネームフラグメントによるアンカー、および基準 URI を変える埋め込みリソース）を使ったスキーマで `$ref` の解決に失敗し、正しいスキーマが schema エラーになる問題を修正する。コードレビューで重要と判断した項目。

## 現状

`src/jsone_schema_uri.erl` の `resolve_opaque/2` は `uri_string:resolve/2` が失敗したときに参照 URI を基準 URI へ単純連結する。基準 URI がフラグメント付き（例: `#a`）の場合、RFC 3986 §5.3 ではフラグメントを置換すべきところを連結するため `#a#b` のような壊れた URI になる。

また `src/jsone_schema_state.erl` の `resolve_pointer/4` と `resolve_document/2` は、基準 URI が変わった埋め込みリソースを索引から引かずに外部ドキュメントとして読み込もうとする。

実測した再現（4 パターン、いずれも `{error, [#{kind => schema, error => {schema_not_found, ...}}]}` になる）:

1. ルートに絶対 `$id` が無く、`$id: "#a"` のサブスキーマから `$ref: "#b"` → `{schema_not_found, <<"#a#b">>}`

```erlang
S = #{<<"definitions">> =>
          #{<<"A">> => #{<<"$id">> => <<"#a">>,
                         <<"properties">> => #{<<"x">> => #{<<"$ref">> => <<"#b">>}}},
            <<"B">> => #{<<"$id">> => <<"#b">>, <<"type">> => <<"integer">>}},
      <<"allOf">> => [#{<<"$ref">> => <<"#a">>}]},
jsone_schema:validate(S, #{<<"x">> => 1}).
```

2. ルートに絶対 `$id` があり、`$id: "#a"` のサブスキーマ内から `$ref: "#/definitions/B"` → `{schema_not_found, <<"http://example.com/root.json">>}`

```erlang
S = #{<<"$id">> => <<"http://example.com/root.json">>,
      <<"definitions">> =>
          #{<<"A">> => #{<<"$id">> => <<"#a">>,
                         <<"properties">> =>
                             #{<<"x">> => #{<<"$ref">> => <<"#/definitions/B">>}}},
            <<"B">> => #{<<"type">> => <<"integer">>}},
      <<"allOf">> => [#{<<"$ref">> => <<"#a">>}]},
jsone_schema:validate(S, #{<<"x">> => 1}).
```

3. bundle に埋め込んだ `"$id": "https://example.com/user.json"` のサブスキーマ内から自身の `#/definitions/name` を参照 → `{schema_not_found, <<"https://example.com/user.json">>}`

4. ルート `$id` が相対 URI (`"foo.json"`) のときの `#/definitions/a` → `{schema_not_found, <<"foo.json">>}`

仕様の根拠:

- draft-06 core §5「Fragment identifiers matching the JSON Pointer syntax, including the empty string, MUST be interpreted as JSON Pointer fragment identifiers.」
- 同 §9.2「`$id` はスキーマの URI と、そのスキーマ内の他の URI 参照を解決する基準 URI を定義する」

テストスイートの `tests/draft6/ref.json` は「アンカー 1 個」または「絶対 URI の埋め込みリソース」の組み合わせのみで、アンカー配下からの追加参照を網羅していないため 702 ケース通過では検出できない。

## 設計方針

- `resolve_opaque/2` は、基準 URI にフラグメントがある場合は置換する。不透明なストアキー（`user` など）に対するフラグメント参照という従来の用途は維持する
- `resolve_pointer/4` は、`DocumentURI` が現在の `document_uri` と一致しない場合に、まず `jsone_schema_index:lookup/2` で埋め込みリソースのエントリを探し、見つかればそのスキーマをルートとして JSON Pointer を評価する。見つからない場合のみローダにフォールバックする
- 相対 URI の `$id` を持つルートの扱い（`document_uri/2` が絶対 URI のみを採用している点）を整理する

## 完了条件

- 上記 4 パターンが期待どおりの検証結果を返す
- 4 パターンの回帰テストが追加され、`?assertMatch({error, _}, ...)` ではなく `{ok, _}` / `{error, [#{kind := data, ...}]}` まで検査している
- `tests/draft6` の 702 ケースが引き続き通る

## 解決方法

{未着手}
