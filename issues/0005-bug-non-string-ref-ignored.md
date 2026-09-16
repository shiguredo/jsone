# 文字列以外の $ref が無視されて常に検証成功になる

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-non-string-ref-ignored
- Polished: {YYYY-MM-DD}

## 目的

`{"$ref": 5}` のように URI 参照でない `$ref` を書いたスキーマが、キーワードごと無視されて「どんなデータでも成功するスキーマ」になる問題を修正する。コードレビューで重要と判断した項目。

タイポが「検証していないのに成功する」失敗モードになる。他のキーワード（`required` / `pattern` / `properties` など）の型不正を schema エラーにしている方針とも不整合。

## 現状

`src/jsone_schema_validator.erl` の `check_value/3` は `#{?REF := Reference} when is_binary(Reference)` のガードを外れると `check_keywords/3` に落ち、`check_keyword_value/5` の catch-all 節が `$ref` を無視する。

再現（実測）:

```erlang
jsone_schema:validate(#{<<"$ref">> => 5}, <<"anything">>).
%% {ok, <<"anything">>}
```

仕様の根拠:

- draft-06 core §8「The value of the "$ref" property MUST be a URI Reference.」
- 同梱のメタスキーマ `test/meta-schemas/draft-06.json` の `"$ref": {"type": "string", "format": "uri-reference"}`

テストスイートに該当ケースが無いため 702 ケース通過では検出できない。

## 設計方針

- `?REF` が存在して binary でない場合は schema エラーを返す。専用の reason を追加するか `schema_invalid` を使う
- `$ref` の値が URI 参照として妥当かどうか（スキームや相対参照の許容範囲）をどこまで検査するかを決め、コメントに明記する

## 完了条件

- `{"$ref": 5}` が `{error, [#{kind := schema, ...}]}` を返す
- 回帰テストが `test/jsone_schema_tests.erl` に追加されている
- 正常な `$ref`（文字列）の挙動が変わっていない

## 解決方法

{未着手}
