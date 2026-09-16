# $ref と併記した id が無視されずスキーマエラーになる

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-ref-sibling-id-ignored
- Polished: {YYYY-MM-DD}

## 目的

draft 6 core §8 の「`$ref` を持つオブジェクトの他のプロパティは無視する（MUST）」に反し、`$ref` と `id` を併記したスキーマが `wrong_draft6_id_tag` で失敗する問題を修正する。コードレビューで重要と判断した項目。

## 現状

`src/jsone_schema_validator.erl` の `check_value/3` は `maps:is_key(?ID_OLD, JsonSchema)` を `$ref` の判定より先に評価するため、`$ref` があっても `id` の存在だけで schema エラーになる。`$schema` も `validate_with_state/3` が `check_value/3` より先に評価するため、`$ref` と併記された未対応 `$schema` で参照を辿らずに失敗する。

再現（実測）:

```erlang
S = #{<<"definitions">> => #{<<"a">> => #{<<"type">> => <<"integer">>}},
      <<"$ref">> => <<"#/definitions/a">>,
      <<"id">> => <<"legacy">>},
jsone_schema:validate(S, 1).
%% {error, [#{kind => schema, error => wrong_draft6_id_tag, ...}]}
```

仕様の根拠: draft-06 core §8「An object schema with a "$ref" property MUST be interpreted as a "$ref" reference. ... All other properties in a "$ref" object MUST be ignored.」

`id` 単体を弾く挙動は draft-04 スキーマの取り違えを防ぐ意図と見られるが、同 §4.4「A JSON Schema MAY contain properties which are not schema keywords. Unknown keywords SHOULD be ignored.」からの逸脱であり、その理由がコード・README・CHANGES.md のどこにも書かれていない。

## 設計方針

- `$ref` の判定を `id` の判定より先に行い、`$ref` がある場合は他のプロパティを評価しない
- `id` 単体を弾く挙動は維持する場合、意図（draft-04 スキーマの検出）をコメントと README に明記する。不要なら無視する挙動に変える
- `$ref` と併記された `$schema` の扱いも同じ方針で揃える

## 完了条件

- `$ref` と `id` を併記したスキーマが参照先の検証結果を返す
- `id` 単体のスキーマは従来どおり schema エラーになる
- 両方の回帰テストが `test/jsone_schema_tests.erl` に追加されている

## 解決方法

{未着手}
