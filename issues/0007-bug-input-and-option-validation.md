# 入力とオプションの検証漏れを塞ぐ

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-option-and-input-validation
- Polished: {YYYY-MM-DD}

## 目的

`jsone_schema` の公開 API が、誤ったオプションや想定外のスキーマ表現を黙って受け入れて「検証したつもり」になる問題を修正する。コードレビューで重要と判断した項目。

## 現状

- 不明なオプションを黙って無視する。`src/jsone_schema.erl` の `validate/3` / `validate_key/3` / `add_schema/3` / `load_schemas/2` は `maps:get(..., Options, Default)` で読むだけなので、`#{max_error => 3}` のようなタイポが素通りする（実測: `{ok, 1}`）。既存の `src/jsone.erl` の `create_decoders/2` と `build_encode_options/2` は「不明なオプションが指定されていたらエラーにする」として `erlang:error(badarg, ...)` を投げており方針が不揃い
- `jsone_schema:options()` 型は `validate/3` では使わない `parser_fun` / `recursive` も許容している
- `max_errors` の値を検査していない。`src/jsone_schema_state.erl` の `new/3` が `maps:get(max_errors, Options, 1)` でそのまま受け取るため、`max_errors => 0` や `max_errors => all` が素通りする（実測）
- `jsone:decode/2` の `{keys, attempt_atom}` でデコードしたスキーマ（キーが atom）を渡すと、`src/jsone_schema_validator.erl` の `check_keywords/3` のキーワード照合がすべて空振りして常に valid になる（実測: `{ok, <<"not an integer">>}`）。エラーにもならないため気づけない

## 設計方針

- API ごとに受け付けるオプションのキーを検証し、不明なキーは `erlang:error(badarg, ...)` にする（既存 `jsone` と同方針）。`validate/3` 用と `load_schemas/2` / `add_schema/3` 用でオプション型を分ける
- `max_errors` は `pos_integer()` か `infinity` のみ受け付け、それ以外は `badarg` にする
- スキーマのキーがバイナリでない場合は schema エラーにする（黙って全キーワードを無視しない）。少なくとも README に「`{keys, attempt_atom}` でデコードしたスキーマは受け付けない」と明記する

## 完了条件

- 不明なオプション、`max_errors => 0`、atom キーのスキーマのそれぞれについて、期待する挙動と回帰テストが揃っている
- 受け付けるオプションが API ごとに型または README で説明されている

## 解決方法

{未着手}
