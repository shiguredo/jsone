# 入力とオプションの検証漏れを塞ぐ

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-option-and-input-validation
- Polished: 2026-09-18

## 目的

`jsone_schema` の公開 API が、誤ったオプションや想定外のスキーマ表現を黙って受け入れて「検証したつもり」になる問題を修正する。コードレビューで重要と判断した項目。

## 現状

- 不明なオプションを黙って無視する。`validate/3` / `validate_key/3` は `src/jsone_schema_state.erl` の `new/3` が、`add_schema/3` / `load_schemas/2` は `src/jsone_schema.erl` が `maps:get(..., Options, Default)` で読むだけなので、`#{max_error => 3}` のようなタイポが素通りする（実測: `{ok, 1}`）。既存の `src/jsone.erl` の `create_decoders/2` と `build_encode_options/2` は「不明なオプションが指定されていたらエラーにする」として `erlang:error(badarg, ...)` を投げており方針が不揃い
- `jsone_schema:options()` 型は 1 つしかなく、`validate/3` / `validate_key/3` では使わない `parser_fun` / `recursive` も、`add_schema/3` / `load_schemas/2` では使わない `max_errors` / `schema_loader` / `schemas` も許容している
- `max_errors` の値を検査していない。`src/jsone_schema_state.erl` の `new/3` が `maps:get(max_errors, Options, 1)` でそのまま受け取るため、`max_errors => 0` / `all` / `-1` が素通りする（実測: いずれも例外にならず、`0` は既定と同じくエラー 1 件、`all` は `infinity` と同じ挙動になる）
- `jsone:decode/2` の `{keys, attempt_atom}` でデコードしたスキーマ（キーが atom）を渡すと、`src/jsone_schema_validator.erl` の `check_keywords/3` はキーワードをバイナリとしか照合しないため、atom キーのキーワードは `check_keyword_value/5` の catch-all に落ちてすべて無視され、常に valid になる（実測: `{ok, <<"not an integer">>}`）。`{keys, attempt_atom}` は `binary_to_existing_atom` で既存の atom にしか変換しないため、atom キーとバイナリキーが混在したスキーマもあり得る。エラーにもならないため気づけない

## 設計方針

- 受け付けるオプションのキーを API ごとに検証し、不明なキーは `erlang:error(badarg, ...)` にする（既存 `jsone` と同方針）。グループは `validate/3` / `validate_key/3`（`max_errors` / `schema_loader` / `schemas`）、`add_schema/3`（`parser_fun`）、`load_schemas/2`（`parser_fun` / `recursive`）の 3 つに分ける。0008 が追加する `validate_format` は `validate/3` / `validate_key/3` のグループに属する
- 検証は `jsone_schema` の公開 API の入口で行い、`validate_key/3` が内部で `schema_loader` を注入する処理より前に済ませる。内部で組み立てる `#{parser_fun => ...}`（`load_schemas/2` → `add_schema/3`）は検証済みのキーだけを含むため、内部呼び出しはそのまま通る。`jsone_schema_state:new/3` は検証済みのオプションだけを受け取る前提にする
- `max_errors` は `pos_integer()` か `infinity` のみ受け付け、それ以外は `badarg` にする
- スキーマの map にバイナリでないキーが 1 つでもある場合は schema エラーにする（黙ってキーワードを無視しない）。検査は `check_value/3` でスキーマの map を評価するときに行い、到達した入れ子のサブスキーマも対象にする。したがって `definitions` の下や、インスタンスに対応するプロパティが無いために評価されない `properties` の下は対象外になる（スキーマの妥当性検査を網羅的に行うかは 0017 が扱う）。データ（インスタンス）のキーは対象外とし、`jsone:json_value()` の型は変更しない
- README には API ごとに受け付けるオプションを記載し、`{keys, attempt_atom}` でデコードしたスキーマを受け付けない（schema エラーになる）旨も明記する

## 完了条件

- 不明なオプション（例: `max_error`）が `validate/3` / `validate_key/3` / `add_schema/3` / `load_schemas/2` のそれぞれで `badarg` になる
- `max_errors => 0` / `all` / `-1` が `badarg` になる
- atom キーのスキーマが `{error, [#{kind := schema, ...}]}` を返す。評価される入れ子のサブスキーマ（インスタンスに対応するプロパティの下など）にある atom キーも検出する
- 正当なオプション（`max_errors => infinity` / `schema_loader` / `schemas` / `parser_fun` / `recursive`）が従来どおり動作する
- 上記の回帰テストが `test/jsone_schema_tests.erl` に追加されている
- README に API ごとの受け付けるオプションと、`{keys, attempt_atom}` でデコードしたスキーマを受け付けない旨が記載されている
- `test/JSON-Schema-Test-Suite/tests/draft6` の 702 ケースと `test/prop_jsone_schema.erl` の全プロパティが引き続き通る

## 解決方法

{未着手}
