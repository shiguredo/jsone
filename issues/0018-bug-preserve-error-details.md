# エラー理由の詳細が失われ原因を追えない

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-preserve-error-details
- Polished: {YYYY-MM-DD}

## 目的

スキーマの読み込み失敗や正規表現のコンパイル失敗で、原因の情報を捨ててしまい利用者が対処できない問題を修正する。

## 現状

- `src/jsone_schema_state.erl` の `load_document/2` は `catch _:_ -> {error, {?schema_not_found, DocumentURI}}` としており、`schema_loader` の実装不具合（`function_clause` など）もファイル入出力エラーも「スキーマが見つからない」と誤報される
- `src/jsone_schema_validator.erl` の `run_pattern/2` は `{error, Reason}` を返すが、呼び出し元の `check_pattern/3` は `?schema_invalid` に潰しており、正規表現のどこが不正なのかが分からない
- `src/jsone_schema.erl` の `ensure_schema/2` は `catch _Class:Reason -> {error, {parse_error, Reason}}` として例外クラスを捨てている。既存の `src/jsone.erl` の `try_decode/2` は `{Reason, Stacktrace}` を返しており方針が不揃い

## 設計方針

- `load_document/2` は `{schema_load_error, DocumentURI, {Class, Reason}}` のように原因を含む理由を返す。未登録と読み込み失敗を区別する
- 正規表現のエラーは `{schema_invalid, Reason}` のように詳細を含める
- `ensure_schema/2` は例外クラスを含める
- エラー理由の語彙を増やす場合は `src/jsone_schema.hrl` の定義と `jsone_schema_error:to_json/1` の出力を揃える

## 完了条件

- `schema_loader` が例外を投げた場合と未登録の場合で、異なるエラーが返る
- 不正な正規表現のエラーに元の理由が含まれる
- 追加した理由を検査する回帰テストがある

## 解決方法

{未着手}
