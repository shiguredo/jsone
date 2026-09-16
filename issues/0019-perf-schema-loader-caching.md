# スキーマローダの重複呼び出しとキャッシュ破棄をなくす

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/refactor-schema-loader-caching
- Polished: {YYYY-MM-DD}

## 目的

外部 `$ref` を含むスキーマで `schema_loader` が同じドキュメントを何度も読み込む無駄をなくす。

## 現状

- `src/jsone_schema_state.erl` の `lookup_loaded_identifier/3` は `resolve_document/2` でドキュメントを読み込んだあと索引引きに失敗すると `LoadedState` を捨てて `error` を返す。直後の `resolve_pointer/4` が同じドキュメントを再度読み込むため、`schema_loader` が 2 回呼ばれる
- `src/jsone_schema_validator.erl` の `run_subschema/3` は読み込んだドキュメントのキャッシュ（`schemas`）と `$id` の索引（`index`）を呼び出し元へ返さない。`check_any_of_1/5` / `check_one_of_1/5` / `check_not/3` / `check_contains/3` は元の状態を使い続けるため、分岐ごとに同じドキュメントを読み込み直す

## 設計方針

- `lookup_loaded_identifier/3` は索引引きに失敗した場合も読み込み済みの状態を返し、呼び出し元で再利用する
- `run_subschema/3` の成功・失敗いずれの場合も、キャッシュ（`schemas`）と索引（`index`）だけは引き継ぐ。エラーや基準 URI の復元方針は変えない
- 読み込み回数を数えるテスト（`schema_loader` の呼び出し回数を記録する fun を渡す）を追加して、重複が無いことを担保する

## 完了条件

- 同じ外部ドキュメントを参照する `anyOf` / `oneOf` / `contains` を含むスキーマで、`schema_loader` の呼び出しが 1 回になる
- 呼び出し回数を検査するテストが追加されている
- 既存の EUnit / PropEr が引き続き通る

## 解決方法

{未着手}
