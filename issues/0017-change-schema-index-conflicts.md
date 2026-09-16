# 同一 URI の $id 重複とスキーマ妥当性検査の範囲を決める

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/change-schema-index-conflicts
- Polished: {YYYY-MM-DD}

## 目的

同一 URI を指す複数の `$id` を黙って上書きしている挙動と、スキーマの妥当性検査をどこまで行うかを決めて明文化する。

## 現状

- `src/jsone_schema_state.erl` の `merge_index/2` は `maps:merge/2` で索引を統合するため、後から読み込んだドキュメントの `$id` が既存 URI と衝突しても黙って上書きする。draft-06 core §9.2.2 は「A schema MAY (and likely will) have multiple URIs, but there is no way for a URI to identify more than one schema. When multiple schemas try to identify with the same URI, validators SHOULD raise an error condition.」としている
- `src/jsone_schema_index.erl` の `walk_schema_maps/5` は `$defs` を索引対象に含めているが、`$defs` は draft 6 には無いキーワード（draft 2019-09 以降）。モジュールのコメントは「未知のキーワードの中にある `$id` は対象にしない」としており記述と実装が食い違う。`test/jsone_schema_tests.erl` の `ignored_id_test/0` はこの拡張に依存している
- スキーマの妥当性検査は部分的で、`required` の要素重複（MUST）や `type` 配列の要素重複（MUST）は検査していない。同梱の `test/meta-schemas/draft-06.json` は公式の draft-06 メタスキーマと一致しているため、スキーマ検査に使う選択肢はある

## 設計方針

- `$id` の URI 衝突を検出して schema エラー（または警告）にする。どちらにするかは、既存のスキーマエラー処理の方針に合わせて決める
- `$defs` の扱いを決める。draft 6 の範囲に留めるなら索引対象から外し、前方互換のための拡張として残すならコメントと `CHANGES.md` に明記する
- スキーマの妥当性検査をどこまで行うか（メタスキーマ検証を提供するか、個別キーワードの検査に留めるか）を決めて README に明記する

## 完了条件

- 同一 URI の `$id` を与えたときに、黙って上書きされない
- `$defs` の扱いとスキーマ妥当性検査の範囲が、コメントと README に書かれている
- 上記の挙動を検査するテストが追加されている

## 解決方法

{未着手}
