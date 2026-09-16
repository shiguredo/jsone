# 内部実装の重複を整理する

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/refactor-deduplicate-internals
- Polished: {YYYY-MM-DD}

## 目的

同じ処理が複数箇所に書かれている状態を解消し、片方だけ直して不整合になる余地をなくす。

## 現状

- `schema()` 型が `jsone_schema` / `jsone_schema_state` / `jsone_schema_index` / `jsone_schema_error` で再定義されている。`jsone_schema:schema/0` は export_type 済み
- `schema_loader()` の fun 型が `jsone_schema` と `jsone_schema_state` に重複している
- `$id` を取り出す同型の処理が `jsone_schema_state:enter_schema/2` / `absolute_schema_id/1` / `apply_schema_id/2`、`jsone_schema_store:schema_id/1`、`jsone_schema_index:index_id/5` に重複している
- 状態の復元関数が `jsone_schema_state:restore/2` / `restore_schema/2` / `undo_resolve_ref/2` の 3 つに分かれており、名前から差が読み取れない（`restore/2` は `undo_resolve_ref/2` が戻すフィールドにエラーも加えたもの）
- `jsone_schema_validator:check_max_items/3` / `check_min_items/3` / `check_max_properties/3` / `check_min_properties/3` が比較演算子とエラー理由だけの違い。`check_number_bound/4` や `check_string_length/4` のように述語を渡す形に統一できる
- 検証結果の確定処理（エラーが空なら `{ok, ...}`、そうでなければ `{error, Errors}`）が `jsone_schema:do_validate/4` と `jsone_schema_validator:run_subschema/3` に重複している
- `jsone_schema_validator:run_subschema/3` と `jsone_schema_state:resolve_ref/2` のエラー戻り値に含まれる state は、すべての呼び出し元で未使用

## 設計方針

- 型は `jsone_schema:schema/0` / `options()` / `schema_loader()` を参照する形に統一する
- `$id` の取り出しは 1 つの共通関数に寄せる。`jsone_schema_state` / `jsone_schema_store` / `jsone_schema_index` の依存方向を確認し、置き場所を決める
- max/min 系 4 関数は述語とエラー理由を渡す形に統一する
- 未使用の state 戻り値は型・実装・呼び出し側から削除する
- 挙動を変えない。性能上の改善（`uniqueItems` の 1 パス化など）は別 issue で扱う

## 完了条件

- 同型の定義・処理が 1 箇所になり、`rg` で重複が残っていないことを確認している
- `./rebar3 xref` / `./rebar3 dialyzer` / `./rebar3 as test eunit` / `./rebar3 as test proper` が通る
- 差分がリファクタリングに限定され、挙動の変更を含んでいない

## 解決方法

{未着手}
