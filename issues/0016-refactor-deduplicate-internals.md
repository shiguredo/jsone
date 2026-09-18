# 内部実装の重複を整理する

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/refactor-deduplicate-internals
- Polished: 2026-09-18

## 目的

同じ処理が複数箇所に書かれている状態を解消し、片方だけ直して不整合になる余地をなくす。

## 現状

- `schema()` 型が `jsone_schema` / `jsone_schema_state` / `jsone_schema_index` / `jsone_schema_error` の 4 モジュールで同じ定義（`map() | boolean()`）になっている。`jsone_schema:schema/0` は export_type 済み。`jsone_schema_error` では `reason()` の中でも使っている
- `jsone_schema:schema_loader/0` の fun 型（`fun((binary()) -> {ok, schema()} | schema() | {error, term()})`）が、`jsone_schema_state` の `#state{}` の `schema_loader` フィールドと `get_schema_loader/1` の `-spec` にも同じ形で書かれている。`get_schema_loader/1` は未使用のため別 issue が削除する予定
- `$id` を取り出す同型の処理があわせて 5 箇所にある。`jsone_schema_state:enter_schema/2` / `absolute_schema_id/1` / `apply_schema_id/2`（`#{?ID := Id} when is_binary(Id)` か `undefined`。`absolute_schema_id/1` は絶対 URI かどうかの検査も行う）、`jsone_schema_store:schema_id/1`（`maps:get(?ID, ..., undefined)` が binary か `undefined`）、`jsone_schema_index:index_id/5`（`#{?ID := Id} when is_binary(Id)`）
- 状態の復元関数が `jsone_schema_state:restore/2` / `restore_schema/2` / `undo_resolve_ref/2` の 3 つに分かれており、名前から差が読み取れない。戻すフィールドは `restore/2` が `root_schema` / `current_schema` / `document_uri` / `base_uri` / `errors`、`restore_schema/2` が `current_schema` / `base_uri`、`undo_resolve_ref/2` が `root_schema` / `current_schema` / `document_uri` / `base_uri`（`restore/2` は `undo_resolve_ref/2` が戻すものに `errors` を加えたもの。`restore_schema/2` は `undo_resolve_ref/2` の部分集合）
- `jsone_schema_validator:check_max_items/3` / `check_min_items/3` / `check_max_properties/3` / `check_min_properties/3` は、対象の型の判定（`is_list/1` と `is_map/1`）・サイズの取得（`length/1` と `map_size/1`）・比較演算子・データ不正のエラー理由（`?wrong_size` / `?too_many_properties` / `?too_few_properties`）・スキーマ不正のエラー理由（`?schema_invalid` / `?wrong_max_properties` / `?wrong_min_properties`）だけが違う。同じファイルの `check_number_bound/4` と `check_string_length/4` は述語を渡す形になっており、揃っていない
- 検証結果の確定処理（エラー一覧を取り出して空かどうかを判定する部分）が `jsone_schema:do_validate/4` と `jsone_schema_validator:run_subschema/3` に重複している
- `jsone_schema_state:new/2,3` の `Options` は `map()` と型付けされており、`jsone_schema:options()` を使っていない
- `jsone_schema_state:resolve_ref/2` のエラー戻り値に含まれる state は、唯一の呼び出し元である `jsone_schema_validator:check_ref/3` で使われていない
- `jsone_schema_validator:run_subschema/3` のエラー戻り値の state も現時点では使われていないが、外部 `$ref` のキャッシュを扱う別 issue が「失敗した場合もキャッシュと索引を引き継ぐ」ために使う予定がある

## 設計方針

- 型は `jsone_schema:schema/0` / `jsone_schema:options()` / `jsone_schema:schema_loader/0` を参照する形に統一する。`jsone_schema_error` の `reason()` の中の `schema()` も含める。型の参照は実行時の依存を作らないため、参照元モジュールとの循環は問題にならない
- `jsone_schema_state:new/2,3` の `Options` を `jsone_schema:options()` にする
- `$id` の取り出しは `jsone_schema_uri` に置く共通関数（`?ID` が binary ならその値、そうでなければ `undefined` を返す）に寄せる。`jsone_schema_state` / `jsone_schema_store` / `jsone_schema_index` はいずれも既に `jsone_schema_uri` を参照しているため、新しい依存辺を増やさない（1 関数のために新しいモジュールを追加しない）。`absolute_schema_id/1` は絶対 URI かどうかの検査を共通関数の上に残し、`undefined` の場合を必ず処理する
- 復元するフィールドの一覧は `restore/2` と `undo_resolve_ref/2` の共通部分（`root_schema` / `current_schema` / `document_uri` / `base_uri`）を 1 つの内部関数にまとめ、`restore/2` はそれに `errors` の復元を加える。`restore_schema/2` は `current_schema` と `base_uri` だけを戻す別関数として残し、何を戻し何を残すかをコメントに書く（`undo_resolve_ref/2` は読み込み済みのキャッシュと索引を残す）
- max/min 系 4 関数は、`check_number_bound/4` / `check_string_length/4` と同じ形の 1 つの関数に統一する。渡すのは対象の型の判定・サイズの取得・サイズの比較述語・データ不正のエラー理由・スキーマ不正のエラー理由の 5 つ。`jsone_schema_error:data_invalid/3` に渡す値は現行どおりコンテナ全体（配列または map）にする。未使用になる `check_array_size/3` は削除する
- 結果の確定処理は、`jsone_schema_state` にエラーの有無を返す関数を 1 つ用意し、`jsone_schema:do_validate/4` と `jsone_schema_validator:run_subschema/3` の正常終了時の判定をそれに統一する。結果の組み立て（`{ok, Data}` / `{ok, State}` / `{error, Errors}` / `{error, Errors, State}`）は呼び出し側に残す。`jsone_schema_error:add_reason/2` が throw する経路は現状のままにする
- `resolve_ref/2` のエラー戻り値を `{error, jsone_schema_error:error_info()}` の 2 要素にし、`check_ref/3` をそれに合わせる。`run_subschema/3` の戻り値は別 issue が使う予定があるため、この issue では形を変えない
- `CHANGES.md` に独立したエントリは追加しない。JSON Schema バリデータは `## develop` の未リリース `[ADD]` の中にあり、この変更は内部整理としてその初回リリース内容に含まれる
- 挙動を変えない。性能上の改善（`uniqueItems` の 1 パス化など）は別 issue で扱う

## 完了条件

- `schema()` 型の定義が `jsone_schema:schema/0` の 1 箇所になり、他の 3 モジュールがそれを参照している（`jsone_schema_error:reason()` の中も含む）
- `schema_loader()` の fun 型が `jsone_schema:schema_loader/0` の 1 箇所になり、`jsone_schema_state` の `#state{}` のフィールドと `get_schema_loader/1` の `-spec` がそれを参照している
- `jsone_schema_state:new/2,3` の `Options` が `jsone_schema:options()` になっている
- `$id` を取り出す処理が `jsone_schema_uri` の 1 関数になり、5 箇所がそれを呼んでいる（`rg '\?ID\b'` で取り出しが 1 箇所になっていることを確認する。`?ID_OLD` は含めない）
- `restore/2` と `undo_resolve_ref/2` が共通の内部関数を使い、`restore_schema/2` は `current_schema` と `base_uri` だけを戻す別関数として残っている。3 関数の差がコメントから読み取れる
- max/min 系 4 関数と `check_array_size/3` が消え、統一関数に置き換わっている。データ不正時とスキーマ不正時のエラー理由、および `data_invalid/3` に渡す値（コンテナ全体）が現行どおり
- エラーの有無を返す関数が `jsone_schema_state` に 1 つあり、`jsone_schema:do_validate/4` と `jsone_schema_validator:run_subschema/3` がそれを使っている
- `resolve_ref/2` のエラー戻り値が 2 要素になり、`check_ref/3` がそれに合わせて更新されている。`run_subschema/3` の戻り値の形は変わっていない
- `./rebar3 xref` / `./rebar3 dialyzer` / `./rebar3 as test eunit` / `./rebar3 as test proper` が通る
- 差分がリファクタリングに限定され、テストの期待値を変えていない

## 解決方法

{未着手}
