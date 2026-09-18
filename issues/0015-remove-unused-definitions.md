# 未使用のマクロ・export・到達不能節を削除する

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/remove-unused-definitions
- Polished: 2026-09-18

## 目的

使われていない定義と到達しないコードを削除し、保守対象を減らす。

## 現状

- `src/jsone_schema.hrl` の `?wrong_type_dependency` / `?not_array` / `?external` はリポジトリ全体で参照 0 件（`-define` の定義行だけが残っている）。`?data_invalid` も 0006 で `contains` の理由を `?no_contains_match` にしたため参照 0 件になった（`jsone_schema_error:data_invalid/3` の関数自体は現役）
- `src/jsone_schema_state.erl` の `get_base_uri/1` / `get_document_uri/1` / `get_max_errors/1` / `get_root_schema/1` / `get_schema_loader/1` / `get_schemas/1` / `new/2` はテストを含めて呼び出し 0 件。状態の生成は `jsone_schema:do_validate/4` が `new/3` を呼んでおり、`new/2` を呼ぶ経路は無い。`jsone_schema_state` は README に記載の無い内部モジュールで、`state()` は `-opaque` なので公開 API ではない
- `jsone_schema_state:remove_last_from_path/1` の空パス節は到達不能。呼び出し元は `jsone_schema_validator:check_additional_properties/4` と `check_child/4` の 2 箇所で、いずれも `add_to_path/2` の後（間にエラー付与や検証呼び出しを挟むが、パスを減らす処理は無い）
- `jsone_schema_state:merge_index/2` の `undefined` 節は到達不能。呼び出し元は `cache_document/3` のみで、その直前の `ensure_index/1` が必ず索引を設定する
- `jsone_schema_validator:matches_any_pattern/3` の空マップ節は到達不能ではない。`additionalProperties` を指定して `patternProperties` を書かないスキーマでは `extra_property_names/3` からプロパティごとに呼ばれる通常経路で、`{not_matched, State}` を返しつつ `maps:fold/3` の走査を省いている

## 設計方針

- 未使用のマクロと未使用の export は削除する。`-define` を残しても参照 0 件は解消しないため、マクロを残す選択は取らない
- export を削除するときは、`-export` の行だけでなく関数定義（`-spec` とコメントを含む）も削除する。`warn_unused_function` はコンパイラ既定で有効であり、`rebar.config` の `warnings_as_errors` がそれをエラーにする。`xref_checks` には `locals_not_used` も入っている。どちらも export だけを削除した変更を検出する
- 到達不能節は削除する。削除するのは `remove_last_from_path/1` の空パス節と `merge_index/2` の `undefined` 節で、`remove_last_from_path/1` 自体の export は残す（2 箇所から呼ばれている）
- `matches_any_pattern/3` の空マップ節は残し、通常経路の高速路である理由をコメントに書く
- 削除の前に `rg` で参照が 0 件であることを確認する。`jsone_schema_state` の利用元は `jsone_schema` / `jsone_schema_error` / `jsone_schema_validator` の 3 モジュールなので、3 つすべてからの利用が無いことを確認する
- `matches_any_pattern/3` は 0006 が `run_pattern/2` のエラー扱いを、`merge_index/2` は別 issue が `maps:merge/2` の上書き挙動を扱う。この issue が触るのは空マップ節と `undefined` 節だけにする
- `CHANGES.md` に独立したエントリは追加しない。JSON Schema バリデータは `## develop` の未リリース `[ADD]` の中にあり、この変更は内部整理としてその初回リリース内容に含まれる

## 完了条件

- `rg` で `?wrong_type_dependency` / `?not_array` / `?external` / `?data_invalid` の参照が 0 件になり、`jsone_schema.hrl` から定義が消えている
- `jsone_schema_state` の `-export` から 7 関数が消え、関数定義（`new/2` / 各 getter）も消えている
- `remove_last_from_path/1` の空パス節と `merge_index/2` の `undefined` 節が消えている。`remove_last_from_path/1` の export は残っている
- `matches_any_pattern/3` の空マップ節が残り、通常経路の高速路である理由がコメントに書かれている
- `./rebar3 xref` / `./rebar3 dialyzer` / `./rebar3 as test eunit` / `./rebar3 as test proper` が通る（`warnings_as_errors` の unused function 警告と xref の `locals_not_used` が、export だけを削除した不完全な変更を検出する）
- 削除した関数を参照するテストが無い（`test/` に `jsone_schema_state` の参照が無いことを `rg` で確認する）

## 解決方法

{未着手}
