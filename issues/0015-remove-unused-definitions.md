# 未使用のマクロ・export・到達不能節を削除する

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/remove-unused-definitions
- Polished: {YYYY-MM-DD}

## 目的

使われていない定義と到達しないコードを削除し、保守対象を減らす。

## 現状

- `src/jsone_schema.hrl` の `?wrong_type_dependency` / `?not_array` / `?external` はリポジトリ全体で参照 0 件
- `src/jsone_schema_state.erl` の `get_base_uri/1` / `get_document_uri/1` / `get_max_errors/1` / `get_root_schema/1` / `get_schema_loader/1` / `get_schemas/1` / `new/2` はテストを含めて呼び出し 0 件（`new/2` は `jsone_schema_state:new/3` を直接呼ぶ経路しかない）
- `jsone_schema_state:remove_last_from_path/1` の空パス節は到達不能。呼び出し元は `add_to_path/2` の直後のみ
- `jsone_schema_state:merge_index/2` の `undefined` 節は到達不能。`cache_document/3` が先に `ensure_index/1` を呼び、`ensure_index/1` は必ず索引を設定する
- `jsone_schema_validator:matches_any_pattern/2` の空マップ節は `lists:any/2` と同じ結果を返すだけで、節を分ける意味がない

## 設計方針

- 未使用の定義は削除する。残す場合は理由をコメントに書く
- 到達不能節は、防御的に残すなら理由をコメントに書く。理由が無ければ削除する
- 削除前に `rg` で参照が 0 件であることを確認する
- `jsone_schema_state` の未使用 export を削除する場合は、`jsone_schema` からの利用が無いことを併せて確認する

## 完了条件

- 削除後も `./rebar3 xref` / `./rebar3 dialyzer` / `./rebar3 as test eunit` / `./rebar3 as test proper` が通る
- 削除した公開関数を前提にしたテストが無い
- 残した節には理由のコメントが付いている

## 解決方法

{未着手}
