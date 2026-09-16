# PBT のオラクルが実装の写経になっており未カバーの性質がある

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/refactor-pbt-oracle-quality
- Polished: {YYYY-MM-DD}

## 目的

PropEr のオラクルが実装と同じ判定を書き写しているため実装の誤りを検出できず、また未カバーの性質が残っている。オラクルを仕様から独立に書き、不足している性質を追加する。

## 現状

- `test/prop_jsone_schema.erl` の `prop_types/0` は `type_matches/2` をオラクルに使うが、これは `src/jsone_schema_validator.erl` の `is_type_valid/2` と同じ述語（`integer` を `Value - trunc(Value) == 0.0` で判定する部分まで同一）
- `prop_min_max_length/0` はオラクルが `byte_size/1`、ジェネレータが ASCII のみのため、実装の `length(string_chars/1)`（コードポイント数）がバイト数え上げに退行しても検出できない
- `prop_unique_items/0` は `[Json, Json]` と `1` / `1.0` のみで、`jsone_schema_equality:equal/2` をオラクルにした「総当たり等価判定と一致する」性質が無い。`has_duplicate/1` の集合判定と `find_duplicate/1` の判定がずれても検出できない
- 次のキーワードが PBT でカバーされていない: `multipleOf`（小数・巨大値）、`pattern` / `patternProperties`、`properties` と `additionalProperties` / `patternProperties` の関係、`dependencies`、`propertyNames`、`contains`、タプル形式 `items` と `additionalItems`、`format`、`$ref` 解決、`max_errors` の打ち切り

## 設計方針

- オラクルは仕様の定義から独立に書き、実装のヘルパ関数を呼ばない
- 非 ASCII（サロゲートペア・結合文字・NUL）を含む文字列ジェネレータを追加する
- 上記の未カバー性質を追加する。draft 6 テストスイートでカバー済みの組み合わせではなく、スイートに無い組み合わせを優先する
- `numtests` とサイズ設定は既存の `?OPTS` に合わせる

## 完了条件

- 追加した性質が `./rebar3 as test proper` で通る
- オラクルが `src/` の関数を参照していない
- `prop_types/0` のオラクルが整数の境界（`1.0` / `-0.0` / 巨大値）を含む

## 解決方法

{未着手}
