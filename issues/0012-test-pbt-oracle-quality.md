# PBT のオラクルが実装の写経になっており未カバーの性質がある

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/refactor-pbt-oracle-quality
- Polished: 2026-09-18

## 目的

PropEr のオラクルが実装と同じ判定を書き写しているため実装の誤りを検出できず、また未カバーの性質が残っている。オラクルを仕様から独立に書き、不足している性質を追加する。

## 現状

- `test/prop_jsone_schema.erl` の `prop_types/0` は `type_matches/2` をオラクルに使うが、これは `src/jsone_schema_validator.erl` の `is_type_valid/2` と同じ述語（`integer` を `Value - trunc(Value) == 0.0` で判定する部分まで同一）。7 つの型すべてが 1 対 1 で対応している
- `prop_min_max_length/0` はオラクルが `byte_size/1`、ジェネレータが ASCII のみ（`json_key/0` は `choose($a, $z)`）のため、PBT では文字列長の判定（コードポイント数）を検査できない。必須スイートの `maxLength.json` / `minLength.json` には非 BMP のケースがあり、バイト数え上げへの退行自体は 702 件で検出できる
- `prop_unique_items/0` は `[Json, Json]` のみで、総当たりの等価判定をオラクルにした性質が無い。`[Json, Json]` は先頭で衝突するため、重複探索の後半の経路を通らない
- 次のキーワードが PBT でカバーされていない: `multipleOf`（小数）、`pattern` / `patternProperties`、`properties` と `patternProperties` の関係、`dependencies`、`propertyNames`、`contains`、タプル形式 `items` と `additionalItems`、`max_errors` の打ち切り
  - `properties` と `additionalProperties` の関係は `prop_additional_properties/0` が、ローカルの `$ref` 解決は `prop_local_ref_equivalence/0` がカバーしている
  - `format` は `check_format/3` が意図的に簡易チェックであり（別 issue が方針を決め、`optional/format` の実行はさらに別 issue が担当する）、仕様準拠のオラクルを書くとその方針と衝突するため PBT の対象にしない

## 設計方針

- オラクルは検証対象の内部述語・ヘルパ（`is_type_valid/2` 相当の判定、`run_pattern/2`、`check_*`）を呼ばず、仕様の定義から書く。検証の入口である `jsone_schema:validate/2,3` と、JSON の等価判定という仕様上の概念を表す `jsone_schema_equality:equal/2` は使ってよい
- `type`: 仕様 §6.25 の 6 プリミティブと `integer`（小数部が 0 の数値を含む）をテスト側の対応表として独立に書く。`1.0` / `-0.0` / 巨大値を必ず生成する値ジェネレータを追加し、境界で判定させる
- `minLength` / `maxLength`: オラクルをコードポイント数（`unicode:characters_to_list/1` の長さ）にし、サロゲートペア・結合文字・NUL を含む文字列を生成する。NUL は ASCII だが JSON 文字列の境界として含める
- `uniqueItems`: 総当たりの等価判定をオラクルにし、要素数 2 以上の配列で合否を突き合わせる
- `multipleOf`: 小数の個別回帰テストは別 issue が所有する。PBT は `0.5` / `0.25` のような 2 の負の冪の除数と、それを掛けた厳密な倍数・そうでない値を生成し、合否を独立に判定する。巨大値（`1.0e308`）は別 issue の担当なので PBT では生成しない
- `pattern` / `patternProperties`: オラクルは Erlang の `re:run/3` を直接呼ぶ（実装の `run_pattern/2` は使わない）。スイートにある組み合わせではなく、`pattern` と `type` の併用などスイートに無い組み合わせを優先する
- `properties` と `patternProperties` の関係: 両方に一致するプロパティが `additionalProperties` の対象から外れる部分を対象にする（`properties` と `additionalProperties` の関係は既存の性質が担当する）
- `dependencies` / `propertyNames` / `contains` / タプル形式 `items` と `additionalItems`: 仕様 §6.21 / §6.22 / §6.14 / §6.9・§6.10 の定義から期待値を計算する
- `max_errors`: 仕様ではなく API 契約の検査とする。`max_errors => N` で N 件より多くのエラーが出るスキーマを生成し、返るエラー数が N 件で打ち切られることを検査する。`is_valid/2` では観測できないため `jsone_schema:validate/3` の戻り値を直接見る
- `numtests` とサイズ設定は既存の `?OPTS` に合わせる。既存の性質は 1 件あたり約 0.3 秒なので、10 件程度の追加は CI の実行時間に影響しない

## 完了条件

- 追加した性質が `./rebar3 as test proper` で通る
- オラクルが検証対象の内部述語・ヘルパを呼んでいない（`jsone_schema:validate/2,3` と `jsone_schema_equality:equal/2` は除く）。既存 14 性質についても `type_matches/2` 以外に写経が無いことを確認している
- `prop_types/0` のオラクルが仕様の型定義から書かれ、`1.0` / `-0.0` / 巨大値を必ず生成するジェネレータで検査している
- `prop_min_max_length/0` のオラクルがコードポイント数になり、サロゲートペア・結合文字・NUL を含む文字列を生成している
- `uniqueItems` の合否が総当たりの等価判定と一致する性質が追加されている
- 追加した性質ごとに、オラクルの出所（仕様の節か API 契約か）が `test/prop_jsone_schema.erl` のコメントに書かれている

## 解決方法

{未着手}
