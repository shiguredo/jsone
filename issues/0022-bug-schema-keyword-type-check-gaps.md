# キーワード値とネストしたサブスキーマの型検査がデータに依存して漏れる

- Created: 2026-10-12
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-schema-keyword-type-check-gaps
- Polished: {YYYY-MM-DD}

## 目的

スキーマのキーワード値とネストしたサブスキーマの型検査が、インスタンスの型やプロパティの有無に依存して漏れている。仕様が `MUST` とする型制約を破ったスキーマが、渡すデータによって `{ok, _}` になったりエラーになったりし、スキーマ自体の誤りに気づけない。データ検証とは独立にスキーマの型を検査する。

## 現状

`src/jsone_schema_validator.erl` の `check_keyword_value/5` は「インスタンスの型に合わないキーワードは適用しない」規則に従う。そのためキーワード値の型検査が、対応するインスタンス型のときだけ走る検証関数の内部に置かれている。`properties` / `patternProperties` / `dependencies` はさらに、マップの値（サブスキーマ）が実際に評価されるときだけ型検査される。`check_properties/3` は `#{Name := Property}` がある場合だけ `check_child/4` を呼び、`check_pattern_properties/3` は一致したプロパティだけ、`check_dependencies/3` は依存が起動したプロパティだけを評価する。`definitions` は参照されない限り検証されない。`jsone_schema_index:walk_schema_maps/6` は索引のためにサブスキーマを走査するが型検査はしない。

`$id` は `jsone_schema_state:enter_schema/2` と `jsone_schema_index:index_id/5` が `is_binary/1` を満たす場合だけ採用し、型不正を無視する。`$schema` は `jsone_schema_validator:schema_ver/1` が未対応の値として扱い `schema_unsupported` を返す。`format` は `check_format_1/3` の最後の節で常に無視する。

実測（`jsone_schema:validate/2`、OTP 29 / erts-17.1、ローカル）:

| スキーマ | データ | 現状の結果 |
|---|---|---|
| `{"items":5}` | `1` | `{ok,1}`（配列のときだけ `{wrong_type_items,5}`） |
| `{"additionalItems":5}` | `[]` | `{ok,[]}`（`items` 配列の範囲外要素がないと無視） |
| `{"contains":5}` | `1` | `{ok,1}`（配列だと `no_contains_match` の data エラー） |
| `{"additionalProperties":5}` | `1` | `{ok,1}` |
| `{"propertyNames":5}` | `{}` | `{ok,{}}` |
| `{"properties":{"a":5}}` | `{}` | `{ok,{}}`（該当プロパティがないと無視） |
| `{"patternProperties":{"a":5}}` | `{}` | `{ok,{}}` |
| `{"dependencies":{"a":5}}` | `{}` | `{ok,{}}`（依存が起動しないと無視） |
| `{"definitions":{"a":5}}` | `1` | `{ok,1}` |
| `{"$id":5}` | `1` | `{ok,1}` |
| `{"format":5}` | `"x"` | `{ok,"x"}` |
| `{"$schema":5}` | `1` | `{error,...}`（型不正ではなく `{schema_unsupported,5}` として報告） |

仕様根拠:

- `$id` は core-01 §9.2、`definitions` は core-01 §9.3 で値の型が定まる
- `format` は validation-01 §8、`items` は §6.9、`additionalItems` は §6.10、`contains` は §6.14、`properties` は §6.18、`patternProperties` は §6.19、`additionalProperties` は §6.20、`dependencies` は §6.21、`propertyNames` は §6.22
- https://datatracker.ietf.org/doc/html/draft-wright-json-schema-01
- https://datatracker.ietf.org/doc/html/draft-wright-json-schema-validation-01

なお `allOf` / `anyOf` / `oneOf` / `not` の要素がスキーマでない場合は `check_keyword_value/5` から常に評価され、データに依存せず schema エラーが積まれる。分岐内の schema エラーが分岐の失敗として扱われる問題は別 issue（0021）が扱う。

## 設計方針

- データ検証の適用条件（インスタンス型に合わないキーワードを適用しない規則）と、スキーマ自体の妥当性検査を分離する。スキーマの型検査は検証の入口で 1 回だけ行い、データの型や内容に依存させない
- 検査対象はスキーマ位置にある値とする。`jsone_schema_index` の `walk_schema_maps/6` / `walk_schema_values/6` / `walk_schema_arrays/6` / `walk_items/3` / `walk_dependencies/6` が使うスキーマ位置の定義と同じキーワードを対象にする。`properties` / `patternProperties` / `definitions` / `$defs` のマップのキーが binary である検査は既存の `check_binary_keys/2` と同じ規則にする
- 期待する型は次のとおり。`$schema` / `$id` / `format` は binary、`items` はスキーマまたはスキーマの配列、`additionalItems` / `additionalProperties` は boolean またはスキーマ、`contains` / `propertyNames` はスキーマ、`properties` / `patternProperties` / `definitions` / `$defs` は値がスキーマのマップ、`dependencies` は値がスキーマまたは binary の配列のマップ
- 検査で見つけた型不正は `jsone_schema_error:schema_invalid/2` で積み、`max_errors` の上限判定を通す。`$ref` の循環や深さ上限のように検証を打ち切る理由ではない
- 型検査はデータの検証と同じ検証 1 回につき 1 度だけ走らせる。プロパティごと・要素ごとに再走査しない
- 0017 の「メタスキーマ検証は提供しない」「`required` と `type` 配列の要素重複は検査しない」という決定には踏み込まない。この issue は個別キーワードの型検査の実装漏れを埋めるだけにする
- `CHANGES.md` に独立したエントリは追加しない。JSON Schema バリデータは `## develop` の未リリース `[ADD]` の中にあり、この変更はその初回リリース内容に含まれる

## 完了条件

- 現状の表に挙げた各ケースで、データの型や内容にかかわらずトップレベルに `kind => schema` のエラーが返る（`{ok, _}` にならない）
- `{"properties":{"a":5}}` に `{}` と `{"a":1}` のどちらを渡しても同じ schema エラーになる（該当プロパティの有無に依存しない）
- `{"$schema":5}` が `{schema_unsupported,5}` ではなく型不正の schema エラーになる
- データ検証の適用条件（インスタンス型に合わないキーワードを適用しない規則）が変わっていない。既存の EUnit 753 件と PropEr の全性質が引き続き通る
- 上記を検査する回帰テストが `test/jsone_schema_tests.erl` に追加されている
- `./rebar3 xref` / `./rebar3 dialyzer` / `./rebar3 as test eunit` / `./rebar3 as test proper` が通る

## 解決方法

{未着手}
