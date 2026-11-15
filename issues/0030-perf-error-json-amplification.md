# エラー JSON の出力がコンテナ全体の複製で肥大化するのを抑える

- Created: 2026-11-14
- Completed: {YYYY-MM-DD}
- Branch: feature/refactor-error-json-amplification
- Polished: {YYYY-MM-DD}

## 目的

`jsone_schema_error:to_json/1` は各エラーに `schema` と `value` を丸ごと含める。`additionalProperties` / `additionalItems` のエラーは同じコンテナ全体を `value` に持つため、エラー件数 × データサイズで出力が肥大化し、CPU とメモリを消費する。信頼できない入力に対する資源消費を抑える。

## 現状

`src/jsone_schema_validator.erl` の `check_additional_properties/4` は余分なプロパティ 1 件ごとに `Value`（オブジェクト全体）を `data_invalid/3` に渡す。`check_items_array/6` の `additionalItems: false` も、余分な要素 1 件ごとに配列全体 `All` を渡す。`check_one_of_1/5` は全分岐のエラーを `Errors ++ NewErrors` で蓄積し、`max_errors` とは別に details を増やす。

実測（OTP 29、`max_errors => infinity`）:

- `additionalProperties: false`、800 プロパティ: 800 エラー、`to_json/1` が約 6.3 MB / 276 ms。
- 同 1,000 プロパティ: 約 9.9 MB / 460 ms。
- `additionalItems: false`、2,000 要素: 約 18 MB / 1.2 秒。
- `oneOf` 500 分岐 × 1,000 キー、`max_errors => 1`: top-level エラーは 1 件だが、details の `to_json/1` が約 5 MB / 233 ms。

`jsone_schema_error:json_safe/1` は `jsone:try_encode/1` でエンコード可否を確認した後に本エンコードするため、大きな値を二重に走査する。

## 設計方針

- `additionalProperties: false` のエラーは、オブジェクト全体ではなく該当するプロパティ名（または該当する子の値）を `value` に載せる。
- `additionalItems: false` のエラーは、配列全体ではなく該当する要素を `value` に載せる。
- `oneOf` / `anyOf` の details は `max_errors` とは別に件数上限を持たせ、先頭 N 件だけ保持する。`max_errors` の意味を「top-level エラー数」と「ネスト details 数」に分ける場合は README に明記する。
- `to_json/1` に出力バイト数またはエラー件数の上限オプションを設ける。
- `json_safe/1` の二重エンコードを避ける。`jsone:try_encode/1` の結果を再利用するか、事前検査をやめる。
- エラー表現の `schema` / `value` を常に含めるか、必要に応じて省略できるようにする。

## 完了条件

- `additionalProperties: false` 1,000 プロパティの `to_json/1` が、現状の 9.9 MB から大幅に縮小する（目安 1 MB 未満）。
- `additionalItems: false` 2,000 要素の `to_json/1` が 18 MB から大幅に縮小する。
- `oneOf` の details が上限件数で打ち切られ、`max_errors => 1` でも details が無制限に増えない。
- エラーの `path` は該当するプロパティ／要素を指し、`value` はその位置の値（または名前）を返す。既存のエラー形式を変える場合は README / CHANGES に記載する。
- 出力サイズと時間の回帰テストを追加する。固定の閾値は環境差を考慮して幅を持たせる。
- `xref` / `dialyzer` / `eunit` / `proper` が通る。

## 解決方法

{未着手}
