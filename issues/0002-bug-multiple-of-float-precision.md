# multipleOf が小数で誤判定する

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-multiple-of-float-precision
- Polished: {YYYY-MM-DD}

## 目的

金額のように小数を含む値の倍数判定が誤り、仕様上は正しいデータを invalid にする問題を修正する。コードレビューで致命的と判断した項目。

`{"type": "number", "multipleOf": 0.01}` のような金額スキーマで 4.35 が弾かれるため、実利用で最初に踏む欠陥になる。

## 現状

`src/jsone_schema_validator.erl` の `is_multiple_of/2` は整数同士なら `rem` で判定するが、小数を含む場合は `(Quotient - trunc(Quotient)) * MultipleOf == 0.0` で判定しており、IEEE 754 倍精度の丸め誤差がそのまま合否になる。

再現（実測）:

```erlang
jsone_schema:validate(#{<<"multipleOf">> => 0.01}, 0.07).
%% {error, [#{error => not_multiple_of, ...}]}

jsone_schema:validate(#{<<"multipleOf">> => 0.01}, 4.35).
%% {error, [#{error => not_multiple_of, ...}]}

jsone_schema:validate(#{<<"multipleOf">> => 0.1}, 0.3).
%% {error, [#{error => not_multiple_of, ...}]}

jsone_schema:validate(jsone:decode(<<"{\"multipleOf\":0.01}">>), jsone:decode(<<"4.35">>)).
%% {error, [#{error => not_multiple_of, ...}]}
```

除算結果はそれぞれ 7.000000000000001 / 434.99999999999994 / 2.9999999999999996 になる。

仕様の根拠:

- draft-06 validation (`draft-wright-json-schema-validation-01`) §6.1「A numeric instance is valid only if division by this keyword's value results in an integer.」
- 同 §3.2「The JSON specification allows numbers with arbitrary precision ... numeric instances processed by JSON Schema can be arbitrarily large and/or have an arbitrarily long decimal part, regardless of the ability of the underlying programming language to deal with such data.」

テストスイートの唯一の小数ケース（`0.0075` と `multipleOf: 0.0001`）は偶然 75.0 になるため、702 ケース通過では検出できない（実測で確認済み）。`tests/draft6/optional/float-overflow.json` は実行対象外。

## 設計方針

- 10 進の桁数に合わせて両者を整数化してから `rem` で判定する（例: 小数部の最大桁数 n を求めて 10^n 倍する）
- 桁数が大きく整数化できない場合は、許容誤差を使うか判定不能として扱う。採用した方針はコメントに明記する
- 浮動小数の除算結果をそのまま整数判定に使わない
- 併せて、`multipleOf` の判定方針（10 進として扱う範囲）をコメントに書く

## 完了条件

- 上記の再現ケースが `{ok, _}` を返す
- `0.0075` と `multipleOf: 0.0001` の既存通過ケースが引き続き通る
- 上記再現値を使った回帰テストが `test/jsone_schema_tests.erl` に追加されている

## 解決方法

{未着手}
