# multipleOf が小数で誤判定する

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-multiple-of-float-precision
- Polished: 2026-09-16

## 目的

金額のように小数を含む値の倍数判定が誤り、仕様上は正しいデータを invalid にする問題を修正する。

`{"type": "number", "multipleOf": 0.01}` のような金額スキーマで 4.35 が弾かれる。

## 現状

`src/jsone_schema_validator.erl` の `is_multiple_of/2` は整数同士なら `rem` で判定するが、小数を含む場合は `(Quotient - trunc(Quotient)) * MultipleOf == 0.0` で判定しており、IEEE 754 倍精度の丸め誤差がそのまま合否になる。`check_multiple_of/3` の直上のコメントは「オーバーフローする場合 (1e308 など) は倍数ではないものとする」として `error:badarith` を捕捉して false を返している。

再現（実測）:

```erlang
jsone_schema:validate(#{<<"multipleOf">> => 0.01}, 0.07).
%% {error, [#{error => not_multiple_of, ...}]}

jsone_schema:validate(#{<<"multipleOf">> => 0.01}, 4.35).
%% {error, [#{error => not_multiple_of, ...}]}

jsone_schema:validate(#{<<"multipleOf">> => 0.1}, 0.3).
%% {error, [#{error => not_multiple_of, ...}]}

jsone_schema:validate(#{<<"multipleOf">> => 0.05}, 1.15).
%% {error, [#{error => not_multiple_of, ...}]}  10 進では 1.15 = 23 * 0.05

jsone_schema:validate(#{<<"multipleOf">> => 0.05}, 4.35).
%% {error, [#{error => not_multiple_of, ...}]}  10 進では 4.35 = 87 * 0.05

jsone_schema:validate(jsone:decode(<<"{\"multipleOf\":0.01}">>), jsone:decode(<<"4.35">>)).
%% {error, [#{error => not_multiple_of, ...}]}
```

除算結果はそれぞれ 7.000000000000001 / 434.99999999999994 / 2.9999999999999996 になる。

仕様の根拠:

- draft-06 validation (`draft-wright-json-schema-validation-01`) §6.1「A numeric instance is valid only if division by this keyword's value results in an integer.」
- 同 §3.2「The JSON specification allows numbers with arbitrary precision ... numeric instances processed by JSON Schema can be arbitrarily large and/or have an arbitrarily long decimal part, regardless of the ability of the underlying programming language to deal with such data.」

テストスイート `test/JSON-Schema-Test-Suite/tests/draft6/multipleOf.json` のうち小数の `multipleOf` を使うのは 6 件で、valid になるのは `0` と `1.5`、`4.5` と `1.5`、`0.0075` と `0.0001` の 3 件。いずれも商が偶然ちょうど整数になるため現行実装でも通り、`35` と `1.5`、`0.00751` と `0.0001`、`1.0e308` と `0.123456789` の 3 件も現行実装が正しく invalid とする。702 ケース通過ではこの issue の欠陥は検出できない。

## 設計方針

- 10 進の正本は「倍精度浮動小数の最短往復 10 進表記」とする。Erlang では `float_to_list(Value, [short])` が相当する。`float_to_list/1` の既定は有効数字 20 桁で `0.07` が `7.00000000000000066613e-02` になり、小数部の桁数が定まらない
- 両者を共通の 10 の冪で整数化してから `rem` で判定する。10 の冪と整数化は Erlang の bignum で厳密に扱い、浮動小数の乗算と `trunc/1` の組み合わせは使わない。`trunc(1.15 * 100) = 114` のように桁上がり境界で誤差が残り、この issue が直そうとしている誤判定が別入力で再発する
- 指数表記（`1.0e308` など）も最短往復表記から仮数部と指数部に分けて整数化する。倍精度の 10 進指数はおおむね -324〜308 に収まるため整数化は常に可能で、許容誤差は導入しない
- 整数同士の高速路（`Value rem MultipleOf`）は残す
- 指数表記を整数化する方針では `{"type": "integer", "multipleOf": 0.5}` と `1.0e308` が valid になる。これは `test/JSON-Schema-Test-Suite/tests/draft6/optional/float-overflow.json` の期待と一致し、同じ 702 ケース内の `multipleOf: 0.123456789` と `1e308` は invalid のまま維持できる
- `check_multiple_of/3` の直上のコメントを新しい方針に合わせて書き直す

## 完了条件

- 次の入力が `{ok, _}` を返す
  - `#{<<"multipleOf">> => 0.01}` と `0.07`
  - `#{<<"multipleOf">> => 0.01}` と `4.35`
  - `#{<<"multipleOf">> => 0.1}` と `0.3`
  - `#{<<"multipleOf">> => 0.05}` と `1.15`
  - `#{<<"multipleOf">> => 0.05}` と `4.35`
  - `jsone:decode(<<"{\"multipleOf\":0.01}">>)` と `jsone:decode(<<"4.35">>)`
- 次の入力が `{error, _}` を返す
  - `#{<<"multipleOf">> => 1.5}` と `35`
  - `#{<<"multipleOf">> => 0.0001}` と `0.00751`
  - `#{<<"multipleOf">> => 0.123456789}` と `1.0e308`
- 次の入力が引き続き `{ok, _}` を返す
  - `#{<<"multipleOf">> => 1.5}` と `0`、`4.5`
  - `#{<<"multipleOf">> => 0.0001}` と `0.0075`
- `test/JSON-Schema-Test-Suite/tests/draft6` の 702 ケースと `test/prop_jsone_schema.erl` の全プロパティが引き続き通る
- 上記の回帰テストが `test/jsone_schema_tests.erl` に追加されている

## 解決方法

{未着手}
