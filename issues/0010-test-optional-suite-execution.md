# JSON-Schema-Test-Suite の optional ケースを実行する

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/add-optional-suite-execution
- Polished: 2026-09-18

## 目的

draft 6 の必須ケース 702 件は実行されているが `tests/draft6/optional/` の 194 件が実行されておらず、format や数値の境界の欠陥が検出できていない。実行範囲を広げる、または対象外とする理由を明示する。

## 現状

`test/jsone_schema_draft6_tests.erl` の `setup/0` が `filelib:wildcard(filename:join(SuiteDir, "*.json"))` で `tests/draft6/*.json` のみを読み込む（`draft6_test_/0` は setup を呼ぶだけ）。`tests/draft6/optional/` は実行されない。

件数の実測値:

- トップレベル `tests/draft6/*.json`: 702 件（実行されている）
- `tests/draft6/optional/**`: 194 件（実行されていない）。13 ファイルで、`optional/` 直下が 4 ファイル、`optional/format/` が 9 ファイル

optional を実行すると 43 件失敗する（実測。内訳の合計と一致する）:

- 未対応 format の 29 件: `format/hostname` 9 / `format/json-pointer` 12 / `format/uri` 7 / `format/uri-template` 1
- 対応済み format の緩さ 4 件: `format/email` 3 / `format/ipv6` 1
- `ecmascript-regex` 9 件: 現行の `re:run/3` のオプションでは解消できない。`ucp` を外すと失敗するケースが入れ替わって 9 件のままになり、`dollar_endonly` を足しても 8 件にしか減らない（実測）
- `float-overflow` 1 件: `{"type": "integer", "multipleOf": 0.5}` に `1.0e308` を与えると `not_multiple_of` になる

`bignum` / `non-bmp-regex` / `format/date-time` / `format/ipv4` / `format/uri-reference` は失敗 0 件。

`README.md` は「JSON-Schema-Test-Suite の draft 6 テストを `test/jsone_schema_draft6_tests.erl` で全ケース実行しています」、`CHANGES.md` は「draft 6 の全テストケースを EUnit で実行する」と書いており実態と一致しない。

## 設計方針

- 実行対象は `optional/` の次のファイルとする。対象ファイルの一覧をテストモジュール上に持ち、`filelib:wildcard` に任せない。トップレベルの 702 件は現行どおり実行する
  - `optional/bignum.json`（9 件）
  - `optional/float-overflow.json`（1 件）
  - `optional/non-bmp-regex.json`（12 件）
  - `optional/format/` の対応済み 5 format: `date-time.json`（12 件）/ `email.json`（9 件）/ `ipv4.json`（8 件）/ `ipv6.json`（29 件）/ `uri-reference.json`（7 件）
  - 実行対象は合計 87 件
- 除外するファイルと理由をテストモジュールのコメントに書く
  - `optional/format/hostname.json` / `uri.json` / `uri-template.json` / `json-pointer.json`（29 件）: 未対応の format は常に有効とする方針のため（format の対応範囲を決める別 issue の決定）
  - `optional/ecmascript-regex.json`（9 件）: ECMA 262 と Erlang `re` の差。§3.3 は正規表現を SHOULD で ECMA 262 としており、`re` のオプションでは差が解消しない
- `optional/non-bmp-regex.json` は 12 件すべて通るため実行対象に含める
- `float-overflow` の 1 件は `multipleOf` の小数判定の修正（別 issue）で解消する。この issue はその修正後に実装する
- `format/email` 3 件と `format/ipv6` 1 件は format の方針を扱う別 issue の修正で解消する。この issue は修正後の実行対象を定義する
- `README.md` のテスト実行範囲の記述と `CHANGES.md` の該当行の更新は、README / `CHANGES.md` の記述を実態に合わせる別 issue が行う。その issue は「optional を実行するようになった場合はその時点で更新する」としており、この issue が実行範囲を確定して引き渡す。この issue はテストモジュールのコメントに実行範囲を書く
- 実行範囲を広げると `setup/0` が実行する件数が 702 から 789（702 + 87）に変わる。ハーネスの 0 件検査を扱う別 issue の「正しいチェックアウトでは従来どおり 702 件が実行される」はこの issue の実装で陳腐化するため、その issue 側で件数を読み替える

## 完了条件

- `optional/` の実行対象がコード上の一覧として明示され、トップレベル 702 件と optional 87 件が `make test`（`rebar3 as test eunit`）で実行される
- 実行対象のすべてのケースが通る。`float-overflow` の 1 件は `multipleOf` の小数判定の修正後、`format/email` 3 件と `format/ipv6` 1 件は format の方針の修正後に通る
- 除外した 5 ファイル（未対応 4 format と `ecmascript-regex`）とその理由、実行対象が 87 件であることがテストモジュールのコメントに書かれている
- `./rebar3 xref` / `./rebar3 dialyzer` / `./rebar3 as test eunit` / `./rebar3 as test proper` が通る

## 解決方法

{未着手}
