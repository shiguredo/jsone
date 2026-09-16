# JSON-Schema-Test-Suite の optional ケースを実行する

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/add-optional-suite-execution
- Polished: {YYYY-MM-DD}

## 目的

draft 6 の必須ケース 702 件は実行されているが `tests/draft6/optional/` の 194 件が実行されておらず、format や数値の境界の欠陥が検出できていない。実行範囲を広げる、または対象外とする理由を明示する。

## 現状

`test/jsone_schema_draft6_tests.erl` の `draft6_test_/0` は `filelib:wildcard(filename:join(SuiteDir, "*.json"))` で `tests/draft6/*.json` のみを読み込む。`tests/draft6/optional/` は実行されない。

件数の実測値:

- トップレベル `tests/draft6/*.json`: 702 件（実行されている）
- `tests/draft6/optional/**`: 194 件（実行されていない）

未実行のケースには次が含まれる。

- `optional/float-overflow.json`（`multipleOf` のオーバーフロー）
- `optional/bignum.json`（巨大整数）
- `optional/format/` の 9 ファイル（date-time / email / hostname / ipv4 / ipv6 / json-pointer / uri / uri-reference / uri-template）
- `optional/ecmascript-regex.json` / `optional/non-bmp-regex.json`（ECMA 262 と Erlang の正規表現差）

手元で optional を実行すると約 42 件が失敗する（hostname 9 / json-pointer 12 / uri 7 / email 3 / uri-template 1 / ipv6 1 / ecmascript-regex 9 / float-overflow 1）。

README と CHANGES.md は「draft 6 の全テストケースを実行する」と記載しており実態と一致しない。

## 設計方針

- `optional/` のうち実行すべき範囲を決める。少なくとも `float-overflow` と `bignum` は実装の正しさに直結するため実行対象にする
- ECMA 262 と Erlang の正規表現差のように仕様上許容するものは、除外理由をテストモジュールのコメントに明記して除外する
- 失敗するケースが残る場合は、既知の制約として別 issue で扱う。この issue はテストの実行範囲に限定する

## 完了条件

- optional の実行範囲がコード上で明示され、実行されるケースが CI で走る
- README / CHANGES.md の記述が実行範囲と一致する
- 実行対象から除外したケースには除外理由がコメントで書かれている

## 解決方法

{未着手}
