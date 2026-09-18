# format 検証の対応範囲と無効化手段を整理する

- Created: 2026-09-16
- Completed: 2026-09-18
- Branch: feature/change-format-validation-policy
- Polished: 2026-09-18

## 目的

`format` を指定した利用者が「検証された」と誤解する状態を解消する。

仕様は無効化オプションの提供を SHOULD としており、未対応の format を黙って有効扱いする現状は検証漏れに気づけない失敗モードになっている。

## 現状

- `src/jsone_schema_validator.erl` の `check_format/3` は `date-time` / `email` / `ipv4` / `ipv6` / `uri-reference` の 5 つだけを検証し、それ以外は常に有効として扱う。`hostname` / `uri` / `uri-template` / `json-pointer` は未実装
- 無効化オプションが無い。`jsone_schema:options()` にあるのは `max_errors` / `schema_loader` / `schemas` / `parser_fun` / `recursive` のみ
- 実装済みの format も仕様より緩い。`email` は `^[^@]+@[^@]+$` で `a..b@example.com` のような不正な形を通す。`date-time` は `calendar:rfc3339_to_system_time/1` 依存のため区切り文字が任意で、`2018-02-01X15:18:02Z` や空白区切りも valid になる。`ipv6` は `inet_parse:ipv6strict_address/1` が zone ID 付きの `fe80::a%eth1` を受理する（いずれも実測）
- テストスイートの `tests/draft6/optional/format/` を手元で実行すると、hostname 9 件、json-pointer 12 件、uri 7 件、email 3 件、uri-template 1 件、ipv6 1 件の 33 件が失敗する（`optional/` は実行対象外のため気づかれていない）
- README に format の記述は無く、対応している format も未対応の format も利用者からは分からない

仕様の根拠: draft-06 validation §8.2「Implementations MAY support the "format" keyword. Should they choose to do so: they SHOULD implement validation for attributes defined below; they SHOULD offer an option to disable validation for this keyword.」および「Implementations MAY add custom format attributes. Save for agreement between parties, schema authors SHALL NOT expect a peer implementation to support this keyword and/or custom format attributes.」、§8.3 の定義済み format 一覧（date-time / email / hostname / ipv4 / ipv6 / uri / uri-reference / uri-template / json-pointer の 9 つ）。

## 設計方針

- `validate_format => boolean()`（既定 `true`）を無効化オプションとして追加する。`validate/2,3` と `validate_key/2,3` だけが受け付け、`add_schema/3` と `load_schemas/2` は受け付けない。オプションの許可キーを API ごとに検査する別 issue の一覧にも、`validate/2,3` と `validate_key/2,3` のキーとして `validate_format` を足し、`add_schema/3` と `load_schemas/2` のキーには足さない
- フラグは `jsone_schema_state` の state に持たせ、`new/3` で読む。`check_format/3` は `Options` ではなく state を受け取るため、state 経由で参照する
- 対応する format は現行の 5 つ（`date-time` / `email` / `ipv4` / `ipv6` / `uri-reference`）とし、新規実装は行わない
- 未対応の 4 つ（`hostname` / `uri` / `uri-template` / `json-pointer`）は常に有効のままとする。§8.2 は custom format の追加を MAY としており、未対応の format を schema エラーにすると仕様上妥当なスキーマを拒否することになる。README に未対応と明記し、検証されないことを利用者が把握できるようにする
- 対応済みの 5 つは `optional/format` の該当ファイルをすべて通す。`email` は dot-atom の規則（先頭・末尾・連続するドットの禁止）を追加し、`ipv6` は zone ID（`%`）を拒否し、`date-time` は区切り文字を `T` / `t` に限定する
- 完全な RFC 準拠は目指さず、簡易チェックであることを README と `check_format/3` のコメントに明記する。`validate_format => false` は対応済み format の検証だけを止め、未対応 format の扱いは変えない
- `optional/` の実行範囲は別 issue の担当とする。`optional/format` を実行対象にする場合は、未対応 4 format のファイルを除外理由付きで対象外にするか、実装を別 issue にする

## 完了条件

- 既定（`validate_format` を指定しない）で、`#{<<"format">> => <<"email">>}` に `<<"a..b@example.com">>` を与えると `wrong_format` の data エラーになり、`#{<<"format">> => <<"date-time">>}` に `<<"2018-02-01X15:18:02Z">>` を与えると data エラーになり、`#{<<"format">> => <<"ipv6">>}` に `<<"fe80::a%eth1">>` を与えると data エラーになる
- `validate_format => false` では、対応済み format の違反が data エラーにならない（上記 3 つが `{ok, ...}` になる）
- `validate/2,3` と `validate_key/2,3` が `validate_format` を受け付ける。`add_schema/3` と `load_schemas/2` の許可キーには含めない（許可キーの検査が入った後は `erlang:error(badarg, ...)` になる。`?assertError(badarg, ...)` で検査する）
- 未対応の 4 format は `validate_format` の値にかかわらず常に有効で、schema エラーにもならない（`test/jsone_schema_tests.erl` の `format_test/0` にある「未対応の format は常に有効とする」のアサーションがそのまま通る）
- 対応済み 5 format が `optional/format` の該当ファイル（date-time / email / ipv4 / ipv6 / uri-reference）をすべて通る。失敗していた email 3 件と ipv6 1 件、および `date-time` の区切り文字の回帰テストが `test/jsone_schema_tests.erl` に追加されている
- README に対応 / 未対応の format 一覧、既定の挙動、`validate_format => false` の効果、簡易チェックである旨が書かれている
- `./rebar3 xref` / `./rebar3 dialyzer` / `./rebar3 as test eunit` / `./rebar3 as test proper` が通る

## 解決方法

`format` の対応範囲と無効化オプションを整理した。

- `validate_format => boolean()`（既定 `true`）を追加した。`jsone_schema_state` の状態に持たせ、`check_format/3` が `get_validate_format/1` で参照する。`false` の場合は対応済み format の検証を止める
- オプションの許可キーは `validate/2,3` と `validate_key/2,3` にだけ `validate_format` を足し、`add_schema/3` と `load_schemas/2` では `erlang:error(badarg, ...)` になる
- 対応する format は現行の 5 つ（`date-time` / `email` / `ipv4` / `ipv6` / `uri-reference`）のままとし、新規実装は行っていない。未対応の 4 つ（`hostname` / `uri` / `uri-template` / `json-pointer`）は `validate_format` の値にかかわらず常に有効とする
- 実装済みの format を厳しくした。`email` はローカル部に dot-atom の規則（先頭・末尾・連続するドットの禁止）を追加し、`date-time` は区切り文字を `T` / `t` に限定してから `calendar:rfc3339_to_system_time/1` で日付と時刻を確認し、`ipv6` は `%` を含む値を拒否する
- README に format の節を追加し、対応 / 未対応の一覧、既定の挙動、`validate_format => false` の効果、簡易チェックであることを書いた
- `test/jsone_schema_tests.erl` の `format_test/0` に対応済み 5 format の回帰テストと `validate_format` の検査を追加し、`option_validation_test/0` に許可キーと値の型の検査を追加した

`test/JSON-Schema-Test-Suite/tests/draft6/optional/format` の該当 5 ファイル（date-time 12 件 / email 9 件 / ipv4 8 件 / ipv6 29 件 / uri-reference 7 件）がすべて通ることを確認した。

`./rebar3 xref` / `./rebar3 dialyzer` / `./rebar3 as test eunit` / `./rebar3 as test proper` が通り、eunit は 752 件、PropEr は 15 件すべて通過した。
