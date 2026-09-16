# format 検証の対応範囲と無効化手段を整理する

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/change-format-validation-policy
- Polished: {YYYY-MM-DD}

## 目的

`format` を指定した利用者が「検証された」と誤解する状態を解消する。コードレビューで重要と判断した項目。

仕様は無効化オプションの提供を SHOULD としており、未対応の format を黙って有効扱いする現状は検証漏れに気づけない失敗モードになっている。

## 現状

- `src/jsone_schema_validator.erl` の `check_format/3` は `date-time` / `email` / `ipv4` / `ipv6` / `uri-reference` の 5 つだけを検証し、それ以外は常に有効として扱う。`hostname` / `uri` / `uri-template` / `json-pointer` は未実装
- 無効化オプションが無い。`jsone_schema:options()` にあるのは `max_errors` / `schema_loader` / `schemas` / `parser_fun` / `recursive` のみ
- 実装済みの format も仕様より緩い。`email` は `^[^@]+@[^@]+$` で `a..b@example.com` のような不正な形を通す。`date-time` は `calendar:rfc3339_to_system_time/1` 依存のため区切り文字が任意で、`2018-02-01X15:18:02Z` や空白区切りも valid になる（実測）
- テストスイートの `tests/draft6/optional/format/` を手元で実行すると、hostname 9 件、json-pointer 12 件、uri 7 件、email 3 件、uri-template 1 件、ipv6 1 件が失敗する（`optional/` は実行対象外のため気づかれていない）

仕様の根拠: draft-06 validation §8.2「Implementations MAY support the "format" keyword. Should they choose to do so: they SHOULD implement validation for attributes defined below; they SHOULD offer an option to disable validation for this keyword.」および §8.3 の定義済み format 一覧。

## 設計方針

- `validate_format => boolean()`（既定 true）のような無効化オプションを追加する
- 対応する format の一覧を決め、未対応の format をどう扱うか（無視する / schema エラーにする）を決めて README と `check_format/3` のコメントに明記する
- `email` / `date-time` は「簡易チェックである」ことを明記するか、判定を RFC に寄せる
- 仕様が必須としていない format まで実装する必要はない。実装しないものを「未対応」と明示することを優先する

## 完了条件

- format 検証を無効化でき、その挙動がテストで担保されている
- 対応 / 未対応 format の一覧と既定の挙動が README に記載されている
- 未対応 format を指定したときに、無効化オプションの有無にかかわらず何が起きるかがドキュメントから読み取れる

## 解決方法

{未着手}
