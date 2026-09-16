# コメント中の仕様引用の誤りを修正する

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/refactor-fix-spec-citations
- Polished: {YYYY-MM-DD}

## 目的

ソースコメントが参照している仕様文書と節番号を正し、実装の根拠を一次資料から辿れるようにする。

## 現状

`src/jsone_schema_validator.erl` のモジュールヘッダは core として `draft-wright-json-schema-00` を引用しているが、この版の識別子キーワードは `id` であり、実装が使う `$id` は `draft-wright-json-schema-01`（draft 6）で導入されたもの。validation 側は `draft-wright-json-schema-validation-01` を引用しており、core と validation で版がずれている。

同ファイルのキーワードごとのコメントにもずれがある。

- `type` を `5.5.2.` としている（draft-06 validation では 6.25）
- `items / additionalItems` を `6.10.` としている（`items` は 6.9、`additionalItems` が 6.10）
- `format` を `6.6.` としている（`format` は 8、6.6 は `maxLength`。同じファイル内の `maxLength` のコメントも 6.6 を使っており矛盾している）

参照先:

- https://datatracker.ietf.org/doc/html/draft-wright-json-schema-01
- https://datatracker.ietf.org/doc/html/draft-wright-json-schema-validation-01

## 設計方針

- 引用する文書名と節番号を draft 6 の正式版に合わせる
- モジュールヘッダには core と validation の両方を明記する
- 引用の正誤を確認する手段（`refs/` の整備や確認手順）を用意するかは別途判断する

## 完了条件

- `rg` で洗い出した全節番号が上記 2 文書の目次と一致する
- コメントの修正のみでコードの挙動が変わっていない（差分がコメント行に限られている）

## 解決方法

{未着手}
