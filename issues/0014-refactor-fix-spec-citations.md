# コメント中の仕様引用の誤りを修正する

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/refactor-fix-spec-citations
- Polished: 2026-09-18

## 目的

ソースコメントが参照している仕様文書と節番号を正し、実装の根拠を一次資料から辿れるようにする。

## 現状

`src/jsone_schema_validator.erl` のモジュールヘッダは core として `draft-wright-json-schema-00` を引用している。この版の識別子キーワードは `id`（§8.2）であり、実装が使う `$id` を定義していない。`$id` を定義するのは `draft-wright-json-schema-01`（draft 6）の §9.2 で、実装側も `jsone_schema.hrl` の `?ID` に `$id` を、`?ID_OLD` に `id` を持ち、`id` は `wrong_draft6_id_tag` として弾いている。モジュールヘッダが core-00 を指しているため、実装の根拠を一次資料から辿れない。

なお `draft-wright-json-schema-validation-01` の §10.1 の規範参照は core-00 であり、現行ヘッダの組み合わせはこの参照を写したものになっている。

同ファイルのキーワードごとのコメントにもずれがある（引用は `src/jsone_schema_validator.erl` の 22 行のみで、他の `src/*.erl` と `test/*.erl` に節番号の引用は無い）。

- `type` を `5.5.2.` としている（validation-01 に §5.5 は無く、`type` は 6.25）
- `items / additionalItems` を `6.10.` としている（`items` は 6.9、`additionalItems` が 6.10。`items` の節番号が欠けている）
- `format` を `6.6.` としている（`format` は 8。6.6 は `maxLength` の節番号で、`6.6` は `minLength` / `maxLength` のコメント行にも正しく使われており、`format` の行だけが誤り）

残る 19 行の節番号（`6.1`〜`6.29`）は core-01 と validation-01 の目次と一致している。

参照先:

- https://datatracker.ietf.org/doc/html/draft-wright-json-schema-01
- https://datatracker.ietf.org/doc/html/draft-wright-json-schema-validation-01

## 設計方針

- モジュールヘッダは core-01（`$id` を定義する版）と validation-01 を引用する。あわせて、validation-01 §10.1 の規範参照が core-00 であること、`id` を `wrong_draft6_id_tag` として弾く検査の根拠が core-00 §8.2 であることをコメントに書く。core-00 の引用を消すと `id` を弾く根拠がソースから失われるため、根拠は残す
- キーワードごとのコメントの節番号を core-01 / validation-01 の目次に合わせる。上記 3 件を直し、他の 19 行は変更しない
- `refs/` は整備しない。引用の確認はこの issue に挙げた datatracker の URL で行う
- 差分はコメント行だけにする。コードの挙動を変えない

## 完了条件

- モジュールヘッダが core-01 と validation-01 の両方を引用し、validation-01 の規範参照が core-00 であることと `id` を弾く根拠（core-00 §8.2）がコメントに書かれている
- `rg` で洗い出した `src/jsone_schema_validator.erl` の節番号（22 行）が core-01 と validation-01 の目次と一致する。`type` は 6.25、`items` は 6.9、`additionalItems` は 6.10、`format` は 8 になっている
- `git diff -U0 -- src/` の追加行と削除行がすべて `%` で始まっている（コメント行の変更に限られている）

## 解決方法

{未着手}
