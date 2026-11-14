# README と CHANGES.md の記述を実態に合わせる

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/update-readme-changes-alignment
- Polished: 2026-09-18
- Refreshed: 2026-11-14

## 目的

ドキュメントの記述が実装の実態と食い違っている箇所を直す。

## 現状

- `README.md` は「JSON-Schema-Test-Suite の draft 6 テストを `test/jsone_schema_draft6_tests.erl` で全ケース実行しています」と書くが、実行されるのは `test/JSON-Schema-Test-Suite/tests/draft6/*.json` の 702 件のみで、`tests/draft6/optional/` の 194 件は実行されない。`CHANGES.md` の `### misc` にある `[ADD] JSON-Schema-Test-Suite をサブモジュールとして追加する` の「draft 6 の全テストケースを EUnit で実行する」も同じく実態と一致しない。702 件 / 194 件は 0010 の記録で、`test/JSON-Schema-Test-Suite` が未初期化のこの checkout では未実測
- `CHANGES.md` の `## develop` に `[FIX] 正規表現が不正な pattern / patternProperties でクラッシュするのを修正する` があるが、この不具合は `src/jsone_schema_validator.erl` の `run_pattern/2` に含まれていたもので、`e49b762`（PR #2 の squash）で `[ADD]` と `[FIX]` が同時に入った。`e49b762` の親にはバリデータが無い。元の feature ブランチの中間コミットは残っていないため、「最初から含まれていた」という中間状態は履歴から直接は確認できないが、リリース済みの版には存在しない。`shiguredo-changelog` は「変更履歴は派生元ブランチとの最終的な差分のみを記載する（開発ブランチ内の中間状態の修正は記載しない）」としている
- `README.md` に開発者向けのセクションが無く、prek の導入手順も `prek install --prepare-hooks` も書かれていない。`make efmt-check` / `make elint-check` / `make fmt` は prek 経由であり、prek 本体が無い環境では `prek: No such file or directory` で失敗する（実測）。`prek run` はフック環境を初回実行時に自動で用意するため、`prek install --prepare-hooks` を実行していないこと自体では失敗しない
- `README.md` にはテスト用サブモジュール `test/JSON-Schema-Test-Suite` の取得手順も無い

## 設計方針

- README と `CHANGES.md` のテスト実行範囲の記述から「全ケース」という断定を外し、実際の実行範囲（`tests/draft6/*.json` のトップレベル、`optional/` は実行しない）を書く。件数は書かない。`test/jsone_schema_draft6_tests.erl` のコメントは現在も「全ファイル」と誤っているため、コメントを正本にはせず、コメント自体の修正は 0010 に任せる
- `CHANGES.md` の `[FIX]` エントリを削除し、内容（正規表現が不正な `pattern` / `patternProperties` をスキーマのエラーとして扱い、`wrong_pattern` とメッセージ・位置を返す）を `[ADD] JSON Schema draft 6 のバリデータを追加する` の説明に含める
- README に開発者向けのセクションを新設し、prek 本体の導入手順（`j178/prek` のインストール方法）、`prek install --prepare-hooks`、`make efmt-check` / `make elint-check` の実行、テスト用サブモジュールの取得（`git submodule update --init --recursive`）を書く
- README / `CHANGES.md` の変更は `shiguredo-changelog` の「`.md` の変更は変更履歴に反映しない」に該当するため、`CHANGES.md` に新しいエントリは追加しない

## 完了条件

- `README.md` と `CHANGES.md` のテスト実行範囲の記述が「全ケース」を主張しておらず、`tests/draft6/*.json` のトップレベルを実行し `optional/` は実行しないことを示している。件数は書いていない
- `CHANGES.md` の `## develop` に `[FIX]` エントリが無く、不正な正規表現を `wrong_pattern` とメッセージ・位置を返すスキーマエラーとして扱うことが `[ADD] JSON Schema draft 6 のバリデータを追加する` の説明に含まれている
- `README.md` に開発者向けのセクションがあり、prek 本体の導入、`prek install --prepare-hooks`、`make efmt-check` / `make elint-check`、`git submodule update --init --recursive` が書かれている
- `CHANGES.md` のエントリの種別と順序が `shiguredo-changelog` の規約に沿っている（CHANGE → ADD → UPDATE → FIX）
- `./rebar3 xref` / `./rebar3 dialyzer` / `./rebar3 as test eunit` / `./rebar3 as test proper` が通る

## 解決方法

{未着手}
