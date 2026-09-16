# README と CHANGES.md の記述を実態に合わせる

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-readme-changes-alignment
- Polished: {YYYY-MM-DD}

## 目的

ドキュメントの記述が実装の実態と食い違っている箇所を直す。

## 現状

- `README.md` は「JSON-Schema-Test-Suite の draft 6 テストを `test/jsone_schema_draft6_tests.erl` で全ケース実行しています」と書くが、実行されるのは `tests/draft6/*.json` の 702 件のみで、`tests/draft6/optional/` の 194 件は実行されない
- `CHANGES.md` の `## develop` に `[FIX] 正規表現が不正な pattern / patternProperties でクラッシュするのを修正する` があるが、この不具合はこのブランチの途中のコミットで入ったもので、派生元ブランチには存在しない。`shiguredo-changelog` の規約は「変更履歴は派生元ブランチとの最終的な差分のみを記載する（開発ブランチ内の中間状態の修正は記載しない）」としている
- `README.md` に prek のセットアップ手順が無い。`make efmt-check` / `make elint-check` / `make fmt` は prek 経由になったため、prek のフック環境を用意していない環境では失敗する

## 設計方針

- README のテスト実行範囲の記述を実態（必須 702 件、optional は対象外）に合わせる。optional を実行するようになった場合はその時点で更新する
- `CHANGES.md` の `[FIX]` エントリを削除し、内容は `[ADD]` の説明に含める
- README の開発者向けセクションに `prek install --prepare-hooks` を追加する

## 完了条件

- README / `CHANGES.md` の記述が実装と一致している
- 新規に clone した環境で README の手順どおりにセットアップすれば `make efmt-check` / `make elint-check` が通る
- `CHANGES.md` のエントリの種別と順序が `shiguredo-changelog` の規約に沿っている

## 解決方法

{未着手}
