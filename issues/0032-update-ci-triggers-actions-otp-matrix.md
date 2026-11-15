# CI のトリガー・action 固定・OTP matrix を整備する

- Created: 2026-11-14
- Completed: {YYYY-MM-DD}
- Branch: feature/update-ci-triggers-actions-otp-matrix
- Polished: {YYYY-MM-DD}

## 目的

`.github/workflows/ci.yml` は `push` のみで `pull_request` がなく、一部の action が `@main` の可変参照になっている。また `rebar.config` の `minimum_otp_vsn` は 28.1 だが CI は 29.1.2 のみで、サポート下限を検証していない。CI の再現性と PR 時の検証を確保する。

## 現状

- `.github/workflows/ci.yml` のトリガーは `on: push` のみで、`paths-ignore` は `LICENSE` と `**/*.md`。フォークからの PR ではチェックが走らない。
- `actions/checkout` と `j178/prek-action` はコミットハッシュ + バージョンコメントで固定されている。
- `shiguredo/setup-erlang@main` と `shiguredo/github-actions/.github/actions/slack-notify@main` は可変参照になっている。特に Slack 通知には `GH_TOKEN: ${{ github.token }}` が渡されている。
- `shiguredo/setup-erlang` の `otp-version` は `29.1.2` のみで、`rebar.config` の `minimum_otp_vsn` は `28.1`。
- workflow レベルの `permissions` と `concurrency` がない。
- `check` ジョブは `ubuntu-slim`、`ci` ジョブは `ubuntu-24.04` を使っており、runner の使い分け自体は妥当。

## 設計方針

- `pull_request` トリガーを追加し、`push` と同じ `paths-ignore` を適用する。フォーク PR でも CI が走るようにする。
- `shiguredo/setup-erlang` と `shiguredo/github-actions` をコミットハッシュで固定し、`# vX.Y.Z` のバージョンコメントを付ける。`@main` を残さない。
- OTP の matrix を追加し、`28.1` と `29.1.2`（または最新の 29.x）の両方でビルド・テストする。matrix 化が難しい場合でも、`minimum_otp_vsn` を 29 に上げる判断をして README に明記する。
- workflow に `permissions: contents: read` を設定する。Slack 通知ジョブに必要な権限だけをジョブ単位で付与する。
- 同一 ref の重複実行を抑える `concurrency` を追加する。
- action の SHA 固定は既存の `actions/checkout` / `j178/prek-action` の形式に揃える。

## 完了条件

- `pull_request` で CI が実行される。
- `@main` 参照の action がなくなり、すべてコミットハッシュ + バージョンコメントで固定されている。
- OTP 28.1 と 29.x の matrix で `make compile dialyzer test proper` が通る。matrix をやめる場合は `minimum_otp_vsn` と README が一致している。
- `permissions` と `concurrency` が設定されている。
- CI の YAML が構文的に正しく、prek の check-yaml が通る。
- 実際の PR で CI が走ることを確認する。

## 解決方法

{未着手}
