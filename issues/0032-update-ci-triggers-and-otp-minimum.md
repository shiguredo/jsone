# CI のトリガーと権限を整備し、動作要件を Erlang/OTP 29.1 以上に揃える

- Created: 2026-11-14
- Completed: {YYYY-MM-DD}
- Branch: feature/update-ci-triggers-and-otp-minimum
- Polished: {YYYY-MM-DD}

## 目的

PR でも CI を実行して変更を検証できるようにし、workflow の権限と同時実行を絞る。あわせて、`rebar.config` の `minimum_otp_vsn` を CI で検証している Erlang/OTP に揃え、README に動作要件を記載して宣言と実態を一致させる。

## 現状

- `.github/workflows/ci.yml` のトリガーは `push` のみで、`pull_request` がない。
- workflow レベルの `permissions` と `concurrency` がない。
- `shiguredo/setup-erlang` の `otp-version` は `29.1.2`。
- `rebar.config` の `minimum_otp_vsn` は `28.1`。
- README に動作環境の記載がない。
- `shiguredo/setup-erlang` と `shiguredo/github-actions` は `@main` を参照している。時雨堂の action は `main` を参照して使う運用のため、変更しない。

## 設計方針

- `pull_request` トリガーを追加し、`push` と同じ `paths-ignore` を適用する。
- workflow に `permissions: contents: read` を設定する。Slack 通知ジョブには `actions: read` だけをジョブ単位で与える。
- 同一 ref の重複実行を抑える `concurrency` を追加する。
- `rebar.config` の `minimum_otp_vsn` を `29.1` に上げる。
- CI は `shiguredo/setup-erlang` で `otp-version: "29.1.2"` を指定する。matrix は使わない。
- README に動作環境 (Erlang/OTP 29.1 以上) を記載する。

## 完了条件

- `pull_request` で CI が実行される。
- workflow に `permissions: contents: read` と `concurrency` が設定されている。
- `rebar.config` の `minimum_otp_vsn` が `29.1` になっている。
- CI が Erlang/OTP 29.1.2 で `make compile dialyzer test proper` を通す。
- README の動作要件が `minimum_otp_vsn` と一致している。
- CI の YAML が構文的に正しく、prek の check-yaml が通る。
- PR で CI が走ることを確認する。

## 解決方法

{未着手}
