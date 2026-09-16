# 変更履歴

- CHANGE
  - 下位互換のない変更
- ADD
  - 下位互換がある追加
- UPDATE
  - 下位互換がある変更
- FIX
  - バグ修正

## develop

- [ADD] JSON Schema draft 6 のバリデータを追加する
  - jesse の移植ではなく、jsone の map 表現に合わせた独自実装
  - `jsone_schema:validate/2,3` でスキーマを直接渡して検証できる
  - `jsone_schema:add_schema/2,3` と `jsone_schema:validate_key/2,3` でキーを指定して検証できる
  - データ表現は map のみで、proplist や mochijson2 / jiffy / jsx は受け付けない
  - スキーマは persistent_term に保存する
  - @voluntas
- [FIX] 正規表現が不正な pattern / patternProperties でクラッシュするのを修正する
  - スキーマのエラーとして扱い `schema_invalid` を返す
  - @voluntas

### misc

- [CHANGE] rebar3_efmt / rebar3_lint / elvis を削除し、shiguredo/erlang-pre-commit と prek で efmt / elint を実行する
  - `make efmt-check` / `make elint-check` を追加する
  - prek.toml を追加する
  - GitHub Actions では j178/prek-action で prek.toml のフックを実行する
  - @voluntas
- [ADD] JSON-Schema-Test-Suite をサブモジュールとして追加する
  - https://github.com/json-schema-org/JSON-Schema-Test-Suite を参照する
  - draft 6 の全テストケースを EUnit で実行する
  - @voluntas
- [UPDATE] GitHub Actions の Docker コンテナをやめて shiguredo/setup-erlang で Erlang/OTP 29.0.6 をセットアップする
  - @voluntas

## 2025.1.0

- [UPDATE] rebar3 を 3.25.1 に上げる
  - @voluntas
- [FIX] jsone.app.src の link を shiguredo/jsone に修正する
  - @voluntas

### misc

- [ADD] ELP 用に eqwalizer_support を依存に追加する
  - @voluntas
- [ADD] proper を test 用の依存に追加する
  - @voluntas
- [UPDATE] EUnit を test/ 以下に移動する
  - @voluntas
- [UPDATE] ubuntu-latest から ubuntu-24.04 に変更
  - @voluntas
- [UPDATE] rebar.config の minimum_otp_vsn を 28.1 に更新
  - @voluntas
- [UPDATE] GitHub Actions のコンテナイメージを OTP 28.1 / OpenSSL 3.6.0-beta1 に更新
  - @voluntas

## 2024.1.0

**初リリース**
