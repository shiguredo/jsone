# `eqwalizer_support` が default 依存で hex publish できないのを修正する

- Created: 2026-11-14
- Completed: 2026-11-15
- Branch: feature/fix-hex-publish-eqwalizer-dependency
- Polished: {YYYY-MM-DD}

## 目的

`rebar.config` の default `deps` に git 依存の `eqwalizer_support` が含まれているため、`rebar3 hex build` / `rebar3 hex publish` が「Hex パッケージ以外の依存がある」として失敗する。リリースできない状態を解消する。

## 現状

`rebar.config` の `deps` に次が定義されている。

```erlang
{deps, [{eqwalizer_support,
         {git_subdir,
          "https://github.com/whatsapp/eqwalizer.git",
          {branch, "main"},
          "eqwalizer_support"}}]}.
```

`eqwalizer_support` は `test/prop_jsone_schema.erl` の `eqwalizer:dynamic_cast/1` で使われているだけで、ライブラリ本体の実行時には不要である。`rebar.lock` には `eqwalizer_support` の git_subdir が記録されている。

手元で次を実行すると失敗する。

```text
./rebar3 hex build -o /tmp/hex_out
===> Some dependencies were excluded from the package; only Hex packages can be dependencies: eqwalizer_support
```

`Makefile` の `publish` は `./rebar3 hex publish package` を実行するため、このままではリリースできない。

## 設計方針

- `eqwalizer_support` を default `deps` から外し、`test` または `elp` 専用のプロファイルへ移す。
- `eqwalizer:dynamic_cast/1` をテストから外す。テストの型付けが必要なら、ローカルの小さなヘルパー関数か `-spec` で表現し、テスト実行に ELP の shim を要求しない。
- ELP を CI で実行するかどうかを決める。実行する場合も `eqwalizer_support` は配布パッケージに含めない。
- `rebar.lock` を更新し、default 依存が Hex パッケージだけになることを確認する。
- `rebar3_hex` / `rebar3_proper` のプラグインのバージョン固定は別 issue とする。この issue では `eqwalizer_support` の扱いだけを扱う。

## 完了条件

- `rebar3 hex build` が成功し、`eqwalizer_support` が「Hex パッケージ以外の依存」として除外されない。
- default `deps` に git 依存が含まれない。
- `test/prop_jsone_schema.erl` が `eqwalizer:dynamic_cast/1` なしでコンパイル・実行できる。
- `rebar3 as test eunit` / `proper` が通り、ELP を使う場合も専用プロファイルで実行できる。
- README / CHANGES に依存の変更を記載する。

## 解決方法

- `rebar.config` の default `deps` から git 依存の `eqwalizer_support` を削除した
  - ELP は `eqwalizer_support` を同梱してプロジェクトへ自動で追加するため、依存に含める必要がない
  - default 依存がなくなったため `rebar.lock` も削除した
- `test/prop_jsone_schema.erl` から `eqwalizer:dynamic_cast/1` の呼び出しを削除した
  - PropEr が生成した値をそのまま `jsone_schema:validate/2` に渡す
  - ELP を使う場合も、ELP 同梱の `eqwalizer_support` で型検査できる
- `CHANGES.md` の develop に依存を外したことを追記した
- clean 状態から `make compile dialyzer test proper` と `rebar3 hex build` が通ることを確認した
