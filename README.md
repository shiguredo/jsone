# shiguredo_jsone

![GitHub Actions workflow](https://github.com/shiguredo/jsone/actions/workflows/ci.yml/badge.svg)
[![GitHub tag](https://img.shields.io/github/tag/shiguredo/jsone.svg)](https://github.com/shiguredo/jsone)
[![hex.pm version](https://img.shields.io/hexpm/v/shiguredo_jsone.svg)](https://hex.pm/packages/shiguredo_jsone)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

## 概要

[Erlang/OTP の標準 JSON ライブラリ](https://www.erlang.org/doc/apps/stdlib/json.html) を [sile/jsone: Erlang JSON library](https://github.com/sile/jsone) 互換にするラッパーです。

> [!CAUTION]
> jsone 一部の機能のみを実装しています。

JSON Schema draft 6 のバリデータも同梱しています。

## JSON Schema

`jsone:decode/1` が返す map をそのままスキーマとデータに使います。

```erlang
Schema = jsone:decode(<<"{\"type\":\"object\",\"properties\":{\"foo\":{\"type\":\"integer\"}}}">>),
{ok, Data} = jsone_schema:validate(Schema, jsone:decode(<<"{\"foo\":1}">>)).
```

キーを登録して検証する場合は `jsone_schema:add_schema/2,3` と `jsone_schema:validate_key/2,3` を使います。

```erlang
ok = jsone_schema:add_schema(<<"user">>, Schema),
{ok, Data} = jsone_schema:validate_key(<<"user">>, jsone:decode(<<"{\"foo\":1}">>)).
```

外部の `$ref` は `schemas` オプションか `schema_loader` オプションで解決します。

```erlang
Options = #{schemas => #{<<"https://example.com/user.json">> => UserSchema}},
{ok, Data} = jsone_schema:validate(Schema, Data, Options).
```

受け付けるオプションのキーは API ごとに異なります。不明なキーや受け付けない値は `erlang:error(badarg, ...)` になります。

- `jsone_schema:validate/2,3` と `jsone_schema:validate_key/2,3`: `max_errors` / `schema_loader` / `schemas`
- `jsone_schema:add_schema/2,3`: `parser_fun` (`add_schema/2` はオプション無し)
- `jsone_schema:load_schemas/1,2`: `parser_fun` / `recursive`

`max_errors` は正の整数か `infinity` だけを受け付けます。

スキーマのキーワード名とプロパティ名は binary である必要があります。`jsone:decode/2` の `{keys, attempt_atom}` でデコードしたスキーマは atom になったキーを含むため、`{error, [#{kind := schema, error := schema_invalid, ...}]}` になります（`$ref` を併記したスキーマは兄弟キーを評価しないため、この検査は働きません）。

対応しているのは draft 6 のみです。JSON-Schema-Test-Suite の draft 6 テストを `test/jsone_schema_draft6_tests.erl` で全ケース実行しています。

## rebar.conf

```erlang
{deps, [{jsone, "2024.1.0", {pkg, shiguredo_jsone}}]}.
```

## ライセンス

```text
Copyright 2024-2024, Takeru Ohta (Original Author)
Copyright 2024-2024, Shiguredo Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```
