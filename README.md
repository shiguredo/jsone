# shiguredo_jsone

![GitHub Actions workflow](https://github.com/shiguredo/jsone/actions/workflows/ci.yml/badge.svg)
[![GitHub tag](https://img.shields.io/github/tag/shiguredo/jsone.svg)](https://github.com/shiguredo/jsone)
[![hex.pm version](https://img.shields.io/hexpm/v/shiguredo_jsone.svg)](https://hex.pm/packages/shiguredo_jsone)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

## 概要

[Erlang/OTP の標準 JSON ライブラリ](https://www.erlang.org/doc/apps/stdlib/json.html) を [sile/jsone: Erlang JSON library](https://github.com/sile/jsone) 互換にするラッパーです。

> [!CAUTION]
> jsone 一部の機能のみを実装しています。

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
