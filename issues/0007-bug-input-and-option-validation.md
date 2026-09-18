# 入力とオプションの検証漏れを塞ぐ

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-option-and-input-validation
- Polished: 2026-09-18

## 目的

`jsone_schema` の公開 API が、誤ったオプションや想定外のスキーマ表現を黙って受け入れて「検証したつもり」になる問題を修正する。

## 現状

- 不明なオプションを黙って無視する。`src/jsone_schema.erl` の `validate/3` / `validate_key/3` / `add_schema/3` / `load_schemas/2` は `maps:get(..., Options, Default)` で読むだけなので、`#{max_error => 3}` のようなタイポが素通りする（実測: `jsone_schema:validate(#{<<"type">> => <<"integer">>}, 1, #{max_error => 3})` は `{ok, 1}`）。既存の `src/jsone.erl` の `create_decoders/2` と `build_encode_options/2` は不明なオプションで `erlang:error(badarg, [Options, Acc])` を投げており方針が不揃い
- `jsone_schema:options()` 型は 4 つの API で共用されており、`validate/3` / `validate_key/3` では使わない `parser_fun` / `recursive` も許容している。実際に読むキーは API ごとに異なる
- `max_errors` の値を検査していない。`src/jsone_schema_state.erl` の `new/3` が `maps:get(max_errors, Options, 1)` でそのまま受け取るため `max_errors => 0` や `max_errors => all` が素通りする。素通り後の挙動は値ごとに異なり、`0` と `-1` は `has_reached_max_errors/1` が常に真になるため最初の 1 件で打ち切り、`all` や binary は `infinity` と同じく全件を集める
- `jsone:decode/2` の `{keys, attempt_atom}` でデコードしたスキーマを渡すと、atom になったキーワードは `check_keywords/3` の catch-all 節に落ちて無視される。`attempt_atom` は `binary_to_existing_atom/2` を使い、既存の atom にできないキーは binary のまま残すためキーは混在する（実測: `#{type => <<"integer">>, <<"minimum">> => 100}` に `1` を与えると `type` は無視され `minimum` だけが効いて `not_in_range`）。どのキーが無視されるかは VM の atom テーブルの状態に依存し、`$ref` / `$schema` は binary で残るため機能する
- `properties` と `dependencies`（object 形式）の map のキー（プロパティ名）が binary でない場合、インスタンスのキーは binary なので照合が空振りし、宣言したのに検証されない（実測: `#{<<"properties">> => #{a => #{<<"type">> => <<"string">>}}}` に `#{<<"a">> => 1}` を与えると `{ok, #{<<"a">> => 1}}`）。`patternProperties` の非 binary キーは `run_pattern/2` が `re:run/3` の `badarg` を捕まえて `{error, _}` を返すため、現行でも `schema_invalid` になる
- `add_schema/2`（オプション無し版）は atom キーのスキーマをそのまま登録でき、`validate_key/3` でも同じ問題が起きる（実測: `add_schema(<<"k">>, #{type => <<"integer">>})` のあと `validate_key(<<"k">>, <<"x">>)` は `{ok, <<"x">>}`）
- オプションに map 以外を渡すと `erlang:error(badarg, ...)` ではなく `function_clause` で落ちる（実測: `jsone_schema:validate(#{}, 1, [])`）

再現（実測）:

```erlang
%% {keys, attempt_atom} でデコードしたスキーマでは type が無視される
Schema = jsone:decode(<<"{\"type\": \"integer\"}">>, [{keys, attempt_atom}]).
%% #{type => <<"integer">>}
jsone_schema:validate(Schema, <<"not an integer">>).
%% {ok, <<"not an integer">>}
```

## 設計方針

- API ごとに受け付けるオプションのキーを検証し、不明なキーと不正な値は `erlang:error(badarg, ...)` にする（既存 `jsone` と同方針）。オプションが map でない場合も `function_clause` ではなく `badarg` にする
- 受け付けるキーは API ごとに次のとおりとし、オプション型も 3 つに分ける
  - `validate/2,3` と `validate_key/2,3`: `max_errors` / `schema_loader` / `schemas`
  - `add_schema/3`: `parser_fun`
  - `load_schemas/2`: `parser_fun` / `recursive`
- 検証は各公開 API の入口で行う。内部で組み立てて渡すオプション（`validate_key/3` が足す `schema_loader`、`load_schemas/2` が `add_schema/3` へ渡す `parser_fun`）は許可キーの範囲に収める。`load_schemas/2` はファイルを集める前に検証する
- `max_errors` は `pos_integer()` と `infinity` だけを受け付け、`0` / 負数 / `all` / binary / float は `badarg` にする
- スキーマとして評価する map のキー（キーワード名）が binary でない場合は schema エラー（`?schema_invalid`）にする。atom と binary が混在していても、非 binary のキーが 1 つでもあればエラーにする。無視されるキーの集合が VM の atom テーブルに依存するため、混在を許すと挙動が再現しなくなる
- `properties` / `patternProperties` / `dependencies` の map のキーが binary でない場合も同じく `?schema_invalid` にする。`patternProperties` は現行でも `schema_invalid` になるが、`properties` と `dependencies` は空振りするため、3 つとも同じ理由に揃える
- `definitions` / `$defs` は検証に使わないため対象外とする。`$ref` で参照された場合は参照先がスキーマとして評価されるため同じ検査を受ける
- 登録 (`add_schema/2,3`) ではスキーマのキーを検査しない。登録は成功し、`validate/3` / `validate_key/3` の検証時に schema エラーになる
- `format` の検証を切り替えるキーを追加する場合は、この許可キーの一覧に足す（`format` の方針そのものは別 issue）
- README に API ごとのオプションキーと、`{keys, attempt_atom}` でデコードしたスキーマを受け付けないことを書く。`shiguredo-erlang` の規約で `-doc` / `-moduledoc` は使わないため、型だけでは受け付けるキーを説明できない

## 完了条件

- `validate/3` / `validate_key/3` / `add_schema/3` / `load_schemas/2` に不明なキー（例: `max_error`）を渡すと `erlang:error(badarg, ...)` になる
- オプションが map でない場合も `badarg` になる
- API ごとの許可キーが効いている（`validate/3` と `validate_key/3` に `parser_fun` / `recursive`、`add_schema/3` に `recursive`、`load_schemas/2` に `max_errors` を渡すと `badarg` になる）
- `max_errors => 0` / `-1` / `all` / `<<"2">>` が `badarg` になり、`pos_integer()` と `infinity` は現行どおり動く
- `#{type => <<"integer">>}` のような atom キーのスキーマが `{error, [#{kind := schema, error := schema_invalid, ...}]}` を返す。binary キーが混在していても同じ
- `#{<<"properties">> => #{a => #{<<"type">> => <<"string">>}}}` のようにプロパティ名が binary でない場合も schema エラーになる
- `add_schema/2` で atom キーのスキーマを登録する操作自体は成功し、`validate_key/3` が schema エラーを返す
- 上記の回帰テストが `test/jsone_schema_tests.erl` に追加されている（`badarg` は `?assertError(badarg, ...)` で検査する）
- 受け付けるオプションが README に API ごとに書かれている
- `./rebar3 xref` / `./rebar3 dialyzer` / `./rebar3 as test eunit` / `./rebar3 as test proper` が通る

## 解決方法

{未着手}
