# 分岐内の schema エラーが分岐の失敗として扱われ検証が成功する

- Created: 2026-09-18
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-combinator-schema-error-swallowed
- Polished: {YYYY-MM-DD}

## 目的

`not` / `anyOf` / `oneOf` / `allOf` の分岐内で起きた schema エラーが「分岐の失敗」に変換されるため、スキーマの誤りが検証成功として扱われる問題を修正する。スキーマは信頼できない第三者から渡り得るため、誤ったスキーマが「どのデータでも成功する」状態になるのは望ましくない。

## 現状

`src/jsone_schema_validator.erl` の `run_subschema/3` は、サブスキーマの検証で積まれたエラーを分岐の成否として `{error, Errors, State}` で返す。`check_all_of_1/5` / `check_any_of_1/5` / `check_one_of_1/5` / `check_not/3` はこれを分岐の失敗として扱うため、エラーの `kind` が `schema` のもの（スキーマ自体の誤り）も分岐の失敗になる。

実測（`jsone_schema:validate/2`）:

- `{"not": {"$ref": 5}}` に `42` を渡すと `{ok, 42}` になる。`$ref` が文字列でない schema エラーが分岐の失敗になり、`not` が成功する
- `{"not": {"type": 5}}` に `1` を渡すと `{ok, 1}` になる。同じ経路で、非文字列 `$ref` の修正前から存在する
- `{"oneOf": [{"$ref": 5}, {"type": "integer"}]}` に `1` を渡すと `{ok, 1}` になる
- `{"anyOf": [{"$ref": 5}]}` に `1` を渡すと `all_schemas_not_valid` の data エラーになる（分岐が失敗する側に倒れる）
- `{"allOf": [{"$ref": 5}]}` に `1` を渡すと `{all_schemas_not_valid, [#{kind => schema, error => schema_invalid}]}` の data エラーになり、schema エラーが data エラーに入れ子で現れる
- 分岐を経由しない位置（`{"properties": {"a": {"$ref": 5}}}` など）ではトップレベルに `{error, [#{kind => schema, error => schema_invalid}]}` が返る

## 設計方針

- `run_subschema/3` で、積まれたエラーに `kind => schema` のものが含まれる場合は分岐の失敗にせず、検証全体のエラーとして伝播させる。伝播は `jsone_schema_error:add_reason/2` が上限到達時に使う `?ERRORS` の throw と同じ経路に載せ、`jsone_schema:do_validate/4` で `{error, Errors}` に変換する
- `check_all_of_1/5` / `check_any_of_1/5` / `check_one_of_1/5` / `check_not/3` / `check_contains/3` の分岐判定は変えない。`run_subschema/3` が schema エラーを返さなくなるため、分岐の失敗は data エラーのときだけになる
- エラー形が変わる。`allOf` / `anyOf` / `oneOf` の下に埋め込まれていた schema エラーが、トップレベルの `{error, [#{kind := schema, ...}]}` になる。`test/jsone_schema_tests.erl` の `anchor_cross_ref_test/0` が固定している「未登録のアンカーへの参照が `all_schemas_not_valid` に入れ子になる」形は、`schema_not_found` が schema エラーなのでトップレベルに変わる
- 0019 が `run_subschema/3` の throw の形を `{?ERRORS, Errors, State}` に変え、`schemas` と `index` の引き継ぎを扱う。この issue はエラーの `kind` による判定だけを扱い、throw の形と状態の引き継ぎは 0019 の定めに従う。0016 が `run_subschema/3` と `resolve_ref/2` の整理を扱う場合も、この判定は変えない
- `test/JSON-Schema-Test-Suite/tests/draft6` の 702 ケースは無効データに `?assertMatch({error, _}, ...)` を使うため影響しない見込みだが、実行して確認する
- `CHANGES.md` に独立したエントリは追加しない。JSON Schema バリデータは `## develop` の未リリース `[ADD]` の中にあり、この変更はその初回リリース内容に含まれる

## 完了条件

- `{"not": {"$ref": 5}}` がどのデータでも `{error, [#{kind := schema, error := schema_invalid}]}` になる（`{ok, _}` にならない）
- `{"not": {"type": 5}}` も同じ schema エラーになる
- `{"oneOf": [{"$ref": 5}, {"type": "integer"}]}` / `{"anyOf": [{"$ref": 5}]}` / `{"allOf": [{"$ref": 5}]}` もトップレベルの schema エラーになる（`all_schemas_not_valid` などの data エラーに入れ子にならない）
- 分岐内の data エラーは現行どおり分岐の失敗として扱われる（`{"not": {"type": "string"}}` に `1` を渡すと `{ok, 1}`、`"a"` を渡すと `not_schema_valid` の data エラー）
- `test/jsone_schema_tests.erl` に回帰テストを追加し、`anchor_cross_ref_test/0` の入れ子形の期待を新しい形に更新する
- `test/JSON-Schema-Test-Suite/tests/draft6` の 702 ケースと `test/prop_jsone_schema.erl` の全プロパティが引き続き通る
- `./rebar3 xref` / `./rebar3 dialyzer` / `./rebar3 as test eunit` / `./rebar3 as test proper` が通る

## 解決方法

{未着手}
