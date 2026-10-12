# 分岐内の schema エラーが分岐の失敗として扱われ検証が成功する

- Created: 2026-09-18
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-combinator-schema-error-swallowed
- Polished: 2026-10-12

## 目的

`not` / `anyOf` / `oneOf` / `allOf` / `contains` の分岐内で起きた schema エラーが「分岐の失敗」に変換されるため、スキーマの誤りが検証成功になったり、data エラーに入れ子で埋もれたりする問題を修正する。スキーマは信頼できない第三者から渡り得るため、誤ったスキーマが「どのデータでも成功する」状態になるのは望ましくない。

## 現状

`src/jsone_schema_validator.erl` の `run_subschema/3` は、サブスキーマの検証で積まれたエラーを分岐の成否として `{error, Errors, State}` で返す経路が 2 つある。`check_all_of_1/3` / `check_any_of_1/4` / `check_one_of_1/5` / `check_not/3` はこれを分岐の失敗として扱うため、エラーの `kind` が `schema` のもの（スキーマ自体の誤り）も分岐の失敗になる。

- 通常終了側: 検証が最後まで進み、`jsone_schema_state:get_errors/1` にエラーが残っている場合
- `catch throw:{?ERRORS, Errors}` 節: `jsone_schema_error:add_reason/2` が `max_errors` の上限に達して検証を打ち切った場合

なお、`throw` を捕捉する側の `run_subschema/3` の通常終了側の本体から投げた `throw` は、同じ `try` の `catch` 節では捕捉されない。そのため `catch` 節で schema エラーを投げ直すと、`run_subschema/3` を通り抜けて公開 API まで伝播する。

実測（`jsone_schema:validate/2`、OTP 29 / erts-17.1、ローカル）:

- `{"not": {"$ref": 5}}` に `42` を渡すと `{ok, 42}` になる。`$ref` が文字列でない schema エラーが分岐の失敗になり、`not` が成功する
- `{"not": {"type": 5}}` に `1` を渡すと `{ok, 1}` になる。同じ経路で、非文字列 `$ref` の修正前から存在する
- `{"oneOf": [{"$ref": 5}, {"type": "integer"}]}` に `1` を渡すと `{ok, 1}` になる
- `{"anyOf": [{"$ref": 5}]}` に `1` を渡すと `{any_schemas_not_valid, [#{kind => schema, error => schema_invalid}]}` の data エラーになる（分岐が失敗する側に倒れる）
- `{"allOf": [{"$ref": 5}]}` に `1` を渡すと `{all_schemas_not_valid, [#{kind => schema, error => schema_invalid}]}` の data エラーになり、schema エラーが data エラーに入れ子で現れる
- `{"contains": {"$ref": 5}}` に `[1]` を渡すと `no_contains_match` の data エラーになる。`check_contains/3` は `subschema_valid/3` を通して `run_subschema/3` の結果を boolean に潰すため、schema エラーが `subschema_valid/3` の `false` に変換され、分岐の失敗として扱われる
- 分岐を経由しない位置（`{"properties": {"a": {"$ref": 5}}}` に `{"a": 1}`）ではトップレベルに `{error, [#{kind => schema, error => schema_invalid}]}` が返る

## 設計方針

- `run_subschema/3` の `catch throw:{?ERRORS, Errors}` 節と通常終了側の両方で、エラーリストに `kind => schema` のものが含まれるかどうかを判定する。含まれる場合は `{error, Errors, State}` を返す経路に載せず、`max_errors` の設定にかかわらず schema エラーを検証全体のエラーとして伝播させる
- 伝播の形は、`run_subschema/3` の内側から `jsone_schema_error:add_reason/2` と同じ `{?ERRORS, Errors}` の throw を投げ直し、`jsone_schema:do_validate/4` の `catch throw:{?ERRORS, ThrownErrors}` で `{error, Errors}` に変換する。`do_validate/4` の捕捉は現行の 2 要素のまま変えない
- エラーリストに schema エラーが含まれるかの判定は `jsone_schema_error` の共通関数（`has_schema_error/1` など）に置き、`catch` 節と通常終了側の両方から使う。どちらの経路が必要かは `max_errors` の値で変わる
  - 既定の `max_errors` = 1 では `jsone_schema_error:add_reason/2` が最初の 1 件で必ず `throw` するため `catch` 節が常態になる
  - `max_errors` を 2 以上にした場合は `catch` 節を通らず通常終了側に到達するため、通常終了側の判定が要る
- `jsone_schema_error:abort/2` と `?REF_ABORT` は使わない。あれは `$ref` の循環と解決スタックの深さ上限のように、それまでに集めたエラーを捨てて打ち切りの理由だけを返す経路であり、`max_errors` の上限判定を通した schema エラーの伝播とは目的が別のため
- `check_all_of_1/3` / `check_any_of_1/4` / `check_one_of_1/5` / `check_not/3` / `check_contains/3` の分岐判定は変えない。`run_subschema/3` が schema エラーを返さなくなるため、分岐の失敗は data エラーのときだけになる。`check_contains/3` が呼ぶ `subschema_valid/3` も同様に boolean へ潰す先が data エラーのときだけになる
- エラー形が変わる。`allOf` / `anyOf` / `oneOf` / `contains` の下に埋め込まれていた schema エラーが、トップレベルの `{error, [#{kind := schema, ...}]}` になる。`test/jsone_schema_tests.erl` の `anchor_cross_ref_test/0` が固定している「未登録のアンカーへの参照が `all_schemas_not_valid` に入れ子になる」形は、`schema_not_found` が schema エラーなのでトップレベルに変わる
- 実装順について 0019 は待たない。現行の `{?ERRORS, Errors}` の 2 要素の形のまま実装し、0019 が `{?ERRORS, Errors, State}` に変えた時点で 0019 が形を揃える。この issue が変更するのは `src/jsone_schema_validator.erl` の `run_subschema/3` の kind 判定と `jsone_schema_error` の判定関数だけで、`jsone_schema:do_validate/4` の捕捉は触らない。0019 が `run_subschema/3` の throw の形を `{?ERRORS, Errors, State}` に変え、`schemas` と `index` の引き継ぎを扱うが、この issue はエラーの `kind` による判定だけを扱い、throw の形と状態の引き継ぎは 0019 の定めに従う
- 0019 は `add_reason/2` が投げる箇所と `run_subschema/3` と `jsone_schema:do_validate/4` の捕捉の 3 箇所を挙げているが、この issue は再 throw を 2 箇所（`run_subschema/3` の `catch` 節と通常終了側）新設するため、0019 の 3 要素化のときにこの 2 箇所も合わせて書き換える。0019 の実装時にこの issue の再 throw を洗い出し、形を揃えること。片方だけ 3 要素化すると `do_validate/4` の捕捉に一致せず未捕捉 throw で呼び出し元プロセスが落ちる
- 0016 が `run_subschema/3` と `resolve_ref/2` の整理を扱う場合も、この判定は変えない
- `test/JSON-Schema-Test-Suite/tests/draft6` の 702 ケースは無効データに `?assertMatch({error, _}, ...)` を使うため影響しない見込みだが、実行して確認する
- `CHANGES.md` に独立したエントリは追加しない。JSON Schema バリデータは `## develop` の未リリース `[ADD]` の中にあり、この変更はその初回リリース内容に含まれる

## 完了条件

- `{"not": {"$ref": 5}}` がどのデータでも `{error, [#{kind := schema, error := schema_invalid}]}` になる（`{ok, _}` にならない）
- `{"not": {"type": 5}}` は `{error, [#{kind := schema, error := wrong_type_specification}]}` になる（`{"not": {"$ref": 5}}` の error の値 `schema_invalid` とは異なる。「$ref の場合と同じ schema エラー」を error の値まで同一と読むと回帰テストが落ちる）
- `{"oneOf": [{"$ref": 5}, {"type": "integer"}]}` / `{"anyOf": [{"$ref": 5}]}` / `{"allOf": [{"$ref": 5}]}` もトップレベルの schema エラーになる（`all_schemas_not_valid` / `any_schemas_not_valid` などの data エラーに入れ子にならない）
- `{"contains": {"$ref": 5}}` に `[1]` を渡すとトップレベルの schema エラーになる（`no_contains_match` の data エラーにならない）
- 分岐内の data エラーは現行どおり分岐の失敗として扱われる（`{"not": {"type": "string"}}` に `1` を渡すと `{ok, 1}`、`"a"` を渡すと `not_schema_valid` の data エラー）
- `max_errors` を 2 以上にした場合も同じ結果になる（`catch` 節を通らず通常終了側で判定される経路を確認する）
- `test/jsone_schema_tests.erl` に回帰テストを追加し、`anchor_cross_ref_test/0` の入れ子形の期待を新しい形に更新する
- `test/JSON-Schema-Test-Suite/tests/draft6` の 702 ケースと `test/prop_jsone_schema.erl` の全プロパティが引き続き通る
- `./rebar3 xref` / `./rebar3 dialyzer` / `./rebar3 as test eunit` / `./rebar3 as test proper` が通る

## 解決方法

{未着手}
