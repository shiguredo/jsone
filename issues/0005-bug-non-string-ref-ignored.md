# 文字列以外の $ref が無視されて常に検証成功になる

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-non-string-ref-ignored
- Polished: 2026-09-18

## 目的

`{"$ref": 5}` のように URI 参照でない `$ref` を書いたスキーマが、キーワードごと無視されて「どんなデータでも成功するスキーマ」になる問題を修正する。コードレビューで重要と判断した項目。

タイポが「検証していないのに成功する」失敗モードになる。他のキーワード（`required` / `pattern` / `properties` など）の型不正を schema エラーにしている方針とも不整合。

## 現状

`src/jsone_schema_validator.erl` の `check_value/3` は `#{?REF := Reference} when is_binary(Reference)` のガードを外れると `check_keywords/3` に落ち、`check_keyword_value/5` の catch-all 節が `$ref` を無視する（`id` を併記している場合は `id` の検査が先に働き `wrong_draft6_id_tag` になる。0004 を参照）。

再現（実測）:

```erlang
jsone_schema:validate(#{<<"$ref">> => 5}, <<"anything">>).
%% {ok, <<"anything">>}
```

仕様の根拠:

- draft-06 core §8「The value of the "$ref" property MUST be a URI Reference.」
- 同梱のメタスキーマ `test/meta-schemas/draft-06.json` の `"$ref": {"type": "string", "format": "uri-reference"}`

テストスイートに該当ケースが無いため 702 ケース通過では検出できない。

## 設計方針

- `?REF` が存在して binary でない場合は schema エラーを返す。専用の reason（例: `?wrong_ref_type`）を `jsone_schema.hrl` に追加する。`jsone_schema_error:error_info()` は atom を許容するため `jsone_schema_error:to_json/1` の変更は不要
- 検査は `check_value/3` の `id` 検査より後、`check_keywords/3` に落ちる経路（`check_keyword_value/5` に `?REF` の節を足す）に置く。`$ref` が非文字列で `id` を併記した場合は 0004 の決定どおり `wrong_draft6_id_tag` を優先し、この組み合わせの回帰テストを追加する
- 非文字列 `$ref` の schema エラーは `jsone_schema_error:add_reason/2` の通常経路で積まない。`jsone_schema_validator:run_subschema/3` が `throw:{?ERRORS, _}` を分岐失敗に変換するため、通常経路で積むと `{"not": {"$ref": 5}}` が `{ok, _}` に転じ、この issue が消そうとしている「検証していないのに成功する」失敗モードを別の形で作ってしまう。専用の throw タグで検証全体を打ち切り、`jsone_schema:do_validate/4` の catch に専用タグを追加して `{error, [_]}` に変換する（0001 が再入検出のために導入する専用タグと同じ仕組みを使う。0001 が未実装ならこの issue で導入する）
- 専用の throw タグは 0001 が再入検出のために導入するものと同一のタグを再利用し、`jsone_schema:do_validate/4` の catch 節も 1 つにまとめる（同じ目的のタグを 2 つ増やさない）。ペイロードは `jsone_schema_error:reason()` のリストとする
- `$ref` の値が URI 参照として妥当かどうかの検査は binary かどうかに限定する。文字列 `$ref` の構文検査は追加せず、解決失敗は現行どおり `jsone_schema_state:resolve_ref/2` の `schema_not_found` に委ねる
- 他のキーワード（`pattern` / `properties` / `required` / `id` など）の schema エラーが `not` / `anyOf` / `oneOf` の下で検証成功に転じる既存挙動（実測: `{"not": {"pattern": 5}}` も `{"not": {"$ref": 5, "id": "x"}}` も `{ok, _}`）はこの issue の対象外とし、別 issue で扱う
- `$id` の値が非文字列のときに無視される現行挙動もこの issue の対象外とする

## 完了条件

- `{"$ref": 5}` が `{error, [#{kind := schema, ...}]}` を返す
- `{"not": {"$ref": 5}}` が `{error, [#{kind := schema, error := ...}]}` を返す（現行は `not_schema_valid` の data エラーなので、data エラーではなく schema エラーになることを検査する）
- `{"anyOf": [{"$ref": 5}, {}]}` が `{ok, _}` ではなく `{error, [#{kind := schema, error := ...}]}` を返す
- `$ref` が非文字列で `id` を併記したスキーマが `wrong_draft6_id_tag` を返す（0004 の決定を維持）
- 回帰テストが `test/jsone_schema_tests.erl` に追加されている
- 正常な `$ref`（文字列）の挙動が変わっていない
- `test/JSON-Schema-Test-Suite/tests/draft6` の 702 ケースと `test/prop_jsone_schema.erl` の全プロパティが引き続き通る

## 解決方法

{未着手}
