# jsone_schema_error:to_json が kind キーを持つデータのリストでクラッシュする

- Created: 2026-10-12
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-error-to-json-details-crash
- Polished: {YYYY-MM-DD}

## 目的

検証エラーを `jsone_schema_error:to_json/1` で JSON にするとき、エラー詳細に利用者の値が入る経路で `function_clause` になり、エラーの原因を報告できない。エラー詳細にどんな値が入っても `to_json/1` がクラッシュしないようにする。

## 現状

`src/jsone_schema_error.erl` の `details_to_json/1` は、詳細がリストのとき、全要素が `kind` キーを持つ map かどうかだけで「エラー理由のリスト」と判定する。一方 `reason_to_json/1` は `kind` が `data` か `schema` の場合にしか節が無い。このため `kind` キーを持つが値が `data` / `schema` でない map のリストを詳細に渡すと、判定と節が食い違い `function_clause` になる。

エラー詳細に利用者の値がそのまま入る経路は次の 3 つがある。

- `?not_unique` の詳細（`check_unique_items/2` が重複した要素を渡す）
- `?wrong_type_items` の詳細（`check_items/4` がキーワード値を渡す）
- `?schema_unsupported` の詳細（`schema_ver/1` が `$schema` の値を渡す）

`jsone:json_value()` の `json_object()` は `json_string()`（binary か atom）をキーに取るため、`kind` を atom キーに持つオブジェクトを含む配列はこの型に含まれる。`jsone:decode/2` の `{keys, attempt_atom}` を使った場合や、利用者が map を直接組み立てた場合に起こり得る。

実測（OTP 29 / erts-17.1、ローカル）:

```erlang
Schema = jsone:decode(<<"{\"uniqueItems\":true}">>),
Data = [[#{kind => foo}], [#{kind => foo}]],
{error, Errors} = jsone_schema:validate(Schema, Data),
jsone_schema_error:to_json(Errors).
%% error:function_clause
%%   jsone_schema_error:reason_to_json/1
%%   jsone_schema_error:details_to_json/1
```

同じ経路は `{?wrong_type_items, [#{kind => foo}]}` や `{?schema_unsupported, [#{kind => foo}]}` を直接詳細に渡しても再現する。`details_to_json/1` は `kind` キーの値が `data` / `schema` であることまで確認していない。

## 設計方針

- `reason_to_json/1` と `details_to_json/1` の reason 判定を、`jsone_schema_error:reason()` の型と一致する map だけを対象にするよう厳密化する。`kind` が `data` なら `path` / `schema` / `value` / `error`、`kind` が `schema` なら `schema` / `error` を必須キーとする
- reason の形でない map は reason として変換せず、利用者の値としてそのまま `jsone:encode/1` に渡す。`details_to_json/1` が「reason のリストか」と「それ以外の値」を確実に切り分け、`reason_to_json/1` の節に届かない形は変換しない
- 利用者の値は変換せず保持する。0018 の `ensure_json_encodable_details/1` のように、エンコードできない値だけを安全な表現に落とす必要があるかは、`not_unique` / `wrong_type_items` / `schema_unsupported` の詳細が `jsone:json_value()` の範囲に収まるかで判断する
- `allOf` / `anyOf` / `oneOf` の入れ子エラー（`reason()` のリストを詳細に持つ）の再帰変換は維持する
- `CHANGES.md` に独立したエントリは追加しない。JSON Schema バリデータは `## develop` の未リリース `[ADD]` の中にあり、この変更はその初回リリース内容に含まれる

## 完了条件

- `jsone_schema:validate/2` が返したエラーを `jsone_schema_error:to_json/1` に渡してもクラッシュしない。再現コードの `[[#{kind => foo}],[#{kind => foo}]]` がそのままエンコードされる
- `{?wrong_type_items, [#{kind => foo}]}` と `{?schema_unsupported, [#{kind => foo}]}` を詳細に持つ理由でもクラッシュしない
- `kind` が `data` / `schema` で必須キーが揃った map は、現行どおり reason として再帰変換される（`any_of_error_test/0` などの入れ子エラーの JSON 表現が変わらない）
- 上記を検査する回帰テストが `test/jsone_schema_tests.erl` に追加されている
- `./rebar3 xref` / `./rebar3 dialyzer` / `./rebar3 as test eunit` / `./rebar3 as test proper` が通る

## 解決方法

{未着手}
