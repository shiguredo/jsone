# jsone_schema_error:to_json が kind キーを持つデータのリストでクラッシュする

- Created: 2026-10-12
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-error-to-json-details-crash
- Polished: 2026-10-12

## 目的

検証エラーを `jsone_schema_error:to_json/1` で JSON にするとき、エラー詳細に利用者の値が入る経路で `function_clause` になり、エラーの原因を報告できない。`jsone:decode/1,2` が返す値でも、利用者が map を直接組み立てた値でも、エラー詳細にどんな値が入っても `to_json/1` がクラッシュしないようにする。

## 現状

`src/jsone_schema_error.erl` の `details_to_json/1` は、詳細がリストのとき、全要素が `kind` キーを持つ map かどうかだけで「エラー理由のリスト」と判定する。一方 `reason_to_json/1` は `kind` が `data` か `schema` の場合にしか節が無い。このため `kind` キーを持つが値が `data` / `schema` でない map のリストを詳細に渡すと、判定と節が食い違い `function_clause` になる。

エラー詳細に利用者の値がそのまま入る経路:

- `?not_unique` の詳細（`check_unique_items/2` が重複した要素を渡す）
- `?schema_unsupported` の詳細（`schema_ver/1` が `$schema` の値を渡す）
- `?missing_dependency` の詳細（`dependencies` のプロパティ名を渡す）
- `?schema_not_found` の詳細（`$ref` の URI を渡す）
- `?wrong_type_items` の詳細（`check_items/4` がキーワード値を渡す）。ただし `check_items/4` は `is_list(Items)` の節が先に一致するため、`jsone_schema:validate/2` から生成される詳細は非リスト値だけになる
- `allOf` / `anyOf` / `oneOf` の入れ子エラー（`check_all_of_1/3` / `check_any_of_1/4` / `check_one_of_1/5` が `reason()` のリストを詳細に渡す）。入れ子の理由に `?not_unique` などの詳細が入ると `function_clause` は入れ子の内側で起きる

`jsone:json_value()` の `json_object()` は `json_string()`（binary か atom）をキーに取るため、`kind` を atom キーに持つオブジェクトを含む配列はこの型に含まれる。`jsone:decode/2` の `{keys, attempt_atom}` を使った場合や、利用者が map を直接組み立てた場合に起こり得る。

さらに `json_string()` は UTF-8 として不正なバイナリも許すため、エラー詳細にエンコードできない値が入り得る。`jsone:encode/1` は不正な UTF-8 バイナリで `error:{invalid_byte, N}` を投げる。`src/jsone_schema_state.erl` の `ensure_json_encodable_details/1` は `schema_loader` のエラー詳細についてこの問題を既に回避している（0018）が、それ以外の詳細には効いていない。

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

- `jsone_schema:validate(#{<<"$schema">> => [#{kind => foo}]}, 1)` も `function_clause` になる
- `jsone_schema:validate(#{<<"allOf">> => [#{<<"uniqueItems">> => true}]}, [[#{kind => foo}], [#{kind => foo}]])` も `function_clause` になる（`all_schemas_not_valid` の詳細の理由リストの内側で起きる）
- `jsone_schema:validate(#{<<"items">> => <<255>>}, [1])` の `to_json/1` は `error:{invalid_byte, 255}` になる。`jsone_schema:validate(#{<<"uniqueItems">> => true}, [[<<255>>], [<<255>>]])` も同じ
- `kind` キーを持つが必須キーが欠けた map（`#{kind => data, error => x}` など）のリストも `error:{badkey, path}` などでクラッシュする
- 既定の `jsone:decode/1` が返す binary キーのデータでは `details_to_json/1` の判定と `reason_to_json/1` の節が一致するためクラッシュしない

## 設計方針

- `reason_to_json/1` と `details_to_json/1` の reason 判定を、`jsone_schema_error:reason()` の型と一致する map だけを対象にするよう厳密化する。`kind` が `data` なら `path` / `schema` / `value` / `error`、`kind` が `schema` なら `schema` / `error` を必須キーとする。必須キーが欠けた map は reason として変換しない
- reason の形でない map は reason として変換せず、利用者の値として扱う。`details_to_json/1` が「reason のリストか」と「それ以外の値」を確実に切り分け、`reason_to_json/1` の節に届かない形は変換しない
- 利用者の値はエンコードできる範囲で変換せず保持する。エンコード可否の判断は `jsone:try_encode/1` の成否で行い、`jsone_schema_state:ensure_json_encodable/1` と同じ方法（`~w` で文字列化し `unicode:characters_to_binary/1` で UTF-8 に揃える）でエンコードできない値だけ安全な表現に落とす。`jsone:json_value()` の型に収まるかどうかでは判断しない（`json_string()` は不正な UTF-8 バイナリを許すため）
- この安全な表現への変換は詳細の値（リストの要素とマップの値）を対象にする。`details_to_json/1` は map 専用の `jsone_schema_state:ensure_json_encodable_details/1` をそのままは使えないため、リストの詳細にも適用できる形にする
- 理由の `value` / `path` / `schema` など `details` 以外のフィールドは、`jsone:decode/1` が返す値なら常にエンコードできる。手組みの値ではエンコードできない値が入り得るが、その場合の扱いはこの issue の対象外とする（目的の「利用者が map を直接組み立てた値」は、詳細に `kind` キーを持つ map が入る経路を指す）
- `allOf` / `anyOf` / `oneOf` の入れ子エラー（`reason()` のリストを詳細に持つ）の再帰変換は維持する。入れ子の理由の `details` にもこの issue の変換を再帰的に適用する
- `CHANGES.md` に独立したエントリは追加しない。JSON Schema バリデータは `## develop` の未リリース `[ADD]` の中にあり、この変更はその初回リリース内容に含まれる

## 完了条件

- `jsone_schema:validate/2` が返したエラーを `jsone_schema_error:to_json/1` に渡してもクラッシュしない。再現コードの `[[#{kind => foo}],[#{kind => foo}]]` は `value` が `[[{"kind":"foo"}],[{"kind":"foo"}]]`、`details` が重複した要素 `[{"kind":"foo"}]` としてエンコードされる
- `{?wrong_type_items, [#{kind => foo}]}` と `{?schema_unsupported, [#{kind => foo}]}` を詳細に持つ理由でもクラッシュしない。前者は `jsone_schema:validate/2` では作れない（`check_items/4` の `is_list(Items)` の節が先に一致する）ため、理由 map を直接組み立てて検査する
- エラー詳細にエンコードできない値が入ってもクラッシュしない。`#{<<"uniqueItems">> => true}` に `[[<<255>>], [<<255>>]]`、`#{<<"items">> => <<255>>}` に `[1]`、`#{<<"$schema">> => <<255>>}` に `1`、`#{<<"dependencies">> => #{<<"a">> => [<<255>>]}}` に `#{<<"a">> => 1}` を渡した結果を `to_json/1` に通してクラッシュしない
- `allOf` / `anyOf` / `oneOf` の入れ子エラーの `to_json/1` 出力で、`details` が入れ子の reason のオブジェクト配列に展開される。`any_of_error_test/0` は `jsone_schema:validate/2` の戻り値だけを検査して `to_json/1` を呼ばないため、この出力を検査する回帰テストを追加する
- `kind` が `data` / `schema` で必須キーが揃った map は、現行どおり reason として変換される
- `kind` が `data` / `schema` でも必須キーが欠けた map のリストは、reason としてではなく利用者の値としてエンコードされる
- 上記を検査する回帰テストが `test/jsone_schema_tests.erl` に追加されている
- `./rebar3 xref` / `./rebar3 dialyzer` / `./rebar3 as test eunit` / `./rebar3 as test proper` が通る

## 解決方法

{未着手}
