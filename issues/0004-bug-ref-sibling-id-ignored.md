# $ref と併記した id が無視されずスキーマエラーになる

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-ref-sibling-id-ignored
- Polished: 2026-09-18

## 目的

draft 6 core §8 の「`$ref` を持つオブジェクトの他のプロパティは無視する（MUST）」に反し、`$ref` と `id` を併記したスキーマが `wrong_draft6_id_tag` で失敗する問題を修正する。

`id` 以外の兄弟キーワードは既に無視されており、`id` だけが例外になっている。`$schema` は文書の方言を選ぶキーワードなので兄弟無視の対象にしない（後述）。

## 現状

`src/jsone_schema_validator.erl` の `check_value/3` は `maps:is_key(?ID_OLD, JsonSchema)` を `$ref` の判定より先に評価するため、`$ref` があっても `id` の存在だけで schema エラーになる。

再現（実測）:

```erlang
S = #{<<"definitions">> => #{<<"a">> => #{<<"type">> => <<"integer">>}},
      <<"$ref">> => <<"#/definitions/a">>,
      <<"id">> => <<"legacy">>},
jsone_schema:validate(S, 1).
%% {error, [#{kind => schema, error => wrong_draft6_id_tag, ...}]}
```

現状の把握:

- `id` 以外の兄弟キーワードは既に無視される。実測: `$ref` と `maxItems: 1` を併記したスキーマに `[1, 2, 3]` を与えると `{ok, [1, 2, 3]}`。テストスイートの "ref overrides any sibling keywords" も 702 ケースの中で通過している。§8 の MUST を満たしていないのは `id` だけ
- `$schema` は `validate_with_state/3` が `check_value/3` より先に評価する。実測: `$ref` と draft-06 の `$schema` の併記は `{ok, 1}`、`$ref` と未対応 `$schema`（draft-04 や draft-05 のタイポ）の併記は `{error, [#{kind => schema, error => {schema_unsupported, ...}}]}`。draft-06 core §7 は「The "$schema" keyword SHOULD be used in a root schema. It MUST NOT appear in subschemas.」としており、`$schema` は文書の方言を選ぶキーワードである。§8 の兄弟無視を `$schema` にまで広げると、`$ref` を併記した draft-04 の文書が draft-06 として黙って検証される
- `id` 単体のスキーマは `wrong_draft6_id_tag` で schema エラーになる。この理由はコード・README・CHANGES.md のどこにも書かれていない（§4.4「Unknown keywords SHOULD be ignored.」および §6.4「Implementations SHOULD ignore keywords they do not support.」からの意図的な逸脱）
- テストスイート 702 ケースに `$ref` と `id` の併記は無く、draft6 の `id` キーワードのケースも無い

仕様の根拠:

- draft-06 core §8「An object schema with a "$ref" property MUST be interpreted as a "$ref" reference. ... All other properties in a "$ref" object MUST be ignored.」
- 同 §7「The "$schema" keyword SHOULD be used in a root schema. It MUST NOT appear in subschemas.」
- 同 §4.4「A JSON Schema MAY contain properties which are not schema keywords. Unknown keywords SHOULD be ignored.」

## 設計方針

- `$ref` の判定を `id` の判定より先に行い、binary な `$ref` がある場合は `id` を評価しない
- `id` 単体を弾く検査は残す。単純に順序を入れ替えると `id` 単体のスキーマが `check_keywords/3` から `check_keyword_value/5` の catch-all 節に落ちて無視されるため、`id` の検出は `check_keywords/3` に移さず、`$ref` を検出しない場合の分岐として `check_value/3` に残す構成にする（binary な `$ref` がある場合は §8 により `id` を評価しない）。`$ref` の分岐は `is_binary/1` でガードし、`$ref` が非文字列で `id` を併記した場合は `id` の検出（`wrong_draft6_id_tag`）を現行どおり優先する。`$ref` の値が文字列でない場合の扱い自体は別 issue（0005）で扱う
- `id` 単体をエラーにする理由は「draft-04 の文書を draft-06 として黙って検証しないため」とし、`check_value/3` の分岐にコメントで書く
- `$schema` は方言ゲートとして `validate_with_state/3` で `check_value/3` より先に評価する現行順序を維持する。§8 の兄弟無視は `$schema` 以外の兄弟キーワードに適用する
- サブスキーマの `$schema` は §7 が MUST NOT としている書き方であり、この issue の対象外とする。現行挙動（未対応なら `schema_unsupported`）を変えない
- `$id`（draft-06 の識別子）と `$ref` を併記したスキーマの扱いもこの issue の対象外とする。`check_value/3` が `$ref` の判定より前に `jsone_schema_state:enter_schema/2` を呼び、`$id` を基準 URI に反映する現行挙動は変更しない。この組み合わせは公式スイートにケースが無く、§8 の兄弟無視を `$id` にまで広げるかどうかは別の判断になるため、必要になった時点で別 issue にする

## 完了条件

- `$ref` と `id` を併記したスキーマが参照先の検証結果を返す（`#{<<"type">> => <<"integer">>}` への `$ref` と `id` の併記で、`1` は `{ok, 1}`、`<<"x">>` は data エラー）
- `$ref` と draft-06 の `$schema` と `id` を併記したスキーマも同じ結果になる（`$schema` は方言ゲートとして通り、`id` は無視される）
- `$ref` と未対応 `$schema` を併記したスキーマは現行どおり `schema_unsupported` を返す（`test/jsone_schema_tests.erl` の `schema_unsupported_test/0` に `$ref` 併記ケースを追加する）
- `id` 単体のスキーマは従来どおり `wrong_draft6_id_tag` を返す（既存 `id_keyword_test/0` のアサーションがそのまま通る）
- `$ref` と `id` の併記ケースが `test/jsone_schema_tests.erl` の `id_keyword_test/0` に追加されている
- `test/JSON-Schema-Test-Suite/tests/draft6` の 702 ケースと `test/prop_jsone_schema.erl` の全プロパティが引き続き通る

## 解決方法

{未着手}
