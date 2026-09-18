# エラー報告の内容が箇所によって不揃いで原因が分からない

- Created: 2026-09-16
- Completed: 2026-09-18
- Branch: feature/fix-error-report-inconsistency
- Polished: 2026-09-18

## 目的

検証エラーの `schema` / `path` / `error` の内容が箇所によって不揃いで、利用者が原因を特定できない問題を修正する。

## 現状

- `false` スキーマ: `src/jsone_schema_validator.erl` の `check_value/3` が `false` を `#{?NOT => #{}}` に置き換えるため、エラーの `schema` に利用者が書いた `false` ではなく内部表現 `#{<<"not">> => #{}}` が載る
- `additionalItems: false`: 同 `check_items_array/6` は余分な要素のインデックスを `path` に積まず、`max_errors => infinity` でも 1 件しか報告しない。`additionalProperties: false` を扱う `check_additional_properties/4` は 1 プロパティごとに path 付きで報告しており不揃い（`value` にコンテナ全体を載せる点は `additionalProperties: false` と同じなので不揃いではない）
- 配列形式の `dependencies`: 同 `check_dependency_property/3` は `path` を積まない（スキーマ形式の `check_dependency/4` は `check_child/4` 経由で積む）
- `contains`: 同 `check_contains/3` の失敗理由が `?data_invalid` で `#{kind => data, error => data_invalid}` となり情報量がない
- 不正な正規表現: 同 `matches_any_pattern/2` は `run_pattern/2` の `{error, _}` を「非一致」として扱うため、`patternProperties` に不正な正規表現と `additionalProperties: false` を併用すると、`schema_invalid` ではなく `no_extra_properties_allowed` が返ることがある。どちらが返るかはスキーマの map 走査順に依存する

再現（実測）:

```erlang
%% false スキーマのエラーに内部表現が載る
{error, [R]} = jsone_schema:validate(false, 1),
maps:get(schema, R).
%% #{<<"not">> => #{}}
```

## 設計方針

- エラーの `schema` には利用者が書いたスキーマをそのまま載せる。内部変換した表現を載せない。`false` スキーマの `error` は現行どおり `?not_schema_valid` のままとし、`schema` だけを `false` にする
- `additionalItems: false` は余分な要素 1 件につき 1 件報告し、`path` にその要素のインデックスを積む。`value` は現行どおり配列全体とし、`additionalProperties: false` と同じ形に揃える。報告件数が `max_errors` に従う点も `additionalProperties: false` と同じになる
- 配列形式の `dependencies` は、スキーマ形式の `check_dependency/4` と同じく依存を起動したプロパティ名を `path` に積む。`#{<<"dependencies">> => #{<<"bar">> => [<<"foo">>]}}` の違反なら `path` は `[<<"bar">>]`、`value` はオブジェクト全体とする
- `contains` の失敗には専用の理由 `?no_contains_match` を新設する。既存の `?no_match` は `pattern` の不一致を表しており流用しない。`?data_invalid` を理由として使う箇所はこれで無くなる
- `matches_any_pattern/2` は `run_pattern/2` の `{error, Reason}` を「非一致」として扱わない。`patternProperties` の正規表現がコンパイルできない場合は schema エラーとして報告し、`additionalProperties` が先に評価されても `no_extra_properties_allowed` を返さない。スキーマ全体を事前に走査する検査は導入しない。したがってインスタンスがオブジェクトでない場合と、評価対象のプロパティが 1 つも無いオブジェクトの場合は、現行どおり正規表現を評価せずエラーにもならない。`pattern` キーワードの扱いは現行のまま変えない。正規表現エラーの詳細の持ち方は別 issue の担当であり、この issue ではエラーの種別と順序依存だけを扱う

## 完了条件

- 上記 5 点それぞれについて、エラーの内容を検査する回帰テストが `test/jsone_schema_tests.erl` に追加されている
- 現行挙動を固定している既存テストを新しい期待値に更新する（`items_test/0` の `path => []`、`dependencies_test/0` の `path => []`、`contains_test/0` と `boolean_contains_test/0` の `error => data_invalid`。`boolean_items_test/0` の `error := not_schema_valid` は現行のまま）
- `false` スキーマのエラーが `schema => false` を載せ、`error` は `not_schema_valid` のまま
- `additionalItems: false` が `max_errors => infinity` で余分な要素 1 件につき 1 件報告し、`path` にインデックスが入る
- 配列形式の `dependencies` のエラーが `path => [<<"bar">>]` を載せる
- `contains` の失敗が `error => no_contains_match` になる
- `patternProperties` に不正な正規表現と `additionalProperties: false` を併用したとき、スキーマのキー順に関わらず、既定の `max_errors` で返るエラーが `kind => schema` の 1 件だけになる
- インスタンスがオブジェクトでない場合と、評価対象のプロパティが 1 つも無いオブジェクトの場合は、不正な正規表現でもエラーにならない（現行どおり）
- `./rebar3 xref` / `./rebar3 dialyzer` / `./rebar3 as test eunit` / `./rebar3 as test proper` が通る

## 解決方法

検証エラーの `schema` / `path` / `error` の内容を 5 点で揃えた。

- `false` スキーマ: `check_value/3` が `#{?NOT => #{}}` に置き換える前に `jsone_schema_state:enter_schema/2` で現在のスキーマを `false` にする。エラーの `schema` に利用者が書いた `false` が載り、`error` は `not_schema_valid` のまま
- `additionalItems: false`: `check_items_array/6` が余分な要素 1 件につき 1 件報告し、その要素のインデックスを `path` に積む。`value` は配列全体のままで、報告件数は `max_errors` に従う
- 配列形式の `dependencies`: `check_dependency/4` が依存を起動したプロパティ名を `path` に積む（スキーマ形式と揃う）
- `contains`: 失敗理由を新しい `?no_contains_match` にした。`?data_invalid` を理由として使う箇所は無くなった（マクロの削除は 0015 が扱う）
- 不正な正規表現: `matches_any_pattern/3` が `run_pattern/2` の `{error, _}` を非一致として扱わず schema エラーを積むようにし、`extra_property_names/3` と `check_additional_properties/4` も三値（`matched` / `not_matched` / `error`）に合わせた。`additionalProperties: false` を併用しても、正規表現が不正なプロパティを余分なプロパティとして報告しない
- `test/jsone_schema_tests.erl` の `items_test/0` / `dependencies_test/0` / `contains_test/0` / `boolean_contains_test/0` を新しい期待値に更新し、`boolean_schema_test/0` / `invalid_pattern_properties_test/0` / `additional_properties_test/0` に検査を追加した。`boolean_items_test/0` は現行のまま
- `matches_any_pattern` と `extra_property_names` の arity が変わり、`?data_invalid` の参照が 0 件になったため、0015 と 0018 の記述も現状に合わせて更新した

`./rebar3 xref` / `./rebar3 dialyzer` / `./rebar3 as test eunit` / `./rebar3 as test proper` が通り、eunit は 750 件、PropEr は 15 件すべて通過した。`test/JSON-Schema-Test-Suite/tests/draft6` の 702 ケースも引き続き通る。
