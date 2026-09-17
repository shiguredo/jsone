# エラー報告の内容が箇所によって不揃いで原因が分からない

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-error-report-inconsistency
- Polished: 2026-09-18

## 目的

検証エラーの `schema` / `path` / `error` の内容が箇所によって不揃いで、利用者が原因を特定できない問題を修正する。コードレビューで重要と判断した項目。

## 現状

- `false` スキーマ: `src/jsone_schema_validator.erl` の `check_value/3` が `false` を `#{?NOT => #{}}` に置き換えるため、エラーの `schema` に利用者が書いた `false` ではなく内部表現 `#{"not" => #{}}` が載る
- `additionalItems: false`: 同 `check_items_array/6` は余分な要素のインデックスを `path` に積まず、値は配列全体を載せ、`max_errors => infinity` でも 1 件しか報告しない。`additionalProperties: false` を扱う `check_additional_properties/4` は 1 プロパティごとに path 付きで報告しており不揃い
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

- エラーの `schema` には利用者が書いたスキーマをそのまま載せる。内部変換した表現を載せない。`false` スキーマは `schema => false` とし、失敗理由は現行どおり `?not_schema_valid` を維持する（draft-06 core §4.4 が `false` を `{"not": {}}` と等価と定めているため）
- `additionalItems: false` は `additionalProperties: false` と同じ形に揃える。余分な要素 1 件ごとに `path` にそのインデックスを積んで報告し、`value` は `additionalProperties: false` がオブジェクト全体を載せるのに合わせて配列全体とする。`max_errors => infinity` では余分な要素の数だけ報告される
- 配列形式の `dependencies` は、スキーマ形式と同じく依存を発動したプロパティ名（`check_dependency/4` の `DependencyName`）を `path` に積む。`value` はオブジェクト全体のままとする（欠けたプロパティには値が無いため）。このため `check_dependency_property/3` に発動元のプロパティ名を渡す形にする
- `contains` には新しい理由 `?no_contains_match` を `jsone_schema.hrl` に追加して割り当てる。`?no_match` は `pattern` の不一致が既に使っており、流用すると `error` だけでは `pattern` の失敗か `contains` の失敗かを区別できない
- 不正な正規表現の検査は、キーワード評価の一部ではなくスキーマの評価を始める時点（`check_value/3` の `$ref` 判定より後、`check_keywords/3` より前）に行う。`$ref` を持つスキーマは §8 により他のキーワードを評価しないため、兄弟の `pattern` / `patternProperties` を検査してはならない。この位置なら `matches_any_pattern/2` が `{error, _}` を非一致として扱う経路に依存せず、`additionalProperties` など他のキーワードの評価順にも左右されない
- `run_pattern/2` が返すエラー理由の詳細化（正規表現のどこが不正か）は 0018 が扱う。この issue は検出の順序と打ち切りに限定し、理由は現行どおり `?schema_invalid` のままとする
- `not` / `anyOf` / `oneOf` のサブスキーマで見つかった schema エラーが検証成功に転じる既存挙動は 0005 と同じくこの issue の対象外とし、別 issue で扱う

## 完了条件

- 上記 5 点それぞれについて、エラーの内容を検査する回帰テストが `test/jsone_schema_tests.erl` に追加されている（`false` は `schema => false`、`additionalItems` は `path` にインデックス、配列形式 `dependencies` は `path` に依存を発動したプロパティ名、`contains` は `?no_contains_match`、不正正規表現は `kind => schema`）
- `patternProperties` に不正な正規表現と `additionalProperties: false` を併用したとき、`max_errors` の既定値では `kind => schema` のエラーが 1 件だけ返り、`no_extra_properties_allowed` は返らない。map の走査順（33 要素以上で hashmap になり順序が変わる）に依存しない
- `max_errors => infinity` では schema エラーが data エラーより先に並ぶ
- 現行のエラー形を固定している既存テスト `items_test/0` / `dependencies_test/0` / `contains_test/0` / `boolean_contains_test/0` が新しい期待値に更新されている
- `test/JSON-Schema-Test-Suite/tests/draft6` の 702 ケースと `test/prop_jsone_schema.erl` の全プロパティが引き続き通る

## 解決方法

{未着手}
