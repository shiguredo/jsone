# エラー報告の内容が箇所によって不揃いで原因が分からない

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-error-report-inconsistency
- Polished: {YYYY-MM-DD}

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

- エラーの `schema` には利用者が書いたスキーマをそのまま載せる。内部変換した表現を載せない
- `path` の付け方を `additionalProperties: false` の実装に揃える
- `contains` には専用の理由（例: `?no_match`）を割り当てる
- 正規表現の妥当性検査を独立させ、不正な正規表現は評価順に依存せず常に schema エラーにする

## 完了条件

- 上記 5 点それぞれについて、エラーの内容を検査する回帰テストが `test/jsone_schema_tests.erl` に追加されている
- `patternProperties` に不正な正規表現と `additionalProperties: false` を併用したとき、どのキーワードを先に評価しても `kind => schema` のエラーが 1 件返る

## 解決方法

{未着手}
