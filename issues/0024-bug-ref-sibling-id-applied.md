# `$ref` と兄弟 `$id` の併記で基準 URI が変わり、別スキーマで検証される

- Created: 2026-11-14
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-ref-sibling-id-applied
- Polished: {YYYY-MM-DD}

## 目的

draft-06 core §8 は「`$ref` を持つオブジェクトの他のプロパティはすべて無視する」としている。しかし現状は `$ref` と併記した `$id` が基準 URI に反映され、参照先が別のスキーマに変わる。正しいスキーマで合否が反転するため、`$id` を含む兄弟キーワードを無視するよう修正する。

## 現状

`src/jsone_schema_validator.erl` の `check_value/3` は、`$ref` の分岐より先に `jsone_schema_state:enter_schema/2` を呼び、兄弟の `$id` を基準 URI に反映する。その状態で `check_ref/3` が参照を解決するため、`$ref` の解決先が変わる。

索引側でも同じ問題がある。`src/jsone_schema_index.erl` の `walk/5` は `$ref` の有無を確認せず、兄弟の `definitions` / `properties` / `$defs` などを走査して `$id` を索引する。そのため、無視されるべき兄弟の中の `$id` を `$ref` で参照できてしまう。

再現 1: 兄弟 `$id` により参照先が変わり、合否が反転する。

```erlang
Schema = #{
  <<"$id">> => <<"http://example.com/base/root.json">>,
  <<"allOf">> => [#{<<"$id">> => <<"sub/">>, <<"$ref">> => <<"target.json">>}]
},
Options = #{
  schemas => #{
    <<"http://example.com/base/target.json">> => #{<<"type">> => <<"integer">>},
    <<"http://example.com/base/sub/target.json">> => #{<<"type">> => <<"string">>}
  }
},
jsone_schema:validate(Schema, 1, Options).
%% => {error, ...}（兄弟 $id を無視すれば integer に解決され {ok, 1}）
jsone_schema:validate(Schema, <<"x">>, Options).
%% => {ok, <<"x">>}（正しくは string に反して error）
```

再現 2: `$ref` の兄弟 `definitions` にある `$id` が実在の識別子として使える。

```erlang
Schema = #{
  <<"definitions">> => #{
    <<"bad">> => #{
      <<"$ref">> => <<"#/definitions/good">>,
      <<"$id">> => <<"http://example.com/bad">>,
      <<"definitions">> => #{<<"good">> => #{<<"type">> => <<"integer">>}}
    }
  },
  <<"$ref">> => <<"http://example.com/bad">>
},
jsone_schema:validate(Schema, 1).
%% => {ok, 1}（兄弟 definitions は無視されるべきなので schema_not_found が正しい）
```

## 設計方針

- `check_value/3` で `$ref` が binary の場合を最初に判定し、`enter_schema/2` を呼ばずに `check_ref/3` へ進む。
- `jsone_schema_index:walk/5` で binary の `$ref` を持つ map を見つけたら、そのオブジェクト自身の `$id` と兄弟キーワードの走査を行わない。
- `$ref` が文字列でない場合の既存の schema エラー処理は変えない。
- 兄弟 `$id` を無視した結果、参照先が見つからない場合は `schema_not_found` の schema エラーとして返す。

## 完了条件

- 再現 1 で、`schemas` に `base/target.json` と `base/sub/target.json` を置いても、`1` が `{ok, 1}`、`<<"x">>` が `{error, _}` になる。
- 再現 2 で、兄弟 `definitions` の `$id` を参照できない（`schema_not_found` になる）。
- 兄弟 `$id` が絶対 URI と相対 URI の両方で、基準 URI と索引に影響しないことをテーブルテストで固定する。
- `test/JSON-Schema-Test-Suite/tests/draft6` の 702 件、PropEr の全性質、`xref` / `dialyzer` / `eunit` / `proper` が通る。
- テスト名・コメントに issue 番号を書かない。根拠は draft-06 core §8 と再現条件で示す。

## 解決方法

{未着手}
