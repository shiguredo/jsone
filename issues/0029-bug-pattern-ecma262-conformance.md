# `pattern` / `patternProperties` の ECMA 262 非互換を修正する

- Created: 2026-11-14
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-pattern-ecma262-conformance
- Polished: {YYYY-MM-DD}

## 目的

draft-06 validation §3.3 / §6.8 は、`pattern` の正規表現を ECMA 262 に従うものとしている。現状は Erlang の `re` を `unicode` / `ucp` 付きで使い、PCRE の意味論で照合しているため、ECMA 262 では一致しない文字列を一致と判定する。`pattern` / `patternProperties` の検証結果を仕様に近づける。

## 現状

`src/jsone_schema_validator.erl` の `run_pattern/2` は `re:compile(Pattern, [unicode, ucp])` と `re:run/3` を使う。このため次の差がある。

- `^a$` が `"a\n"` に一致する（PCRE の `$` は末尾改行の前にも一致する）。
- `\d` がアラビア数字 `١` に一致する（`ucp` により Unicode の数字）。
- `\w` が `é` に一致する（`ucp` により Unicode の単語文字）。
- `\b` が Unicode の語境界になる。
- PCRE 固有の `(?i)`、`a++`、`\K`、`(?R)`、`[[:alpha:]]` などを受理する。逆に ECMA 262 の `\u{1F600}` を拒否する。

実測:

```erlang
jsone_schema:validate(#{<<"pattern">> => <<"^a$">>}, <<"a\n">>).
%% => {ok, <<"a\n">>}（ECMA 262 では不一致）

jsone_schema:validate(#{<<"pattern">> => <<"^\\d+$">>}, <<"١٢٣"/utf8>>).
%% => {ok, ...}（ECMA 262 の \d は [0-9]）

jsone_schema:validate(#{<<"pattern">> => <<"^\\w+$">>}, <<"é"/utf8>>).
%% => {ok, ...}
```

さらに、`pattern` の不正な正規表現はインスタンスが文字列のときだけ検出される。`{"pattern":"["}` に `1` を渡すと `{ok, 1}` になる。`patternProperties` の不正な正規表現も、オブジェクトが空だと検出されない。公式スイートの optional `ecmascript-regex` 9 件は fail する。

## 設計方針

- `$` を入力末尾の一致に限定する（PCRE の `$` ではなく `\z` 相当にする、または `dollar_endonly` を使う）。
- `\d` / `\w` / `\s` / `\b` を ECMA 262 の ASCII クラスに合わせる。Unicode 対応が必要なら ECMA 262 の `u` フラグと Unicode プロパティエスケープの範囲を明示する。
- PCRE 固有の構文を受理しない。少なくとも `(?i)`、`a++`、`\K`、`(?R)`、`(?#...)`、`[[:alpha:]]` の扱いを決める。
- 正規表現のコンパイルをスキーマ評価時に 1 回だけ行い、データ型・空オブジェクトに依存せず schema エラーにする。コンパイル済みパターンは評価単位でキャッシュする。
- ECMA 262 完全準拠を目指さない場合でも、README に非互換の範囲と既知の失敗を明記し、公式スイートの optional `ecmascript-regex` の結果を管理する。

## 完了条件

- `^a$` が `"a\n"` に一致しない。`\d` がアラビア数字に一致しない。`\w` が `é` に一致しない。
- ECMA 262 で不正な正規表現が schema エラーになり、データの型やプロパティの有無に依存しない。
- `patternProperties` の不正な正規表現が、空オブジェクトでも schema エラーになる。
- 公式スイートの optional `ecmascript-regex` の結果を実行できる。完全準拠しない場合は、既知の失敗を README に列挙する。
- `xref` / `dialyzer` / `eunit` / `proper` が通る。

## 解決方法

{未着手}
