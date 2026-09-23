# エラー理由の詳細が失われ原因を追えない

- Created: 2026-09-16
- Completed: 2026-09-23
- Branch: feature/fix-preserve-error-details
- Polished: 2026-09-18

## 目的

スキーマの読み込み失敗や正規表現のコンパイル失敗で、原因の情報を捨ててしまい利用者が対処できない問題を修正する。

## 現状

- `src/jsone_schema_state.erl` の `load_document/2` は失敗を 3 通りとも `?schema_not_found` に潰している。(a) ローダ未指定、(b) ローダが `{error, Reason}` を返した（ファイル入出力エラーを含む）かスキーマでない値を返した、(c) ローダが例外を投げた（`function_clause` などの実装不具合を含む）。(b) は `normalize_document/1` が `{error, Reason}` と非スキーマ値を `error` に潰し `maybe` の else 節が同じ項を返すことで、(c) は `catch _:_ ->` で、どちらも `{?schema_not_found, DocumentURI}` になり区別できない
- `src/jsone_schema_validator.erl` の `run_pattern/2` は `re:run/3` の `error:badarg` を捕まえて `{error, Reason}` を返すが、`check_pattern/3` と `check_pattern_properties_1/4` はどちらも `?schema_invalid` に潰しており、正規表現のどこが不正なのかが分からない。`re:run/3` はどの不正パターンでも `badarg` しか返さないため、位置とメッセージを得るには `re:compile/2`（`{error, {Message, Position}}` を返す）を使う必要がある
- `src/jsone_schema.erl` の `ensure_schema/2` は `catch _Class:Reason -> {error, {parse_error, Reason}}` として例外クラスを捨てている。既存の `src/jsone.erl` の `try_decode/2` は `{Reason, Stacktrace}` を返しており、スタックトレースの有無が揃っていない
- `jsone_schema_error:error_info()` の型は `atom() | {atom(), term()}` で、`error_info_to_json/1` も atom と 2 要素タプルの 2 節しか持たない。3 要素タプルを理由にすると `to_json/1` が `function_clause` で落ちる

## 設計方針

- `load_document/2` の失敗を 3 通りに分ける
  - (a) ローダ未指定: `{?schema_not_found, DocumentURI}`（現行どおり）
  - (b) ローダが `{error, Reason}` を返した: `{?schema_load_error, #{<<"uri">> => DocumentURI, <<"reason">> => Reason}}`。スキーマでない値を返した場合は、その値を `reason` に入れる
  - (c) ローダが例外を投げた: `{?schema_load_error, #{<<"uri">> => DocumentURI, <<"class">> => Class, <<"reason">> => Reason}}`
- 理由は `error_info()` の範囲（`{atom(), term()}`）に収める。details は map にし、`details_to_json/1` がそのまま返し `jsone:encode/1` がエンコードできる形にする（atom は文字列、tuple は `~0p` の文字列、list は配列、map はオブジェクトになる）。3 要素タプルにはしない
- `?schema_load_error` を `jsone_schema.hrl` の schema エラー理由に追加する。`load_document/2` の失敗理由としての `schema_not_found` は「ローダ未指定」と「JSON Pointer が見つからない」に限る（`validate_key/2,3` のキー未登録は別の経路で、この issue の対象外）
- 正規表現のコンパイル失敗は `run_pattern/2` を `re:compile/2` + `re:run/2` に変え、`{?wrong_pattern, #{<<"message">> => Message, <<"position">> => Position}}` を返す。`Message` は `iolist_to_binary/1` したバイナリにする。`?wrong_pattern` を `jsone_schema.hrl` に追加し、既存の `?schema_invalid`（atom）とは別の理由にする（同じ名前で atom と詳細付きの 2 形態にしない）
- 詳細を含めるのは `check_pattern/3` / `check_pattern_properties_1/4` / `matches_any_pattern/3` の 3 箇所にする。`matches_any_pattern/3` は `run_pattern/2` のエラーを非一致として扱わず schema エラーを積む形になったため、詳細の付け忘れが起きないよう同じ理由を使う
- `ensure_schema/2` は `{error, {parse_error, {Class, Reason}}}` を返す。`add_schema/3` と `load_schemas/2` の戻り値の形（`{error, {parse_error, _}}` / `{error, {File, Reason}}`）は変えない
- `resolve_document/2` の成功時の戻り値は変えない（失敗時の項だけを変える）
- `CHANGES.md` に独立したエントリは追加しない。JSON Schema バリデータは `## develop` の未リリース `[ADD]` の中にあり、この変更はその初回リリース内容に含まれる（`## develop` の `[FIX]` は別 issue が `[ADD]` に統合する）

## 完了条件

- ローダ未指定が `{?schema_not_found, URI}`、ローダが `{error, Reason}` を返した場合が `{?schema_load_error, #{<<"uri">> := URI, <<"reason">> := Reason}}`、ローダが例外を投げた場合が `{?schema_load_error, #{<<"uri">> := URI, <<"class">> := Class, <<"reason">> := Reason}}` になり、3 つが区別できる
- 新しい理由を含むエラーで `jsone_schema_error:to_json/1` がクラッシュせず、`{atom(), term()}` の形でエンコードできる（`error_to_json_test/0` に検査を追加する）
- 不正な正規表現のエラーが `pattern` と `patternProperties` の両方で `{?wrong_pattern, #{<<"message">> := _, <<"position">> := _}}` を含む
- `ensure_schema/2` が `{error, {parse_error, {Class, Reason}}}` を返し、`add_schema/3` と `load_schemas/2` の戻り値の形が変わっていない（`store_parse_test/0` の `{error, {parse_error, _}}` がそのまま通る）
- 上記を検査する回帰テストが `test/jsone_schema_tests.erl` に追加されている
- `./rebar3 xref` / `./rebar3 dialyzer` / `./rebar3 as test eunit` / `./rebar3 as test proper` が通る

## 解決方法

- `load_document/2` の失敗を 3 通りに分けた。ローダ未指定は `{?schema_not_found, DocumentURI}` のまま、ローダが `{error, Reason}` かスキーマでない値を返した場合は `{?schema_load_error, #{<<"uri">> => DocumentURI, <<"reason">> => Reason}}`、ローダが例外を投げた場合はクラスを足した `{?schema_load_error, #{<<"uri">> => DocumentURI, <<"class">> => Class, <<"reason">> => Reason}}` にした。戻り値の正規化は `normalize_document/1` が `{ok, Schema} | {error, Reason}` を返す形に変え、`{ok, 非スキーマ}` の包みも外してその値自体を理由にした
- ローダが返す理由は任意の term のため、UTF-8 として不正なバイナリや不正なリストをそのまま詳細に載せると `jsone_schema_error:to_json/1` がクラッシュする。`ensure_json_encodable_details/1` でエンコードできない値だけ `~w` の文字列に落とし、`unicode:characters_to_binary/1` で UTF-8 として妥当なバイナリに揃えた
- `run_pattern/2` を `re:compile/2` + `re:run/3` に変え、コンパイル失敗を `{?wrong_pattern, #{<<"message">> => Message, <<"position">> => Position}}` として返すようにした。`check_pattern/3` と `check_pattern_properties_1/4` と `matches_any_pattern/3` はこの理由を潰さずに積む。コンパイルできないパターンや照合できないインスタンスは捕捉して `?schema_invalid` にし、クラッシュさせない
- `ensure_schema/2` は `{error, {parse_error, {Class, Reason}}}` を返す。`add_schema/3` と `load_schemas/2` の戻り値の外側の形は変えていない
- `CHANGES.md` は変更していない。`## develop` の未リリース `[ADD]` の中の変更であり、`[FIX]` の統合は別の issue が扱う
- `test/jsone_schema_tests.erl` に、3 通りの失敗の区別、`{ok, 非スキーマ}` と `throw` / `exit` のクラス、エンコードできない理由の `to_json/1`、`validate_key/2,3` の既定ローダ、不正な正規表現の `pattern` と `patternProperties`、非バイナリの `patternProperties` のキー、`parse_error` のクラスの回帰テストを追加した
