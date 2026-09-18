# $ref の自己参照・相互参照で検証が停止しない

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-ref-infinite-loop
- Polished: 2026-09-18

## 目的

`jsone_schema:validate/2,3` がスキーマの `$ref` 循環で停止しなくなり、呼び出しプロセスのメモリを食い尽くす問題を修正する。

draft 6 の仕様は validator 側の責務として無限ループの禁止を MUST で定めている。`schema_loader` オプションや `load_schemas/1` は外部スキーマを読み込む経路であり、停止しないスキーマを与えられただけでノードの資源を消費し続ける。

## 現状

`src/jsone_schema_validator.erl` の `check_ref/3` は `jsone_schema_state:resolve_ref/2` で `$ref` を解決したあと `validate_with_state/3` を再帰呼び出しする。解決先が同じスキーマ・同じインスタンスに戻ってくる場合の再入検出が無い。`undo_resolve_ref/2` は再帰から戻ったあとに呼ばれるため末尾再帰にもならず、スタックとヒープが伸び続ける。

停止しないのは自己参照だけではない。インスタンスが降下しない再帰はすべて停止しない。

```erlang
%% 自己参照: 停止しない
jsone_schema:validate(#{<<"$ref">> => <<"#">>}, 1).

%% 相互参照: 停止しない
S = #{<<"definitions">> =>
          #{<<"a">> => #{<<"$ref">> => <<"#/definitions/b">>},
            <<"b">> => #{<<"$ref">> => <<"#/definitions/a">>}},
      <<"$ref">> => <<"#/definitions/a">>},
jsone_schema:validate(S, 1).

%% allOf 経由: 停止しない
jsone_schema:validate(#{<<"allOf">> => [#{<<"$ref">> => <<"#">>}]}, 1).

%% map 形式の dependencies 経由: インスタンスは降下せずパスだけが伸びるため停止しない
jsone_schema:validate(#{<<"dependencies">> => #{<<"a">> => #{<<"$ref">> => <<"#">>}}},
                      #{<<"a">> => 1}).
```

計測例（OTP 29.0、ローカル。自己参照のスキーマを検証するワーカープロセスを `spawn` し、`erlang:memory(total)` と `erlang:statistics(reductions)` を読んだ。GC のタイミングで大きく変動するため幅を持たせている）:

- 2 秒で memory 約 1.0〜1.2 GB、reductions 約 3.4 億
- 4 秒で約 1.5〜5.2 GB（同じ手順でも最も振れた点）
- 6 秒で約 2.9〜3.2 GB
- 8 秒時点でも終了しない

仕様の根拠:

- draft-06 core (`draft-wright-json-schema-01`) §8「A schema MUST NOT be run into an infinite loop against a schema.」
- 同 §11 Security considerations「Instances and schemas are both frequently written by untrusted third parties, to be deployed on public Internet servers. Validators should take care that the parsing of schemas doesn't consume excessive system resources. Validators MUST NOT fall into an infinite loop.」

同 §8 には「Schemas SHOULD NOT make use of infinite recursive nesting like this; the behavior is undefined.」もありスキーマ側の責務にも触れているが、validator 側の MUST は別に定められている。

テストスイートに循環スキーマのケースは無い。`test/JSON-Schema-Test-Suite/tests/draft6/infinite-loop-detection.json` は「兄弟分岐で同じ (スキーマ位置, データ位置) を 2 回評価することは循環ではない」という正常系のみで、循環は未カバー。

## 設計方針

- 再入検出のキーは「解決先スキーマ」と「そのスキーマで評価しているインスタンス値」の組にする。`jsone_schema_state:get_current_path/1` はエラー報告用の検証パスであり、インスタンス位置として使ってはならない。`check_dependency/4`（map 形式の dependencies）はインスタンスを降下させずにパスだけを積み、`check_contains/3` はパスを積まずにインスタンスだけを降下させる。パスとインスタンス位置が一致しないため、どちらの用途にも `get_current_path/1` は使えない
- 検出したら検証全体を打ち切る。`jsone_schema_validator:run_subschema/3` は `throw:{?ERRORS, _}` を分岐失敗に変換するため、既存の `?ERRORS` で投げると `not` / `anyOf` / `oneOf` / `allOf` の下で握り潰される。専用の throw タグを設け、`run_subschema/3` では捕捉せず `jsone_schema:do_validate/4` まで伝播させ、`do_validate/4` の catch に専用タグを追加して `{error, [...]}` に変換する。この変換を入れないと API の戻り値にならず未捕捉例外で呼び出し元プロセスが落ちる
- 循環する入力では `jsone_schema_error:add_reason/2` に到達しない。`run_subschema/3` が毎回 `reset_errors/1` するためエラーが積まれず、既定の `max_errors => 1` でも `max_errors => infinity` でも throw による打ち切りは起きない。停止性は再入検出（と深さ上限）だけで担保し、検出側で明示的に打ち切ること
- 検出用のスタックは「現在の解決経路」として持つ。訪問済み集合にすると `test/JSON-Schema-Test-Suite/tests/draft6/infinite-loop-detection.json` が要求する「兄弟分岐で同じ (スキーマ位置, データ位置) を 2 回評価する」正常系を誤検出する
- インスタンスが降下する正当な再帰（`test/JSON-Schema-Test-Suite/tests/draft6/ref.json` の "root pointer ref" = `{"properties": {"foo": {"$ref": "#"}}, "additionalProperties": false}` と入れ子データ）を通すため、キーにインスタンス値の違いが反映されること
- `#{<<"contains">> => #{<<"$ref">> => <<"#">>}}` はインスタンスが降下する正当な入力（現状 `{ok, [[1]]}` を返す）なので誤検出しないこと
- 取りこぼしの保険として、`$ref` 解決スタックの長さに上限を設ける。`jsone_schema.hrl` に `-define(REF_STACK_LIMIT, 1000).` として定義し、`jsone_schema_validator:check_ref/3` から参照する。`options` には出さない（公開 API を増やさず、0007 が定める API ごとの許可キーにも影響させないため）。超過時は再入検出と同じ専用タグで打ち切って `{error, _}` を返す。`jsone_schema_error:schema_invalid/2` の通常経路で積むと `not` / `anyOf` / `oneOf` の下で `run_subschema/3` に吸われて `{ok, _}` になり得るため、再入検出と同じ打ち切り経路に載せる。上限を超える深さの入れ子データは正当でも `{error, _}` になる。資源消費を抑えるための保険としてこの値を選び、根拠と挙動をコメントに明記する
- 検出時と上限超過時のエラーは `jsone_schema_error:reason()` の形で組み立てる。`jsone_schema.hrl` に `?ref_cycle`（再入検出）と `?ref_depth_limit`（深さ上限超過）の 2 つの理由を追加し、どちらも `kind => schema`、`error => ?ref_cycle` / `error => ?ref_depth_limit` の形にする。2 つを区別するのは、利用者がスキーマの循環と、正当だが深すぎるデータを切り分けられるようにするため。この経路は `jsone_schema_error:add_reason/2` の上限判定を通さずに throw する
- スタックは `jsone_schema_state` の専用フィールドとして追加し、既存の戻り値の形を変えない。`run_subschema/3` と `resolve_ref/2` の整理は別 issue（0016）が扱うため、変更範囲を最小限にする
- スタックは `check_ref/3` の `$ref` 解決の入口で積み、解決から戻るときに降ろす。`?ERRORS` の throw で抜ける経路では state ごと捨てられるため積んだスタックが残る実害は無いが、`check_ref/3` の全戻り経路で降ろすことを不変条件とする。これにより「現在の解決経路」だけを保持し、兄弟分岐で同じ (スキーマ位置, データ位置) を 2 回評価しても再入と誤検出しない。`reset_errors/1` / `restore/2` / `undo_resolve_ref/2` はこのフィールドを変更しない。深さ上限の判定にはスタックの長さを使い、解決の総回数は数えない。別 issue（0019）が扱う `schemas` / `index` の引き継ぎとは独立したフィールドにする
- 別 issue（0019）が `?ERRORS` の throw の形を `{?ERRORS, Errors, State}` に変え、`run_subschema/3` と `jsone_schema:do_validate/4` の捕捉を書き換える。この issue が追加する専用タグはキャッシュの引き継ぎとは独立であり、`run_subschema/3` の catch 節に追加しない方針も変えない。どちらを先に実装しても成立する
- `$id` アンカーや埋め込みリソースを跨ぐ `$ref` の解決が失敗している現状では、循環検出に到達する前に `schema_not_found` になりうる。`$id` を含む循環スキーマの再入検出は別 issue（0003）の実装後に確認する。実装順は 0003 を先にする

## 完了条件

- 次の入力に対して `test/jsone_schema_tests.erl` のテストが返り、戻り値が `{error, [#{kind := schema, error := ?ref_cycle, ...}]}` になる（`{ok, _}` にはならない）
  - `#{<<"$ref">> => <<"#">>}`
  - 相互参照（definitions/a ⇄ b）
  - `#{<<"allOf">> => [#{<<"$ref">> => <<"#">>}]}`
  - `#{<<"dependencies">> => #{<<"a">> => #{<<"$ref">> => <<"#">>}}}` と `#{<<"a">> => 1}`
- 循環の検出が `not` / `anyOf` / `oneOf` / `allOf` の下でも API の戻り値として現れる（`{"not": {"$ref": "#"}}` が `{ok, _}` ではなく `{error, [#{kind := schema, error := ?ref_cycle, ...}]}` になる）
- `max_errors => infinity` でも停止する
- `$ref` 解決スタックが `?REF_STACK_LIMIT` を超えた場合も `{error, [#{kind := schema, error := ?ref_depth_limit, ...}]}` で停止する
- 次の正当な入力が引き続き成功する
  - `test/JSON-Schema-Test-Suite/tests/draft6/ref.json` の "root pointer ref" = `{"properties": {"foo": {"$ref": "#"}}, "additionalProperties": false}` と `{"foo": {"foo": false}}`
  - `#{<<"contains">> => #{<<"$ref">> => <<"#">>}}` と `[[1]]`
- `test/jsone_schema_tests.erl` に `ref_cycle_test/0` を追加する。循環検出の 4 入力、`not` / `anyOf` / `oneOf` / `allOf` の下での検出、`max_errors => infinity`、深さ上限超過、正当な再帰の 2 入力を含める
- 追加した理由を持つエラーで `jsone_schema_error:to_json/1` がクラッシュしない（0018 の `error_to_json_test/0` と同じ観点）
- `test/JSON-Schema-Test-Suite/tests/draft6` の 702 ケースと `test/prop_jsone_schema.erl` の全プロパティが引き続き通る
- `./rebar3 xref` / `./rebar3 dialyzer` / `./rebar3 as test eunit` / `./rebar3 as test proper` が通る

## 解決方法

{未着手}
