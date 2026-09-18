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

計測例（OTP 29.0、ローカル。GC のタイミングで変動するため幅を持たせている）:

- 2 秒で memory 約 1.0〜1.2 GB、reductions 約 3.4 億
- 4 秒で約 2.5〜2.8 GB
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
- 取りこぼしの保険として、`$ref` 解決スタックの長さに上限を設ける。定数（既定 1000）とし、超過時は再入検出と同じ専用タグで打ち切って `{error, _}` を返す。`jsone_schema_error:schema_invalid/2` の通常経路で積むと `not` / `anyOf` / `oneOf` の下で `run_subschema/3` に吸われて `{ok, _}` になり得るため、再入検出と同じ打ち切り経路に載せる。上限を超える深さの入れ子データは正当でも `{error, _}` になる。資源消費を抑えるための保険としてこの値を選び、根拠と挙動をコメントに明記する
- 検出時と上限超過時のエラーは `jsone_schema_error:reason()` の形（`kind => schema`）で組み立て、専用の理由を `jsone_schema.hrl` に追加する。この経路は `jsone_schema_error:add_reason/2` の上限判定を通さずに throw する
- スタックは `jsone_schema_state` の専用フィールドとして追加し、既存の戻り値の形を変えない。`run_subschema/3` と `resolve_ref/2` は別 issue で整理を予定しているため、変更範囲を最小限にする
- 検出スタックは `check_ref/3` の `$ref` 解決の入口で積み、解決から戻るときに降ろす。これにより「現在の解決経路」だけを保持し、兄弟分岐で同じ (スキーマ位置, データ位置) を 2 回評価しても再入と誤検出しない。`reset_errors/1` / `restore/2` / `undo_resolve_ref/2` はこのフィールドを変更しない。深さ上限の判定にはスタックの長さを使い、解決の総回数は数えない。別 issue が扱う `schemas` / `index` の引き継ぎとは独立したフィールドにする

## 完了条件

- 次の入力が有限時間で停止し、`{error, _}` を返す（`{ok, _}` にはならない）
  - `#{<<"$ref">> => <<"#">>}`
  - 相互参照（definitions/a ⇄ b）
  - `#{<<"allOf">> => [#{<<"$ref">> => <<"#">>}]}`
  - `#{<<"dependencies">> => #{<<"a">> => #{<<"$ref">> => <<"#">>}}}` と `#{<<"a">> => 1}`
- 循環の検出が `not` / `anyOf` / `oneOf` / `allOf` の下でも API の戻り値として現れる（`{"not": {"$ref": "#"}}` が `{ok, _}` ではなく `{error, _}` になる）
- `max_errors => infinity` でも停止する
- `$ref` 解決スタックが深さ上限を超えた場合も `{error, _}` で停止する
- 次の正当な入力が引き続き成功する
  - `test/JSON-Schema-Test-Suite/tests/draft6/ref.json` の "root pointer ref" 相当（`{"properties": {"foo": {"$ref": "#"}}, "additionalProperties": false}` と `{"foo": {"foo": false}}`）
  - `#{<<"contains">> => #{<<"$ref">> => <<"#">>}}` と `[[1]]`
- 上記の回帰テストが `test/jsone_schema_tests.erl` に追加されている
- `test/JSON-Schema-Test-Suite/tests/draft6` の 702 ケースと `test/prop_jsone_schema.erl` の全プロパティが引き続き通る

## 解決方法

{未着手}
