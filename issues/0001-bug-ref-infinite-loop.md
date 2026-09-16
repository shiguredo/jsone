# $ref の自己参照・相互参照で検証が停止しない

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-ref-infinite-loop
- Polished: {YYYY-MM-DD}

## 目的

`jsone_schema:validate/2,3` がスキーマの `$ref` 循環で停止しなくなり、呼び出しプロセスのメモリを食い尽くす問題を修正する。コードレビューで致命的と判断した項目。

JSON Schema draft 6 の仕様は validator 側の責務として無限ループの禁止を MUST で定めている。`schema_loader` オプションや `load_schemas/1` は外部スキーマを読み込む経路であり、信頼できないスキーマを与えられただけでノードが落ちる。

## 現状

`src/jsone_schema_validator.erl` の `check_ref/3` は `jsone_schema_state:resolve_ref/2` で `$ref` を解決したあと `validate_with_state/3` を再帰呼び出しする。解決先が同じスキーマ・同じデータ位置に戻ってくる場合の再入検出が無い。`undo_resolve_ref/2` は再帰から戻ったあとに呼ばれるため末尾再帰にもならず、スタックとヒープが伸び続ける。

再現:

```erlang
%% 停止せず、メモリ使用量が増え続ける
jsone_schema:validate(#{<<"$ref">> => <<"#">>}, 1).

%% 相互参照でも同様
S = #{<<"definitions">> =>
          #{<<"a">> => #{<<"$ref">> => <<"#/definitions/b">>},
            <<"b">> => #{<<"$ref">> => <<"#/definitions/a">>}},
      <<"$ref">> => <<"#/definitions/a">>},
jsone_schema:validate(S, 1).
```

計測結果（OTP 29.0、ローカル、`erlang:process_info/2` で計測）:

- 2 秒時点で memory 約 1.0 GB、reductions 約 3.4 億
- 4 秒時点で memory 約 2.5 GB
- 6 秒時点で memory 約 2.9 GB
- 8 秒時点でも終了しない

仕様の根拠:

- draft-06 core (`draft-wright-json-schema-01`) §8「A schema MUST NOT be run into an infinite loop against a schema.」
- 同 §11 Security considerations「Instances and schemas are both frequently written by untrusted third parties ... Validators MUST NOT fall into an infinite loop.」

なお同 §8 には「Schemas SHOULD NOT make use of infinite recursive nesting like this; the behavior is undefined.」もあり、スキーマ側の責務にも触れているが、validator 側の MUST は別に定められている。

テストスイートの `tests/draft6/infinite-loop-detection.json` は「同じスキーマ位置・同じデータ位置を 2 回評価することは無限ループではない」という正常系のみで、スキーマ水準の循環は未カバー。

## 設計方針

- `jsone_schema_state` に `$ref` 解決中のスタック（解決先スキーマの同一性とデータ位置の組）を持たせ、同じ組への再入を検出したら schema エラーとして返す
- 単純な訪問済み集合ではなく「現在の解決経路」で判定する。データが降下する正当な再帰（`tests/draft6/infinite-loop-detection.json` が要求する形）を壊さないため
- 深さ上限だけに頼らない。正当な深い再帰を誤検出するため、併用する場合も十分に大きい値にする

## 完了条件

- `#{<<"$ref">> => <<"#">>}` と相互参照のスキーマが、有限時間で `{error, ...}` を返す（停止しない・メモリを消費し続けない）
- 上記 2 ケースの回帰テストが `test/jsone_schema_tests.erl` に追加されている
- `tests/draft6` の 702 ケースと `test/prop_jsone_schema.erl` の全プロパティが引き続き通る

## 解決方法

{未着手}
