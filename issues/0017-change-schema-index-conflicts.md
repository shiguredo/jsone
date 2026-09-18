# 同一 URI の $id 重複とスキーマ妥当性検査の範囲を決める

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/change-schema-index-conflicts
- Polished: 2026-09-18

## 目的

同一 URI を指す複数の `$id` を黙って上書きしている挙動と、スキーマの妥当性検査をどこまで行うかを決めて明文化する。

どちらも「スキーマ側の入力の扱いを決める」という 1 つの目的に対する 2 つの論点であり、キーワード単位の型検査を扱う別 issue（オプションと入力の検証、format の方針）とは対象が別。

## 現状

- `$id` が同じ URI を指すとき、後から来たスキーマで黙って上書きされる経路が 2 つある
  - 別ドキュメントの統合: `src/jsone_schema_state.erl` の `merge_index/2` が `maps:merge/2` で索引を統合する（後勝ち）
  - 同一ドキュメント内: `src/jsone_schema_index.erl` の `index_id/5` が `Acc#{Absolute => Entry}` で同じ URI を後勝ちにする
  - なお `jsone_schema_store:add/2` も `$id` の URI で登録するため同じ URI を上書きするが、これは利用者が明示的に登録し直す操作なのでこの issue の対象外とする
- draft-06 core §9.2.2 は「A schema MAY (and likely will) have multiple URIs, but there is no way for a URI to identify more than one schema. When multiple schemas try to identify with the same URI, validators SHOULD raise an error condition.」としている。条件は「複数のスキーマが同じ URI を名乗るとき」であり、同じスキーマが複数回索引される場合は含まない。`merge_index/2` は `maps:merge/2` の後勝ちなので、同じスキーマが再索引されても上書きが起きる
- `src/jsone_schema_index.erl` の `walk_schema_maps/5` は `$defs` を索引対象に含めているが、`$defs` は draft 6 には無いキーワード（draft 2019-09 以降）。モジュールのコメントは「未知のキーワードの中にある `$id` は対象にしない」としており記述と実装が食い違う。`$defs` の索引に依存しているのは `test/jsone_schema_tests.erl` の `ignored_id_test/0` だけでなく、公式スイートの `tests/draft6/unknownKeyword.json` の「type matches second anyOf, which has a real schema in it」も該当する（`$defs` の中の `$id` でしか解決できない `$ref` を持つ）
- スキーマの妥当性検査は個別キーワードの型検査に留まっており、`required` の要素重複（validation-01 §6.17「Elements of this array, if any, MUST be strings, and MUST be unique.」）と `type` 配列の要素重複（同 §6.25「If it is an array, elements of the array MUST be strings and MUST be unique.」）は検査していない。なお `test/meta-schemas/draft-06.json` は公式の draft-06 メタスキーマと一致しているが、`test/` 配下のテスト用フィクスチャであり、配布物（`priv/`）には含まれない

## 設計方針

- `$id` の衝突は索引を作る時点で検出し、schema エラーにする。このライブラリに警告を返す仕組みは無く（`validate/2,3` は `{ok, _}` か `{error, _}` のみ、`logger` の使用も無い）、§9.2.2 も error condition を求めているため、警告は選択肢にしない
- 検出するのは「同じ URI に**異なるスキーマ**が対応する場合」だけにする。索引に同じ URI が既にあるとき、スキーマが `=:=` で等しければ再索引として許し、異なれば衝突として扱う。キーの有無だけで判定すると、ストア経由で同じスキーマが 2 回索引される正当な経路を誤って拒否する
- 検出は `jsone_schema_index:build/2`（同一ドキュメント内）と `jsone_schema_state:merge_index/2`（別ドキュメントの統合）の両方に入れる。どちらも衝突した URI を返し、`resolve_ref/2` のエラー経路で `check_ref/3` まで伝播させて schema エラー（`kind => schema`）にする。専用の reason を `jsone_schema.hrl` に追加する
- `$defs` は前方互換のための拡張として索引対象に残す。索引から外すと公式スイートの `unknownKeyword.json` の合格済みケースが落ちるため、実装ではなくモジュールのコメントを直し、未知のキーワード一般は対象にしないが `$defs` だけは拡張として対象にすることを明記する
- スキーマの妥当性検査は個別キーワードの型検査に留め、メタスキーマ検証は提供しない。あわせて `required` / `type` の要素重複の検査も追加しない。理由は、(1) メタスキーマはテスト用フィクスチャで配布物に含まれず、提供するなら配置から変える必要がある、(2) 重複を許す入力を生成している既存の PropEr の性質（`prop_jsone_schema.erl` の `prop_required/0`）が落ちる、(3) 現行の方針（キーワードの値の型不正は検出するが、値の意味的な制約までは見ない）と一貫させられる。この範囲を README に明記する
- `CHANGES.md` に独立したエントリは追加しない。JSON Schema バリデータは `## develop` の未リリース `[ADD]` の中にあり、この変更はその初回リリース内容に含まれる

## 完了条件

- 同じ URI を異なるスキーマが名乗ったときに schema エラーになる。`merge_index/2` の経路（外部 `$ref` を別ドキュメントで解決する場合）と `index_id/5` の経路（1 つのドキュメント内で `$id` が重複する場合）の両方で確認できる
- 同じスキーマが複数回索引される場合はエラーにならない（衝突の判定はキーの有無ではなくスキーマの同一性で行う）。この規則を検査するテストを追加し、`jsone_schema:add_schema/2` で登録したスキーマを `validate_key/2` で検証する経路が引き続き成功することも確認する
- `$defs` が索引対象のままで、`jsone_schema_index` のモジュールコメントが実装と一致している（未知のキーワード一般は対象にしないが、`$defs` は前方互換の拡張として対象にする）
- README に、メタスキーマ検証を提供しないことと、スキーマの検査は個別キーワードの型検査に留めること（`required` と `type` 配列の要素重複は検査しない）が書かれている
- 上記の衝突検出と再索引の許容を検査するテストが `test/jsone_schema_tests.erl` に追加されている
- `ignored_id_test/0` と `test/JSON-Schema-Test-Suite/tests/draft6` の 702 件（`unknownKeyword.json` を含む）が引き続き通る
- `./rebar3 xref` / `./rebar3 dialyzer` / `./rebar3 as test eunit` / `./rebar3 as test proper` が通る

## 解決方法

{未着手}
