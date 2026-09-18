# スキーマローダの重複呼び出しとキャッシュ破棄をなくす

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/refactor-schema-loader-caching
- Polished: 2026-09-18

## 目的

外部 `$ref` を含むスキーマで `schema_loader` が同じドキュメントを何度も読み込む無駄をなくす。

## 現状

呼び出し回数の実測（`schema_loader` の呼び出し回数を URI ごとに数える fun を渡し、同じ外部ドキュメント 1 つ（参照先に `$id` なし）を `$ref` で参照するスキーマを `jsone_schema:validate/3` で検証した。既定の `max_errors` は 1。分岐はすべて `$ref`）:

- ルート直下の `$ref`（JSON Pointer 付き）: 2 回
- `anyOf` の 2 分岐: 1 番目が成功して 2 番目を評価しない場合は 2 回、1 番目が失敗して 2 番目が成功する場合は 4 回
- `anyOf` の 3 分岐（全分岐失敗、throw 経路）: 6 回
- `oneOf` の 2 分岐: 4 回
- `contains`（3 要素で全要素不一致）: 6 回（要素数に比例し、5 要素では 10 回）
- `not`: 2 回
- `properties` の 2 プロパティ: 2 回（プロパティごとの再読み込みは起きず、1 つの `$ref` の 2 回がそのまま出る）

原因は 2 つある。

- `src/jsone_schema_state.erl` の `lookup_loaded_identifier/3` は `resolve_document/2` でドキュメントを読み込んだあと索引引きに失敗すると `LoadedState` を捨てて `error` を返す。`resolve_ref/2` は `error` を受けて元の `State` で `resolve_pointer/4` を呼ぶため、`resolve_pointer/4` が `resolve_document/2` を呼び直し、参照全体が成功する場合でも `schema_loader` が 2 回呼ばれる。これが 1 つの `$ref` あたりの 2 回の下限になる
- `src/jsone_schema_validator.erl` の `run_subschema/3` は、正常終了時は `restore/2` が `schemas` / `index` を戻さないため戻り値の state にキャッシュが残るが、`?ERRORS` の throw 経路は `catch throw:{?ERRORS, Errors} -> {error, Errors, State}` で元の state を返すため、読み込んだキャッシュと索引が失われる。既定の `max_errors` が 1 のため、失敗する分岐はこの経路になる。さらに 5 つの呼び出し側（`check_all_of_1/5` の失敗節、`check_any_of_1/5` の成功節と失敗節、`check_one_of_1/5` の成功節と失敗節、`check_not/3` の両方の節、`subschema_valid/3` 経由の `check_contains/3`）が戻り値の state を使わない。`subschema_valid/3` は `run_subschema/3` の結果を boolean に潰すため state を外に出せない

## 設計方針

- 引き継ぐのは `schemas` と `index` だけにする。`root_schema` / `current_schema` / `document_uri` / `base_uri` / `errors` は元の state から戻す（`jsone_schema_state:restore/2` と同じ規則）。失敗した分岐の state をそのまま次の分岐へ渡すと、その分岐のエラー一覧とスキーマが残り、成功する検証が `{error, _}` になる（試作では既存テスト 31 件が落ちる）
- `lookup_loaded_identifier/3` は `{ok, SubSchema, State} | {not_found, State} | {error, Reason}` を返す。索引引きに失敗した場合は読み込み済みの state を `{not_found, State}` で返し、`lookup_identifier/3` はそれをそのまま返し、`resolve_ref/2` は `{not_found, State}` のときその state を `resolve_pointer/4` に渡す（`resolve_document/2` は `schemas` にあれば読み込み直さないため、2 回目の呼び出しが消える）
- `resolve_document/2` が失敗した場合は `{error, Reason}` とし、`resolve_pointer/4` にフォールバックさせず `resolve_ref/2` のエラー経路にそのまま載せる。未登録と読み込み失敗の理由は別 issue が定める形をそのまま使い、0017 の `$id` 衝突もこの経路で伝播させる。`resolve_ref/2` のエラー戻り値は別 issue が定める 2 要素の形をそのまま使う（state は載せない）。埋め込みリソースの索引引きは別 issue（0003）が `resolve_document/2` を呼ぶ前の識別子解決（`lookup_identifier/3`）に置くため、この変更後も索引にある `DocumentURI` は `resolve_document/2` に到達せず、`jsone_schema_index:entry_schema/1` をルートとした JSON Pointer の評価で解決される
- `run_subschema/3` は成功・失敗いずれの場合も `schemas` と `index` だけを引き継ぐ（`restore/2` の規則）。throw 経路は `?ERRORS` にその時点の state を載せて `{?ERRORS, Errors, State}` にし、`run_subschema/3` は受け取った state から `schemas` と `index` だけを取り出して呼び出し元の state に戻す。変更するのは `jsone_schema_error:add_reason/2` の throw 1 箇所と、`run_subschema/3` と `jsone_schema:do_validate/4` の捕捉 2 箇所。この throw の形の変更は別 issue の「throw の経路は現状のままにする」を上書きする
- 呼び出し側を直す。`check_all_of_1/5` の失敗節、`check_any_of_1/5` の成功節と失敗節、`check_one_of_1/5` の成功節と失敗節、`check_not/3` の両方の節で戻り値の state を使い、`check_contains/3` は `subschema_valid/3` を `{Valid, State}` を返す形に変えて要素を走査しながら state を引き継ぐ
- `restore_schema/2` は変更しない。`schemas` / `index` は子スキーマの state のまま残るため、`properties` / `items` などのプロパティごとの再読み込みは起きていない（実測でもプロパティ数によらず 2 回）
- 0017 の衝突検出は `resolve_document/2` の失敗として扱い、衝突したドキュメントは `schemas` に登録せずに `{error, Reason}` を返す（成功時の戻り値の形は変えないため 0018 と両立する）。キャッシュ済みでも `cache_document/3` は `jsone_schema_index:build/2` と `merge_index/2` を実行するが、0017 の「同じスキーマの再索引は許す」規則によりエラーにはならない
- `CHANGES.md` に独立したエントリは追加しない。JSON Schema バリデータは `## develop` の未リリース `[ADD]` の中にあり、この変更はその初回リリース内容に含まれる

## 完了条件

- ルート直下の `$ref`（JSON Pointer 付き、参照先に `$id` なし）で `schema_loader` の呼び出しが 1 回になる
- `anyOf` / `oneOf` の複数分岐がすべて同じドキュメントを `$ref` で参照する場合に、分岐の成否と `max_errors` の設定（1 と infinity）によらず呼び出しが 1 回になる。`not` は単一のサブスキーマなので、`$ref` の解決が 1 回になる
- `contains` は、一致する要素がある入力では 1 回、全要素が不一致の入力では要素数によらず 1 回になる
- `properties` の複数プロパティが同じドキュメントを参照する場合も 1 回になる（1 つの `$ref` の 2 回が消えることを確認する）
- 挙動が変わっていない。成功する検証が `{error, _}` にならない（`any_of_error_test/0` / `one_of_error_test/0` / `contains_test/0` と `test/JSON-Schema-Test-Suite/tests/draft6` の 702 件が通る）
- 呼び出し回数を URI ごとに記録する fun を渡す回帰テストが `test/jsone_schema_tests.erl` に追加されている（既存の `schema_loader_test/0` と同じく fun を渡す形にする）
- 0017 の衝突検出が入っている場合、衝突したドキュメントはキャッシュに残らない
- `./rebar3 xref` / `./rebar3 dialyzer` / `./rebar3 as test eunit` / `./rebar3 as test proper` が通る

## 解決方法

{未着手}
