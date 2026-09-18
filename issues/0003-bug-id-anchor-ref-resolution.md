# $id アンカーと埋め込みリソースを跨ぐ $ref が解決できない

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-id-anchor-ref-resolution
- Polished: 2026-09-18

## 目的

draft 6 の `$id`（プレーンネームフラグメントによるアンカー、基準 URI を変える埋め込みリソース、相対 URI のルート `$id`）を使ったスキーマで `$ref` の解決に失敗し、正しいスキーマが schema エラーになる問題を修正する。

## 現状

原因は 3 つある。

1. `src/jsone_schema_index.erl` の `build/2` が `walk/5` の仮引数順と食い違っている。`build(JsonSchema, DocumentURI) -> walk(JsonSchema, DocumentURI, DocumentURI, JsonSchema, #{})` に対して `walk/5` は `(JsonSchema, Base, RootSchema, DocumentURI, Acc0)` を取るため、索引エントリの `root_schema` にドキュメント URI、`document_uri` にルートスキーマ（map）が入る。実測では、ルートの `$id` が絶対 URI でドキュメント URI が定まる場合は `jsone_schema_index:entry_root_schema/1` が `<<"http://example.com/root.json">>`、`entry_document_uri/1` がルートスキーマの map を返し、ドキュメント URI が `undefined` の場合（ルートに絶対 `$id` が無い場合）は `entry_root_schema/1` が `undefined` になる。いずれの場合も `root_schema` と `document_uri` の位置が入れ替わっている。`state_from_entry/2` はこの値で状態を作り直すため、アンカー経由で入ったサブスキーマ内では `resolve_document/2` の同一文書判定が常に不一致になり、外部ドキュメントの読み込みに落ちる
2. `src/jsone_schema_uri.erl` の `resolve/2` は `uri_string:resolve/2` が失敗したときに `resolve_opaque/2` へフォールバックし、`#` で始まる参照を基準 URI へ単純連結する。フォールバックが起きるのは基準 URI が絶対 URI として解決できない場合で、基準が scheme を持たない `#a` や `foo.json` では `uri_string:resolve/2` が `{error, invalid_scheme, []}` を返す（どちらも同じ）。RFC 3986 §5.2.2 ではフラグメントを置換すべきところを連結するため `#a#b` のような壊れた URI になる。基準 URI が絶対 URI（`http://example.com/root.json#a` など）の場合は `uri_string:resolve/2` が成功してフラグメントが置換されるため、この問題は起きない。`resolve/2` は `$ref` だけでなく `$id` の解決（`enter_schema/2` / `index_id/5`）からも呼ばれる
3. `src/jsone_schema_state.erl` の `resolve_document/2` は `document_uri` の一致か `schemas` / ローダのみを見るため、基準 URI が変わった埋め込みリソースや相対 `$id` のルートを外部ドキュメントとして読み込もうとする。`jsone_schema_index:lookup/2` は `resolve_ref/2` の識別子解決（`lookup_identifier/3` と `lookup_loaded_identifier/3`）からしか呼ばれない

実測した再現（5 パターン、いずれも現行コードで失敗する）。パターン 1〜3 は `allOf` 経由のため最外殻は `kind => data` で、内側の `all_schemas_not_valid` に `kind => schema` の `schema_not_found` が入る。パターン 4・5 は最外殻が `kind => schema`:

```erlang
%% 1. アンカーからアンカー（ルートに絶対 $id が無い）
S1 = #{<<"definitions">> =>
           #{<<"A">> => #{<<"$id">> => <<"#a">>,
                          <<"properties">> => #{<<"x">> => #{<<"$ref">> => <<"#b">>}}},
             <<"B">> => #{<<"$id">> => <<"#b">>, <<"type">> => <<"integer">>}},
       <<"allOf">> => [#{<<"$ref">> => <<"#a">>}]},
jsone_schema:validate(S1, #{<<"x">> => 1}).
%% {error, [#{kind => data, error => {all_schemas_not_valid,
%%     [#{kind => schema, error => {schema_not_found, <<"#a#b">>}}]}}]}
%% 原因 2

%% 2. アンカー配下からドキュメントルートのポインタ（ルートに絶対 $id がある）
S2 = #{<<"$id">> => <<"http://example.com/root.json">>,
       <<"definitions">> =>
           #{<<"A">> => #{<<"$id">> => <<"#a">>,
                          <<"properties">> =>
                              #{<<"x">> => #{<<"$ref">> => <<"#/definitions/B">>}}},
             <<"B">> => #{<<"type">> => <<"integer">>}},
       <<"allOf">> => [#{<<"$ref">> => <<"#a">>}]},
jsone_schema:validate(S2, #{<<"x">> => 1}).
%% {error, [#{kind => data, error => {all_schemas_not_valid,
%%     [#{kind => schema, error => {schema_not_found, <<"http://example.com/root.json">>}}]}}]}
%% 原因 1

%% 3. アンカー配下からドキュメントルートのポインタ（ルートに絶対 $id が無い）
S3 = #{<<"definitions">> =>
           #{<<"A">> => #{<<"$id">> => <<"#a">>,
                          <<"properties">> =>
                              #{<<"x">> => #{<<"$ref">> => <<"#/definitions/B">>}}},
             <<"B">> => #{<<"type">> => <<"integer">>}},
       <<"allOf">> => [#{<<"$ref">> => <<"#a">>}]},
jsone_schema:validate(S3, #{<<"x">> => 1}).
%% {error, [#{kind => data, error => {all_schemas_not_valid,
%%     [#{kind => schema, error => {schema_not_found, <<"#a#/definitions/B">>}}]}}]}
%% 原因 1 と 2

%% 4. 埋め込みリソース内から自分のポインタ
S4 = #{<<"$id">> => <<"https://example.com/bundle.json">>,
       <<"definitions">> =>
           #{<<"user">> =>
                 #{<<"$id">> => <<"https://example.com/user.json">>,
                   <<"definitions">> => #{<<"name">> => #{<<"type">> => <<"string">>}},
                   <<"properties">> =>
                       #{<<"name">> => #{<<"$ref">> => <<"#/definitions/name">>}}}},
       <<"properties">> => #{<<"user">> => #{<<"$ref">> => <<"#/definitions/user">>}}},
jsone_schema:validate(S4, #{<<"user">> => #{<<"name">> => <<"x">>}}).
%% {error, [#{kind => schema, error => {schema_not_found, <<"https://example.com/user.json">>}}]}
%% 原因 3

%% 5. 相対 $id のルートからポインタ
S5 = #{<<"$id">> => <<"foo.json">>,
       <<"definitions">> => #{<<"a">> => #{<<"type">> => <<"integer">>}},
       <<"properties">> => #{<<"p">> => #{<<"$ref">> => <<"#/definitions/a">>}}},
jsone_schema:validate(S5, #{<<"p">> => 1}).
%% {error, [#{kind => schema, error => {schema_not_found, <<"foo.json">>}}]}
%% 原因 3
```

仕様の根拠:

- draft-06 core (`draft-wright-json-schema-01`) §5「Fragment identifiers matching the JSON Pointer syntax, including the empty string, MUST be interpreted as JSON Pointer fragment identifiers.」
- 同 §9.2「The "$id" keyword defines a URI for the schema, and the base URI that other URI references within the schema are resolved against.」
- 同 §9.1 初期基準 URI と §9.2 の SHOULD（ルートスキーマの `$id` は scheme を含む URI とすることを仕様が推奨している）
- RFC 3986 §5.2.2 Transform References（フラグメントの置換）

テストスイート `test/JSON-Schema-Test-Suite/tests/draft6/ref.json` はアンカー 1 個または絶対 URI の埋め込みリソースのみを扱い、アンカー配下からの追加参照を網羅していない。`refRemote.json` のポインタはすべてルートスキーマ基準で書かれている。702 ケース通過ではこの issue の欠陥は検出できない。

## 設計方針

- `jsone_schema_index:build/2` の呼び出しを `walk(JsonSchema, DocumentURI, JsonSchema, DocumentURI, #{})` に直し、索引エントリの `root_schema` にルートスキーマ、`document_uri` にドキュメント URI が入るようにする。これが原因 1 の修正であり、アンカー配下から `#` や `#/definitions/...` でドキュメントルートを指す参照が通るようになる
- 埋め込みリソースの索引引きは、`resolve_ref/2` の識別子解決（`lookup_identifier/3`）で `resolve_document/2` を呼ぶ前に行う。`DocumentURI` が空でない場合は、まず `jsone_schema_index:lookup/2` で `DocumentURI` を鍵に埋め込みリソースのエントリを探し、見つかればそのエントリのスキーマをルートとして JSON Pointer を評価する。ルートに使うのは `jsone_schema_index:entry_schema/1`（その URI が識別するスキーマそのもの）であり、`entry_root_schema/1`（ドキュメントルート）ではない。ドキュメント URI 自身のエントリでは両者が一致するためパターン 2 の解決は変わらず、埋め込みリソースの `$id` のエントリでは `entry_schema/1` が埋め込みリソース自身になるためパターン 4 の `#/definitions/name` がリソース内で解決される。`entry_root_schema/1` をルートにすると `jsone_schema_json_pointer:eval_path(<<"/definitions/name">>, ...)` が `not_found` になりパターン 4 は直らない。見つからない場合は既存の `resolve_document/2` にフォールバックする。これは外部ドキュメントの `$ref`（`refRemote.json` が扱う経路）を維持するためであり、フォールバックが起きたこと自体はエラーではない。索引に無い鍵で外部ドキュメントを読もうとした場合は現行どおり `schema_not_found` になる。別 issue（0019）が `resolve_document/2` の失敗時に `resolve_pointer/4` へフォールバックさせない設計にしているため、索引引きを `resolve_pointer/4` ではなく `resolve_document/2` より前の識別子解決に置く。この置き場所なら 0019 の変更後もパターン 4・5 が直る。解決先として返すのは `eval_path/2` が返すサブスキーマであり、エントリのスキーマをそのまま返してはならない（参照先が誤り、パターン 2・4・5 が `{ok, _}` になってしまう）。`DocumentURI` が空の場合はこの索引引きを行わず、現在のルートスキーマを使う既存の解決順を変えない（パターン 3 がこれに依存する）。`jsone_schema_uri:split_fragment/1` の第 1 要素は常にバイナリなので、`DocumentURI` が `undefined` の場合の節は作らない（dialyzer が到達不能節として警告する）。`document_uri` と一致する場合も、`schemas` マップ、ローダという既存の解決順は変えない（`schemas` オプション経路がこれに依存する）
- 索引引きで見つかった場合の状態は、`jsone_schema_state:state_from_entry/2` と同じく `root_schema` / `document_uri` / `base_uri` をエントリに合わせ、`schemas` と `index` は呼び出し元の状態のものを引き継ぐ（`jsone_schema_state:restore/2` と同じ規則）。`base_uri` には `jsone_schema_json_pointer:eval_path/2` が返す訪問経路を `compose_base_uri/2` で合成する。読み込み済みの外部ドキュメントを捨てると、埋め込みリソースと外部ドキュメントを跨ぐ `$ref` が再読み込みになる
- `resolve/2` の `resolve_opaque/2` フォールバックで、基準 URI にフラグメントがある場合は置換する。不透明なストアキー（`user` など）に対するフラグメント参照という従来の用途は維持する。`$id` の解決からも同じ経路を通ることに注意する
- 埋め込みリソース内の JSON Pointer は、基準 URI（埋め込みリソースの `$id`）で識別されるスキーマの中で解決する、と解釈を確定する。draft 6 の本文は埋め込みリソースの概念を明示しておらず解釈が割れ得るため、採用した解釈を実装コメントに残す。公式スイートのポインタはルート基準のみなので、この解釈は 702 ケースの合否に影響しない
- ルートの `$id` が相対 URI の場合（パターン 5 の `foo.json`）は、索引引きで索引の鍵 `foo.json` を引いて解決する。`document_uri/2` が絶対 URI のみを採用する現行条件は変更しない。`absolute_schema_id/1` の `is_absolute/1` 条件を外して相対 `$id` を文書 URI に採用する案は採らない（`$id: "#foo"` のようなフラグメントだけの `$id` を文書 URI として扱うと文書 URI が空になり、影響範囲が必要以上に広がる）。`$id` にフラグメントと他成分を併記した `$id: "foo.json#bar"` は索引の鍵が `foo.json#bar` になり索引引きでも拾えないが、draft-06 core §9.2 の CREF2 がこの形の解釈を未解決の問いとして残しているためこの issue の対象外とし、必要になった時点で別 issue にする。`validate_key/2,3` はストアキーをドキュメント URI にするため影響しないことを確認する
- `jsone_schema_index:entry_root_schema/1` / `entry_document_uri/1` の契約（ルートスキーマとドキュメント URI を返す）をテストで固定し、引数順の取り違えが再発しないようにする
- 実装順は原因 1（`build/2` の引数順）→ 原因 3（識別子解決への索引引きの追加）→ 原因 2（`resolve_opaque/2` のフラグメント置換）とする。原因 1 が直らないと索引の中身が正しくならず、原因 2 の変更だけではパターン 1 が通らないため
- 3 つの原因はいずれも「`$id` を使ったスキーマの `$ref` 解決を成立させる」という 1 つの目的に対する論点であり、分けると中途半端な状態が残るため 1 つの issue で扱う
- `jsone_schema_state:resolve_pointer/4` は別 issue（0019）が `resolve_document/2` の二重呼び出しの解消のために変更する。この issue は `resolve_pointer/4` に索引引きを足さないため、0019 の変更と両立する

## 完了条件

- 上記 5 パターンが次の結果になる
  - パターン 1: `#{<<"x">> => 1}` で `{ok, _}`、`#{<<"x">> => <<"a">>}` で `{error, [#{kind := data, error := ...}]}`
  - パターン 2: `#{<<"x">> => 1}` で `{ok, _}`、`#{<<"x">> => <<"a">>}` で `{error, [#{kind := data, error := ...}]}`
  - パターン 3: `#{<<"x">> => 1}` で `{ok, _}`、`#{<<"x">> => <<"a">>}` で `{error, [#{kind := data, error := ...}]}`
  - パターン 4: `#{<<"user">> => #{<<"name">> => <<"x">>}}` で `{ok, _}`、`#{<<"user">> => #{<<"name">> => 1}}` で `{error, [#{kind := data, error := ...}]}`
  - パターン 5: `#{<<"p">> => 1}` で `{ok, _}`、`#{<<"p">> => <<"x">>}` で `{error, [#{kind := data, error := ...}]}`
- 索引に無い鍵で外部ドキュメントを読もうとした場合（未登録の URI への `$ref`）が、現行どおり `{error, [#{kind := schema, error := {schema_not_found, URI}}]}` になる
- 外部ドキュメントを `schema_loader` で読み込むスキーマで、埋め込みリソースを跨いだ後に外部ドキュメントの `$ref` を解決しても `schema_loader` の呼び出しが増えない。この条件は別 issue（0019）の修正を前提とする。`lookup_loaded_identifier/3` が読み込み済みの state を捨てて `resolve_pointer/4` が `resolve_document/2` を呼び直す経路は 0019 が解消するため、0003 単独では満たせない。0003 の範囲では、索引引きで見つかった場合に `schemas` と `index` を呼び出し元の state から引き継ぐことを検査する
- `test/jsone_schema_tests.erl` に `anchor_cross_ref_test/0` を追加してパターン 1〜5 を検査する。`anchor_ref_test/0` / `resource_ref_test/0` / `anchor_with_base_uri_change_test/0` は変更しない
- `test/jsone_schema_tests.erl` に `schema_index_entry_test/0` を追加して索引エントリの契約を検査する（`entry_root_schema/1` がルートスキーマ、`entry_document_uri/1` がドキュメント URI を返し、ドキュメント URI が `undefined` の場合の `entry_document_uri/1` も `undefined` になる）
- 回帰テストは `?assertMatch({error, _}, ...)` ではなく `{ok, _}` / `{error, [#{kind := data, error := ...}]}` まで検査している
- `test/JSON-Schema-Test-Suite/tests/draft6` の 702 ケースと `test/prop_jsone_schema.erl` の全プロパティが引き続き通る
- `./rebar3 xref` / `./rebar3 dialyzer` / `./rebar3 as test eunit` / `./rebar3 as test proper` が通る

## 解決方法

{未着手}
