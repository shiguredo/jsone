# load_schemas が部分適用のまま失敗し file_uri がパスをエスケープしない

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-load-schemas-partial-apply
- Polished: 2026-09-18

## 目的

`jsone_schema:load_schemas/1,2` の失敗時の挙動、生成する `file://` キーのエスケープ、先頭が `.` のファイルを対象に含めるかを正す。

## 現状

- `src/jsone_schema.erl` の `load_schema_files/2` は最初に失敗したファイルで打ち切るが、それまでに `jsone_schema_store:add/2` で登録したスキーマをロールバックしない。グローバルな `persistent_term` に中途半端な状態が残る（実測: `a_ok.json` と壊れた `b_bad.json` を置いて `load_schemas/1` を呼ぶと `{error, {"/.../b_bad.json", {parse_error, unexpected_end}}}` が返り、`a_ok.json` のキーだけが `list_schemas/0` に残る）
- `jsone_schema_store:add/2` は `$id` を持つスキーマで `schema_keys/2` により 2 本のキー（登録キーと `$id` を絶対 URI にしたキー）を登録する。また `add_schema/2` は任意のキーを受け付けるため、`load_schemas/1` のキーは利用者が事前に `add_schema/2` で登録したキーを上書きし得る
- 同 `file_uri/1` は `filename:absname/1` で絶対化したパスに `<<"file://">>` を前置するだけで、パスをパーセントエンコードしない。ファイル名に `#` を含む `a#b.json` では登録キーが `<<"file:///.../a#b.json">>` になり、そのファイルを `$ref` で参照すると `jsone_schema_uri:split_fragment/1` が `#` 以降をフラグメントとして分割し、`{schema_not_found, <<"file:///.../a">>}` になる。`#` を含むディレクトリでは相対 `$ref` も別のディレクトリに解決される（実測: `file:///tmp/.../d#ir/main.json` からの `$ref: "target.json"` が `<<"file:///tmp/.../target.json">>` になる）。空白を含むパスでは、生の空白を書いた `$ref` が `jsone_schema_uri:try_resolve/2` に失敗して `resolve_opaque/2` に落ちるため解決できない。非 ASCII を含む `file://` URI は `jsone_schema_uri:is_absolute/1` が false を返す
- 上記のうち `validate_key/2` のキー直接引きは現状でも通る（登録キーと参照キーがバイト単位で一致するため。実測: `validate_key(<<"file:///.../a#c.json">>, 1)` が `{ok, 1}`）。壊れるのは `$ref` を解決する経路
- 同 `collect_files/2` は `filelib:wildcard/1` を使う。`filelib:wildcard/1` はシェルの glob と違い先頭が `.` のファイルも列挙するため、`.foo.json` のようなファイルも対象になる（OTP 28.1 / 28.3 / 29.0 / 29.1 で実測。`.foo.json` だけを置いたディレクトリで `load_schemas/1` が `ok` を返し、`file:///.../.foo.json` がキーとして登録される）。この挙動はテストで固定されておらず、`collect_files/2` のコメントにも書かれていない

## 設計方針

- 全件の読み込みとパースが成功してから登録する方式にする。1 件でも失敗した場合はストアに一切登録せず、`{error, {File, Reason}}` を返す。削除によるロールバックを選ばない理由は 2 つある。(1) `$id` を持つスキーマは `schema_keys/2` でキーが 2 本になるため、巻き戻しには `jsone_schema_store:delete/1`（公開 API は `del_schema/1`）が必要で、`persistent_term:erase/1` を直接使う実装では `$id` 側のキーを取り残す。(2) `load_schemas/1` のキーは利用者が `add_schema/2` で先に登録したキーを上書きし得るため、削除すると利用者の登録済みスキーマを失う。「読み込み前の状態に戻す」を正しく実装するにはストア内容の退避が必要になり、全件成功後に登録する方式より複雑になる
- `collect_files/2` が返した全ファイルについて、1 周目で `file:read_file/1` とスキーマのパースを実行し、成功したスキーマだけを 2 周目で `jsone_schema_store:add/2` により登録する。1 周目では登録しない。戻り値の形（`ok | {error, {file:filename(), term()}}`）と `File` が生のファイルパスであることは変えない。0018 が定める「`add_schema/3` と `load_schemas/2` の戻り値の形を変えない」と両立する
- `file_uri/1` は `uri_string:quote(Path, "/")` でパスをパーセントエンコードし、`<<"file://", Encoded/binary>>` を組み立てる。`uri_string:quote/1` は `/` も `%2F` にエンコードして `file://%2Ftmp%2F...` のようにパスが authority として解釈される壊れた URI を作るため使わない（実測）。`uri_string:quote/2` に安全文字として `"/"` を渡すと `/tmp/a b#c.json` が `/tmp/a%20b%23c.json`、`/tmp/日本語.json` が `/tmp/%E6%97%A5%E6%9C%AC%E8%AA%9E.json` になる
- エンコードの対象はパス全体とし、対象文字は `#` と空白に加えて非 ASCII など URI でそのまま使えない文字を含む。`filename:absname/1` の呼び順と、クエリ文字列やシンボリックリンクの扱いは現行のままにする
- 登録キーがパーセントエンコード済み URI になるため、`$ref` にはエンコード済みの URI（`#` は `%23`、空白は `%20`）を書く前提とする。生の `#` や空白を `$ref` に書いた場合は `jsone_schema_uri:try_resolve/2` が失敗して `resolve_opaque/2` に落ちるため解決できない。この前提は `load_schemas/2` のドキュメントコメントに明記する。`is_absolute/1` が非 ASCII の URI に対して false を返す件は、`jsone_schema_state` の `absolute_schema_id/1` の扱いを含めてこの issue の対象外とする
- `collect_files/2` は先頭が `.` のファイルを現状どおり対象に含める。挙動を変えず、その旨を `collect_files/2` のコメントに明記する。理由は 3 つある。(1) 現行版で既に読み込まれており、除外すると `.foo.json` を置いていた環境でスキーマが黙って読まれなくなる、(2) 除外には `filelib:wildcard/1` の結果を別途フィルタする実装が必要になり、`recursive` が true と false の両方で分岐が増える、(3) この issue の目的である失敗時の挙動とキーのエスケープとは独立している
- 0017 が対象外とした「`$id` の URI による既存キーの上書き」には踏み込まない。0007 が定める `load_schemas/2` のオプション検証と「ファイルを集める前に検証する」順序も前提としてそのまま使う
- この issue で 3 件をまとめて扱う理由は、いずれも「スキーマファイルをまとめて読み込むときの入力の扱いを決める」という 1 つの目的に対する論点であり、キーワード単位の型検査を扱う 0007（オプションと入力の検証）とは対象が別だからである
- `CHANGES.md` に独立したエントリは追加しない。JSON Schema バリデータは `## develop` の未リリース `[ADD]` の中にあり、この変更はその初回リリース内容に含まれる。README には `load_schemas/1,2` の記載が無いため README は変更しない。代わりに `load_schemas/2` のドキュメントコメントを更新する

## 完了条件

- 途中のファイルが不正な場合、そのファイルより前に読んだファイルのスキーマも含めてストアに登録されない（`load_schemas/1` の呼び出し前後で `list_schemas/0` が同じ内容を返す）。`$id` を持つスキーマの `$id` のキーも登録されない。事前に `add_schema/2` で登録したキーも変化しない（`clear_schemas/0` による全消しは使わない）
- `#` を含むパスでは `file:///.../a%23b.json`、空白を含むパスでは `file:///.../a%20b.json` が `list_schemas/0` のキーになり、そのキーで `get_schema/1` と `validate_key/2` が引ける
- `file://` で始まる登録キーを基準 URI とし、`$ref` にエンコード済みの相対参照（`a%23b.json` / `a%20b.json`）を書いた場合に `validate_key/3` の `$ref` 解決が成功する
- 先頭が `.` のファイルが対象に含まれること（`list_schemas/0` のキーに現れること）がテストで固定され、`collect_files/2` のコメントに挙動が明記されている
- 上記を検査するテストが `test/jsone_schema_tests.erl` の `load_schemas_test/0` に追加されている。テストは `clear_schemas/0` で初期化し `try ... after` で後始末する。`file_uri/1` は内部関数でモックも使えないため、実ファイルを置いて `load_schemas/1` を呼び、`list_schemas/0` が返すキーを検査する（キーの組み立て式をテスト側で再実装しない）
- `./rebar3 xref` / `./rebar3 dialyzer` / `./rebar3 as test eunit` / `./rebar3 as test proper` が通る

## 解決方法

{未着手}
