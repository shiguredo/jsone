# load_schemas が部分適用のまま失敗し file_uri がパスをエスケープしない

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-load-schemas-partial-apply
- Polished: {YYYY-MM-DD}

## 目的

`jsone_schema:load_schemas/1,2` の失敗時の挙動と、生成する `file://` キーの扱いを正す。

## 現状

- `src/jsone_schema.erl` の `load_schema_files/2` は最初に失敗したファイルで打ち切るが、それまでに `jsone_schema_store:add/2` で登録したスキーマをロールバックしない。グローバルな `persistent_term` に中途半端な状態が残る
- 同 `file_uri/1` は `<<"file://", AbsolutePath/binary>>` を組み立てるだけでパスをエスケープしない。ファイル名に `#` が含まれると、その URI を `$ref` として解決するときに `jsone_schema_uri:split_fragment/1` が `#` 以降をフラグメントとして扱い、登録したキーと一致しなくなる。空白を含むパスも URI として不正になる
- 同 `collect_files/2` は `filelib:wildcard/1` を使うため、先頭が `.` のファイル（`.foo.json` など）を列挙しない

## 設計方針

- 失敗時に登録済みのキーを削除するか、全件の読み込みとパースが成功してから登録する
- `file_uri/1` は `uri_string:quote/1` などでパスをパーセントエンコードする。エンコードしたキーで `validate_key/2` から引けることをテストで担保する
- 先頭が `.` のファイルを対象にするかどうかを決め、挙動をコメントに明記する

## 完了条件

- 途中のファイルが不正な場合に、登録済みのスキーマが残らない（またはその方針が明記されている）
- `#` や空白を含むディレクトリ・ファイル名でも `load_schemas/1` の登録キーで `validate_key/2` が引ける
- 上記を検査するテストが追加されている

## 解決方法

{未着手}
