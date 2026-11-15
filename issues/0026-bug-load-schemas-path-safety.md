# `load_schemas/1,2` のパス検証とファイル走査を安全にする

- Created: 2026-11-14
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-load-schemas-path-safety
- Polished: {YYYY-MM-DD}

## 目的

`jsone_schema:load_schemas/1,2` はスキーマディレクトリを再帰的に読み込むが、パスの検証とファイル走査に問題があり、空パスでファイルシステム全体を走査したり、glob メタ文字や symlink で意図しないファイルを読んだり、非 ASCII ディレクトリを黙って 0 件で成功させたりする。入力パスに対して安全で予測可能な挙動にする。

## 現状

`src/jsone_schema.erl` の `load_schemas/2` は `jsone_schema_uri:to_binary/1` の結果を `binary_to_list/1` に通し、`collect_files/2` で `filelib:wildcard/1` を使う。次の挙動を実測した。

- `load_schemas(<<>>)` / `load_schemas("")` で `filename:join([], "**")` が `"/**"` になり、ルートから全ファイルを列挙する。10〜20 秒の timeout でも終了しない。
- ディレクトリ名に `[` などの glob メタ文字があると、パスをパターンとして解釈し、別ディレクトリを読む、または 0 件になる。
- symlink を辿る。`dir/ext -> /tmp/outside` のような symlink があると、入力ディレクトリ外のファイルを読む。自己 symlink では 82 エントリを列挙した。
- 非 ASCII ディレクトリ名を `binary_to_list/1` で UTF-8 バイト列にして `filelib:wildcard/1` に渡すため、存在するディレクトリでも 0 件になる。ASCII ディレクトリ内の非 ASCII ファイル名は通る。
- 存在しないパス、通常ファイル、読めないディレクトリで `ok` を返し、0 件登録する。
- 読めないサブディレクトリがあると、読めるファイルだけ登録して `ok` を返す。「全ファイルの読み込みとパースが成功した場合だけ登録する」という `load_schemas/2` のコメントに反する。

## 設計方針

- `filelib:wildcard/1` にユーザー入力をそのまま渡さない。`file:list_dir/1` を起点に、ディレクトリを明示的に再帰する。
- パスが空、ルート、通常ファイル、存在しない、読めない場合は `{error, {Path, enoent | enotdir | eacces}}` を返す。
- symlink はデフォルトで辿らない。辿る必要がある場合も、realpath が入力ディレクトリ配下にあることを検査する。
- 非 ASCII パスは `unicode:characters_to_list/1` でコードポイント列に変換する。
- 全ファイルの読み込み・パース・登録前検査が成功した場合だけ登録する。読めないサブディレクトリや glob の取りこぼしを黙認しない。
- `recursive => false` でも同じ検証を適用する。

## 完了条件

- `load_schemas(<<>>)`、`load_schemas("/")`、`load_schemas(通常ファイル)`、`load_schemas(存在しないパス)`、`load_schemas(読めないディレクトリ)` が、全ファイル走査を起こさず明確なエラーを返す。
- glob メタ文字を含むディレクトリ名がリテラルとして扱われ、別ディレクトリのファイルを読まない。
- symlink が入力ディレクトリ外のファイルを登録しない。自己 symlink でも有限時間で終わる。
- 非 ASCII ディレクトリ配下のスキーマが登録され、`file://` のパーセントエンコードされたキーで引ける。
- 読めないサブディレクトリがある場合、部分登録せず `{error, {File, Reason}}` を返す。
- 上記の異常系テストは timeout を設定し、全 FS 走査を実際に起こさない形で検査する。
- `xref` / `dialyzer` / `eunit` / `proper` が通る。

## 解決方法

{未着手}
