# テストハーネスがサブモジュール未取得時に黙って 0 件になる

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-test-harness-silent-skip
- Polished: {YYYY-MM-DD}

## 目的

JSON-Schema-Test-Suite のサブモジュールを取得していない環境で、draft 6 の 702 ケースが「0 件実行で成功」になり、テストが通ったと誤認される問題を修正する。

## 現状

`test/jsone_schema_draft6_tests.erl` の `setup/0` は `filelib:wildcard/1` でテストファイルを集める。サブモジュール未取得（`test/JSON-Schema-Test-Suite` が空）の場合は空リストが返り、`build_tests/1` が空のテスト集合を返すため EUnit は 0 件で成功する。リモートスキーマの読み込みも同様に空になる。

さらに `test_dir/0` のフォールバック候補 `filename:join([code:lib_dir(jsone), "..", "..", "..", "test"])` は `_build/test/lib/jsone/../../../test` = `_build/test` を指す。`filelib:is_dir/1` が真になるため候補として採用され、プロジェクトルート以外から実行された場合はテストデータが見つからず、この場合も 0 件で成功する。

再現手順:

1. `test/JSON-Schema-Test-Suite` を空のまま `./rebar3 as test eunit` を実行する
2. 0 件で成功する（本来は失敗すべき）

## 設計方針

- `setup/0` でテストファイル数とリモートスキーマ数が 0 でないことを検査し、0 件なら `erlang:error/1` で失敗させる
- `test_dir/0` のフォールバックをリポジトリルートの `test` を指す正しいパスに直すか、候補を削除してカレントディレクトリ前提を明示する

## 完了条件

- サブモジュール未取得の状態でテストを実行すると、0 件成功ではなく失敗する
- 正しいチェックアウトでは従来どおり 702 件が実行される
- 上記を確認する手順がテストモジュールのコメントに書かれている

## 解決方法

{未着手}
