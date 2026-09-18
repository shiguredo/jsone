# 検証の二次関数的な処理を解消する

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/refactor-quadratic-validation-paths
- Polished: 2026-09-18

## 目的

要素数の多いデータや、失敗するデータで検証時間が急激に伸びる箇所を解消する。

`uniqueItems` は信頼できない入力に対する CPU 消費（DoS の増幅）になりうる。

## 現状

計測は Erlang/OTP 29 (erts-17.1)、ローカル。GC のタイミングで変動するため幅を持たせている。

- `uniqueItems`: `src/jsone_schema_validator.erl` の `has_duplicate/1` は集合で重複の有無を判定したあと、重複がある場合のみ `find_duplicate/1` で総当たり探索する。`find_duplicate/1` は「後に同値が現れる最初の要素」を先頭から順に探すため、重複する値の組が末尾にあるほど遅い
  - 1 万要素（全て異なる）: 3 ms
  - 1 万要素（末尾の 2 要素が同値 = `[1, 2, ..., 10000, 10000]`）: 約 210 ms
  - 1 万要素（先頭の 2 要素が同値 = `[1, 1, 2, ..., 10000]`）: 1 ms
  - 同じ値が末尾に 1 件だけ現れる場合（`[1, 2, ..., 10000, 1]`）は先頭の要素で衝突が見つかるため 1 ms で終わる
  - 10 万要素（末尾の 2 要素が同値）では約 20 秒
- `oneOf`: `check_one_of_1/5` が分岐ごとに `Errors ++ NewErrors` でリストを連結しており O(n²)。分岐ごとに 20 件のエラーが出るスキーマで `max_errors => infinity` の場合、分岐数 1000 / 2000 / 4000 が 34 / 134 / 615 ms（分岐数を倍にすると約 4 倍）
- `max_errors`: `src/jsone_schema_state.erl` の `has_reached_max_errors/1` がエラー追加のたびに `length/1` で件数を数える。費用が出るのは有限かつ 1 より大きい値を指定したときだけで、既定の 1 では 1 件目で throw するため 1 回、`infinity` では `length/1` を呼ばない。32000 件のエラーを集める場合、`max_errors => 1000000` は約 1 秒、`infinity` は約 4 ms
- エラーは `jsone_schema_state:add_error/2` が先頭に積み、`get_errors/1` が反転して時系列順で返す。この 2 つは既に実装済みで、改善対象ではない

## 設計方針

- `uniqueItems` は `has_duplicate/1` と `find_duplicate/1` を 1 パスの探索に置き換える。`jsone_schema_equality:normalize/1` で正規化した値をキーにした map に初出の位置と値を記録し、2 回目以降の出現で衝突を検出する。報告する重複要素は現行どおり「後に同値が現れる最初の要素」とし、衝突した候補のうち初出位置が最小のものを選ぶ。`[1, 1.0]` は `1`、`[1, 2, 2, 1]` は `1` を報告する。`find_duplicate/1` は廃止する
- `oneOf` は `check_one_of_1/5` の `Errors ++ NewErrors` をやめ、分岐ごとのエラーリストをチャンクとして逆順に積み、最後に `lists:append(lists:reverse(Chunks))` で 1 回だけ連結する。エラーの順序は現行どおり分岐順を保つ
- `max_errors` はエラー件数を state に持って O(1) で判定する。`reset_errors/1` は件数も 0 に戻し、`restore/2` は元の state の件数も戻す。一覧と件数が常に一致することを不変条件にする
- `jsone_schema_state` の「先頭に積んで読み出し時に反転する」方式は変更しない
- `CHANGES.md` に独立したエントリは追加しない。JSON Schema バリデータは `## develop` の未リリース `[ADD]` の中にあり、今回の変更はその初回リリース内容に含まれる
- 計測値は実装後にも同じ環境で再取得し、この issue に追記する

## 完了条件

- `[1, 2, ..., 10000, 10000]`（末尾の 2 要素が同値）の `uniqueItems` が 10 ms 未満で完了する（実装前は約 210 ms）
- `[1, 2, ..., 100000, 100000]` の `uniqueItems` が 1 秒未満で完了する（実装前は約 20 秒）
- 報告する重複要素が変わっていない。`unique_items_test/0` の `{not_unique, 1}` がそのまま通り、`[1, 2, 2, 1]` は `1` を報告する
- `oneOf` の入れ子エラーの順序が変わっていない。`one_of_error_test/0` がそのまま通る
- 分岐数 4000（各分岐 20 件のエラー、`max_errors => infinity`）の `oneOf` が実装前の 1/4 以下の時間で完了する（実装前は約 600 ms）
- `max_errors => 1000000` で 32000 件のエラーを集める場合が、実装前の 1/4 以下の時間で完了する（実装前は約 1 秒）
- `max_errors` の既定 (1) と `infinity` の挙動が変わっていない。`max_errors_test/0` がそのまま通る
- 既存の EUnit / PropEr が引き続き通る
- 実装前後の計測値（環境、対象データ、時間）が issue に記録されている
- `./rebar3 xref` / `./rebar3 dialyzer` / `./rebar3 as test eunit` / `./rebar3 as test proper` が通る

## 解決方法

{未着手}
