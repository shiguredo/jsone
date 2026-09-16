# 検証の二次関数的な処理を解消する

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/refactor-quadratic-validation-paths
- Polished: {YYYY-MM-DD}

## 目的

要素数の多いデータや、失敗するデータで検証時間が急激に伸びる箇所を解消する。コードレビューで重要と判断した項目。

`uniqueItems` は信頼できない入力に対する CPU 消費（DoS の増幅）になりうる。

## 現状

- `uniqueItems`: `src/jsone_schema_validator.erl` の `has_duplicate/1` は集合で重複の有無を判定したあと、重複がある場合のみ `find_duplicate/1` で総当たり探索する。計測結果（OTP 29.0、ローカル）:
  - 1 万要素（全て異なる）: 3 ms
  - 1 万要素 + 末尾に重複 1 件: 209 ms
  - 1 万要素 + 先頭に重複 1 件: 1 ms
  - 要素数を増やすと二次関数的に伸びる（10 万要素では約 20 秒相当）
- `oneOf`: `check_one_of_1/5` が分岐ごとに `Errors ++ NewErrors` でリストを連結しており O(n²)
- `max_errors`: `src/jsone_schema_state.erl` の `has_reached_max_errors/1` がエラー追加のたびに `length/1` を呼ぶ

## 設計方針

- `uniqueItems` は集合へ挿入しながら最初に衝突した要素を記録し、1 パスで重複要素まで求める。`find_duplicate/1` は廃止する
- エラー蓄積は逆順に積んで最後に反転する
- `max_errors` はエラー件数を state に持って O(1) で判定する
- 計測値は実装後にも再取得し、この issue に追記する

## 完了条件

- 1 万要素 + 末尾重複の `uniqueItems` が線形に近い時間で完了する
- 既存の EUnit / PropEr が引き続き通る
- 実装前後の計測値を issue に記録している

## 解決方法

{未着手}
