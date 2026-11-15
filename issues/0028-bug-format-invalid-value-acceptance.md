# `format` が明白な不正値を受理するのを修正する

- Created: 2026-11-14
- Completed: {YYYY-MM-DD}
- Branch: feature/fix-format-invalid-value-acceptance
- Polished: {YYYY-MM-DD}

## 目的

`format` の検証は簡易チェックとされているが、`date-time` の日付・offset、`email` のドメイン、`ipv4` の符号、`uri-reference` のパーセントエンコーディングに明白な誤受理がある。`validate_format` は既定で true のため、利用者に誤った検証結果を返す。基本範囲だけでも正しくする。

## 現状

`src/jsone_schema_validator.erl` の `check_datetime/2` は正規表現で形を確認した後、`calendar:rfc3339_to_system_time/1` に日付・時刻の妥当性を委ねている。同関数は月 13、2 月 30 日、非閏年の 2 月 29 日、4 月 31 日、`+24:00`、`+99:99`、`+01:60` などを正規化して受理する。

`check_email/2` のドメイン部は `[^@]+` だけなので、空白・末尾ドット・先頭ドット・連続ドット・ハイフン始まり・改行・タブ・空のドメインラベルを受理する。`check_ip/3` は `inet_parse:ipv4strict_address/1` に依存し、`+1.2.3.4` を受理する。`check_uri_reference/2` は `uri_string:parse/1` が `%`、`%ZZ`、末尾 `%` をそのまま返す場合を不正として扱わない。

実測した誤受理の例:

```erlang
%% すべて {ok, ...} になる（RFC 3339 では invalid）
jsone_schema:validate(#{<<"format">> => <<"date-time">>}, <<"2026-02-30T00:00:00Z">>).
jsone_schema:validate(#{<<"format">> => <<"date-time">>}, <<"2026-13-01T00:00:00Z">>).
jsone_schema:validate(#{<<"format">> => <<"date-time">>}, <<"2026-09-15T00:00:00+24:00">>).
jsone_schema:validate(#{<<"format">> => <<"date-time">>}, <<"2026-09-15T00:00:00+99:99">>).

jsone_schema:validate(#{<<"format">> => <<"ipv4">>}, <<"+1.2.3.4">>).
jsone_schema:validate(#{<<"format">> => <<"email">>}, <<"a@exa mple.com">>).
jsone_schema:validate(#{<<"format">> => <<"uri-reference">>}, <<"http://example.com/%">>).
```

公式スイートの optional format は、この範囲の誤りを検出しない。`date-time` の invalid ケースは秒 60 など `calendar:rfc3339_to_system_time/1` が拒否する値に偏っている。

## 設計方針

- `date-time` は `calendar:rfc3339_to_system_time/1` に任せず、年・月・日・時・分・秒・offset の範囲と、月ごとの日数・閏年を自前で検証する。offset は時 00〜23、分 00〜59 に限定する。
- `email` のドメインは dot-atom の最低限の規則（空ラベル禁止、先頭・末尾ドット禁止、空白・制御文字禁止）を検査する。完全な RFC 5322 準拠は目指さない。
- `ipv4` は `inet_parse` に任せる前に、符号・空要素・先頭ゼロの扱いを明示的に検査する。
- `uri-reference` は `%` の後に 2 桁の 16 進数が続くことを事前検査する。
- `format` 自体が文字列でない場合は schema エラーにする。
- README に検査項目と非検査項目を列挙し、簡易チェックの範囲を明確にする。

## 完了条件

- 上記の誤受理例がすべて `{error, [#{kind := data, error := wrong_format}]}` になる。
- `2026-02-29`（非閏年）、`2026-02-30`、`2026-13-01`、`2026-04-31`、`+24:00`、`+99:99`、`+01:60`、`+1.2.3.4`、`a@exa mple.com`、`a@bar.`、`%`、`%ZZ` の回帰テストを追加する。
- 正常値（閏日の `2024-02-29`、`+23:59`、正しい IPv4 / IPv6、正常な email、相対 URI）を引き続き受理する。
- 公式スイートの optional format を実行するか、実行しない理由を README に明記する。
- `xref` / `dialyzer` / `eunit` / `proper` が通る。

## 解決方法

{未着手}
