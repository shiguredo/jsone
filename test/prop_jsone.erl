-module(prop_jsone).

-export([prop_decode_no_crash/1]).

-include_lib("proper/include/proper.hrl").


%% PropEr の ... を出力しない
proper_output(".", _Args) ->
    ok;
proper_output(Format, Args) ->
    io:format(Format, Args).


prop_decode_no_crash(doc) ->
    "バイナリを引数に decode() を呼び出す。クラッシュしなければ成功";
prop_decode_no_crash(opts) ->
    [{numtests, 100000}, {on_output, fun proper_output/2}].


prop_decode_no_crash() ->
    ?FORALL(Bin,
            oneof([
                   %% ランダムなバイナリ（512バイト未満）
                   ?LET(Size, choose(0, 512), binary(Size)),

                   %% JSON 風の文字列（512バイト未満）
                   ?LET(Size,
                        choose(0, 512),
                        ?LET(Chars,
                             vector(Size, json_char()),
                             list_to_binary(Chars)))]),
            begin
                case jsone:try_decode(Bin) of
                    {ok, _Json, _Rest} ->
                        true;
                    {error, _Reason} ->
                        %% クラッシュしなければ OK
                        true;
                    _ ->
                        false
                end
            end).


%% JSON文字の生成（JSON構文に関連する文字の出現確率を高める）
json_char() ->
    frequency([{10, oneof([${, $}, $[, $], $:, $,, $", $\\])},  % JSON構造文字
               {5, oneof([$t, $r, $u, $e, $f, $a, $l, $s, $n, $u, $l])},  % JSON予約語の文字
               {3, oneof(lists:seq($0, $9))},  % 数字
               {2, oneof(lists:seq($a, $z) ++ lists:seq($A, $Z))},  % アルファベット
               {1, integer(32, 126)}  % その他の表示可能文字
              ]).
