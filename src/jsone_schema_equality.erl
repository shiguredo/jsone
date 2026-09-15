%% JSON の等価判定
%%
%% JSON Schema の `enum` / `const` / `uniqueItems` では、
%% 数値の 1 と 1.0 が等しく、オブジェクトのキー順に依存しない等価判定が必要になる。
-module(jsone_schema_equality).

-export([equal/2, normalize/1]).


%% JSON の値として等しいかどうかを返す
%%
%% 呼び出し元は JSON の値を渡すが、リストが配列なのかオブジェクト
%% (proplist) なのかを型で区別できないため、ここでは term() を受け取る。
-spec equal(term(), term()) -> boolean().
equal(A, B) when is_number(A), is_number(B) ->
    A == B;
equal(A, B) when is_map(A), is_map(B) ->
    maps:size(A) =:= maps:size(B) andalso
    maps:fold(fun(Key, Value, Acc) ->
                      case B of
                          #{Key := OtherValue} ->
                              Acc andalso equal(Value, OtherValue);
                          _ ->
                              false
                      end
              end,
              true,
              A);
equal(A, B) when is_list(A), is_list(B) ->
    length(A) =:= length(B) andalso
    lists:all(fun({X, Y}) ->
                      equal(X, Y)
              end,
              lists:zip(A, B));
equal(A, B) ->
    A =:= B.


%% 集合に入れられる正規化キーへ変換する
%%
%% `uniqueItems` の重複判定で使う。数値は整数と小数を同じ表現に寄せ、
%% オブジェクトはキー順をソートしたリストに変換する。
-spec normalize(term()) -> term().
normalize(Value) when is_map(Value) ->
    {object, lists:sort([ {Key, normalize(Element)} || {Key, Element} <:- maps:to_list(Value) ])};
normalize(Value) when is_list(Value) ->
    {array, [ normalize(Element) || Element <:- Value ]};
normalize(Value) when is_number(Value) ->
    {number, normalize_number(Value)};
normalize(Value) ->
    Value.


normalize_number(Value) when is_integer(Value) ->
    Value;
normalize_number(Value) when is_float(Value) ->
    case Value == trunc(Value) of
        true ->
            trunc(Value);
        false ->
            Value
    end.
