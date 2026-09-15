%% jsone_schema の PropEr テスト
-module(prop_jsone_schema).

-export([prop_empty_schema/1,
         prop_false_schema/1,
         prop_type_array/1,
         prop_enum_self/1,
         prop_const_self/1,
         prop_local_ref_equivalence/1]).

-include_lib("proper/include/proper.hrl").


%% PropEr の ... を出力しない
proper_output(".", _Args) ->
    ok;
proper_output(Format, Args) ->
    io:format(Format, Args).


prop_empty_schema(doc) ->
    "空スキーマはどのような JSON の値も受け入れる";
prop_empty_schema(opts) ->
    [{numtests, 200}, {max_size, 20}, {on_output, fun proper_output/2}].


prop_empty_schema() ->
    ?FORALL(Json0,
            json_value(),
            begin
                %% PropEr が生成する値は型付けの対象外なので dynamic に寄せる
                Json = eqwalizer:dynamic_cast(Json0),
                {ok, Json} = jsone_schema:validate(#{}, Json),
                true
            end).


prop_false_schema(doc) ->
    "false スキーマはどのような JSON の値も拒否する";
prop_false_schema(opts) ->
    [{numtests, 200}, {max_size, 20}, {on_output, fun proper_output/2}].


prop_false_schema() ->
    ?FORALL(Json0,
            json_value(),
            begin
                Json = eqwalizer:dynamic_cast(Json0),
                {error, _Reasons} = jsone_schema:validate(false, Json),
                true
            end).


prop_type_array(doc) ->
    "type: array は配列だけを受け入れる";
prop_type_array(opts) ->
    [{numtests, 200}, {max_size, 20}, {on_output, fun proper_output/2}].


prop_type_array() ->
    ?FORALL(Json0,
            json_value(),
            begin
                Json = eqwalizer:dynamic_cast(Json0),
                case jsone_schema:validate(#{<<"type">> => <<"array">>}, Json) of
                    {ok, Json} ->
                        is_list(Json);
                    {error, _Reasons} ->
                        not is_list(Json)
                end
            end).


prop_enum_self(doc) ->
    "enum に自身と同じ値を持つスキーマはその値を受け入れる";
prop_enum_self(opts) ->
    [{numtests, 200}, {max_size, 20}, {on_output, fun proper_output/2}].


prop_enum_self() ->
    ?FORALL(Json0,
            json_value(),
            begin
                Json = eqwalizer:dynamic_cast(Json0),
                {ok, Json} = jsone_schema:validate(#{<<"enum">> => [Json]}, Json),
                true
            end).


prop_const_self(doc) ->
    "const に自身と同じ値を持つスキーマはその値を受け入れる";
prop_const_self(opts) ->
    [{numtests, 200}, {max_size, 20}, {on_output, fun proper_output/2}].


prop_const_self() ->
    ?FORALL(Json0,
            json_value(),
            begin
                Json = eqwalizer:dynamic_cast(Json0),
                {ok, Json} = jsone_schema:validate(#{<<"const">> => Json}, Json),
                true
            end).


prop_local_ref_equivalence(doc) ->
    "ローカルの $ref は参照先のスキーマと同じ結果になる";
prop_local_ref_equivalence(opts) ->
    [{numtests, 200}, {max_size, 20}, {on_output, fun proper_output/2}].


prop_local_ref_equivalence() ->
    ?FORALL(Json0,
            json_value(),
            begin
                Json = eqwalizer:dynamic_cast(Json0),
                Target = #{<<"type">> => <<"array">>},
                Schema =
                    #{
                      <<"definitions">> => #{<<"a">> => Target},
                      <<"$ref">> => <<"#/definitions/a">>
                     },
                jsone_schema:validate(Schema, Json) =:= jsone_schema:validate(Target, Json)
            end).


%% Internal Functions


json_value() ->
    ?SIZED(Size, json_value(Size)).


json_value(Size) when Size =< 0 ->
    json_scalar();
json_value(Size) ->
    %% 再帰する場合はサイズを半分にして渡し、要素数を抑える
    Smaller = Size div 2,
    Length = erlang:min(Size, 4),
    oneof([json_scalar(),
           resize(Length, list(json_value(Smaller))),
           resize(Length, map(json_key(), json_value(Smaller)))]).


json_scalar() ->
    oneof([null, boolean(), integer(), float(), json_key()]).


json_key() ->
    ?LET(Chars, list(choose($a, $z)), list_to_binary(eqwalizer:dynamic_cast(Chars))).
