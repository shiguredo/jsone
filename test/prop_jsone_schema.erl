%% jsone_schema の PropEr テスト
-module(prop_jsone_schema).

-export([prop_empty_schema/1,
         prop_false_schema/1,
         prop_types/1,
         prop_required/1,
         prop_additional_properties/1,
         prop_min_max_items/1,
         prop_min_max_length/1,
         prop_min_max_number/1,
         prop_unique_items/1,
         prop_unique_items_number_equality/1,
         prop_enum_self/1,
         prop_const_self/1,
         prop_local_ref_equivalence/1,
         prop_combinators/1]).

-include_lib("proper/include/proper.hrl").

%% 各プロパティの共通オプション
-define(OPTS, [{numtests, 500}, {max_size, 20}, {on_output, fun proper_output/2}]).


%% PropEr の ... を出力しない
proper_output(".", _Args) ->
    ok;
proper_output(Format, Args) ->
    io:format(Format, Args).


prop_empty_schema(doc) ->
    "空スキーマはどのような JSON の値も受け入れる";
prop_empty_schema(opts) ->
    ?OPTS.


prop_empty_schema() ->
    ?FORALL(Json0,
            json_value(),
            begin
                %% PropEr が生成する値は型付けの対象外なので dynamic に寄せる
                Json = eqwalizer:dynamic_cast(Json0),
                is_valid(#{}, Json)
            end).


prop_false_schema(doc) ->
    "false スキーマはどのような JSON の値も拒否する";
prop_false_schema(opts) ->
    ?OPTS.


prop_false_schema() ->
    ?FORALL(Json0,
            json_value(),
            begin
                Json = eqwalizer:dynamic_cast(Json0),
                not is_valid(false, Json)
            end).


prop_types(doc) ->
    "type は Erlang の型判定と一致する";
prop_types(opts) ->
    ?OPTS.


prop_types() ->
    ?FORALL(Json0,
            json_value(),
            begin
                Json = eqwalizer:dynamic_cast(Json0),
                lists:all(fun(Type) ->
                                  IsType = type_matches(Type, Json),
                                  IsType =:= is_valid(#{<<"type">> => Type}, Json)
                          end,
                          types())
            end).


prop_required(doc) ->
    "required はオブジェクトのキーの存在と一致する";
prop_required(opts) ->
    ?OPTS.


prop_required() ->
    ?FORALL({Json0, Keys0},
            {json_value(), list(json_key())},
            begin
                Json = eqwalizer:dynamic_cast(Json0),
                Keys = eqwalizer:dynamic_cast(Keys0),
                %% オブジェクト以外のインスタンスには required を適用しない
                Expected =
                    case is_map(Json) of
                        true ->
                            lists:all(fun(Key) -> maps:is_key(Key, Json) end, Keys);
                        false ->
                            true
                    end,
                Expected =:= is_valid(#{<<"required">> => Keys}, Json)
            end).


prop_additional_properties(doc) ->
    "additionalProperties は properties に無いキーの有無と一致する";
prop_additional_properties(opts) ->
    ?OPTS.


prop_additional_properties() ->
    ?FORALL({Json0, Flags0},
            {json_object(), list(boolean())},
            begin
                Json = eqwalizer:dynamic_cast(Json0),
                Flags = eqwalizer:dynamic_cast(Flags0),
                Keys = maps:keys(Json),
                %% データのキー数に合わせてフラグを揃える
                Flags1 =
                    lists:sublist(Flags ++ lists:duplicate(length(Keys), false), length(Keys)),
                PropertyKeys = [ Key || {Key, Flag} <:- lists:zip(Keys, Flags1), Flag ],
                Properties = maps:from_list([ {Key, #{}} || Key <:- PropertyKeys ]),
                FalseSchema =
                    #{<<"properties">> => Properties, <<"additionalProperties">> => false},
                TrueSchema =
                    #{<<"properties">> => Properties, <<"additionalProperties">> => true},
                Extras = [ Key || Key <:- Keys, not lists:member(Key, PropertyKeys) ],
                HasNoExtras = (Extras =:= []),
                (HasNoExtras =:= is_valid(FalseSchema, Json)) andalso
                is_valid(TrueSchema, Json)
            end).


prop_min_max_items(doc) ->
    "minItems / maxItems は配列の長さと一致する";
prop_min_max_items(opts) ->
    ?OPTS.


prop_min_max_items() ->
    ?FORALL({Json0, Min0, Max0},
            {json_value(), nat(), nat()},
            begin
                Json = eqwalizer:dynamic_cast(Json0),
                Min = eqwalizer:dynamic_cast(Min0),
                Max = eqwalizer:dynamic_cast(Max0),
                ExpectedMin =
                    case is_list(Json) of
                        true ->
                            length(Json) >= Min;
                        false ->
                            true
                    end,
                ExpectedMax =
                    case is_list(Json) of
                        true ->
                            length(Json) =< Max;
                        false ->
                            true
                    end,
                (ExpectedMin =:= is_valid(#{<<"minItems">> => Min}, Json)) andalso
                (ExpectedMax =:= is_valid(#{<<"maxItems">> => Max}, Json))
            end).


prop_min_max_length(doc) ->
    "minLength / maxLength は文字列の長さと一致する";
prop_min_max_length(opts) ->
    ?OPTS.


prop_min_max_length() ->
    ?FORALL({Json0, Min0, Max0},
            {json_value(), nat(), nat()},
            begin
                Json = eqwalizer:dynamic_cast(Json0),
                Min = eqwalizer:dynamic_cast(Min0),
                Max = eqwalizer:dynamic_cast(Max0),
                %% 生成する文字列は ASCII だけなのでバイト数が文字数と一致する
                ExpectedMin =
                    case is_binary(Json) of
                        true ->
                            byte_size(Json) >= Min;
                        false ->
                            true
                    end,
                ExpectedMax =
                    case is_binary(Json) of
                        true ->
                            byte_size(Json) =< Max;
                        false ->
                            true
                    end,
                (ExpectedMin =:= is_valid(#{<<"minLength">> => Min}, Json)) andalso
                (ExpectedMax =:= is_valid(#{<<"maxLength">> => Max}, Json))
            end).


prop_min_max_number(doc) ->
    "minimum / maximum 系は数値の比較と一致する";
prop_min_max_number(opts) ->
    ?OPTS.


prop_min_max_number() ->
    ?FORALL({Json0, Min0, Max0},
            {json_value(), number(), number()},
            begin
                Json = eqwalizer:dynamic_cast(Json0),
                Min = eqwalizer:dynamic_cast(Min0),
                Max = eqwalizer:dynamic_cast(Max0),
                %% 数値以外のインスタンスには範囲のキーワードを適用しない
                Expected =
                    fun(Pred) ->
                            case is_number(Json) of
                                true ->
                                    Pred(Json);
                                false ->
                                    true
                            end
                    end,
                (Expected(fun(Value) -> Value >= Min end) =:=
                 is_valid(#{<<"minimum">> => Min}, Json)) andalso
                (Expected(fun(Value) -> Value =< Max end) =:=
                 is_valid(#{<<"maximum">> => Max}, Json)) andalso
                (Expected(fun(Value) -> Value > Min end) =:=
                 is_valid(#{<<"exclusiveMinimum">> => Min}, Json)) andalso
                (Expected(fun(Value) -> Value < Max end) =:=
                 is_valid(#{<<"exclusiveMaximum">> => Max}, Json))
            end).


prop_unique_items(doc) ->
    "uniqueItems は同じ値の重複を検出する";
prop_unique_items(opts) ->
    ?OPTS.


prop_unique_items() ->
    ?FORALL(Json0,
            json_value(),
            begin
                Json = eqwalizer:dynamic_cast(Json0),
                Schema = #{<<"uniqueItems">> => true},
                (is_valid(Schema, [Json, Json]) =:= false) andalso
                (is_valid(Schema, [Json]) =:= true)
            end).


prop_unique_items_number_equality(doc) ->
    "uniqueItems では 1 と 1.0 を同じ値として扱う";
prop_unique_items_number_equality(opts) ->
    ?OPTS.


prop_unique_items_number_equality() ->
    ?FORALL(N0,
            integer(-1000000, 1000000),
            begin
                N = eqwalizer:dynamic_cast(N0),
                is_valid(#{<<"uniqueItems">> => true}, [N, N * 1.0]) =:= false
            end).


prop_enum_self(doc) ->
    "enum に自身と同じ値を持つスキーマはその値を受け入れる";
prop_enum_self(opts) ->
    ?OPTS.


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
    ?OPTS.


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
    ?OPTS.


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


prop_combinators(doc) ->
    "allOf / anyOf / oneOf / not の結果は個別の検証結果と一致する";
prop_combinators(opts) ->
    ?OPTS.


prop_combinators() ->
    ?FORALL({S10, S20, Json0},
            {simple_schema(), simple_schema(), json_value()},
            begin
                S1 = eqwalizer:dynamic_cast(S10),
                S2 = eqwalizer:dynamic_cast(S20),
                Json = eqwalizer:dynamic_cast(Json0),
                Valid1 = is_valid(S1, Json),
                Valid2 = is_valid(S2, Json),
                ExpectedAll = Valid1 andalso Valid2,
                ExpectedAny = Valid1 orelse Valid2,
                ExpectedOne = (Valid1 andalso not Valid2) orelse (not Valid1 andalso Valid2),
                ExpectedNot = not Valid1,
                (ExpectedAll =:= is_valid(#{<<"allOf">> => [S1, S2]}, Json)) andalso
                (ExpectedAny =:= is_valid(#{<<"anyOf">> => [S1, S2]}, Json)) andalso
                (ExpectedOne =:= is_valid(#{<<"oneOf">> => [S1, S2]}, Json)) andalso
                (ExpectedNot =:= is_valid(#{<<"not">> => S1}, Json))
            end).


%% Internal Functions


%% JSON の値のジェネレータ
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


%% オブジェクトのジェネレータ
json_object() ->
    ?SIZED(Size, map(json_key(), json_value(Size div 2))).


json_scalar() ->
    oneof([null, boolean(), integer(), float(), json_key()]).


json_key() ->
    ?LET(Chars, list(choose($a, $z)), list_to_binary(eqwalizer:dynamic_cast(Chars))).


%% 組み合わせのテストで使う単純なスキーマのジェネレータ
simple_schema() ->
    oneof([#{},
           true,
           false,
           ?LET(Type, oneof(types()), #{<<"type">> => Type}),
           ?LET(Json, json_value(), #{<<"const">> => Json}),
           ?LET(Min, nat(), #{<<"minItems">> => Min}),
           ?LET(Max, nat(), #{<<"maxItems">> => Max}),
           ?LET(Min, nat(), #{<<"minLength">> => Min}),
           ?LET(Max, nat(), #{<<"maxLength">> => Max}),
           ?LET(Min, integer(), #{<<"minimum">> => Min}),
           ?LET(Max, integer(), #{<<"maximum">> => Max})]).


types() ->
    [<<"array">>,
     <<"boolean">>,
     <<"integer">>,
     <<"null">>,
     <<"number">>,
     <<"object">>,
     <<"string">>].


type_matches(<<"array">>, Value) ->
    is_list(Value);
type_matches(<<"boolean">>, Value) ->
    is_boolean(Value);
type_matches(<<"integer">>, Value) when is_float(Value) ->
    Value - trunc(Value) == 0.0;
type_matches(<<"integer">>, Value) ->
    is_integer(Value);
type_matches(<<"null">>, Value) ->
    Value =:= null;
type_matches(<<"number">>, Value) ->
    is_number(Value);
type_matches(<<"object">>, Value) ->
    is_map(Value);
type_matches(<<"string">>, Value) ->
    is_binary(Value).


%% 検証が成功したかどうかだけを返す
is_valid(Schema, Json) ->
    case jsone_schema:validate(Schema, Json) of
        {ok, _} ->
            true;
        {error, _} ->
            false
    end.
