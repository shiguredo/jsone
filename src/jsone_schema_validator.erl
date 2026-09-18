%% JSON Schema draft 6 の検証コア
%%
%% データは jsone:decode/1 が返す map / list / binary / number / boolean / null を
%% 前提とし、スキーマも同じ表現の map / boolean だけを扱う。
%%
%% 検証は状態 (jsone_schema_state) を引き回しながら進める。エラーは
%% jsone_schema_error が状態に追加し、上限に達した時点で throw して打ち切る。
%% https://datatracker.ietf.org/doc/html/draft-wright-json-schema-00
%% https://datatracker.ietf.org/doc/html/draft-wright-json-schema-validation-01
-module(jsone_schema_validator).

-export([validate_with_state/3]).

-include("jsone_schema.hrl").


%% `$schema' を確認してからスキーマを評価する
%%
%% 対応していない `$schema' が指定されていた場合はスキーマエラーを追加する。
-spec validate_with_state(jsone_schema:schema(),
                          jsone:json_value(),
                          jsone_schema_state:state()) ->
          jsone_schema_state:state().
validate_with_state(JsonSchema, Value, State) ->
    case schema_ver(JsonSchema) of
        ?JSON_SCHEMA_DRAFT6 ->
            check_value(Value, JsonSchema, State);
        SchemaVer ->
            jsone_schema_error:schema_invalid({?schema_unsupported, SchemaVer}, State)
    end.


%% Internal Functions


%% `$schema' が無い場合は draft 6 として扱う
schema_ver(JsonSchema) when is_map(JsonSchema) ->
    normalize_schema_ver(maps:get(?SCHEMA, JsonSchema, ?DEFAULT_SCHEMA_VER));
schema_ver(_JsonSchema) ->
    ?DEFAULT_SCHEMA_VER.


%% `#' の有無だけの違いを吸収する
normalize_schema_ver(<<"http://json-schema.org/draft-06/schema">>) ->
    ?JSON_SCHEMA_DRAFT6;
normalize_schema_ver(SchemaVer) ->
    SchemaVer.


%% bool のスキーマはそれぞれ空スキーマと `{"not": {}}' と等価になる
check_value(Value, true, State) ->
    check_value(Value, #{}, State);
check_value(Value, false, State) ->
    check_value(Value, #{?NOT => #{}}, State);
check_value(Value, JsonSchema, State0) when is_map(JsonSchema) ->
    State = jsone_schema_state:enter_schema(State0, JsonSchema),
    case JsonSchema of
        #{?REF := Reference} when is_binary(Reference) ->
            check_ref(Value, Reference, State);
        _ ->
            %% `$ref' の兄弟無視により `id' を評価しないのは `$ref' が binary の
            %% 場合だけである。`id' は draft-06 より前のドラフト (core-00 §8.2) の
            %% キーワードであり、draft-04 の文書を draft-06 として黙って検証
            %% しないため、`$ref' を持たないスキーマの `id' は schema エラーにする
            case maps:is_key(?ID_OLD, JsonSchema) of
                true ->
                    jsone_schema_error:schema_invalid(?wrong_draft6_id_tag, State);
                false ->
                    check_keywords(Value, JsonSchema, State)
            end
    end;
check_value(_Value, _JsonSchema, State) ->
    jsone_schema_error:schema_invalid(?schema_invalid, State).


%% スキーマに指定されているキーワードだけを評価する
%%
%% 存在しないキーワードを照合しないようにするため、スキーマ自身を走査する。
%% 評価順は map の走査順に従い、複数のキーワードが失敗した場合の
%% エラーの順番もこの順番に依存する。
check_keywords(Value, JsonSchema, State) ->
    maps:fold(fun(Keyword, KeywordValue, Acc) ->
                      check_keyword_value(Keyword, KeywordValue, Value, JsonSchema, Acc)
              end,
              State,
              JsonSchema).


%% キーワードごとの検証
%%
%% インスタンスの型に合わないキーワードは適用しない。たとえば `minimum' は
%% 数値以外のインスタンスでは何もしない。
check_keyword_value(?TYPE, Type, Value, _JsonSchema, State) ->
    check_type(Value, Type, State);
check_keyword_value(?ENUM, Enum, Value, _JsonSchema, State) ->
    check_enum(Value, Enum, State);
check_keyword_value(?CONST, Const, Value, _JsonSchema, State) ->
    check_enum(Value, [Const], State);
check_keyword_value(?MULTIPLEOF, MultipleOf, Value, _JsonSchema, State) ->
    check_multiple_of(Value, MultipleOf, State);
check_keyword_value(?MAXIMUM, Maximum, Value, _JsonSchema, State) ->
    check_number_bound(Value, Maximum, fun(V) -> V =< Maximum end, State);
check_keyword_value(?EXCLUSIVEMAXIMUM, Maximum, Value, _JsonSchema, State) ->
    check_number_bound(Value, Maximum, fun(V) -> V < Maximum end, State);
check_keyword_value(?MINIMUM, Minimum, Value, _JsonSchema, State) ->
    check_number_bound(Value, Minimum, fun(V) -> V >= Minimum end, State);
check_keyword_value(?EXCLUSIVEMINIMUM, Minimum, Value, _JsonSchema, State) ->
    check_number_bound(Value, Minimum, fun(V) -> V > Minimum end, State);
check_keyword_value(?MAXLENGTH, MaxLength, Value, _JsonSchema, State) ->
    check_string_length(Value, MaxLength, fun(L) -> L =< MaxLength end, State);
check_keyword_value(?MINLENGTH, MinLength, Value, _JsonSchema, State) ->
    check_string_length(Value, MinLength, fun(L) -> L >= MinLength end, State);
check_keyword_value(?PATTERN, Pattern, Value, _JsonSchema, State) when is_binary(Pattern) ->
    case is_binary(Value) of
        true ->
            check_pattern(Value, Pattern, State);
        false ->
            State
    end;
check_keyword_value(?PATTERN, _Pattern, _Value, _JsonSchema, State) ->
    jsone_schema_error:schema_invalid(?schema_invalid, State);
check_keyword_value(?ITEMS, Items, Value, JsonSchema, State) ->
    case is_list(Value) of
        true ->
            check_items(Value, Items, JsonSchema, State);
        false ->
            State
    end;
check_keyword_value(?MAXITEMS, MaxItems, Value, _JsonSchema, State) ->
    check_max_items(Value, MaxItems, State);
check_keyword_value(?MINITEMS, MinItems, Value, _JsonSchema, State) ->
    check_min_items(Value, MinItems, State);
check_keyword_value(?UNIQUEITEMS, true, Value, _JsonSchema, State) ->
    check_unique_items(Value, State);
check_keyword_value(?UNIQUEITEMS, false, _Value, _JsonSchema, State) ->
    State;
check_keyword_value(?UNIQUEITEMS, _UniqueItems, _Value, _JsonSchema, State) ->
    jsone_schema_error:schema_invalid(?schema_invalid, State);
check_keyword_value(?CONTAINS, ContainsSchema, Value, _JsonSchema, State) ->
    check_contains(Value, ContainsSchema, State);
check_keyword_value(?MAXPROPERTIES, MaxProperties, Value, _JsonSchema, State) ->
    check_max_properties(Value, MaxProperties, State);
check_keyword_value(?MINPROPERTIES, MinProperties, Value, _JsonSchema, State) ->
    check_min_properties(Value, MinProperties, State);
check_keyword_value(?REQUIRED, Required, Value, _JsonSchema, State) when is_list(Required) ->
    case is_map(Value) of
        true ->
            check_required(Value, Required, State);
        false ->
            State
    end;
check_keyword_value(?REQUIRED, _Required, _Value, _JsonSchema, State) ->
    jsone_schema_error:schema_invalid(?wrong_required_array, State);
check_keyword_value(?PROPERTIES, Properties, Value, _JsonSchema, State) when is_map(Properties) ->
    case is_map(Value) of
        true ->
            check_properties(Value, Properties, State);
        false ->
            State
    end;
check_keyword_value(?PROPERTIES, _Properties, _Value, _JsonSchema, State) ->
    jsone_schema_error:schema_invalid(?schema_invalid, State);
check_keyword_value(?PATTERNPROPERTIES, PatternProperties, Value, _JsonSchema, State)
  when is_map(PatternProperties) ->
    case is_map(Value) of
        true ->
            check_pattern_properties(Value, PatternProperties, State);
        false ->
            State
    end;
check_keyword_value(?PATTERNPROPERTIES, _PatternProperties, _Value, _JsonSchema, State) ->
    jsone_schema_error:schema_invalid(?schema_invalid, State);
check_keyword_value(?ADDITIONALPROPERTIES, AdditionalProperties, Value, JsonSchema, State) ->
    case is_map(Value) of
        true ->
            check_additional_properties(Value, AdditionalProperties, JsonSchema, State);
        false ->
            State
    end;
check_keyword_value(?PROPERTYNAMES, PropertySchema, Value, _JsonSchema, State) ->
    case is_map(Value) of
        true ->
            check_property_names(Value, PropertySchema, State);
        false ->
            State
    end;
check_keyword_value(?DEPENDENCIES, Dependencies, Value, _JsonSchema, State)
  when is_map(Dependencies) ->
    case is_map(Value) of
        true ->
            check_dependencies(Value, Dependencies, State);
        false ->
            State
    end;
check_keyword_value(?DEPENDENCIES, _Dependencies, _Value, _JsonSchema, State) ->
    jsone_schema_error:schema_invalid(?schema_invalid, State);
check_keyword_value(?ALLOF, Schemas, Value, _JsonSchema, State) ->
    check_all_of(Value, Schemas, State);
check_keyword_value(?ANYOF, Schemas, Value, _JsonSchema, State) ->
    check_any_of(Value, Schemas, State);
check_keyword_value(?ONEOF, Schemas, Value, _JsonSchema, State) ->
    check_one_of(Value, Schemas, State);
check_keyword_value(?NOT, NotSchema, Value, _JsonSchema, State) ->
    check_not(Value, NotSchema, State);
check_keyword_value(?FORMAT, Format, Value, _JsonSchema, State) ->
    check_format(Value, Format, State);
%% `$schema' / `$id' / `definitions' など検証に使わないキーワードは無視する
check_keyword_value(_Keyword, _KeywordValue, _Value, _JsonSchema, State) ->
    State.


%% 5.5.2. type
%%
%% draft 6 では "integer" は小数部が 0 の数値も含む。
check_type(Value, Type, State) ->
    case type_matches(Value, Type) of
        true ->
            State;
        false ->
            jsone_schema_error:data_invalid(?wrong_type, Value, State);
        invalid ->
            jsone_schema_error:schema_invalid(?wrong_type_specification, State)
    end.


type_matches(Value, Type) when is_binary(Type) ->
    is_type_valid(Value, Type);
type_matches(Value, [_ | _] = Types) ->
    case lists:all(fun erlang:is_binary/1, Types) of
        true ->
            lists:any(fun(UnionType) -> is_type_valid(Value, UnionType) end, Types);
        false ->
            invalid
    end;
type_matches(_Value, _Type) ->
    invalid.


is_type_valid(Value, ?TYPE_STRING) ->
    is_binary(Value);
is_type_valid(Value, ?TYPE_NUMBER) ->
    is_number(Value);
is_type_valid(Value, ?TYPE_INTEGER) when is_float(Value) ->
    Value - trunc(Value) == 0.0;
is_type_valid(Value, ?TYPE_INTEGER) ->
    is_integer(Value);
is_type_valid(Value, ?TYPE_BOOLEAN) ->
    is_boolean(Value);
is_type_valid(Value, ?TYPE_OBJECT) ->
    is_map(Value);
is_type_valid(Value, ?TYPE_ARRAY) ->
    is_list(Value);
is_type_valid(Value, ?TYPE_NULL) ->
    Value =:= null;
is_type_valid(_Value, _Type) ->
    invalid.


%% 6.23. enum / 6.24. const
%%
%% JSON の等価判定なので 1 と 1.0 は等しい。
check_enum(Value, Enum, State) when is_list(Enum) ->
    case lists:any(fun(Expected) -> jsone_schema_equality:equal(Value, Expected) end, Enum) of
        true ->
            State;
        false ->
            jsone_schema_error:data_invalid(?not_in_enum, Value, State)
    end;
check_enum(_Value, _Enum, State) ->
    jsone_schema_error:schema_invalid(?schema_invalid, State).


%% 6.1. multipleOf
%%
%% 整数同士は rem で正確に判定する。小数を含む場合は、倍精度浮動小数の
%% 最短往復 10 進表記を正本として両オペランドを整数化してから rem で判定する。
%% 指数差が上限を超える組み合わせは、10 進の厳密な判定を行わず倍数ではない
%% ものとする。
check_multiple_of(Value, MultipleOf, State) when is_number(MultipleOf), MultipleOf > 0 ->
    case is_number(Value) of
        true ->
            check_multiple_of_1(Value, MultipleOf, State);
        false ->
            State
    end;
check_multiple_of(_Value, _MultipleOf, State) ->
    jsone_schema_error:schema_invalid(?wrong_multiple_of, State).


check_multiple_of_1(Value, MultipleOf, State) ->
    case is_multiple_of(Value, MultipleOf) of
        true ->
            State;
        false ->
            jsone_schema_error:data_invalid(?not_multiple_of, Value, State)
    end.


%% 整数同士は rem で正確に判定する
is_multiple_of(Value, MultipleOf) when is_integer(Value), is_integer(MultipleOf) ->
    Value rem MultipleOf =:= 0;
is_multiple_of(Value, MultipleOf) ->
    {ValueInt, ValueExp} = decimal_parts(Value),
    {MultipleInt, MultipleExp} = decimal_parts(MultipleOf),
    ExponentDiff = ValueExp - MultipleExp,
    case abs(ExponentDiff) > ?MULTIPLE_OF_EXPONENT_LIMIT of
        true ->
            false;
        false when ExponentDiff >= 0 ->
            ValueInt * pow10(ExponentDiff) rem MultipleInt =:= 0;
        false ->
            ValueInt rem (MultipleInt * pow10(-ExponentDiff)) =:= 0
    end.


%% 10 進の最短往復表記を {整数, 10 の指数} に分解する
%%
%% 値は `整数 * 10^指数' として表す。0.07 は {7, -2}、1.0e308 は {10, 307}、
%% -4.35 は {-435, -2} になる。整数のオペランドは整数のまま扱い、
%% `float_to_list(Value, [short])' は浮動小数のオペランドにだけ使う。
%% 正本は最短往復表記であり、倍精度の厳密値からは分解しない。
decimal_parts(Value) when is_integer(Value) ->
    {Value, 0};
decimal_parts(Value) ->
    Binary = list_to_binary(float_to_list(Value, [short])),
    [Mantissa | ExponentPart] = binary:split(Binary, <<"e">>),
    {Sign, Digits, Scale} = mantissa_parts(Mantissa),
    Exponent =
        case ExponentPart of
            [ExponentBinary] ->
                binary_to_integer(ExponentBinary);
            [] ->
                0
        end,
    {Sign * binary_to_integer(Digits), Exponent - Scale}.


%% 仮数部を {符号, 数字, 小数部の桁数} に分ける
mantissa_parts(<<$-, Rest/binary>>) ->
    {Sign, Digits, Scale} = mantissa_parts(Rest),
    {-Sign, Digits, Scale};
mantissa_parts(Mantissa) ->
    case binary:split(Mantissa, <<".">>) of
        [IntegerPart, FractionPart] ->
            {1, <<IntegerPart/binary, FractionPart/binary>>, byte_size(FractionPart)};
        [IntegerPart] ->
            {1, IntegerPart, 0}
    end.


%% 10 の冪を整数で計算する
pow10(Exponent) ->
    binary_to_integer(<<"1", (binary:copy(<<"0">>, Exponent))/binary>>).


%% 6.2. maximum / 6.3. exclusiveMaximum / 6.4. minimum / 6.5. exclusiveMinimum
%%
%% 数値以外のインスタンスには適用しない。
check_number_bound(Value, Bound, Pred, State) when is_number(Bound), is_number(Value) ->
    case Pred(Value) of
        true ->
            State;
        false ->
            jsone_schema_error:data_invalid(?not_in_range, Value, State)
    end;
check_number_bound(_Value, Bound, _Pred, State) when is_number(Bound) ->
    State;
check_number_bound(_Value, _Bound, _Pred, State) ->
    jsone_schema_error:schema_invalid(?schema_invalid, State).


%% 6.7. minLength / 6.6. maxLength
%%
%% 文字列の長さは Unicode のコードポイント数で数える。
check_string_length(Value, Length, Pred, State) when is_integer(Length), Length >= 0 ->
    case is_binary(Value) of
        true ->
            case Pred(length(string_chars(Value))) of
                true ->
                    State;
                false ->
                    jsone_schema_error:data_invalid(?wrong_length, Value, State)
            end;
        false ->
            State
    end;
check_string_length(_Value, _Length, _Pred, State) ->
    jsone_schema_error:schema_invalid(?schema_invalid, State).


%% バイナリを Unicode のコードポイントのリストに変換する
%%
%% jsone:decode/1 は常に正しい UTF-8 を返すため通常は失敗しない。
%% 変換できない場合はバイト列として扱う。
-spec string_chars(binary()) -> string().
string_chars(Value) ->
    case unicode:characters_to_list(Value) of
        Chars when is_list(Chars) ->
            Chars;
        _Error ->
            binary_to_list(Value)
    end.


%% 6.8. pattern
%%
%% ECMA 262 と Erlang の正規表現の違いは許容している。
check_pattern(Value, Pattern, State) ->
    case run_pattern(Value, Pattern) of
        match ->
            State;
        nomatch ->
            jsone_schema_error:data_invalid(?no_match, Value, State);
        {error, _Reason} ->
            jsone_schema_error:schema_invalid(?schema_invalid, State)
    end.


run_pattern(Subject, Pattern) ->
    try re:run(Subject, Pattern, [{capture, none}, unicode, ucp]) of
        Result ->
            Result
    catch
        %% 不正な正規表現はクラッシュさせずスキーマのエラーとして扱う
        error:Reason ->
            {error, Reason}
    end.


%% 6.10. items / additionalItems
%%
%% items がスキーマの場合は全要素に、配列の場合は位置ごとに対応する。
%% 配列の範囲外の要素には additionalItems を適用する。
check_items(Value, Items, _JsonSchema, State) when is_map(Items); is_boolean(Items) ->
    check_items_schema(Value, Items, 0, State);
check_items(Value, Items, JsonSchema, State) when is_list(Items) ->
    check_items_array(Value, Value, Items, 0, JsonSchema, State);
check_items(_Value, Items, _JsonSchema, State) ->
    jsone_schema_error:schema_invalid({?wrong_type_items, Items}, State).


check_items_schema([], _Items, _Index, State) ->
    State;
check_items_schema([Element | Rest], Items, Index, State) ->
    State1 = check_child(Index, Element, Items, State),
    check_items_schema(Rest, Items, Index + 1, State1).


check_items_array(_All, [], _Items, _Index, _JsonSchema, State) ->
    State;
check_items_array(All, [Element | Rest], [ItemSchema | Schemas], Index, JsonSchema, State) ->
    State1 = check_child(Index, Element, ItemSchema, State),
    check_items_array(All, Rest, Schemas, Index + 1, JsonSchema, State1);
check_items_array(All, [Element | Rest], [], Index, JsonSchema, State) ->
    case maps:get(?ADDITIONALITEMS, JsonSchema, true) of
        false ->
            jsone_schema_error:data_invalid(?no_extra_items_allowed, All, State);
        true ->
            State;
        AdditionalItems ->
            State1 = check_child(Index, Element, AdditionalItems, State),
            check_items_array(All, Rest, [], Index + 1, JsonSchema, State1)
    end.


%% 6.11. maxItems / 6.12. minItems
check_max_items(Value, MaxItems, State) when is_integer(MaxItems), MaxItems >= 0 ->
    case is_list(Value) of
        true ->
            check_array_size(Value, length(Value) =< MaxItems, State);
        false ->
            State
    end;
check_max_items(_Value, _MaxItems, State) ->
    jsone_schema_error:schema_invalid(?schema_invalid, State).


check_min_items(Value, MinItems, State) when is_integer(MinItems), MinItems >= 0 ->
    case is_list(Value) of
        true ->
            check_array_size(Value, length(Value) >= MinItems, State);
        false ->
            State
    end;
check_min_items(_Value, _MinItems, State) ->
    jsone_schema_error:schema_invalid(?schema_invalid, State).


check_array_size(_Value, true, State) ->
    State;
check_array_size(Value, false, State) ->
    jsone_schema_error:data_invalid(?wrong_size, Value, State).


%% 6.13. uniqueItems
%%
%% オブジェクトのキー順や 1 と 1.0 の違いを吸収して重複を判定する。
check_unique_items(Value, State) when is_list(Value) ->
    case has_duplicate(Value) of
        none ->
            State;
        {duplicate, Item} ->
            jsone_schema_error:data_invalid({?not_unique, Item}, Value, State)
    end;
check_unique_items(_Value, State) ->
    State.


%% 正規化した値の集合で重複を素早く判定する
%%
%% 重複がある場合だけ、どの値が重複しているかを調べる。
has_duplicate(Value) ->
    Normalized = [ jsone_schema_equality:normalize(Element) || Element <:- Value ],
    case sets:size(sets:from_list(Normalized, [{version, 2}])) =:= length(Value) of
        true ->
            none;
        false ->
            find_duplicate(Value)
    end.


find_duplicate([Item | Rest]) ->
    case lists:any(fun(Other) -> jsone_schema_equality:equal(Item, Other) end, Rest) of
        true ->
            {duplicate, Item};
        false ->
            find_duplicate(Rest)
    end;
find_duplicate([]) ->
    none.


%% 6.14. contains
%%
%% 配列の中にスキーマを満たす要素が 1 つ以上あれば良い。
check_contains(Value, ContainsSchema, State) when is_list(Value) ->
    case lists:any(fun(Element) -> subschema_valid(ContainsSchema, Element, State) end, Value) of
        true ->
            State;
        false ->
            jsone_schema_error:data_invalid(?data_invalid, Value, State)
    end;
check_contains(_Value, _ContainsSchema, State) ->
    State.


%% 6.15. maxProperties / 6.16. minProperties
check_max_properties(Value, MaxProperties, State) when is_integer(MaxProperties), MaxProperties >= 0 ->
    case is_map(Value) of
        true ->
            case map_size(Value) =< MaxProperties of
                true ->
                    State;
                false ->
                    jsone_schema_error:data_invalid(?too_many_properties, Value, State)
            end;
        false ->
            State
    end;
check_max_properties(_Value, _MaxProperties, State) ->
    jsone_schema_error:schema_invalid(?wrong_max_properties, State).


check_min_properties(Value, MinProperties, State) when is_integer(MinProperties), MinProperties >= 0 ->
    case is_map(Value) of
        true ->
            case map_size(Value) >= MinProperties of
                true ->
                    State;
                false ->
                    jsone_schema_error:data_invalid(?too_few_properties, Value, State)
            end;
        false ->
            State
    end;
check_min_properties(_Value, _MinProperties, State) ->
    jsone_schema_error:schema_invalid(?wrong_min_properties, State).


%% 6.17. required
check_required(Value, Required, State) ->
    case lists:all(fun erlang:is_binary/1, Required) of
        true ->
            lists:foldl(fun(Name, Acc) -> check_required_property(Name, Value, Acc) end,
                        State,
                        Required);
        false ->
            jsone_schema_error:schema_invalid(?wrong_required_array, State)
    end.


check_required_property(Name, Value, State) ->
    case maps:is_key(Name, Value) of
        true ->
            State;
        false ->
            jsone_schema_error:data_invalid(?missing_required_property, Name, State)
    end.


%% 6.18. properties
check_properties(Value, Properties, State) ->
    maps:fold(fun(Name, PropertySchema, Acc) ->
                      case Value of
                          #{Name := Property} ->
                              check_child(Name, Property, PropertySchema, Acc);
                          _ ->
                              Acc
                      end
              end,
              State,
              Properties).


%% 6.19. patternProperties
check_pattern_properties(Value, PatternProperties, State) ->
    maps:fold(fun(Pattern, PropertySchema, Acc) ->
                      check_pattern_properties_1(Value, Pattern, PropertySchema, Acc)
              end,
              State,
              PatternProperties).


check_pattern_properties_1(Value, Pattern, PropertySchema, State) ->
    maps:fold(fun(Name, Property, Acc) ->
                      case run_pattern(Name, Pattern) of
                          match ->
                              check_child(Name, Property, PropertySchema, Acc);
                          nomatch ->
                              Acc;
                          {error, _Reason} ->
                              jsone_schema_error:schema_invalid(?schema_invalid, Acc)
                      end
              end,
              State,
              Value).


%% 6.20. additionalProperties
%%
%% properties と patternProperties のどちらにも一致しないプロパティだけを検証する。
check_additional_properties(Value, false, JsonSchema, State) ->
    Lists = extra_property_names(Value, JsonSchema),
    lists:foldl(fun(Name, Acc) ->
                        Acc1 = jsone_schema_state:add_to_path(Acc, Name),
                        Acc2 =
                            jsone_schema_error:data_invalid(?no_extra_properties_allowed,
                                                            Value,
                                                            Acc1),
                        jsone_schema_state:remove_last_from_path(Acc2)
                end,
                State,
                Lists);
check_additional_properties(_Value, true, _JsonSchema, State) ->
    State;
check_additional_properties(Value, AdditionalProperties, JsonSchema, State)
  when is_map(AdditionalProperties); is_boolean(AdditionalProperties) ->
    lists:foldl(fun(Name, Acc) ->
                        Property = maps:get(Name, Value),
                        check_child(Name, Property, AdditionalProperties, Acc)
                end,
                State,
                extra_property_names(Value, JsonSchema));
check_additional_properties(_Value, _AdditionalProperties, _JsonSchema, State) ->
    jsone_schema_error:schema_invalid(?schema_invalid, State).


extra_property_names(Value, JsonSchema) ->
    Properties = schema_map(?PROPERTIES, JsonSchema),
    PatternProperties = schema_map(?PATTERNPROPERTIES, JsonSchema),
    [ Name
      || Name <:- maps:keys(Value),
         not maps:is_key(Name, Properties),
         not matches_any_pattern(Name, PatternProperties) ].


schema_map(Keyword, JsonSchema) ->
    case maps:get(Keyword, JsonSchema, #{}) of
        Map when is_map(Map) ->
            Map;
        _Other ->
            #{}
    end.


matches_any_pattern(_Name, PatternProperties) when map_size(PatternProperties) =:= 0 ->
    false;
matches_any_pattern(Name, PatternProperties) ->
    lists:any(fun(Pattern) ->
                      run_pattern(Name, Pattern) =:= match
              end,
              maps:keys(PatternProperties)).


%% 6.21. dependencies
check_dependencies(Value, Dependencies, State) ->
    maps:fold(fun(Name, Dependency, Acc) ->
                      case maps:is_key(Name, Value) of
                          true ->
                              check_dependency(Value, Name, Dependency, Acc);
                          false ->
                              Acc
                      end
              end,
              State,
              Dependencies).


check_dependency(Value, DependencyName, Dependency, State)
  when is_map(Dependency); is_boolean(Dependency) ->
    check_child(DependencyName, Value, Dependency, State);
check_dependency(Value, _DependencyName, Dependency, State) when is_list(Dependency) ->
    lists:foldl(fun(PropertyName, Acc) ->
                        check_dependency_property(Value, PropertyName, Acc)
                end,
                State,
                Dependency);
check_dependency(_Value, _DependencyName, _Dependency, State) ->
    jsone_schema_error:schema_invalid(?invalid_dependency, State).


check_dependency_property(Value, PropertyName, State) when is_binary(PropertyName) ->
    case maps:is_key(PropertyName, Value) of
        true ->
            State;
        false ->
            jsone_schema_error:data_invalid({?missing_dependency, PropertyName}, Value, State)
    end;
check_dependency_property(_Value, _PropertyName, State) ->
    jsone_schema_error:schema_invalid(?invalid_dependency, State).


%% 6.22. propertyNames
check_property_names(Value, PropertySchema, State) ->
    maps:fold(fun(Name, _Property, Acc) ->
                      check_child(Name, Name, PropertySchema, Acc)
              end,
              State,
              Value).


%% 6.26. allOf
%%
%% すべてのスキーマを満たす必要がある。
check_all_of(Value, [_ | _] = Schemas, State) ->
    check_all_of_1(Value, Schemas, State);
check_all_of(_Value, _Schemas, State) ->
    jsone_schema_error:schema_invalid(?wrong_all_of_schema_array, State).


check_all_of_1(_Value, [], State) ->
    State;
check_all_of_1(Value, [Schema | Schemas], State) ->
    case run_subschema(Schema, Value, State) of
        {ok, State1} ->
            check_all_of_1(Value, Schemas, State1);
        {error, Errors, _State} ->
            jsone_schema_error:data_invalid({?all_schemas_not_valid, Errors}, Value, State)
    end.


%% 6.27. anyOf
%%
%% 1 つ以上のスキーマを満たせば良い。失敗時のエラーは最も短いリストを返す。
check_any_of(Value, [_ | _] = Schemas, State) ->
    check_any_of_1(Value, Schemas, State, empty);
check_any_of(_Value, _Schemas, State) ->
    jsone_schema_error:schema_invalid(?wrong_any_of_schema_array, State).


check_any_of_1(Value, [], State, empty) ->
    jsone_schema_error:data_invalid(?any_schemas_not_valid, Value, State);
check_any_of_1(Value, [], State, Errors) ->
    jsone_schema_error:data_invalid({?any_schemas_not_valid, Errors}, Value, State);
check_any_of_1(Value, [Schema | Schemas], State, Errors) ->
    case run_subschema(Schema, Value, State) of
        {ok, State1} ->
            State1;
        {error, NewErrors, _State} ->
            check_any_of_1(Value, Schemas, State, shortest(NewErrors, Errors))
    end.


%% 6.28. oneOf
%%
%% ちょうど 1 つのスキーマを満たす必要がある。
check_one_of(Value, [_ | _] = Schemas, State) ->
    check_one_of_1(Value, Schemas, State, 0, []);
check_one_of(_Value, _Schemas, State) ->
    jsone_schema_error:schema_invalid(?wrong_one_of_schema_array, State).


check_one_of_1(_Value, [], State, 1, _Errors) ->
    State;
check_one_of_1(Value, [], State, 0, Errors) ->
    jsone_schema_error:data_invalid({?not_one_schema_valid, Errors}, Value, State);
check_one_of_1(Value, _Schemas, State, Valid, _Errors) when Valid > 1 ->
    jsone_schema_error:data_invalid(?more_than_one_schema_valid, Value, State);
check_one_of_1(Value, [Schema | Schemas], State, Valid, Errors) ->
    case run_subschema(Schema, Value, State) of
        {ok, _State1} ->
            check_one_of_1(Value, Schemas, State, Valid + 1, Errors);
        {error, NewErrors, _State} ->
            check_one_of_1(Value, Schemas, State, Valid, Errors ++ NewErrors)
    end.


%% 6.29. not
%%
%% スキーマを満たさない場合だけ有効になる。
check_not(Value, NotSchema, State) ->
    case run_subschema(NotSchema, Value, State) of
        {ok, _State1} ->
            jsone_schema_error:data_invalid(?not_schema_valid, Value, State);
        {error, _Errors, _State} ->
            State
    end.


%% サブスキーマを検証し、成否とエラーリストを返す
%%
%% エラーリストはサブスキーマ専用のものを用意するので、
%% 呼び出し元のエラーリストと混ざらない。
-spec run_subschema(jsone_schema:schema(), jsone:json_value(), jsone_schema_state:state()) ->
          {ok, jsone_schema_state:state()} |
          {error, [jsone_schema_error:reason()], jsone_schema_state:state()}.
run_subschema(JsonSchema, Value, State) ->
    SubState = jsone_schema_state:reset_errors(State),
    try validate_with_state(JsonSchema, Value, SubState) of
        ResultState ->
            case jsone_schema_state:get_errors(ResultState) of
                [] ->
                    {ok, jsone_schema_state:restore(ResultState, State)};
                Errors ->
                    {error, Errors, jsone_schema_state:restore(ResultState, State)}
            end
    catch
        throw:{?ERRORS, Errors} ->
            {error, Errors, State}
    end.


subschema_valid(JsonSchema, Value, State) ->
    case run_subschema(JsonSchema, Value, State) of
        {ok, _State1} ->
            true;
        {error, _Errors, _State} ->
            false
    end.


%% `$ref' を解決して検証する
%%
%% draft 6 では `$ref' がある場合、同じオブジェクトの他のキーワードは無視する。
check_ref(Value, Reference, State) ->
    case jsone_schema_state:resolve_ref(State, Reference) of
        {ok, RefState, JsonSchema} ->
            check_ref_schema(Value, JsonSchema, RefState, State);
        {error, Reason, _State} ->
            jsone_schema_error:schema_invalid(Reason, State)
    end.


%% 解決先を解決スタックに積んでから検証する
%%
%% 同じ (解決先スキーマ, インスタンス値) を現在の解決経路で 2 回評価する場合は
%% 循環とみなして検証全体を打ち切る。解決スタックが上限の長さに達した場合も
%% 同じ経路で打ち切る。どちらもサブスキーマの分岐として握り潰されないよう、
%% エラーリストではなく専用の throw で伝播させる。
%% 循環と上限が同時に成立する場合は、原因を特定できる循環を報告する。
check_ref_schema(Value, JsonSchema, RefState, State) ->
    case jsone_schema_state:enter_ref(JsonSchema, Value, RefState) of
        cycle ->
            jsone_schema_error:abort(?ref_cycle, State);
        limit ->
            jsone_schema_error:abort(?ref_depth_limit, State);
        {ok, PushedState} ->
            ResultState = validate_with_state(JsonSchema, Value, PushedState),
            jsone_schema_state:undo_resolve_ref(
              jsone_schema_state:leave_ref(ResultState), State)
    end.


%% 子スキーマを検証し、パスに要素を追加してから元のスキーマに戻す
check_child(PathItem, Value, JsonSchema, State) ->
    State1 = jsone_schema_state:add_to_path(State, PathItem),
    State2 = validate_with_state(JsonSchema, Value, State1),
    jsone_schema_state:restore_schema(
      jsone_schema_state:remove_last_from_path(State2), State).


%% 6.6. format
%%
%% 対応している format だけ検証する。未対応の format は常に有効として扱う。
check_format(Value, <<"date-time">>, State) when is_binary(Value) ->
    check_datetime(Value, State);
check_format(Value, <<"email">>, State) when is_binary(Value) ->
    check_email(Value, State);
check_format(Value, <<"ipv4">>, State) when is_binary(Value) ->
    check_ip(Value, fun inet_parse:ipv4strict_address/1, State);
check_format(Value, <<"ipv6">>, State) when is_binary(Value) ->
    check_ip(Value, fun inet_parse:ipv6strict_address/1, State);
check_format(Value, <<"uri-reference">>, State) when is_binary(Value) ->
    check_uri_reference(Value, State);
check_format(_Value, _Format, State) ->
    State.


check_datetime(Value, State) ->
    %% calendar:rfc3339_to_system_time/1 はバイナリも受け付ける
    try calendar:rfc3339_to_system_time(Value) of
        _SystemTime ->
            State
    catch
        _:_ ->
            jsone_schema_error:data_invalid(?wrong_format, Value, State)
    end.


check_email(Value, State) ->
    case re:run(Value, <<"^[^@]+@[^@]+$">>, [{capture, none}]) of
        match ->
            State;
        nomatch ->
            jsone_schema_error:data_invalid(?wrong_format, Value, State)
    end.


check_ip(Value, ParseFun, State) ->
    case ParseFun(binary_to_list(Value)) of
        {ok, _Address} ->
            State;
        {error, _Reason} ->
            jsone_schema_error:data_invalid(?wrong_format, Value, State)
    end.


check_uri_reference(Value, State) ->
    case uri_string:parse(Value) of
        {error, _ErrorType, _Term} ->
            jsone_schema_error:data_invalid(?wrong_format, Value, State);
        _ ->
            State
    end.


%% 短い方のエラーリストを返す
%%
%% anyOf の失敗時は、最も短いリストをエラーとして採用する。
shortest(X, empty) ->
    X;
shortest(X, Y) when length(X) < length(Y) ->
    X;
shortest(_X, Y) ->
    Y.
