-module(jsone).

-export([decode/1, decode/2,
         try_decode/1, try_decode/2,
         encode/1, encode/2,
         try_encode/1, try_encode/2]).

-export_type([json_value/0,
              json_boolean/0,
              json_number/0,
              json_string/0,
              json_array/0,
              json_object/0,

              encode_option/0,
              decode_option/0]).

-type json_value() :: json_number() |
                      json_string() |
                      json_array() |
                      json_object() |
                      json_boolean() |
                      null |
                      undefined.
-type json_boolean() :: boolean().
-type json_number() :: number().
-type json_string() :: binary() | atom().
-type json_array() :: [json_value()].
-type json_object() :: #{json_string() => json_value()}.

-type encode_option() :: skip_undefined |
                         undefined_as_null |
                         {float_format, [float_format_option()]}.

-type decode_option() :: {keys, attempt_atom}.

-type float_format_option() :: {scientific, Decimals :: 0..249} |
                               {decimals, Decimals :: 0..253} |
                               compact |
                               short.

-type stack_item() :: {Module :: module(),
                       Function :: atom(),
                       Arity :: arity() | (Args :: [term()]),
                       Location :: [{file, Filename :: string()} | {line, Line :: pos_integer()}]}.


%% @equiv decode(Json, [])
-spec decode(binary()) -> json_value().
decode(Json) ->
    decode(Json, []).


%% @doc Decodes an erlang term from json text (a utf8 encoded binary)
%%
%% Raises an error exception if input is not valid json
-spec decode(binary(), [decode_option()]) -> json_value().
decode(Json, Options) ->
    Decoders = create_decoders(Options, #{}),
    {Value, ok, Remainings} = json:decode(Json, ok, Decoders),
    check_decode_remainings(Remainings),
    Value.


%% @equiv try_decode(Json, [])
-spec try_decode(binary()) -> {ok, json_value(), Remainings :: binary()} |
                              {error, {Reason :: term(), [stack_item()]}}.
try_decode(Json) ->
    try_decode(Json, []).


%% @doc Decodes an erlang term from json text (a utf8 encoded binary)
-spec try_decode(binary(), [decode_option()]) ->
          {ok, json_value(), Remainings :: binary()} | {error, {Reason :: term(), [stack_item()]}}.
try_decode(Json, Options) ->
    Decoders = create_decoders(Options, #{}),
    try
        {Value, ok, Remainings} = json:decode(Json, ok, Decoders),
        {ok, Value, Remainings}
    catch
        error:Reason:Stacktrace ->
            {erorr, {Reason, Stacktrace}}
    end.


%% @equiv encode(JsonValue, [])
-spec encode(json_value()) -> binary().
encode(JsonValue) ->
    encode(JsonValue, []).


%% @doc Encodes an erlang term into json text (a utf8 encoded binary)
%%
%% Raises an error exception if input is not an instance of type `json_value()'
-spec encode(json_value(), [encode_option()]) -> binary().
encode(JsonValue, Options) ->
    try
        {ok, Binary} = try_encode(JsonValue, Options),
        Binary
    catch
        error:{badmatch, {error, {Reason, [StackItem]}}}:Stacktrace ->
            erlang:raise(error, Reason, [StackItem | Stacktrace])
    end.


%% @equiv try_encode(JsonValue, [])
-spec try_encode(json_value()) -> {ok, binary()} | {error, {Reason :: term(), [stack_item()]}}.
try_encode(JsonValue) ->
    try_encode(JsonValue, []).


%% @doc Encodes an erlang term into json text (a utf8 encoded binary)
-spec try_encode(json_value(), [encode_option()]) -> {ok, binary()} |
                                                     {error, {Reason :: term(), [stack_item()]}}.
try_encode(JsonValue, Options) ->
    error(todo, [JsonValue, Options]).


%%--------------------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------------------
-spec create_decoders([decode_option()], json:decoders()) -> json:decoders().
create_decoders([], Acc) ->
    Acc;
create_decoders([{keys, attempt_atom} | Options], Acc) ->
    ObjectPush =
        fun(Key, Value, Members) ->
                try
                    [{binary_to_existing_atom(Key, utf8), Value} | Members]
                catch
                    error:badarg ->
                        [{Key, Value} | Members]
                end
        end,
    create_decoders(Options, Acc#{object_push => ObjectPush});
create_decoders([_ | _] = Options, Acc) ->
    %% 不明なオプションがあった
    erlang:error(badarg, [Options, Acc]).


-spec check_decode_remainings(binary()) -> ok.
check_decode_remainings(<<>>) ->
    ok;
check_decode_remainings(<<$ , Bin/binary>>) ->
    check_decode_remainings(Bin);
check_decode_remainings(<<$\t, Bin/binary>>) ->
    check_decode_remainings(Bin);
check_decode_remainings(<<$\r, Bin/binary>>) ->
    check_decode_remainings(Bin);
check_decode_remainings(<<$\n, Bin/binary>>) ->
    check_decode_remainings(Bin);
check_decode_remainings(<<Bin/binary>>) ->
    erlang:error(badarg, [Bin]).


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


decode_test() ->
    ?assertEqual(#{<<"foo">> => 1}, decode(~'{"foo": 1}')),
    ?assertEqual(#{foo => 1}, decode(~'{"foo": 1}', [{keys, attempt_atom}])),
    ok.


-endif.
