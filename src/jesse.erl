%% jesse (JSon Schema Erlang) 互換 API
%%
%% swidden が必要とする API だけを提供する薄い互換層で、検証は jsone_schema が行う。
%% データ表現は map のみ。mochijson2 / jiffy / jsx / proplist は受け付けない。
%% CLI と http/https によるスキーマ取得は提供しない。
-module(jesse).

-export([add_schema/3,
         validate/3]).

-export_type([json_term/0]).

-include("jsone_schema.hrl").

-type json_term() :: jsone:json_value().
-type parser_fun() :: fun((term()) -> term()).
-type option() :: {parser_fun, parser_fun()}.
-type options() :: [option()].


%% バイナリのスキーマをパースして登録する
%%
%% parser_fun の指定が無い場合はパースせずそのまま登録する。
-spec add_schema(binary() | string(), binary(), options()) ->
          ok | [term()] | jesse_error:error().
add_schema(Key, JsonSchema, Options) ->
    ParserFun = proplists:get_value(parser_fun, Options, fun(X) -> X end),
    try ParserFun(JsonSchema) of
        ParsedSchema ->
            to_store_result(Key, jsone_schema:add_schema(Key, ParsedSchema))
    catch
        _Class:Reason ->
            {error, [{?schema_error, {parse_error, Reason}}]}
    end.


%% 登録済みのスキーマをキーで指定して検証する
%%
%% parser_fun が指定された場合、Data はバイナリとしてパースする。
%% 外部スキーマはストアから読み込む。
-spec validate(binary() | string(), term(), options()) ->
          {ok, json_term()} | jesse_error:error() | jesse_database:error().
validate(Key, Data, Options) ->
    try
        ParserFun = proplists:get_value(parser_fun, Options, fun(X) -> X end),
        ParsedData = try_parse(ParserFun, Data),
        maybe
            {ok, _ParsedData} ?= jsone_schema:validate_key(Key, ParsedData, schema_options())
        else
            {error, {schema_not_found, _KeyBin}} ->
                {error, {database_error, Key, schema_not_found}};
            {error, Reasons} ->
                {error, jesse_error:from_core(Reasons)}
        end
    catch
        throw:Thrown ->
            {error, Thrown}
    end.


%% Internal Functions


schema_options() ->
    #{schema_loader => fun jsone_schema_store:get/1}.


to_store_result(_Key, ok) ->
    ok;
to_store_result(Key, {error, Reason}) ->
    [{Key, undefined, Reason}].


try_parse(ParserFun, Json) ->
    try ParserFun(Json) of
        Parsed ->
            Parsed
    catch
        _Class:Reason ->
            throw([{?data_error, {parse_error, Reason}}])
    end.
