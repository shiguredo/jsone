%% JSON Schema draft 6 のバリデータ
%%
%% データとスキーマは jsone:decode/1 が返す map / boolean を前提とする。
%% 外部の `$ref' は options の `schemas' か `schema_loader' で解決する。
%%
%% `validate/2,3' はスキーマを直接渡して検証し、`validate_key/2,3' は
%% ストアに登録したスキーマをキーで検証する。ストアを使わない検証は
%% グローバルな状態に依存しない。
-module(jsone_schema).

-export([add_schema/2, add_schema/3,
         clear_schemas/0,
         del_schema/1,
         get_schema/1,
         list_schemas/0,
         load_schemas/1, load_schemas/2,
         validate/2, validate/3,
         validate_key/2, validate_key/3]).

-export_type([add_schema_options/0,
              error/0,
              json_value/0,
              key_error/0,
              load_schemas_options/0,
              max_errors/0,
              schema/0,
              schema_loader/0,
              validate_options/0]).

-include("jsone_schema.hrl").

%% 公開 API ごとに受け付けるオプションのキー
-define(OPTIONS_VALIDATE,     [max_errors, schema_loader, schemas, validate_format]).
-define(OPTIONS_ADD_SCHEMA,   [parser_fun]).
-define(OPTIONS_LOAD_SCHEMAS, [parser_fun, recursive]).

-type schema() :: map() | boolean().
-type json_value() :: jsone:json_value().

%% 何件のエラーを集めたら検証を打ち切るか
%%
%% 1 を指定すると最初のエラーで打ち切り、infinity ですべてのエラーを集める。
-type max_errors() :: pos_integer() | infinity.

%% 外部の `$ref' を解決するためのローダ
%%
%% `{ok, Schema}' でもスキーマそのものでも受け付ける。
%% スキーマでない値や例外を返した場合も、原因を追えるよう
%% `schema_load_error' の詳細として報告する。
-type schema_loader() :: fun((binary()) -> {ok, schema()} | schema() | {error, term()}).

%% `validate/2,3' と `validate_key/2,3' が受け付けるオプション
-type validate_options() :: #{
                              max_errors => max_errors(),
                              schema_loader => schema_loader(),
                              schemas => #{binary() => schema()},
                              validate_format => boolean()
                             }.

%% `add_schema/3' が受け付けるオプション
-type add_schema_options() :: #{parser_fun => fun((binary()) -> term())}.

%% `load_schemas/2' が受け付けるオプション
-type load_schemas_options() :: #{
                                  parser_fun => fun((binary()) -> term()),
                                  recursive => boolean()
                                 }.

%% 検証のエラー
%%
%% スキーマを直接渡す `validate/2,3' はこの形だけを返す。
-type error() :: {error, [jsone_schema_error:reason()]}.

%% キーを指定する `validate_key/2,3' のエラー
%%
%% キーがストアに登録されていない場合は schema_not_found を返す。
-type key_error() :: error() | {error, {schema_not_found, binary()}}.


%% スキーマを直接渡してデータを検証する
-spec validate(schema(), json_value()) -> {ok, json_value()} | error().
validate(JsonSchema, Data) ->
    validate(JsonSchema, Data, #{}).


%% オプション付きでスキーマを直接渡してデータを検証する
%%
%% ストアは参照しない。外部の `$ref' は `schemas' か `schema_loader' で解決する。
-spec validate(schema(), json_value(), validate_options()) -> {ok, json_value()} | error().
validate(JsonSchema, Data, Options) ->
    ok = check_options(Options, ?OPTIONS_VALIDATE),
    do_validate(JsonSchema, Data, Options, undefined).


%% 登録済みのスキーマをキーで指定してデータを検証する
-spec validate_key(binary() | string(), json_value()) -> {ok, json_value()} | key_error().
validate_key(Key, Data) ->
    validate_key(Key, Data, #{}).


%% オプション付きで登録済みのスキーマをキーで指定してデータを検証する
%%
%% `schema_loader' を指定しない場合は、ストアから外部スキーマを読み込む。
-spec validate_key(binary() | string(), json_value(), validate_options()) ->
          {ok, json_value()} | key_error().
validate_key(Key, Data, Options) ->
    ok = check_options(Options, ?OPTIONS_VALIDATE),
    KeyBin = jsone_schema_uri:to_binary(Key),
    maybe
        {ok, JsonSchema} ?= jsone_schema_store:get(KeyBin),
        SchemaLoader = maps:get(schema_loader, Options, fun jsone_schema_store:get/1),
        do_validate(JsonSchema, Data, Options#{schema_loader => SchemaLoader}, KeyBin)
    else
        {error, not_found} ->
            {error, {schema_not_found, KeyBin}}
    end.


%% スキーマをパース済みの値として登録する
-spec add_schema(binary() | string(), schema()) -> ok | {error, term()}.
add_schema(Key, JsonSchema) when is_map(JsonSchema); is_boolean(JsonSchema) ->
    jsone_schema_store:add(Key, JsonSchema);
add_schema(_Key, JsonSchema) ->
    {error, {invalid_schema, JsonSchema}}.


%% スキーマをバイナリからパースして登録する
%%
%% `parser_fun' を指定しない場合は jsone:decode/1 を使う。
-spec add_schema(binary() | string(), binary() | schema(), add_schema_options()) ->
          ok | {error, term()}.
add_schema(Key, JsonSchema, Options) ->
    ok = check_options(Options, ?OPTIONS_ADD_SCHEMA),
    maybe
        {ok, ParsedSchema} ?= ensure_schema(JsonSchema, Options),
        jsone_schema_store:add(Key, ParsedSchema)
    end.


%% キーに紐付いたスキーマを削除する
%%
%% `$id' で登録した分もあわせて削除する。
-spec del_schema(binary() | string()) -> ok.
del_schema(Key) ->
    jsone_schema_store:delete(Key).


%% キーに紐付いたスキーマを取得する
-spec get_schema(binary() | string()) -> {ok, schema()} | {error, not_found}.
get_schema(Key) ->
    jsone_schema_store:get(Key).


%% 登録されている全スキーマをキーとスキーマのマップで返す
-spec list_schemas() -> #{binary() => schema()}.
list_schemas() ->
    maps:from_list(jsone_schema_store:list()).


%% 登録されている全スキーマを削除する
-spec clear_schemas() -> ok.
clear_schemas() ->
    jsone_schema_store:clear().


%% ディレクトリ以下のスキーマファイルを読み込んで登録する
%%
%% キーは `file://' で始まる絶対パスの URI になる。
-spec load_schemas(binary() | string()) -> ok | {error, {file:filename(), term()}}.
load_schemas(Path) ->
    load_schemas(Path, #{}).


%% オプション付きでスキーマファイルを読み込んで登録する
%%
%% `parser_fun' を指定しない場合は jsone:decode/1 を使う。
%% `recursive' を false にするとディレクトリ直下のファイルだけを読む。
-spec load_schemas(binary() | string(), load_schemas_options()) ->
          ok | {error, {file:filename(), term()}}.
load_schemas(Path, Options) ->
    %% ファイルを集める前にオプションを検査する
    ok = check_options(Options, ?OPTIONS_LOAD_SCHEMAS),
    ParserFun = maps:get(parser_fun, Options, fun jsone:decode/1),
    Recursive = maps:get(recursive, Options, true),
    Dir = binary_to_list(jsone_schema_uri:to_binary(Path)),
    Files = collect_files(Dir, Recursive),
    load_schema_files(Files, ParserFun).


%% Internal Functions


%% オプションを検査する
%%
%% 不明なキーは受け付けない。既存の `jsone:decode/2' と同じく badarg にする。
check_options(Options, AllowedKeys) when is_map(Options) ->
    maps:fold(fun(Key, Value, ok) ->
                      case lists:member(Key, AllowedKeys) of
                          true ->
                              check_option_value(Key, Value);
                          false ->
                              erlang:error(badarg, [Options, AllowedKeys])
                      end
              end,
              ok,
              Options);
check_options(Options, AllowedKeys) ->
    erlang:error(badarg, [Options, AllowedKeys]).


%% オプションの値を検査する
check_option_value(max_errors, Value) when is_integer(Value), Value > 0 ->
    ok;
check_option_value(max_errors, infinity) ->
    ok;
check_option_value(parser_fun, Value) when is_function(Value, 1) ->
    ok;
check_option_value(recursive, Value) when is_boolean(Value) ->
    ok;
check_option_value(schema_loader, Value) when is_function(Value, 1) ->
    ok;
check_option_value(schemas, Value) when is_map(Value) ->
    ok;
check_option_value(validate_format, Value) when is_boolean(Value) ->
    ok;
check_option_value(Key, Value) ->
    erlang:error(badarg, [Key, Value]).


%% 上限に達するまでエラーを集めて検証する
do_validate(JsonSchema, Data, Options, DocumentURI) ->
    try
        State0 = jsone_schema_state:new(JsonSchema, Options, DocumentURI),
        State1 = jsone_schema_validator:validate_with_state(JsonSchema, Data, State0),
        case jsone_schema_state:get_errors(State1) of
            [] ->
                {ok, Data};
            Errors ->
                {error, Errors}
        end
    catch
        throw:{?ERRORS, ThrownErrors} ->
            {error, ThrownErrors};
        throw:{?REF_ABORT, Reason} ->
            {error, [Reason]}
    end.


%% パース済みのスキーマはそのまま、バイナリは parser_fun でパースする
%%
%% パースで例外が起きた場合は例外クラスと理由を残す。クラスを捨てると
%% `error' と `throw' の区別が付かず、原因を追えなくなる。
ensure_schema(JsonSchema, _Options) when is_map(JsonSchema); is_boolean(JsonSchema) ->
    {ok, JsonSchema};
ensure_schema(JsonSchema, Options) ->
    ParserFun = maps:get(parser_fun, Options, fun jsone:decode/1),
    try ParserFun(JsonSchema) of
        ParsedSchema when is_map(ParsedSchema); is_boolean(ParsedSchema) ->
            {ok, ParsedSchema};
        Other ->
            {error, {invalid_schema, Other}}
    catch
        Class:Reason ->
            {error, {parse_error, {Class, Reason}}}
    end.


%% ファイルを順に読み込み、最初に失敗したファイルで打ち切る
load_schema_files([], _ParserFun) ->
    ok;
load_schema_files([File | Rest], ParserFun) ->
    maybe
        ok ?= load_schema_file(File, ParserFun),
        load_schema_files(Rest, ParserFun)
    else
        {error, Reason} ->
            {error, {File, Reason}}
    end.


load_schema_file(File, ParserFun) ->
    maybe
        {ok, Binary} ?= file:read_file(File),
        add_schema(file_uri(File), Binary, #{parser_fun => ParserFun})
    end.


%% ディレクトリ以下の通常ファイルを集める
%%
%% `recursive' が true の場合はサブディレクトリも対象にする。
%% ディレクトリ自身は filelib:is_regular/1 で除外する。
-spec collect_files(file:filename(), boolean()) -> [file:filename()].
collect_files(Dir, true) ->
    [ File || File <:- filelib:wildcard(filename:join(Dir, "**")), filelib:is_regular(File) ];
collect_files(Dir, false) ->
    [ File || File <:- filelib:wildcard(filename:join(Dir, "*")), filelib:is_regular(File) ].


file_uri(File) ->
    <<"file://", (jsone_schema_uri:to_binary(filename:absname(File)))/binary>>.
