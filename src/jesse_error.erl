%% jesse_error 互換 API
%%
%% swidden が必要とするエラー型と、コアのエラーからの変換だけを提供する。
%% jesse のタプル形式は `{data_invalid, Schema, Error, Data, Path}' と
%% `{schema_invalid, Schema, Error}' の 2 種類。
-module(jesse_error).

-export([from_core/1]).

-export_type([error/0, error_info/0, error_reason/0]).

-type json_value() :: jsone:json_value().
-type schema() :: map() | boolean().

%% エラーの内容。単純なエラーは atom、詳細がある場合は `{atom(), term()}'。
-type error_info() :: atom() | {atom(), term()}.

-type error_reason() ::
        {data_invalid, schema(), error_info(), json_value(), [binary() | non_neg_integer()]} |
        {schema_invalid, schema(), error_info()} |
        {data_error, {parse_error, term()}} |
        {schema_error, {parse_error, term()}}.

-type error() :: {error, [error_reason()]}.


%% コアのエラー理由を jesse のタプル形式に変換する
-spec from_core([jsone_schema_error:reason()]) -> [error_reason()].
from_core(Reasons) ->
    [ reason_from_core(Reason) || Reason <:- Reasons ].


%% Internal Functions


%% コアのエラー理由 1 件を jesse のタプルに変換する
reason_from_core(#{kind := data, schema := Schema, error := Error, value := Value, path := Path}) ->
    case Error of
        {Name, Details} ->
            {data_invalid, Schema, {Name, details_from_core(Details)}, Value, Path};
        _ ->
            {data_invalid, Schema, Error, Value, Path}
    end;
reason_from_core(#{kind := schema, schema := Schema, error := Error}) ->
    case Error of
        {Name, Details} ->
            {schema_invalid, Schema, {Name, details_from_core(Details)}};
        _ ->
            {schema_invalid, Schema, Error}
    end.


%% 詳細に別のエラー理由が入っている場合は再帰的に変換する
details_from_core(Details) when is_list(Details) ->
    case lists:all(fun(Detail) -> is_map(Detail) andalso maps:is_key(kind, Detail) end, Details) of
        true ->
            from_core(Details);
        false ->
            Details
    end;
details_from_core(Details) ->
    Details.
