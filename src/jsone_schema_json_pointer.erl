%% JSON Schema の `$ref` で使う JSON Pointer (RFC 6901) の実装
%%
%% JSON Pointer は `/` で区切られたトークン列でドキュメント内の値を指す。
%% `~1` は `/`、`~0` は `~` にデコードする。
%% URI フラグメントとして使われる場合はパーセントエンコードもデコードする。
-module(jsone_schema_json_pointer).

-export([parse/1, eval/2, eval_path/2]).

-export_type([tokens/0]).

%% 空文字列はドキュメント全体を指す
-type token() :: binary().
-type tokens() :: [token()].


%% JSON Pointer をトークン列に分解する
-spec parse(binary() | string()) -> {ok, tokens()} | {error, invalid_pointer}.
parse(Pointer) when is_list(Pointer) ->
    parse(jsone_schema_uri:to_binary(Pointer));
parse(~"") ->
    {ok, []};
parse(<<"/", _/binary>> = Pointer) ->
    [_ | Tokens] = binary:split(Pointer, <<"/">>, [global]),
    {ok, [ unescape(Token) || Token <:- Tokens ]};
parse(_Pointer) ->
    {error, invalid_pointer}.


%% JSON Pointer を評価してドキュメント内の値を取り出す
-spec eval(binary() | string(), term()) -> {ok, term()} | {error, not_found}.
eval(Pointer, Document) ->
    case eval_path(Pointer, Document) of
        {ok, Value, _Visited} ->
            {ok, Value};
        {error, not_found} ->
            {error, not_found}
    end.


%% JSON Pointer を評価して、訪問した値のリストも返す
%%
%% リストはドキュメントのルートから参照先まで順に並ぶ。
%% 途中のスキーマの `$id' を基準 URI に反映するために使う。
-spec eval_path(binary() | string(), term()) -> {ok, term(), [term()]} | {error, not_found}.
eval_path(Pointer, Document) when is_list(Pointer) ->
    eval_path(jsone_schema_uri:to_binary(Pointer), Document);
eval_path(Pointer, Document) when is_binary(Pointer) ->
    case parse(Pointer) of
        {ok, Tokens} ->
            eval_tokens(Tokens, Document, []);
        {error, invalid_pointer} ->
            {error, not_found}
    end.


%% Internal Functions


eval_tokens([], Value, Parents) ->
    {ok, Value, lists:reverse([Value | Parents])};
eval_tokens([Token | Rest], Document, Parents) when is_map(Document) ->
    case Document of
        #{Token := Value} ->
            eval_tokens(Rest, Value, [Document | Parents]);
        _ ->
            {error, not_found}
    end;
eval_tokens([Token | Rest], Document, Parents) when is_list(Document) ->
    case parse_index(Token) of
        {ok, Index} when Index >= 0, Index < length(Document) ->
            eval_tokens(Rest, lists:nth(Index + 1, Document), [Document | Parents]);
        _ ->
            {error, not_found}
    end;
eval_tokens(_Tokens, _Document, _Parents) ->
    {error, not_found}.


%% RFC 6901 では配列のインデックスは "0" か [1-9][0-9]* のみ許可される
parse_index(<<"0">>) ->
    {ok, 0};
parse_index(<<C, _/binary>> = Token) when C >= $1, C =< $9 ->
    try binary_to_integer(Token) of
        Index ->
            {ok, Index}
    catch
        error:badarg ->
            error
    end;
parse_index(_Token) ->
    error.


unescape(Token) ->
    Decoded = percent_decode(Token),
    binary:replace(binary:replace(Decoded, <<"~1">>, <<"/">>, [global]),
                   <<"~0">>,
                   <<"~">>,
                   [global]).


%% URI フラグメントの JSON Pointer はパーセントエンコードされているため、
%% トークン単位でデコードする
percent_decode(Token) ->
    case uri_string:percent_decode(Token) of
        Decoded when is_binary(Decoded) ->
            Decoded;
        _Other ->
            Token
    end.
