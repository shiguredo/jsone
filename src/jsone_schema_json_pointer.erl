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
    unescape_tokens(Tokens, []);
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


%% トークン列をまとめてアンエスケープする
%%
%% パーセントデコードに失敗したトークンが 1 つでもあれば、
%% JSON Pointer 全体を不正として返す。
unescape_tokens([], Acc) ->
    {ok, lists:reverse(Acc)};
unescape_tokens([Token | Rest], Acc) ->
    case unescape(Token) of
        {ok, Decoded} ->
            unescape_tokens(Rest, [Decoded | Acc]);
        {error, invalid_pointer} = Error ->
            Error
    end.


unescape(Token) ->
    case percent_decode(Token) of
        {ok, Decoded} ->
            EscapedSlash = binary:replace(Decoded, <<"~1">>, <<"/">>, [global]),
            Unescaped = binary:replace(EscapedSlash, <<"~0">>, <<"~">>, [global]),
            {ok, Unescaped};
        {error, invalid_pointer} = Error ->
            Error
    end.


%% URI フラグメントの JSON Pointer はパーセントエンコードされているため、
%% トークン単位でデコードする
%%
%% uri_string:percent_decode/1 は不正なパーセントエンコーディングと
%% 不正 UTF-8 に対して例外を投げる。公開 API まで例外を漏らさないよう、
%% ここで捕まえて不正な JSON Pointer として扱う。
percent_decode(Token) ->
    try uri_string:percent_decode(Token) of
        Decoded when is_binary(Decoded) ->
            {ok, Decoded};
        _Other ->
            {error, invalid_pointer}
    catch
        _Class:_Reason ->
            {error, invalid_pointer}
    end.
