%% JSON Schema の `$id` / `$ref` で使う URI 解決
%%
%% 基本は `uri_string:resolve/2' で解決し、URI として解釈できない不透明な
%% キー (ストアの任意キーなど) でもフラグメント参照が動くようにフォールバックする。
-module(jsone_schema_uri).

-export([resolve/2, split_fragment/1, to_binary/1, to_binary_or_undefined/1, is_absolute/1]).


%% 基準 URI に対して参照 URI を解決する
%%
%% 参照 URI が絶対 URI の場合は基準 URI を使わない。基準 URI が無い場合は
%% 参照 URI をそのまま返す。基準 URI が不透明なキーの場合は、フラグメント
%% 参照だけを連結してローカル参照として扱う。
-spec resolve(undefined | binary() | string(), binary() | string()) -> binary().
resolve(Base0, Ref0) ->
    Base = to_binary_or_undefined(Base0),
    Ref = to_binary(Ref0),
    case has_scheme(Ref) of
        true ->
            Ref;
        false when Base =:= undefined ->
            Ref;
        false ->
            maybe
                {ok, Resolved} ?= try_resolve(Ref, Base),
                Resolved
            else
                error ->
                    resolve_opaque(Base, Ref)
            end
    end.


%% URI をドキュメント URI とフラグメントに分割する
-spec split_fragment(binary()) -> {binary(), binary()}.
split_fragment(URI) ->
    case binary:split(URI, <<"#">>) of
        [Document] ->
            {Document, ~""};
        [Document, Fragment] ->
            {Document, Fragment}
    end.


%% 文字列またはバイナリをバイナリに変換する
-spec to_binary(binary() | string()) -> binary().
to_binary(Value) when is_binary(Value) ->
    Value;
to_binary(Value) when is_list(Value) ->
    case unicode:characters_to_binary(Value) of
        Binary when is_binary(Binary) ->
            Binary;
        _Error ->
            erlang:error(badarg, [Value])
    end.


%% 絶対 URI かどうかを返す
-spec is_absolute(binary() | string()) -> boolean().
is_absolute(URI) ->
    has_scheme(to_binary(URI)).


%% 未定義を許容してバイナリに変換する
-spec to_binary_or_undefined(undefined | binary() | string()) -> undefined | binary().
to_binary_or_undefined(undefined) ->
    undefined;
to_binary_or_undefined(Value) ->
    to_binary(Value).


%% Internal Functions


%% `uri_string:resolve/2' の戻り値を `{ok, binary()} | error' に正規化する
try_resolve(Ref, Base) ->
    case uri_string:resolve(Ref, Base) of
        Resolved when is_binary(Resolved) ->
            {ok, Resolved};
        {error, _Reason, _Rest} ->
            error
    end.


%% 不透明な基準キーに対しては、フラグメント参照だけを連結してローカル参照にする。
%% 基準に既にフラグメントがある場合は、RFC 3986 5.2.2 と同じく置換する。
%% 相対参照はストアのキーそのものを指しているとみなし、そのまま返す。
resolve_opaque(Base, <<"#", _/binary>> = Ref) ->
    {BaseDocument, _Fragment} = split_fragment(Base),
    <<BaseDocument/binary, Ref/binary>>;
resolve_opaque(_Base, Ref) ->
    Ref.


%% URI がスキームを持つかどうかを調べる
%%
%% `uri_string:parse/1' は不正な URI に対しては `{error, Reason, Rest}' を
%% 返し、バイナリ以外を渡した場合は例外になる。どちらも false として扱う。
has_scheme(URI) ->
    try uri_string:parse(URI) of
        Parsed when is_map(Parsed) ->
            maps:is_key(scheme, Parsed);
        _Other ->
            false
    catch
        _:_ ->
            false
    end.
