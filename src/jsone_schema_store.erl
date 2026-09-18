%% JSON Schema のスキーマストア
%%
%% persistent_term のキー `{jsone_schema_store, Key}' にスキーマを保存する。
%% スキーマは起動時に登録して検証時に読み出す使い方を想定しており、
%% 読み出しはコピーなしで高速、書き込みは頻度が低いという前提で永続 term を使う。
%% ETS と違いテーブルの所有者プロセスが不要で、アプリケーションを起動しなくても使える。
-module(jsone_schema_store).

-export([add/2, clear/0, delete/1, get/1, list/0]).

-include("jsone_schema.hrl").


%% スキーマをキーに紐付けて登録する
%%
%% `$id' を持つスキーマは、キーだけでなく解決した絶対 URI でも引けるようにする。
-spec add(binary() | string(), jsone_schema:schema()) -> ok.
add(Key, JsonSchema) ->
    KeyBin = jsone_schema_uri:to_binary(Key),
    lists:foreach(fun(SchemaKey) -> persistent_term:put(key(SchemaKey), JsonSchema) end,
                  schema_keys(KeyBin, JsonSchema)).


%% キーに紐付いたスキーマを取得する
-spec get(binary() | string()) -> {ok, jsone_schema:schema()} | {error, not_found}.
get(Key) ->
    KeyBin = jsone_schema_uri:to_binary(Key),
    try persistent_term:get(key(KeyBin)) of
        JsonSchema ->
            {ok, JsonSchema}
    catch
        %% 未登録のキーを引くと badarg になる
        error:badarg ->
            {error, not_found}
    end.


%% キーに紐付いたスキーマを削除する
%%
%% `$id' で登録した分もあわせて削除する。未登録のキーは何もしない。
-spec delete(binary() | string()) -> ok.
delete(Key) ->
    KeyBin = jsone_schema_uri:to_binary(Key),
    maybe
        {ok, JsonSchema} ?= ?MODULE:get(KeyBin),
        lists:foreach(fun(SchemaKey) -> persistent_term:erase(key(SchemaKey)) end,
                      schema_keys(KeyBin, JsonSchema))
    else
        {error, not_found} ->
            ok
    end.


%% 登録されている全スキーマを返す
-spec list() -> [{binary(), jsone_schema:schema()}].
list() ->
    [ unwrap_entry(Entry) || Entry <:- persistent_term:get(), is_store_entry(Entry) ].


%% 登録されている全スキーマを削除する
-spec clear() -> ok.
clear() ->
    Keys = [ Key || {Key, _Value} <:- persistent_term:get(), is_store_key(Key) ],
    lists:foreach(fun(Key) -> persistent_term:erase(Key) end, Keys),
    ok.


%% Internal Functions


%% persistent_term のキー
%%
%% 他の用途の term と混ざらないようにモジュール名を前置する。
key(KeyBin) ->
    {?MODULE, KeyBin}.


%% スキーマを引くためのキーの一覧
%%
%% `$id' を持つスキーマは、登録したキーと `$id' を解決した絶対 URI の
%% 両方で引けるようにする。
schema_keys(KeyBin, JsonSchema) ->
    case schema_id(JsonSchema) of
        undefined ->
            [KeyBin];
        Id ->
            [KeyBin, jsone_schema_uri:resolve(KeyBin, Id)]
    end.


schema_id(JsonSchema) when is_map(JsonSchema) ->
    case maps:get(?ID, JsonSchema, undefined) of
        Id when is_binary(Id) ->
            Id;
        _ ->
            undefined
    end;
schema_id(_JsonSchema) ->
    undefined.


is_store_key({?MODULE, _Key}) ->
    true;
is_store_key(_Key) ->
    false.


is_store_entry({{?MODULE, _Key}, _JsonSchema}) ->
    true;
is_store_entry(_Entry) ->
    false.


unwrap_entry({{?MODULE, Key}, JsonSchema}) ->
    {Key, JsonSchema}.
