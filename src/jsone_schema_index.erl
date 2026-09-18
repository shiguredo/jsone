%% ドキュメント内の `$id` の索引
%%
%% `$ref` の解決では JSON Pointer による参照のほかに、
%% `$id` で定義されたリソース URI とプレーンネームフラグメントを解決する必要がある。
%% 索引はスキーマとして解釈できる位置だけをたどり、`enum` / `const` や
%% 未知のキーワードの中にある `$id` は対象にしない。
-module(jsone_schema_index).

-export([build/2,
         lookup/2,
         entry_schema/1,
         entry_base_uri/1,
         entry_root_schema/1,
         entry_document_uri/1]).

-export_type([entry/0, index/0]).

-include("jsone_schema.hrl").

-type schema() :: map() | boolean().
-type index() :: #{binary() => entry()}.

%% 索引のエントリ
%%
%% `base_uri' は `$id' を反映する前の基準 URI で、参照先の検証を始めるときに
%% `enter_schema' が改めて `$id' を反映する。
-record(entry, {
          schema :: schema(),
          base_uri :: undefined | binary(),
          root_schema :: schema(),
          document_uri :: undefined | binary()
         }).

-opaque entry() :: #entry{}.


%% ドキュメントをたどって `$id' を索引する
-spec build(schema(), undefined | binary()) -> index().
build(JsonSchema, DocumentURI) ->
    walk(JsonSchema, DocumentURI, JsonSchema, DocumentURI, #{}).


%% 絶対 URI で索引を引く
%%
%% 索引がまだ作られていない場合は error を返す。
-spec lookup(index() | undefined, binary()) -> {ok, entry()} | error.
lookup(undefined, _URI) ->
    error;
lookup(Index, URI) ->
    case Index of
        #{URI := Entry} ->
            {ok, Entry};
        _ ->
            error
    end.


%% エントリのスキーマを返す
-spec entry_schema(entry()) -> schema().
entry_schema(#entry{schema = JsonSchema}) ->
    JsonSchema.


%% エントリの `$id' を反映する前の基準 URI を返す
-spec entry_base_uri(entry()) -> undefined | binary().
entry_base_uri(#entry{base_uri = BaseURI}) ->
    BaseURI.


%% エントリを含むドキュメントのルートスキーマを返す
-spec entry_root_schema(entry()) -> schema().
entry_root_schema(#entry{root_schema = RootSchema}) ->
    RootSchema.


%% エントリを含むドキュメントの URI を返す
-spec entry_document_uri(entry()) -> undefined | binary().
entry_document_uri(#entry{document_uri = DocumentURI}) ->
    DocumentURI.


%% Internal Functions


walk(JsonSchema, Base, RootSchema, DocumentURI, Acc0) when is_map(JsonSchema) ->
    {Base1, Acc} = index_id(JsonSchema, Base, RootSchema, DocumentURI, Acc0),
    walk_keywords(JsonSchema, Base1, RootSchema, DocumentURI, Acc);
walk(_JsonSchema, _Base, _RootSchema, _DocumentURI, Acc) ->
    Acc.


index_id(JsonSchema, Base, RootSchema, DocumentURI, Acc) ->
    case JsonSchema of
        #{?ID := Id} when is_binary(Id) ->
            Absolute = jsone_schema_uri:resolve(Base, Id),
            Entry =
                #entry{
                  schema = JsonSchema,
                  base_uri = Base,
                  root_schema = RootSchema,
                  document_uri = DocumentURI
                 },
            {Absolute, Acc#{Absolute => Entry}};
        _ ->
            {Base, Acc}
    end.


walk_keywords(JsonSchema, Base, RootSchema, DocumentURI, Acc0) ->
    Acc1 = walk_schema_maps(JsonSchema, Base, RootSchema, DocumentURI, Acc0),
    Acc2 = walk_schema_values(JsonSchema, Base, RootSchema, DocumentURI, Acc1),
    Acc3 = walk_schema_arrays(JsonSchema, Base, RootSchema, DocumentURI, Acc2),
    Acc4 = walk_items(JsonSchema, Base, RootSchema, DocumentURI, Acc3),
    walk_dependencies(JsonSchema, Base, RootSchema, DocumentURI, Acc4).


%% スキーマのマップを持つキーワード
walk_schema_maps(JsonSchema, Base, RootSchema, DocumentURI, Acc) ->
    Keywords = [?PROPERTIES, ?PATTERNPROPERTIES, ?DEFINITIONS, ?DEFS],
    lists:foldl(fun(Keyword, SubAcc) ->
                        walk_schema_map(Keyword, JsonSchema, Base, RootSchema, DocumentURI, SubAcc)
                end,
                Acc,
                Keywords).


walk_schema_map(Keyword, JsonSchema, Base, RootSchema, DocumentURI, Acc) ->
    case maps:get(Keyword, JsonSchema, undefined) of
        Map when is_map(Map) ->
            maps:fold(fun(_Name, SubSchema, SubAcc) ->
                              walk(SubSchema, Base, RootSchema, DocumentURI, SubAcc)
                      end,
                      Acc,
                      Map);
        _Other ->
            Acc
    end.


%% スキーマを 1 つ持つキーワード
walk_schema_values(JsonSchema, Base, RootSchema, DocumentURI, Acc) ->
    Keywords = [?ADDITIONALPROPERTIES, ?ADDITIONALITEMS, ?CONTAINS, ?PROPERTYNAMES, ?NOT],
    lists:foldl(fun(Keyword, SubAcc) ->
                        walk(maps:get(Keyword, JsonSchema, undefined),
                             Base,
                             RootSchema,
                             DocumentURI,
                             SubAcc)
                end,
                Acc,
                Keywords).


%% スキーマの配列を持つキーワード
walk_schema_arrays(JsonSchema, Base, RootSchema, DocumentURI, Acc) ->
    Keywords = [?ALLOF, ?ANYOF, ?ONEOF],
    lists:foldl(fun(Keyword, SubAcc) ->
                        walk_schema_array(Keyword, JsonSchema, Base, RootSchema, DocumentURI, SubAcc)
                end,
                Acc,
                Keywords).


walk_schema_array(Keyword, JsonSchema, Base, RootSchema, DocumentURI, Acc) ->
    case maps:get(Keyword, JsonSchema, undefined) of
        Schemas when is_list(Schemas) ->
            lists:foldl(fun(SubSchema, SubAcc) ->
                                walk(SubSchema, Base, RootSchema, DocumentURI, SubAcc)
                        end,
                        Acc,
                        Schemas);
        _Other ->
            Acc
    end.


%% items はスキーマかスキーマの配列
walk_items(JsonSchema, Base, RootSchema, DocumentURI, Acc) ->
    case maps:get(?ITEMS, JsonSchema, undefined) of
        Schemas when is_list(Schemas) ->
            lists:foldl(fun(SubSchema, SubAcc) ->
                                walk(SubSchema, Base, RootSchema, DocumentURI, SubAcc)
                        end,
                        Acc,
                        Schemas);
        SubSchema ->
            walk(SubSchema, Base, RootSchema, DocumentURI, Acc)
    end.


%% dependencies の値はスキーマかプロパティ名の配列
walk_dependencies(JsonSchema, Base, RootSchema, DocumentURI, Acc) ->
    case maps:get(?DEPENDENCIES, JsonSchema, undefined) of
        Dependencies when is_map(Dependencies) ->
            maps:fold(fun(_Name, Dependency, SubAcc) ->
                              walk(Dependency, Base, RootSchema, DocumentURI, SubAcc)
                      end,
                      Acc,
                      Dependencies);
        _Other ->
            Acc
    end.
