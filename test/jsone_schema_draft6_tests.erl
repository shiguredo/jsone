%% JSON-Schema-Test-Suite の draft 6 テストを EUnit で実行する
%%
%% https://github.com/json-schema-org/JSON-Schema-Test-Suite
%% スイートの全ファイルを読み込み、テストケースごとに EUnit のテストを生成する。
-module(jsone_schema_draft6_tests).

-include_lib("eunit/include/eunit.hrl").


%% すべてのテストケースを EUnit のテストとして生成する
draft6_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun build_tests/1}.


%% Internal Functions


setup() ->
    TestDir = test_dir(),
    SuiteDir = filename:join([TestDir, "JSON-Schema-Test-Suite", "tests", "draft6"]),
    RemotesDir = filename:join([TestDir, "JSON-Schema-Test-Suite", "remotes"]),
    MetaSchema = load_json(filename:join([TestDir, "meta-schemas", "draft-06.json"])),
    Schemas =
        (load_remote_schemas(RemotesDir))#{
          <<"http://json-schema.org/draft-06/schema">> =>
              MetaSchema
         },
    Files = filelib:wildcard(filename:join(SuiteDir, "*.json")),
    #{schemas => Schemas, files => [ {filename:basename(File), load_json(File)} || File <:- Files ]}.


cleanup(_Data) ->
    ok.


build_tests(Data) ->
    Schemas = maps:get(schemas, Data),
    [ {test_name(File, Group, Test), fun() -> run_case(Schemas, Group, Test) end}
      || {File, Groups} <:- maps:get(files, Data),
         Group <:- Groups,
         Test <:- maps:get(<<"tests">>, Group) ].


test_name(File, Group, Test) ->
    lists:flatten(io_lib:format("~s: ~s: ~s",
                                [File,
                                 maps:get(<<"description">>, Group),
                                 maps:get(<<"description">>, Test)])).


run_case(Schemas, Group, Test) ->
    JsonSchema = maps:get(<<"schema">>, Group),
    Instance = maps:get(<<"data">>, Test),
    Options = #{schemas => Schemas},
    Result = jsone_schema:validate(JsonSchema, Instance, Options),
    case maps:get(<<"valid">>, Test) of
        true ->
            ?assertEqual({ok, Instance}, Result);
        false ->
            ?assertMatch({error, _}, Result)
    end.


%% テストデータのあるディレクトリを探す
%%
%% rebar3 はプロジェクトルートで実行されることを期待しつつ、
%% ビルドディレクトリからも辿れるようにしておく。
test_dir() ->
    Candidates = ["test", filename:join([code:lib_dir(jsone), "..", "..", "..", "test"])],
    case [ Dir || Dir <:- Candidates, filelib:is_dir(Dir) ] of
        [Dir | _] ->
            Dir;
        [] ->
            erlang:error({test_dir_not_found, Candidates})
    end.


%% リモートのスキーマをファイルから読み込む
%%
%% refRemote のテストは http://localhost:1234/... を参照するため、
%% HTTP サーバを立てずにファイルをその URI に紐付けて渡す。
load_remote_schemas(RemotesDir) ->
    Files =
        [ File
          || File <:- filelib:wildcard(filename:join(RemotesDir, "**")),
             filelib:is_regular(File) ],
    maps:from_list([ {remote_uri(RemotesDir, File), load_json(File)} || File <:- Files ]).


%% リモートディレクトリからの相対パスを URI に変換する
remote_uri(RemotesDir, File) ->
    AbsDir = jsone_schema_uri:to_binary(filename:absname(RemotesDir)),
    AbsFile = jsone_schema_uri:to_binary(filename:absname(File)),
    Relative =
        binary:part(AbsFile, byte_size(AbsDir) + 1, byte_size(AbsFile) - byte_size(AbsDir) - 1),
    <<"http://localhost:1234/", Relative/binary>>.


load_json(File) ->
    {ok, Binary} = file:read_file(File),
    jsone:decode(Binary).
