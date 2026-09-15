%% swidden が使う jesse 互換 API の EUnit テスト
-module(jesse_compat_tests).

-include_lib("eunit/include/eunit.hrl").


parser_fun() ->
    fun jsone:decode/1.


%% swidden と同じ使い方で、スキーマを登録してキーで検証する
jesse_add_and_validate_test() ->
    ok = jsone_schema:clear_schemas(),
    Key = "jesse_compat_tests",
    Schema = ~'{"type":"object","properties":{"foo":{"type":"integer"}},"required":["foo"]}',
    ?assertEqual(ok, jesse:add_schema(Key, Schema, [{parser_fun, parser_fun()}])),
    ?assertEqual({ok, #{<<"foo">> => 1}},
                 jesse:validate(Key, ~'{"foo":1}', [{parser_fun, parser_fun()}])),
    ?assertMatch({error, _}, jesse:validate(Key, ~'{"bar":1}', [{parser_fun, parser_fun()}])),
    ok.


%% エラー理由は swidden の to_json が扱うタプル形式で返る
jesse_error_reason_test() ->
    ok = jsone_schema:clear_schemas(),
    Key = "jesse_compat_tests_error",
    ok = jesse:add_schema(Key, ~'{"type":"integer"}', [{parser_fun, parser_fun()}]),
    Result = jesse:validate(Key, ~'"x"', [{parser_fun, parser_fun()}]),
    ?assertMatch({error, [{data_invalid, _, wrong_type, <<"x">>, []}]}, Result),
    ok.


%% スキーマのパースエラーも jesse の形で返る
jesse_schema_parse_error_test() ->
    Key = "jesse_compat_tests_parse_error",
    ?assertMatch({error, [{schema_error, {parse_error, _}}]},
                 jesse:add_schema(Key, ~'{"type":', [{parser_fun, parser_fun()}])),
    ok.


%% データのパースエラーも jesse の形で返る
jesse_data_parse_error_test() ->
    ok = jsone_schema:clear_schemas(),
    Key = "jesse_compat_tests_data_parse_error",
    ok = jesse:add_schema(Key, ~'{"type":"integer"}', [{parser_fun, parser_fun()}]),
    ?assertMatch({error, [{data_error, {parse_error, _}}]},
                 jesse:validate(Key, ~'', [{parser_fun, parser_fun()}])),
    ok.


%% 未登録のキーは database_error を返す
jesse_validate_missing_schema_test() ->
    ?assertEqual({error, {database_error, "no_such_key", schema_not_found}},
                 jesse:validate("no_such_key", #{}, [])),
    ok.
