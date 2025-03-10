-module(jsone_tests).

-include_lib("eunit/include/eunit.hrl").

-import(jsone, [decode/1, decode/2, encode/1, encode/2]).


decode_test() ->
    %% Basic decoding.
    ?assertEqual(#{<<"foo">> => 1}, decode(~'{"foo": 1}')),

    %% `attempt_atom` option.
    ?assertEqual(#{foo => 1}, decode(~'{"foo": 1}', [{keys, attempt_atom}])),
    ?assertEqual(#{<<"no_existing_atom">> => 1}, decode(~'{"no_existing_atom": 1}', [{keys, attempt_atom}])),

    ok.


decode_error_test() ->
    %% 制御文字が入っていた
    ?assertError({invalid_byte, 2}, jsone:decode(<<2>>)),
    %% object の value が "" で囲まれていない
    ?assertError({invalid_byte, 97}, jsone:decode(~'{"foo": abc}')),

    %% object の開始だけが送られてきた
    ?assertError({invalid_byte, 125}, jsone:decode(~"}")),
    %% object の終了だけが送られてきた
    ?assertError(unexpected_end, jsone:decode(~"{")),
    ok.


encode_test() ->
    %% Basic encoding.
    ?assertEqual(~'{"foo":1}', encode(#{foo => 1})),
    ?assertEqual(~'{"foo":1}', encode(#{<<"foo">> => 1})),
    ?assertEqual(~'{"foo":1}', encode([{foo, 1}])),

    %% `undefined_as_null` option.
    ?assertEqual(~'{"foo":null}', encode([{foo, undefined}], [undefined_as_null])),
    ?assertEqual(~'{"undefined":[null]}', encode(#{undefined => [undefined]}, [undefined_as_null])),

    %% `skip_undefined` option.
    ?assertEqual(~'{"bar":1}', encode([{foo, undefined}, {bar, 1}], [skip_undefined])),
    ?assertEqual(~'{"bar":1}', encode(#{foo => undefined, bar => 1}, [skip_undefined])),

    %% `float_format` option.
    ?assertEqual(~'{"foo":1.1000}', encode(#{foo => 1.1}, [{float_format, [{decimals, 4}]}])),
    ?assertEqual(~'{"foo":1.1}', encode(#{foo => 1.1}, [{float_format, [{decimals, 4}, compact]}])),

    %% json モジュールが直接はサポートしていないタイプのエンコード
    %% => io_lib:format() で文字列に変換される
    ?assertEqual(~'{"foo":"{bar,baz}"}', encode(#{foo => {bar, baz}})),

    ok.
