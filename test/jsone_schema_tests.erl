%% jsone_schema の EUnit テスト
-module(jsone_schema_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DRAFT6, <<"http://json-schema.org/draft-06/schema#">>).


data_invalid_test() ->
    IntegerSchema = #{<<"$schema">> => ?DRAFT6, <<"type">> => <<"integer">>},
    Schema =
        #{
          <<"$schema">> => ?DRAFT6,
          <<"type">> => <<"object">>,
          <<"properties">> =>
              #{
                <<"foo">> =>
                    #{
                      <<"type">> => <<"object">>,
                      <<"properties">> => #{<<"subfoo">> => IntegerSchema}
                     }
               },
          <<"patternProperties">> => #{<<"^b">> => IntegerSchema}
         },

    %% 型が合う場合はそのまま返る
    ?assertEqual({ok, 42}, jsone_schema:validate(IntegerSchema, 42)),

    %% ルートレベルの型エラー
    ?assertEqual({error,
                  [#{
                     kind => data,
                     path => [],
                     schema => Schema,
                     value => <<"foo">>,
                     error => wrong_type
                    }]},
                 jsone_schema:validate(Schema, <<"foo">>)),

    %% properties の 2 階層分のパス
    ?assertEqual({error,
                  [#{
                     kind => data,
                     path => [<<"foo">>, <<"subfoo">>],
                     schema => IntegerSchema,
                     value => <<"bar">>,
                     error => wrong_type
                    }]},
                 jsone_schema:validate(Schema, #{<<"foo">> => #{<<"subfoo">> => <<"bar">>}})),

    %% patternProperties のパス
    ?assertEqual({error,
                  [#{
                     kind => data,
                     path => [<<"bar">>],
                     schema => IntegerSchema,
                     value => <<"baz">>,
                     error => wrong_type
                    }]},
                 jsone_schema:validate(Schema, #{<<"bar">> => <<"baz">>})),
    ok.


additional_properties_test() ->
    IntegerSchema = #{<<"type">> => <<"integer">>},
    Schema =
        #{
          <<"type">> => <<"object">>,
          <<"properties">> => #{<<"foo">> => IntegerSchema},
          <<"additionalProperties">> => false
         },
    ?assertEqual({ok, #{<<"foo">> => 0}}, jsone_schema:validate(Schema, #{<<"foo">> => 0})),
    ?assertEqual({error,
                  [#{
                     kind => data,
                     path => [<<"bar">>],
                     schema => Schema,
                     value => #{<<"foo">> => 0, <<"bar">> => <<"baz">>},
                     error => no_extra_properties_allowed
                    }]},
                 jsone_schema:validate(Schema, #{<<"foo">> => 0, <<"bar">> => <<"baz">>})),

    %% 2 階層目の additionalProperties
    NestedSchema =
        #{
          <<"type">> => <<"object">>,
          <<"properties">> =>
              #{
                <<"foo">> =>
                    #{
                      <<"type">> => <<"object">>,
                      <<"properties">> => #{<<"subfoo">> => IntegerSchema},
                      <<"additionalProperties">> => false
                     }
               },
          <<"additionalProperties">> => false
         },
    ?assertEqual({error,
                  [#{
                     kind => data,
                     path => [<<"foo">>, <<"bar">>],
                     schema =>
                         #{
                           <<"type">> => <<"object">>,
                           <<"properties">> => #{<<"subfoo">> => IntegerSchema},
                           <<"additionalProperties">> => false
                          },
                     value => #{<<"subfoo">> => 1, <<"bar">> => 2},
                     error => no_extra_properties_allowed
                    }]},
                 jsone_schema:validate(NestedSchema,
                                       #{<<"foo">> => #{<<"subfoo">> => 1, <<"bar">> => 2}})),
    ok.


items_test() ->
    IntegerSchema = #{<<"type">> => <<"integer">>},
    ItemsSchema =
        #{<<"type">> => <<"array">>, <<"items">> => IntegerSchema, <<"maxItems">> => 3},
    ?assertEqual({error,
                  [#{kind => data, path => [1], schema => IntegerSchema, value => <<"baz">>, error => wrong_type}]},
                 jsone_schema:validate(ItemsSchema, [2, <<"baz">>, 3])),
    ?assertEqual({error,
                  [#{
                     kind => data,
                     path => [],
                     schema => ItemsSchema,
                     value => [2, 3, 4, 5],
                     error => wrong_size
                    }]},
                 jsone_schema:validate(ItemsSchema, [2, 3, 4, 5])),

    TupleSchema =
        #{
          <<"type">> => <<"array">>,
          <<"items">> => [IntegerSchema, IntegerSchema, IntegerSchema],
          <<"additionalItems">> => false
         },
    ?assertEqual({error,
                  [#{kind => data, path => [2], schema => IntegerSchema, value => <<"baz">>, error => wrong_type}]},
                 jsone_schema:validate(TupleSchema, [2, 3, <<"baz">>])),
    ?assertEqual({error,
                  [#{
                     kind => data,
                     path => [],
                     schema => TupleSchema,
                     value => [2, 3, 4, 5],
                     error => no_extra_items_allowed
                    }]},
                 jsone_schema:validate(TupleSchema, [2, 3, 4, 5])),
    ok.


dependencies_test() ->
    Schema =
        #{
          <<"type">> => <<"object">>,
          <<"dependencies">> => #{<<"bar">> => [<<"foo">>]}
         },
    ?assertEqual({error,
                  [#{
                     kind => data,
                     path => [],
                     schema => Schema,
                     value => #{<<"bar">> => 42},
                     error => {missing_dependency, <<"foo">>}
                    }]},
                 jsone_schema:validate(Schema, #{<<"bar">> => 42})),
    ok.


dots_used_in_keys_test() ->
    Schema =
        #{
          <<"type">> => <<"object">>,
          <<"properties">> =>
              #{
                <<"3.4.5.6.7">> => #{<<"type">> => <<"string">>},
                <<"additionalProperties">> => false
               }
         },
    ?assertEqual({ok, #{<<"3.4.5.6.7">> => <<"Hello world!">>}},
                 jsone_schema:validate(Schema, #{<<"3.4.5.6.7">> => <<"Hello world!">>})),
    ?assertEqual({error,
                  [#{
                     kind => data,
                     path => [<<"3.4.5.6.7">>],
                     schema => #{<<"type">> => <<"string">>},
                     value => true,
                     error => wrong_type
                    }]},
                 jsone_schema:validate(Schema, #{<<"3.4.5.6.7">> => true})),
    ok.


empty_list_as_invalid_string_test() ->
    Schema =
        #{
          <<"type">> => <<"object">>,
          <<"properties">> => #{<<"foo">> => #{<<"type">> => <<"string">>}}
         },
    ?assertEqual({error,
                  [#{
                     kind => data,
                     path => [<<"foo">>],
                     schema => #{<<"type">> => <<"string">>},
                     value => [],
                     error => wrong_type
                    }]},
                 jsone_schema:validate(Schema, #{<<"foo">> => []})),
    ok.


schema_unsupported_test() ->
    SupportedSchema = #{<<"$schema">> => ?DRAFT6},
    UnsupportedSchema = #{<<"$schema">> => <<"http://json-schema.org/draft-05/schema#">>},
    Data = #{<<"Doesn't matter">> => true},
    ?assertEqual({ok, Data}, jsone_schema:validate(SupportedSchema, Data)),
    ?assertEqual({error,
                  [#{
                     kind => schema,
                     schema => UnsupportedSchema,
                     error =>
                         {schema_unsupported, <<"http://json-schema.org/draft-05/schema#">>}
                    }]},
                 jsone_schema:validate(UnsupportedSchema, Data)),
    ok.


one_of_error_test() ->
    IntegerSchema = #{<<"type">> => <<"integer">>},
    StringSchema = #{<<"type">> => <<"string">>},
    ObjectSchema =
        #{
          <<"type">> => <<"object">>,
          <<"properties">> => #{<<"name">> => StringSchema, <<"age">> => IntegerSchema},
          <<"additionalProperties">> => false
         },
    Schema = #{<<"$schema">> => ?DRAFT6, <<"oneOf">> => [IntegerSchema, StringSchema, ObjectSchema]},
    Data = #{<<"name">> => 42, <<"age">> => <<"John">>},
    %% サブスキーマのエラーも同じ map 形式で入れ子になる
    ?assertEqual({error,
                  [#{
                     kind => data,
                     path => [],
                     schema => Schema,
                     value => Data,
                     error =>
                         {not_one_schema_valid,
                          [#{kind => data, path => [], schema => IntegerSchema, value => Data, error => wrong_type},
                           #{kind => data, path => [], schema => StringSchema, value => Data, error => wrong_type},
                           #{
                             kind => data,
                             path => [<<"age">>],
                             schema => IntegerSchema,
                             value => <<"John">>,
                             error => wrong_type
                            }]}
                    }]},
                 jsone_schema:validate(Schema, Data)),
    ok.


any_of_error_test() ->
    IntegerSchema = #{<<"type">> => <<"integer">>},
    StringSchema = #{<<"type">> => <<"string">>},
    ObjectSchema =
        #{
          <<"type">> => <<"object">>,
          <<"properties">> => #{<<"name">> => StringSchema, <<"age">> => IntegerSchema},
          <<"additionalProperties">> => false
         },
    Schema = #{<<"$schema">> => ?DRAFT6, <<"anyOf">> => [IntegerSchema, StringSchema, ObjectSchema]},
    Data = #{<<"name">> => 42, <<"age">> => <<"John">>},
    ?assertEqual({error,
                  [#{
                     kind => data,
                     path => [],
                     schema => Schema,
                     value => Data,
                     error =>
                         {any_schemas_not_valid,
                          [#{kind => data, path => [], schema => IntegerSchema, value => Data, error => wrong_type}]}
                    }]},
                 jsone_schema:validate(Schema, Data)),
    ok.


exclusive_maximum_minimum_test() ->
    Schema = fun(Property) ->
                     #{<<"$schema">> => ?DRAFT6, <<"type">> => <<"number">>, Property => 43}
             end,
    ?assertEqual({ok, 42},
                 jsone_schema:validate(Schema(<<"exclusiveMaximum">>), 42)),
    ?assertEqual({ok, 44}, jsone_schema:validate(Schema(<<"exclusiveMinimum">>), 44)),
    ?assertEqual({error,
                  [#{
                     kind => data,
                     path => [],
                     schema => Schema(<<"exclusiveMinimum">>),
                     value => 42,
                     error => not_in_range
                    }]},
                 jsone_schema:validate(Schema(<<"exclusiveMinimum">>), 42)),
    ?assertEqual({error,
                  [#{
                     kind => data,
                     path => [],
                     schema => Schema(<<"exclusiveMaximum">>),
                     value => 44,
                     error => not_in_range
                    }]},
                 jsone_schema:validate(Schema(<<"exclusiveMaximum">>), 44)),
    ok.


id_keyword_test() ->
    %% draft 6 では id ではなく $id を使う
    ?assertEqual({error,
                  [#{
                     kind => schema,
                     schema => #{<<"id">> => <<"foo">>},
                     error => wrong_draft6_id_tag
                    }]},
                 jsone_schema:validate(#{<<"id">> => <<"foo">>}, #{<<"foo">> => <<"bar">>})),
    ?assertEqual({ok, #{<<"foo">> => <<"bar">>}},
                 jsone_schema:validate(#{<<"$id">> => <<"foo">>}, #{<<"foo">> => <<"bar">>})),
    ok.


contains_test() ->
    Schema =
        #{
          <<"$schema">> => ?DRAFT6,
          <<"type">> => <<"array">>,
          <<"contains">> => #{<<"type">> => <<"number">>}
         },
    ?assertEqual({ok, [<<"foo">>, 42]}, jsone_schema:validate(Schema, [<<"foo">>, 42])),
    ?assertEqual({error,
                  [#{
                     kind => data,
                     path => [],
                     schema => Schema,
                     value => [<<"foo">>, <<"bar">>],
                     error => data_invalid
                    }]},
                 jsone_schema:validate(Schema, [<<"foo">>, <<"bar">>])),
    ok.


const_test() ->
    Schema = #{<<"$schema">> => ?DRAFT6, <<"type">> => <<"string">>, <<"const">> => <<"foo">>},
    ?assertEqual({ok, <<"foo">>}, jsone_schema:validate(Schema, <<"foo">>)),
    ?assertEqual({error,
                  [#{kind => data, path => [], schema => Schema, value => <<"bar">>, error => not_in_enum}]},
                 jsone_schema:validate(Schema, <<"bar">>)),
    ok.


empty_required_test() ->
    Schema = #{<<"$schema">> => ?DRAFT6, <<"type">> => <<"object">>, <<"required">> => []},
    ?assertEqual({ok, #{}}, jsone_schema:validate(Schema, #{})),
    ok.


empty_dependencies_test() ->
    Schema = #{<<"$schema">> => ?DRAFT6, <<"type">> => <<"object">>, <<"dependencies">> => #{}},
    ?assertEqual({ok, #{}}, jsone_schema:validate(Schema, #{})),
    ok.


boolean_items_test() ->
    Schema = #{<<"$schema">> => ?DRAFT6, <<"type">> => <<"array">>, <<"items">> => true},
    ?assertEqual({ok, []}, jsone_schema:validate(Schema, [])),
    InvalidSchema = #{<<"$schema">> => ?DRAFT6, <<"type">> => <<"array">>, <<"items">> => false},
    ?assertEqual({ok, []}, jsone_schema:validate(InvalidSchema, [])),
    ?assertMatch({error, [#{kind := data, error := not_schema_valid, value := 1, path := [0]}]},
                 jsone_schema:validate(InvalidSchema, [1])),
    ok.


boolean_contains_test() ->
    Schema = #{<<"$schema">> => ?DRAFT6, <<"type">> => <<"array">>, <<"contains">> => true},
    ?assertEqual({ok, [<<"foo">>, 42]}, jsone_schema:validate(Schema, [<<"foo">>, 42])),
    InvalidSchema = #{<<"$schema">> => ?DRAFT6, <<"type">> => <<"array">>, <<"contains">> => false},
    ?assertMatch({error, [#{kind := data, error := data_invalid, value := [], path := []}]},
                 jsone_schema:validate(InvalidSchema, [])),
    ok.


boolean_schema_test() ->
    ?assertEqual({ok, #{}}, jsone_schema:validate(true, #{})),
    ?assertMatch({error, _}, jsone_schema:validate(false, #{})),
    ?assertEqual({ok, 1}, jsone_schema:validate(#{}, 1)),
    ok.


max_errors_test() ->
    Schema =
        #{
          <<"$schema">> => ?DRAFT6,
          <<"type">> => <<"object">>,
          <<"properties">> =>
              #{
                <<"foo">> => #{<<"type">> => <<"object">>},
                <<"baz">> => #{<<"type">> => <<"integer">>}
               }
         },
    Data = #{<<"foo">> => 42, <<"baz">> => #{}},
    %% デフォルトでは最初のエラーで打ち切る
    ?assertMatch({error, [_]}, jsone_schema:validate(Schema, Data)),
    %% infinity を指定すると全エラーを集める
    %% エラーの順番はスキーマのキー順 (baz, foo) に依存する
    ?assertEqual({error,
                  [#{
                     kind => data,
                     path => [<<"baz">>],
                     schema => #{<<"type">> => <<"integer">>},
                     value => #{},
                     error => wrong_type
                    },
                   #{
                     kind => data,
                     path => [<<"foo">>],
                     schema => #{<<"type">> => <<"object">>},
                     value => 42,
                     error => wrong_type
                    }]},
                 jsone_schema:validate(Schema, Data, #{max_errors => infinity})),
    ok.


unique_items_test() ->
    Schema = #{<<"uniqueItems">> => true},
    ?assertEqual({ok, [1, 2]}, jsone_schema:validate(Schema, [1, 2])),
    %% 1 と 1.0 は JSON としては同じ値
    ?assertMatch({error, [#{kind := data, error := {not_unique, 1}}]},
                 jsone_schema:validate(Schema, [1, 1.0])),
    %% オブジェクトはキー順に依存しない
    ?assertMatch({error, _},
                 jsone_schema:validate(Schema,
                                       [#{<<"a">> => 1, <<"b">> => 2},
                                        #{<<"b">> => 2, <<"a">> => 1}])),
    ok.


local_ref_test() ->
    Schema =
        #{
          <<"definitions">> => #{<<"a">> => #{<<"type">> => <<"object">>}},
          <<"type">> => <<"object">>,
          <<"properties">> => #{<<"prop">> => #{<<"$ref">> => <<"#/definitions/a">>}}
         },
    ?assertEqual({ok, #{<<"prop">> => #{}}}, jsone_schema:validate(Schema, #{<<"prop">> => #{}})),
    ?assertMatch({error, _}, jsone_schema:validate(Schema, #{<<"prop">> => 1})),
    ok.


schema_loader_test() ->
    RemoteSchema = #{<<"type">> => <<"integer">>},
    Loader =
        fun(<<"https://example.com/integer.json">>) ->
                {ok, RemoteSchema};
           (_URI) ->
                {error, not_found}
        end,
    Schema = #{<<"$ref">> => <<"https://example.com/integer.json">>},
    ?assertEqual({ok, 1}, jsone_schema:validate(Schema, 1, #{schema_loader => Loader})),
    ?assertMatch({error, _}, jsone_schema:validate(Schema, <<"x">>, #{schema_loader => Loader})),
    ok.


schemas_option_test() ->
    RemoteSchema = #{<<"type">> => <<"integer">>},
    Schema = #{<<"$ref">> => <<"https://example.com/integer.json#">>},
    Options = #{schemas => #{<<"https://example.com/integer.json">> => RemoteSchema}},
    ?assertEqual({ok, 1}, jsone_schema:validate(Schema, 1, Options)),
    ?assertMatch({error, _}, jsone_schema:validate(Schema, <<"x">>, Options)),
    ok.


relative_ref_test() ->
    %% ルートの $id を基準に相対参照を解決する
    Schema =
        #{
          <<"$id">> => <<"https://example.com/main.json">>,
          <<"properties">> => #{<<"value">> => #{<<"$ref">> => <<"sub.json#">>}}
         },
    Options = #{schemas => #{<<"https://example.com/sub.json">> => #{<<"type">> => <<"integer">>}}},
    ?assertEqual({ok, #{<<"value">> => 1}}, jsone_schema:validate(Schema, #{<<"value">> => 1}, Options)),
    ?assertMatch({error, _},
                 jsone_schema:validate(Schema, #{<<"value">> => <<"x">>}, Options)),
    ok.


format_test() ->
    ?assertEqual({ok, <<"2026-09-15T00:00:00Z">>},
                 jsone_schema:validate(#{<<"format">> => <<"date-time">>},
                                       <<"2026-09-15T00:00:00Z">>)),
    ?assertMatch({error, _},
                 jsone_schema:validate(#{<<"format">> => <<"date-time">>}, <<"not a date">>)),
    ?assertEqual({ok, <<"foo@example.com">>},
                 jsone_schema:validate(#{<<"format">> => <<"email">>}, <<"foo@example.com">>)),
    ?assertMatch({error, _},
                 jsone_schema:validate(#{<<"format">> => <<"email">>}, <<"not an email">>)),
    ?assertEqual({ok, <<"192.0.2.1">>},
                 jsone_schema:validate(#{<<"format">> => <<"ipv4">>}, <<"192.0.2.1">>)),
    ?assertMatch({error, _},
                 jsone_schema:validate(#{<<"format">> => <<"ipv4">>}, <<"999.0.2.1">>)),
    %% 未対応の format は常に有効とする
    ?assertEqual({ok, <<"anything">>},
                 jsone_schema:validate(#{<<"format">> => <<"unknown">>}, <<"anything">>)),
    ok.


error_to_json_test() ->
    {error, Reasons} = jsone_schema:validate(#{<<"type">> => <<"integer">>}, <<"x">>),
    Json = jsone_schema_error:to_json(Reasons),
    ?assertMatch(#{<<"errors">> := [#{<<"kind">> := <<"data">>, <<"error">> := <<"wrong_type">>}]},
                 jsone:decode(Json)),
    ok.


json_pointer_test() ->
    Document =
        #{
          <<"definitions">> => #{<<"a">> => #{<<"type">> => <<"integer">>}},
          <<"tilde~field">> => 1,
          <<"slash/field">> => 2,
          <<"percent%field">> => 3,
          <<"items">> => [#{<<"x">> => 1}, #{<<"x">> => 2}]
         },
    ?assertEqual({ok, #{<<"type">> => <<"integer">>}},
                 jsone_schema_json_pointer:eval(<<"/definitions/a">>, Document)),
    ?assertEqual({ok, Document}, jsone_schema_json_pointer:eval(~"", Document)),
    ?assertEqual({ok, 1}, jsone_schema_json_pointer:eval(<<"/tilde~0field">>, Document)),
    ?assertEqual({ok, 2}, jsone_schema_json_pointer:eval(<<"/slash~1field">>, Document)),
    ?assertEqual({ok, 3}, jsone_schema_json_pointer:eval(<<"/percent%25field">>, Document)),
    ?assertEqual({ok, #{<<"x">> => 2}}, jsone_schema_json_pointer:eval(<<"/items/1">>, Document)),
    ?assertEqual({error, not_found}, jsone_schema_json_pointer:eval(<<"/items/01">>, Document)),
    ?assertEqual({error, not_found}, jsone_schema_json_pointer:eval(<<"/items/2">>, Document)),
    ok.


uri_test() ->
    ?assertEqual(<<"https://example.com/schemas/b.json">>,
                 jsone_schema_uri:resolve(<<"https://example.com/schemas/main.json">>,
                                          <<"b.json">>)),
    ?assertEqual(<<"https://example.com/schemas/main.json#/definitions/a">>,
                 jsone_schema_uri:resolve(<<"https://example.com/schemas/main.json">>,
                                          <<"#/definitions/a">>)),
    ?assertEqual(<<"https://other.example.com/x.json">>,
                 jsone_schema_uri:resolve(<<"https://example.com/schemas/main.json">>,
                                          <<"https://other.example.com/x.json">>)),
    %% 基準 URI が無い場合はそのまま返す
    ?assertEqual(<<"#/definitions/a">>, jsone_schema_uri:resolve(undefined, <<"#/definitions/a">>)),
    %% 不透明なキーの場合はフラグメントを連結する
    ?assertEqual(<<"user#/definitions/a">>,
                 jsone_schema_uri:resolve(<<"user">>, <<"#/definitions/a">>)),
    ?assertEqual(<<"other.json">>, jsone_schema_uri:resolve(<<"user">>, <<"other.json">>)),
    ?assertEqual({<<"https://example.com/main.json">>, <<"/definitions/a">>},
                 jsone_schema_uri:split_fragment(<<"https://example.com/main.json#/definitions/a">>)),
    ok.


store_test() ->
    ok = jsone_schema:clear_schemas(),
    Key = <<"jsone_schema_tests_store">>,
    Schema = #{<<"type">> => <<"integer">>},
    ?assertEqual(ok, jsone_schema:add_schema(Key, Schema)),
    ?assertEqual({ok, Schema}, jsone_schema:get_schema(Key)),
    ?assertEqual({ok, 1}, jsone_schema:validate_key(Key, 1)),
    ?assertMatch({error, _}, jsone_schema:validate_key(Key, <<"x">>)),
    ?assertEqual(#{Key => Schema}, jsone_schema:list_schemas()),
    ?assertEqual(ok, jsone_schema:del_schema(Key)),
    ?assertEqual({error, not_found}, jsone_schema:get_schema(Key)),
    ?assertEqual({error, {schema_not_found, Key}}, jsone_schema:validate_key(Key, 1)),
    ok.


store_id_test() ->
    ok = jsone_schema:clear_schemas(),
    Key = <<"store_id_test">>,
    Schema = #{<<"$id">> => <<"https://example.com/store_id_test.json">>, <<"type">> => <<"integer">>},
    ok = jsone_schema:add_schema(Key, Schema),
    %% $id でも引ける
    ?assertEqual({ok, Schema}, jsone_schema:get_schema(<<"https://example.com/store_id_test.json">>)),
    ?assertEqual(ok, jsone_schema:del_schema(Key)),
    ?assertEqual({error, not_found}, jsone_schema:get_schema(<<"https://example.com/store_id_test.json">>)),
    ok.


store_parse_test() ->
    ok = jsone_schema:clear_schemas(),
    Key = <<"store_parse_test">>,
    ?assertEqual(ok, jsone_schema:add_schema(Key, ~'{"type":"integer"}', #{})),
    ?assertEqual({ok, 1}, jsone_schema:validate_key(Key, 1)),
    ?assertMatch({error, {parse_error, _}},
                 jsone_schema:add_schema(<<"invalid">>, ~'{"type":', #{})),
    ok.


load_schemas_test() ->
    ok = jsone_schema:clear_schemas(),
    Dir = filename:join(tmp_dir(), "jsone_schema_tests_load_schemas"),
    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
    File = filename:join(Dir, "integer.json"),
    ok = file:write_file(File, ~'{"type":"integer"}'),
    try
        ?assertEqual(ok, jsone_schema:load_schemas(Dir)),
        Key = <<"file://", (jsone_schema_uri:to_binary(filename:absname(File)))/binary>>,
        ?assertEqual({ok, 1}, jsone_schema:validate_key(Key, 1)),
        ?assertMatch({error, _}, jsone_schema:validate_key(Key, <<"x">>))
    after
        ok = file:del_dir_r(Dir),
        ok = jsone_schema:clear_schemas()
    end,
    ok.


ref_override_siblings_test() ->
    %% draft 6 では $ref があると兄弟キーワードは無視される
    Schema =
        #{
          <<"definitions">> => #{<<"reffed">> => #{<<"type">> => <<"array">>}},
          <<"properties">> =>
              #{<<"foo">> => #{<<"$ref">> => <<"#/definitions/reffed">>, <<"maxItems">> => 2}}
         },
    ?assertEqual({ok, #{<<"foo">> => [1, 2, 3]}}, jsone_schema:validate(Schema, #{<<"foo">> => [1, 2, 3]})),
    ?assertMatch({error, _}, jsone_schema:validate(Schema, #{<<"foo">> => <<"string">>})),
    ok.


anchor_ref_test() ->
    %% プレーンネームフラグメント (#foo) で参照する
    Schema =
        #{
          <<"definitions">> => #{<<"A">> => #{<<"$id">> => <<"#foo">>, <<"type">> => <<"integer">>}},
          <<"allOf">> => [#{<<"$ref">> => <<"#foo">>}]
         },
    ?assertEqual({ok, 1}, jsone_schema:validate(Schema, 1)),
    ?assertMatch({error, _}, jsone_schema:validate(Schema, <<"a">>)),
    ok.


resource_ref_test() ->
    %% $id で定義したリソース URI と再帰参照
    Schema =
        #{
          <<"$id">> => <<"https://example.com/tree">>,
          <<"type">> => <<"object">>,
          <<"properties">> =>
              #{
                <<"nodes">> =>
                    #{
                      <<"type">> => <<"array">>,
                      <<"items">> => #{<<"$ref">> => <<"node">>}
                     }
               },
          <<"definitions">> =>
              #{
                <<"node">> =>
                    #{
                      <<"$id">> => <<"https://example.com/node">>,
                      <<"type">> => <<"object">>,
                      <<"properties">> => #{<<"subtree">> => #{<<"$ref">> => <<"tree">>}}
                     }
               }
         },
    Data = #{<<"nodes">> => [#{<<"subtree">> => #{<<"nodes">> => [#{}]}}]},
    ?assertEqual({ok, Data}, jsone_schema:validate(Schema, Data)),
    ?assertMatch({error, _},
                 jsone_schema:validate(Schema, #{<<"nodes">> => [#{<<"subtree">> => 1}]})),
    ok.


anchor_with_base_uri_change_test() ->
    %% サブスキーマの $id で基準 URI が変わる場合のアンカー参照
    Schema =
        #{
          <<"$id">> => <<"https://example.com/root">>,
          <<"allOf">> => [#{<<"$ref">> => <<"https://example.com/nested.json#foo">>}],
          <<"definitions">> =>
              #{
                <<"A">> =>
                    #{
                      <<"$id">> => <<"nested.json">>,
                      <<"definitions">> =>
                          #{
                            <<"B">> =>
                                #{<<"$id">> => <<"#foo">>, <<"type">> => <<"integer">>}
                           }
                     }
               }
         },
    ?assertEqual({ok, 1}, jsone_schema:validate(Schema, 1)),
    ?assertMatch({error, _}, jsone_schema:validate(Schema, <<"a">>)),
    ok.


ignored_id_test() ->
    %% 未知のキーワードと enum の中の $id は識別子にならない
    Schema =
        #{
          <<"$defs">> =>
              #{
                <<"real">> =>
                    #{<<"$id">> => <<"https://example.com/real-id">>, <<"type">> => <<"integer">>},
                <<"unknown">> =>
                    #{
                      <<"unknownKeyword">> =>
                          #{<<"$id">> => <<"https://example.com/unknown-id">>}
                     },
                <<"in_enum">> =>
                    #{<<"enum">> => [#{<<"$id">> => <<"https://example.com/enum-id">>}]}
               },
          <<"$ref">> => <<"https://example.com/real-id">>
         },
    ?assertEqual({ok, 1}, jsone_schema:validate(Schema, 1)),
    ?assertMatch({error, _},
                 jsone_schema:validate(Schema#{<<"$ref">> := <<"https://example.com/unknown-id">>},
                                       1)),
    ?assertMatch({error, _},
                 jsone_schema:validate(Schema#{<<"$ref">> := <<"https://example.com/enum-id">>}, 1)),
    ok.


invalid_pattern_test() ->
    %% 不正な正規表現の pattern はクラッシュせずスキーマのエラーになる
    Schema = #{<<"type">> => <<"string">>, <<"pattern">> => <<"[">>},
    ?assertMatch({error, [#{kind := schema}]}, jsone_schema:validate(Schema, <<"a">>)),
    ok.


invalid_pattern_properties_test() ->
    %% 不正な正規表現の patternProperties もクラッシュせずスキーマのエラーになる
    Schema = #{<<"patternProperties">> => #{<<"[">> => #{<<"type">> => <<"string">>}}},
    ?assertMatch({error, [#{kind := schema}]}, jsone_schema:validate(Schema, #{<<"a">> => 1})),
    ok.


%% Internal Functions


tmp_dir() ->
    case os:getenv("TMPDIR") of
        false ->
            "/tmp";
        Dir ->
            Dir
    end.
