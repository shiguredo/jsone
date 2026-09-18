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

    %% 余分なプロパティが複数ある場合は、値のキーの順に 1 件ずつ報告する
    %% (キーが 32 個以下の map はキー順に走査されるため順序が定まる)
    AnySchema = #{<<"type">> => <<"object">>, <<"additionalProperties">> => false},
    AnyValue = #{<<"a">> => 1, <<"b">> => 2, <<"c">> => 3},
    ?assertEqual({error,
                  [#{
                     kind => data,
                     path => [<<"a">>],
                     schema => AnySchema,
                     value => AnyValue,
                     error => no_extra_properties_allowed
                    }]},
                 jsone_schema:validate(AnySchema, AnyValue)),
    ?assertEqual({error,
                  [#{
                     kind => data,
                     path => [<<"a">>],
                     schema => AnySchema,
                     value => AnyValue,
                     error => no_extra_properties_allowed
                    },
                   #{
                     kind => data,
                     path => [<<"b">>],
                     schema => AnySchema,
                     value => AnyValue,
                     error => no_extra_properties_allowed
                    },
                   #{
                     kind => data,
                     path => [<<"c">>],
                     schema => AnySchema,
                     value => AnyValue,
                     error => no_extra_properties_allowed
                    }]},
                 jsone_schema:validate(AnySchema, AnyValue, #{max_errors => infinity})),

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
                     path => [3],
                     schema => TupleSchema,
                     value => [2, 3, 4, 5],
                     error => no_extra_items_allowed
                    }]},
                 jsone_schema:validate(TupleSchema, [2, 3, 4, 5])),
    %% 余分な要素 1 件につき 1 件報告し、それぞれのインデックスを積む
    ?assertEqual({error,
                  [#{
                     kind => data,
                     path => [3],
                     schema => TupleSchema,
                     value => [2, 3, 4, 5, 6],
                     error => no_extra_items_allowed
                    },
                   #{
                     kind => data,
                     path => [4],
                     schema => TupleSchema,
                     value => [2, 3, 4, 5, 6],
                     error => no_extra_items_allowed
                    }]},
                 jsone_schema:validate(TupleSchema, [2, 3, 4, 5, 6], #{max_errors => infinity})),
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
                     path => [<<"bar">>],
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

    %% $ref を併記していても未対応の $schema は方言ゲートとして弾く
    RefWithUnsupported =
        #{
          <<"$schema">> => <<"http://json-schema.org/draft-05/schema#">>,
          <<"definitions">> => #{<<"a">> => #{<<"type">> => <<"integer">>}},
          <<"$ref">> => <<"#/definitions/a">>
         },
    ?assertMatch({error, [#{kind := schema, error := {schema_unsupported, _}}]},
                 jsone_schema:validate(RefWithUnsupported, 1)),
    %% 参照先に反するデータでも、方言ゲートのエラーが参照先の検証より先に出る
    ?assertMatch({error, [#{kind := schema, error := {schema_unsupported, _}}]},
                 jsone_schema:validate(RefWithUnsupported, <<"x">>)),
    %% $ref と id を併記していても方言ゲートが先に評価される
    ?assertMatch({error, [#{kind := schema, error := {schema_unsupported, _}}]},
                 jsone_schema:validate(RefWithUnsupported#{<<"id">> => <<"legacy">>}, 1)),
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

    %% $ref と併記した id は無視し、参照先の検証結果を返す
    RefWithId =
        #{
          <<"definitions">> => #{<<"a">> => #{<<"type">> => <<"integer">>}},
          <<"$ref">> => <<"#/definitions/a">>,
          <<"id">> => <<"legacy">>
         },
    ?assertEqual({ok, 1}, jsone_schema:validate(RefWithId, 1)),
    ?assertMatch({error, [#{kind := data, error := wrong_type}]},
                 jsone_schema:validate(RefWithId, <<"x">>)),

    %% $ref と $schema と id を併記した場合も id は無視される
    RefWithSchemaAndId = RefWithId#{<<"$schema">> => ?DRAFT6},
    ?assertEqual({ok, 1}, jsone_schema:validate(RefWithSchemaAndId, 1)),
    ?assertMatch({error, [#{kind := data, error := wrong_type}]},
                 jsone_schema:validate(RefWithSchemaAndId, <<"x">>)),

    %% $ref が非文字列の場合は id の検出を優先する
    RefNumberAndId = #{<<"$ref">> => 5, <<"id">> => <<"legacy">>},
    ?assertEqual({error,
                  [#{
                     kind => schema,
                     schema => RefNumberAndId,
                     error => wrong_draft6_id_tag
                    }]},
                 jsone_schema:validate(RefNumberAndId, 1)),
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
                     error => no_contains_match
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
    ?assertMatch({error,
                  [#{
                     kind := data,
                     error := no_contains_match,
                     value := [],
                     path := [],
                     schema := InvalidSchema
                    }]},
                 jsone_schema:validate(InvalidSchema, [])),
    ok.


boolean_schema_test() ->
    ?assertEqual({ok, #{}}, jsone_schema:validate(true, #{})),
    %% false スキーマのエラーには、利用者が書いた false をそのまま載せる
    ?assertEqual({error,
                  [#{
                     kind => data,
                     path => [],
                     schema => false,
                     value => #{},
                     error => not_schema_valid
                    }]},
                 jsone_schema:validate(false, #{})),
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

    %% email は dot-atom の規則に従い、先頭・末尾・連続するドットを認めない
    ?assertMatch({error, [#{kind := data, error := wrong_format}]},
                 jsone_schema:validate(#{<<"format">> => <<"email">>}, <<".test@example.com">>)),
    ?assertMatch({error, [#{kind := data, error := wrong_format}]},
                 jsone_schema:validate(#{<<"format">> => <<"email">>}, <<"test.@example.com">>)),
    ?assertMatch({error, [#{kind := data, error := wrong_format}]},
                 jsone_schema:validate(#{<<"format">> => <<"email">>}, <<"te..st@example.com">>)),
    ?assertEqual({ok, <<"te.s.t@example.com">>},
                 jsone_schema:validate(#{<<"format">> => <<"email">>}, <<"te.s.t@example.com">>)),

    %% date-time の区切り文字は T / t に限る
    ?assertMatch({error, [#{kind := data, error := wrong_format}]},
                 jsone_schema:validate(#{<<"format">> => <<"date-time">>},
                                       <<"2018-02-01X15:18:02Z">>)),
    ?assertEqual({ok, <<"1963-06-19t08:30:06.283185z">>},
                 jsone_schema:validate(#{<<"format">> => <<"date-time">>},
                                       <<"1963-06-19t08:30:06.283185z">>)),

    %% ipv6 に zone ID は含めない
    ?assertMatch({error, [#{kind := data, error := wrong_format}]},
                 jsone_schema:validate(#{<<"format">> => <<"ipv6">>}, <<"fe80::a%eth1">>)),
    ?assertEqual({ok, <<"::1">>},
                 jsone_schema:validate(#{<<"format">> => <<"ipv6">>}, <<"::1">>)),

    %% validate_format => false では対応済み format を検証しない
    ?assertEqual({ok, <<"a..b@example.com">>},
                 jsone_schema:validate(#{<<"format">> => <<"email">>},
                                       <<"a..b@example.com">>,
                                       #{validate_format => false})),
    ?assertEqual({ok, <<"2018-02-01X15:18:02Z">>},
                 jsone_schema:validate(#{<<"format">> => <<"date-time">>},
                                       <<"2018-02-01X15:18:02Z">>,
                                       #{validate_format => false})),
    ?assertEqual({ok, <<"fe80::a%eth1">>},
                 jsone_schema:validate(#{<<"format">> => <<"ipv6">>},
                                       <<"fe80::a%eth1">>,
                                       #{validate_format => false})),
    ?assertEqual({ok, <<"anything">>},
                 jsone_schema:validate(#{<<"format">> => <<"unknown">>},
                                       <<"anything">>,
                                       #{validate_format => false})),

    %% uri-reference も検証する
    ?assertEqual({ok, <<"/abc">>},
                 jsone_schema:validate(#{<<"format">> => <<"uri-reference">>}, <<"/abc">>)),
    ?assertMatch({error, [#{kind := data, error := wrong_format}]},
                 jsone_schema:validate(#{<<"format">> => <<"uri-reference">>}, <<"#frag\\ment">>)),
    ?assertEqual({ok, <<"#frag\\ment">>},
                 jsone_schema:validate(#{<<"format">> => <<"uri-reference">>},
                                       <<"#frag\\ment">>,
                                       #{validate_format => false})),

    %% validate_key/3 も validate_format を受け付ける
    Key = <<"jsone_schema_tests_format">>,
    try
        ?assertEqual(ok, jsone_schema:add_schema(Key, #{<<"format">> => <<"email">>})),
        ?assertMatch({error, [#{kind := data, error := wrong_format}]},
                     jsone_schema:validate_key(Key, <<"a..b@example.com">>)),
        ?assertEqual({ok, <<"a..b@example.com">>},
                     jsone_schema:validate_key(Key,
                                               <<"a..b@example.com">>,
                                               #{validate_format => false}))
    after
        jsone_schema:del_schema(Key)
    end,
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


non_string_ref_test() ->
    %% $ref が文字列でない場合は URI 参照にならないため schema エラーになる
    ?assertEqual({error,
                  [#{
                     kind => schema,
                     schema => #{<<"$ref">> => 5},
                     error => schema_invalid
                    }]},
                 jsone_schema:validate(#{<<"$ref">> => 5}, 1)),
    ?assertMatch({error, [#{kind := schema, error := schema_invalid}]},
                 jsone_schema:validate(#{<<"$ref">> => 5.0}, 1)),
    ?assertMatch({error, [#{kind := schema, error := schema_invalid}]},
                 jsone_schema:validate(#{<<"$ref">> => null}, 1)),
    ?assertMatch({error, [#{kind := schema, error := schema_invalid}]},
                 jsone_schema:validate(#{<<"$ref">> => true}, 1)),
    ?assertMatch({error, [#{kind := schema, error := schema_invalid}]},
                 jsone_schema:validate(#{<<"$ref">> => false}, 1)),
    ?assertMatch({error, [#{kind := schema, error := schema_invalid}]},
                 jsone_schema:validate(#{<<"$ref">> => [<<"#">>]}, 1)),
    ?assertMatch({error, [#{kind := schema, error := schema_invalid}]},
                 jsone_schema:validate(#{<<"$ref">> => #{<<"$ref">> => <<"#">>}}, 1)),
    %% $id を併記していても $ref の型不正は schema エラーになる
    ?assertMatch({error, [#{kind := schema, error := schema_invalid}]},
                 jsone_schema:validate(#{<<"$ref">> => 5, <<"$id">> => <<"foo">>}, 1)),

    %% 空文字列の $ref は文字列なので、参照先の解決に進む
    ?assertMatch({error, [#{kind := schema, error := ref_cycle}]},
                 jsone_schema:validate(#{<<"$ref">> => <<"">>}, 1)),

    %% 兄弟キーワードは評価しない
    RefWithType = #{<<"$ref">> => 5, <<"type">> => <<"string">>},
    ?assertMatch({error, [#{kind := schema, error := schema_invalid}]},
                 jsone_schema:validate(RefWithType, 42)),
    ?assertMatch({error, [#{kind := schema, error := schema_invalid}]},
                 jsone_schema:validate(RefWithType, <<"a">>)),
    ?assertMatch({error, [#{kind := schema, error := schema_invalid}]},
                 jsone_schema:validate(#{<<"$ref">> => 5, <<"required">> => [<<"a">>]}, #{})),
    ?assertMatch({error, [#{kind := schema, error := schema_invalid}]},
                 jsone_schema:validate(#{<<"$ref">> => 5, <<"enum">> => [1]}, 2)),
    ?assertMatch({error, [#{kind := schema, error := schema_invalid}]},
                 jsone_schema:validate(#{<<"$ref">> => 5, <<"maxItems">> => 1}, [1, 2, 3])),

    %% 文字列の $ref は現行どおり参照先を検証する
    RefSchema =
        #{
          <<"definitions">> => #{<<"a">> => #{<<"type">> => <<"integer">>}},
          <<"$ref">> => <<"#/definitions/a">>
         },
    ?assertEqual({ok, 1}, jsone_schema:validate(RefSchema, 1)),
    ?assertMatch({error, [#{kind := data, error := wrong_type}]},
                 jsone_schema:validate(RefSchema, <<"x">>)),
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


anchor_cross_ref_test() ->
    %% アンカー配下から別のアンカーを参照する (ルートに絶対 $id が無い)
    AnchorToAnchor =
        #{
          <<"definitions">> =>
              #{
                <<"A">> =>
                    #{
                      <<"$id">> => <<"#a">>,
                      <<"properties">> => #{<<"x">> => #{<<"$ref">> => <<"#b">>}}
                     },
                <<"B">> => #{<<"$id">> => <<"#b">>, <<"type">> => <<"integer">>}
               },
          <<"allOf">> => [#{<<"$ref">> => <<"#a">>}]
         },
    ?assertEqual({ok, #{<<"x">> => 1}}, jsone_schema:validate(AnchorToAnchor, #{<<"x">> => 1})),
    ?assertMatch({error, [#{kind := data, error := {all_schemas_not_valid, _}}]},
                 jsone_schema:validate(AnchorToAnchor, #{<<"x">> => <<"a">>})),

    %% アンカー配下からドキュメントルートのポインタを参照する
    AnchorToRootWithId =
        #{
          <<"$id">> => <<"http://example.com/root.json">>,
          <<"definitions">> =>
              #{
                <<"A">> =>
                    #{
                      <<"$id">> => <<"#a">>,
                      <<"properties">> => #{<<"x">> => #{<<"$ref">> => <<"#/definitions/B">>}}
                     },
                <<"B">> => #{<<"type">> => <<"integer">>}
               },
          <<"allOf">> => [#{<<"$ref">> => <<"#a">>}]
         },
    ?assertEqual({ok, #{<<"x">> => 1}},
                 jsone_schema:validate(AnchorToRootWithId, #{<<"x">> => 1})),
    ?assertMatch({error, [#{kind := data, error := {all_schemas_not_valid, _}}]},
                 jsone_schema:validate(AnchorToRootWithId, #{<<"x">> => <<"a">>})),

    AnchorToRoot =
        #{
          <<"definitions">> =>
              #{
                <<"A">> =>
                    #{
                      <<"$id">> => <<"#a">>,
                      <<"properties">> => #{<<"x">> => #{<<"$ref">> => <<"#/definitions/B">>}}
                     },
                <<"B">> => #{<<"type">> => <<"integer">>}
               },
          <<"allOf">> => [#{<<"$ref">> => <<"#a">>}]
         },
    ?assertEqual({ok, #{<<"x">> => 1}}, jsone_schema:validate(AnchorToRoot, #{<<"x">> => 1})),
    ?assertMatch({error, [#{kind := data, error := {all_schemas_not_valid, _}}]},
                 jsone_schema:validate(AnchorToRoot, #{<<"x">> => <<"a">>})),

    %% 埋め込みリソースの中で自分のポインタを参照する
    EmbeddedResource =
        #{
          <<"$id">> => <<"https://example.com/bundle.json">>,
          <<"definitions">> =>
              #{
                <<"user">> =>
                    #{
                      <<"$id">> => <<"https://example.com/user.json">>,
                      <<"definitions">> =>
                          #{<<"name">> => #{<<"type">> => <<"string">>}},
                      <<"properties">> =>
                          #{<<"name">> => #{<<"$ref">> => <<"#/definitions/name">>}}
                     }
               },
          <<"properties">> => #{<<"user">> => #{<<"$ref">> => <<"#/definitions/user">>}}
         },
    ?assertEqual({ok, #{<<"user">> => #{<<"name">> => <<"x">>}}},
                 jsone_schema:validate(EmbeddedResource, #{<<"user">> => #{<<"name">> => <<"x">>}})),
    ?assertMatch({error, [#{kind := data, error := wrong_type}]},
                 jsone_schema:validate(EmbeddedResource, #{<<"user">> => #{<<"name">> => 1}})),

    %% 相対 $id のルートから自分のポインタを参照する
    RelativeRootId =
        #{
          <<"$id">> => <<"foo.json">>,
          <<"definitions">> => #{<<"a">> => #{<<"type">> => <<"integer">>}},
          <<"properties">> => #{<<"p">> => #{<<"$ref">> => <<"#/definitions/a">>}}
         },
    ?assertEqual({ok, #{<<"p">> => 1}}, jsone_schema:validate(RelativeRootId, #{<<"p">> => 1})),
    ?assertMatch({error, [#{kind := data, error := wrong_type}]},
                 jsone_schema:validate(RelativeRootId, #{<<"p">> => <<"x">>})),

    %% 索引に無い鍵で外部ドキュメントを読もうとすると schema_not_found になる
    MissingRef = #{<<"$ref">> => <<"https://example.com/missing.json">>},
    ?assertMatch({error,
                  [#{
                     kind := schema,
                     error := {schema_not_found, <<"https://example.com/missing.json">>}
                    }]},
                 jsone_schema:validate(MissingRef, 1)),

    %% 未登録のアンカーへの参照は、壊れた URI ではなく参照そのものを報告する
    MissingAnchor =
        #{
          <<"definitions">> =>
              #{
                <<"A">> =>
                    #{
                      <<"$id">> => <<"#a">>,
                      <<"properties">> => #{<<"x">> => #{<<"$ref">> => <<"#c">>}}
                     }
               },
          <<"allOf">> => [#{<<"$ref">> => <<"#a">>}]
         },
    ?assertMatch({error,
                  [#{
                     kind := data,
                     error :=
                         {all_schemas_not_valid,
                          [#{kind := schema, error := {schema_not_found, <<"#c">>}}]}
                    }]},
                 jsone_schema:validate(MissingAnchor, #{<<"x">> => 1})),
    ok.


embedded_resource_state_test() ->
    %% 埋め込みリソースを跨いだあとも schemas オプションを引き継ぐ
    Schema =
        #{
          <<"$id">> => <<"https://example.com/bundle.json">>,
          <<"definitions">> =>
              #{
                <<"user">> =>
                    #{
                      <<"$id">> => <<"https://example.com/user.json">>,
                      <<"properties">> =>
                          #{<<"age">> => #{<<"$ref">> => <<"https://example.com/ext.json">>}}
                     }
               },
          <<"properties">> => #{<<"user">> => #{<<"$ref">> => <<"#/definitions/user">>}}
         },
    Options = #{schemas => #{<<"https://example.com/ext.json">> => #{<<"type">> => <<"integer">>}}},
    ?assertEqual({ok, #{<<"user">> => #{<<"age">> => 1}}},
                 jsone_schema:validate(Schema, #{<<"user">> => #{<<"age">> => 1}}, Options)),
    ?assertMatch({error, [#{kind := data, error := wrong_type}]},
                 jsone_schema:validate(Schema, #{<<"user">> => #{<<"age">> => <<"x">>}}, Options)),
    ok.


schema_index_entry_test() ->
    %% ルートに絶対 $id がある場合はドキュメント URI が定まる
    Root =
        #{
          <<"$id">> => <<"http://example.com/root.json">>,
          <<"definitions">> =>
              #{<<"A">> => #{<<"$id">> => <<"#a">>, <<"type">> => <<"integer">>}}
         },
    Index = jsone_schema_index:build(Root, <<"http://example.com/root.json">>),
    {ok, AnchorEntry} = jsone_schema_index:lookup(Index, <<"http://example.com/root.json#a">>),
    ?assertEqual(Root, jsone_schema_index:entry_root_schema(AnchorEntry)),
    ?assertEqual(<<"http://example.com/root.json">>,
                 jsone_schema_index:entry_document_uri(AnchorEntry)),
    %% 基準 URI は参照先の検証を始めるときに $id を反映する前の値になる
    ?assertEqual(<<"http://example.com/root.json">>,
                 jsone_schema_index:entry_base_uri(AnchorEntry)),
    ?assertEqual(#{<<"$id">> => <<"#a">>, <<"type">> => <<"integer">>},
                 jsone_schema_index:entry_schema(AnchorEntry)),

    %% 埋め込みリソースのエントリもルートスキーマとドキュメント URI を指す
    Embedded =
        #{
          <<"$id">> => <<"https://example.com/bundle.json">>,
          <<"definitions">> =>
              #{
                <<"user">> =>
                    #{
                      <<"$id">> => <<"https://example.com/user.json">>,
                      <<"type">> => <<"object">>
                     }
               }
         },
    EmbeddedIndex = jsone_schema_index:build(Embedded, <<"https://example.com/bundle.json">>),
    {ok, EmbeddedEntry} =
        jsone_schema_index:lookup(EmbeddedIndex, <<"https://example.com/user.json">>),
    ?assertEqual(Embedded, jsone_schema_index:entry_root_schema(EmbeddedEntry)),
    ?assertEqual(<<"https://example.com/bundle.json">>,
                 jsone_schema_index:entry_document_uri(EmbeddedEntry)),
    ?assertEqual(<<"https://example.com/bundle.json">>,
                 jsone_schema_index:entry_base_uri(EmbeddedEntry)),
    ?assertEqual(#{<<"$id">> => <<"https://example.com/user.json">>, <<"type">> => <<"object">>},
                 jsone_schema_index:entry_schema(EmbeddedEntry)),

    %% ルートに絶対 $id が無い場合はドキュメント URI と基準 URI が undefined になる
    Anonymous =
        #{
          <<"definitions">> =>
              #{<<"A">> => #{<<"$id">> => <<"#a">>, <<"type">> => <<"integer">>}}
         },
    AnonymousIndex = jsone_schema_index:build(Anonymous, undefined),
    {ok, AnonymousEntry} = jsone_schema_index:lookup(AnonymousIndex, <<"#a">>),
    ?assertEqual(Anonymous, jsone_schema_index:entry_root_schema(AnonymousEntry)),
    ?assertEqual(undefined, jsone_schema_index:entry_document_uri(AnonymousEntry)),
    ?assertEqual(undefined, jsone_schema_index:entry_base_uri(AnonymousEntry)),
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

    %% additionalProperties: false を併用しても、既定の max_errors では
    %% 正規表現のエラーが 1 件だけ返る (additionalProperties の評価順に依存しない)
    PatternWithAdditional =
        #{
          <<"patternProperties">> => #{<<"[">> => #{<<"type">> => <<"string">>}},
          <<"additionalProperties">> => false
         },
    ?assertEqual({error,
                  [#{
                     kind => schema,
                     schema => PatternWithAdditional,
                     error => schema_invalid
                    }]},
                 jsone_schema:validate(PatternWithAdditional, #{<<"a">> => 1})),

    %% max_errors を infinity にしても、正規表現が不正なプロパティを
    %% 余分なプロパティとして報告しない
    {error, PatternErrors} =
        jsone_schema:validate(PatternWithAdditional, #{<<"a">> => 1}, #{max_errors => infinity}),
    ?assertEqual([schema], lists:usort([ maps:get(kind, Error) || Error <- PatternErrors ])),

    %% 評価対象のプロパティが 1 つも無い場合は、正規表現を評価しない
    ?assertEqual({ok, #{}},
                 jsone_schema:validate(PatternWithAdditional, #{})),
    %% インスタンスがオブジェクトでない場合も正規表現を評価しない
    ?assertEqual({ok, 1},
                 jsone_schema:validate(PatternWithAdditional, 1)),
    ok.


multiple_of_float_test() ->
    %% 10 進で割り切れる小数は倍数として扱う
    ?assertEqual({ok, 0.07}, jsone_schema:validate(#{<<"multipleOf">> => 0.01}, 0.07)),
    ?assertEqual({ok, 4.35}, jsone_schema:validate(#{<<"multipleOf">> => 0.01}, 4.35)),
    ?assertEqual({ok, 0.3}, jsone_schema:validate(#{<<"multipleOf">> => 0.1}, 0.3)),
    ?assertEqual({ok, 1.15}, jsone_schema:validate(#{<<"multipleOf">> => 0.05}, 1.15)),
    ?assertEqual({ok, 4.35}, jsone_schema:validate(#{<<"multipleOf">> => 0.05}, 4.35)),
    ?assertEqual({ok, 4.35},
                 jsone_schema:validate(jsone:decode(<<"{\"multipleOf\":0.01}">>),
                                       jsone:decode(<<"4.35">>))),
    ?assertEqual({ok, 1.0e308},
                 jsone_schema:validate(#{<<"type">> => <<"integer">>, <<"multipleOf">> => 0.5},
                                       1.0e308)),

    %% 負の値も同じ経路で判定する
    ?assertEqual({ok, -4.35}, jsone_schema:validate(#{<<"multipleOf">> => 0.05}, -4.35)),
    ?assertMatch({error, [#{error := not_multiple_of}]},
                 jsone_schema:validate(#{<<"multipleOf">> => 0.05}, -4.36)),

    %% 10 進で割り切れない値は倍数として扱わない
    ?assertMatch({error, [#{error := not_multiple_of}]},
                 jsone_schema:validate(#{<<"multipleOf">> => 1.5}, 35)),
    ?assertMatch({error, [#{error := not_multiple_of}]},
                 jsone_schema:validate(#{<<"multipleOf">> => 0.0001}, 0.00751)),
    ?assertMatch({error, [#{error := not_multiple_of}]},
                 jsone_schema:validate(#{<<"multipleOf">> => 0.123456789}, 1.0e308)),

    %% 指数差が大きい組み合わせも 10 進で厳密に判定する
    %% 1.0e308 = 1.0e-300 * 10^608、1.0e308 = 1.0e-93 * 10^401 でどちらも整数倍
    ?assertEqual({ok, 1.0e308}, jsone_schema:validate(#{<<"multipleOf">> => 1.0e-300}, 1.0e308)),
    ?assertEqual({ok, 1.0e308}, jsone_schema:validate(#{<<"multipleOf">> => 1.0e-93}, 1.0e308)),
    %% 指数差 632 は倍精度で取り得る最大。1.0e308 = 5.0e-324 * 2 * 10^631
    ?assertEqual({ok, 1.0e308}, jsone_schema:validate(#{<<"multipleOf">> => 5.0e-324}, 1.0e308)),
    ?assertMatch({error, [#{error := not_multiple_of}]},
                 jsone_schema:validate(#{<<"multipleOf">> => 3.0e-93}, 1.0e308)),

    %% 整数オペランドと、値が整数の浮動小数
    ?assertEqual({ok, 0}, jsone_schema:validate(#{<<"multipleOf">> => 1.5}, 0)),
    ?assertEqual({ok, 4.5}, jsone_schema:validate(#{<<"multipleOf">> => 1.5}, 4.5)),
    ?assertEqual({ok, 0.0075}, jsone_schema:validate(#{<<"multipleOf">> => 0.0001}, 0.0075)),
    ?assertEqual({ok, 2.0}, jsone_schema:validate(#{<<"multipleOf">> => 1.0}, 2.0)),
    ?assertEqual({ok, 4}, jsone_schema:validate(#{<<"multipleOf">> => 2}, 4)),

    %% 2^53 を超える整数を浮動小数に丸めず、整数のまま判定する
    ?assertEqual({ok, 9007199254740992},
                 jsone_schema:validate(#{<<"multipleOf">> => 2.0}, 9007199254740992)),
    ?assertMatch({error, [#{error := not_multiple_of}]},
                 jsone_schema:validate(#{<<"multipleOf">> => 2.0}, 9007199254740993)),

    %% multipleOf が正の数値でない場合はスキーマのエラーになる
    ?assertMatch({error, [#{kind := schema, error := wrong_multiple_of}]},
                 jsone_schema:validate(#{<<"multipleOf">> => 0}, 1)),
    ?assertMatch({error, [#{kind := schema, error := wrong_multiple_of}]},
                 jsone_schema:validate(#{<<"multipleOf">> => -1}, 1)),
    ?assertMatch({error, [#{kind := schema, error := wrong_multiple_of}]},
                 jsone_schema:validate(#{<<"multipleOf">> => <<"2">>}, 1)),
    ok.


ref_cycle_test() ->
    %% 解決経路で同じ (解決先スキーマ, インスタンス値) を 2 回評価する場合は循環
    ?assertEqual({error,
                  [#{
                     kind => schema,
                     schema => #{<<"$ref">> => <<"#">>},
                     error => ref_cycle
                    }]},
                 jsone_schema:validate(#{<<"$ref">> => <<"#">>}, 1)),

    %% 相互参照
    MutualSchema =
        #{
          <<"definitions">> =>
              #{
                <<"a">> => #{<<"$ref">> => <<"#/definitions/b">>},
                <<"b">> => #{<<"$ref">> => <<"#/definitions/a">>}
               },
          <<"$ref">> => <<"#/definitions/a">>
         },
    ?assertMatch({error, [#{kind := schema, error := ref_cycle}]},
                 jsone_schema:validate(MutualSchema, 1)),

    %% allOf / anyOf / oneOf / not の下でも分岐の失敗に変換されず検証全体が止まる
    ?assertMatch({error, [#{kind := schema, error := ref_cycle}]},
                 jsone_schema:validate(#{<<"allOf">> => [#{<<"$ref">> => <<"#">>}]}, 1)),
    ?assertMatch({error, [#{kind := schema, error := ref_cycle}]},
                 jsone_schema:validate(#{<<"anyOf">> => [#{<<"$ref">> => <<"#">>}]}, 1)),
    ?assertMatch({error, [#{kind := schema, error := ref_cycle}]},
                 jsone_schema:validate(#{<<"oneOf">> => [#{<<"$ref">> => <<"#">>}]}, 1)),
    ?assertMatch({error, [#{kind := schema, error := ref_cycle}]},
                 jsone_schema:validate(#{<<"not">> => #{<<"$ref">> => <<"#">>}}, 1)),

    %% map 形式の dependencies はインスタンスを降下させずにパスだけを伸ばす
    DependencySchema = #{<<"dependencies">> => #{<<"a">> => #{<<"$ref">> => <<"#">>}}},
    ?assertMatch({error, [#{kind := schema, error := ref_cycle}]},
                 jsone_schema:validate(DependencySchema, #{<<"a">> => 1})),

    %% エラー件数の上限を infinity にしても停止する
    ?assertMatch({error, [#{kind := schema, error := ref_cycle}]},
                 jsone_schema:validate(#{<<"$ref">> => <<"#">>}, 1, #{max_errors => infinity})),

    %% 解決スタックの上限を超える入れ子データは、正当なものでもエラーになる
    %% 境界は上限と同じ段数まで成功し、その次の段数でエラーになる
    DeepSchema = #{<<"properties">> => #{<<"x">> => #{<<"$ref">> => <<"#">>}}},
    ValueAtLimit = lists:foldl(fun(_, Acc) -> #{<<"x">> => Acc} end, 1, lists:seq(1, 1000)),
    ValueOverLimit = lists:foldl(fun(_, Acc) -> #{<<"x">> => Acc} end, 1, lists:seq(1, 1001)),
    ?assertMatch({ok, _}, jsone_schema:validate(DeepSchema, ValueAtLimit)),
    ?assertMatch({error, [#{kind := schema, error := ref_depth_limit}]},
                 jsone_schema:validate(DeepSchema, ValueOverLimit)),

    %% インスタンスが降下する正当な再帰は引き続き成功する
    RecursiveSchema =
        #{
          <<"properties">> => #{<<"foo">> => #{<<"$ref">> => <<"#">>}},
          <<"additionalProperties">> => false
         },
    ?assertEqual({ok, #{<<"foo">> => #{<<"foo">> => false}}},
                 jsone_schema:validate(RecursiveSchema, #{<<"foo">> => #{<<"foo">> => false}})),
    ?assertEqual({ok, [[1]]},
                 jsone_schema:validate(#{<<"contains">> => #{<<"$ref">> => <<"#">>}}, [[1]])),

    %% 追加した理由は検証の戻り値からそのままエラー理由の JSON にできる
    {error, [CycleReason]} = jsone_schema:validate(#{<<"$ref">> => <<"#">>}, 1),
    #{<<"errors">> := [CycleJson]} = jsone:decode(jsone_schema_error:to_json([CycleReason])),
    ?assertEqual(#{<<"error">> => <<"ref_cycle">>, <<"kind">> => <<"schema">>},
                 maps:with([<<"error">>, <<"kind">>], CycleJson)),
    {error, [LimitReason]} = jsone_schema:validate(DeepSchema, ValueOverLimit),
    #{<<"errors">> := [LimitJson]} = jsone:decode(jsone_schema_error:to_json([LimitReason])),
    ?assertEqual(#{<<"error">> => <<"ref_depth_limit">>, <<"kind">> => <<"schema">>},
                 maps:with([<<"error">>, <<"kind">>], LimitJson)),
    ok.


option_validation_test() ->
    Schema = #{<<"type">> => <<"integer">>},

    %% 不明なキーは badarg になる
    ?assertError(badarg, jsone_schema:validate(Schema, 1, #{max_error => 3})),
    ?assertError(badarg, jsone_schema:validate_key(<<"key">>, 1, #{max_error => 3})),
    ?assertError(badarg, jsone_schema:add_schema(<<"key">>, <<"{}">>, #{recursive => false})),
    ?assertError(badarg, jsone_schema:load_schemas("/nonexistent", #{max_errors => 1})),

    %% オプションが map でない場合も badarg になる
    ?assertError(badarg, jsone_schema:validate(Schema, 1, [])),
    ?assertError(badarg, jsone_schema:validate_key(<<"key">>, 1, [])),
    ?assertError(badarg, jsone_schema:add_schema(<<"key">>, <<"{}">>, [])),
    ?assertError(badarg, jsone_schema:load_schemas("/nonexistent", [])),

    %% 受け付けるキーは API ごとに異なる
    ?assertError(badarg, jsone_schema:validate(Schema, 1, #{parser_fun => fun jsone:decode/1})),
    ?assertError(badarg, jsone_schema:validate(Schema, 1, #{recursive => false})),
    ?assertError(badarg, jsone_schema:validate_key(<<"key">>, 1, #{recursive => false})),

    %% 値の型も検査する
    ?assertError(badarg, jsone_schema:validate(Schema, 1, #{schemas => []})),
    ?assertError(badarg, jsone_schema:validate(Schema, 1, #{schema_loader => <<"loader">>})),
    ?assertError(badarg, jsone_schema:validate(Schema, 1, #{validate_format => 1})),
    ?assertError(badarg, jsone_schema:add_schema(<<"key">>, <<"{}">>, #{parser_fun => 5})),
    ?assertError(badarg,
                 jsone_schema:add_schema(<<"key">>, <<"{}">>, #{validate_format => false})),
    ?assertError(badarg, jsone_schema:load_schemas("/nonexistent", #{recursive => 5})),
    ?assertError(badarg, jsone_schema:load_schemas("/nonexistent", #{parser_fun => 5})),
    ?assertError(badarg, jsone_schema:load_schemas("/nonexistent", #{validate_format => false})),

    %% max_errors は正の整数か infinity だけを受け付ける
    ?assertError(badarg, jsone_schema:validate(Schema, 1, #{max_errors => 0})),
    ?assertError(badarg, jsone_schema:validate(Schema, 1, #{max_errors => -1})),
    ?assertError(badarg, jsone_schema:validate(Schema, 1, #{max_errors => all})),
    ?assertError(badarg, jsone_schema:validate(Schema, 1, #{max_errors => <<"2">>})),
    ?assertError(badarg, jsone_schema:validate(Schema, 1, #{max_errors => 1.0})),
    ?assertEqual({ok, 1}, jsone_schema:validate(Schema, 1, #{max_errors => 1})),
    ?assertEqual({ok, 1}, jsone_schema:validate(Schema, 1, #{max_errors => infinity})),

    %% 受け付けるキーと値は現行どおり動く
    ?assertEqual({ok, 1}, jsone_schema:validate(Schema, 1, #{schemas => #{}})),
    ?assertEqual({ok, 1},
                 jsone_schema:validate(#{<<"$ref">> => <<"https://example.com/a.json">>},
                                       1,
                                       #{schemas => #{<<"https://example.com/a.json">> => Schema}})),
    ?assertEqual(ok,
                 jsone_schema:add_schema(<<"option-key">>, <<"{}">>, #{parser_fun => fun jsone:decode/1})),
    ?assertEqual(ok, jsone_schema:del_schema(<<"option-key">>)),
    ok.


schema_key_validation_test() ->
    %% キーワード名が binary でないスキーマは schema エラーになる
    ?assertMatch({error, [#{kind := schema, error := schema_invalid}]},
                 jsone_schema:validate(#{type => <<"integer">>}, 1)),
    %% binary キーが混在していても同じ
    ?assertMatch({error, [#{kind := schema, error := schema_invalid}]},
                 jsone_schema:validate(#{type => <<"integer">>, <<"minimum">> => 100}, 1)),
    %% {keys, attempt_atom} でデコードしたスキーマは受け付けない
    AtomSchema = jsone:decode(<<"{\"type\": \"integer\"}">>, [{keys, attempt_atom}]),
    ?assertMatch({error, [#{kind := schema, error := schema_invalid}]},
                 jsone_schema:validate(AtomSchema, <<"not an integer">>)),

    %% プロパティ名が binary でない場合も schema エラーになる
    ?assertMatch({error, [#{kind := schema, error := schema_invalid}]},
                 jsone_schema:validate(#{<<"properties">> => #{a => #{<<"type">> => <<"string">>}}},
                                       #{<<"a">> => 1})),
    ?assertMatch({error, [#{kind := schema, error := schema_invalid}]},
                 jsone_schema:validate(#{<<"dependencies">> => #{a => [<<"b">>]}},
                                       #{<<"a">> => 1})),
    ?assertMatch({error, [#{kind := schema, error := schema_invalid}]},
                 jsone_schema:validate(#{<<"patternProperties">> => #{a => #{}}},
                                       #{<<"a">> => 1})),

    %% プロパティ名の検査はインスタンスの型に依存しない
    ?assertMatch({error, [#{kind := schema, error := schema_invalid}]},
                 jsone_schema:validate(#{<<"properties">> => #{a => #{<<"type">> => <<"string">>}}},
                                       42)),
    ?assertMatch({error, [#{kind := schema, error := schema_invalid}]},
                 jsone_schema:validate(#{<<"dependencies">> => #{a => [<<"b">>]}}, 42)),
    ?assertMatch({error, [#{kind := schema, error := schema_invalid}]},
                 jsone_schema:validate(#{<<"patternProperties">> => #{a => #{}}}, 42)),

    %% 登録は成功し、検証のときに schema エラーになる
    Key = <<"jsone_schema_tests_atom_key">>,
    try
        ?assertEqual(ok, jsone_schema:add_schema(Key, #{type => <<"integer">>})),
        ?assertMatch({error, [#{kind := schema, error := schema_invalid}]},
                     jsone_schema:validate_key(Key, <<"x">>))
    after
        jsone_schema:del_schema(Key)
    end,
    ok.


%% Internal Functions


tmp_dir() ->
    case os:getenv("TMPDIR") of
        false ->
            "/tmp";
        Dir ->
            Dir
    end.
