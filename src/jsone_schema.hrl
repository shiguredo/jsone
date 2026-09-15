%% JSON Schema draft 6 の実装で共有する定数
%%
%% キーワード名・型名・エラー情報は jsone_schema の独自表現として定義する。
%% jesse 互換のタプル形式への変換は jesse_error が行う。

%% 対応する `$schema` の値
-define(JSON_SCHEMA_DRAFT6, <<"http://json-schema.org/draft-06/schema#">>).
-define(DEFAULT_SCHEMA_VER, ?JSON_SCHEMA_DRAFT6).

%% 内部でエラーリストを伝播させるための throw タグ
-define(ERRORS, jsone_schema_errors).

%% JSON Schema draft 6 のキーワード
-define(SCHEMA,               <<"$schema">>).
-define(ID,                   <<"$id">>).
-define(ID_OLD,               <<"id">>).
-define(REF,                  <<"$ref">>).
-define(DEFINITIONS,          <<"definitions">>).
-define(DEFS,                 <<"$defs">>).
-define(TYPE,                 <<"type">>).
-define(PROPERTIES,           <<"properties">>).
-define(PATTERNPROPERTIES,    <<"patternProperties">>).
-define(PROPERTYNAMES,        <<"propertyNames">>).
-define(ADDITIONALPROPERTIES, <<"additionalProperties">>).
-define(ITEMS,                <<"items">>).
-define(ADDITIONALITEMS,      <<"additionalItems">>).
-define(CONTAINS,             <<"contains">>).
-define(REQUIRED,             <<"required">>).
-define(DEPENDENCIES,         <<"dependencies">>).
-define(MINIMUM,              <<"minimum">>).
-define(MAXIMUM,              <<"maximum">>).
-define(EXCLUSIVEMINIMUM,     <<"exclusiveMinimum">>).
-define(EXCLUSIVEMAXIMUM,     <<"exclusiveMaximum">>).
-define(MINITEMS,             <<"minItems">>).
-define(MAXITEMS,             <<"maxItems">>).
-define(UNIQUEITEMS,          <<"uniqueItems">>).
-define(PATTERN,              <<"pattern">>).
-define(MINLENGTH,            <<"minLength">>).
-define(MAXLENGTH,            <<"maxLength">>).
-define(ENUM,                 <<"enum">>).
-define(CONST,                <<"const">>).
-define(FORMAT,               <<"format">>).
-define(MULTIPLEOF,           <<"multipleOf">>).
-define(MAXPROPERTIES,        <<"maxProperties">>).
-define(MINPROPERTIES,        <<"minProperties">>).
-define(ALLOF,                <<"allOf">>).
-define(ANYOF,                <<"anyOf">>).
-define(ONEOF,                <<"oneOf">>).
-define(NOT,                  <<"not">>).

%% JSON の型名
-define(TYPE_ARRAY,   <<"array">>).
-define(TYPE_BOOLEAN, <<"boolean">>).
-define(TYPE_INTEGER, <<"integer">>).
-define(TYPE_NULL,    <<"null">>).
-define(TYPE_NUMBER,  <<"number">>).
-define(TYPE_OBJECT,  <<"object">>).
-define(TYPE_STRING,  <<"string">>).

%% スキーマのエラー理由
-define(invalid_dependency,        invalid_dependency).
-define(schema_error,              schema_error).
-define(schema_invalid,            schema_invalid).
-define(schema_not_found,          schema_not_found).
-define(schema_unsupported,        schema_unsupported).
-define(wrong_all_of_schema_array, wrong_all_of_schema_array).
-define(wrong_any_of_schema_array, wrong_any_of_schema_array).
-define(wrong_max_properties,      wrong_max_properties).
-define(wrong_min_properties,      wrong_min_properties).
-define(wrong_multiple_of,         wrong_multiple_of).
-define(wrong_one_of_schema_array, wrong_one_of_schema_array).
-define(wrong_required_array,      wrong_required_array).
-define(wrong_type_dependency,     wrong_type_dependency).
-define(wrong_type_items,          wrong_type_items).
-define(wrong_type_specification,  wrong_type_specification).
-define(wrong_draft6_id_tag,       wrong_draft6_id_tag).

%% データのエラー理由
-define(data_error,                  data_error).
-define(data_invalid,                data_invalid).
-define(missing_required_property,   missing_required_property).
-define(missing_dependency,          missing_dependency).
-define(no_match,                    no_match).
-define(no_extra_properties_allowed, no_extra_properties_allowed).
-define(no_extra_items_allowed,      no_extra_items_allowed).
-define(not_unique,                  not_unique).
-define(not_in_enum,                 not_in_enum).
-define(not_in_range,                not_in_range).
-define(not_multiple_of,             not_multiple_of).
-define(not_array,                   not_array).
-define(wrong_type,                  wrong_type).
-define(wrong_size,                  wrong_size).
-define(wrong_length,                wrong_length).
-define(wrong_format,                wrong_format).
-define(too_many_properties,         too_many_properties).
-define(too_few_properties,          too_few_properties).
-define(all_schemas_not_valid,       all_schemas_not_valid).
-define(any_schemas_not_valid,       any_schemas_not_valid).
-define(not_one_schema_valid,        not_one_schema_valid).
-define(more_than_one_schema_valid,  more_than_one_schema_valid).
-define(not_schema_valid,            not_schema_valid).
-define(external,                    external).
