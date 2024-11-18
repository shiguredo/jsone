-module(jsone).

-export([]).

-export_type([json_value/0,
              json_boolean/0,
              json_number/0,
              json_string/0,
              json_array/0,
              json_object/0,

              encode_option/0,
              decode_option/0]).

-type json_value() :: json_number() |
                      json_string() |
                      json_array() |
                      json_object() |
                      json_boolean() |
                      null |
                      undefined.
-type json_boolean() :: boolean().
-type json_number() :: number().
-type json_string() :: binary() | atom().
-type json_array() :: [json_value()].
-type json_object() :: #{json_string() => json_value()}.

-type encode_option() :: skip_undefined |
                         undefined_as_null |
                         {float_format, [float_format_option()]}.

-type decode_option() :: {keys, attempt_atom}.

-type float_format_option() :: {scientific, Decimals :: 0..249} |
                               {decimals, Decimals :: 0..253} |
                               compact |
                               short.
