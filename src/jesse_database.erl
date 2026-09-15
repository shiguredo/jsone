%% jesse_database 互換 API
%%
%% swidden がエラー型として参照するためだけに残している。
%% スキーマの保存と読み込みは jsone_schema_store が行う。
-module(jesse_database).

-export_type([error/0, error_reason/0]).

-type error() :: {error, error_reason()}.
-type error_reason() ::
        {database_error, Key :: binary() | string(), schema_not_found | unknown_uri_scheme}.
