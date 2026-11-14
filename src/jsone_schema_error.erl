%% JSON Schema のエラー表現
%%
%% エラー理由は JSON Schema の結果をそのまま表せる独自の map で持つ。
%%
%% データの検証エラーは `kind' が `data' で、どの値のどの場所が
%% どのスキーマに反したのかを保持する。スキーマ自体が不正な場合は
%% `kind' が `schema' で、path と value は持たない。
%% `$ref' の循環のように検証全体を打ち切る理由も、path を持たないため
%% `kind' は `schema' になる。
-module(jsone_schema_error).

-export([abort/2,
         data_invalid/3,
         schema_invalid/2,
         to_json/1, to_json/2]).

-export_type([error_info/0, reason/0]).

-include("jsone_schema.hrl").

-type schema() :: map() | boolean().
-type path() :: [binary() | non_neg_integer()].

%% エラーの内容
%%
%% 単純なエラーは atom で表し、詳細がある場合は `{atom(), term()}' で表す。
%% 詳細に別のエラー理由のリストが入ることもある (allOf / anyOf / oneOf など)。
-type error_info() :: atom() | {atom(), term()}.

-type reason() ::
        #{
          kind := data,
          path := path(),
          schema := schema(),
          value := jsone:json_value(),
          error := error_info()
         } |
        #{
          kind := schema,
          schema := schema(),
          error := error_info()
         }.


%% データの検証エラーを追加する
-spec data_invalid(error_info(), jsone:json_value(), jsone_schema_state:state()) ->
          jsone_schema_state:state().
data_invalid(Error, Value, State) ->
    Reason =
        #{
          kind => data,
          %% パスは検証中は逆順で持ち、エラーに載せるときに直す
          path => lists:reverse(jsone_schema_state:get_current_path(State)),
          schema => jsone_schema_state:get_current_schema(State),
          value => Value,
          error => Error
         },
    add_reason(Reason, State).


%% スキーマの検証エラーを追加する
-spec schema_invalid(error_info(), jsone_schema_state:state()) -> jsone_schema_state:state().
schema_invalid(Error, State) ->
    Reason =
        #{
          kind => schema,
          schema => jsone_schema_state:get_current_schema(State),
          error => Error
         },
    add_reason(Reason, State).


%% 検証全体を打ち切るスキーマエラーを投げる
%%
%% エラー件数の上限判定を通さずに throw する。`$ref' の循環と解決スタックの
%% 深さ上限は、検出した時点で検証を終えないと結果が確定しないため。
%% サブスキーマの分岐判定では捕捉せず、公開 API まで伝播させる。
%% この経路は検証そのものを打ち切るため、それまでに集めたエラーは捨てて
%% 打ち切りの理由だけを返す。
-spec abort(error_info(), jsone_schema_state:state()) -> no_return().
abort(Error, State) ->
    Reason =
        #{
          kind => schema,
          schema => jsone_schema_state:get_current_schema(State),
          error => Error
         },
    throw({?REF_ABORT, Reason}).


%% エラー理由を JSON にエンコードする
-spec to_json([reason()]) -> binary().
to_json(Reasons) when is_list(Reasons) ->
    jsone:encode(json_term(Reasons)).


%% オプション付きでエラー理由を JSON にエンコードする
-spec to_json([reason()], [jsone:encode_option()]) -> binary().
to_json(Reasons, Options) when is_list(Reasons) ->
    jsone:encode(json_term(Reasons), Options).


%% Internal Functions


%% エンコードできる形へ変換する
json_term(Reasons) ->
    #{<<"errors">> => [ reason_to_json(Reason) || Reason <:- Reasons ]}.


%% エラー理由を追加し、上限に達したら検証を打ち切る
%%
%% 上限に達したかどうかは追加後に判定する。上限が 1 の場合、
%% 最初のエラーを追加した時点で throw して呼び出し元へ伝える。
add_reason(Reason, State) ->
    State1 = jsone_schema_state:add_error(State, Reason),
    case jsone_schema_state:has_reached_max_errors(State1) of
        true ->
            throw({?ERRORS, jsone_schema_state:get_errors(State1)});
        false ->
            State1
    end.


reason_to_json(#{kind := data} = Reason) ->
    #{
      <<"kind">> => <<"data">>,
      <<"path">> => json_safe(maps:get(path, Reason)),
      <<"schema">> => json_safe(maps:get(schema, Reason)),
      <<"value">> => json_safe(maps:get(value, Reason)),
      <<"error">> => error_info_to_json(maps:get(error, Reason))
     };
reason_to_json(#{kind := schema} = Reason) ->
    #{
      <<"kind">> => <<"schema">>,
      <<"schema">> => json_safe(maps:get(schema, Reason)),
      <<"error">> => error_info_to_json(maps:get(error, Reason))
     }.


error_info_to_json(Error) when is_atom(Error) ->
    atom_to_binary(Error, utf8);
error_info_to_json({Name, Details}) ->
    #{<<"type">> => atom_to_binary(Name, utf8), <<"details">> => details_to_json(Details)}.


%% 詳細がエラー理由のリストであれば再帰的に変換する
%%
%% データの値は利用者が任意に組み立てられるため、kind キーを持つ map の
%% リストを無条件にエラー理由として扱うと、reason_to_json/1 がクラッシュする。
%% エラー理由として必要なキーがすべて揃っている場合だけ再帰変換する。
details_to_json(Details) when is_list(Details) ->
    case lists:all(fun is_reason/1, Details) of
        true ->
            try
                [ reason_to_json(Detail) || Detail <:- Details ]
            catch
                _Class:_Reason ->
                    json_safe(Details)
            end;
        false ->
            json_safe(Details)
    end;
details_to_json(Details) ->
    json_safe(Details).


is_reason(#{kind := data, path := Path, schema := Schema, value := _Value, error := Error}) ->
    is_list(Path) andalso is_schema(Schema) andalso is_error_info(Error);
is_reason(#{kind := schema, schema := Schema, error := Error}) ->
    is_schema(Schema) andalso is_error_info(Error);
is_reason(_Reason) ->
    false.


is_schema(Schema) ->
    is_map(Schema) orelse is_boolean(Schema).


is_error_info(Error) when is_atom(Error) ->
    true;
is_error_info({Name, _Details}) when is_atom(Name) ->
    true;
is_error_info(_Error) ->
    false.


%% JSON としてエンコードできない値は読み取れる文字列に落とす
%%
%% データの値、スキーマ、詳細には非 UTF-8 のバイナリなど JSON として
%% 不正な term が入り得る。エラー報告自体がクラッシュしないようにする。
json_safe(Term) ->
    case jsone:try_encode(Term) of
        {ok, _Json} ->
            Term;
        {error, _Reason} ->
            unicode:characters_to_binary(io_lib:format("~w", [Term]))
    end.
