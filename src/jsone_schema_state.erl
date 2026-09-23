%% JSON Schema の検証状態
%%
%% 検証中のスキーマ・基準 URI・エラー理由などを保持する。
%% 検証コアはこの状態を引き回すだけで、グローバルな状態には依存しない。
%%
%% エラー理由は新しいものが先頭になるように保持する。1 件追加するたびに
%% リストの末尾へ足すと O(n) になるため、追加は先頭、読み出し時に反転する。
-module(jsone_schema_state).

-export([new/2, new/3,
         add_error/2,
         add_to_path/2,
         enter_schema/2,
         enter_ref/3,
         get_base_uri/1,
         get_current_path/1,
         get_current_schema/1,
         get_document_uri/1,
         get_errors/1,
         get_max_errors/1,
         get_root_schema/1,
         get_schema_loader/1,
         get_schemas/1,
         get_validate_format/1,
         has_reached_max_errors/1,
         leave_ref/1,
         remove_last_from_path/1,
         reset_errors/1,
         resolve_ref/2,
         restore/2,
         restore_schema/2,
         undo_resolve_ref/2]).

-export_type([state/0]).

-include("jsone_schema.hrl").

-type schema() :: map() | boolean().

-record(state, {
          base_uri :: undefined | binary(),
          current_path = [] :: [binary() | non_neg_integer()],
          current_schema :: schema(),
          document_uri :: undefined | binary(),
          errors = [] :: [jsone_schema_error:reason()],
          index = undefined :: undefined | jsone_schema_index:index(),
          max_errors = 1 :: pos_integer() | infinity,
          ref_stack = [] :: [{schema(), jsone:json_value()}],
          root_schema :: schema(),
          schema_loader :: undefined | fun((binary()) -> {ok, schema()} | schema() | {error, term()}),
          schemas = #{} :: #{binary() => schema()},
          validate_format = true :: boolean()
         }).

-opaque state() :: #state{}.


%% 検証状態を生成する
-spec new(schema(), map()) -> state().
new(RootSchema, Options) ->
    new(RootSchema, Options, undefined).


%% ドキュメント URI 付きで検証状態を生成する
%%
%% ストアに登録したキーで検証する場合、そのキーがドキュメント URI になる。
%% ルートスキーマに絶対 URI の `$id` があれば、それをドキュメント URI とみなし、
%% ローカルの `$ref` (#/...) を同じドキュメントとして解決できるようにする。
-spec new(schema(), map(), undefined | binary() | string()) -> state().
new(RootSchema, Options, DocumentURI0) ->
    DocumentURI = document_uri(RootSchema, DocumentURI0),
    #state{
      base_uri = DocumentURI,
      current_path = [],
      current_schema = RootSchema,
      document_uri = DocumentURI,
      max_errors = maps:get(max_errors, Options, 1),
      root_schema = RootSchema,
      schema_loader = maps:get(schema_loader, Options, undefined),
      schemas = maps:get(schemas, Options, #{}),
      validate_format = maps:get(validate_format, Options, true)
     }.


%% エラー理由を追加する
-spec add_error(state(), jsone_schema_error:reason()) -> state().
add_error(#state{errors = Errors} = State, Reason) ->
    State#state{errors = [Reason | Errors]}.


%% 現在のパスに要素を追加する
%%
%% パスは逆順 (深い方が先頭) で保持する。
-spec add_to_path(state(), binary() | non_neg_integer()) -> state().
add_to_path(#state{current_path = Path} = State, Item) ->
    State#state{current_path = [Item | Path]}.


%% スキーマに入り、`$id` を反映した基準 URI を設定する
%%
%% 基準 URI は、そのスキーマの中の相対 `$ref` を解決するために使う。
-spec enter_schema(state(), schema()) -> state().
enter_schema(State, JsonSchema) when is_map(JsonSchema) ->
    case JsonSchema of
        #{?ID := Id} when is_binary(Id) ->
            Base = jsone_schema_uri:resolve(State#state.base_uri, Id),
            State#state{current_schema = JsonSchema, base_uri = Base};
        _ ->
            State#state{current_schema = JsonSchema}
    end;
enter_schema(State, JsonSchema) ->
    State#state{current_schema = JsonSchema}.


%% 現在の基準 URI を取得する
-spec get_base_uri(state()) -> undefined | binary().
get_base_uri(#state{base_uri = BaseURI}) ->
    BaseURI.


%% 現在のパスを取得する
%%
%% 逆順 (深い方が先頭) で返す。
-spec get_current_path(state()) -> [binary() | non_neg_integer()].
get_current_path(#state{current_path = Path}) ->
    Path.


%% 現在のスキーマを取得する
-spec get_current_schema(state()) -> schema().
get_current_schema(#state{current_schema = JsonSchema}) ->
    JsonSchema.


%% 現在のドキュメント URI を取得する
-spec get_document_uri(state()) -> undefined | binary().
get_document_uri(#state{document_uri = DocumentURI}) ->
    DocumentURI.


%% エラー理由を時系列順で取得する
-spec get_errors(state()) -> [jsone_schema_error:reason()].
get_errors(#state{errors = Errors}) ->
    lists:reverse(Errors).


%% エラーの上限を取得する
-spec get_max_errors(state()) -> pos_integer() | infinity.
get_max_errors(#state{max_errors = MaxErrors}) ->
    MaxErrors.


%% ルートスキーマを取得する
-spec get_root_schema(state()) -> schema().
get_root_schema(#state{root_schema = RootSchema}) ->
    RootSchema.


%% スキーマローダを取得する
-spec get_schema_loader(state()) ->
          undefined | fun((binary()) -> {ok, schema()} | schema() | {error, term()}).
get_schema_loader(#state{schema_loader = SchemaLoader}) ->
    SchemaLoader.


%% `format' を検証するかどうかを返す
-spec get_validate_format(state()) -> boolean().
get_validate_format(#state{validate_format = ValidateFormat}) ->
    ValidateFormat.


%% 静的スキーママップを取得する
%%
%% 検証中に読み込んだドキュメントも含む。
-spec get_schemas(state()) -> #{binary() => schema()}.
get_schemas(#state{schemas = Schemas}) ->
    Schemas.


%% エラーの上限に達したかどうかを返す
%%
%% 上限が infinity の場合は常に false になる。
-spec has_reached_max_errors(state()) -> boolean().
has_reached_max_errors(#state{max_errors = infinity}) ->
    false;
has_reached_max_errors(#state{max_errors = MaxErrors, errors = Errors}) ->
    length(Errors) >= MaxErrors.


%% 現在のパスから要素を削除する
-spec remove_last_from_path(state()) -> state().
remove_last_from_path(#state{current_path = [_ | Path]} = State) ->
    State#state{current_path = Path};
remove_last_from_path(State) ->
    State.


%% エラー理由をすべて削除する
%%
%% サブスキーマの検証を始めるときに使う。
-spec reset_errors(state()) -> state().
reset_errors(State) ->
    State#state{errors = []}.


%% `$ref` を解決する
%%
%% `$id` で索引されたリソース URI とプレーンネームフラグメントを先に探し、
%% 見つからない場合は JSON Pointer として解決する。
%% 解決に成功した場合は、ルートスキーマと基準 URI を更新した状態と
%% 解決先のスキーマを返す。
-spec resolve_ref(state(), binary()) ->
          {ok, state(), schema()} | {error, jsone_schema_error:error_info(), state()}.
resolve_ref(State0, Reference) ->
    State = ensure_index(State0),
    Absolute = jsone_schema_uri:resolve(State#state.base_uri, Reference),
    {DocumentURI, Fragment} = jsone_schema_uri:split_fragment(Absolute),
    case lookup_identifier(State, Absolute, DocumentURI) of
        {ok, SubSchema, RefState} ->
            {ok, RefState, SubSchema};
        error ->
            resolve_pointer(State, Absolute, DocumentURI, Fragment)
    end.


%% サブスキーマ検証の前後でスキーマ・基準 URI・エラー理由を復元する
%%
%% サブスキーマの中で追加されたエラーは呼び出し元のエラーリストには残さない。
-spec restore(state(), state()) -> state().
restore(State, OriginalState) ->
    State#state{
      root_schema = OriginalState#state.root_schema,
      current_schema = OriginalState#state.current_schema,
      document_uri = OriginalState#state.document_uri,
      base_uri = OriginalState#state.base_uri,
      errors = OriginalState#state.errors
     }.


%% スキーマと基準 URI だけを復元する
-spec restore_schema(state(), state()) -> state().
restore_schema(State, OriginalState) ->
    State#state{
      current_schema = OriginalState#state.current_schema,
      base_uri = OriginalState#state.base_uri
     }.


%% `$ref` 解決前の状態に戻す
%%
%% 読み込んだドキュメントのキャッシュと `$id` の索引は、同じ検証の中で
%% 何度も `$ref` を解決するときに再利用できるため残す。
-spec undo_resolve_ref(state(), state()) -> state().
undo_resolve_ref(State, OriginalState) ->
    State#state{
      root_schema = OriginalState#state.root_schema,
      current_schema = OriginalState#state.current_schema,
      document_uri = OriginalState#state.document_uri,
      base_uri = OriginalState#state.base_uri
     }.


%% Internal Functions


%% `$ref' の解決先をスタックに積む
%%
%% 同じ解決先スキーマを同じインスタンス値で 2 回評価する場合は循環とみなす。
%% スタックは現在の解決経路だけを保持し、解決から戻るときに降ろすため、
%% 兄弟分岐で同じ組を 2 回評価しても循環とはならない。
%% 循環とスタックの上限が同時に成立する場合は、原因を特定できる循環を優先する。
%% 上限に達している場合は積まずに `limit' を返す。
-spec enter_ref(schema(), jsone:json_value(), state()) -> cycle | limit | {ok, state()}.
enter_ref(JsonSchema, Value, State) ->
    RefStack = State#state.ref_stack,
    case lists:member({JsonSchema, Value}, RefStack) of
        true ->
            cycle;
        false when length(RefStack) >= ?REF_STACK_LIMIT ->
            limit;
        false ->
            {ok, State#state{ref_stack = [{JsonSchema, Value} | RefStack]}}
    end.


%% `$ref' の解決先をスタックから降ろす
-spec leave_ref(state()) -> state().
leave_ref(#state{ref_stack = []} = State) ->
    State;
leave_ref(#state{ref_stack = [_ | Rest]} = State) ->
    State#state{ref_stack = Rest}.


%% `$id' の索引を必要になった時点で作る
%%
%% 索引は `$ref' を解決するときにだけ必要になる。`$ref' を使わないスキーマでは
%% 索引を作らないことで、検証 1 回あたりのオーバーヘッドを抑える。
ensure_index(#state{index = undefined} = State) ->
    Index = jsone_schema_index:build(State#state.root_schema, State#state.document_uri),
    State#state{index = Index};
ensure_index(State) ->
    State.


%% ドキュメント URI を決める
%%
%% 呼び出し元が指定していない場合は、ルートスキーマの絶対 URI の `$id` を
%% フラグメント付きでもドキュメント URI として扱う。
document_uri(RootSchema, undefined) ->
    case absolute_schema_id(RootSchema) of
        undefined ->
            undefined;
        Id ->
            {DocumentURI, _Fragment} = jsone_schema_uri:split_fragment(Id),
            DocumentURI
    end;
document_uri(_RootSchema, DocumentURI0) ->
    jsone_schema_uri:to_binary_or_undefined(DocumentURI0).


%% 絶対 URI の `$id` を持つマップだけを対象にする
absolute_schema_id(RootSchema) when is_map(RootSchema) ->
    maybe
        #{?ID := Id} ?= RootSchema,
        true ?= is_binary(Id),
        true ?= jsone_schema_uri:is_absolute(Id),
        Id
    else
        _Reason ->
            undefined
    end;
absolute_schema_id(_RootSchema) ->
    undefined.


%% `$id` で索引されたスキーマを探す
%%
%% 未読み込みのドキュメントに `$id` がある場合のために、
%% ドキュメントを読み込んで索引を更新してからもう一度探す。
%% 埋め込みリソースを指す参照は、ドキュメントの読み込みより先に索引から引く。
lookup_identifier(State, Absolute, DocumentURI) ->
    case jsone_schema_index:lookup(State#state.index, Absolute) of
        {ok, Entry} ->
            {ok, jsone_schema_index:entry_schema(Entry), state_from_entry(State, Entry)};
        error ->
            case lookup_resource_identifier(State, Absolute, DocumentURI) of
                error ->
                    lookup_loaded_identifier(State, Absolute, DocumentURI);
                Resolved ->
                    Resolved
            end
    end.


%% 埋め込みリソースをドキュメント URI で引く
%%
%% 参照先の URI がドキュメントの `$id` ではなく埋め込みリソースの `$id` の
%% 場合、その URI を鍵にした索引エントリがリソースそのものを指す。JSON Pointer
%% はリソースのスキーマをルートとして評価する。索引はスキーマ位置かどうかに
%% かかわらず `$id` を鍵にするため、索引にある URI はすべてリソースとして扱う。
%% ドキュメント URI 自身のエントリではエントリのスキーマがドキュメントルートと
%% 一致するため、解決結果は変わらない。空のドキュメント URI は今検証している
%% ルートスキーマを指すため、索引は引かない。
%% 索引でポインタが見つからない場合は error を返し、ドキュメントの読み込みを
%% 経由する既存の解決に委ねる。
lookup_resource_identifier(_State, _Absolute, ~"") ->
    error;
lookup_resource_identifier(State, Absolute, DocumentURI) ->
    case jsone_schema_index:lookup(State#state.index, DocumentURI) of
        {ok, Entry} ->
            %% 参照先自身の `$id` は検証を始めるときに反映されるため、
            %% ここでは途中のスキーマの `$id` だけを基準 URI に反映する
            {_DocumentURI, Fragment} = jsone_schema_uri:split_fragment(Absolute),
            ResourceSchema = jsone_schema_index:entry_schema(Entry),
            case jsone_schema_json_pointer:eval_path(Fragment, ResourceSchema) of
                {ok, SubSchema, Visited} ->
                    RefState =
                        (state_from_entry(State, Entry))#state{
                          base_uri = compose_base_uri(DocumentURI, Visited)
                         },
                    {ok, SubSchema, RefState};
                {error, not_found} ->
                    error
            end;
        error ->
            error
    end.


%% ドキュメントを読み込んでから索引を引き直す
lookup_loaded_identifier(State, Absolute, DocumentURI) ->
    maybe
        {ok, _RootSchema, _EffectiveURI, LoadedState} ?= resolve_document(State, DocumentURI),
        {ok, Entry} ?= jsone_schema_index:lookup(LoadedState#state.index, Absolute),
        {ok, jsone_schema_index:entry_schema(Entry), state_from_entry(LoadedState, Entry)}
    else
        _Reason ->
            error
    end.


%% 索引のエントリに合わせて、ルートスキーマと基準 URI を更新する
%%
%% 参照先自身の `$id` は検証を始めるときに反映されるため、ここでは
%% `$id` を反映する前の基準 URI を設定する。
state_from_entry(State, Entry) ->
    State#state{
      root_schema = jsone_schema_index:entry_root_schema(Entry),
      document_uri = jsone_schema_index:entry_document_uri(Entry),
      base_uri = jsone_schema_index:entry_base_uri(Entry)
     }.


%% JSON Pointer として `$ref` を解決する
resolve_pointer(State, Absolute, DocumentURI, Fragment) ->
    case resolve_document(State, DocumentURI) of
        {ok, RootSchema, EffectiveURI, DocState} ->
            case jsone_schema_json_pointer:eval_path(Fragment, RootSchema) of
                {ok, SubSchema, Visited} ->
                    %% 参照先の `$id` は検証開始時に反映されるため、
                    %% ここでは途中のスキーマの `$id` だけを基準 URI に反映する
                    Base = compose_base_uri(EffectiveURI, Visited),
                    RefState =
                        DocState#state{
                          root_schema = RootSchema,
                          document_uri = EffectiveURI,
                          base_uri = Base
                         },
                    {ok, RefState, SubSchema};
                {error, not_found} ->
                    {error, {?schema_not_found, Absolute}, State}
            end;
        {error, Reason} ->
            {error, Reason, State}
    end.


%% ドキュメント URI に対応するルートスキーマを返す
%%
%% 空のドキュメント URI と現在のドキュメント URI は、今検証している
%% ルートスキーマを指す。
resolve_document(State, ~"") ->
    {ok, State#state.root_schema, State#state.document_uri, State};
resolve_document(State, DocumentURI) ->
    case DocumentURI =:= State#state.document_uri of
        true ->
            {ok, State#state.root_schema, State#state.document_uri, State};
        false ->
            find_or_load_document(State, DocumentURI)
    end.


%% 静的スキーママップにあれば使い、無ければローダで読み込む
find_or_load_document(State, DocumentURI) ->
    maybe
        #{DocumentURI := JsonSchema} ?= State#state.schemas,
        {ok, JsonSchema, DocumentURI, cache_document(State, DocumentURI, JsonSchema)}
    else
        _Reason ->
            load_document(State, DocumentURI)
    end.


%% ローダでドキュメントを読み込む
%%
%% 読み込みの失敗は次の 3 通りに分けて返し、原因の情報を潰さずに伝える。
%%
%% - ローダ未指定の場合は `{?schema_not_found, DocumentURI}`
%% - ローダが `{error, Reason}` かスキーマでない値を返した場合は
%%   `{?schema_load_error, #{<<"uri">> := DocumentURI, <<"reason">> := Reason}}`
%% - ローダが例外を投げた場合は
%%   `{?schema_load_error, #{<<"uri">> := DocumentURI,
%%                           <<"class">> := Class, <<"reason">> := Reason}}`
%%
%% ローダが返す理由は任意の term であり、UTF-8 として不正なバイナリなど
%% `jsone:encode/1' が扱えない値もあり得る。そのまま詳細に載せると原因を
%% 報告する `jsone_schema_error:to_json/1' がクラッシュするため、
%% 詳細の値はエンコードできる形に寄せてから載せる。
%%
%% `cache_document/3' のように読み込みに成功したあとの索引構築で起きた例外は
%% ローダの失敗ではないため、ここでは捕まえず呼び出し元へ伝える。
load_document(#state{schema_loader = undefined}, DocumentURI) ->
    {error, {?schema_not_found, DocumentURI}};
load_document(#state{schema_loader = SchemaLoader} = State, DocumentURI) ->
    try SchemaLoader(DocumentURI) of
        Result ->
            maybe
                {ok, JsonSchema} ?= normalize_document(Result),
                {ok, JsonSchema, DocumentURI, cache_document(State, DocumentURI, JsonSchema)}
            else
                {error, Reason} ->
                    {error,
                     {?schema_load_error,
                      ensure_json_encodable_details(
                        #{<<"uri">> => DocumentURI, <<"reason">> => Reason})}}
            end
    catch
        Class:Reason ->
            {error,
             {?schema_load_error,
              ensure_json_encodable_details(
                #{<<"uri">> => DocumentURI, <<"class">> => Class, <<"reason">> => Reason})}}
    end.


%% ローダの戻り値をスキーマか失敗の理由に正規化する
%%
%% ローダは `{ok, Schema}` でもスキーマそのものでも受け付ける。
%% `{ok, Value}` と `{error, Reason}` の包みは外し、スキーマでない値を
%% 返した場合はその値自体を理由にして利用者が原因を追えるようにする。
normalize_document({ok, JsonSchema}) when is_map(JsonSchema); is_boolean(JsonSchema) ->
    {ok, JsonSchema};
normalize_document({ok, Other}) ->
    {error, Other};
normalize_document({error, Reason}) ->
    {error, Reason};
normalize_document(JsonSchema) when is_map(JsonSchema); is_boolean(JsonSchema) ->
    {ok, JsonSchema};
normalize_document(Other) ->
    {error, Other}.


%% 詳細の値を JSON にエンコードできる形へ寄せる
%%
%% エンコードできる値はそのまま残し、できない値だけ文字列に落とす。
%% 文字列化には `~w' を使う。`~p' はバイナリを文字列として表示するため、
%% UTF-8 として不正なバイトがそのまま現れ、結局 `to_json/1' がクラッシュする。
%% `~w' はバイト列を `<<255>>' のような数値表記で出力するため安全であり、
%% 不正なバイトもそのまま読み取れる。
%% `~w' は整形しないため幅を指定しない (`~0w' は空文字列になる)。
%% 出力は `unicode:characters_to_binary/1' に通して UTF-8 として妥当な
%% バイナリに揃える。
ensure_json_encodable_details(Details) ->
    maps:map(fun(_Key, Value) -> ensure_json_encodable(Value) end, Details).


ensure_json_encodable(Value) ->
    case jsone:try_encode(Value) of
        {ok, _Json} ->
            Value;
        {error, _Reason} ->
            unicode:characters_to_binary(io_lib:format("~w", [Value]))
    end.


%% 読み込んだドキュメントをキャッシュし、`$id' の索引を追加する
cache_document(State0, DocumentURI, JsonSchema) ->
    State = ensure_index(State0),
    Index = jsone_schema_index:build(JsonSchema, DocumentURI),
    Schemas = maps:put(DocumentURI, JsonSchema, State#state.schemas),
    State#state{schemas = Schemas, index = merge_index(State#state.index, Index)}.


%% 索引を統合する
merge_index(undefined, Index) ->
    Index;
merge_index(Index, NewIndex) ->
    maps:merge(Index, NewIndex).


%% 訪問した値のうち、ルートと参照先を除いた中間のスキーマの `$id' を
%% 基準 URI に反映する。参照先自身の `$id' は検証開始時に反映される。
compose_base_uri(Base, [_Root | Rest]) ->
    Intermediate = drop_last(Rest),
    lists:foldl(fun apply_schema_id/2, Base, Intermediate).


drop_last([]) ->
    [];
drop_last(List) ->
    lists:droplast(List).


apply_schema_id(JsonSchema, Base) when is_map(JsonSchema) ->
    case JsonSchema of
        #{?ID := Id} when is_binary(Id) ->
            jsone_schema_uri:resolve(Base, Id);
        _ ->
            Base
    end;
apply_schema_id(_JsonSchema, Base) ->
    Base.
