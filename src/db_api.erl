-module(db_api).
-author("nikita").

-record(record, {
    key :: term(),
    value :: term()
}).


%% API
-export([
    setup/0,
    set/1,
    set/2,
    get_v/0,
    get_v/1
]).

%%====================================================================
%% API functions
%%====================================================================

%% Создание схемы, запуск mnesia, создание таблицы с именем record и полями key, value
setup() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(record,
        [{disc_copies, [node()]},
            {attributes, record_info(fields, record)}]).


-spec set(Val::term()) -> ok.
set(Val) ->
    set(_Key = foo, Val).

-spec set(Key :: term(), Val :: term()) -> ok.
set(Key, Val) ->
    Insert =
        fun() ->
            mnesia:write(
                #record{
                    key = Key,
                    value = Val
                })
        end,
    {atomic, _} = mnesia:transaction(Insert),
    ok.


-spec get_v() -> {ok, Val :: term()}|{error, not_found}.
get_v() ->
    get_v(foo).

-spec get_v(Key :: term) -> {ok, Val :: term()}|{error, not_found}.
get_v(Key) ->
    F =
        fun() ->
            mnesia:read({record, Key})
        end,
    {atomic, Results} = mnesia:transaction(F),
    case Results of
        [{record, _, Val}] -> {ok, Val};
        [] -> {error, not_found}
    end.

%%====================================================================
%% Internal function
%%====================================================================
