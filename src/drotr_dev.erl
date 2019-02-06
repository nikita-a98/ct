%%% ==================================================================
%%% @author Oleksandr Boiko <erlydev@gmail.com>
%%% @doc
%%% WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!
%%%
%%% !!! Utilities for testing and development only !!!
%%%
%%% WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!
%%% @end
%%% ==================================================================

-module(drotr_dev).

%% API
-export([
    modules/1,
    debug_app/1,
    debug_app/2,
    debug/0,
    debug/1,
    debug/2,
    debug_stop/0
]).

%% API
-export([
    clear_tab/1,
    clear_all_tabs/0,
    count_tabs/0,
    tab_records/0,
    tab_words/0,
    tab_bytes/0,
    mnesia_nodes/0,
    mnesia_dir/0
]).

%% API
-export([
    sys_info/0
]).

%%% ==================================================================
%%% Includes
%%% ==================================================================

-include_lib("stdlib/include/ms_transform.hrl").

%%-include("drotr_names.hrl").
-include("drotr_mnesia.hrl").

%%% ==================================================================
%%% API functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% Returns application modules.
%% @end
%% -------------------------------------------------------------------
-spec modules(App) -> [Mod, ...] when
    App :: atom(),
    Mod :: atom().

modules(App) when is_atom(App) ->
    {ok, L} = application:get_key(App, modules),
    L.

%% -------------------------------------------------------------------
%% @doc
%% Utility for debugging of applications.
%%
%% Usage:
%%
%% ```
%% 1> drotr_dev:debug_app(drotr).
%% ...
%% 2> drotr_dev:debug_app([drotr, jwt]).
%% ...
%% ```
%% @see debug_app/2
%% @see debug/0
%% @see debug/1
%% @see stop_debug/0
%% @end
%% -------------------------------------------------------------------
-spec debug_app(X) -> ok when
    X :: App | [App, ...],
    App :: atom().

debug_app(App) ->
    debug_app(App, []).

%% -------------------------------------------------------------------
%% @doc
%% Utility for debugging of applications.
%%
%% Usage:
%%
%% ```
%% 1> drotr_dev:debug_app(drotr, [return]).
%% ...
%% 2> drotr_dev:debug_app([drotr, jwt], [return]).
%% ...
%% ```
%% @see debug/0
%% @see debug/1
%% @see stop_debug/0
%% @end
%% -------------------------------------------------------------------
-spec debug_app(X, Opts) -> ok when
    X :: App | [App, ...],
    App :: atom(),
    Opts :: [Opt],
    Opt :: return.

debug_app(App, Opts) when is_atom(App) ->
    debug_app([App], Opts);

debug_app(Apps, Opts) when is_list(Apps) ->
    F =
        fun(E, AccIn) ->
            {ok, L} = application:get_key(E, modules),
            L ++ AccIn
        end,
    L = lists:foldl(F, [], Apps),
    io:format("~n[DEBUG] Modules to debugging: ~p~n", [L]),
    debug(L, Opts).

%% -------------------------------------------------------------------
%% @doc
%% Utility for debugging the `drotr` application.
%%
%% Usage:
%%
%% ```
%% 1> drotr_dev:debug().
%% ```
%% @see debug/1
%% @see stop_debug/0
%% @end
%% -------------------------------------------------------------------
-spec debug() -> ok.

debug() ->
    {ok, L} = application:get_key(test_cowboy, modules),
    io:format("~n[DEBUG] Modules to debugging: ~p~n", [L]),
    debug(L).

%% -------------------------------------------------------------------
%% @doc
%% Utility for debugging of modules and/or functions.
%%
%% Usage:
%%
%% ```
%% 1> drotr_dev:debug([drotr_api_v1, drotr_users, drotr_net_ws, drotr_net_http]).
%% ```
%% @see stop_debug/0
%% @end
%% -------------------------------------------------------------------
-spec debug(L) -> ok when
    L :: [E, ...],
    E :: Module | {Module, Function} | {Module, Function, Arity},
    Module :: atom(),
    Function :: atom(),
    Arity :: non_neg_integer().

debug(L) ->
    debug(L, []).

%% -------------------------------------------------------------------
%% @doc
%% Utility for debugging of modules and/or functions.
%%
%% Usage:
%%
%% ```
%% 1> drotr_dev:debug([drotr_api_v1, drotr_users, drotr_net_ws, drotr_net_http], [return]).
%% ```
%% @see stop_debug/0
%% @end
%% -------------------------------------------------------------------
-spec debug(L, Opts) -> ok when
    L :: [E, ...],
    E :: Module | {Module, Function} | {Module, Function, Arity},
    Module :: atom(),
    Function :: atom(),
    Arity :: non_neg_integer(),
    Opts :: [Opt],
    Opt :: return.

debug(L, Opts) ->
    MS =
        case lists:member(return, Opts) of
            true ->
                [{'_', [], [{return_trace}]}];
            false ->
                []
        end,
    F =
        fun
            (M) when is_atom(M) ->
                dbg:tp(M, '_', '_', MS);
            ({M, F}) when is_atom(M), is_atom(F) ->
                dbg:tp(M, F, '_', MS);
            ({M, F, A}) when is_atom(M), is_atom(F), is_integer(A) ->
                dbg:tp(M, F, A, MS);
            (_) ->
                ok
        end,
    dbg:tracer(),
    lists:foreach(F, L),
    dbg:p(all, c),
    ok.

%% -------------------------------------------------------------------
%% @doc
%% ...
%% @end
%% -------------------------------------------------------------------
-spec debug_stop() -> ok.

debug_stop() ->
    dbg:stop_clear().

%% -------------------------------------------------------------------
%% @doc
%% Clear specified Mnesia table.
%% @end
%% -------------------------------------------------------------------
-spec clear_tab(Tab) -> ok | {error, Reason} when
    Tab :: atom(),
    Reason :: term().

clear_tab(Tab) when is_atom(Tab) ->
    case mnesia:clear_table(Tab) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end;

clear_tab([Tab | Tabs]) when is_atom(Tab) ->
    case mnesia:clear_table(Tab) of
        {atomic, ok} ->
            clear_tab(Tabs);
        {aborted, Reason} ->
            {error, Reason}
    end;

clear_tab([]) ->
    ok.

%% -------------------------------------------------------------------
%% @doc
%% Clear all Mnesia tables.
%% @end
%% -------------------------------------------------------------------
-spec clear_all_tabs() -> ok | {error, Reason} when
    Reason :: term().

clear_all_tabs() ->
    clear_tab(?MNESIA_TABS_LIST).

%% -------------------------------------------------------------------
%% @doc
%% Returns count of Mnesia tables.
%% @end
%% -------------------------------------------------------------------
-spec count_tabs() -> non_neg_integer() | {error, Reason} when
    Reason :: mnesia_not_running | term().

count_tabs() ->
    case mnesia:system_info(is_running) of
        yes ->
            erlang:length(mnesia:system_info(local_tables));
        no ->
            {error, mnesia_not_running}
    end.

%% -------------------------------------------------------------------
%% @doc
%% ...
%% @end
%% -------------------------------------------------------------------
-spec tab_records() -> ok | {error, Reason} when
    Reason :: term().

tab_records() ->
    F =
        fun(Tab, AccIn) ->
            [{Tab, mnesia:table_info(Tab, size)} | AccIn]
        end,
    lists:foldl(F, [], ?MNESIA_TABS_LIST).

%% -------------------------------------------------------------------
%% @doc
%% ...
%% @end
%% -------------------------------------------------------------------
-spec tab_words() -> ok | {error, Reason} when
    Reason :: term().

tab_words() ->
    F =
        fun(Tab, AccIn) ->
            [{Tab, mnesia:table_info(Tab, memory)} | AccIn]
        end,
    lists:foldl(F, [], ?MNESIA_TABS_LIST).

%% -------------------------------------------------------------------
%% @doc
%% ...
%% @end
%% -------------------------------------------------------------------
-spec tab_bytes() -> ok | {error, Reason} when
    Reason :: term().

tab_bytes() ->
    N = erlang:system_info({wordsize, external}),
    F =
        fun(Tab, AccIn) ->
            [{Tab, mnesia:table_info(Tab, memory) * N} | AccIn]
        end,
    lists:foldl(F, [], ?MNESIA_TABS_LIST).

%% -------------------------------------------------------------------
%% @doc
%% ...
%% @end
%% -------------------------------------------------------------------
mnesia_nodes() ->
    mnesia:system_info(db_nodes).

%% -------------------------------------------------------------------
%% @doc
%% ...
%% @end
%% -------------------------------------------------------------------
mnesia_dir() ->
    mnesia:system_info(directory).

%% -------------------------------------------------------------------
%% @doc
%% ...
%% @end
%% -------------------------------------------------------------------
-spec sys_info() -> Info when
    Info :: maps:map().

sys_info() ->
    #{
        system_version => erlang:system_info(system_version),
        cpu_topology_used => erlang:system_info({cpu_topology, used}),
        cpu_topology_detected => erlang:system_info({cpu_topology, detected}),
        mnesia_dir => mnesia_dir(),
        mnesia_nodes => mnesia_nodes(),
        mnesia_tab_records => tab_records()
    }.

%%% ==================================================================
%%% Unit tests
%%% ==================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.