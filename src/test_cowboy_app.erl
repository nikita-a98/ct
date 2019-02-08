-module(test_cowboy_app).
-behaviour(application).

%% Callbacks
-export([
    start/2,
    stop/1
]).

%%% ==================================================================
%%% Callback functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec start(_, _) -> {ok, pid()}.

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", rest_h, []},
            {"/websocket", ws_h, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        http,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    test_cowboy_sup:start_link().

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec stop(_) -> ok.

stop(_State) ->
    ok.
