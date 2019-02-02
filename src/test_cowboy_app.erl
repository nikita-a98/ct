-module(test_cowboy_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", test_cowboy, []},
			{"/websocket", ws_h, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(
		http,
		[{port, 8080}],
		#{env => #{dispatch => Dispatch}}
	),
	test_cowboy_sup:start_link().

stop(_State) ->
	ok.
