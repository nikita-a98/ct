%%-module(test_cowboy_SUITE).
%%
%%-compile(export_all).
%%
%%-include_lib("common_test/include/ct.hrl").
%%-include_lib("eunit/include/eunit.hrl").
%%
%%
%%all() ->
%%  [
%%    test_case_get
%%  ].
%%
%%init_per_suite(Config) ->
%%  % действия, выполняемые перед запуском набора тестов
%%  Config.
%%
%%init_per_testcase(_, Config) ->
%%  ServerPid = start_server(),
%%  [{server_pid,ServerPid} | Config].
%%
%%end_per_testcase(_, _Config) ->
%%  stop_server().
%%  % действия, выполняемые после завершения теста
%%
%%end_per_suite(Config) ->
%%  % действия, выполняемые после завершения всего набора тестов
%%  Config.
%%
%%test_case_get(_Config) ->
%%  1=1.
%%%ok.
%%
%%start_server() ->
%%  {ok,ServerPid} = test_cowboy_app:start(1,1),
%%  ServerPid.
%%
%%stop_server() ->
%%  ok = test_cowboy_app:stop(1),
%%  ok.
%%
%%
%%
%%-module(test_cowboy_SUITE).
%%-include_lib("common_test/include/ct.hrl").
%%
%%-export([all/0, init_per_testcase/2, end_per_testcase/2]).
%%-export([ets_tests/1]).
%%
%%all() -> [ets_tests].
%%
%%init_per_testcase(ets_tests, Config) ->
%%  TabId = ets:new(account, [ordered_set, public]),
%%  ets:insert(TabId, {andy, 2131}),
%%  ets:insert(TabId, {david, 12}),
%%  ets:insert(TabId, {steve, 12943752}),
%%  [{table,TabId} | Config].
%%
%%end_per_testcase(ets_tests, Config) ->
%%  ets:delete(?config(table, Config)).
%%
%%ets_tests(Config) ->
%%  TabId = ?config(table, Config),
%%  [{david, 12}] = ets:lookup(TabId, david),
%%  steve = ets:last(TabId),
%%  true = ets:insert(TabId, {zachary, 99}),
%%  zachary = ets:last(TabId).
-module(test_cowboy_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


all() ->
  [
    test_case_get
  ].


init_per_testcase(_, Config) ->
  application:start(test_cowboy_app),
  Config.

end_per_testcase(_, _Config) ->
  application:start(test_cowboy_app),
  ok.
  % действия, выполняемые после завершения теста

test_case_get(_Config) ->
  1=1.
%ok.




