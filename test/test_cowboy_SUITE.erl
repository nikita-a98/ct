-module(test_cowboy_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
  [
    test_case_get
  ].

init_per_suite(Config) ->
  % действия, выполняемые перед запуском набора тестов
  Config.

init_per_testcase(_, Config) ->
  % действия, выполняемые перед запуском теста
  Config.

end_per_testcase(_, Config) ->
  % действия, выполняемые после завершения теста
  Config.

end_per_suite(Config) ->
  % действия, выполняемые после завершения всего набора тестов
  Config.

test_case_get(_Config) ->
  os:cmd("curl -i \"http://localhost:8080/?echo=hihi\""),
ok.