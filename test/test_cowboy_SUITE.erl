-module(test_cowboy_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


all() ->
  [
    echo_get,
    echo_post
%%    echo_ws
  ].

suite() ->
  [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(test_cowboy),
  {ok, _} = application:ensure_all_started(crypto),
  {ok, _} = application:ensure_all_started(public_key),
  {ok, _} = application:ensure_all_started(ssl),
  {ok, _} = application:ensure_all_started(inets),
%%  Rel = "/home/nikita/IdeaProjects/ct/_rel/test_cowboy_release/bin/test_cowboy_release",
%%  os:cmd(Rel ++ " start"),
  Config.

end_per_suite(_Config) ->
  ok = application:stop(test_cowboy),
  ok = application:stop(crypto),
  ok = application:stop(public_key),
  ok = application:stop(ssl),
  ok = application:stop(inets),
  Rel = "/home/nikita/IdeaProjects/ct/_rel/test_cowboy_release/bin/test_cowboy_release",
  os:cmd(Rel ++ " stop"),
  ok.

echo_get(_Config) ->
  Url = "http://localhost:8080/?echo=hihihi",
  {ok, R} = req_api:req(get, Url),
  200 = req_api:get_resp_code(R),
  <<"This is a GET hihihi">> = req_api:get_resp_body(R).

echo_post(_Config) ->
  Url = "http://localhost:8080",
  Body = "echo=hihihi",
  {ok, R} = req_api:req(post, Url, [], Body),
  200 = req_api:get_resp_code(R),
  <<"This is a POST hihihi">> = req_api:get_resp_body(R).

echo_ws(_Config) ->
  ok.









