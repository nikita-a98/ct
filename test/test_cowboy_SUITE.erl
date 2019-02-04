-module(test_cowboy_SUITE).

-compile(export_all).
-import(ct_helper, [config/2]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").



all() ->
  [
    {group,req}
  ].


suite() ->
  [{timetrap,{minutes,1}}].

init_per_suite(Config) ->
  _NewConfig = [{module, test_cowboy}|Config].

end_per_suite(_) ->
  ok.


groups() ->
  [{_GroupName = req, _Opt = [parallel], _TestCases = [echo_get, echo_post, echo_ws]}].

init_per_group(_GroupName = req, Config) ->
  {ok, _} = application:ensure_all_started(test_cowboy),
  {ok, _} = application:ensure_all_started(inets),
  Config.

end_per_group(_GroupName = req, Config) ->
  ok = application:stop(test_cowboy),
  ok = application:stop(crypto),
  ok = application:stop(public_key),
  ok = application:stop(ssl),
  ok = application:stop(inets),
  {ok, Cwd} = file:get_cwd(),
  Module = atom_to_list(config(module, Config)),
  Rel = Cwd ++ "/_rel/" ++ Module ++ "_release/bin/" ++ Module ++ "_release",
  os:cmd(Rel ++ " stop"),
  ok.

init_per_testcase(echo_ws, Config) ->
  {ok, _} = application:ensure_all_started(gun),
  Config;
init_per_testcase(echo_post, Config) ->
  Config;
init_per_testcase(echo_get, Config) ->
  Config.

end_per_testcase(echo_ws, _Config) ->
  ok = application:stop(gun);
end_per_testcase(echo_post, _Config) ->
  ok;
end_per_testcase(echo_get, _Config) ->
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
  {ok, Pid} = gun:open(
    "127.0.0.1",
    8080,
    #{protocols =>[http], retry => 0}
  ),
  {ok, http} = gun:await_up(Pid),
  _ = monitor(process, Pid),
  StreamRef = gun:ws_upgrade(Pid, "/websocket", [], #{compress => true}),
  receive
    {gun_upgrade, Pid, StreamRef, _, _} ->
      ok;
    Msg1 ->
      exit({connection_failed, Msg1})
  end,

  %% Check that we receive the message sent on timer on init.
  receive
    {gun_ws, Pid, StreamRef, {text, <<"Start!">>}} ->
      ok
  after 2000 ->
    exit(timeout)
  end,

  %% Check that we receive subsequent messages sent on timer.
  receive
    {gun_ws, Pid, StreamRef, {text, _}} ->
      ok
  after 20000 ->
    exit(timeout)
  end,

  %% Check that we receive the echoed message.
  gun:ws_send(Pid, {text, <<"hello">>}),
  receive
    {gun_ws, Pid, StreamRef, {text, <<"Your text: hello">>}} ->
      ok
  after 500 ->
    exit(timeout)
  end,
  gun:ws_send(Pid, close),

  ok.









