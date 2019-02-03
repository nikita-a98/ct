-module(test_cowboy_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


all() ->
  [
    echo_get
  ].


init_per_testcase(_, Config) ->
  application:start(test_cowboy_app),
%%  application:start(inets),
%%  application:start(ssl),
%%  application:start(cowboy),
  Config.

end_per_testcase(_, _Config) ->
  application:stop(test_cowboy_app),
  ok.

%%echo_get(_Config) ->
%%  {ok,{{_,200,_},_,_}} = httpc:request("http://localhost:8080/?echo=saymyname").


echo_get(Config) ->
  do_echo_get(tcp, http, Config).

do_echo_get(Transport, Protocol, Config) ->
  {200, _, <<"this is fun">>} = do_get(Transport, Protocol, "/?echo=this+is+fun", [], Config),
  {400, _, _} = do_get(Transport, Protocol, "/", [], Config),
  ok.

do_get(Transport, Protocol, Path, ReqHeaders, Config) ->
  Port = case Transport of
           tcp -> 8080;
           ssl -> 8443
         end,
  ConnPid = gun_open([{port, Port}, {type, Transport}, {protocol, Protocol}|Config]),

  Ref = gun:get(ConnPid, Path, ReqHeaders),
  case gun:await(ConnPid, Ref) of
    {response, nofin, Status, RespHeaders} ->
      {ok, Body} = gun:await_body(ConnPid, Ref),
      {Status, RespHeaders, Body};
    {response, fin, Status, RespHeaders} ->
      {Status, RespHeaders, <<>>}
  end.

gun_open(Config) ->
  gun_open(Config, #{}).

gun_open(Config, Opts) ->
  {ok, ConnPid} = gun:open("localhost", config(port, Config), Opts#{
    retry => 0,
    transport => config(type, Config),
    transport_opts => proplists:get_value(transport_opts, Config, []),
    protocols => [config(protocol, Config)]
  }),
  ConnPid.

config(Key, Config) ->
  {_, Value} = lists:keyfind(Key, 1, Config),
  Value.

