-module(test_cowboy_SUITE).

-compile(export_all).

%%% ==================================================================
%%% Imports
%%% ==================================================================
-import(ct_helper, [config/2]).

%%% ==================================================================
%%% Includes
%%% ==================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


%%% ==================================================================
%%% Common test callback functions
%%% ==================================================================

%%%-------------------------------------------------------------------
%%% Function: all() -> GroupsAndTestCases
%%%
%%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%%% GroupName = atom()
%%%   Name of a test case group.
%%% TestCase = atom()
%%%   Name of a test case.
%%%
%%% Description: Returns the list of groups and test cases that
%%%              are to be executed.
%%%-------------------------------------------------------------------
all() ->
    [
        {group, req}
    ].

%%%-------------------------------------------------------------------
%%% Function: suite() -> Info
%%%
%%% Info = [tuple()]
%%%   List of key/value pairs.
%%%
%%% Description: Returns list of tuples to set default properties
%%%              for the suite.
%%%-------------------------------------------------------------------
suite() ->
    [{timetrap, {minutes, 1}}].

%%%-------------------------------------------------------------------
%%% Function: init_per_suite(Config0) -> Config1
%%%
%% Config0 = Config1 = [tuple()]
%%%   A list of key/value pairs, holding the test case configuration.
%%%
%%% Description: Initialization before the suite.
%%%-------------------------------------------------------------------
init_per_suite(Config) ->
    _NewConfig = [{module, test_cowboy} | Config].

%%%-------------------------------------------------------------------
%%% Function: end_per_suite(Config) -> term()
%%%
%%% Config = [tuple()]
%%%   A list of key/value pairs, holding the test case configuration.
%%%
%%% Description: Cleanup after the suite.
%%%-------------------------------------------------------------------
end_per_suite(_) ->
    ok.

%%%-------------------------------------------------------------------
%%% Function: groups() -> [Group]
%%%
%%% Group = {GroupName,Properties,GroupsAndTestCases}
%%% GroupName = atom()
%%%   The name of the group.
%%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%%   Group properties that may be combined.
%%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%%% TestCase = atom()
%%%   The name of a test case.
%%% Shuffle = shuffle | {shuffle,Seed}
%%%   To get cases executed in random order.
%%% Seed = {integer(),integer(),integer()}
%%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%%              repeat_until_any_ok | repeat_until_any_fail
%%%   To get execution of cases repeated.
%%% N = integer() | forever
%%%
%%% Description: Returns a list of test case group definitions.
%%%-------------------------------------------------------------------
groups() ->
    [{_GroupName = req, _Opt = [parallel], _TestCases = [echo_get, echo_post, echo_ws]}].

%%%-------------------------------------------------------------------
%%% Function: init_per_group(GroupName, Config0) ->
%%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%%
%%% GroupName = atom()
%%%   Name of the test case group that is about to run.
%%% Config0 = Config1 = [tuple()]
%%%   A list of key/value pairs, holding configuration data for the group.
%%% Reason = term()
%%%   The reason for skipping all test cases and subgroups in the group.
%%%
%%% Description: Initialization before each test case group.
%%%-------------------------------------------------------------------
init_per_group(_GroupName = req, Config) ->
    ?assertMatch({ok, _}, application:ensure_all_started(test_cowboy)),
    ?assertMatch({ok, _}, application:ensure_all_started(inets)),
    Config.

%%%--------------------------------------------------------------------
%%% Function: end_per_testcase(TestCase, Config) -> term()
%%%
%%% TestCase = atom()
%%%   Name of the test case that is finished.
%%% Config = [tuple()]
%%%   A list of key/value pairs, holding the test case configuration.
%%%
%%% Description: Cleanup after each test case.
%%%--------------------------------------------------------------------
end_per_group(_GroupName = req, Config) ->
    ?assertMatch(ok, application:stop(test_cowboy)),
    ?assertMatch(ok, application:stop(crypto)),
    ?assertMatch(ok, application:stop(public_key)),
    ?assertMatch(ok, application:stop(ssl)),
    ?assertMatch(ok, application:stop(inets)),
    {ok, Cwd} = file:get_cwd(),
    Module = atom_to_list(config(module, Config)),
    Rel = Cwd ++ "/_rel/" ++ Module ++ "_release/bin/" ++ Module ++ "_release",
%%    Log = Cwd ++ "/_rel/" ++ Module ++ "_release/log/erlang.log.1",
    %%os:cmd(Rel ++ " stop"),
    ct:log("~s~n", [os:cmd(Rel ++ " stop")]),
    ok.

%%%--------------------------------------------------------------------
%%% Function: init_per_testcase(TestCase, Config0) ->
%%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%%
%%% TestCase = atom()
%%%   Name of the test case that is about to run.
%%% Config0 = Config1 = [tuple()]
%%%   A list of key/value pairs, holding the test case configuration.
%%% Reason = term()
%%%   The reason for skipping the test case.
%%%
%%% Description: Initialization before each test case.
%%%--------------------------------------------------------------------
init_per_testcase(echo_ws, Config) ->
    ?assertMatch({ok, _}, application:ensure_all_started(gun)),
    Config;
init_per_testcase(echo_post, Config) ->
    Config;
init_per_testcase(echo_get, Config) ->
    Config.

%%%--------------------------------------------------------------------
%%% Function: end_per_testcase(TestCase, Config0) ->
%%%               term() | {save_config,Config1} | {fail,Reason}
%%%
%%% TestCase = atom()
%%%   Name of the test case that is finished.
%%% Config0 = Config1 = [tuple()]
%%%   A list of key/value pairs, holding the test case configuration.
%%% Reason = term()
%%%   The reason for failing the test case.
%%%
%%% Description: Cleanup after each test case.
%%%--------------------------------------------------------------------
end_per_testcase(echo_ws, _Config) ->
    ?assertMatch(ok, application:stop(gun));
end_per_testcase(echo_post, _Config) ->
    ok;
end_per_testcase(echo_get, _Config) ->
    ok.

%%% ==================================================================
%%% Test cases
%%% ==================================================================

%%%-------------------------------------------------------------------
%%% Test get request
%%%-------------------------------------------------------------------
echo_get(_Config) ->
    Url = "http://localhost:8080/?echo=hihihi",

    Auth = base64:encode(<<"Nikita:open sesame">>),
    Header =
        [{<<"authorization">>, iolist_to_binary([<<"Basic ">>, Auth])}],

    {ok, R} = req_api:req(get, Url, Header),
    ct:pal("GET request ~n~p~n", [R]),
    ?assertMatch(200, req_api:get_resp_code(R)),
    ?assertMatch(<<"This is a GET hihihi">>, req_api:get_resp_body(R)).

%%%-------------------------------------------------------------------
%%% Test post request
%%%-------------------------------------------------------------------
echo_post(_Config) ->
    Url = "http://localhost:8080",

    Auth = base64:encode(<<"Nikita:open sesame">>),
    Header =
        [
            {<<"authorization">>, iolist_to_binary([<<"Basic ">>, Auth])},
            {<<"content-type">>, <<"text/plain">>}
        ],
    Body = <<"echo=hihihi">>,
    {ok, R} = req_api:req(post, Url, Header, Body),
    ct:pal("POST request ~n~p~n", [R]),
    ?assertMatch(200, req_api:get_resp_code(R)),
    ?assertMatch(<<"This is a POST hihihi">>, req_api:get_resp_body(R)).

%%%-------------------------------------------------------------------
%%% Test websocket request
%%%-------------------------------------------------------------------
echo_ws(_Config) ->
    {ok, Pid} = gun:open(
        "127.0.0.1",
        8080,
        #{protocols =>[http], retry => 0}
    ),
    ?assertMatch({ok, http}, gun:await_up(Pid)),
    _ = monitor(process, Pid),
    StreamRef = gun:ws_upgrade(Pid, "/websocket", [], #{compress => true}),
    ct:pal("WS request StreamRef: ~n~p~n", [StreamRef]),
    receive
        {gun_upgrade, Pid, StreamRef, _, _} ->
            ok;
        Msg1 ->
            exit({connection_failed, Msg1})
    end,

    %% Check that we receive the message sent on timer on init.
    R1 = receive
        {gun_ws, Pid, StreamRef, {text, <<"Start!">>}} ->
            ok
    after 2000 ->
        exit(timeout)
    end,
    ct:pal("WS request Check that we receive the message sent on timer on init: ~n~p~n", [R1]),

    %% Check that we receive subsequent messages sent on timer.
    R2 = receive
        {gun_ws, Pid, StreamRef, {text, _}} ->
            ok
    after 20000 ->
        exit(timeout)
    end,
    ct:pal("WS request Check that we receive subsequent messages sent on timer: ~n~p~n", [R2]),

    %% Check that we receive the echoed message.
    R3 = gun:ws_send(Pid, {text, <<"hello">>}),
    R4 =  receive
        {gun_ws, Pid, StreamRef, {text, <<"Your text: hello">>}} ->
            ok
    after 500 ->
        exit(timeout)
    end,
    ct:pal("WS request Check that we receive the echoed message: ~n Send ~p~n Receive~p~n", [R3,R4]),
    gun:ws_send(Pid, close),
    ok.






