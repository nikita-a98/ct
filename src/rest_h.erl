%% Testing Get and Post requests
%% For GET request
%% curl -i -H "Accept: text/plain" -u "Nikita:open sesame" http://localhost:8080/?echo=hihihi
%% curl -i -H "Accept: application/json" -u "Nikita:open sesame" http://localhost:8080/
%%
%% For Post request
%% curl -i -H "content-type: text/plain" -d echo=hihihi -u "Nikita:open sesame" http://localhost:8080
%% curl -i -H "content-type: application/json" -d "{\"v\": \"1\", \"obj\": \"foo\", \"op\": \"create\", \"data\": {\"foo\": \"bar\"}}" -u "Nikita:open sesame" http://localhost:8080
%% curl -i -H "content-type: application/json" -d "{\"v\": \"1\", \"obj\": \foo\"}" -u "Nikita:open sesame" http://localhost:8080
-module(rest_h).
-author("nikita").

%% Standard callbacks.
-export([
    init/2,
    is_authorized/2,
    allowed_methods/2,
    content_types_provided/2,
    content_types_accepted/2
]).

%% Custom callbacks.
-export([
    to_text/2,
    to_json/2,
    from_json/2
]).

%%% ==================================================================
%%% Callback functions
%%% ==================================================================

init(Req, Opts) ->
	io:format("body: ~p~n~n", [Req]),
    {cowboy_rest, Req, Opts}.

is_authorized(Req, State) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, User = <<"Nikita">>, <<"open sesame">>} ->
            {true, Req, User};
        _ ->
            {{false, <<"Basic realm=\"cowboy\"">>}, Req, State}
    end.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"text">>, <<"plain">>, []}, to_text},
        {<<"application/json">>, to_json}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {{<<"text">>, <<"plain">>, []}, to_text},
        {<<"application/json">>, from_json}
    ], Req, State}.


%%% ==================================================================
%%% Custom callback functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% Based on the method calls the function echo and and checks the
%% correctness of the answer
%% @end
%% -------------------------------------------------------------------
to_text(Req, State) ->
    Method = cowboy_req:method(Req),
    io:format("Method: ~n~n~p~n~n", [Method]),
    echo(Method, Req, State).


to_json(Req, State) ->
    io:format("Req2: ~n~n~p~n~n", [Req]),
    Body = #{
        <<"status">> => <<"ok">>,
        <<"data">> =>
        #{
            <<"foo">> => <<"bar">>
        }
    },
    Body1 = jiffy:encode(Body),
    io:format("Body: ~n~n~p~n~n", [Body1]),
    {Body1, Req, State}.

%%
%%echo_to_json(<<"GET">>, Req, State) ->
%%    io:format("Req2: ~n~n~p~n~n", [Req]),
%%    Body = #{
%%        <<"status">> => <<"ok">>,
%%        <<"data">> =>
%%            #{
%%                <<"foo">> => <<"bar">>
%%            }
%%    },
%%    Body1 = jiffy:encode(Body),
%%    io:format("Body: ~n~n~p~n~n", [Body1]),
%%    {Body1, Req, State};
%%
%%echo_to_json(<<"POST">>, Req, State) ->
%%    io:format("Req2: ~n~n~p~n~n", [Req]),
%%    Body = #{
%%        <<"status">> => <<"ok">>,
%%        <<"data">> =>
%%        #{
%%            <<"foo">> => <<"bar">>
%%        }
%%    },
%%    Body1 = jiffy:encode(Body),
%%    io:format("Body: ~n~n~p~n~n", [Body1]),
%%    Resp = cowboy_req:set_resp_body(Body1, Req),
%%    {true, Resp, State}.


from_json(Req0, State) ->
    io:format("Req2: ~n~n~p~n~n", [Req0]),
    {ok, [{Echo, _}], Req} = cowboy_req:read_urlencoded_body(Req0),
    try jiffy:decode(binary_to_list(Echo), [return_maps]) of
        Echo1 ->
            io:format("Echo1: ~n~p~n~n", [Echo1]),
            #{
                <<"v">> := _V,
                <<"obj">> := _Obj,
                <<"op">> := _Op,
                <<"data">> :=
                #{
                    <<"foo">> := _Foo
                }
            } = Echo1,

            Resp = cowboy_req:set_resp_body([<<"5">>], Req),
            io:format("Resp1: ~n~p~n~n", [Resp]),
            {true, Resp, State}
    catch
        _ ->
            Resp = cowboy_req:set_resp_body([], Req),
            io:format("Resp2: ~n~p~n~n", [Resp]),
            {false, Resp, State}
    end.


%%
%%echo_from_json(<<"GET">>, Req, State) ->
%%    io:format("Req2: ~n~p~n~n", [Req]),
%%    #{echo := Echo} = cowboy_req:match_qs([{echo, [], undefined}], Req),
%%    io:format("Echo: ~n~p~n~n", [Echo]),
%%
%%    Echo1 = jiffy:decode(Echo, [return_maps]),
%%    io:format("Echo1: ~n~p~n~n", [Echo1]),
%%    #{
%%        <<"v">> := _V,
%%        <<"obj">> := _Obj,
%%        <<"op">> := _Op,
%%        <<"data">> :=
%%            #{
%%                <<"foo">> := _Foo
%%            }
%%    } = Echo1,
%%
%%    {[], Req, State};
%%
%%echo_from_json(<<"POST">>, Req0, State) ->
%%    io:format("Req2: ~n~n~p~n~n", [Req0]),
%%    {ok, Echo, Req} = cowboy_req:read_urlencoded_body(Req0),
%%
%%    io:format("Echo: ~n~p~n~n", [Echo]),
%%    Echo1 = jiffy:decode(Echo, [return_maps]),
%%    io:format("Echo1: ~n~p~n~n", [Echo1]),
%%    #{
%%        <<"v">> := _V,
%%        <<"obj">> := _Obj,
%%        <<"op">> := _Op,
%%        <<"data">> :=
%%        #{
%%            <<"foo">> := _Foo
%%        }
%%    } = Echo1,
%%
%%    Resp = cowboy_req:set_resp_body([], Req),
%%    {true, Resp, State}.

%%% ==================================================================
%%% Internal functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Sends POST request
%% @end
%% -------------------------------------------------------------------
echo(<<"POST">>, Req0, State) ->
    io:format("Req2: ~n~n~p~n~n", [Req0]),
    {ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
    Echo = proplists:get_value(<<"echo">>, PostVals),
    io:format("Echo: ~n~n~p~n~n", [Echo]),
    Body = [<<"This is a POST ">>|Echo],
    io:format("Body: ~n~n~p~n~n", [Body]),
    Resp = cowboy_req:set_resp_body(Body, Req),
    {true, Resp, State};

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Sends GET request
%% @end
%% -------------------------------------------------------------------
echo(<<"GET">>, Req, State) ->
    io:format("Req2: ~n~n~p~n~n", [Req]),
    #{echo := Echo} = cowboy_req:match_qs([{echo, [], undefined}], Req),
    Body = [<<"This is a GET ">>|Echo],
    io:format("Body: ~n~n~p~n~n", [Body]),
    {Body, Req, State}.
