%% Testing Get and Post requests
%% For GET request
%% curl -i -H "Accept: text/plain" -u "Nikita:open sesame" http://localhost:8080/?echo=hihihi
%%get_to_json curl -i -H "Accept: application/json" -u "Nikita:open sesame" http://localhost:8080/foo
%%post to_json curl -i -H "content-type: application/json" -u "Nikita:open sesame" http://localhost:8080/foo

%% For Post request
%% curl -i -H "content-type: text/plain" -d echo=hihihi -u "Nikita:open sesame" http://localhost:8080
%% curl -i -H "content-type: application/json" -d <<"{"v": "1", "obj": "foo", "op": "create", "data": {"foo": "bar"}}">> -u "Nikita:open sesame" http://localhost:8080

-module(rest_h1).
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
    to_json/2
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
        {<<"application/json">>, to_json}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, to_json}
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
to_json(Req, State) ->
    Method = cowboy_req:method(Req),
    echo_to_json(Method, Req, State).



%%% ==================================================================
%%% Internal functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Sends POST request
%% @end
%% -------------------------------------------------------------------
echo_to_json(<<"GET">>, Req, State) ->
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
    {Body1, Req, State};

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Sends GET request
%% @end
%% -------------------------------------------------------------------
echo_to_json(<<"POST">>, Req, State) ->
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
    Resp = cowboy_req:set_resp_body(Body1, Req),
    {true, Resp, State}.
