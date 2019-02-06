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
-export([to_text/2]).

%%curl -i -H "Accept: text/plain" -u "Nikita:open sesame" http://localhost:8080/?echo=hihihi
%%curl -i -H "Accept: text/plain" -d echo=hihihi -u "Nikita:open sesame" http://localhost:8080
%%curl -i -H "Accept: application/x-www-form-urlencoded" -d echo=hihihi -u "Nikita:open sesame" http://localhost:8080
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
        {{<<"text">>, <<"plain">>, []}, to_text}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[
%%        {{<<"text">>, <<"plain">>, []}, to_text}
        {{<<"application">>, <<"x-www-form-urlencoded">>, []}, to_text}
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
    case Method of
        <<"POST">> ->
            echo(<<"POST">>, Req, State);
        <<"GET">> ->
            echo(<<"GET">>, Req, State)
%%            #{echo := Echo} = cowboy_req:match_qs([{echo, [], undefined}], Req),
%%            echo([<<"This is a GET ">>|Echo], Req)
    end.



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
    {ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
    Echo = proplists:get_value(<<"echo">>, PostVals),
    io:format("Echo: ~n~n~p~n~n", [Echo]),
    Body = [<<"This is a POST ">>|Echo],
    io:format("Body: ~n~n~p~n~n", [Body]),
    {Body, Req, State};

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Sends GET request
%% @end
%% -------------------------------------------------------------------
echo(<<"GET">>, Req, State) ->
    #{echo := Echo} = cowboy_req:match_qs([{echo, [], undefined}], Req),
    Body = [<<"This is a GET ">>|Echo],
    io:format("Body: ~n~n~p~n~n", [Body]),
    {Body, Req, State}.
