-module(test_cowboy).

-export([init/2]).


init(Req, Opts) ->
	Method = cowboy_req:method(Req),
	Req1 = method(Method, Req, Opts),
	io:format("body: ~p~n~n", [Req1]),
	{ok, Req1, Opts}.

method(<<"POST">>, Req, _Opts) ->
	HasBody = cowboy_req:has_body(Req),
	echo(<<"POST">>, HasBody, Req);
method(<<"GET">>, Req, _Opts) ->
	#{echo := Echo} = cowboy_req:match_qs([{echo, [], undefined}], Req),
	echo([<<"This is a GET ">>|Echo], Req).



echo(<<"POST">>, true, Req0) ->
	{ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
	Echo = proplists:get_value(<<"echo">>, PostVals),
	cowboy_req:reply(
		200,
		#{<<"content-type">> => <<"text/plain; charset=utf-8">>},
		[<<"This is a POST ">>|Echo],
		Req
	);
echo(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req);
echo(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).

echo(undefined, Req) ->
	cowboy_req:reply(400, #{}, <<"Missing echo parameter.">>, Req);
echo(Echo, Req) ->
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain; charset=utf-8">>
	}, Echo, Req).