-module(ws_h1).

%% Callbacks
-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2
]).

%%% ==================================================================
%%% Callback functions
%%% ==================================================================

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, <<"\n">>}, State) ->
    {ok, State};
websocket_handle({text, Msg}, State) ->
    io:format("Msg: ~n~n~p~n~n", [Msg]),
    try jiffy:decode(binary_to_list(Msg), [return_maps]) of
        Msg1 ->
            io:format("Msg1: ~n~p~n~n", [Msg1]),
            #{
                <<"v">> := _V,
                <<"obj">> := _Obj,
                <<"op">> := _Op,
                <<"data">> :=
                #{
                    <<"foo">> := _Foo
                }
            } = Msg1,

            Body = #{
                <<"status">> => <<"ok">>,
                <<"data">> =>
                #{
                    <<"foo">> => <<"bar">>
                }
            },
            Body1 = jiffy:encode(Body),
            io:format("Body1: ~n~p~n~n", [Body1]),
            {reply, {text, <<Body1/binary>>}, State}
    catch
        _ ->
            Body = jiffy:encode(#{<<"status">> => <<"error">>}),
            {reply, {text, <<Body/binary>>}, State}
    end;
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info(_Info, State) ->
    {ok, State}.


%%% ==================================================================
%%% Internal functions
%%% ==================================================================

