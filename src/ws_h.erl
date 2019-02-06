-module(ws_h).

%% Callbacks
-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    bin_time/0
]).

%%% ==================================================================
%%% Callback functions
%%% ==================================================================

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
    erlang:start_timer(1000, self(), <<"Start!">>),
    {ok, State}.

websocket_handle({text, <<"\n">>}, State) ->
    {ok, State};
websocket_handle({text, Msg}, State) ->
    {reply, {text, <<"Your text: ", Msg/binary>>}, State};
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({timeout, _Ref, Msg}, State) ->
    erlang:start_timer(10000, self(), bin_time()),
    {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
    {ok, State}.


%%% ==================================================================
%%% Internal functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Returns the current time in the format binary
%% @end
%% -------------------------------------------------------------------
bin_time() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(erlang:now()),
    lists:flatten(
        io_lib:format(
            "~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",
            [Year, Month, Day, Hour, Minute, Second]
        )
    ).