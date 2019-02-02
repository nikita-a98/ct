-module(ws_h).

%% API
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
  erlang:start_timer(1000, self(), <<"Start!">>),
  {ok, State}.

websocket_handle({text, <<"\n">>}, State) ->
  {ok, State};
websocket_handle({text, Msg}, State) ->
  {reply, {text, << "Your text: ", Msg/binary >>}, State};
websocket_handle(_Data, State) ->
  {ok, State}.

websocket_info({timeout, _Ref, Msg}, State) ->
  erlang:start_timer(10000, self(), bin_time()),
  {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
  {ok, State}.

%% Internal func
bin_time() ->
  {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(erlang:now()),
  lists:flatten(
    io_lib:format(
      "~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",
      [Year,Month,Day,Hour,Minute,Second]
    )
  ).