-module(req_api).

%% API
-export([
  req/2,
  req/3,
  req/4
]).

%% API
-export([
  get_resp_code/1,
  get_resp_headers/1,
  get_resp_body/1,
  get_http_resp_body_decoded/1
]).

%%% ==================================================================
%%% Macro
%%% ==================================================================

-define(HTTPC_TIMEOUT, 30000).
-define(HTTPC_CONTENT_TYPE, "application/json").
-define(HTTPC_HTTP_OPTS, [{timeout, ?HTTPC_TIMEOUT}]).
-define(HTTPC_OPTS, [{body_format, binary}]).

%%% ==================================================================
%%% API functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% Wrapper for standard httpc application.
%% @end
%% -------------------------------------------------------------------
-spec req(Method, Url) -> HttpcResult when
  Method :: get,
  Url :: string() | binary(),
  HttpcResult :: {ok, term()} | {error, term()}.

req(get, Url) ->
  req(get, Url, []).

%% -------------------------------------------------------------------
%% @doc
%% Wrapper for standard httpc application.
%% @end
%% -------------------------------------------------------------------
-spec req(Method, Url, Headers) -> HttpcResult when
  Method :: get | post | put | delete,
  Url :: string() | binary(),
  Headers :: [Header],
  Header :: {K, V},
  K :: string() | binary(),
  V :: string() | binary(),
  HttpcResult :: {ok, term()} | {error, term()}.

req(Method, Url, Headers) ->
  Url2 = to_string(Url),
  Headers2 = headers_to_string(Headers),
  Request = {Url2, Headers2},
  httpc:request(Method, Request, ?HTTPC_HTTP_OPTS, ?HTTPC_OPTS).

%% -------------------------------------------------------------------
%% @doc
%% Wrapper for standard httpc application.
%% See http://erlang.org/doc/man/httpc.html#request-5 for more information.
%%
%% Note that default Content-Type is "application/json".
%% @end
%% -------------------------------------------------------------------
-spec req(Method, Url, Headers, Body) -> HttpcResult when
  Method :: get | post | put | delete,
  Url :: string() | binary(),
  Headers :: [Header],
  Header :: {K, V},
  K :: string() | binary(),
  V :: string() | binary(),
  Body :: binary(),
  HttpcResult :: {ok, term()} | {error, term()}.

req(Method, Url, Headers, Body) ->
  Url2 = to_string(Url),
  Headers2 = headers_to_string(Headers),
  ContentType = get_content_type(Headers2),
  Request = {Url2, Headers2, ContentType, Body},
  httpc:request(Method, Request, ?HTTPC_HTTP_OPTS, ?HTTPC_OPTS).

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec get_content_type(Headers) -> term() when
  Headers :: [Header],
  Header :: {K, V},
  K :: string(),
  V :: string().

get_content_type(Headers) ->
  get_content_type(Headers, ?HTTPC_CONTENT_TYPE).

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec get_content_type(Headers, Default) -> ContentType when
  Headers :: [Header],
  Header :: {K, V},
  K :: string(),
  V :: string(),
  Default :: binary() | string(),
  ContentType :: string().

get_content_type([{K, V} | Headers], Default) when is_list(K) ->
  case string:to_lower(K) of
    "content-type" ->
      V;
    _ ->
      get_content_type(Headers, Default)
  end;

get_content_type([], Default) when is_binary(Default) ->
  binary_to_list(Default);

get_content_type([], Default) when is_list(Default) ->
  Default.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec headers_to_string(Headers1) -> Headers2 when
  Headers1 :: [Header1],
  Headers2 :: [Header2],
  Header1 :: {K1, V1},
  Header2 :: {K2, V2},
  K1 :: binary() | string(),
  V1 :: binary() | string(),
  K2 :: string(),
  V2 :: string().

headers_to_string(Headers) ->
  headers_to_string(Headers, []).

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec headers_to_string(Headers1, Acc) -> Headers2 when
  Headers1 :: [Header1],
  Headers2 :: [Header2],
  Header1 :: {K1, V1},
  Header2 :: {K2, V2},
  K1 :: binary() | string(),
  V1 :: binary() | string(),
  K2 :: string(),
  V2 :: string(),
  Acc :: list().

headers_to_string([Header | Headers], Acc) ->
  headers_to_string(Headers, [header_to_string(Header) | Acc]);

headers_to_string([], Acc) ->
  Acc.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
header_to_string({K, V}) ->
  {to_string(K), to_string(V)}.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec to_string(X1) -> X2 when
  X1 :: binary() | string(),
  X2 :: string.

to_string(X) when is_binary(X) ->
  binary_to_list(X);

to_string(X) when is_list(X) ->
  X;

to_string(X) ->
  X.

%% -------------------------------------------------------------------
%% @doc
%% Returns HTTP response code.
%% @end
%% -------------------------------------------------------------------
-spec get_resp_code(HttpcResult :: tuple()) -> Code :: pos_integer().

get_resp_code({{_HttpVer, Code, _ReasonPhrase}, _Headers, _Body}) ->
  Code;

get_resp_code({Code, _Body}) ->
  Code.

%% -------------------------------------------------------------------
%% @doc
%% Returns HTTP response headers.
%% @end
%% -------------------------------------------------------------------
-spec get_resp_headers(HttpcResult :: tuple()) -> Headers :: list().

get_resp_headers({_StatusLine, Headers, _Body}) ->
  Headers;

get_resp_headers({_Code, _Body}) ->
  [].

%% -------------------------------------------------------------------
%% @doc
%% Returns HTTP response header.
%% @end
%% -------------------------------------------------------------------
%%-spec get_resp_header(HeaderName, HttpcResult) -> {HeaderName, Value} when
%%  HeaderName :: binary(),
%%  HttpcResult :: tuple(),
%%  Value :: term().
%%
%%get_resp_header(HeaderName, HttpcResult) ->
%%  Headers = get_resp_headers(HttpcResult),
%%  Value = proplists:get_value(HeaderName, Headers),
%%  {HeaderName, Value}.

%% -------------------------------------------------------------------
%% @doc
%% Returns HTTP response body.
%% @end
%% -------------------------------------------------------------------
-spec get_resp_body(HttpcResult :: tuple()) -> Body :: binary().

get_resp_body({_StatusLine, _Headers, Body}) ->
  Body;

get_resp_body({_Code, Body}) ->
  Body.

%% -------------------------------------------------------------------
%% @doc
%% Returns HTTP response body in map format.
%%
%% @end
%% -------------------------------------------------------------------
-spec get_http_resp_body_decoded(HttpcResult :: tuple()) -> {ok, Map :: maps:map()} | {error, Reason :: invalid_json}.

get_http_resp_body_decoded({_StatusLine, _Headers, Body}) ->
  json_to_map(Body);

get_http_resp_body_decoded({_Code, Body}) ->
  json_to_map(Body).

%%% ==================================================================
%%% Internal functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% Converts binary JSON into map.
%% @end
%% -------------------------------------------------------------------
-spec json_to_map(JsonBin) -> {ok, Map} | {error, Reason} when
  JsonBin :: binary(),
  Map :: maps:map(),
  Reason :: invalid_json.

json_to_map(JsonBin) when is_binary(JsonBin) ->
  case catch jiffy:decode(JsonBin, [return_maps]) of
    Map when is_map(Map) ->
      {ok, Map};
    _ ->
      {error, invalid_json}
  end;

json_to_map(_) ->
  {error, invalid_json}.