-module(lynch_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3, handle/2, terminate/3]).
-export([
    websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3
]).


init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.


handle(Req, State) ->
    lager:debug("Request not expected: ~p", [Req]),
    {ok, Req2} = cowboy_http_req:reply(404, [{'Content-Type', <<"text/html">>}]),
    {ok, Req2, State}.


websocket_init(_Type, Req, _Opts) ->
    lager:debug("init websocket"),
    {ok, Req, undefined_state, 60000}.


websocket_handle({text, Text}, Req, State) ->
    lager:debug("Got data: ~p", [Text]),
    {reply, {text, [{echo, <<Text/binary>>}]}, Req, State, hibernate};

websocket_handle(_Frame, Req, State) ->
    % {ok, Req, State}.
    {reply, {text, <<"whut?">>}, Req, State, hibernate}.


websocket_info({timeout, _Ref, Text}, Req, State) ->
    lager:debug("websocket timeout: ~p", [Text]),
    {reply, {text, Text}, Req, State};

websocket_info({log, Text}, Req, State) ->
    {reply, {text, Text}, Req, State};

websocket_info(_Info, Req, State) ->
    lager:debug("websocket info"),
    {ok, Req, State, hibernate}.


websocket_terminate(_Reason, _Req, _State) ->
    ok.


terminate(_Reason, _Req, _State) ->
    ok.
