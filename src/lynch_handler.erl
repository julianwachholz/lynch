-module(lynch_handler).
-compile([export_all]).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3, handle/2, terminate/3]).
-export([
    websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3
]).


init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.


handle(_Req, State) ->
    {ok, Req} = cowboy_http_req:reply(404, [{'Content-Type', <<"text/html">>}]),
    {ok, Req, State}.


websocket_init(_Type, Req, _Opts) ->
    {ok, Req, undefined_state, 60000}.


websocket_handle({text, <<"PING">>}, Req, State) ->
    {ok, Req, State};

websocket_handle({text, Text}, Req, State) ->
    Json = jiffy:decode(Text, [return_maps]),
    case maps:find(<<"echo">>, Json) of
        {ok, Echo} -> {reply, {text, jiffy:encode(#{echo => Echo})}, Req, State, hibernate};
        _ -> {ok, Req, State, hibernate}
    end;

websocket_handle(_Frame, Req, State) ->
    {ok, Req, State}.


websocket_info(Info, Req, State) ->
    lager:debug("~p (~p) websocket info: ~p", [self(), State, Info]),
    {ok, Req, State, hibernate}.


websocket_terminate(Reason, _Req, State) ->
    lager:debug("~p (~p) websocket terminate: ~p", [self(), State, Reason]),
    ok.


terminate(_Reason, _Req, _State) ->
    ok.
