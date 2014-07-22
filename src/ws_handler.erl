-module(ws_handler).

-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3, handle/2, terminate/3]).
-export([
    websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3
]).

-define(JSON_OK, jiffy:encode(#{status => ok})).


init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.


handle(_Req, State) ->
    {ok, Req} = cowboy_http_req:reply(404, [{'Content-Type', <<"text/html">>}]),
    {ok, Req, State}.


websocket_init(_Type, Req, _Opts) ->
    lager:debug("~p init websocket", [self()]),
    game_lobby:join(self()),
    {ok, Req, undefined_state}.


websocket_handle({text, <<"PING">>}, Req, State) ->
    {ok, Req, State};

websocket_handle({text, Text}, Req, State) ->
    Json = jiffy:decode(Text, [return_maps]),
    case maps:find(<<"message">>, Json) of
        {ok, Message} ->
            game_lobby:message(self(), Message),
            {reply, {text, ?JSON_OK}, Req, State, hibernate};
        _ ->
            {ok, Req, State, hibernate}
    end;

websocket_handle(_Frame, Req, State) ->
    {ok, Req, State, hibernate}.


websocket_info({message, From, Message}, Req, State) ->
    lager:debug("~p got message from ~p: ~p", [self(), From, Message]),
    Sender = list_to_binary(pid_to_list(From)),
    Reply = jiffy:encode(#{message => Message, from => Sender}),
    {reply, {text, Reply}, Req, State, hibernate};

websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.


websocket_terminate(Reason, _Req, State) ->
    lager:debug("~p (~p) websocket terminate: ~p", [self(), State, Reason]),
    game_lobby:leave(self()),
    ok.


terminate(_Reason, _Req, _State) ->
    ok.
