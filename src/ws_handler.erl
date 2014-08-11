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
    lager:debug("[~s] init: ~p", [?MODULE, self()]),
    {ok, Req, undefined_state, 60000}.


websocket_handle({text, Text}, Req, State) ->
    Data = jiffy:decode(Text, [return_maps]),
    handle_req(Data, Req, State);

websocket_handle(Frame, Req, State) ->
    lager:info("[~s] Unexpected handle: ~p", [?MODULE, Frame]),
    {ok, Req, State, hibernate}.


websocket_info({state, NewState}, Req, _State) ->
    lager:debug("[~s] set state ~p: ~p", [?MODULE, self(), maps:to_list(NewState)]),
    {ok, Req, NewState, hibernate};

websocket_info({reply, Data}, Req, State) ->
    Json = jiffy:encode(Data),
    lager:debug("[~s] reply to ~p: ~p", [?MODULE, self(), Json]),
    {reply, {text, Json}, Req, State, hibernate};

websocket_info(Info, Req, State) ->
    lager:info("[~s] Unexpected info: ~p", [?MODULE, Info]),
    {ok, Req, State, hibernate}.


websocket_terminate(Reason, _Req, State) ->
    lager:debug("[~s] terminate ~p: ~p", [?MODULE, self(), Reason]),
    game_master:route(self(), State, terminate),
    ok.


terminate(_Reason, _Req, _State) ->
    ok.


handle_req(#{ <<"type">> := <<"PING">>, <<"ping">> := Ping }, Req, State) ->
    Reply = jiffy:encode(#{type => <<"PONG">>, pong => Ping}),
    {reply, {text, Reply}, Req, State, hibernate};

handle_req(Data, Req, State) ->
    game_master:route(self(), State, Data),
    {reply, {text, ?JSON_OK}, Req, State, hibernate}.
