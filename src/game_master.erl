-module(game_master).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([player_route/3]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


%%% API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


player_route(PlayerPid, PlayerState, Data) ->
    gen_server:cast(?SERVER, {route, PlayerPid, PlayerState, Data}).


%%% gen_server

init([]) ->
    State = #{ servers => [] },
    {ok, State}.


handle_call(Msg, _From, State) ->
    lager:info("[~s] Unexpected call: ~p", [?MODULE, Msg]),
    {noreply, State}.


handle_cast({route, PlayerPid, PlayerState, Data}, State) ->
    %% TODO
    {noreply, State};

handle_cast(Msg, State) ->
    lager:info("[~s] Unexpected cast: ~p", [?MODULE, Msg]),
    {noreply, State}.


handle_info(Msg, State) ->
    lager:info("[~s] Unexpected info: ~p", [?MODULE, Msg]),
    {noreply, State}.


terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
