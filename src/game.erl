-module(game).
-behaviour(gen_fsm).

%% API
-export([start_link/1]).
-export([join/3, rename/3, talk/3]).

%% gen_fsm
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).
-export([lobby/2, lobby/3]).


%%% API

start_link(Name) ->
    gen_fsm:start_link({global, Name}, ?MODULE, [Name], []).


join(PlayerPid, PlayerState, Data) ->
    gen_fsm:send_event(PlayerPid, {join, PlayerPid, PlayerState, Data}).


rename(PlayerPid, PlayerState, Data) ->
    gen_fsm:send_event(PlayerPid, {rename, PlayerPid, PlayerState, Data}).


talk(PlayerPid, PlayerState, Data) ->
    gen_fsm:send_event(PlayerPid, {talk, PlayerPid, PlayerState, Data}).


%%% gen_fsm

init(Name) ->
    State = #{ name => Name,
               players => [], },
    {ok, lobby, State}.

lobby({join, PlayerPid, PlayerState, Data}, State) ->
    Players = [PlayerPid | maps:get(players, State)],
    NewState = State#{ players := Players },
    {ok, NewState};

lobby(_Event, State) ->
    {next_state, lobby, State}.


lobby(_Event, _From, State) ->
    {next_state, lobby, State}.
