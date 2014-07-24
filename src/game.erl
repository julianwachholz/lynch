-module(game).
-behaviour(gen_fsm).

%% API
-export([start/1]).
-export([host/3, join/3, rename/3, chat/3]).

%% gen_fsm
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).
-export([lobby/2, lobby/3]).


%%% API

start(Name) ->
    gen_fsm:start({via, game_master, Name}, ?MODULE, [Name], []).


host(GameName, Player, PlayerName) ->
    lager:debug("Host new game ~p by ~p", [GameName, PlayerName]),
    {ok, Pid} = start(GameName),
    lager:debug("New game ~p: ~p", [GameName, Pid]),
    join(GameName, Player, PlayerName).


join(GameName, Player, PlayerName) ->
    lager:debug("Player ~p joins ~p", [PlayerName, GameName]),
    Pid = game_master:whereis_name(GameName),
    game_master:connect(Player, Pid),
    gen_fsm:send_event(Pid, {join, Player, PlayerName}).


%% unused
rename(Player, PlayerState, Data) ->
    gen_fsm:send_event(Player, {rename, Player, PlayerState, Data}).


%% unused
chat(Player, PlayerState, Data) ->
    gen_fsm:send_all_state_event(Player, {chat, Player, PlayerState, Data}).


%%% gen_fsm

init(Name) ->
    State = #{ name => Name,
               players => [] },
    {ok, lobby, State}.


lobby({join, Player, PlayerName}, State) ->
    Players = [Player | maps:get(players, State)],
    lager:info("Game ~p joins: ~p", [maps:get(name, State), PlayerName]),
    Player ! {state, #{ name => PlayerName } },
    NewState = State#{ players := Players },
    {next_state, lobby, NewState};

lobby(_Event, State) ->
    {next_state, lobby, State}.


lobby(_Event, _From, State) ->
    {next_state, lobby, State}.


% All state events

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.


handle_sync_event(_Event, _From, StateName, State) ->
    {next_state, StateName, State}.


handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.


terminate(normal, _StateName, _State) ->
    ok.


code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
