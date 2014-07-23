-module(game_master).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([route/3, connect/2, disconnect/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% game registry
-export([register_name/2, unregister_name/1, whereis_name/1, send/2]).


-define(SERVER, ?MODULE).


%%% API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


route(PlayerPid, PlayerState, Data) ->
    gen_server:cast(?SERVER, {route, PlayerPid, PlayerState, Data}).


connect(PlayerPid, Game) ->
    gen_server:cast(?SERVER, {connect, PlayerPid, Game}).


disconnect(PlayerPid) ->
    gen_server:cast(?SERVER, {disconnect, PlayerPid}).


%% Simple process registry
register_name(Name, Pid) ->
    gen_server:call(?SERVER, {register, Name, Pid}).


unregister_name(Name) ->
    gen_server:cast(?SERVER, {unregister, Name}).


whereis_name(Name) ->
    gen_server:call(?SERVER, {whereis, Name}).


send(Name, Msg) ->
    case whereis_name(Name) of
        undefined -> exit({badarg, {Name, Msg}});
        Pid -> Pid ! Msg
    end,
    Pid.


%%% gen_server

init([]) ->
    State = #{ servers => #{},
               players => #{} },
    {ok, State}.


handle_call({register, Name, Pid}, _From, #{ servers := Servers } = State) ->
    case maps:is_key(Name, Servers) of
        false ->
            NewServers = maps:put(Name, Pid, Servers),
            {reply, yes, State#{ servers := NewServers } };
        true ->
            {reply, no, State}
    end;

handle_call({whereis, Name}, _From, #{ servers := Servers } = State) ->
    case maps:find(Name, Servers) of
        {ok, Pid} ->
            {reply, Pid, State};
        error ->
            {reply, undefined, State}
    end;

handle_call(Msg, _From, State) ->
    lager:info("[~s] Unexpected call: ~p", [?MODULE, Msg]),
    {noreply, State}.


handle_cast({route, PlayerPid, PlayerState, Data}, #{ players := Players } = State) ->
    case maps:find(PlayerPid, Players) of
        {ok, Game} ->
            game_action:player_action(Game, PlayerPid, PlayerState, Data);
        error ->
            game_action:player_new(PlayerPid, PlayerState, Data)
    end,
    {noreply, State};

handle_cast({connect, PlayerPid, Game}, #{ players := Players} = State) ->
    %%% TODO connect player with game
    {noreply, State};

handle_cast({disconnect, PlayerPid}, #{ players := Players } = State) ->
    %%% TODO disconnect player from game
    {noreply, State};

handle_cast({unregister, Name}, #{ servers := Servers } = State) ->
    NewServers = maps:remove(Name, Servers),
    {noreply, State#{ servers := NewServers } };

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
