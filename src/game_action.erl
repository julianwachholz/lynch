-module(game_action).
-behaviour(gen_server).

-export([start_link/0]).
-export([player_new/3, player_action/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


%%% API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


player_new(PlayerPid, PlayerState, Data) ->
    gen_server:cast(?SERVER, {player_new, PlayerPid, PlayerState, Data}).

%% Game, Player :: pid()
%% State, Data :: maps()
player_action(Game, Player, State, Data) ->
    lager:debug("Unexpected player_action [~p] ~p (~p): ~p", [Game, Player, maps:to_list(State), maps:to_list(Data)]),
    ok.


%%% gen_server

init([]) ->
    {ok, no_state}.


handle_cast({player_new, Player, undefined_state, Data}, State) ->
    #{ <<"game_name">> := GameName,
       <<"player_name">> := PlayerName } = Data,
    case maps:find(<<"host_game">>, Data) of
        {ok, true} ->
            lager:debug("Player ~p wants to host game: ~p.", [PlayerName, GameName]),
            game:host(GameName, Player, PlayerName);
        _ ->
            lager:debug("Player ~p wants to join game: ~p.", [PlayerName, GameName]),
            game:join(GameName, Player, PlayerName)
    end,
    {noreply, State};

handle_cast(Msg, State) ->
    lager:info("[~s] Unexpected cast: ~p", [?MODULE, Msg]),
    {noreply, State}.


handle_call(Msg, _From, State) ->
    lager:info("[~s] Unexpected call: ~p", [?MODULE, Msg]),
    {noreply, State}.


handle_info(Msg, State) ->
    lager:info("[~s] Unexpected info: ~p", [?MODULE, Msg]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
