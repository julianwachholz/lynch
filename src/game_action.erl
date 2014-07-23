-module(game_action).

-export([player_new/3, player_action/4]).


%%% TODO
%% bind functions to calls on a game.

player_new(Player, State, Data) ->
    lager:debug("Unexpected player_new [~p] ~p (~p): ~p", [PlayerPid, maps:to_list(State), maps:to_list(Data)]),
    ok.


%% Game, Player :: pid()
%% State, Data :: maps()
player_action(Game, Player, State, Data) ->
    lager:debug("Unexpected player_action [~p] ~p (~p): ~p", [GameName, PlayerPid, maps:to_list(State), maps:to_list(Data)]),
    ok.
