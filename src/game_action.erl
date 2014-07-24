-module(game_action).

-export([player_new/3, player_action/4]).


%%% TODO
%% bind functions to calls on a game.

player_new(Player, undefined_state, #{ <<"game_name">> := GameName,
                                       <<"player_name">> := PlayerName } = Data) ->
    case maps:find(<<"host_game">>, Data) of
        {ok, true} ->
            lager:debug("Player ~p wants to host game: ~p.", [PlayerName, GameName]),
            game:host(GameName, Player, PlayerName);
        _ ->
            lager:debug("Player ~p wants to join game: ~p.", [PlayerName, GameName]),
            game:join(GameName, Player, PlayerName)
    end;

player_new(Player, State, Data) ->
    lager:debug("Unexpected player_new [~p] ~p (~p): ~p", [Player, maps:to_list(State), maps:to_list(Data)]),
    ok.


%% Game, Player :: pid()
%% State, Data :: maps()
player_action(Game, Player, State, Data) ->
    lager:debug("Unexpected player_action [~p] ~p (~p): ~p", [Game, Player, maps:to_list(State), maps:to_list(Data)]),
    ok.
