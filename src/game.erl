-module(game).
-behaviour(gen_fsm).

%% API
-export([start/1, start_link/1]).
-export([]).

%% gen_fsm
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).
% states
-export([lobby/2, lobby/3,
         starting/2, starting/3,
         discussion/2, discussion/3,
         vote/2, vote/3,
         defense/2, defense/3,
         judgement/2, judgement/3,
         final_words/2, final_words/3,
         last_will/2, last_will/3,
         evening/2, evening/3,
         night/2, night/3,
         morning/2, morning/3,
         death_note/2, death_note/3,
        ]).


%%% API

start(Name) ->
    gen_fsm:start(?MODULE, [Name], []).


start_link() ->
    gen_fsm:start_link(?MODULE, [Name], []).


%join(Pid) ->
%    gen_server:cast(?SERVER, {join, Pid}).


%%% gen_fsm

init([]) ->
    State = #{ players => [] },
    {ok, State}.


handle_call(_Request, _From, State) ->
    {noreply, State}.


handle_cast({join, Pid}, State) ->
    lager:debug("joined: ~p", [Pid]),
    Connections = [Pid | maps:get(connections, State)],
    {noreply, State#{ connections := Connections }};

handle_cast({leave, Pid}, State) ->
    lager:debug("left: ~p", [Pid]),
    Connections = maps:get(connections, State) -- [Pid],
    {noreply, State#{ connections := Connections }};

handle_cast({message, Pid, Message}, State) ->
    lager:debug("message from ~p: ~p", [Pid, Message]),
    send_message(Pid, Message, State),
    {noreply, State};

handle_cast(_Message, State) ->
    {noreply, State}.


handle_info(Msg, State) ->
    lager:debug("Unexpected message: ~p", [Msg]),
    {noreply, State}.


terminate(normal, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%% Internal functions

send_message(Pid, Message, State) ->
    lists:foreach(
        fun (Recipient) ->
            Recipient ! {message, Pid, Message}
        end, maps:get(connections, State)).


player_state(PlayerPid, PlayerState) ->
    PlayerPid ! {state, PlayerState}.


player_reply(PlayerPid, Data) ->
    PlayerPid ! {reply, Data}.
