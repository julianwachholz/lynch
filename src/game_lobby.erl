-module(game_lobby).
-behaviour(gen_server).

%% API
-export([start_link/0, join/1, leave/1, message/2]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

join(Pid) ->
    gen_server:cast(?SERVER, {join, Pid}).

leave(Pid) ->
    gen_server:cast(?SERVER, {leave, Pid}).

message(Pid, Message) ->
    gen_server:cast(?SERVER, {message, Pid, Message}).


%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

init([]) ->
    State = #{ connections => [] },
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


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

send_message(Pid, Message, State) ->
    lists:foreach(
        fun (Recipient) ->
            Recipient ! {message, Pid, Message}
        end, maps:get(connections, State)).
