-module(test).
-behaviour(gen_server).
%-include("../include/records.hrl").

-export([register_me/2]).
-export([start_link/0, init/1, handle_call/3, terminate/2, shutdown/1,
         handle_info/2, code_change/3]).

register_me(ServerRef, PlayerName) ->
	gen_server:call(ServerRef, {register, PlayerName}).

shutdown(Pid) ->
	gen_server:call(Pid, terminate).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

register_player(PlayerName, Players) when length(Players) < 2->
	{"<<Register success>>", [PlayerName | Players]};

register_player(_, Players) ->
	{"<<All players already register>>", Players}.

handle_call({register, PlayerName}, _From, State) ->
		{Message, NewState} = register_player(PlayerName, State),
    	{reply, Message, NewState};

handle_call(terminate, _From, _) ->
    {stop, normal, ok, []}.

init([]) -> {ok, []}.

terminate(normal, _) ->
    io:format("Okay.~n",[]),
	ok.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}.
