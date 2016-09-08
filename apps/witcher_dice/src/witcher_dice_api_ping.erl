-module(witcher_dice_api_ping).
-export([init/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([content_types_provided/2]).
-export([read_json/2]).

init(Req, _Opts) ->
	{cowboy_rest, Req, undefined}.

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>], Req, State}.


resource_exists(Req, State) ->
    {true, Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, read_json}
    ], Req, State}.

read_json(Req, State) ->
    {jsx:prettify(jsx:encode(ping())), Req, State}.

ping() ->
    {TotalWallClock, _} = erlang:statistics(wall_clock),
    Versions = lists:keysort(1, [{list_to_binary(atom_to_list(Lib)), list_to_binary(Version)} || {Lib, _, Version} <- proplists:get_value(loaded, application:info())]),
    [
        {<<"pong">>, TotalWallClock},
        {<<"node">>, list_to_binary(atom_to_list(node()))},
        {<<"cluster_nodes">>, [list_to_binary(atom_to_list(Node)) || Node <- nodes()]},
        {<<"process_count">>, erlang:system_info(process_count)},
        {<<"run_queue">>, erlang:statistics(run_queue)},
        {<<"erlang_version">>, list_to_binary(erlang:system_info(otp_release))},
        {<<"versions">>, Versions}
    ].
