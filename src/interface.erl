-module(interface).

-export([reward_player/2, local_players/0]).

connect_player(User) ->
	case dist_gen_server_sup:start_child(User) of
		{ok, _Pid} ->
			ok;
		{error, {already_started, _Pid}} ->
			ok;
		Error ->
			Error
	end.

reward_player(User, Points) when is_binary(User) ->
	case connect_player(User) of
		ok ->
			connected_user:add_points(User, Points);
		Error -> 
			Error
	end;
reward_player(_User, _Points) ->
	{error, invalid_player}.

local_players() ->
	ChildrenList = dist_gen_server_sup:list_children(),
	parse_children_names(ChildrenList).

parse_children_names(ChildrenList) ->
	parse_children_names(ChildrenList, []).

parse_children_names([], Result) ->
	Result;
parse_children_names([Child|Rest], Result) ->
	{_, Pid, _, _} = Child,
	ChildName = connected_user:get_user(Pid),
	parse_children_names(Rest, [ChildName | Result]).
