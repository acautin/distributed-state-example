-module(connected_user).

-behaviour(gen_server).

%% API
-export([start_link/1,
	add_points/2,
	get_user/1]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-export_type([]).

-record(user, {user, points = 0}).

-define(SERVER, ?MODULE).

%%% API Implementation
start_link(User) ->
	gen_server:start_link({global, User}, ?MODULE, [User], []).

add_points(User, Points) ->
	gen_server:call({global, User}, {add_points, Points}).

get_user(Pid) when is_pid(Pid) ->
	gen_server:call(Pid, get_user);
get_user(User) ->
	gen_server:call({global, User}, get_user).

%%% gen_server callbacks
init([User]) ->
	io:format("Connecting the player ~s~n", [User]),
	random:seed(erlang:now()),
	%% Here we should load the user from the db...
	%% db:load_user(User).
	ensure_started_backup(nodes(), User, #user{user = User}).

handle_call({add_points, Points}, _From,
				#user{user = User, points = OldPoints} = OldState)
					when is_integer(Points) ->
	NewState = OldState#user{points = OldPoints + Points},
	save_state(User, NewState, OldState);
handle_call(get_user, _From, State) ->
	{reply, {State#user.user, State#user.points}, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	%% In case of normal termination we should kill the backup and commit
	%% the user information to the persistent storage.
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% Private functions
ensure_started_backup([], _User, _State) ->
	{stop, single_node};
ensure_started_backup(Nodes, User, State) ->
	RemoteNode = lists:nth(random:uniform(length(Nodes)), Nodes),
	case rpc:call(RemoteNode, user_backup, start, [User, State], 2000) of
		{ok, Pid} ->
			BackupNodeName = node(Pid),
			io:format("Backup created on node: ~s~n", [BackupNodeName]),
			{ok, State};
		{error, {already_started, _Pid}} ->
			SavedState = user_backup:get_state(User),
			BackupNodeName = user_backup:get_node(User),
			io:format("Backup is in node: ~s~n", [BackupNodeName]),
			%% Check if the backup is in the same node
			if
				node() =:= BackupNodeName ->
					%% Lets move the node to somewhere else
					user_backup:stop(User),
					ensure_started_backup(nodes(), User, SavedState);
				true ->
					{ok, SavedState}
			end;
		_ ->
			{stop, backup_process_fail}
	end.

save_state(User, NewState, OldState) ->
	case ensure_started_backup(nodes(), User, NewState) of
		{ok, _State} ->
		 	  user_backup:save_state(User, NewState),
		 	  {reply, NewState#user.points, NewState};
		{stop, Reason} ->
			  {reply, Reason, OldState}
	end.