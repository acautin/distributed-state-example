-module(dist_gen_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1, list_children/0]).

%% Supervisor callbacks
-export([init/1]).

%% Macro to register the supervisor with the module name
-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(User) when is_binary(User) ->
	supervisor:start_child(?SERVER, [User]).

list_children() ->
	supervisor:which_children(?SERVER).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	RestartStrategy = simple_one_for_one,
	MaxRestarts = 5,
	MaxSecondsBetweenRestarts = 100,
	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
	Restart = transient,
	Shutdown = 2000,
	Type = worker,
	AChild = {connected_user, {connected_user, start_link, []}, Restart, Shutdown, Type, [connected_user]},
	{ok, {SupFlags, [AChild]}}.
