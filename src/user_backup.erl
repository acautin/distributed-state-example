-module(user_backup).

-behaviour(gen_server).

%% API
-export([start/2,
	stop/1,
	save_state/2,
	get_state/1,
	get_node/1]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-export_type([]).

%%% API Implementation
start(User, State) ->
	gen_server:start({global, {User, backup}}, ?MODULE, [State], []).

stop(User) ->
	gen_server:call({global, {User, backup}}, stop).

save_state(User, State) ->
	gen_server:call({global, {User, backup}}, {save_state, State}).

get_state(User) ->
	gen_server:call({global, {User, backup}}, get_state).

get_node(User) ->
	gen_server:call({global, {User, backup}}, get_node).

%%% gen_server callbacks
init([State]) ->
	{ok, State}.

handle_call(get_state, _From, State) ->
	{reply, State, State};
handle_call({save_state, NewState},	_From, _OldState) ->
	{reply, ok, NewState};
handle_call(get_node, _From, State) ->
	{reply, node(self()), State};
handle_call(stop, _From, State) ->
	{stop, normal, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
