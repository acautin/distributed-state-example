-module(dist_gen_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	io:format("dist_gen_server_app:start"),
    dist_gen_server_sup:start_link().

stop(_State) ->
    ok.
