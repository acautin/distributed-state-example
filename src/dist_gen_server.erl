-module(dist_gen_server).
-export([start/0, start_link/0, stop/0]).

start_link() ->
    dist_gen_server_sup:start_link().

start() ->
    application:start(dist_gen_server).

stop() ->
    application:stop(dist_gen_server).
