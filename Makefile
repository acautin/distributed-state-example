ERL ?= erl
APP := dist_gen_server

all:
	@./rebar compile
clean:
	@./rebar clean
docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'
