-module(cm_shell_app).

-behaviour(application).

-export([start/0]).
-export([start/2]).
-export([stop/1]).

start() ->
	application:start(cm_shell).

start(_StartType, _StartArgs) ->
    cm_shell_sup:start_link().

stop(_State) ->
	halt().
