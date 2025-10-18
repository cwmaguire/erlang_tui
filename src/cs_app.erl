-module(cs_app).

-behaviour(application).

-export([start/0]).
-export([start/2]).
-export([stop/1]).

start() ->
	application:start(cs).

start(_StartType, _StartArgs) ->
    cs_sup:start_link().

stop(_State) ->
	halt().
