-module(cs_window).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1]).
-export([terminate/2]).

-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-record(state, {coords_fun,
			    h = 0,
			    w = 0}).

start_link(CoordsFun, {H, W}) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [CoordsFun, {H, W}], []).

init([CoordsFun, {H, W}]) ->
    cs_io:cursor_pos(50, 15),
	io:put_chars("pos:50,15"),
	{ok, #state{coords_fun = CoordsFun,
			    h = H,
			    w = W}}.

handle_call(_Req, _From, State) ->
	{reply, ok, State}.

handle_cast(_Req, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_, _) ->
	ok.
