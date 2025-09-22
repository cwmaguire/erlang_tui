-module(cs_window).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1]).
-export([terminate/2]).

-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-record(state, {translate_fun,
			    h = 0,
			    w = 0,
			    cursor_pos}).

start_link(TranslateFun, {H, W}) ->
	gen_server:start_link(?MODULE,
						  [TranslateFun, {H, W}],
						  _Opts = []).

init([TranslateFun, {H, W}]) ->
	{X, Y} = TranslateFun(0, 0),
    Msg = io_lib:format("cursor ~p,~p", [X, Y]),
    cs_io:do_atomic_ops([{cursor_pos, X, Y},
						 {write, Msg}]),
	{ok, #state{translate_fun = TranslateFun,
				cursor_pos = {0, 0},
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
