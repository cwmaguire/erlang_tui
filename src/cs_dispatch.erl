-module(cs_dispatch).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1]).
-export([terminate/2]).

-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-record(state, {mode = command :: command | insert}).

start_link() ->
    gen_server:start_link(?MODULE, [], _Opts = []).

init(_Args) ->
    {ok, #state{}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({input, Input}, State = #state{mode = command}) ->
    cs_command:input(Input),
    {noreply, State};
handle_cast({input, Input}, State = #state{mode = insert}) ->
    cs_screen:text(Input),
    {noreply, State};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.
