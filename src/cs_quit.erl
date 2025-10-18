-module(cs_quit).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1]).
-export([terminate/2]).

-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-export([quit/0]).

quit() ->
    gen_server:cast(self(), quit).

start_link() ->
    gen_server:start_link(?MODULE, [], _Opts = []).

init(_Args) ->
    {ok, undefined}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(quit, State) ->
    {stop, normal, State}; 
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

