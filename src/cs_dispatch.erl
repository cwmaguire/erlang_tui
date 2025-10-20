-module(cs_dispatch).
-behaviour(gen_server).

-include("debug.hrl").

-export([start_link/0]).
-export([init/1]).
-export([terminate/2]).

-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-export([input/1]).
-export([something/0]).
-export([exit_insert_mode/0]).
-export([enter_insert_mode/0]).

-define(ESC, 27).

-record(state, {mode = command :: command | insert}).

input(Input) ->
    gen_server:cast(?MODULE, {input, Input}).

enter_insert_mode() ->
    gen_server:cast(?MODULE, enter_insert_mode).

exit_insert_mode() ->
    gen_server:cast(?MODULE, exit_insert_mode).

something() ->
    ok.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], _Opts = []).

init(_Args) ->
    cs_io:clear(?MIN_DEBUG_LINE, ?MAX_DEBUG_LINE),
    cs_io:debug("cs_dispatch init", 0, ?DEBUG_DISPATCH_INIT),
    {ok, #state{}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({input, Input}, State = #state{mode = command}) ->
    cs_io:clear(?MIN_DEBUG_LINE, ?MAX_DEBUG_LINE),
    cs_io:debug("Input in command mode", 0, ?DEBUG_CMD_INPUT),
    cs_command:input(Input),
    {noreply, State};
handle_cast({input, ?ESC}, State = #state{mode = insert}) ->
    cs_io:clear(?MIN_DEBUG_LINE, ?MAX_DEBUG_LINE),
    cs_io:debug("Exiting insert while in insert", 0, ?DEBUG_INS_TO_CMD),
    {noreply, State#state{mode = command}};
handle_cast({input, Input}, State = #state{mode = insert}) ->
    cs_io:clear(?MIN_DEBUG_LINE, ?MAX_DEBUG_LINE),
    cs_io:debug("Input in insert mode", 0, ?DEBUG_INS_INPUT),
    cs_screen:text(Input),
    {noreply, State};
handle_cast(enter_insert_mode, State = #state{mode = command}) ->
    cs_io:clear(?MIN_DEBUG_LINE, ?MAX_DEBUG_LINE),
    cs_io:debug("Entering insert while in command", 0, ?DEBUG_CMD_TO_INS),
    {noreply, State#state{mode = insert}};
handle_cast(exit_insert_mode, State = #state{mode = insert}) ->
    cs_io:clear(10, 17),
    cs_io:debug("Exiting insert while in insert", 0, ?DEBUG_INS_TO_CMD),
    {noreply, State#state{mode = command}};
handle_cast(enter_insert_mode, State = #state{mode = insert}) ->
    cs_io:clear(?MIN_DEBUG_LINE, ?MAX_DEBUG_LINE),
    cs_io:debug("Entering insert while in insert", 0, ?DEBUG_INS_TO_INS),
    {noreply, State};
handle_cast(Req, State) ->
    cs_io:clear(?MIN_DEBUG_LINE, ?MAX_DEBUG_LINE),
    cs_io:debug(Req, 0, ?DEBUG_UNKNOWN_REQ),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.
