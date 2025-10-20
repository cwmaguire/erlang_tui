-module(cs_dispatch).
-behaviour(gen_statem).

-include("debug.hrl").

-export([start_link/0]).
-export([terminate/3]).
-export([code_change/4]).
-export([init/1]).
-export([callback_mode/0]).

-export([insert/3]).
-export([normal/3]).
-export([command/3]).

-record(data, {}).

-define(ESC, 27).

callback_mode() -> state_functions.

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

terminate(_Reason, _State, _Data) ->
    void.

code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.

init([]) ->
    {ok, _State = normal, _Data = #data{}}.

insert(cast, {input, ?ESC}, Data) ->
    cs_io:clear(?MIN_DEBUG_LINE, ?MAX_DEBUG_LINE),
    cs_io:debug("Input in command mode", 0, ?DEBUG_CMD_INPUT),
    {next_state, normal, Data};
insert(cast, {input, Input}, _Data) ->
    cs_io:clear(?MIN_DEBUG_LINE, ?MAX_DEBUG_LINE),
    cs_io:debug("Input in insert mode", 0, ?DEBUG_INS_INPUT),
    cs_screen:text(Input),
    {keep_state_and_data};
insert(cast, exit_insert_mode, Data) ->
    cs_io:clear(10, 17),
    cs_io:debug("Exiting insert while in insert", 0, ?DEBUG_INS_TO_CMD),
    {next_state, normal, Data};
insert(cast, enter_insert_mode, _Data) ->
    cs_io:clear(?MIN_DEBUG_LINE, ?MAX_DEBUG_LINE),
    cs_io:debug("Entering insert while in insert", 0, ?DEBUG_INS_TO_INS),
    {keep_state_and_data};
insert(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

normal(cast, {input, Input}, _Data) ->
    cs_io:clear(?MIN_DEBUG_LINE, ?MAX_DEBUG_LINE),
    cs_io:debug("Input in command mode", 0, ?DEBUG_CMD_INPUT),
    cs_normal:input(Input),
    {keep_state_and_data};
normal(cast, enter_insert_mode, Data) ->
    cs_io:clear(?MIN_DEBUG_LINE, ?MAX_DEBUG_LINE),
    cs_io:debug("Entering insert while in command", 0, ?DEBUG_CMD_TO_INS),
    {next_state, insert, Data};
normal(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

command(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

%% Handle events common to all states
handle_event({call,From}, get_count, Data) ->
    %% Reply with the current count
    {keep_state,Data,[{reply,From,Data}]};
handle_event(_, Req, Data) ->
    cs_io:clear(?MIN_DEBUG_LINE, ?MAX_DEBUG_LINE),
    cs_io:debug(Req, 0, ?DEBUG_UNKNOWN_REQ),
    {keep_state,Data}.
