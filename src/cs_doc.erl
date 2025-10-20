-module(cs_doc).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1]).
-export([terminate/2]).

-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-export([new/1]).
-export([add_window/1]).
-export([text/3]).

% Line = {LineNumber, Text}

-record(state, {
            window_pids = [],
            text = []
        }).

new(WindowPid) ->
    {ok, Pid} = supervisor:start_child(cs_doc_sup, [WindowPid]),
    Pid.

add_window(WindowPid) ->
    gen_server:cast(?MODULE, {add_window, WindowPid}).

text(Pid, CursorPos, Text) ->
    gen_server:cast(Pid, {text, CursorPos, Text}).

start_link(WindowPid) ->
    gen_server:start_link(?MODULE,
                          _Args = [WindowPid],
                          _Opts = []).

init([WindowPid]) ->
    {ok, #state{window_pids = [WindowPid]}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({text, {LineNo, ColNo}, NewText0},
            State = #state{text = Text0,
                           window_pids = WindowPids}) ->
    NewText1 =
        case NewText0 of
            List when is_list(List) ->
                List;
            _ ->
                [NewText0]
        end,
    {PreLines, Line, PostLines} = line(Text0, LineNo),
    {PreText, PostText} = col(Line, ColNo),

    Line2 = PreText ++ NewText1 ++ PostText,
    Text2 = PreLines ++ [Line2 | PostLines],
    notify(WindowPids, LineNo, Line2),
    {noreply, State#state{text = Text2}};
handle_cast({add_window, WindowPid},
            State = #state{window_pids = WindowPids}) ->
    {noreply, State#state{window_pids = [WindowPid | WindowPids]}};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

line(Text, LineNo) ->
    {Pre, [Line | Post]} = lists:split(LineNo - 1, Text),
    {Pre, Line, Post}.

col(Line, ColNo) ->
    lists:split(Line, ColNo).

notify(WindowPids, LineNo, Line) ->
    [cs_window:notify(Pid, LineNo, Line) || Pid <- WindowPids].
