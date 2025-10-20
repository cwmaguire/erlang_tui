-module(cs_window).
-behaviour(gen_server).

-export([start_link/3]).
-export([init/1]).
-export([terminate/2]).

-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-export([new/2]).
-export([new/3]).
-export([text/2]).
-export([display/2]).
-export([random_file_name/0]).
-export([notify/3]).

-record(state, {document_pid :: pid(),
                translate_fun :: fun() | undefined,
                h = 0 :: integer(),
                w = 0 :: integer(),
                has_border = false :: boolean(),
                has_status_bar = true :: boolean(),
                cursor_pos,
                temp_file_name}).

new(H, W) ->
    new(undefined, H, W).

new(DocumentPid, H, W) ->
    {ok, Pid} = supervisor:start_child(cs_window_sup, [DocumentPid, H, W]),
    Pid.

text(Pid, Text) ->
    gen_server:cast(Pid, {text, Text}).

notify(Pid, LineNo, Line) ->
    gen_server:cast(Pid, {line_change, LineNo, Line}).

display(Pid, Text) ->
    gen_server:cast(Pid, {display, Text}).

start_link(DocumentPid, H, W) ->
    gen_server:start_link(?MODULE,
                          [DocumentPid, H, W],
                          _Opts = []).

init([MaybeDocumentPid, H, W]) ->
    % gen_server:cast(self, draw),
    DocumentPid =
        case MaybeDocumentPid of
            Pid when is_pid(Pid) ->
                cs_doc:add_window(self()),
                Pid;
            _ ->
                cs_doc:new(self())
        end,

    {ok, #state{document_pid = DocumentPid,
                cursor_pos = {1, 0}, % I think the rows and columns are 1-based.
                h = H,
                w = W,
                temp_file_name = random_file_name()}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({text, Text}, State = #state{translate_fun = TFun,
                                         cursor_pos = CursorPos,
                                         w = W,
                                         h = H}) ->
    NewCursorPos = text_(TFun, CursorPos, Text, W, H),
    {noreply, State#state{cursor_pos = NewCursorPos}};
handle_cast({text, Text}, State = #state{document_pid = DocumentPid,
                                         translate_fun = TFun,
                                         cursor_pos = CursorPos,
                                         w = W,
                                         h = H}) ->
    cs_doc:text(DocumentPid, CursorPos, Text),
    % NewCursorPos = text_(TFun, CursorPos, Text, W, H),
    {noreply, State};
%% cs_screen should send 'draw' once windows are laid out.
%% Will need translate function.
handle_cast(draw, State = #state{translate_fun = TFun,
                                 w = W,
                                 h = H,
                                 has_border = HasBorder,
                                 has_status_bar = HasStatusBar}) ->
    draw(TFun, H, W, HasBorder, HasStatusBar),
    {noreply, State};
handle_cast({translate_fun, F}, State = #state{}) ->
    {noreply, State#state{translate_fun = F}};
handle_cast({update, Fun, W, H, HasBorder}, State) ->
    {noreply, State#state{translate_fun = Fun,
                          w = W,
                          h = H,
                          has_border = HasBorder}};
handle_cast(delete,
            State = #state{translate_fun = TFun,
                           cursor_pos = CursorPos}) ->
    NewCursorPos = delete_(TFun, CursorPos),
    {noreply, State#state{cursor_pos = NewCursorPos}};
handle_cast(focus, State = #state{translate_fun = TFun,
                                  cursor_pos = CursorPos}) ->
    focus(TFun, CursorPos),
    {noreply, State};
handle_cast({line_change, LineNo, Line},
            State = #state{%cursor_pos = CursorPos,
                           translate_fun = TFun,
                           w = W,
                           h = H}) ->
    NewCursorPos = text_(TFun, {0, LineNo}, Line, W, H),
    {noreply, State#state{cursor_pos = NewCursorPos}};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

%% TODO wrap text if wrap on
%% TODO calculate new cursor pos
%% TODO calculate max width
text_(TFun, {X, Y}, [Char | _], W, _H) when X >= W ->
    {ScreenX, ScreenY} = TFun(W, Y),
    cs_io:do_atomic_ops(
        [{cursor_pos, ScreenX, ScreenY},
         {text, [Char]}]),
    {X, Y};
text_(TFun, {X, Y}, Text, W, _H) ->
    {ScreenX, ScreenY} = TFun(X, Y),

    MaxLength = W - X + 1,
    VisibleText = lists:sublist(Text, MaxLength),

    cs_io:do_atomic_ops(
        [{cursor_pos, ScreenX, ScreenY},
         {text, VisibleText}]),
    {X + length(Text), Y}.

delete_(TFun, {X, Y}) ->
    {ScreenX, ScreenY} = TFun(X, Y),
    cs_io:do_atomic_ops([{cursor_pos, ScreenX - 1, ScreenY},
                         delete]),
    {X - 1, Y}.

focus(TFun, {X, Y}) ->
    {ScreenX, ScreenY} = TFun(X, Y),
    cs_io:cursor_pos(ScreenX, ScreenY).

draw(TFun, H, W, HasBorder, HasStatusBar) ->
    {X, Y} = TFun(5, 5),

    cs_io:do_atomic_ops([{cursor_pos, X, Y},
                         {text, "Hi!"}]),

    case HasBorder of
        true ->
            [draw_left_border(TFun(0, Y_)) || Y_ <- lists:seq(0, H)];
        false ->
            ok
    end,
    case HasStatusBar of
        true ->
            [draw_status_bar(TFun(X_, H - 1)) || X_ <- lists:seq(0, W)];
        false ->
            ok
    end,
    reset_cursor().

reset_cursor() ->
    cs_io:cursor_pos(5, 5).

draw_status_bar({X, Y}) ->
    %% █ is 9608 (U+2588 where 2588 is hex)
    Ops = [{cursor_pos, X, Y}, {text, "█"}],
    cs_io:do_atomic_ops(Ops).

draw_left_border({X, Y}) ->
    %% █ is 9474
    Ops = [{cursor_pos, X, Y}, {text, "│"}],
    cs_io:do_atomic_ops(Ops).

-define(CHARS, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_").

random_file_name() ->
    RandBytes = crypto:strong_rand_bytes(40),
    << <<(char(X))>> || <<X>> <= RandBytes>>.

char(Index) ->
    Nth = Index rem (length(?CHARS) - 1),
    lists:nth(Nth + 1, ?CHARS).
