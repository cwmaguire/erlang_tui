-module(cs_window).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1]).
-export([terminate/2]).

-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-export([text/2]).
-export([random_file_name/0]).

-record(state, {translate_fun,
                h = 0,
                w = 0,
                has_border = false,
                cursor_pos,
                temp_file_name}).

text(Pid, Text) ->
    gen_server:cast(Pid, {text, Text}).

start_link(TranslateFun, {H, W}) ->
    gen_server:start_link(?MODULE,
                          [TranslateFun, {H, W}],
                          _Opts = []).

init([TranslateFun, {H, W}]) ->
    % gen_server:cast(self, draw),
    {ok, #state{translate_fun = TranslateFun,
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
%% cs_screen should send 'draw' once windows are laid out.
%% Will need translate function.
handle_cast(draw, State = #state{translate_fun = TFun,
                                 w = W,
                                 h = H,
                                 has_border = HasBorder}) ->
    draw(TFun, H, W, HasBorder),
    {noreply, State};
handle_cast({translate_fun, F}, State = #state{}) ->
    {noreply, State#state{translate_fun = F}};
handle_cast({update, Fun, W, H, HasBorder}, State) ->
    {noreply, State#state{translate_fun = Fun,
                          w = W,
                          h = H,
                          has_border = HasBorder}};
handle_cast(delete, State = #state{translate_fun = TFun,
                                         cursor_pos = CursorPos}) ->
    NewCursorPos = delete_(TFun, CursorPos),
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

draw(TFun, H, W, HasBorder) ->
    {X, Y} = TFun(5, 5),

    cs_io:do_atomic_ops([{cursor_pos, X, Y},
                         {text, "Hi!"}]),

    case HasBorder of
        true ->
            [draw_left_border(TFun(0, Y_)) || Y_ <- lists:seq(0, H)];
        false ->
            ok
    end,
    [draw_status_bar(TFun(X_, H)) || X_ <- lists:seq(0, W)],
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
