-module(cs_io).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1]).
-export([terminate/2]).

-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-export([input/1]).
-export([debug/3]).
-export([clear/2]).
-export([cursor_pos/2]).
-export([clear_screen/0]).
-export([do_atomic_ops/1]).

-record(state, {textarea_size,
                monitor,
                esc_buffer = [],
                send_after_ref :: reference() | undefined}).

-define(ESC, 27).

debug(Text, X, Y) ->
    gen_server:cast(?MODULE, {debug, Text, X, Y}).

clear(Row1, Row2) ->
    gen_server:cast(?MODULE, {clear, Row1, Row2}).

input(Char) ->
    gen_server:cast(?MODULE, {input, Char}).

do_atomic_ops(Ops) ->
    timer:sleep(5),
    gen_server:cast(cs_io, {atomic, Ops}).

cursor_pos(X, Y) ->
    gen_server:cast(cs_io, {cursor_pos, X, Y}).
    % gen_server:cast(cs_io, {text, "***"}).

clear_screen() ->
    gen_server:cast(cs_io, clear_screen).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    ok = gen_server:cast(self(), start),
    {ok, #state{}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(start, State) ->
    start(),
    {noreply, State};
handle_cast({input, Char},
            State1 = #state{esc_buffer = EscBuffer1,
                            send_after_ref = SendAfterRef1}) ->
    State2 =
        case maybe_parse(EscBuffer1, Char, SendAfterRef1) of
            {EscBuffer2, SendAfterRef2} ->
                State1#state{esc_buffer = EscBuffer2,
                             send_after_ref = SendAfterRef2};
            EscBuffer2 ->
                State1#state{esc_buffer = EscBuffer2}
        end,
    {noreply, State2};
handle_cast({cursor_pos, X, Y}, State) ->
    cursor_pos_(X, Y),
    {noreply, State};
handle_cast(clear_screen, State) ->
    clear_screen_(),
    {noreply, State};
handle_cast({text, Text}, State) ->
    text(Text),
    {noreply, State};
handle_cast({textarea_size, H, W}, State = #state{monitor = undefined}) ->
    {Monitor, Group} = pg:monitor(textarea_size),
    publish(Group, {textarea_size, H, W}),
    {noreply, State#state{textarea_size = {H, W}, monitor = Monitor}};
handle_cast({textarea_size, H, W}, State) ->
    Group = pg:get_members(textarea_size),
    publish(Group, {textarea_size, H, W}),
    {noreply, State#state{textarea_size = {H, W}}};
handle_cast({atomic, Ops}, State) ->
    [do_atomic_ops_(Op) || Op <- Ops],
    {noreply, State};
handle_cast({debug, "~", X, Y}, State) ->
    debug_("~~", X, Y),
    {noreply, State};
handle_cast({debug, Text, X, Y}, State) ->
    debug_(Text, X, Y),
    {noreply, State};
handle_cast({clear, Row1, Row2}, State) ->
    clear_(Row1, Row2),
    {noreply, State};
handle_cast(Req, State) ->
    Debug = lists:flatten(io_lib:format("cs_io unrecognized cast: ~p~n", [Req])),
    debug_(Debug, 1, 12),
    {noreply, State}.

handle_info({Ref, join, textarea_size, Joined},
            State = #state{monitor = Ref,
                           textarea_size = {H, W}}) ->
    publish(Joined, {textarea_size, H, W}),
    {noreply, State};
handle_info(esc_timeout, State = #state{esc_buffer = EscBuffer}) ->
    SendAfterRef = undefined,
    EscBuffer2 = maybe_parse(EscBuffer, esc_timeout, SendAfterRef),
    {noreply, State#state{esc_buffer = EscBuffer2,
                          send_after_ref = SendAfterRef}};
handle_info({'DOWN', _Ref, process, Pid2, Reason}, State) ->
    io:format("~p died because ~p", [Pid2, Reason]),
    gen_server:cast(self(), restart),    
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_, _) ->
    io:format("shutting down 1"),
    tui_mode(false),
    io:format("shutting down 2~n"),
    ok.

start() ->
    tui_mode(true),
    get_textarea_size().

tui_mode(true) ->
    io:put_chars(cs_esc:alternate_screen_buffer(true));
tui_mode(false) ->
    io:put_chars(cs_esc:show_cursor(true)),
    io:put_chars(cs_esc:alternate_screen_buffer(false)).

get_textarea_size() ->
    io:put_chars(cs_esc:get_textarea_size()).

publish(Group, {textarea_size, H, W}) ->
    [Pid ! {textarea_size, H, W} || Pid <- Group].

maybe_parse([], ?ESC, _) ->
    Ref = erlang:send_after(200, ?MODULE, esc_timeout),
    {[?ESC], Ref};
maybe_parse([], Char, _) ->
    cs_map:input(Char),
    [];
maybe_parse(NotEscapeCode, esc_timeout, _) ->
    cs_io:clear(10, 17),
    cs_io:debug("Esc timeout", 0, 17),
    cs_map:input(NotEscapeCode),
    [];
maybe_parse(NotEscapeCode, ?ESC, SendAfterRef) ->
    erlang:cancel_timer(SendAfterRef),
    cs_map:input(NotEscapeCode),
    maybe_parse([], ?ESC, undefined);
maybe_parse(MaybeEscapeCode, Char, SendAfterRef) ->
    erlang:cancel_timer(SendAfterRef),
    case cs_esc:parse_escape(MaybeEscapeCode ++ [Char]) of
        {escape, EscapeCode} ->
            cs_normal:escape_code(EscapeCode),
            [];
        not_escape ->
            cs_map:input(MaybeEscapeCode ++ [Char]),
            [];
        _ ->
            Ref = erlang:send_after(200, ?MODULE, esc_timeout),
            {MaybeEscapeCode ++ [Char], Ref}
    end.

debug_(Text, X, Y) ->
    cursor_pos_(X, Y),
    debug_text(Text).

clear_(Row1, Row2) ->
    [clear_row(Row) || Row <- lists:seq(Row1, Row2)].

clear_row(Row) ->
    Spaces = ".,                              ",
    cursor_pos_(1, Row),
    debug_text(Spaces).

cursor_pos_(X, Y) ->
    io:put_chars(cs_esc:cursor_pos(X, Y)).

debug_text(Text) ->
    Flattened = lists:flatten(Text),
    Parsed = [parse(Char) || Char <- Flattened],
    io:put_chars(Parsed).

text(Text) ->
    io:put_chars(Text).

clear_screen_() ->
    io:put_chars(cs_esc:clear_screen()).

do_atomic_ops_({cursor_pos, X, Y}) ->
    cursor_pos_(X, Y);
do_atomic_ops_({text, Str}) ->
    io:put_chars(Str);
do_atomic_ops_(delete) ->
    io:put_chars(cs_esc:delete()).

parse(8) -> "\\b";
parse(9) -> "\\t";
parse(10) -> "\\n";
parse(11) -> "\\v";
parse(12) -> "\\f";
parse(13) -> "\\r";
parse(?ESC) -> "\\e";
parse(N) when N > 0, N < 8 -> [$\\ | integer_to_list(N)];
parse(N) when N > 13, N < 32 -> [$\\ | integer_to_list(N)];
parse(N) when N > 126 -> [$\\ | integer_to_list(N)];
parse($~) -> "~~";
parse(PrintableChar) -> PrintableChar.

