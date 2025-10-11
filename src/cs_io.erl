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
-export([quit/0]).
-export([cursor_pos/2]).
-export([clear_screen/0]).
-export([do_atomic_ops/1]).

-record(state, {textarea_size,
			    monitor,
                esc_buffer = []}).

-define(ESC, 27).

debug(Text, X, Y) ->
    gen_server:cast(?MODULE, {debug, Text, X, Y}).

input(Char) ->
    gen_server:cast(?MODULE, {input, Char}).

quit() ->
	gen_server:cast(self(), quit).

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
handle_cast({input, Char}, State = #state{esc_buffer = EscBuffer}) ->
    % io:format("[~p]", [Char]),
    NewBuffer = maybe_parse(EscBuffer, Char),
	{noreply, State#state{esc_buffer = NewBuffer}};
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
handle_cast(quit, State) ->
    {stop, normal, State}; 
handle_cast(Req, State) ->
	Debug = lists:flatten(io_lib:format("cs_io unrecognized cast: ~p~n", [Req])),
    debug_(Debug, 1, 12),
	{noreply, State}.
%
handle_info({Ref, join, textarea_size, Joined},
			State = #state{monitor = Ref,
			               textarea_size = {H, W}}) ->
	publish(Joined, {textarea_size, H, W}),
	{noreply, State};
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
	%io:put_chars(cs_esc:show_cursor(false));
tui_mode(false) ->
	io:put_chars(cs_esc:show_cursor(true)),
    io:put_chars(cs_esc:alternate_screen_buffer(false)).

get_textarea_size() ->
	io:put_chars(cs_esc:get_textarea_size()).

% - no buffer, get ESC                -> flip to escape mode
% - no buffer, get non-ESC            -> stay normal
% - buffer, get ESC                   -> not valid escape code, flip to normal, process buffer
% - buffer, part of escape code       -> maybe escape code, in escape mode
% - buffer, not part of escape code   -> not valid escape code, flip to normal, process buffer

maybe_parse([], ?ESC) ->
    erlang:send_after(20, self(), esc_timeout),
    [?ESC];
maybe_parse([], Char) ->
    parse(Char),
    [];
maybe_parse(NotEscapeCode, esc_timeout) ->
    parse(NotEscapeCode),
    [];
maybe_parse(NotEscapeCode, ?ESC) ->
    parse(NotEscapeCode ++ [?ESC]),
    [];
maybe_parse(MaybeEscapeCode, Char) ->
    case cs_esc:parse_escape(MaybeEscapeCode ++ [Char]) of
        {escape, EscapeCode} ->
            escape_code(EscapeCode),
            [];
        not_escape ->
            parse(MaybeEscapeCode ++ [Char]),
            [];
        _ ->
            MaybeEscapeCode ++ [Char]
    end.

escape_code(up) ->
    debug_("up", 20, 20);
escape_code(down) ->
    debug_("down", 20, 20);
escape_code(left) ->
    debug_("left", 20, 20);
escape_code(right) ->
    debug_("right", 20, 20);
escape_code(f1) ->
    debug_("F1", 20, 20),
    io:put_chars(cs_esc:clear_screen());
escape_code(f2) ->
    debug_("F2", 20, 20),
    gen_server:cast(cs_screen, split_vertical);
escape_code(f3) ->
    debug_("F3", 20, 20),
    gen_server:cast(cs_screen, split_horizontal);
escape_code(f4) ->
    debug_("F4", 20, 20);
escape_code(f5) ->
    debug_("F5", 20, 20);
escape_code(f6) ->
    debug_("F6", 20, 20);
escape_code(f7) ->
    debug_("F7", 20, 20);
escape_code(f8) ->
    debug_("F8", 20, 20);
escape_code(f9) ->
    debug_("F9", 20, 20);
escape_code(f10) ->
    debug_("F10", 20, 20);
escape_code(f11) ->
    debug_("F11", 20, 20);
escape_code(f12) ->
    debug_("F12", 20, 20);
escape_code(shift_f1) ->
    debug_("Shift+F12", 20, 20);
escape_code(option_f1) ->
    debug_("Option+F12", 20, 20);
escape_code(shift_option_f1) ->
    debug_("Shift+Option+F12", 20, 20);
escape_code(ctrl_f1) ->
    debug_("Ctrl+F12", 20, 20);
escape_code(shift_ctrl_f1) ->
    debug_("Shift+Ctrl+F12", 20, 20);
escape_code(option_ctrl_f1) ->
    debug_("Option+Ctrl+F12", 20, 20);
escape_code(shift_option_ctrl_f1) ->
    debug_("Shift+Option+Ctrl+F12", 20, 20);
escape_code({text_area, H, W}) ->
    Debug = lists:flatten(io_lib:format("text area ~p,~p", [H,W])),
    debug_(Debug, 1, 10),
    gen_server:cast(self(), {textarea_size, H, W});
escape_code({screen_size, H, W}) ->
    Debug = lists:flatten(io_lib:format("screen size ~p,~p", [H,W])),
    debug_(Debug, 1, 11),
    gen_server:cast(self(), {screen_size, H, W}).

parse($q) ->
    quit();
parse(8) ->  % \b    ctrl-left
    debug_("Focus <-"),
    cs_screen:focus(left);
parse(10) -> % \n    ctrl-down
    debug_("Focus down"),
    cs_screen:focus(down);
parse(11) -> % \v    ctrl-up
    debug_("Focus up"),
    cs_screen:focus(up);
parse(12) -> % \f    ctrl-right
    debug_("Focus ->"),
    cs_screen:focus(right);
parse(127) -> % delete
    debug_("Delete"),
    cs_screen:delete();
%parse($u) ->
%    debug_(cs_esc:format("Double Underline", [double_underline])),
%    text(cs_esc:format("Double Underline", [double_underline]));
%parse($s) ->
%    debug_(cs_esc:get_screen_size()),
%    text(cs_esc:get_screen_size());
%parse($S) ->
%    debug_(cs_esc:get_textarea_size()),
%    text(cs_esc:get_textarea_size());
%parse($c) ->
%    debug_("@");
% parse($C) ->
    % clear_screen();
%parse($1) ->
%    debug_(cs_esc:clear_col()),
%    text(cs_esc:clear_col());
%parse($2) ->
%    debug_(cs_esc:clear_screen()),
%    text(cs_esc:clear_screen());
%parse($3) ->
%    debug_("                                                    "
%           "                                                    ",
%           1, 4);
%parse($4) ->
%    debug_("\\e O"),
%    text([27, $O]);
%parse($5) ->
%    debug_("\\e O C"),
%    text([27, $O, $C]);
%parse($r) ->
%    debug_("$"),
%    text("$");
%parse($|) ->
%gen_server:cast(cs_screen, split_vertical);
% parse([Char | Rest]) ->
    % parse(Char),
    % parse(Rest);
parse(List) when is_list(List) ->
    [parse(Char) || Char <- List];
parse(Other) ->
    debug_(["Sent ", Other, " to screen"]),
    cs_screen:text(Other).

publish(Group, {textarea_size, H, W}) ->
	[Pid ! {textarea_size, H, W} || Pid <- Group].

debug_(Text) ->
    debug_("CSIO: " ++ Text ++ "<                    ", 1, 6).

debug_(Text, X, Y) ->
    cursor_pos_(X, Y),
    debug_text(Text).

cursor_pos_(X, Y) ->
	io:put_chars(cs_esc:cursor_pos(X, Y)).

debug_text(Text) ->
    Flattened = lists:flatten(Text),
    Parsed = [parse2(Char) || Char <- Flattened],
    io:put_chars(Parsed).

parse2(8) -> "\\b";
parse2(9) -> "\\t";
parse2(10) -> "\\n";
parse2(11) -> "\\v";
parse2(12) -> "\\f";
parse2(13) -> "\\r";
parse2(?ESC) -> "\\e";
parse2(N) when N > 0, N < 8 -> [$\\ | integer_to_list(N)];
parse2(N) when N > 13, N < 32 -> [$\\ | integer_to_list(N)];
parse2(N) when N > 126 -> [$\\ | integer_to_list(N)];
parse2($~) -> "~~";
parse2(PrintableChar) -> PrintableChar.

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
