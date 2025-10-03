-module(cs_io).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1]).
-export([terminate/2]).

-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-export([quit/0]).
-export([cursor_pos/2]).
-export([do_atomic_ops/1]).

-record(state, {textarea_size,
			    monitor,
                esc_buffer = []}).

-define(ESC, 27).

quit() ->
	gen_server:cast(self(), quit).

do_atomic_ops(Ops) ->
    timer:sleep(25),
	gen_server:cast(cs_io, {atomic, Ops}).

cursor_pos(X, Y) ->
	gen_server:cast(cs_io, {cursor_pos, X, Y}).


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
    io:format("[~p]", [Char]),
    NewBuffer = maybe_parse(EscBuffer, Char),
	{noreply, State#state{esc_buffer = NewBuffer}};
% TODO move these textarea_size clauses to a function
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
handle_cast(restart, State) ->
	start_input_loop(),
	{noreply, State};
handle_cast(quit, State) ->
    {stop, normal, State}; 
handle_cast(Req, State) ->
	io:format("cs_io unrecognized cast: ~p~n", [Req]),
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
    % ok = shell:start_interactive({noshell, raw}),
	tui_mode(true),
	% start_input_loop(),
	get_textarea_size().

tui_mode(true) ->
    io:put_chars(cs_esc:alternate_screen_buffer(true)),
	io:put_chars(cs_esc:show_cursor(false));
tui_mode(false) ->
	io:put_chars(cs_esc:show_cursor(true)),
    io:put_chars(cs_esc:alternate_screen_buffer(false)).

start_input_loop() ->
	Self = self(),
    spawn_monitor(fun() -> input_loop(Self) end).

input_loop(Parent) ->
    Chars = io:get_chars('', 1024),  % Blocks until a char is available
    gen_server:cast(Parent, {input, Chars}),
    input_loop(Parent).

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

escape_code({text_area, H, W}) ->
    io:format("text area ~p,~p", [H,W]),
    gen_server:cast(self(), {textarea_size, H, W});
escape_code({screen_size, H, W}) ->
    io:format("text area ~p,~p", [H,W]),
    gen_server:cast(self(), {screen_size, H, W}).

parse($q) ->
    quit();
parse($u) ->
    io:put_chars(cs_esc:format("Double Underline", [double_underline]));
parse($s) ->
    io:put_chars(cs_esc:get_screen_size());
parse($S) ->
    io:put_chars(cs_esc:get_textarea_size());
parse($c) ->
    io:put_chars("@");
	% {ok, Cols} = io:columns(),
	% io:put_chars(["Columns ", integer_to_list(Cols)]);
parse($r) ->
    io:put_chars("$");
	% {ok, Rows} = io:rows(),
	% io:put_chars(["Rows ", integer_to_list(Rows), "\r\n"]);
parse($|) ->
	gen_server:cast(cs_screen, split_vertical);
% parse([Char | Rest]) ->
    % parse(Char),
    % parse(Rest);
parse(List) when is_list(List) ->
    [parse(Char) || Char <- List];
parse(Other) ->
	% Formats {a} as "a"
	io:format("(~p)", [Other]).

publish(Group, {textarea_size, H, W}) ->
	[Pid ! {textarea_size, H, W} || Pid <- Group].

cursor_pos_(X, Y) ->
	io:put_chars(cs_esc:cursor_pos(X, Y)).

do_atomic_ops_({cursor_pos, X, Y}) ->
	cursor_pos_(X, Y);
do_atomic_ops_({write, Str}) ->
	io:put_chars(Str).

