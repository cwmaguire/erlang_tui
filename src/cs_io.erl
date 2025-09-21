-module(cs_io).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1]).
-export([terminate/2]).

-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-export([quit/0]).
-export([sub_textarea_size/1]).
-export([cursor_pos/2]).

-record(state, {textarea_size,
			    monitor}).

-define(ESC, 27).

quit() ->
	gen_server:cast(self(), quit).

sub_textarea_size(Pid) ->
	gen_server:cast(?MODULE, {sub_text_area_size, Pid}).

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
handle_cast({input, Chars}, State) ->
    parse(Chars),
	{noreply, State};
% TODO move these textarea_size clauses to a function
handle_cast({textarea_size, H, W}, State = #state{monitor = undefined}) ->
	{Monitor, Group} = pg:monitor(textarea_size),
	publish(Group, {textarea_size, H, W}),
	{noreply, State#state{textarea_size = {H, W}, monitor = Monitor}};
handle_cast({textarea_size, H, W}, State) ->
    Group = pg:get_members(textarea_size),
	publish(Group, {textarea_size, H, W}),
	{noreply, State#state{textarea_size = {H, W}}};
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
	tui_mode(false),
	ok.

start() ->
    ok = shell:start_interactive({noshell, raw}),
	tui_mode(true),
	start_input_loop(),
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

%% TODO send this to one or more parsing servers
parse([?ESC]) ->
	io:put_chars("?ESC");
parse([?ESC | Rest]) ->
	gen_server:cast(self(), cs_esc:parse_escape_code(Rest));
parse("q") ->
    quit();
parse("u") ->
    io:put_chars(cs_esc:format("Double Underline", [double_underline]));
parse("s") ->
    io:put_chars(cs_esc:get_screen_size());
parse("S") ->
    io:put_chars(cs_esc:get_textarea_size());
parse("c") ->
	{ok, Cols} = io:columns(),
	io:put_chars(["Columns ", integer_to_list(Cols)]);
parse("r") ->
	{ok, Rows} = io:rows(),
	io:put_chars(["Rows ", integer_to_list(Rows), "\r\n"]);
parse(List) when is_list(List) ->
    io:put_chars(List);
parse(Other) ->
	% Formats [a] as "a"
	io:format("~p", [Other]).

publish(Group, {textarea_size, H, W}) ->
	[Pid ! {textarea_size, H, W} || Pid <- Group].

cursor_pos(X, Y) ->
    [?ESC | Rest] = cs_esc:cursor_pos(X, Y),
	io:format("Set cursor: ~p~n", [Rest]),
	io:put_chars(cs_esc:cursor_pos(X, Y)).
