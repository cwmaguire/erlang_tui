-module(cm_shell_io).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1]).
-export([terminate/2]).

-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

quit() ->
	gen_server:cast(self(), quit).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
	ok = gen_server:cast(self(), start),
	{ok, state}.

handle_call(_Req, _From, _State) ->
	{reply, ok, state}.

handle_cast(start, _State) ->
	start(),
	{noreply, state};
handle_cast({input, Chars}, State) ->
    parse(Chars),
	{noreply, State};
handle_cast(restart, _State) ->
	start_input_loop(),
	{noreply, state};
handle_cast(quit, state) ->
    {stop, normal, state}; 
handle_cast(_Req, _State) ->
	{noreply, state}.

handle_info({'DOWN', _Ref, process, Pid2, Reason}, state) ->
	io:format("~p died because ~p", [Pid2, Reason]),
    gen_server:cast(self(), restart),    
	{noreply, state};
handle_info(_Info, _State) ->
	{noreply, state}.

terminate(_, _) ->
	tui_mode(false),
	ok.

start() ->
    ok = shell:start_interactive({noshell, raw}),
    %ok = shell:start_interactive({noshell, cooked}),
	tui_mode(true),
	start_input_loop().

tui_mode(true) ->
    io:put_chars(esc_codes:alternate_screen_buffer(true)),
	io:put_chars(esc_codes:show_cursor(false));
tui_mode(false) ->
	io:put_chars(esc_codes:show_cursor(true)),
    io:put_chars(esc_codes:alternate_screen_buffer(false)).

start_input_loop() ->
	Self = self(),
    spawn_monitor(fun() -> input_loop(Self) end).

input_loop(Parent) ->
    Chars = io:get_chars('', 1024),  % Blocks until a char is available
    gen_server:cast(Parent, {input, Chars}),
    input_loop(Parent).

%% TODO send this to one or more parsing servers
parse("q") ->
    quit();
parse(List) when is_list(List) ->
    io:put_chars(List);
parse(Other) ->
	% Formats [a] as "a"
	io:format("~p", [Other]).
