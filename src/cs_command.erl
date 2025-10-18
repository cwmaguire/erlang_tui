-module(cs_command).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1]).
-export([terminate/2]).

-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-export([input/1]).
-export([parse/1]).
-export([escape_code/1]).

-record(state, {}).

input(Input) ->
    % debug([Input]),
    gen_server:cast(?MODULE, {input, Input}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], _Opts = []).

init(_Args) ->
    {ok, #state{}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({input, Input}, State) ->
    debug("handle_cast"),
    parse(Input),
    {noreply, State};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

escape_code(up) ->
    debug("up", 20, 20);
escape_code(down) ->
    debug("down", 20, 20);
escape_code(left) ->
    debug("left", 20, 20);
escape_code(right) ->
    debug("right", 20, 20);
escape_code(f1) ->
    debug("F1", 20, 20),
    io:put_chars(cs_esc:clear_screen());
escape_code(f2) ->
    debug("F2", 20, 20),
    gen_server:cast(cs_screen, split_vertical);
escape_code(f3) ->
    debug("F3", 20, 20),
    gen_server:cast(cs_screen, split_horizontal);
escape_code(f4) ->
    debug("F4", 20, 20),
    gen_server:cast(cs_screen, print_windows);
escape_code(f5) ->
    debug("F5", 20, 20);
escape_code(f6) ->
    debug("F6", 20, 20);
escape_code(f7) ->
    debug("F7", 20, 20);
escape_code(f8) ->
    debug("F8", 20, 20);
escape_code(f9) ->
    debug("F9", 20, 20);
escape_code(f10) ->
    debug("F10", 20, 20);
escape_code(f11) ->
    debug("F11", 20, 20);
escape_code(f12) ->
    debug("F12", 20, 20);
escape_code(shift_f1) ->
    debug("Shift+F12", 20, 20);
escape_code(option_f1) ->
    debug("Option+F12", 20, 20);
escape_code(shift_option_f1) ->
    debug("Shift+Option+F12", 20, 20);
escape_code(ctrl_f1) ->
    debug("Ctrl+F12", 20, 20);
escape_code(shift_ctrl_f1) ->
    debug("Shift+Ctrl+F12", 20, 20);
escape_code(option_ctrl_f1) ->
    debug("Option+Ctrl+F12", 20, 20);
escape_code(shift_option_ctrl_f1) ->
    debug("Shift+Option+Ctrl+F12", 20, 20);
escape_code({text_area, H, W}) ->
    Debug = lists:flatten(io_lib:format("text area ~p,~p", [H,W])),
    debug(Debug, 1, 10),
    gen_server:cast(cs_io, {textarea_size, H, W});
escape_code({screen_size, H, W}) ->
    Debug = lists:flatten(io_lib:format("screen size ~p,~p", [H,W])),
    debug(Debug, 1, 11),
    gen_server:cast(cs_io, {screen_size, H, W}).

parse($q) ->
    debug("Got to quit with q"),
    cs_quit:quit();
parse(8) ->  % \b    ctrl-left
    debug("Focus <-"),
    cs_screen:focus(left);
parse(10) -> % \n    ctrl-down
    debug("Focus down"),
    cs_screen:focus(down);
parse(11) -> % \v    ctrl-up
    debug("Focus up"),
    cs_screen:focus(up);
parse(12) -> % \f    ctrl-right
    debug("Focus ->"),
    cs_screen:focus(right);
parse(127) -> % delete
    debug("Delete"),
    cs_screen:delete();
parse(List) when is_list(List) ->
    [parse(Char) || Char <- List];
parse(Other) ->
    debug(["Sent ", Other, " to screen"]),
    cs_screen:text(Other).

debug(Text) ->
    debug("CMD: " ++ Text ++ "<                    ", 1, 6).

debug(Text, X, Y) ->
    cs_io:debug(Text, X, Y).


