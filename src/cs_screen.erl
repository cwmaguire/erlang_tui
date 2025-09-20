-module(cs_screen).
-behaviour(gen_server).

%% the enter screen is called "screen"
%% a screen has sections called "windows"
%%
%% For now, the screen is implied and this module will draw on the only
%% screen.
%%
%% You always start with one window which can be split. Each split
%% window can also be split, forming a tree.

%% Handling terminal resize events:
%% Add handler to erl_signal_server to handle SIGWINCH
%% and then call os:set_signals to get erl_signal_server to call
%% out to our handler when that kernel signal is received.
%% Then we can update the window size.

%% Flowchart of window

%% win -> io "I am pid 9, send me textarea size"
%% io: add 9 to list of textarea subscribers
%% io -> terminal "[get textarea] ansi code"
%% terminal -> io " CSI 9 C ; R t"
%% io: [sub ! textarea_size | sub <- subscribers]

-export([start_link/0]).
-export([init/1]).
-export([terminate/2]).

-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-record(state, {windows = [],
			    h = 0,
			    w = 0}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
	pg:join(textarea_size, self()),
	{ok, #state{}}.

handle_call(_Req, _From, State) ->
	{reply, ok, State}.

handle_cast(_Req, State) ->
	{noreply, State}.

handle_info({textarea_size, H, W}, State = #state{windows = []}) ->
	F = fun(X, Y) -> {X, Y} end,
	Window = window(F, H, W),
	{noreply, State#state{h = H, w = W, windows = [Window]}};
handle_info({textarea_size, H, W}, State) ->
	{noreply, State#state{h = H, w = W}};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_, _) ->
	ok.

window(F, H, W) ->
	{ok, Pid} = supervisor:start_child(cs_window_sup, [F, {H, W}]),
	Pid.
