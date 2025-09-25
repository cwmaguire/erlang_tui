-module(cs_screen).
-behaviour(gen_server).

-include("cs.hrl").
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

-export([rotate_right/2]).

-record(state, {windows = [],
                borders = [],
                focused_window_id,
                next_id = 1,
                h = 0,
                w = 0}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    pg:join(textarea_size, self()),
    {ok, #state{}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(split_vertical, State) ->
    {noreply, _NewState = split_vertical(State)};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info({textarea_size, H, W}, State = #state{windows = []}) ->
    {noreply, _NewState = create_first_window(State, H, W)};
handle_info({textarea_size, H, W}, State) ->
    % TODO update window sizes
    {noreply, State#state{h = H, w = W}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

window(Id, X, Y, H, W) ->
    F = translate_fun(X, Y),
    {ok, Pid} = supervisor:start_child(cs_window_sup, [F, {H, W}]),
    #window{id = Id, pid = Pid, h = H, w = W}.

translate_fun(ScreenX, ScreenY) ->
    fun(WindowX, WindowY) ->
            {ScreenX + WindowX,
             ScreenY + WindowY}
    end.

get_focused_window(FocusId, Windows) ->
    Index = #window.id,
    #window{} = lists:keyfind(FocusId, Index, Windows).

create_first_window(State = #state{next_id = Id}, H, W) ->
    Window = window(Id, _X = 0, _Y = 0, H, W),
    State#state{h = H,
                w = W,
                focused_window_id = Id,
                windows = [Window],
                next_id = Id + 1}.

split_vertical( State = #state{windows = Windows,
                               borders = Borders,
                               focused_window_id = FocusId,
                               next_id = Id,
                               h = H,
                               w = W}) ->
    io:put_chars("split vert"),
    OldWindow = 
        #window{x = X,
                y = Y,
                h = H,
                w = W} = get_focused_window(FocusId, Windows),
    W1 = W div 2,
    W2 = W1 - (1 - (W rem 2)),
    BorderWidth = 1,
    NewWindow = window(Id, X + W1 + BorderWidth, Y, H, W2),
    OldWindow2 = OldWindow#window{w = W1},
    OtherWindows = lists:filter(fun(#window{id = Id_})
                                  when Id_ /= FocusId ->
                                    true;
                                   (_) ->
                                    false
                                end,
                                Windows),
    NewWindows = [OldWindow2, NewWindow | OtherWindows],

    NewBorders = [{FocusId, Id} | Borders],

    State#state{windows = NewWindows,
                borders = NewBorders,
                focused_window_id = Id}.

rotate_right(_WindowId, Windows = []) ->
    Windows;
rotate_right(WindowId, Windows) ->
    rotate_right(WindowId, Windows, []).

rotate_right(WindowId,
             [List| Rest],
             Acc) when is_list(List) ->
    RotatedSubTree = rotate_right(WindowId, List, []),
    NewAcc = Acc ++ [RotatedSubTree],
    rotate_right(WindowId, Rest, NewAcc);
rotate_right(WindowId,
             [W1 = #window{id = WindowId}, X | Rest],
             Acc) ->
    Acc ++ [X, W1 | Rest];
rotate_right(WindowId, [Window = #window{} | Rest], Acc) ->
    NewAcc = Acc ++ [Window],
    rotate_right(WindowId, Rest, NewAcc);
rotate_right(_WindowId, [], Acc) ->
    Acc.


% []
% [x]
% [[x, y]]
% [[[x, y], z][a]]
