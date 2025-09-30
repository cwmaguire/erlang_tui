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

-export([split_window/4]).
-export([rotate_right/2]).
-export([layout_windows/3]).

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

handle_cast(split_vertical, State1) ->
    State2 = split_vertical(State1),
    % TODO
    % - add layout
    % - add draw call
    {noreply, State2};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info({textarea_size, H, W}, State = #state{windows = []}) ->
    State2 = #state{windows = Windows} = create_first_window(State, H, W),
    draw(Windows),
    {noreply, State2};
handle_info({textarea_size, H, W}, State) ->
    % TODO update window sizes
    {noreply, State#state{h = H, w = W}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

create_first_window(State = #state{next_id = Id}, H, W) ->
    Window = window(Id, _X = 0, _Y = 0, H, W),
    State#state{h = H,
                w = W,
                focused_window_id = Id,
                windows = [[Window]],
                next_id = Id + 1}.

window(Id, X, Y, H, W) ->
    % F = translate_fun(X, Y),
    {ok, Pid} = supervisor:start_child(cs_window_sup, [undefined, {H, W}]),
    #window{id = Id, pid = Pid, h = H, w = W}.

translate_fun(ScreenX, ScreenY) ->
    fun(WindowX, WindowY) ->
            {ScreenX + WindowX,
             ScreenY + WindowY}
    end.

get_window(FocusId, Windows) ->
    Index = #window.id,
    #window{} = lists:keyfind(FocusId, Index, Windows).

%% TODO move to split/4
split_vertical( State = #state{windows = Windows1,
                               focused_window_id = FocusId,
                               next_id = NextId,
                               h = H,
                               w = W}) ->
    Windows2 = split_window(FocusId, NextId, Windows1, vertical),
    Windows3 = layout_windows(Windows2, H, W),
    draw(Windows3),

    State#state{windows = Windows3,
                focused_window_id = NextId}.

rotate_right(_WindowId, Windows = []) ->
    Windows;
rotate_right(WindowId, Windows) ->
    rotate_right(WindowId, Windows, rows, []).


rotate_right(WindowId,
             [Row1 = [#window{id = WindowId}], Row2 | Rows],
             rows,
             RowAcc1) ->
    RowAcc1 ++ [Row2, Row1 | Rows];
rotate_right(WindowId,
             % maybe rows, maybe columns
             [Row | Rows],
             rows,
             RowAcc1) ->
    RotatedRow = rotate_right(WindowId,
                              Row,
                              columns,
                              []),
    RowAcc2 = RowAcc1 ++ [RotatedRow],
    rotate_right(WindowId, Rows, rows, RowAcc2);
rotate_right(WindowId,
             [W1 = #window{id = WindowId}, Col | Rest],
             columns,
             Acc) ->
    Acc ++ [Col, W1 | Rest];
rotate_right(WindowId,
             [W1 = #window{} | Rest],
             columns,
             Acc1) ->
    Acc2 = Acc1 ++ [W1],
    rotate_right(WindowId, Rest, columns, Acc2);
rotate_right(WindowId,
             [Row | Rest],
             columns,
             Acc1) ->
    RotatedRow = rotate_right(WindowId,
                              Row,
                              rows,
                              []),
    Acc2 = Acc1 ++ [RotatedRow],
    rotate_right(WindowId, Rest, columns, Acc2);
rotate_right(_WindowId,
             [],
             _row_or_column,
             Acc) ->
    Acc.

split_window(WindowId, NextId, Windows, Dir) ->
     split_window(WindowId, NextId, Windows, rows, Dir, []).

% [x, y]
% [[x], [y]]

split_window(WindowId,
             NextId,
             [ [W1 = #window{id = WindowId}] | Rest],
             rows,
             horizontal,
             Acc) ->
    W = #window{id = NextId},
    Acc ++ [[W1], [W]] ++ Rest;

split_window(WindowId,
             NextId,
             [Row | Rows],
             rows,
             Dir,
             Acc) ->
    NewRow =
        split_window(WindowId,
                     NextId,
                     Row,
                     columns,
                     Dir,
                     []),
      
    split_window(WindowId,
                 NextId,
                 Rows,
                 rows,
                 Dir,
                 Acc ++ [NewRow]);

% x is in a row (R) with one column (C2).
% That row (R) is in a column (C1).
% Since the row is only 1 column wide, and we're inserting
% another column below, we can just add another row to the
% column
%   C1
%    R                
%     C2
% [[[[x],[y]],z]]  There must be at least two rows in the  
% +---+---+        x,y column or else it would be
% | x |   |        [[[[x]]]], which doesn't make sense.
% +---+ z |        It would be simplified down to [[x]]
% | y |   |          
% +---+---+          
split_window(WindowId,
             NextId,
             [ [[W1 = #window{id = WindowId}] | RestR] | RestC],
             columns,
             horizontal,
             Acc) ->
    W = window(NextId, 0, 0, 0, 0),
    Acc ++ [[[W1], [W] | RestR]] ++ RestC;

split_window(WindowId,
             NextId,
             [W1 = #window{id = WindowId} | Rest],
             columns,
             Dir,
             Acc) ->
    W = window(NextId, 0, 0, 0, 0),
    case Dir of 
        vertical ->
            % [[[[x], [y]], z]]     [[[[x, *], [y]], z]]
            % +---+---+             +---+---+---+
            % | x |   |             | x | * |   |
            % +---+ z +     ->      +---+---+ z |
            % | y |   |             |   y   |   |
            % +-------+             +-------+---+
            Acc ++ [W1, W] ++ Rest;

        horizontal ->
            % [[x, z]]     [[[[x], [*]], z]]
            % +---+---+         +---+---+
            % | x | z |         | x |   |
            % +---+---+     ->  +---+ z |
            %                   | * |   |
            %                   +---+---+
            Acc ++ [[[W1], [W]]] ++ Rest
    end;

split_window(WindowId,
             NextId,
             [W = #window{} | Rest],
             columns,
             Dir,
             Acc) ->
    split_window(WindowId,
                 NextId,
                 Rest,
                 columns,
                 Dir,
                 Acc ++ [W]);

%  RCRC,C  RC   C
% [[[[x,a],[y]],z]]
% +---+---+---+
% | x | a |   |
% +---+---+ z |
% |   y   |   |
% +---+---+---+

split_window(WindowId,
             NextId,
             [ColumnRows | Rest],
             columns,
             Dir,
             Acc) when is_list(ColumnRows) ->
    NewRows = split_window(WindowId,
                           NextId,
                           ColumnRows,
                           rows,
                           Dir,
                           []),
    split_window(WindowId,
                 NextId,
                 Rest,
                 columns,
                 Dir,
                 Acc ++ [NewRows]);
split_window(_WindowId,
             _NextId,
             [],
             _ListType,
             _Dir,
             Acc) ->
    Acc.

%% Having a single percentage doesn't work AT ALL.
%% There are two percentages: height and width.
%% Rows take up height percentages. Columns take up width percentages.

% [[[[x],[y]],z],[a]]
%
% +---+---+
% | x |   |
% +---+ z |
% | y |   |
% +---+---+
% |   a   |
% +---+---+

layout_windows(Rows, Height, Width) ->
    layout_windows(Rows, Height, Width, 0, 0).

layout_windows(Rows, Height, Width, X, Y) ->
    N = length(Rows),
    H = Height div N,
    HR = Height rem N,
    layout_windows(Rows, rows, H, HR, Width, 0, X, Y, []).

layout_windows([LastRow1], rows, H, HR, Width, 0, X, Y, Acc) ->
    N = length(LastRow1),
    W = Width div N,
    WR = Width rem N,
    LastRow2 = layout_windows(LastRow1, cols, H + HR, 0, W, WR, X, Y, []),
    Acc ++ [LastRow2];

layout_windows([Row | Rest], rows, H, HR, Width, _WR, X, Y, Acc1) ->
    N = length(Row),
    W = Width div N,
    WR = Width rem N,
    Row2 = layout_windows(Row, cols, H, 0, W, WR, X, Y, []),
    Acc2 = Acc1 ++ [Row2],
    layout_windows(Rest, rows, H, HR, Width, 0, X, Y + H, Acc2);

layout_windows([Win1 = #window{}], cols, H, _HR, W, WR, X, Y, Acc) ->
    Win2 = Win1#window{h = H, w = W + WR, x = X, y = Y},
    Acc ++ [Win2];

layout_windows([Win1 = #window{} | Rest], cols, H, _HR, W, WR, X, Y, Acc1) ->
    Win2 = Win1#window{h = H, w = W, x = X, y = Y},
    Acc2 = Acc1 ++ [Win2],
    layout_windows(Rest, cols, H, 0, W, WR, X + W, Y, Acc2);

layout_windows([Col], cols, H, _HR, W, WR, X, Y, Acc) ->
    Acc ++ [layout_windows(Col, H, W + WR, X, Y)];

layout_windows([Col | Rest], cols, H, _HR, W, WR, X, Y, Acc1) ->
    Acc2 = Acc1 ++ [layout_windows(Col, H, W, X, Y)],
    layout_windows(Rest, cols, H, 0, W, WR, X, Y, Acc2).

draw([#window{pid = Pid, x = X, y = Y, w = W, h = H} | Rest]) ->
    gen_server:cast(Pid, {update, translate_fun(X, Y), W, H}),
    gen_server:cast(Pid, draw),
    draw(Rest);
draw([List | Rest]) ->
    draw(List),
    draw(Rest);
draw([]) ->
    ok.
