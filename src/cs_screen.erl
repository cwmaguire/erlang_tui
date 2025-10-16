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

-export([focus/1]).
-export([focus_/2]).
-export([text/1]).
-export([split_window/5]).
-export([rotate_right/2]).
-export([layout_windows/3]).
-export([delete/0]).

-record(state, {windows = [],
                focused_window_id,
                focused_window_pid,
                next_id = 1,
                h = 0,
                w = 0,
                notify_fun = fun gen_server:cast/2}).

focus(Direction) ->
    gen_server:cast(?MODULE, {focus, Direction}).

-define(ESC, 27).

text(?ESC) ->
    gen_server:cast(?MODULE, {text, "ESC"});
text(Char) ->
    gen_server:cast(?MODULE, {text, [Char]}).

delete() ->
    gen_server:cast(?MODULE, delete).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    pg:join(textarea_size, self()),
    {ok, #state{}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({focus, Direction}, State1) ->
    State2 = focus_(Direction, State1),
    {noreply, State2};
handle_cast({text, Text}, State = #state{focused_window_pid = Pid}) ->
    % io:format("Pid: ~p", [Pid]),
    gen_server:cast(Pid, {text, Text}),
    {noreply, State};
handle_cast(split_vertical, State1) ->
    State2 = split_window_(vertical,State1),
    {noreply, State2};
handle_cast(split_horizontal, State1) ->
    State2 = split_window_(horizontal, State1),
    {noreply, State2};
handle_cast(delete, State = #state{focused_window_pid = Pid}) ->
    gen_server:cast(Pid, delete),
    {noreply, State};
handle_cast(print_windows, State = #state{windows = Windows}) ->
    CWindows = convert_windows(Windows, []),
    Text = io_lib:format("Windows: ~p", [CWindows]),
    gen_server:cast(cs_io, {debug, Text, 2, 10}),
    {noreply, State};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info({textarea_size, H, W}, State = #state{windows = []}) ->
    State2 = #state{windows = Windows} = create_first_window(State, H, W),
    cs_io:clear_screen(),
    draw(Windows),
    {noreply, State2};
handle_info({textarea_size, H, W}, State) ->
    {noreply, State#state{h = H, w = W}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

create_first_window(State = #state{next_id = Id}, H, W) ->
    Window = #window{pid = Pid} = window(Id, _X = 0, _Y = 0, H, W),
    State#state{h = H,
                w = W,
                focused_window_id = Id,
                focused_window_pid = Pid,
                windows = [[Window]],
                next_id = Id + 1}.

window(Id, _X, _Y, H, W) ->
    % F = translate_fun(X, Y),
    {ok, Pid} = supervisor:start_child(cs_window_sup, [undefined, {H, W}]),
    #window{id = Id, pid = Pid, h = H, w = W}.

translate_fun(ScreenX, ScreenY) ->
    fun(WindowX, WindowY) ->
            {ScreenX + WindowX,
             ScreenY + WindowY}
    end.

%% get_window(FocusId, Windows) ->
%%     Index = #window.id,
%%     #window{} = lists:keyfind(FocusId, Index, Windows).

split_window_(Direction,
              State = #state{windows = Windows1,
                             focused_window_id = FocusId,
                             next_id = NextId,
                             h = H,
                             w = W}) ->
    Windows2 = split_window(FocusId, NextId, Windows1, Direction, fun window/5),
    Windows3 = layout_windows(Windows2, H, W),
    cs_io:clear_screen(),
    draw(Windows3),

    [#window{pid = FocusedWindowPid}] =
        lists:filter(
            fun(#window{id = NextId_}) when NextId_ == NextId ->
                true;
                (_) ->
                false
            end,
            lists:flatten(Windows3)),

    State#state{windows = Windows3,
                focused_window_id = NextId,
                focused_window_pid = FocusedWindowPid,
                next_id = NextId + 1}.

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

split_window(WindowId, NextId, Windows, Dir, NewWindow) ->
     split_window(WindowId, NextId, Windows, rows, Dir, NewWindow, []).

% [x, y]
% [[x], [y]]

split_window(WindowId,
             NextId,
             [ [W1 = #window{id = WindowId}] | Rest],
             rows,
             horizontal,
             NewWindow,
             Acc) ->
    W = NewWindow(NextId, 0, 0, 0, 0),
    Acc ++ [[W1], [W]] ++ Rest;

split_window(WindowId,
             NextId,
             [Row | Rows],
             rows,
             Dir,
             NewWindow,
             Acc) ->
    NewRow =
        split_window(WindowId,
                     NextId,
                     Row,
                     columns,
                     Dir,
                     NewWindow,
                     []),

    split_window(WindowId,
                 NextId,
                 Rows,
                 rows,
                 Dir,
                 NewWindow,
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
             NewWindow,
             Acc) ->
    W = NewWindow(NextId, 0, 0, 0, 0),
    Acc ++ [[[W1], [W] | RestR]] ++ RestC;

split_window(WindowId,
             NextId,
             [W1 = #window{id = WindowId} | Rest],
             columns,
             Dir,
             NewWindow,
             Acc) ->
    W = NewWindow(NextId, 0, 0, 0, 0),
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
             NewWindow,
             Acc) ->
    split_window(WindowId,
                 NextId,
                 Rest,
                 columns,
                 Dir,
                 NewWindow,
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
             NewWindow,
             Acc) when is_list(ColumnRows) ->
    NewRows = split_window(WindowId,
                           NextId,
                           ColumnRows,
                           rows,
                           Dir,
                           NewWindow,
                           []),
    split_window(WindowId,
                 NextId,
                 Rest,
                 columns,
                 Dir,
                 NewWindow,
                 Acc ++ [NewRows]);
split_window(_WindowId,
             _NextId,
             [],
             _ListType,
             _Dir,
             _NewWindow,
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
    layout_windows(Rows, Height, Width, 0, 0, false).

layout_windows(Rows, Height, Width, X, Y, HasBorder) ->
    N = length(Rows),
    H = Height div N,
    HR = Height rem N,
    layout_windows(Rows, rows, H, HR, Width, 0, X, Y, HasBorder, []).

layout_windows([LastRow1], rows, H, HR, Width, 0, X, Y, HasBorder, Acc) ->
    N = length(LastRow1),
    W = Width div N,
    WR = Width rem N,
    LastRow2 = layout_windows(LastRow1, cols, H + HR, 0, W, WR, X, Y, HasBorder, []),
    Acc ++ [LastRow2];

layout_windows([Row | Rest], rows, H, HR, Width, _WR, X, Y, HasBorder, Acc1) ->
    N = length(Row),
    W = Width div N,
    WR = Width rem N,
    Row2 = layout_windows(Row, cols, H, 0, W, WR, X, Y, HasBorder, []),
    Acc2 = Acc1 ++ [Row2],
    layout_windows(Rest, rows, H, HR, Width, 0, X, Y + H, HasBorder, Acc2);

layout_windows([Win1 = #window{}], cols, H, _HR, W, WR, X, Y, HasBorder, Acc) ->
    Win2 = Win1#window{h = H, w = W + WR, x = X, y = Y, has_border = HasBorder},
    Acc ++ [Win2];

layout_windows([Win1 = #window{} | Rest], cols, H, _HR, W, WR, X, Y, HasBorder, Acc1) ->
    Win2 = Win1#window{h = H, w = W, x = X, y = Y, has_border = HasBorder},
    Acc2 = Acc1 ++ [Win2],
    layout_windows(Rest, cols, H, 0, W, WR, X + W, Y, true, Acc2);

layout_windows([Col], cols, H, _HR, W, WR, X, Y, HasBorder, Acc) ->
    Acc ++ [layout_windows(Col, H, W + WR, X, Y, HasBorder)];

layout_windows([Col | Rest], cols, H, _HR, W, WR, X, Y, HasBorder, Acc1) ->
    Acc2 = Acc1 ++ [layout_windows(Col, H, W, X, Y, HasBorder)],
    layout_windows(Rest, cols, H, 0, W, WR, X, Y, true, Acc2).

draw([#window{pid = Pid,
              x = X,
              y = Y,
              w = W,
              h = H,
              has_border = HasBorder} | Rest]) ->
    gen_server:cast(Pid, {update, translate_fun(X, Y), W, H, HasBorder}),
    gen_server:cast(Pid, draw),
    draw(Rest);
draw([List | Rest]) ->
    draw(List),
    draw(Rest);
draw([]) ->
    ok.

focus_(Direction, State = #state{windows = Windows,
                                 focused_window_id = Id,
                                 notify_fun = NotifyFun}) ->
    Result = focus_(Direction, Id, Windows, undefined),
    case {Direction, Result} of
        {_, {done, {FocusId, FocusPid}}} ->
            NotifyFun(FocusPid, focus),
            State#state{focused_window_id = FocusId,
                        focused_window_pid = FocusPid};
        {_, _} ->
            State
    end.

focus_(left, Id, #window{id = Id, pid = Pid}, undefined) ->
    {done, {Id, Pid}};
focus_(left, Id, #window{id = Id}, {FocusId, FocusPid}) ->
    {done, {FocusId, FocusPid}};
focus_(left, _Id, #window{id = Id, pid = Pid}, _) ->
    {Id, Pid};
focus_(left, Id, List, LastIdPid) when is_list(List) ->
    Result =
        lists:foldl(fun(_Child, {done, IdPid}) ->
                            {done, IdPid};
                       (Child, IdPid) ->
                        focus_(left, Id, Child, IdPid)
                    end,
                    LastIdPid,
                    List),
    case Result of
        {done, IdPid} ->
            {done, IdPid};
        _ ->
            LastIdPid
    end;
focus_(right, Id, #window{id = Id}, undefined) ->
    {found, column};
focus_(right, _, #window{}, undefined) ->
    undefined;
focus_(right, _, #window{id = Id, pid = Pid}, {found, _}) ->
    {done, {Id, Pid}};
focus_(right, _Id, List, {found, row}) when is_list(List) ->
    {found, row};
focus_(right, Id, List, MaybeFound) when is_list(List) ->
    Result =
        lists:foldl(fun(_Child, {done, IdPid}) ->
                            {done, IdPid};
                       (Child, MaybeFound_) ->
                        focus_(right, Id, Child, MaybeFound_)
                    end,
                    MaybeFound,
                    List),
    case Result of
        {found, column} ->
            {found, row};
        {found, row} ->
            {found, column};
        Other ->
            Other
    end;

focus_(up, Id, Windows, undefined) ->
    #up{new = Window} = focus_(up, Id, Windows, #up{type = col}),
    {done, Window};
focus_(up,
       Id,
       #window{id = Id, pid = Pid},
       #up{parent = undefined}) ->
    #up{new = {Id, Pid}};
focus_(up,
       Id,
       #window{id = Id},
       #up{parent = {PId, PPid}}) ->
    #up{new = {PId, PPid}};
focus_(up,
       _Id,
       #window{id = Id, pid = Pid},
       Up = #up{}) ->
    Up#up{primary = {Id, Pid}, parent = {Id, Pid}};
focus_(up,
       Id,
       _Column = [Row | Rest],
       Up = #up{type = col,
                pos = first,
                primary = _OrigPrimary}) ->
    #up{primary = Primary, new = New} =
        focus_(up,
               Id,
               Row,
               Up#up{type = row,
                     pos = first,
                     primary = undefined}),
    Up2 =
        lists:foldl(fun(Row_, Acc) ->
                        focus_(up, Id, Row_, Acc)
                    end,
                    #up{type = row,
                        pos = rest,
                        primary = undefined,
                        parent = Primary,
                        new = New},
                    Rest),
    Up2#up{type = col,
           pos = first,
           parent = Up#up.parent,
           primary = Primary};
focus_(up,
       Id,
       _Column = [Row | Rest],
       Up = #up{type = col,
                pos = rest}) ->
    #up{primary = Primary, new = New} =
        focus_(up, Id, Row, Up#up{type = row, pos = first, primary = undefined}),
    Up2 =
        lists:foldl(fun(Row_, Acc) ->
                        focus_(up, Id, Row_, Acc)
                    end,
                    Up#up{type = row,
                          primary = undefined,
                          parent = Primary,
                          pos = rest,
                          new = New},
                    Rest),
    Up2#up{type = col, pos = rest, parent = Primary};
focus_(up,
       Id,
       _Row = [Col | Rest],
       Up = #up{type = row,
                pos = first}) ->
    #up{new = New, primary = Primary} = focus_(up, Id, Col, Up#up{type = col}),
    Up2 =
        lists:foldl(fun(Col_, Acc) ->
                        focus_(up, Id, Col_, Acc)
                    end,
                    #up{type = col,
                        primary = Primary, % row primary already set
                        pos = rest,
                        new = New},
                    Rest),
    Up2#up{type = row,
           pos = first,
           primary = Primary,
           parent = Primary};
focus_(up,
       Id,
       _Row = [ColOrWin | Rest],
       Up = #up{type = row,
                pos = rest}) ->
    Up1 = #up{primary = Primary, new = New} = focus_(up, Id, ColOrWin, Up#up{type = col, pos = first}),
    Up2 =
        lists:foldl(fun(ColOrWin_, Acc) ->
                        focus_(up, Id, ColOrWin_, Acc)
                    end,
                    Up#up{type = col, primary= Primary, pos = rest, new = New},
                    Rest),
    Up2#up{type = row, pos = rest, parent = Up1#up.parent};

focus_(down, Id, Rows, undefined) ->
    focus_(down, Id, Rows, {false, col});
focus_(down,
       Id,
       #window{id = Id},
       {false, _}) ->
    {true, col};
focus_(down,
       _Id,
       #window{id = Id, pid = Pid},
       {true, col}) ->
    {done, {Id, Pid}};
focus_(down,
       _Id,
       #window{},
       _Acc) ->
    {false, col};
focus_(down,
       _Id,
       _Windows,
       {done, {Id, Pid}}) ->
    {done, {Id, Pid}};
focus_(down,
       _Id,
       _Rows,
       {skip, col}) ->
    {skip, col};
focus_(down,
       Id,
       Rows,
       {Bool, col}) ->
    Result =
        lists:foldl(
            fun(Row, Acc_) ->
                focus_(down, Id, Row, Acc_)
            end,
            {Bool, row},
            Rows),
    case Result of
        {true, _} ->
            {skip, col};
        {done, _} ->
            Result;
        {Bool_, _} ->
            {Bool_, col}
    end;
focus_(down,
       Id,
       Columns,
       {Bool, row}) ->
    Result =
        lists:foldl(
            fun(Col, Acc_) ->
                focus_(down, Id, Col, Acc_)
            end,
            {Bool, col},
            Columns),
    case Result of
        {skip, col} ->
            {true, row};
        {done, _} ->
            Result;
        {Bool_, _} ->
            {Bool_, row}
    end.


convert_windows([#window{id = Id} | Rest], Acc) ->
    convert_windows(Rest, Acc ++ [Id]);
convert_windows([List | Rest], Acc) ->
    Converted = convert_windows(List, []),
    convert_windows(Rest, Acc ++ [Converted]);
convert_windows([], Acc) ->
    Acc.
