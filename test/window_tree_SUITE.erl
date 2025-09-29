-module(window_tree_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("cs.hrl").

-export([all/0]).

-export([test_rotate_right_down/1]).
-export([test_split_window/1]).
-export([test_layout_windows/1]).

-export([split_call/6]).
-export([split_return/3]).
-export([rotate_call/6]).
-export([rotate_return/3]).
-export([dbg_layout/0]).
-export([layout_call/6]).
-export([layout_return/3]).

all() ->
    [test_rotate_right_down,
     test_split_window,
     test_layout_windows].

% all() -> [test_split_window].

test_rotate_right_down(_Config) ->
    Wx = #window{id = x},
    Wy = #window{id = y},
    Wz = #window{id = z},
    Wa = #window{id = a},
    % Wb = #window{id = b},

    W1 = [],
    E1 = [],
    A1 = cs_screen:rotate_right(x, W1),
    ?assertEqual(E1, A1),

    State = {fun rotate_call/6,
             fun rotate_return/3,
             0},
    dbg:tracer(process, {fun convert_trace/2, State}),
    dbg:p(all, c),
    dbg:tpl(cs_screen,
            rotate_right,
            4,
            [{'_',[],[{return_trace},
                      {exception_trace},
                      {message,{caller_line}}]}]),

    W2 = [[Wx]],
    E2 = [[Wx]],
    A2 = cs_screen:rotate_right(x, W2),
    ?assertEqual(E2, A2),

    dbg:stop(),

    W3 = [[Wx, Wy]],
    E3 = [[Wy, Wx]],
    A3 = cs_screen:rotate_right(x, W3),
    ?assertEqual(E3, A3),

    %% no where to rotate to
    W4 = [[Wy, Wx]],
    E4 = [[Wy, Wx]],
    A4 = cs_screen:rotate_right(x, W4),
    ?assertEqual(E4, A4),

    W5 = [[Wx, Wy, Wz]],
    E5 = [[Wy, Wx, Wz]],
    A5 = cs_screen:rotate_right(x, W5),
    ?assertEqual(E5, A5),

    W6 = [[Wy, Wx, Wz]],
    E6 = [[Wy, Wz, Wx]],
    A6 = cs_screen:rotate_right(x, W6),
    ?assertEqual(E6, A6),

    W10 = [[Wx, Wy], [Wz]],
    E10 = [[Wy, Wx], [Wz]],
    A10 = cs_screen:rotate_right(x, W10),
    ?assertEqual(E10, A10),

    % Allow swapping a single column row with
    % another row
    W11 = [[Wx], [Wy, Wz]],
    E11 = [[Wy, Wz], [Wx]],
    A11 = cs_screen:rotate_right(x, W11),
    ?assertEqual(E11, A11),

    % +---+---+
    % |   | y |
    % | x +---+
    % |   | z |
    % +---+---+

    % Columns can be lists or windows
    % They are *only* lists, if they
    % contain rows

    % Allow switch single window columns
    % with other columns

    % +---+---+    +---+---+
    % |   | y |    | y |   |
    % | x +---+ -> +---+ x |
    % |   | z |    | z |   |
    % +---+---+    +---+---+

    W12 = [[Wx, [[Wy], [Wz]]]],
    E12 = [[[[Wy], [Wz]], Wx]],
    A12 = cs_screen:rotate_right(x, W12),
    ?assertEqual(E12, A12),

    %                          y
    % x is in the same row as --- so they can switch
    %                          z
    % +---+---+    +---+---+
    % |   | y |    | y |   |
    % | x +---+ -> +---+ x |
    % |   | z |    | z |   |
    % +---+---+    +---+---+
    % |   a   |    |   a   |
    % +-------+    +-------+

    W13 = [[Wx, [[Wy], [Wz]]], [Wa]],
    E13 = [[[[Wy], [Wz]], Wx], [Wa]],
    A13 = cs_screen:rotate_right(x, W13),
    ?assertEqual(E13, A13),

    ok.

test_split_window(_Config) ->
    Wx = #window{id = x},
    Wy = #window{id = y},
    Wz = #window{id = z},
    Wa = #window{id = a},

    W1 = [],
    E1 = [],
    A1 = cs_screen:split_window(x, y, W1, vertical),
    ?assertEqual(E1, A1),

    W2 = [],
    E2 = [],
    A2 = cs_screen:split_window(x, y, W2, horizontal),
    ?assertEqual(E2, A2),

    W3 = [[Wx]],
    E3 = [[Wx, Wy]],
    A3 = cs_screen:split_window(x, y, W3, vertical),
    ?assertEqual(E3, A3),

    W4 = [[Wx]],
    E4 = [[Wx], [Wy]],
    A4 = cs_screen:split_window(x, y, W4, horizontal),
    ?assertEqual(E4, A4),

    % [[x, z]]

    % +---+---+    +---+---+---+
    % | x | z | -> | x | y | z |
    % +---+---+    +---+---+---+

    W5 = [[Wx, Wz]],
    E5 = [[Wx, Wy, Wz]],
    A5 = cs_screen:split_window(x, y, W5, vertical),
    ?assertEqual(E5, A5),

    % [x,z]     -> [[[x,y], z]]

    % +---+---+    +---+---+
    % | x | z | -> | x |   |
    % +---+---+    +---+ z |
    %              | y |   |
    %              +---+---+

    W6 = [[Wx, Wz]],
    E6 = [[[[Wx], [Wy]], Wz]],
    A6 = cs_screen:split_window(x, y, W6, horizontal),
    ?assertEqual(E6, A6),

    % [[[x,z], a]] ->   [[[[x, y], z], a]]
    %
    % +---+---+         +---+---+---+
    % | x |   |         | x | y |   |
    % +---+ a |    ->   +---+---+ a |
    % | z |   |         |   z   |   |
    % +---+---+         +-------+---+

    State = {fun split_call/6, fun split_return/3, 0},
    dbg:tracer(process, {fun convert_trace/2, State}),
    dbg:p(all, c),
    dbg:tpl(cs_screen,
            split_window,
            6,
            [{'_',[],[{return_trace},
                      {exception_trace},
                      {message,{caller_line}}]}]),

    W7 = [[[[Wx], [Wz]], Wa]],
    E7 = [[[[Wx, Wy], [Wz]], Wa]],
    A7 = cs_screen:split_window(x, y, W7, vertical),
    ?assertEqual(E7, A7),

    dbg:stop(),

    % [[[x,z], a]] ->   [[[[x, y], z], a]]
    %
    % +---+---+         +---+---+
    % | x |   |         | x |   |
    % +---+ a |    ->   +---+   |
    % | z |   |         | y | a |
    % +---+---+         +---+   |
    %                   | z |   |
    %                   +---+---+

    W8 = [[[[Wx], [Wz]], Wa]],
    E8 = [[[[Wx], [Wy], [Wz]], Wa]],
    A8 = cs_screen:split_window(x, y, W8, horizontal),
    ?assertEqual(E8, A8).

test_layout_windows(_Config) ->
    Wx = #window{id = x},
    Wy = #window{id = y},
    Wz = #window{id = z},
    Wa = #window{id = a},
    Wb = #window{id = b},

    W1 = [[Wx]],
    E1 = [[#window{id = x, w = 1, h = 1, x = 0, y = 0}]],
    A1 = cs_screen:layout_windows(W1, 1, 1),
    ?assertEqual(E1, A1),

    W2 = [[Wx, Wy]],
    E2 = [[#window{id = x, w = 1, h = 1, x = 0, y = 0},
           #window{id = y, w = 1, h = 1, x = 1, y = 0}]],
    A2 = cs_screen:layout_windows(W2, 1, 2),
    ?assertEqual(E2, A2),

    % dbg:stop(),

    % Increase all numbers, jumps over lines with
    % searches
    % j/^ *A:nohlj0l?^  *W\d:noh0

    % Do the same, but slowly
    % :sleep 200mj:sleep 200m:sleep 200m/^ *A:sleep 200m:noh:sleep 200m:sleep 200ml:sleep 200m:sleep 200mj:sleep 200m0:sleep 200m:sleep 200ml:sleep 200m:sleep 200m?^  *W\d:sleep 200m:noh:sleep 200m0


    W3 = [[Wx, Wy, Wz]],
    E3 = [[#window{id = x, w = 1, h = 1, x = 0, y = 0},
           #window{id = y, w = 1, h = 1, x = 1, y = 0},
           #window{id = z, w = 2, h = 1, x = 2, y = 0}]],
    A3 = cs_screen:layout_windows(W3, 1, 4),
    ?assertEqual(E3, A3),

    W4 = [[Wx, Wy], [Wz]],
    E4 = [[#window{id = x, w = 2, h = 1, x = 0, y = 0},
           #window{id = y, w = 2, h = 1, x = 2, y = 0}],
          [#window{id = z, w = 4, h = 1, x = 0, y = 1}]],
    A4 = cs_screen:layout_windows(W4, 2, 4),
    ?assertEqual(E4, A4),

    W5 = [[Wx, Wy], [Wz]],
    E5 = [[#window{id = x, w = 2, h = 1, x = 0, y = 0},
           #window{id = y, w = 3, h = 1, x = 2, y = 0}],
          [#window{id = z, w = 5, h = 1, x = 0, y = 1}]],
    A5 = cs_screen:layout_windows(W5, 2, 5),
    ?assertEqual(E5, A5),

    W6 = [[Wx, Wy], [Wz]],
    E6 = [[#window{id = x, w = 2, h = 1, x = 0, y = 0},
           #window{id = y, w = 2, h = 1, x = 2, y = 0}],
          [#window{id = z, w = 4, h = 2, x = 0, y = 1}]],
    A6 = cs_screen:layout_windows(W6, 3, 4),
    ?assertEqual(E6, A6),

    % +---+---+---+
    % |   |   | a |
    % | x | y +---+
    % |   |   | b |
    % +---+---+---+
    % |     z     |
    % +---+---+---+

    dbg_layout(),

    W8 = [[Wx, Wy, [[Wa], [Wb]]], [Wz]],
    E8 = [[#window{id = x, w = 3, h = 4, x = 0, y = 0},
           #window{id = y, w = 3, h = 4, x = 3, y = 0},
           [[#window{id = a, w = 4, h = 2, x = 6, y = 0}],
            [#window{id = b, w = 4, h = 2, x = 6, y = 2}]]],
          [#window{id = z, w = 10, h = 5, x = 0, y = 4}]],
    A8 = cs_screen:layout_windows(W8, 9, 10),
    ?assertEqual(E8, A8),

    dbg:stop(),

    ok.

dbg_layout() ->
    State = {fun layout_call/6,
             fun layout_return/3,
             0},
    dbg:tracer(process, {fun convert_trace/2, State}),
    dbg:p(all, c),
    dbg:tpl(cs_screen,
            layout_windows,
            7,
            [{'_',[],[{return_trace},
                      {exception_trace},
                      {message,{caller_line}}]}]).

rotate_call(Spaces,
            M,
            F,
            BaseName,
            Line,
            [_, Windows, Orientation, Acc]) ->
    Converted = convert_windows(Windows, []),
    AccConverted = convert_windows(Acc, []),

    ct:pal("~s~p:~p(~p, ~p, ~p) [~p:~p]~n",
           [Spaces,
            M,
            F,
            Converted,
            Orientation,
            AccConverted,
            BaseName,
            Line]).

%% same as split return
rotate_return(Spaces, MFA, Return) ->
    Converted = convert_windows(Return, []),
    ct:pal("~sreturn: ~p - ~p~n", [Spaces, MFA, Converted]).

split_call(Spaces,
           M,
           F,
           BaseName,
           Line,
           [_, _, Windows, Orientation, Direction, Acc]) ->
    Converted = convert_windows(Windows, []),
    AccConverted = convert_windows(Acc, []),

    ct:pal("~s~p:~p(~p, ~p, ~p, ~p) [~p:~p]~n",
           [Spaces,
            M,
            F,
            Converted,
            Orientation,
            Direction,
            AccConverted,
            BaseName,
            Line]).

split_return(Spaces, MFA, Return) ->
    Converted = convert_windows(Return, []),
    ct:pal("~sreturn: ~p - ~p~n", [Spaces, MFA, Converted]).

layout_call(Spaces,
            M,
            F,
            BaseName,
            Line,
            [Windows, Orientation, H, HR, W, WR, Acc]) ->
    Converted = convert_windows(Windows, []),
    AccConverted = convert_windows(Acc, []),

    ct:pal("~s~p:~p(~p, ~p, ~p, ~p, ~p, ~p, ~p) [~p:~p]~n",
           [Spaces,
            M,
            F,
            Converted,
            Orientation,
            H,
            HR,
            W,
            WR,
            AccConverted,
            BaseName,
            Line]).

layout_return(Spaces, MFA, Return) ->
    Converted = convert_windows(Return, []),
    ct:pal("~sreturn: ~p - ~p~n", [Spaces, MFA, Converted]).


convert_trace({trace, _Pid, return_from, MFA, Return},
              {CallFun, ReturnFun, Indent}) ->
    Unindented = Indent - 4,
    Spaces = indent(Unindented),
    ReturnFun(Spaces, MFA, Return),
    {CallFun, ReturnFun, Unindented};
convert_trace({trace,
               _Pid,
               call,
               {M, F, Args},
               MFA_Path_Line},
              {CallFun, ReturnFun, Indent}) ->
    Spaces = indent(Indent),
    {_, _, _, {Path, Line}} = MFA_Path_Line,
    BaseName = filename:basename(Path, ".erl"),
    CallFun(Spaces, M, F, BaseName, Line, Args),
    {CallFun, ReturnFun, Indent + 4}.

-define(SPACE, $ ).

indent(N) ->
    indent(N, []).

indent(0, Spaces) ->
    Spaces;
indent(N, Spaces) ->
    indent(N - 1, [?SPACE | Spaces]).

convert_windows([#window{id = Id, h = 0, w = 0} | Rest], Acc) ->
    convert_windows(Rest, Acc ++ [Id]);
convert_windows([#window{id = Id, h = H, w = W} | Rest], Acc) ->
    convert_windows(Rest, Acc ++ [{Id, H, W}]);
convert_windows([List | Rest], Acc) ->
    Converted = convert_windows(List, []),
    convert_windows(Rest, Acc ++ [Converted]);
convert_windows(#window{id = Id, h = 0, w = 0}, Acc) ->
    Acc ++ [Id];
convert_windows(#window{id = Id, h = H, w = W}, Acc) ->
    Acc ++ [{Id, H, W}];
convert_windows([], Acc) ->
    Acc.

% {
%   trace,
%   0.342.0>,call,
%   {cs_screen,
%    split_window,
%    [x,y,[],rows,vertical,
%                             [[[[{window,x,undefined,0,0,0,0},
%                                 {window,y,undefined,0,0,0,0}],
%                                [[{window,x,undefined,0,0,0,0},
%                                  {window,y,undefined,0,0,0,0}],
%                                 {window,z,undefined,0,0,0,0}]],
%                               {window,a,undefined,0,0,0,0}]]]
%   },
%   {
%    window_tree_SUITE,test_split_window,1,
%    {"Module path", 178}
%   }
% }

% {trace,
%  <0.342.0>,
%  return_from,
%  {cs_screen,split_window,6},
%  [
%   [[[{window,x,undefined,0,0,0,0},{window,y,undefined,0,0,0,0}],
%                  [[{window,x,undefined,0,0,0,0},{window,y,undefined,0,0,0,0}],
%                   {window,z,undefined,0,0,0,0}]],
%                 {window,a,undefined,0,0,0,0}]
%  ]}
