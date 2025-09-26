-module(window_tree_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("cs.hrl").

-export([all/0]).

-export([test_rotate_horizontal/1]).
-export([test_split_window/1]).

all() ->
    [test_rotate_horizontal,
     test_split_window].

test_rotate_horizontal(_Config) ->
    Wx = #window{id = x},
    Wy = #window{id = y},
    Wz = #window{id = z},
    Wa = #window{id = a},
    % Wb = #window{id = b},

    W1 = [],
    E1 = [],
    A1 = cs_screen:rotate_right(x, W1),
    ?assertEqual(E1, A1),

    W2 = [[Wx]],
    E2 = [[Wx]],
    A2 = cs_screen:rotate_right(x, W2),
    ?assertEqual(E2, A2),

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

    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(cs_screen, split_window, 6, cx),

    W5 = [[Wx, Wz]],
    E5 = [[Wx, Wy, Wz]],
    A5 = cs_screen:split_window(x, y, W5, vertical),
    ?assertEqual(E5, A5),

    dbg:stop(),

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

    % dbg:tracer(),
    % dbg:p(all, c),
    % dbg:tpl(cs_screen, split_window, 6, cx),

    W7 = [[[[Wx], [Wz]], Wa]],
    E7 = [[[[Wx, Wy], [Wz]], Wa]],
    A7 = cs_screen:split_window(x, y, W7, vertical),
    ?assertEqual(E7, A7),

    % dbg:stop(),

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
