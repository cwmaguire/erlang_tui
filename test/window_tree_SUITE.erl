-module(window_tree_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("cs.hrl").

-export([all/0]).

-export([test_rotate_horizontal/1]).

all() ->
    [test_rotate_horizontal].

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

    W2 = [Wx],
    E2 = [Wx],
    A2 = cs_screen:rotate_right(x, W2),
    ?assertEqual(E2, A2),

    W3 = [Wx, Wy],
    E3 = [Wy, Wx],
    A3 = cs_screen:rotate_right(x, W3),
    ?assertEqual(E3, A3),

    %% no where to rotate to
    W4 = [Wy, Wx],
    E4 = [Wy, Wx],
    A4 = cs_screen:rotate_right(x, W4),
    ?assertEqual(E4, A4),

    W5 = [Wx, Wy, Wz],
    E5 = [Wy, Wx, Wz],
    A5 = cs_screen:rotate_right(x, W5),
    ?assertEqual(E5, A5),

    W6 = [Wy, Wx, Wz],
    E6 = [Wy, Wz, Wx],
    A6 = cs_screen:rotate_right(x, W6),
    ?assertEqual(E6, A6),

    % no where to rotate to
    W7 = [Wy, Wz, Wx],
    E7 = [Wy, Wz, Wx],
    A7 = cs_screen:rotate_right(x, W7),
    ?assertEqual(E7, A7),

    % cols
    W8 = [[Wx, Wy]],
    E8 = [[Wy, Wx]],
    A8 = cs_screen:rotate_right(x, W8),
    ?assertEqual(E8, A8),

    W9 = [[Wx, Wy], Wz],
    E9 = [[Wy, Wx], Wz],
    A9 = cs_screen:rotate_right(x, W9),
    ?assertEqual(E9, A9),

    W10 = [Wx, [Wy, Wz]],
    E10 = [[Wy, Wz], Wx],
    A10 = cs_screen:rotate_right(x, W10),
    ?assertEqual(E10, A10),

    W11 = [[Wx, [Wy, Wz]]],
    E11 = [[[Wy, Wz], Wx]],
    A11 = cs_screen:rotate_right(x, W11),
    ?assertEqual(E11, A11),

    W12 = [[Wx, [Wy, Wz]], Wa],
    E12 = [[[Wy, Wz], Wx], Wa],
    A12 = cs_screen:rotate_right(x, W12),
    ?assertEqual(E12, A12),

    ok.
