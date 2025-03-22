-module(tui_fun).

-export([xo_table/2]).

% draw a table of X's and O's with contracsting foreground and background
% colors
%
% XOX
% OXO

-define(CR, 10).

xo_table(W, H) ->
    WhiteX = tui:format("X", [bg_white, fg_green]),
    BlackO = tui:format("O", [bg_black, fg_yellow]),
    Row1 = lists:flatten([[WhiteX, BlackO] || _ <- lists:seq(1, W)]),
    Row2 = lists:flatten([[BlackO, WhiteX] || _ <- lists:seq(1, W)]),
    Rows = lists:flatten([[Row1, ?CR, Row2, ?CR] || _ <- lists:seq(1, H)]),
    io:format("~s", [Rows]).
