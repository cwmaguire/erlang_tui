-module(tui_fun).

-export([xo_table/2]).
-export([colors_by_index/0]).
-export([clock_start/0]).
-export([clock_stop/0]).

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

colors_by_index() ->
    ColorIndexes = [integer_to_list(I) || I <- lists:seq(1, 255)],
    F =
        fun(F, I, [C | Rest]) ->
            Padded = lists:flatten(io_lib:format("~5.. s", [C])),
            tui:print(Padded, [{fg, C}]),
            case I rem 10 of
                0 ->
                    tui:print([10], []);
                _ -> ok
            end,
            F(F, I + 1, Rest);
         (_, _, []) ->
                ok
        end,
    F(F, 1, ColorIndexes).

clock_start() ->
    Pid = spawn(fun clock/0),
    register(tui_clock, Pid).

clock_stop() ->
    tui_clock ! stop.

clock() ->
    case
        receive
            stop ->
                stop
        after 1000 ->
            ok
        end of
        stop ->
            ok;
        _ ->
            Time = format_time(),
            tui:pos("1", "1"),
            tui:print(Time, [bg_white, fg_green]),
            clock()
    end.

format_time() ->
    {{Y, M, D}, {H, Min, S}} = erlang:localtime(),
    Strings = [integer_to_list(I) || I <- [Y, M, D, H, Min, S]],
    io_lib:format("~s-~s-~s ~s:~s:~s", Strings).

