-module(tui).

-export([escape/1]).
-export([format/2]).
-export([print/2]).
-export([cursor/1]).
-export([pos/2]).
-export([repeat_prev_char/1]).
-export([repeat_char/2]).

% * = doesn't work in gnome-terminal
-define(FEATURES,
        #{none => "0",
          none_16 => "100",
          bold => "1",
          faint => "2",
          italic => "3",
          underline => "4",
          blink => "5",
          inverse => "7",
          invis => "8",
          crossed_out => "9",
          double_underline => "21",
          normal => "22",
          fg_black => "30",
          fg_red => "31",
          fg_green => "32",
          fg_yellow => "33",
          fg_blue => "34",
          fg_magenta => "35",
          fg_cyan => "36",
          fg_white => "37",
          fg_dark_red => "91",
          fg_grey => "92",
          fg_dark_yellow => "93", % *
          fg_dark_blue => "94", %   *
          fg_dark_magenta => "95",
          fg_dark_cyan => "96", %   *
          bg_black => "40",
          bg_red => "41",
          bg_green => "42",
          bg_yellow => "43",
          bg_blue => "44",
          bg_magenta => "45",
          bg_cyan => "46",
          bg_white => "47",
          bg_black_16 => "101",
          bg_orange => "101",
          bg_dark_green => "102", % grey
          bg_dark_yellow => "103", % grey
          bg_dark_blue => "104", % light grey
          bg_dark_magenta => "105", % purple
          bg_dark_cyan => "106", % grey
          bg_white_16 => "107"
         }).

-define(ESC, 27).
-define(CTRL_SEQ_INTRO, $[).
-define(CHAR_ATTRIBUTE, $m).
-define(FG_RGB, "38:2:").
-define(BG_RGB, "48:2:").
-define(INDEXED_FG_COLOR, "38:5:").
-define(INDEXED_BG_COLOR, "48:5:").

escape({fg, IndexedColor}) when is_list(IndexedColor) ->
    % "0" to "255"
    [?ESC, ?CTRL_SEQ_INTRO, ?INDEXED_FG_COLOR, IndexedColor, ?CHAR_ATTRIBUTE];
escape({bg, IndexedColor}) when is_list(IndexedColor) ->
    % "0" to "255"
    [?ESC, ?CTRL_SEQ_INTRO, ?INDEXED_BG_COLOR, IndexedColor, ?CHAR_ATTRIBUTE];
escape({fg, R, G, B}) ->
    % "0" to "255" or R, G, and B
    [?ESC, ?CTRL_SEQ_INTRO, ?FG_RGB, R, $:, G, $:, B, ?CHAR_ATTRIBUTE];
escape({bg, R, G, B}) ->
    % "0" to "255" or R, G, and B
    [?ESC, ?CTRL_SEQ_INTRO, ?BG_RGB, R, $:, G, $:, B, ?CHAR_ATTRIBUTE];
escape(Feature) ->
    #{Feature := Code} = ?FEATURES,
    [?ESC, ?CTRL_SEQ_INTRO, Code, ?CHAR_ATTRIBUTE].

format(Text, Features) ->
    Escapes = [escape(F) || F <- Features],
    Normal = escape(none),
    %Normal16 = escape(none_16),
    lists:flatten([Escapes, Text, Normal]).

print(Text, Features) ->
    io:format("~s", [format(Text, Features)]).

-define(CURSORS,
        #{blinking_block => "0",
          blinking_block_default => "1",
          steady_block => "2",
          blinking_underline => "3",
          steady_underline => "4",
          blinking_bar => "5",
          steady_bar => "6"}).

-define(SPACE, $ ).
-define(CURSOR, $q).

cursor(Cursor) ->
    #{Cursor := Code} = ?CURSORS,
    EscapeCommand = [?ESC, ?CTRL_SEQ_INTRO, Code, ?SPACE, ?CURSOR],
    io:format("~s", [EscapeCommand]).

-define(POS, $f).

pos(X, Y) ->
    EscapeCommand = [?ESC, ?CTRL_SEQ_INTRO, X, $;, Y, ?POS],
    io:format("~s", [EscapeCommand]).

-define(REPEAT, $b).

% doesn't seem to work
repeat_prev_char(Count) ->
    EscapeCommand = [?ESC, ?CTRL_SEQ_INTRO, Count, ?REPEAT],
    io:format("~s", [EscapeCommand]).

repeat_char(Char, Count) ->
    EscapeCommand = [?ESC, ?CTRL_SEQ_INTRO, Count, ?REPEAT],
    io:format("~s~s", [Char, EscapeCommand]).
