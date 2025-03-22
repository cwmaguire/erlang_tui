-module(erlang_tui).

-export([format/2]).
-export([print/2]).

-define(FEATURES,
        #{none => "0",
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
          bg_black => "40",
          bg_red => "41",
          bg_green => "42",
          bg_yellow => "43",
          bg_blue => "44",
          bg_magenta => "45",
          bg_cyan => "46",
          bg_white => "47"}).

-define(ESC, 27).
-define(CHAR_ATTRIBUTE, $m).
-define(CTRL_SEQ_INTRO, $[).

escape(Feature) ->
    #{Feature := Code} = ?FEATURES,
    [?ESC, ?CTRL_SEQ_INTRO, Code, ?CHAR_ATTRIBUTE].

format(Text, Features) ->
    Escapes = [escape(F) || F <- Features],
    Normal = escape(none),
    lists:flatten([Escapes, Text, Normal]).

print(Text, Features) ->
    io:format("~s", [format(Text, Features)]).
