-module(cs_esc).

%        https://invisible-island.net/xterm/ctlseqs/ctlseqs.html
%                        
%                        XTerm Control Sequences
%
%                               Edward Moy
%                   University of California, Berkeley
%
%                               Revised by
%
%                             Stephen Gildea
%                          X Consortium (1994)
%
%                             Thomas Dickey
%                      XFree86 Project (1996-2006)
%                    invisible-island.net (2006-2025)
%               updated for XTerm Patch #401 (2025/06/22)

-export([alternate_screen_buffer/1]).
-export([show_cursor/1]).
-export([xterm_name_version/0]).

-export([fg_idx/1]).
-export([bg_idx/1]).
-export([fg_rbg/3]).
-export([bg_rgb/3]).
-export([set_feature/1]).
-export([device_status_report/0]).
-export([get_cursor_position/0]).
-export([get_screen_size/0]).
-export([get_textarea_size/0]).

-export([format/2]).
-export([cursor_type/1]).
-export([cursor_pos/2]).
-export([repeat_prev_char/1]).
-export([repeat_char/2]).

-export([parse_escape_code/1]).
-export([parse_escape_code/2]).

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

-define(CURSORS,
        #{blinking_block => "0",
          blinking_block_default => "1",
          steady_block => "2",
          blinking_underline => "3",
          steady_underline => "4",
          blinking_bar => "5",
          steady_bar => "6"}).

-define(ALT_SCREEN_BUFFER, "1049").
-define(ALT_SCREEN_BUFFER_ON, $h).
-define(ALT_SCREEN_BUFFER_OFF, $l).

-define(CURSOR_ON_OFF, "25").
-define(CURSOR_ON, $h).
-define(CURSOR_OFF, $l).


-define(CURSOR_POS, $H).
-define(SPACE, $ ).
-define(CURSOR, $q).
-define(ESC, 27).
-define(CTRL_SEQ_INTRO, $[).
%-define(DEVICE_CTRL_STRING, $P).
%-define(STRING_TERMINATOR, $\).
%-define(OS_CMD, $]).
%-define(APP_PROGRAM_CMD, $_).
-define(CHAR_ATTRIBUTE, $m).
-define(REPEAT, $b).
-define(FG_RGB, "38:2:").
-define(BG_RGB, "48:2:").
-define(INDEXED_FG_COLOR, "38:5:").
-define(INDEXED_BG_COLOR, "48:5:").

alternate_screen_buffer(true) ->
    [?ESC, ?CTRL_SEQ_INTRO, $?, ?ALT_SCREEN_BUFFER, ?ALT_SCREEN_BUFFER_ON];
alternate_screen_buffer(false) ->
    [?ESC, ?CTRL_SEQ_INTRO, $?, ?ALT_SCREEN_BUFFER, ?ALT_SCREEN_BUFFER_OFF].

show_cursor(true) ->
    [?ESC, ?CTRL_SEQ_INTRO, $?, ?CURSOR_ON_OFF, ?CURSOR_ON];
show_cursor(false) ->
    [?ESC, ?CTRL_SEQ_INTRO, $?, ?CURSOR_ON_OFF, ?CURSOR_OFF].

fg_idx(IndexedColor) when is_list(IndexedColor) ->
	% "0" to "255"
    [?ESC, ?CTRL_SEQ_INTRO, ?INDEXED_FG_COLOR, IndexedColor, ?CHAR_ATTRIBUTE].

bg_idx(IndexedColor) when is_list(IndexedColor) ->
    % "0" to "255"
    [?ESC, ?CTRL_SEQ_INTRO, ?INDEXED_BG_COLOR, IndexedColor, ?CHAR_ATTRIBUTE].

fg_rbg(R, G, B) ->
    % "0" to "255" or R, G, and B
    [?ESC, ?CTRL_SEQ_INTRO, ?FG_RGB, R, $:, G, $:, B, ?CHAR_ATTRIBUTE].

bg_rgb(R, G, B) ->
    % "0" to "255" or R, G, and B
    [?ESC, ?CTRL_SEQ_INTRO, ?BG_RGB, R, $:, G, $:, B, ?CHAR_ATTRIBUTE].

set_feature(Feature) ->
    #{Feature := Code} = ?FEATURES,
    [?ESC, ?CTRL_SEQ_INTRO, Code, ?CHAR_ATTRIBUTE].

format(Text, Features) ->
    Escapes = [set_feature(F) || F <- Features],
    Normal = set_feature(none),
    %Normal16 = escape(none_16),
    lists:flatten([Escapes, Text, Normal]).

cursor_type(Cursor) ->
    #{Cursor := Code} = ?CURSORS,
    [?ESC, ?CTRL_SEQ_INTRO, Code, ?SPACE, ?CURSOR].

cursor_pos(X0, Y0) ->
	X = integer_to_list(X0),
	Y = integer_to_list(Y0),
    [?ESC, ?CTRL_SEQ_INTRO, Y, $;, X, ?CURSOR_POS].

% doesn't seem to work
repeat_prev_char(Count) ->
    [?ESC, ?CTRL_SEQ_INTRO, Count, ?REPEAT].

repeat_char(Char, Count) ->
    [Char, ?ESC, ?CTRL_SEQ_INTRO, Count, ?REPEAT].

% CSI Ps n  Device Status Report (DSR).
%             Ps = 5  ⇒  Status Report.
%           Result ("OK") is CSI 0 n
%             Ps = 6  ⇒  Report Cursor Position (CPR) [row;column].
%           Result is CSI r ; c R
% 
%           Note: it is possible for this sequence to be sent by a
%           function key.  For example, with the default keyboard
%           configuration the shifted F3 key may send (with shift-,
%           control-, alt-modifiers)
% 
%             CSI 1 ; 2  R , or
%             CSI 1 ; 5  R , or
%             CSI 1 ; 6  R , etc.
% 
%           The second parameter encodes the modifiers; values range from
%           2 to 16.  See the section PC-Style Function Keys for the
%           codes.  The modifyFunctionKeys and modifyKeyboard resources
%           can change the form of the string sent from the modified F3
%           key.
device_status_report() ->
    [?ESC, ?CTRL_SEQ_INTRO, $5, $n].

get_cursor_position() ->
    [?ESC, ?CTRL_SEQ_INTRO, $6, $n].

% CSI > Ps q
%  Ps = 0  ⇒  Report xterm name and version (XTVERSION).

%  The response is a DSR sequence identifying the version: DCS > | text ST
xterm_name_version() ->
    [?ESC, ?CTRL_SEQ_INTRO, ">0q"].

get_screen_size() ->
	[?ESC, ?CTRL_SEQ_INTRO, "19t"].

get_textarea_size() ->
	[?ESC, ?CTRL_SEQ_INTRO, "18t"].

-define(A, A > 47, A < 58).
-define(B, B > 47, B < 58).
-define(C, C > 47, C < 58).
-define(D, D > 47, D < 58).
-define(E, 27,$[).
-define(F, F > 47, F < 58).
-define(G, G > 47, G < 58).

esc(X=[27]) -> X;
esc(X=[?E]) -> X;
esc(X=[?E,$8]) -> X;
esc(X=[?E,$8,$;]) -> X;
esc(X=[?E,$8,$;,A]) when ?A -> X;
esc(X=[?E,$8,$;,A,$;]) when ?A -> X;
esc(X=[?E,$8,$;,A,$;,B]) when ?A,?B -> X;
esc(X=[?E,$8,$;,A,$;,B,$t]) when ?A,?B -> esc(text_area, X);
esc(X=[?E,$8,$;,A,$;,B,C]) when ?A,?B,?C -> X;
esc(X=[?E,$8,$;,A,$;,B,C,$t]) when ?A,?B,?C -> esc(text_area, X);
esc(X=[?E,$8,$;,A,$;,B,C,D]) when ?A,?B,?C,?D -> X;
esc(X=[?E,$8,$;,A,$;,B,C,D,$t]) when ?A,?B,?C,?D -> esc(text_area, X);
esc(X=[?E,$8,$;,A,B]) when ?A,?B -> X;
esc(X=[?E,$8,$;,A,B,$;]) when ?A,?B -> X;
esc(X=[?E,$8,$;,A,B,$;,C]) when ?A,?B,?C -> X;
esc(X=[?E,$8,$;,A,B,$;,C,$t]) when ?A,?B,?C -> esc(text_area, X);
esc(X=[?E,$8,$;,A,B,$;,C,D]) when ?A,?B,?C,?D -> X;
esc(X=[?E,$8,$;,A,B,$;,C,D,$t]) when ?A,?B,?C,?D -> esc(text_area, X);
esc(X=[?E,$8,$;,A,B,$;,C,D,F]) when ?A,?B,?C,?D,?F -> X;
esc(X=[?E,$8,$;,A,B,$;,C,D,F,$t]) when ?A,?B,?C,?D,?F -> esc(text_area, X);
esc(X=[?E,$8,$;,A,B,C,$;]) when ?A,?B,?C -> X;
esc(X=[?E,$8,$;,A,B,C,$;,D]) when ?A,?B,?C,?D -> X;
esc(X=[?E,$8,$;,A,B,C,$;,D,$t]) when ?A,?B,?C,?D -> esc(text_area, X);
esc(X=[?E,$8,$;,A,B,C,$;,D,F]) when ?A,?B,?C,?D,?F -> X;
esc(X=[?E,$8,$;,A,B,C,$;,D,F,$t]) when ?A,?B,?C,?D,?F -> esc(text_area, X);
esc(X=[?E,$8,$;,A,B,C,$;,D,F,G]) when ?A,?B,?C,?D,?F,?G -> X;
esc(X=[?E,$8,$;,A,B,C,$;,D,F,G,$t]) when ?A,?B,?C,?D,?F,?G -> esc(text_area, X);
esc(X=[?E,$9]) -> X;
esc(X=[?E,$9, $;]) -> X;



parse_escape_code([?ESC, ?CTRL_SEQ_INTRO, $8, $; | Rest]) ->
    [H0, W0] = string:split(Rest, ";"),
	{H, _} = string:to_integer(H0),
	{W, _} = string:to_integer(W0),
	io:put_chars("."),
	{textarea_size, H, W};
parse_escape_code([?ESC, ?CTRL_SEQ_INTRO, $9, $; | Rest]) ->
    [H0, W0] = string:split(Rest, ";"),
	{H, _} = string:to_integer(H0),
	{W, _} = string:to_integer(W0),
	io:put_chars("."),
	{screen_size, H, W};
