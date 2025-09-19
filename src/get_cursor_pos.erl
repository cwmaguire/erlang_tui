
-module(get_cursor_pos).

-export([get_cursor_pos/0]).
-export([get_cursor_pos_2/0]).
-export([get_cursor_pos_3/0]).
-export([get_cursor_pos_4/0]).
-export([test/0]).

get_cursor_pos() ->

    io:format("Sending query"),
    io:format("\033[5n"),
    io:format("Waiting for get_line"),
    Response = io:get_line(""),
    io:format("Turning on echo"),
    os:cmd("stty echo"),

    % disable_terminal_auto_echo(),
    % query_terminal_pos(),
    % Response = read_terminal_pos(),
    % enable_terminal_auto_echo(),
    io:format(user, "Response = ~p~n", [Response]),

    % Process the response
    case Response of
        eof ->
            {error, no_response};
        [_, _ | Rest] ->
            %Trimmed = string:trim(Response, both, "\n"),
            Trimmed = string:trim(Rest, both, "\n"),
            %WithoutEsc = string:slice(Trimmed, 1, string:length(Trimmed) - 2),
            %[RowStr, ColStr] = string:split(WithoutEsc, ";"),
            [RowStr, ColStr] = string:split(Trimmed, ";"),
            {Row, _} = string:to_integer(RowStr),
            {Col, _} = string:to_integer(ColStr),
            {ok, Row, Col};
        Other ->
            Other
    end.

%disable_terminal_auto_echo() ->
%    os:cmd("stty -echo").
%
%enable_terminal_auto_echo() ->
%    os:cmd("stty echo").
%
%query_terminal_pos() ->
%    io:format("\033[6n").
%
%read_terminal_pos() ->
%    io:get_line("").

get_cursor_pos_2() ->
    % Save current terminal settings
    _ = init:get_plain_arguments(), % Not ideal, but we need stty
    os:cmd("stty -echo"), % Disable echo via shell command

    % Send the cursor position query
    io:format("\033[6n", []),

    % Read the response
    Response = io:get_line(""),

    % Restore terminal settings
    os:cmd("stty echo"), % Restore echo

    % Process the response
    case Response of
        eof -> {error, no_response};
        _ ->
            Trimmed = string:trim(Response, both, "\n"),
            WithoutEsc = string:slice(Trimmed, 1, string:length(Trimmed) - 2),
            [RowStr, ColStr] = string:split(WithoutEsc, ";"),
            {Row, _} = string:to_integer(RowStr),
            {Col, _} = string:to_integer(ColStr),
            {ok, Row, Col}
    end.

get_cursor_pos_3() ->
    % Open the tty device explicitly
    {ok, TTY} = file:open("/dev/tty", [read, raw]),

    % Send the query
    io:format("\033[6n", []),

    % Read the response with a timeout
    Response = case file:read_line(TTY) of
        {ok, Data} -> Data;
        _ -> timeout
    end,

    % Close the tty
    file:close(TTY),

    % Parse the response
    case Response of
        timeout -> {error, no_response};
        eof -> {error, no_response};
        Data2 ->
            Trimmed = string:trim(Data2, both, "\n"),
            WithoutEsc = string:slice(Trimmed, 1, string:length(Trimmed) - 2),
            [RowStr, ColStr] = string:split(WithoutEsc, ";"),
            {Row, _} = string:to_integer(RowStr),
            {Col, _} = string:to_integer(ColStr),
            {ok, Row, Col}
    end.

test() ->
    io:format("Sending query: ~p~n", ["\033[6n"]),
    io:format("\033[6n", []),
    timer:sleep(100), % Give the terminal 100ms to respond
    io:format("Attempting to read...\n", []),
    Response = io:get_chars("", 3), % Read up to 10 chars
    io:format("Got: ~p~n", [Response]).

get_cursor_pos_4() ->
    % Open a port to the shell with tty control
    Port = open_port({spawn, "sh"}, [binary, in, out, eof]),

    % Set tty to raw mode and send the query
    port_command(Port, "stty raw; echo -e '\033[6n' > /dev/tty"),

    % Read the response (up to 20 bytes, should be enough for [row;colR)
    Response = receive
        {Port, {data, Data}} -> Data;
        {Port, eof} -> <<>>;
        _ -> timeout
    after 500 -> timeout % 500ms timeout
    end,

    % Restore tty and close port
    port_command(Port, "stty cooked"),
    port_close(Port),

    % Parse the response (binary)
    case Response of
        timeout -> {error, timeout};
        <<>> -> {error, no_response};
        Data2 ->
            Str = binary_to_list(Data2),
            case string:split(Str, "R") of
                [Part, _] ->
                    WithoutEsc = string:slice(Part, 1),
                    [RowStr, ColStr] = string:split(WithoutEsc, ";"),
                    {Row, _} = string:to_integer(RowStr),
                    {Col, _} = string:to_integer(ColStr),
                    {ok, Row, Col};
                _ -> {error, invalid_response}
            end
    end.
