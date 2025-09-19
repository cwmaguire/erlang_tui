-module(cm_shell).

% Run with:
% erl -pa _build/default/lib/erlang_shell_play/ebin -S cm_shell start_shell
% -S implies noshell
% When you exit from noshell you're dumped back into erlang but without
% a shell, so we call halt() when we're done.

-export([start_shell/0]).
-export([start_shell/1]).

start_shell([]) ->
    start_shell().

start_shell() ->
	io:put_chars("Hello world\n"),
    ok = shell:start_interactive({noshell, raw}),

    io:put_chars("\e[?1049h"), %% Enable alternate screen buffer
    io:put_chars("\e[?25l"), %% Hide the cursor
	tui_fun:xo_table(10,5),
	loop(),
    io:put_chars("\e[?25h"), %% Show the cursor
    io:put_chars("\e[?1049l"), %% Disable alternate screen buffer
    halt().

loop() ->
    Chars = io:get_chars("", 1024),

    case handle_input(Chars, 1) of
        stop ->
            stop;
		undefined ->
			loop();
        [27] ->   % handle the escape from the response
            loop();
        Other ->
            io:put_chars(Other),
            loop()
    end.

handle_input("q", _) ->
    stop;
handle_input("c", _) ->
    io:put_chars("\033[6n"), % get cursor pos?
    undefined;
handle_input("x", _) ->
    % io:put_chars("\033[5n"), % get something
    io:put_chars("\033[>0q"),
    undefined;
handle_input("s", _) ->
    %io:format("Sending query for xterm screen pos"),
    % io:put_chars("\033[5n"),
    io:put_chars("\033[13t"),
    %io:format("Waiting for get_line"),
    undefined;
handle_input([27 | Other], _) ->
    Other;
handle_input(Other, _) ->
    Other.
