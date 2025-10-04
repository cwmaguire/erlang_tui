-module(cs_io_nif).
-behaviour(gen_server).

-nifs([set_raw_mode/0,
       restore_term/0,
       read_key/0]).
-on_load(on_module_load/0).

-export([start_link/0]).
-export([init/1]).
-export([terminate/2]).

-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-record(state, {}).

on_module_load() ->
    PrivDir = code:priv_dir(cm_shell),
    NifPath = filename:join(PrivDir, "erlang_tui"),
    io:format("PrivDir: ~p~n", [PrivDir]),
    io:format("NifPath: ~p~n", [NifPath]),
    ok = erlang:load_nif(NifPath, 0).

set_raw_mode() ->
    erlang:nif_error(nif_library_not_loaded).

restore_term() ->
    erlang:nif_error(nif_library_not_loaded).

read_key() ->
    erlang:nif_error(nif_library_not_loaded).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    set_raw_mode(),
	ok = gen_server:cast(self(), start_input_loop),
	{ok, #state{}}.

handle_call(_Req, _From, State) ->
	{reply, ok, State}.

handle_cast(start_input_loop, State) ->
	start_input_loop(),
	{noreply, State};
handle_cast(restart, State) ->
	start_input_loop(),
	{noreply, State};
handle_cast(_Req, State) ->
	{noreply, State}.

handle_info({'DOWN', _Ref, process, Pid2, Reason}, State) ->
	io:format("~p died because ~p", [Pid2, Reason]),
    gen_server:cast(self(), restart),
	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_, _) ->
    io:format("restore term 1"),
    restore_term(),
    io:format("restore term 2~n"),
	ok.

start_input_loop() ->
    spawn_monitor(fun input_loop/0).

input_loop() ->
    input_loop(1).

input_loop(Column1) ->
    Column2 = 
        case read_key() of
            no_data ->
                case Column1 of
                    1 ->
                        ok;
                    _ ->
                        % Debug = lists:flatten(io_lib:format("Column1: ~p", [Column1])),
                        % cs_io:debug(Debug, Column1, 20)
                         cs_io:debug("                                                                   ",
                                     Column1, 3)
                end,
                timer:sleep(10),
                1;
            Char ->
                case Column1 of
                    1 ->
                        cs_io:debug("PTY:", 1, 3);
                    _ ->
                        ok
                end,
                cs_io:debug([Char], Column1 + 5, 3),
                cs_io:input(Char),
                Column1 + 6
        end,
    input_loop(Column2).
