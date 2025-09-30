-module(cs_window).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1]).
-export([terminate/2]).

-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-record(state, {translate_fun,
                h = 0,
                w = 0,
                cursor_pos}).

start_link(TranslateFun, {H, W}) ->
    gen_server:start_link(?MODULE,
                          [TranslateFun, {H, W}],
                          _Opts = []).

init([TranslateFun, {H, W}]) ->
    % gen_server:cast(self, draw),
    {ok, #state{translate_fun = TranslateFun,
                cursor_pos = {0, 0},
                h = H,
                w = W}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

%% cs_screen should send 'draw' once windows are laid out.
%% Will need translate function.
handle_cast(draw, State = #state{translate_fun = TFun,
                                 w = W,
                                 h = H}) ->
    draw(TFun, H, W),
    {noreply, State};
handle_cast({translate_fun, F}, State = #state{}) ->
    {noreply, State#state{translate_fun = F}};
handle_cast({update, Fun, W, H}, State) ->
    {noreply, State#state{translate_fun = Fun, w = W, h = H}};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

draw(TFun, H, W) ->
    {X, Y} = TFun(5, 5),
    cs_io:do_atomic_ops([{cursor_pos, X, Y}, {write, "Hi!"}]),
    [draw_left_border(TFun(X_, H)) || X_ <- lists:seq(0, W)].

draw_left_border({X, Y}) ->
    %% █ is 9608 (U+2588 where 2588 is hex)
    Ops = [{cursor_pos, X, Y}, {write, "█"}],
    cs_io:do_atomic_ops(Ops).
