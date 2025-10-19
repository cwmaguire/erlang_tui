-module(cs_map).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1]).
-export([terminate/2]).

-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-export([input/1]).

-record(state, {partial_map,
                mappings = [{"z", "zz"}] :: list()}).

input(Input) when is_list(Input) ->
    [gen_server:cast(?MODULE, {input, I}) || I <- Input];
input(Input) ->
    gen_server:cast(?MODULE, {input, Input}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], _Opts = []).

init(_Args) ->
    {ok, #state{partial_map = []}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({input, Input},
            State = #state{partial_map = PartialMap,
                           mappings = Mappings}) ->
    case PartialMap of
        [] ->
            % TODO only disply displayable characters
            % e.g. not <Esc>
            cs_screen:display(Input);
        _ ->
            ok
    end,
    MaybeMap = PartialMap ++ [Input],
    PartialMap2 =
        case check_mappings(MaybeMap, Mappings) of
            [{map, MapOutput} | _] ->
                %% do something
                [cs_dispatch:input(O) || O <- MapOutput],
                [];
            [partial | _] ->
                PartialMap;
            _ ->
                %% add all characters to buffer at cursor
                %% redraw portion of line
                %%   - includes deleting first character
                cs_io:clear(10, 16),
                cs_io:debug("no map match", 0, 11),
                [cs_dispatch:input(O) || O <- MaybeMap],
                []
        end,
    {noreply, State#state{partial_map = PartialMap2}};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

check_mappings(MaybeMap, Mappings) ->
    lists:filtermap(fun(Mapping) ->
                        is_mapping(MaybeMap, Mapping)
                    end,
                    Mappings).

is_mapping(To, {To, From}) ->
    {true, {map, From}};
is_mapping(PartialTo, {To, _From}) ->
    case string:prefix(To, PartialTo) of
        nomatch ->
            false;
        _ ->
            {true, partial}
    end.

