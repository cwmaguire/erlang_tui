-module(cs_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1,
        auto_shutdown => any_significant
    },
    ChildSpecs =
        [#{id => pg, start => {pg, start_link, []}},
         #{id => io_nif, start => {cs_io_nif, start_link, []}},
         #{id => io, start => {cs_io, start_link, []}},
         #{id => quit,
           start => {cs_quit, start_link, []},
           restart => transient,
           significant => true},
         #{id => map, start => {cs_map, start_link, []}},
         #{id => dispatch, start => {cs_dispatch, start_link, []}},
         #{id => command, start => {cs_command, start_link, []}},
         #{id => screen, start => {cs_screen, start_link, []}},
         #{id => window, start => {cs_window_sup, start_link, []}},
         #{id => doc, start => {cs_doc_sup, start_link, []}}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
