-module(cm_shell_sup).

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
    ChildSpecs = [#{id => pg,
					start => {pg, start_link, []},
				    restart => permanent},
				  #{id => io_nif,
					start => {cs_io_nif, start_link, []},
				    restart => permanent},
				  #{id => io,
					start => {cs_io, start_link, []},
				    restart => transient,
				    significant => true},
				  #{id => screen,
					start => {cs_screen, start_link, []},
				    restart => transient,
				    significant => true},
                  #{id => window,
					start => {cs_window_sup, start_link, []},
				    restart => permanent}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
