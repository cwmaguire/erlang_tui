-module(window_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("cs.hrl").

-export([all/0]).

-export([test_random_name/1]).

all() ->
    [test_random_name].

% all() -> [test_split_window].

test_random_name(_Config) ->
    Filename = cs_window:random_file_name(),
    ?assertEqual(byte_size(Filename), 40),
    ok.
