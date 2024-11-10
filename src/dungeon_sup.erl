-module(dungeon_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupervisorSpecification = #{
        strategy => one_for_one, % one_for_one | one_for_all | rest_for_one | simple_one_for_one
        intensity => 0,
        period => 1},

    ChildSpecifications = lists:map(fun ({Name, Port}) -> #{
            id => Name,
            start => {dungeon_server, start_link, [Name, Port]},
            restart => permanent, % permanent | transient | temporary
            shutdown => 2000, % use 'infinity' for supervisor child
            type => worker, % worker | supervisor
            modules => [dungeon_server]
        }
    end, [{dungeon_first, 7123},{dungeon_second, 7124}]),

    {ok, {SupervisorSpecification, ChildSpecifications}}.
