-module(dungeon_app).

-behaviour(application).

-export([
  start/2,
  start/0,
  stop/1
]).

start(_Type, _StartArgs) ->
  case dungeon_sup:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Other -> 
      {error, Other}
    end.

start() ->
  case dungeon_sup:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Other -> 
      {error, Other}
    end.

stop(_State) -> 
  ok.