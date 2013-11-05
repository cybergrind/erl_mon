-module(erl_mon_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).


-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  EMon = ?CHILD(erl_mon, worker),
  GUnix = ?CHILD(gen_unix, worker),
  {ok, { {one_for_one, 5, 10}, [EMon, GUnix]} }.

