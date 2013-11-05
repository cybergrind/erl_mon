-module(erl_mon).
-author('cybergrind <cybergrind@gmail.com>').

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).
-export([stop/0, terminate/2]).
-export([inc/1, inc/2, start/0]).


-define(SERVER, ?MODULE).
-define(ROTATE_LOOP, 60000).
-record(emon, {curr, prev, ref}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?SERVER, stop).

init([]) ->
  process_flag(trap_exit, true),
  {ok, Ref} = timer:send_after(?ROTATE_LOOP, self(), {'$gen_cast', rotate}),
  State = #emon{curr=dict:new(), ref=Ref},
  
  {ok, State}.

handle_call(Req, _From, State) ->
  lager:error("Unhandled call ~p~n", [Req]),
  {reply, State}.

handle_cast(rotate, #emon{curr=Curr, ref=OldRef}=State) ->
  timer:cancel(OldRef),
  {ok, Ref} = timer:send_after(?ROTATE_LOOP, self(), {'$gen_cast', rotate}),
  {noreply, State#emon{curr=dict:new(), prev=Curr, ref=Ref}};
handle_cast({inc, Req, ANum}, #emon{curr=Curr}=State) ->
  NewState = case Curr:find(Req) of
    {ok, Num} ->
      State#emon{curr=Curr:store(Req, Num + ANum)};
    error ->
      State#emon{curr=Curr:store(Req, 1)} end,
  {noreply, NewState};
handle_cast({req, <<"running">>, Sock, BackPid}, State) ->
  BackPid ! { resp, <<"1">>, Sock},
  {noreply, State};
handle_cast({req, Req, Sock, BackPid}, #emon{curr=Curr, prev=undefined}=State) ->
  case Curr:find(Req) of
    {ok, Resp} ->
      BackPid ! { resp, list_to_binary(integer_to_list(Resp)), Sock};
    error ->
      BackPid ! { resp, <<"0">>, Sock} end,
  {noreply, State};
handle_cast({req, Req, Sock, BackPid}, #emon{prev=Prev}=State) ->
  case Prev:find(Req) of
    {ok, Resp} ->
      BackPid ! { resp, list_to_binary(integer_to_list(Resp)), Sock};
    error ->
      BackPid ! { resp, <<"0">>, Sock} end,
  {noreply, State};
handle_cast({req, _AnyReq, Sock, BackPid}, State) ->
  lager:warning("Unhandled request: ~p", [_AnyReq]),
  BackPid ! { resp, <<"0">>, Sock},
  {noreply, State};
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(Req, State) ->
  lager:error("Unhandled cast: ~p~n", [Req]),
  {noreply, State}.

handle_info(Info, State) ->
  lager:error("Unhandled info: ~p~n", [Info]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, _State) ->
  ok;
terminate(shutdown, _State) ->
  ok;
terminate({shutdown, _Reason}, _State) ->
  ok;
terminate(_Reason, _State) ->
  ok.


inc(What) ->
  inc(What, 1).

inc(What, Num) ->
  gen_server:cast(erl_mon, {inc, What, Num}).

start() ->
  application:start(erl_mon).
