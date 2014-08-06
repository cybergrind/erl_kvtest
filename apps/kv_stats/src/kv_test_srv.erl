-module(kv_test_srv).
-author('cybergrind <cybergrind@gmail.com>').

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).
-export([stop/0, terminate/2]).

-define(SERVER, ?MODULE).

start_link(Params) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Params, []).

-spec stop() -> ok.
stop() ->
  gen_server:cast(?SERVER, stop).

-record(st, {history=[], params}).


init(Params) ->
  process_flag(priority, max),
  %lager:info("Test with params: ~p", [Params]),
  StopTimeout = maps:get(time, Params, 10000),
  erlang:send_after(StopTimeout, self(), stop),
  State = #st{params=Params},
  gen_server:cast(self(), start_test),
  {ok, State}.

handle_call(Req, _From, State) ->
  lager:warning("Unhandled call ~p~n", [Req]),
  {reply, State}.

handle_cast(start_test, #st{params=P}=State) ->
  kv_stats:metrics_init(),
  #{write_func := WF, read_func := RF,
    nreaders := NumReaders, nwriters := NumWriters,
    init_db := InitDb} = P,
  Db = InitDb(P),
  NewP = P#{db => Db},
  lager:info("Writers: ~p Readers: ~p", [NumWriters, NumReaders]),
  spawn(fun () -> [supervisor:start_child(tst_sup, [[NewP, RF]]) || _ <- lists:seq(1, NumReaders)] end),
  spawn(fun () ->[supervisor:start_child(tst_sup, [[NewP, WF]]) || _ <- lists:seq(1, NumWriters)] end),
  timer:sleep(2000),
  lager:info("Start looping"),
  erlang:send_after(1000, self(), loop),
  {noreply, State};
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(Req, State) ->
  lager:warning("Unhandled cast: ~p~n", [Req]),
  {noreply, State}.
handle_info(loop, #st{history=H}=State) ->
  CurrM = kv_stats:metrics_get(),
  lager:info("Curr: ~p", [CurrM]),
  NewState = State#st{history=[CurrM | H]},
  erlang:send_after(1000, self(), loop),
  {noreply, NewState};
handle_info(stop, State) ->
  lager:info("Stop test"),
  %lager:info("State: ~p", [State]),
  TstPids = [Pid || {_, Pid, _, _} <- supervisor:which_children(tst_sup)],
  [supervisor:terminate_child(tst_sup, Pid) || Pid <- TstPids],
  timer:sleep(1000),
  {stop, normal, State};
handle_info(Info, State) ->
  lager:warning("Unhandled info: ~p~n", [Info]),
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
