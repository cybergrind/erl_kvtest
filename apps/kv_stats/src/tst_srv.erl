-module(tst_srv).
-author('Kirill Pinchuk <k_pinchuk@wargaming.net>').

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).
-export([stop/0, terminate/2]).

-define(SERVER, ?MODULE).

start_link(Params) ->
  gen_server:start_link(?MODULE, Params, []).

stop() ->
  gen_server:cast(?SERVER, stop).

-record(st, {db, size, loop_time=100, operation}).
init([Params, Operation]) ->
  process_flag(priority, max),
  State = #st{db = maps:get(db, Params),
              size = maps:get(size, Params),
              loop_time = maps:get(interval, Params, 100),
              operation = Operation},
  self() ! loop,
  {ok, State}.


handle_call(Req, _From, State) ->
  lager:warning("Unhandled call ~p~n", [Req]),
  {reply, State}.


handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(Req, State) ->
  lager:warning("Unhandled cast: ~p~n", [Req]),
  {noreply, State}.
handle_info(loop, #st{db=Db, size=S, loop_time=L,
                      operation=Operation}=State) ->
  K = integer_to_binary(random:uniform(S)),
  Operation(Db, K, K),
  erlang:send_after(L, self(), loop),
  {noreply, State};
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
