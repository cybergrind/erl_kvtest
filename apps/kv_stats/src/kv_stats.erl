-module(kv_stats).
-export([start/0, metrics_init/0, metrics_get/0]).
-export([do_test/1, cwrite/1, cread/1]).
-include("common.hrl").


do_test(Params) ->
  supervisor:start_child(kv_test_sup, [Params]).

start() -> start(kv_stats).

cwrite(Num) ->
  folsom_metrics:notify({?WRITE, Num}).

cread(Num) ->
  folsom_metrics:notify({?READ, Num}).

metrics_init() ->
  MetricsList = [?WRITE, ?READ],
  [restart_metric(Name) || Name <- MetricsList].

metrics_get() ->
  MetricsList = [?WRITE, ?READ],
  [{Name, folsom_metrics:get_metric_value(Name)} || Name <- MetricsList].

restart_metric(Name) ->
  case folsom_metrics:new_meter(Name) of
    ok -> ok;
    {error, Name, metric_already_exists} ->
      folsom_metrics:delete_metric(Name),
      restart_metric(Name)
  end.


% copied from lager
start(App) ->
    start_ok(App, application:start(App, permanent)).

start_ok(_App, ok) -> ok;
start_ok(_App, {error, {already_started, _App}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) ->
    ok = start(Dep),
    start(App);
start_ok(App, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).
