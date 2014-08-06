-module(kv_stats_app).
-behaviour(application).
-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
  kv_stats_sup:start_link().

stop(_State) ->
  ok.
      
