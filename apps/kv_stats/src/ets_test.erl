-module(ets_test).
-export([do_simple_test/1,
         init_db/1,
         write/3,
         read/3]).
-include("common.hrl").

%% params:
%% time - test time
%% size - num k/v pairs
%% interval - worker loop interval
%% init_db/1, write_func/3, read_func/3
%% nreaders, nwriters

% ets_test:do_simple_test(30000).
do_simple_test(Time) ->
  Params = #{time => Time,
             size => 500000,
             interval => 600,
             init_db => fun init_db/1,
             nreaders => 40000,
             nwriters => 4000,
             write_func => fun write/3,
             read_func => fun read/3},
  kv_stats:do_test(Params).


init_db(Opts) ->
  Num = maps:get(size, Opts),
  E = ets:new(a, [set, public,
                  {write_concurrency,true},
                  {read_concurrency,true}]),
  Data = [{integer_to_binary(K), integer_to_binary(K)} || K <- lists:seq(0, Num)],
  ets:insert(E, Data),
  E.

write(Db, K, V) ->
  ets:insert(Db, {K, V}),
  kv_stats:cwrite(1),
  ok.

read(Db, K, _) ->
  ets:lookup(Db, K),
  kv_stats:cread(1),
  ok.
