erl_kvtest
==========

Erlang key-value storages test

1. Before usage do `rebar get-deps`

Basic usage:

```
make console

>> ets_test:do_simple_test(30000).
```

For raw hacking see/edit `apps/kv_stats/src/ets_test.erl`

h3. Some results

**NOTE: now it possibly not correct at all**

Initial: 500k+ keys, 80 writers, 800 readers


1. [{write_ops,107830},{read_ops,1089635}]

```erlang
   [set, public,
    {write_concurrency,true},
    {read_concurrency,true}]
```

2. [{write_ops,82652},{read_ops,837003}]

```erlang
   [set, public,
    {write_concurrency,false},
    {read_concurrency,true}]
```

3. [{write_ops,140910},{read_ops,1054215}]

```erlang
   [set, public,
    {write_concurrency,true},
    {read_concurrency,false}]
```

4. [{write_ops,106911},{read_ops,841153}]

```erlang
   [set, public,
    {write_concurrency,false},
    {read_concurrency,false}]
```