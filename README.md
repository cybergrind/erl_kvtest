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

