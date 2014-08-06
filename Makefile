

compile:
	rebar compile

console: compile
	erl -pa apps/*/ebin -pa deps/*/ebin -config kv_test -s kv_stats
