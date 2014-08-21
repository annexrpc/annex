annex
=====

rpc interface for erlang

Usage
-----

`annex` acts as a common interface to different rpc [backends](#backends).

```erlang
{ok, Pid} = annex:start_link(my_rpc_interface),

{ok, 4} = annex:call(Pid, math, pow, [2, 2]),

ok = annex:stop(Pid).
```

Backends
--------

* [annex-port](https://github.com/annexrpc/annex-port)
