-module(annex).

-export([start_link/1]).
-export([start_link/2]).
-export([monitor/1]).
-export([call/4]).
-export([call/5]).
-export([call/6]).
-export([await/2]).
-export([await/3]).
-export([cast/4]).
-export([stop/1]).

-compile({no_auto_import, [monitor/1]}).

-define(DEFAULT_TIMEOUT, 2000).

start_link(Interface) ->
  start_link(Interface, []).

monitor({_Interface, Pid}) when is_pid(Pid) ->
  erlang:monitor(process, Pid);
monitor({Interface, Pid}) ->
  Interface:monitor(Pid).

start_link(Interface, Options) ->
  case Interface:start_link(Options) of
    {ok, Pid} ->
      {ok, {Interface, Pid}};
    Error ->
      Error
  end.

call(Pid, Module, Function, Arguments) ->
  call(Pid, Module, Function, Arguments, ?DEFAULT_TIMEOUT).

call(Pid, Module, Function, Arguments, Timeout) ->
  Ref = monitor(Pid),
  Res = case call(Pid, Module, Function, Arguments, self(), Ref) of
    {Status, Value} ->
      {Status, Value};
    ok ->
      await(Pid, Ref, Timeout)
  end,
  demonitor(Ref),
  Res.

call({Interface, Pid}, Module, Function, Arguments, Sender, Ref) ->
  Interface:call(Pid, Module, Function, Arguments, Sender, Ref).

await(Pid, Ref) ->
  await(Pid, Ref, ?DEFAULT_TIMEOUT).

await({_Interface, Pid}, Ref, Timeout) ->
  %% TODO handle more messages like errors and close events
  receive
    {'DOWN', Ref, process, Pid, Reason} ->
      {error, Reason};
    {'EXIT', Pid, Reason} ->
      {error, Reason};
    {Ref, {Status, Value}} ->
      {Status, Value}
  after Timeout ->
    {error, timeout}
  end.

cast({Interface, Pid}, Module, Function, Arguments) ->
  Interface:cast(Pid, Module, Function, Arguments).

stop({Interface, Pid}) ->
  Interface:stop(Pid).
