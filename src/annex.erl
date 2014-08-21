-module(annex).

-export([start_link/2]).
-export([call/4]).
-export([call/5]).
-export([call/6]).
-export([stop/1]).

-define(DEFAULT_TIMEOUT, 2000).

start_link(Interface) ->
  start_link(Interface, []).

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
  Ref = erlang:make_ref(),
  ok = call(Pid, Module, Function, Arguments, self(), Ref),
  receive
    {Status, Value, Ref} ->
      {Status, Value}
  after Timeout ->
    {error, timeout}
  end.

call({Interface, Pid}, Module, Function, Arguments, Sender, Ref) ->
  Interface:call(Pid, Module, Function, Arguments, Sender, Ref).

stop({Interface, Pid}) ->
  Interface:stop(Pid).
