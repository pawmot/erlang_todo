-module(evserv).
-author("pawmot").

%% API
-export([]).

loop(State) ->
  receive
    {Pid, MsgRef, {subscribe, Client}} ->
      ok;
    {Pid, MsgRef, {add, Name, Description, TimeOut}} ->
      ok;
    {Pid, MsgRef, {cancel, Name}} ->
      ok;
    {done, Name} ->
      ok;
    shutdown ->
      ok;
    {'DOWN', Ref, process, _Pid, _Reason} ->
      ok;
    code_change ->
      ok;
    Unknown ->
      io:format("Unknown message: ~p~n", [Unknown]),
      loop(State)
  end.