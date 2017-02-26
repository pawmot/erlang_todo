-module(evserv).
-author("pawmot").

%% API
-export([init/0]).

-record(state, {events, clients}).
-record(event, {name = "", description = "", pid, timeout = {{1970, 1, 1}, {0, 0, 0}}}).

init() ->
  %% Loading events from a static file could be done here.
  %% You would need to pass an argument to init telling where the
  %% resource to find the events is. Then load it from here.
  %% Another option is to just pass the events straight to the server through this function.
  loop(#state{events = orddict:new(), clients = orddict:new()}).

loop(S = #state{}) ->
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
      loop(S)
  end.