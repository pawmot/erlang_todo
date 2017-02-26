-module(event).
-author("pawmot").

%% API
-export([start/2, start_link/2, cancel/1]).

-record(state, {server,
  name = "",
  to_go = []}).

start(EventName, Delay) ->
  Server = self(),
  spawn(fun() -> init(Server, EventName, Delay) end).

start_link(EventName, Delay) ->
  Server = self(),
  spawn_link(fun() -> init(Server, EventName, Delay) end).

cancel(Pid) ->
  %% Monitor in case the process is already dead/
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, cancel},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, Pid, _Reason} ->
      ok
  end.

%%% event's innards
init(Server, EventName, DateTime) ->
  loop(#state{
    server = Server,
    name = EventName,
    to_go = time_to_go(DateTime)}).

loop(S = #state{server = Server, to_go = [T | Next]}) ->
  receive
    {Server, Ref, cancel} ->
      Server ! {Ref, ok}
  after T * 1000 ->
    if Next =:= [] ->
      Server ! {done, S#state.name};
      Next =/= [] ->
        loop(S#state{to_go = Next})
    end
  end.

%% Because Erlang is limited to about 49 days (49*24*60*60*1000) in
%% milliseconds, the following function is used.
normalize(N) ->
  Limit = 49 * 24 * 60 * 60,
  [N rem Limit | lists:duplicate(N div Limit, Limit)].

time_to_go(TimeOut = {{_, _, _}, {_, _, _}}) ->
  Now = calendar:local_time(),
  ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) - calendar:datetime_to_gregorian_seconds(Now),
  Secs = if ToGo > 0 -> ToGo;
           true -> 0
         end,
  normalize(Secs);
time_to_go(TimeOut) ->
  normalize(TimeOut).