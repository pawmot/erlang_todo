%%%-------------------------------------------------------------------
%%% @author pawmot
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Feb 2017 14:45
%%%-------------------------------------------------------------------
-module(event).
-author("pawmot").

%% API
-export([loop/1]).

-record(state, {server,
  name = "",
  to_go = 0}).

loop(S = #state{server = Server}) ->
  receive
    {Server, Ref, cancel} ->
      Server ! {Ref, ok}
  after S#state.to_go * 1000 ->
    Server ! {done, S#state.name}
  end.
