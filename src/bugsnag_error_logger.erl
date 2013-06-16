-module(bugsnag_error_logger).

-behaviour(gen_event).

-export([
    init/1,
    handle_call/2,
    handle_event/2,
    handle_info/2,
    code_change/3,
    terminate/2
  ]).

-record(state, {}).

% Callbacks

init(_InitArgs) ->
  {ok, #state{}}.

handle_event(Event, State) ->
  lager:info("handle_event(~p)", [Event]),
  {ok, State}.

handle_call(Request, State) ->
  lager:info("handle_call(~p)", [Request]),
  {ok, noreply, State}.

handle_info(Info, State) ->
  lager:info("handle_info(~p)", [Info]),
  {ok, State}.

terminate(Arg, _State) ->
  lager:info("terminate(~p)", [Arg]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
