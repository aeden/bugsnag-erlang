-module(bugsnag_app).
-behavior(application).

% Application hooks
-export([start/0, start/2, stop/1]).

start() ->
  application:start(bugsnag).

start(_Type, _Args) ->
  bugsnag_sup:start_link().

stop(_State) ->
  ok.
