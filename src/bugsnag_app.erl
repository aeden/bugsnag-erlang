-module(bugsnag_app).
-behavior(application).

% Application hooks
-export([start/2, stop/1]).

start(_Type, _Args) ->
  lager:info("Starting bugsnag notifier"),
  bugsnag_sup:start_link().

stop(_State) ->
  lager:info("Stopping bugsnag notifier"),
  ok.
