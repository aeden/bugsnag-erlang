-module(bugsnag_app).
-behavior(application).

% Application hooks
-export([start/2, stop/1]).

start(_Type, _Args) ->
  case application:get_env(bugsnag_erlang, enabled, true) of
    true ->
      start();
    false ->
      lager:info("Bugsnag notifier disabled"),
      %% we still need to start the sup to comply with the application
      %% behaviour
      bugsnag_sup:start_link(disabled)
  end.

start() ->
  lager:info("Starting bugsnag notifier"),
  ReleaseState = case application:get_env(bugsnag_erlang, release_state) of
    {ok, Value} -> Value;
    undefined -> undefined
  end,
  case application:get_env(bugsnag_erlang, api_key) of
    {ok, "ENTER_API_KEY"} -> {error, no_api_key};
    {ok, ApiKey} ->
      case application:get_env(bugsnag_erlang, error_logger) of
        {ok, true} ->
          error_logger:add_report_handler(bugsnag_error_logger);
        _ -> ok
      end,
      bugsnag_sup:start_link({ApiKey, ReleaseState});
    undefined -> {error, no_api_key}
  end.

stop(_State) ->
  lager:info("Stopping bugsnag notifier"),
  ok.
