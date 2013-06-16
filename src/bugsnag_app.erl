-module(bugsnag_app).
-behavior(application).

% Application hooks
-export([start/2, stop/1]).

start(_Type, _Args) ->
  lager:info("Starting bugsnag notifier"),

  case application:get_env(bugsnag, api_key) of
    {ok, "ENTER_API_KEY"} -> {error, no_api_key};
    {ok, ApiKey} ->
      case application:get_env(bugsnag, error_logger) of
        {ok, true} ->
          error_logger:add_report_handler(bugsnag_error_logger);
        _ -> ok
      end,
      bugsnag_sup:start_link(ApiKey);
    undefined -> {error, no_api_key}
  end.

stop(_State) ->
  lager:info("Stopping bugsnag notifier"),
  ok.
