-module(bugsnag).
-behavior(gen_server).

-export([start/0, start_link/1]).

% Gen server hooks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
  ]).

-record(state, {api_key}).

% Public API
start() ->
  inets:start(),
  crypto:start(),
  ssl:start(),
  lager:start(),
  application:start(bugsnag).

start_link(ApiKey) ->
  %lager:info("Starting bugsnag gen server with API key ~p", [ApiKey]),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [ApiKey], []).

% Gen server hooks
init([ApiKey]) ->
  {ok, #state{api_key = ApiKey}}.

handle_call(_, _, State) ->
  {reply, ok, State}.

handle_cast({exception, Data = [{type, _Type}, {reason, _Reason}, {message, _Message}, {module, _Module}, {line, _Line}, {trace, _Trace}]}, State) ->
  send_exception(Data),
  {noreply, State}.

handle_info(_Message, State) ->
  {reply, ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% Internal API
send_exception(Data) ->
  %lager:info("Sending exception: ~p", [Data]),
  ok.
