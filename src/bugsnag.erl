-module(bugsnag).
-behavior(gen_server).

-export([start/0, start_link/2, notify/5, notify/7, test_error/0]).

% Gen server hooks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
  ]).

-record(state, {api_key, release_stage}).

-define(NOTIFY_ENDPOINT, "https://notify.bugsnag.com").

-define(NOTIFIER_NAME, <<"Bugsnag Erlang">>).
-define(NOTIFIER_VERSION, <<"1.0.0">>).
-define(NOTIFIER_URL, <<"https://github.com/aeden/bugsnag-erlang">>).

% Public API
start() ->
  inets:start(),
  crypto:start(),
  ssl:start(),
  lager:start(),
  application:start(bugsnag).

start_link(ApiKey, ReleaseStage) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [ApiKey, ReleaseStage], []).

notify(Type, Reason, Message, Module, Line) ->
  notify(Type, Reason, Message, Module, Line, generate_trace(), undefined).
notify(Type, Reason, Message, Module, Line, Trace, Request) ->
  gen_server:cast(?MODULE, {exception, Type, Reason, Message, Module, Line, Trace, Request}).

test_error() ->
  gen_server:cast(?MODULE, {test_error}).

% Gen server hooks
init([ApiKey, ReleaseStage]) ->
  {ok, #state{api_key = ApiKey, release_stage = ReleaseStage}}.

handle_call(_, _, State) ->
  {reply, ok, State}.

handle_cast({exception, Type, Reason, Message, Module, Line, Trace, Request}, State) ->
  send_exception(Type, Reason, Message, Module, Line, Trace, Request, State),
  {noreply, State};

handle_cast({test_error}, State) ->
  erlang:error(test_error),
  {noreply, State}.

handle_info(_Message, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% Internal API
% See https://bugsnag.com/docs/notifier-api
send_exception(_Type, Reason, Message, _Module, _Line, Trace, _Request, State) ->
  {ok, Hostname} = inet:gethostname(),
  Payload = [
    {apiKey, to_bin(State#state.api_key)},
    {notifier, [
        {name, ?NOTIFIER_NAME},
        {version, ?NOTIFIER_VERSION},
        {url, ?NOTIFIER_URL}
      ]},
    {events, [
        [
          {payloadVersion, <<"2">>},
          {device, [
              {hostname, to_bin(Hostname)}
          ]},
          {app, [
              {releaseStage, to_bin(State#state.release_stage)}
          ]},
          {exceptions, [
              [
                {errorClass, to_bin(Reason)},
                {message, to_bin(Message)},
                {stacktrace, process_trace(Trace)}
              ]
            ]}
        ]
      ]}
  ],
  deliver_payload(jsx:encode(Payload)).

process_trace(Trace) ->
  lager:info("Processing trace ~p", [Trace]),
  process_trace(Trace, []).

process_trace([], ProcessedTrace) -> ProcessedTrace;
process_trace([Current|Rest], ProcessedTrace) ->
  StackTraceLine = case Current of
    {_, F, _, [{file, File}, {line, Line}]} ->
      [
        {file, to_bin(File)},
        {lineNumber, Line},
        {method, to_bin(F)}
      ];
    {_, F, _} ->
      [
        {method, to_bin(F)}
      ];
    _ ->
      lager:warning("Discarding stack trace line: ~p", [Current]),
      []
  end,
  process_trace(Rest, ProcessedTrace ++ [StackTraceLine]).

deliver_payload(Payload) ->
  lager:info("Sending exception: ~p", [Payload]),
  case httpc:request(post, {?NOTIFY_ENDPOINT, [], "application/json", Payload}, [{timeout, 5000}], []) of
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
      lager:info("Error sent. Response: ~p", [Body]);
    {_, {{_Version, Status, ReasonPhrase}, _Headers, _Body}} ->
      lager:warning("Failed to send error to bugsnag (~p : ~p)", [Status, ReasonPhrase])
  end,

  ok.

to_bin(Atom) when erlang:is_atom(Atom) ->
    erlang:atom_to_binary(Atom, utf8);
to_bin(Bin) when erlang:is_binary(Bin) ->
    Bin;
to_bin(Int) when erlang:is_integer(Int) ->
    erlang:integer_to_binary(Int);
to_bin(List) when erlang:is_list(List) ->
    erlang:iolist_to_binary(List).

generate_trace() ->
  lager:info("Generating trace"),
  try
    throw(bugsnag_gen_trace)
  catch bugsnag_gen_trace -> erlang:get_stacktrace()
  end.
