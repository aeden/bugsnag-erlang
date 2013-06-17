-module(bugsnag_sup).
-behavior(supervisor).

% API
-export([start_link/2]).

% Supervisor hooks
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% Public API
start_link(ApiKey, ReleaseState) ->
  %lager:debug("Starting ~p", [?MODULE]),
  supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, [ApiKey, ReleaseState]).

init([ApiKey, ReleaseState]) ->
  Procs = [
    ?CHILD(bugsnag, worker, [ApiKey, ReleaseState])
  ],

  {ok, {{one_for_one, 20, 10}, Procs}}.
