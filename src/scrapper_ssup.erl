-module (scrapper_ssup).
-behaviour (supervisor).

-export ([start_link/0]).
-export ([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link () ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

init ([]) ->
  Procs =
    [{scrapper_sup,
      {scrapper_sup, start_link, []},
      permanent,
      5000,
      supervisor,
      [scrapper_sup]},
     {scrapper_broker,
      {scrapper_broker, start_link, []},
      permanent,
      5000,
      worker,
      [scrapper_broker]}],
  {ok, {{one_for_one, 1, 5}, Procs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

