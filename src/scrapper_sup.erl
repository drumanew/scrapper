-module (scrapper_sup).
-behaviour (supervisor).

-export ([start_link/0]).
-export ([start_worker/2]).

-export ([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link () ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker (Url, Proxy) ->
  supervisor:start_child(?MODULE, [Url, Proxy]).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

init ([]) ->
  {ok, {{simple_one_for_one, 1, 5}, [{scrapper_worker,
                                      {scrapper_worker, start_link, []},
                                      temporary,
                                      5000,
                                      worker,
                                      [scrapper_worker]}]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
