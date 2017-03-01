-module (scrapper_broker).
-behaviour (gen_server).

%% API
-export ([start_link/0]).

-export ([report/2]).

-export ([get_state/0]).
-export ([active_jobs/0]).

%% gen_server callbacks
-export ([init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          terminate/2,
          code_change/3]).

-include ("scrapper_config.hrl").

-record(state, { urls, proxy, workers = gb_sets:new(), free = ?MAX_WORKERS }).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link () -> {ok, pid()}.
start_link () ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

report (Pid, Reason) ->
  gen_server:cast(?MODULE, {report, Pid, Reason}).

get_state () ->
  gen_server:call(?MODULE, {get, state}).

active_jobs () ->
  gen_server:call(?MODULE, {get, workers}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init ([]) ->
  try
    {ok, shedule(#state{ urls  = get_urls(), proxy = get_proxy() })}
  catch
    _Class:_Error -> {stop, _Error}
  end.

handle_call ({get, state}, _From, State) ->
  {reply, State, State};
handle_call ({get, workers}, _From, State) ->
  {reply, gb_sets:to_list(State#state.workers), State};
handle_call (_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast ({report, Pid, _Reason}, State = #state{ workers = Workers,
                                                     free    = Free }) ->
  NewState =
    case gb_sets:is_member(Pid, Workers) of
      true ->
        shedule(State#state{ workers = gb_sets:delete(Pid, Workers),
                             free = Free + 1 });
      _ ->
        shedule(State)
    end,
  {noreply, NewState};
handle_cast (_Msg, State) ->
  {noreply, State}.

handle_info (_Info, State) ->
  {noreply, State}.

terminate (_Reason, _State) ->
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_urls () ->
  file_read_lines(?URLS_FILE).

get_proxy () ->
  file_read_lines(?PROXY_FILE).

file_read_lines (FileName) ->
  case file:read_file(FileName) of
    {ok, Binary} -> string:tokens(binary_to_list(Binary), "\n\r");
    {error, Err} -> throw(Err)
  end.

shedule (State = #state{ urls = [] }) -> State;
shedule (State = #state{ proxy = [] }) -> State;
shedule (State = #state{ free = 0 }) -> State;
shedule (State = #state{ urls    = [Url | RestUrls],
                         proxy   = [Proxy | RestProxy],
                         workers = Workers,
                         free    = Free }) when Free /= 0 ->
  {ok, Pid} = scrapper_sup:start_worker(Url, Proxy),
  NextState =
    State#state{ urls    = RestUrls,
                 proxy   = RestProxy ++ [Proxy],
                 workers = gb_sets:balance(gb_sets:add(Pid, Workers)),
                 free    = Free - 1 },
  shedule(NextState).

