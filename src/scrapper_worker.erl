-module (scrapper_worker).
-behaviour (gen_server).

%% API
-export ([start_link/2]).

-export ([get_state/1]).

%% gen_server callbacks
-export ([init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          terminate/2,
          code_change/3]).

-record(state, { url,
                 proxy,
                 ref,
                 data = [],
                 downloading = false,
                 done = false }).

-include ("scrapper_config.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link (string(), {string(), string()}) -> {ok, pid()}.
start_link (Url, Proxy) ->
  gen_server:start_link(?MODULE, [Url, Proxy], []).

get_state (Pid) ->
  gen_server:call(Pid, {get, state}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init ([Url, Proxy]) ->
  gen_server:cast(self(), start),
  {ok, #state{ url = Url, proxy = Proxy }}.

handle_call ({get, state}, _From, State = #state{ url = Url,
                                                  proxy = Proxy,
                                                  downloading = false }) ->
  {reply, {Url, Proxy, waiting}, State};
handle_call ({get, state}, _From, State = #state{ url = Url,
                                                  proxy = Proxy,
                                                  data = Data,
                                                  downloading = true }) ->
  {reply, {Url, Proxy,
           {downloading, lists:sum(lists:map(fun size/1, Data))}}, State};
handle_call (_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast (start, State = #state{ url = Url, proxy = Proxy }) ->
  case hackney:get(Url, [], <<>>, [async, {proxy, Proxy}]) of
    {ok, ClientRef} -> {noreply, State#state{ ref = ClientRef }};
    {error, Error}  -> {stop, Error, State}
  end;
handle_cast (_Msg, State) ->
  {noreply, State}.

handle_info ({hackney_response, Ref, Msg}, State = #state{ ref = Ref, data = Data }) ->
  case Msg of
    {status, 200, _} ->
      log("status: ok", State),
      {noreply, State#state{ downloading = true }};
    {status, Status, StatusData} ->
      log("status: ~p - ~p", [Status, StatusData], State),
      {stop, {badstatus, Status}, State};
    done ->
      log("status: done", State),
      {stop, normal, State#state{ done = true }};
    _ when is_binary(Msg) ->
      {noreply, State#state{ data = [Msg | Data] }};
    _ ->
      {noreply, State}
  end;
handle_info (_Info, State) ->
  {noreply, State}.

terminate (normal, State = #state{ url = Url, data = Data, done = true }) ->
  Output = get_output(Url),
  log("saving to: " ++ Output, State),
  file:write_file(Output, lists:reverse(Data)),
  scrapper_broker:report(self(), normal),
  ok;
terminate (_Reason, _State) ->
  scrapper_broker:report(self(), _Reason),
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

log (Format, Args, State) ->
  log(io_lib:format(Format, Args), State).

log (Msg, #state{ url = Url, proxy = Proxy }) ->
  error_logger:info_report(lists:flatten(io_lib:format("** ~s: ~s via ~s:~n** ~s",
                                                       [?MODULE, Url, Proxy, Msg]))).

get_output ("http://" ++ Url) ->
  get_output(Url);
get_output ("https://" ++ Url) ->
  get_output(Url);
get_output (Url) ->
  ?OUTPUT_DIR ++ "/" ++
  ?OUTPUT_PREFIX ++
  re:replace(Url, "/", "_", [global, {return, list}]) ++
  ?OUTPUT_SUFFIX.
