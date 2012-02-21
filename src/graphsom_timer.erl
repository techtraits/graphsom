-module(graphsom_timer).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]). 
-export([handle_info/2, terminate/2, code_change/3]).
 
-export([start_link/6, stop/0]).

-include("graphsom.hrl").

-record(state, {report_interval,              % ms interval between stat reporting
                report_timer, 	              % TRef of interval timer
                graphite_host,                % graphite server host
                graphite_port,	              % graphite server port
                graphite_prefix,              % prefix added to all graphite keys
                vm_metrics,	              % list of vm metrics to report
                report_all_folsom_metrics,    % whether to report all folsom  metrics
                folsom_metrics                % list of registered folsom metrics
               }).               
               
-type state() :: #state{}.

-spec start_link(pos_integer(), string(), pos_integer(), string(), list(), boolean()) -> {ok, pid()} | {error, term()}.

start_link(ReportIntervalMs, GraphiteHost, GraphitePort, Prefix, VmMetrics, AllFolsomMetrics) ->
    %% io:format("graphsom_timer started ~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE,  
                          [ReportIntervalMs, GraphiteHost, GraphitePort, Prefix, VmMetrics, AllFolsomMetrics], []).

-spec init(list()) -> {ok, state()}.

init([ReportIntervalMs, GraphiteHost, GraphitePort, Prefix, VmMetrics, AllFolsomMetrics]) ->
    %% io:format("graphsom will report stats to ~p:~p every ~p ms ~n",
    %%          [ GraphiteHost, GraphitePort, ReportIntervalMs ]),
    {ok, Tref} = timer:apply_interval(ReportIntervalMs, gen_server, cast,  [?MODULE, report]),                     
    State = #state{ 
      report_interval = ReportIntervalMs,
      report_timer = Tref,
      graphite_host = GraphiteHost,
      graphite_port = GraphitePort,
      graphite_prefix = Prefix,
      vm_metrics = VmMetrics,
      report_all_folsom_metrics = AllFolsomMetrics,
      folsom_metrics = []
     },
    %% io:format("graphsom_timer Vm Metrics: ~w ~n", [VmMetrics]),
    {ok, State}.

-type cast_msg_type() :: report | stop | term().

-spec handle_cast(cast_msg_type(), state()) -> {noreply, state()} | {stop, normal, state()}.

handle_cast(report, State = #state{ graphite_host = GHost, graphite_port = GPort, graphite_prefix = GPrefix}) ->
    VmMetrics = State#state.vm_metrics,
    Metrics = case State#state.report_all_folsom_metrics of
                  true ->
                      folsom_metrics:get_metrics();
                  _ ->
                      State#state.folsom_metrics
              end,
    _ = report_metrics(Metrics, VmMetrics, GHost, GPort, GPrefix),
    {noreply, State};

handle_cast({register, FolsomMetric}, State) ->
    RegMetrics = State#state.folsom_metrics ++ [FolsomMetric],
    {noreply, State#state{ folsom_metrics = RegMetrics }};
	
handle_cast(stop, State) ->
   {stop, normal, State};
   
handle_cast(_, State) -> 
	{noreply, State}.

-spec handle_call(term(), term(), state()) -> {reply, ok, state()}.

handle_call(registered_metrics, _From, State) ->
    {reply, State#state.folsom_metrics, State};

handle_call(_,_,State) ->
	{reply,ok,State}.

-spec terminate(term(), state()) -> ok.

terminate(_Reason, _State) ->
	ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
	
code_change(_OldVsn, State, _Extra) -> 
	{ok, State}. 

-spec handle_info(term(), state()) -> {noreply, state()}.
	
handle_info(_Info, State) ->
	{noreply,State}.

-spec stop() -> ok.
	
stop() ->
    gen_server:cast(?MODULE, stop).	

%% Internal API

-spec report_metrics(list(), list(), string(), pos_integer(), string()) -> ok | {error | term()}.

report_metrics(Metrics, VmMetrics, GHost, GPort, GPrefix) ->
    CurTime = graphsom_util:current_time(),
    MetricStr = stringify_metrics(Metrics, VmMetrics, GPrefix, CurTime),
    io:format("Metric string: ~s ~n", [MetricStr]),
    graphsom_graphite:report(MetricStr, GHost, GPort).

-spec stringify_metrics(list(), list(),  string(), pos_integer()) -> string().

stringify_metrics(Metrics, VmMetrics, GPrefix, CurTime) ->
    MList = graphsom_folsom:get_metrics(Metrics, VmMetrics),
    lists:flatten([graphsom_graphite:stringify_proplist_metric(Name, Val, GPrefix, CurTime, "")|| {Name, Val} <- MList]).


