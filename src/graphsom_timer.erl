-module(graphsom_timer).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]). 
-export([handle_info/2, terminate/2, code_change/3]).

-export([
         report_now/0,
         start_reporting/0, 
         stop_reporting/0,
         update_config/2
]).
 
-export([start_link/6, stop/0]).

-include("graphsom.hrl").

-record(state, {report_interval,              % ms interval between stat reporting
                report_timer, 	              % TRef of interval timer
                graphite_host,                % graphite server host
                graphite_port,	              % graphite server port
                graphite_prefix,              % prefix added to all graphite keys
                vm_metrics,	                  % list of vm metrics to report
                report_all_folsom_metrics,    % whether to report all folsom  metrics
                report = false :: boolean()   % whether to report metrics or not
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
      report = false
     },
    %% io:format("graphsom_timer Vm Metrics: ~w ~n", [VmMetrics]),
    {ok, State}.

-spec report_now() -> ok | {error, term()}.

report_now() ->
    gen_server:cast(?MODULE, report_now).

-spec start_reporting() -> ok | {error, term()}.

start_reporting() -> 
    gen_server:cast(?MODULE, start_reporting).

-spec stop_reporting() -> ok | {error, term()}.

stop_reporting() ->
    gen_server:cast(?MODULE, stop_reporting).

-spec update_config(atom(), term()) -> ok.

update_config(Key, Val) ->
    gen_server:cast(?MODULE, {update_config, {Key, Val}}),
    ok.

-type cast_msg_type() :: report | stop | start_reporting | stop_reporting | term().

-spec handle_cast(cast_msg_type(), state()) -> {noreply, state()} | {stop, normal, state()}.

%% don't do anything if report is set to false
handle_cast(report, State = #state{ report = false }) ->
    {noreply, State};

handle_cast(report, State = #state{ report = true }) ->
    handle_cast(report_now, State);

handle_cast(report_now, State = #state{ graphite_host = GHost, graphite_port = GPort, graphite_prefix = GPrefix}) ->
    MetricValues = graphsom_metrics:all(State#state.vm_metrics, State#state.report_all_folsom_metrics),
    _ = report_metrics(MetricValues, GHost, GPort, GPrefix),
    {noreply, State};

handle_cast({update_config, {Key, Val}}, State) ->
    {noreply, update_state(Key, Val, State)};    

handle_cast(start_reporting, State = #state{}) ->
    {noreply, update_state(report, true, State)};

handle_cast(stop_reporting, State = #state{}) ->
    {noreply, update_state(report, false, State)};

handle_cast(stop, State) ->
   {stop, normal, State};
   
handle_cast(_, State) -> 
	{noreply, State}.

-spec handle_call(term(), term(), state()) -> {reply, ok, state()}.

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

-spec report_metrics(list(), string(), pos_integer(), string()) -> ok | {error | term()}.

report_metrics([], _GHost, _GPort, _Gprefix) ->
    ok;

report_metrics(MetricValues, GHost, GPort, GPrefix) ->
    CurTime = graphsom_util:current_time(),
    MetricStr = stringify_metrics(MetricValues, GPrefix, CurTime),
    graphsom_graphite:report(MetricStr, GHost, GPort).

-spec stringify_metrics(list(),  string(), pos_integer()) -> string().

stringify_metrics(MetricValues, GPrefix, CurTime) ->
    lists:flatten([graphsom_graphite:stringify_proplist_metric(Name, Val, GPrefix, CurTime, "")|| {Name, Val} <- MetricValues]).

-spec update_state(atom(), term(), state()) -> ok.

update_state(report_interval, Val, State) when is_number(Val) -> State#state{ report_interval = Val};
update_state(graphite_host, Val, State) when is_list(Val) -> State#state{ graphite_host = Val};
update_state(graphite_port, Val, State) when is_integer(Val) -> State#state{ graphite_port = Val};
update_state(graphite_prefix, Val, State) when is_list(Val) -> State#state{ graphite_prefix = Val};
update_state(graphite_host, Val, State) when is_list(Val) -> State#state{ graphite_host = Val};
update_state(vm_metrics, Val, State) when is_list(Val) -> State#state{ vm_metrics = Val};
update_state(report_all_folsom_metrics, Val, State) when is_boolean(Val) -> State#state{ report_all_folsom_metrics = Val};
update_state(report, Val, State) when is_boolean(Val) -> State#state{ report = Val};
update_state(_, _, State) -> State.
