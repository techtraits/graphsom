-module(graphsom_timer).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]). 
-export([handle_info/2, terminate/2, code_change/3]).
 
-export([start_link/4, stop/0]).

-include("graphsom.hrl").
  
-record(state, {report_interval, % ms interval between stat reporting
                report_timer, 	 % TRef of interval timer
                graphite_host, 	 % graphite server host
                graphite_port,	 % graphite server port
                system_stats	 % list of system stats to report
               }).               

-type state() :: #state{}.

-spec start_link(pos_integer(), string(), pos_integer(), system_stats_type()) -> {ok, pid()} | {error, term()}.

start_link(ReportIntervalMs, GraphiteHost, GraphitePort, SystemStats) ->
    io:format("graphsom_timer start called ~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE,  
                          [ReportIntervalMs, GraphiteHost, GraphitePort, SystemStats ], []).

-spec init(list()) -> {ok, state()}.

init([ReportIntervalMs, GraphiteHost, GraphitePort, SystemStats]) ->
    io:format("graphsom timer started ....~n"),
    io:format("graphsom will report stats to ~p:~p every ~p ms ~n",
              [ GraphiteHost, GraphitePort, ReportIntervalMs ]),
    {ok, Tref} = timer:apply_interval(ReportIntervalMs, gen_server, cast,  [?MODULE, report]),                          
    State = #state{ 
      report_interval = ReportIntervalMs,
      report_timer = Tref,
      graphite_host = GraphiteHost,
      graphite_port = GraphitePort,
      system_stats = SystemStats
     },
    {ok, State}.

-type cast_msg_type() :: report | stop | term().

-spec handle_cast(cast_msg_type(), state()) -> {noreply, state()} | {stop, normal, state()}.

handle_cast(report, State) -> 
    Metrics = folsom_metrics:get_metrics(),
    io:format("List of metrics: ~w", [Metrics]),
    MetricStr = graphsom:collect_metrics([], Metrics, State#state.system_stats ),
    io:format("Metric string: ~p", [MetricStr]),
    graphsom:send_to_graphite(lists:flatten(MetricStr),
                              State#state.graphite_host, 
                              State#state.graphite_port),
    {noreply, State};
	
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

   
