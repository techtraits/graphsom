-module(graphsom_timer).
-behaviour(gen_server).

 -export([init/1, handle_call/3, handle_cast/2]). 
 -export([handle_info/2, terminate/2, code_change/3]).
 
 -export([stop/0]).

 -export([start_link/4]).
  
 -record(state, {report_interval, 	% ms interval between stat reporting
                 report_timer, 		% TRef of interval timer
                 graphite_host, 	% graphite server host
                 graphite_port,		% graphite server port
                 graphite_prefix,	%The prefix added to all  graphite keys
                 system_stats		% An array with list of system stats to report
               }).               
               
start_link(ReportIntervalMs, GraphiteHost, GraphitePort, SystemStats, Prefix) ->
    io:format("graphsom_timer start called ~n"),
	gen_server:start_link({local, ?MODULE}, ?MODULE,  
                              [ReportIntervalMs, GraphiteHost, GraphitePort, SystemStats ], []).

init([ReportIntervalMs, GraphiteHost, GraphitePort, SystemStats, Prefix]) ->
    io:format("graphsom timer started ....~n"),
    io:format("graphsom will report stats to ~p:~p every ~p ms ~n",
                          [ GraphiteHost, GraphitePort, ReportIntervalMs ]),
    {ok, Tref} = timer:apply_interval(ReportIntervalMs, gen_server, cast,  [?MODULE, {report}]),                          
    State = #state{ 
      report_interval = ReportIntervalMs,
      report_timer = Tref,
      graphite_host = GraphiteHost,
      graphite_port = GraphitePort,
      graphite_prefix = Prefix,
      system_stats = SystemStats
     },
    {ok, State}.

handle_cast({report}, State) -> 
    Metrics = folsom_metrics:get_metrics(),
    io:format("List of metrics: ~w", [Metrics]),
    MetricStr = graphsom:collect_metrics([], Metrics, State#state.system_stats ),
    io:format("Metric string: ~p", [MetricStr]),
    graphsom:send_to_graphite(lists:flatten(MetricStr),
                     State#state.graphite_host, State#state.graphite_port),
    {noreply, State};
	
	
handle_cast(stop, State) ->
   {stop, normal, State};
   
handle_cast(_, State) -> 
	{noreply, State}.

handle_call(_,_,State) ->
	{reply,ok,State}.

terminate(_Reason, _State) ->
	ok.
	
code_change(_OldVsn, State, _Extra) -> 
	{ok, State}. 
	
handle_info(_Info, State) ->
	{noreply,State}.
	
stop() ->
    gen_server:cast(?MODULE, stop).	

   
