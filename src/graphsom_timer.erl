-module(graphsom_timer).
-behaviour(gen_server).

 -export([init/1, handle_call/3, handle_cast/2]). 
 -export([handle_info/2, terminate/2, code_change/3]).
 
 -export([stop/0]).

 -export([start_link/3]).
  
 -record(state, {report_interval, 	% ms interval between stat reporting
                 report_timer, 		% TRef of interval timer
                 graphite_host, 	% graphite server host
                 graphite_port 		% graphite server port
               }).               
               
start_link(ReportIntervalMs, GraphiteHost, GraphitePort) ->
    io:format("graphsom_timer start called ~n"),
	gen_server:start_link({local, ?MODULE}, ?MODULE,  
                              [ReportIntervalMs, GraphiteHost, GraphitePort], []).

init([ReportIntervalMs, GraphiteHost, GraphitePort]) ->
    io:format("graphsom timer started ....~n"),
    io:format("graphsom will report stats to ~p:~p every ~p ms ~n",
                          [ GraphiteHost, GraphitePort, ReportIntervalMs ]),
    {ok, Tref} = timer:apply_interval(ReportIntervalMs, gen_server, cast,  [?MODULE, {report}]),                          
    State = #state{ 
      report_interval = ReportIntervalMs,
      report_timer = Tref,
      graphite_host = GraphiteHost,
      graphite_port = GraphitePort
     },
    {ok, State}.

handle_cast({report}, State) -> 
    Metrics = folsom_metrics:get_metrics(),
    io:format("List of metrics: ~w", [Metrics]),
    MetricStr = collect_metrics([], Metrics),
    io:format("Metric string: ~p", [MetricStr]),
    send_to_graphite(lists:flatten(MetricStr),
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

%% TODO collect non-gauge metrics
collect_metrics(MetricStr, [MetricName | T]) ->
    MetricStr2 = case catch folsom_metrics:get_metric_value(MetricName) of
                     {Error, Reason} ->
                         error_logger:info_msg("Error when getting metric from folsom_metrics: error: ~p, reason: ~p",[Error, Reason]),
                         MetricStr;
                     MetricValue ->
                         error_logger:info_msg("Got value ~p for metric ~p \n",[MetricValue, MetricName]),
                         CurrentTime = current_time(),
                         string:concat(MetricStr , io_lib:format("p ~p ~w ~n",
                                                                 [MetricName
                                                                  ,MetricValue, CurrentTime]))
                 end,
    collect_metrics(MetricStr2, T);

collect_metrics(MetricStr, []) ->	
	MetricStr.
   
send_to_graphite(MetricStr, GraphiteHost, GraphitePort) ->
    case gen_tcp:connect(GraphiteHost, GraphitePort, [list, {packet, 0}]) of
        {ok, Sock} ->
            gen_tcp:send(Sock, MetricStr),
            gen_tcp:close(Sock),
            ok;
        {error, Reason} ->
            error_logger:error_msg("Failed to connect to graphite: ~p", [Reason]),
            {error, Reason}
    end. 

current_time() ->    
    calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time( now()))-719528*24*3600.    
