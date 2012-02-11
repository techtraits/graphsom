
-module(graphsom_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/4]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% Default values for report interval, host and port

-define(REPORT_INTERVAL_MS, 10000).
-define(GRAPHITE_HOST, "graphite-dev.electronicartspoker.com").
-define(GRAPHITE_PORT, 2003).
-define(GRAPHITE_PREFIX, "graphsom").
-define(SYSTEM_STATS, [memory, system_info, statistics, process_info, port_info]).



%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    start_link(?REPORT_INTERVAL_MS, ?GRAPHITE_HOST, ?GRAPHITE_PORT, ?SYSTEM_STATS, ?GRAPHITE_PREFIX).

start_link(ReportIntervalMs, GraphiteHost, GraphitePort, SystemStats, Prefix) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE,
                          [ReportIntervalMs, GraphiteHost, GraphitePort, SystemStats, Prefix]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Parms = [ReportIntervalMs, GraphiteHost, GraphitePort, SystemStats]) ->
    %% adding folsom_sup and graphsom to graphsom_sup's supervision tree
    Folsom = {folsom,
              {folsom_sup, start_link, []},
              permanent,
              5000,
              supervisor,
              [folsom_sup]
             },
    GraphsomTimer = {graphsom_timer,
                      {graphsom_timer, start_link, 
                       [ReportIntervalMs,GraphiteHost, GraphitePort, SystemStats]},
                      permanent, 
                      5000, 
                      worker, 
                      [graphsom_timer]},
    {ok, { {one_for_one, 100, 10}, [Folsom, GraphsomTimer]} }.
