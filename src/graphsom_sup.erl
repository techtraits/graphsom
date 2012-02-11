
-module(graphsom_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/5]).

%% Supervisor callbacks
-export([init/1]).

%% Include

-include("graphsom.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% Default values for report interval, host and port

-define(REPORT_INTERVAL_MS, 10000).
-define(GRAPHITE_HOST, "localhost").
-define(GRAPHITE_PORT, 2003).
-define(GRAPHITE_PREFIX, "graphsom.").
-define(SYSTEM_STATS, [memory, system_info, statistics, process_info, port_info]).



%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.

start_link() ->
    start_link(?REPORT_INTERVAL_MS, ?GRAPHITE_HOST, ?GRAPHITE_PORT, ?SYSTEM_STATS, ?GRAPHITE_PREFIX).

-spec start_link(pos_integer(), string(), integer(), system_stats_type(), string()) -> {ok, pid()}.

start_link(ReportIntervalMs, GraphiteHost, GraphitePort, SystemStats, Prefix) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE,
                          [ReportIntervalMs, GraphiteHost, GraphitePort, SystemStats, Prefix]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================


-spec init(list()) -> {ok, term()}.

init(Parms = [ReportIntervalMs, GraphiteHost, GraphitePort, SystemStats, Prefix]) ->
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
                       [ReportIntervalMs,GraphiteHost, GraphitePort, SystemStats, Prefix]},
                      permanent, 
                      5000, 
                      worker, 
                      [graphsom_timer]},
    {ok, { {one_for_one, 100, 10}, [Folsom, GraphsomTimer]} }.
