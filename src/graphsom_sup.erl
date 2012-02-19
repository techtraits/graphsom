
-module(graphsom_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, start_link/6]).

%% Supervisor callbacks
-export([init/1]).

%% Include

-include("graphsom.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% Default values for report interval, host and port

-define(REPORT_INTERVAL_MS, 30000).
-define(GRAPHITE_HOST, "localhost").
-define(GRAPHITE_PORT, 2003).
-define(GRAPHITE_PREFIX, "graphsom.").
-define(VM_STATS, [memory, system_info, statistics, process_info, port_info]).
-define(REPORT_ALL_USER_METRICS, false).


%% ===================================================================
%% API functions
%% ===================================================================


-spec start_link(config()) -> {ok, pid()}.

start_link(Config) ->
    start_link(value(report_interval, Config), 
               value(graphite_host, Config), 
               value(graphite_port, Config),
               value(graphite_prefix, Config), 
               value(vm_metrics, Config),
               value(report_all_user_metrics, Config)).

-spec start_link(pos_integer(), string(), integer(), string(), vm_metrics_type(), boolean()) -> {ok, pid()}.

start_link(ReportIntervalMs, GraphiteHost, GraphitePort, Prefix, VmMetrics, AllUserMetrics) ->
    %% io:format("graphsom_sup: VmStats: ~w ~n", [VmMetrics]),
    supervisor:start_link({local, ?MODULE}, ?MODULE,
                          [ReportIntervalMs, GraphiteHost, GraphitePort, Prefix, VmMetrics, AllUserMetrics]).

%% ===================================================================

%% Supervisor callbacks
%% ===================================================================


-spec init(list()) -> {ok, term()}.

init([ReportIntervalMs, GraphiteHost, GraphitePort, Prefix, VmMetrics, AllUserMetrics]) ->
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
                       [ReportIntervalMs,GraphiteHost, GraphitePort, Prefix, VmMetrics, AllUserMetrics]},
                      permanent, 
                      5000, 
                      worker, 
                      [graphsom_timer]},
    {ok, { {one_for_one, 100, 10}, [Folsom, GraphsomTimer]} }.

%% private functions

-spec value(atom(), config()) -> term().

value(report_interval, Config) ->
    proplists:get_value(report_interval, Config, ?REPORT_INTERVAL_MS);

value(graphite_host, Config) ->
    proplists:get_value(graphite_host, Config, ?GRAPHITE_HOST);

value(graphite_port, Config) ->
    proplists:get_value(graphite_port, Config, ?GRAPHITE_PORT);

value(vm_metrics, Config) ->
    proplists:get_value(vm_metrics, Config, ?VM_STATS);

value(graphite_prefix, Config) ->
    proplists:get_value(graphite_prefix, Config, ?GRAPHITE_PREFIX);

value(report_all_user_metrics, Config) ->
    proplists:get_value(report_all_user_metrics, Config, ?REPORT_ALL_USER_METRICS).

