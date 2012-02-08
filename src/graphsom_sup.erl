
-module(graphsom_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(ReportIntervalMs, GraphiteHost, GraphitePort) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE,
                          [ReportIntervalMs, GraphiteHost, GraphitePort]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([ReportIntervalMs, GraphiteHost, GraphitePort]) ->
    %% adding folsom_sup and graphsom to graphsom_sup's supervision tree
    Folsom = {folsom,
              {folsom_sup, start_link, []},
              permanent,
              5000,
              supervisor,
              [folsom_sup]
             },
    GraphsomServer = {graphsom_server,
                      {graphsom_server, start_link, 
                       [ReportIntervalMs,GraphiteHost, GraphitePort]},
                      permanent, 
                      5000, 
                      worker, 
                      [graphsom_server]},
    {ok, { {one_for_one, 5, 10}, [Folsom, GraphsomServer]} }.
