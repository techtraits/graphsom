%% Default configurations for various visualization adaptors
%% Currently Graphite and Collectd

-define(GRAPHSOM_CONFIG_KEY, graphsom).
-define(GRAPHSOM_BACKENDS, [graphite, collectd]).

-define(GRAPHSOM_GRAPHITE, 
            [
             {report_interval, 30000}, %% report interval (ms)
             {host, "localhost"},
             {port, 2003},
             {prefix, "graphsom"}, %% prefix for all metrics
             {vm_metrics, []},
             {report_all, false} %% report all folsom metrics
            ]
        ).

-define(GRAPHSOM_COLLECTD, []).
