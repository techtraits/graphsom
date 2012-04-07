graphsom
===============


graphsom is a flexible metrics management system for Erlang
applications. It's main goal is for easier collection, management and
reporting of metrics to multiple visualization backends. 

**[Getting started guide](https://github.com/techtraits/graphsom/wiki/Getting-Started-Guide)**

Features
--------

**Multiple Metric Systems Support**

* [Folsom Metrics](https://github.com/boundary/folsom):
    * All metric types: `gauge`, `counter`, `meter`, `histogram` and `history`
    * All VM metrics: `memory`,`statistics`, `process_info`, `system_info` and `port_info`
* A simple callback-based **Graphsom Metrics API** is available for custom
  user-defined metrics
* Support for [estatsd](https://github.com/RJ/estatsd) coming soon

**Visualization Backends Support**  

* Currently, only [Graphite](http://graphite.wikidot.com/) is
  supported. Support for other visualization backends coming soon

Create and Register Folsom Metrics
----------------------------------

Add some folsom metrics ([folsom API](https://github.com/boundary/folsom)):

    folsom_metrics:new_counter(metric_name_1).
    folsom_metrics:new_meter(metric_name_2).

Update values for the metrics ([folsom API](https://github.com/boundary/folsom)):

	folsom_metrics:notify({metric_name_1, {inc, 100}}).
    folsom_metrics:notify({metric_name_2, 300}).

Register folsom metric with `graphsom` for reporting:

    graphsom:register_folsom_metric(metric_name_1).

De-register folsom metric:

    graphsom:deregister_folsom_metric(metric_name_1).

To get the list of currently registered folsom metrics

    graphsom:registered_metrics().

Custom Metrics
--------------

Graphsom metrics API is offered when a custom user-defined
metric needs to be reported. 
  
To register a custom metric:

    graphsom:register_graphsom_metric(METRIC_NAME, MODULE, FUN, PARAMS).

Graphsom expects a property list as a response to the callback
`MODULE:FUN` with `PARAMS` as parameters. 
Note that `FUN` must be exported by the module `MODULE` with the a correct arity.    

For example, we can use Graphsom API to report the number children of a `worker_sup` supervisor, i.e., the result of

    > supervisor:count_children(worker_sup).
    > [{specs,2},{active,2},{supervisors,1},{workers,1}]

This can be directly registered with graphsom as follows:
    
    graphsom:register_graphsom_metric(worker_count, supervisor, count_children, [worker_sup]).

Reporting Metrics
-----------------


**Interval-Based Reporting**

Tell graphsom to start reporting. All registered metrics are
reported periodically after a configurable interval. 

    graphsom:start_reporting().
    
**Manual Reporting**    

    graphsom:report_now().
    

Check graphite for the values!!

Configuration
-------------

The configuration parameters for graphsom can be added to application
config file (`[RELEASE]/files/sys.config`). An example configuration:
    
    {graphsom, [
             {report_interval, 30000},     %% report interval (ms)
             {graphite_host, "techtraits.com"}, %% defaults to localhost
             {graphite_port, 2003},        %% defaults to 2003
             {graphite_prefix, "graphsom"}, %% prefix added to all metrics
             {vm_metrics, [memory, statistics]},   %% defaults to []
             {report_all_folsom_metrics, false}   %% defaults to false
            ]}
           
* `graphite_prefix` can be used to specify the prefix prepended to all
metric names. For example, "response_time" would be reported as
"graphsom.response_time". 

* `vm_metrics` can be used to report folsom VM metrics

* If `report_all_folsom_metrics` is set to `true`, graphsom reports all folsom
  metrics that exist at the time of reporting. Alternatively,
  `register_folsom_metric` API can be used for reporting selective metrics to graphite.

Authors 
-------

Bilal Sheikh (<bilal@techtraits.com>)  
Usman Ismail (<usman@techtraits.com>)
