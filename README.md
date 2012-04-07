graphsom
===============

graphsom dumps provides visualizations for metrics collected using
[folsom](https://github.com/boundary/folsom). Currently graphsom only
supports [Graphite](http://graphite.wikidot.com/) visualizations.
Support for multiple backends will be added soon. 

Note: [Graphite](http://graphite.wikidot.com/) is good, despite the
website being really ghetto. 

**[Getting Started Guide](https://github.com/techtraits/graphsom/wiki/Getting-Started-Guide)**

Status
------

* Supports all [folsom](https://github.com/boundary/folsom) metric
  types: `gauge`, `counter`, `meter`, `histogram` and `history`
* Supports all [folsom](https://github.com/boundary/folsom) VM metrics
  including `memory`,`statistics`, `process_info`, `system_info` and `port_info`
* Support for other visualization backends coming soon

Usage
-----

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

Tell graphsom to start reporting:

    graphsom:start_reporting().
    
Check graphite for the values!!

To get the list of currently registered folsom metrics

    graphsom:registered_metrics().

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
