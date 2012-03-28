graphsom
===============

graphsom dumps provides visualizations for metrics collected using [folsom](https://github.com/boundary/folsom). Currently we support only [Graphite](http://graphite.wikidot.com/) visualizations but we will be adding support for multiple backends soon. 

Note: [Graphite](http://graphite.wikidot.com/) is good, despite the
website being really ghetto. 

Status
------

* Supports all [folsom](https://github.com/boundary/folsom) metric
  types (`gauge`, `counter`, `meter`, `histogram` and `history`)
* Supports all [folsom](https://github.com/boundary/folsom) VM metrics
  including `memory`,`statistics`, `process_info`, `system_info` and `port_info`
* Support for other visualization backends is coming very soon

Getting Started
--------------

Prerequisites 

* [Download](https://launchpad.net/graphite/+download),
  [Install](http://graphite.wikidot.com/installation) and run graphite
* [Download](https://github.com/downloads/basho/rebar/rebar) or [build](https://github.com/basho/rebar) rebar
* Clone [graphsom](https://github.com/techtraits/graphsom.git)

Building graphsom

    cd graphsom
	chmod u+x rebar  # Make [rebar](https://github.com/basho/rebar) executable  
    
Get all dependencies (only [folsom](https://github.com/boundary/folsom))

    mkdir deps
    ./rebar get-deps
    
Compile and generate a release

    ./rebar compile
    ./rebar generate
    
Running graphsom node

    ./rel/graphsom/bin/graphsom console

Graphsom will dump all `folsom metrics` every `30 seconds`
to the local graphite backend.

**Note:** Graphsom takes care of starting `folsom`

Add Some metrics:

    folsom_metrics:new_counter(metric_name_1).
    folsom_metrics:new_gauge(metric_name_2).
    folsom_metrics:new_meter(metric_name_3).

Update values for the metrics:

	folsom_metrics:notify({metric_name_1, {inc, 100}}).
    folsom_metrics:notify({metric_name_2, 200}).
    folsom_metrics:notify({metric_name_3, 300}).

Check graphite for the values:    

Authors 
------

Bilal Sheikh (<bilal@techtraits.com>)  
Usman Ismail (<usman@techtraits.com>)
