graphsom
===============

graphsom dumps provides visualizations for metrics collected using [folsom](https://github.com/boundary/folsom). Currently we support only [Graphite](http://graphite.wikidot.com/) visualizations but we will be adding suppor for multiple back ends soon. 

Note: [Graphite](http://graphite.wikidot.com/) is good, despite the
website being really ghetto. 

Status
------

* Supports [folsom's](https://github.com/boundary/folsom) `gauge`,
  `counter` and `meter` metrics
* Support for other metric types (`histogram` and `history`) and other
  visualization backends is coming very soon

Playing Around
--------------

**Prerequisite:** [Install](http://graphite.wikidot.com/installation)
  and run graphite locally
  
In graphsom's root directory:

Make [rebar](https://github.com/basho/rebar) executable
    chmod u+x rebar
    
Get graphsom's only dependency, namely
[folsom](https://github.com/boundary/folsom)

    mkdir deps
    ./rebar get-deps
    
Compile

    ./rebar compile

Start an erlang shell (eshell):

    erl -pa ebin/ -pa deps/folsom/ebin/
    
In eshell start graphsom application:

    application:start(graphsom).

Graphsom will dump all folsom metrics after every `30 seconds`
to the local graphite backend. 
**Note:** Graphsom takes care of starting `folsom`



Authors 
------

Bilal Sheikh (<bilal@techtraits.com>)  
Usman Ismail (<usman@techtraits.com>)


