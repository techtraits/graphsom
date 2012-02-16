REBAR=./rebar
DIALYZER=`which dialyzer || ./dialyzer`

all: deps compile

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

release: clean compile
	@$(REBAR) generate

run: release
	./rel/graphsom/bin/graphsom console

test:
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

plt:
	@$(DIALYZER) --build_plt --output_plt graphsom.plt \
		--apps kernel stdlib sasl \
		public_key ssl runtime_tools erts \
		compiler tools syntax_tools hipe webtool
		-pa deps/folsom/ebin \

analyze: compile
	@$(DIALYZER) ebin/*.beam --plt graphsom.plt \
		-pa deps/folsom/ebin \
		-Werror_handling \
		-Wunmatched_returns \
		-Wno_undefined_callbacks 
