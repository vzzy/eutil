REBAR = rebar3

all: 
	$(REBAR) clean -a
	$(REBAR) compile
	erl -pa _build/default/lib/eutil/ebin
	
.PHONY:all 


