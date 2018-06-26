REBAR = rebar3

git:
	git pull
	git add -A
	git commit -m '新增方法'	
	git push origin master

all: 
	$(REBAR) clean -a
	$(REBAR) compile
	erl -pa _build/default/lib/eutil/ebin
	
.PHONY:git all 


