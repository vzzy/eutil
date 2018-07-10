-module(eutil).

%% API exports
-export([
	get_proc/1,

	start_app/1,	 
	
	random/1,
	list_to_atom/1,	 	
	
	get_now/0, 
	get_seconds/0,
	get_timestamp/0,
	get_micros/0,
	seconds_to_local_time/1,
	
	gc/0,
	src/1
]).

%% 进程名获取方法
%% @param List  list
%% @return atom
get_proc(List)->
	eutil:list_to_atom(lists:concat(lists:join("@",List))).

%% 启动App
%% @param App term()
start_app(App) ->
    start_app_sub(App, application:start(App, permanent)).
start_app_sub(_App, ok) -> ok;
start_app_sub(_App, {error, {already_started, _App}}) -> ok;
start_app_sub(App, {error, {not_started, Dep}}) ->
    ok = start_app(Dep),
    start_app(App);
start_app_sub(App, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).

%%返回1~N之间的随机数
%% @param N >=1的一个整数
%% @return int
random(N)when is_integer(N) andalso N>=1->
	<<A:32,B:32,C:32>> = crypto:strong_rand_bytes(12),
	rand:seed(exs1024s,{A,B,C}),
	rand:uniform(N).

%% 改写erlang:list_to_atom()，更高效一些。
%% @param List list
%% @return atom
list_to_atom(List)->
	case catch erlang:list_to_existing_atom(List) of
		{'EXIT',_}->
			erlang:list_to_atom(List);
		Atom->
			Atom
	end.

%% 获取当前时间 yyyy-mm-dd hh:mm:ss
get_now()->
	{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    list_to_binary(lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",[Year, Month, Day, Hour, Minute, Second]))).

%%获取从公元1970年到当前时间的秒数
get_seconds()->
	Micros = get_micros(),
	Micros div 1000000.

%%获取从公元1970年到当前时间的毫秒数
get_timestamp() ->
	Micros = get_micros(),
	Micros div 1000.

%%获取从公元1970年到当前时间的微秒数
get_micros()->
	{MegaSecs,Secs,Micro} = os:timestamp(),
	(MegaSecs*1000000 + Secs)*1000000 + Micro.

%%秒数转成日期时间
%%@param Seconds 从公元1970年到当前时间的秒数
%%@return {{2012,12,1},{23,52,2}}
seconds_to_local_time(Seconds)->
	MegaSecs = Seconds div 1000000,
	Secs = Seconds rem 1000000,
	calendar:now_to_local_time({MegaSecs,Secs,0}).


% 对所有process做gc
gc() ->
    [erlang:garbage_collect(Pid) || Pid <- processes()].

%% 反编译源码
src(Mod) when is_atom(Mod)->
	{ok,{_,[{abstract_code,{_,AC}}]}} = beam_lib:chunks(code:which(Mod),[abstract_code]),
	io:fwrite("~s~n", [erl_prettypr:format(erl_syntax:form_list(AC))]).




