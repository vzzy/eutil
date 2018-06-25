-module(eutil).

%% API exports
-export([
	get_seconds/0,
	get_timestamp/0,
	get_micros/0,
	seconds_to_local_time/1,
	
	gc/0		 
]).

%%====================================================================
%% API functions
%%====================================================================


%%====================================================================
%% Internal functions
%%====================================================================

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


