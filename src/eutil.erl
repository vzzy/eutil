-module(eutil).

%% API exports
-export([
	check_process_alive/1,		 
		 
	partition/2,
	
	term_to_binary/1,
	term_to_bitstring/1,
	bitstring_to_term/1,
	term_to_string/1,
	string_to_term/1,		 
		 
	get_proc/1,
	start_app/1,	 
	
	random/1,
	list_to_atom/1,	
	
	get_pos/2, 	
	
	get_now/0, 
	get_seconds/0,
	get_timestamp/0,
	get_micros/0,
	seconds_to_local_time/1,
	uuid/0,
	jump_consistent_hash/2,
	
	gc/0,
	src/1,
	info/0
]).

%% 判断进程是否存活
check_process_alive(Pid) when is_pid(Pid)->
	Node = node(),
	case catch node(Pid) of
		{'EXIT',_R}->
			false;
		Node ->
			erlang:is_process_alive(Pid);
		Node_Pid->
			case catch rpc:call(Node_Pid, erlang, is_process_alive, [Pid],3*1000) of
				Flag when is_boolean(Flag)->
					Flag;
				_->
					false
			end
	end;
check_process_alive(_Pid)->
	false.

%% 找出列表元素所在下标
get_pos(E,List)->
	get_pos(E,List,0).
get_pos(_E,[],_Pos)-> -1;	
get_pos(E,[E|_T],Pos)->
	Pos;
get_pos(E,[_H|T],Pos)->
	get_pos(E,T,Pos+1).					


%% 取模方法
partition(_Key,Num_buckets) when Num_buckets=<0->
	0;
partition(Key,Num_buckets)->
	<<A:8,B:8,C:8,_Bin/binary>> = erlang:md5(Key),
	Num = erlang:abs(A bsl 16 + B bsl 8 + C), 
	Num rem Num_buckets.

%% term 转换成二进制
term_to_binary(Term) when is_binary(Term)->
	Term;
term_to_binary(Term)->
	list_to_binary(io_lib:format("~p", [Term])).
%% term转换为bitstring格式   [{a},1] => <<"[{a},1]">>
term_to_bitstring(Term) ->
    erlang:list_to_bitstring(io_lib:format("~w", [Term])).
%% 二进制字符串转term  <<"[{a},1]">> => [{a},1] 
bitstring_to_term(BitString) ->
    string_to_term(binary_to_list(BitString)).
%% term转换为string格式  [{a},1] => "[{a},1]"
term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~w", [Term]))).
%%string转换为term "[{a},1]"  => [{a},1]
string_to_term(String) ->
    case erl_scan:string(String++".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> error
            end;
        _Error ->
            error
    end.

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

%% UUID生成方法
%% @return string
uuid() ->
    Term = erlang:term_to_binary({node(), make_ref()}),
    Digest = erlang:md5(Term),
	[H1,H2,H3,H4,H5,H6,H7,H8,H9,H10,
	 H11,H12,H13,H14,H15,H16,H17,H18,H19,H20 | T] = binary_to_hex(Digest),
	List = [H1,H2,H3,H4,H5,H6,H7,H8,"-",H9,H10,
	 H11,H12,"-",H13,H14,H15,H16,"-",H17,H18,H19,H20,"-",T],
    list_to_binary(List).
binary_to_hex(Bin) when is_binary(Bin) ->
    [oct_to_hex(N) || <<N:4>> <= Bin].
oct_to_hex(0) -> $0;
oct_to_hex(1) -> $1;
oct_to_hex(2) -> $2;
oct_to_hex(3) -> $3;
oct_to_hex(4) -> $4;
oct_to_hex(5) -> $5;
oct_to_hex(6) -> $6;
oct_to_hex(7) -> $7;
oct_to_hex(8) -> $8;
oct_to_hex(9) -> $9;
oct_to_hex(10) -> $a;
oct_to_hex(11) -> $b;
oct_to_hex(12) -> $c;
oct_to_hex(13) -> $d;
oct_to_hex(14) -> $e;
oct_to_hex(15) -> $f.


%% Jump-consistent hashing.
%% OTP 19.3 does not support exs1024s
-define(SEED_ALGORITHM, exs1024).
jump_consistent_hash(_Key, 1) ->
    0;
jump_consistent_hash(KeyList, NumberOfBuckets) when is_list(KeyList) ->
    jump_consistent_hash(hd(KeyList), NumberOfBuckets);
jump_consistent_hash(Key, NumberOfBuckets) when is_integer(Key) ->
    SeedState = rand:seed_s(?SEED_ALGORITHM, {Key, Key, Key}),
    jump_consistent_hash_value(-1, 0, NumberOfBuckets, SeedState);
jump_consistent_hash(Key, NumberOfBuckets) ->
    jump_consistent_hash(erlang:phash2(Key), NumberOfBuckets).
jump_consistent_hash_value(B, J, NumberOfBuckets, _SeedState) when J >= NumberOfBuckets ->
    B;
jump_consistent_hash_value(_B0, J0, NumberOfBuckets, SeedState0) ->
    B = J0,
    {R, SeedState} = rand:uniform_s(SeedState0),
    J = trunc((B + 1) / R),
    jump_consistent_hash_value(B, J, NumberOfBuckets, SeedState).

% 对所有process做gc
gc() ->
    [erlang:garbage_collect(Pid) || Pid <- processes()].

%% 反编译源码
src(Mod) when is_atom(Mod)->
	case beam_lib:chunks(code:which(Mod),[abstract_code]) of
		{ok,{_,[{abstract_code,{_,AC}}]}} -> 
			io:fwrite("~s~n", [erl_prettypr:format(erl_syntax:form_list(AC))]);
		Others->
			Others
	end.

%% 系统监控信息
info()->
	List = erlang:memory(),
	Dict = lists:foldl(fun({K,V},Temp_Dict)->
		dict:store(K,V/1024/1024,Temp_Dict)
	end,dict:new(),List),

	Mem_total = dict:fetch(total,Dict),
	Mem_processes = dict:fetch(processes,Dict),
	Mem_processes_used = dict:fetch(processes_used,Dict),
	
	Mem_system = dict:fetch(system,Dict),
	
	Mem_atom = dict:fetch(atom,Dict),
	Mem_atom_used = dict:fetch(atom_used,Dict),

	Mem_binary = dict:fetch(binary,Dict),
	Mem_code = dict:fetch(code,Dict),
	Mem_ets = dict:fetch(ets,Dict),
	
	Process_limit = erlang:system_info(process_limit),
	Process_count = erlang:system_info(process_count),
	
	Atom_limit = erlang:system_info(atom_limit),
	Atom_count = erlang:system_info(atom_count),
	 
	Port_limit = erlang:system_info(port_limit),
	Port_count = erlang:system_info(port_count),

	io:format("=============Mem: ~.3..f MB===========~n",[Mem_total]),
	io:format("Process: ~.3..f - ~.3..f MB~n",[Mem_processes_used,Mem_processes]),
	io:format("Atom   : ~.3..f - ~.3..f MB~n",[Mem_atom_used,Mem_atom]),
	io:format("System : ~.3..f MB~n",[Mem_system]),
	io:format("Binary : ~.3..f MB~n",[Mem_binary]),
	io:format("Code   : ~.3..f MB~n",[Mem_code]),
	io:format("Ets    : ~.3..f MB~n",[Mem_ets]),
	io:format("~n",[]),

	io:format("=============Count============~n",[]),
	io:format("Process: ~p - ~p~n",[Process_count,Process_limit]),
	io:format("Atom   : ~p - ~p~n",[Atom_count,Atom_limit]),
	io:format("Prot   : ~p - ~p~n",[Port_count,Port_limit]),

	ok.



