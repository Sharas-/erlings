-module(cheep).

-compile(export_all).

process_galore(N)->
	io:format("process limit: ~p~n", [erlang:system_info(process_limit)]),
	statistics(runtime),
	statistics(wall_clock),
	L = for(0, N, fun()-> spawn(fun() -> wait() end) end),
	{_, Rt} = statistics(runtime),
	{_, Wt} = statistics(wall_clock),
	lists:foreach(fun(Pid)-> Pid ! die end, L),
	io:format("Processes created: ~p~n
		  Run time ms: ~p~n
		  Wall time ms: ~p~n
		  Average run time: ~p microseconds!~n", [N, Rt, Wt, (Rt/N)*1000]).

		  


wait()->
	receive die ->
		 done
	end.

for(N, N, _)->
	[];
for(I, N, Fun) when I < N ->
	[Fun()|for(I+1, N, Fun)].
