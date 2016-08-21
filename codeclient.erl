-module(codeclient).

-compile(export_all).

start(Tag)->
	spawn(?MODULE, loop, [Tag]).

loop(Tag)->
	wait(),
	io:format("~p~n", ["naujas"]),
	io:format("~p: ~p~n", [Tag, codeload:hot_code({a, {1,2}}, [1,2,3])]), 
	loop(Tag).

wait()->
	receive
		after 3000 -> ok
	end.
