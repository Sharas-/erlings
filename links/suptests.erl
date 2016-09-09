-module(suptests).
-compile(export_all).

start_worker(Name)->
	io:format("worker ~p started~n", [Name]),
	register(Name, Pid = spawn_link(fun worker_loop/0)),
	Pid.

worker_loop()->
	receive {From, ping} ->
			From ! {self(), pong},
			worker_loop();
		stop ->	ok;
		{From, M} -> From ! list_to_tuple(M) 
	end.


track_exits(WorkerNames)->
	[spawn(fun()-> 
		Ref = monitor(process, Name),
		receive {'DOWN', Ref, _, Pid, Reason}->
			io:format("~p(~p) died from ~p~n", [Name, Pid, Reason])
		end
		end)
		|| Name <- WorkerNames].

start(WorkerNames, Strategy)->
	WorkerFuns = [fun()-> start_worker(Name) end || Name <- WorkerNames],
	sup:start_link(WorkerFuns, Strategy).

true_for_all_pids(Predicate, RegNames)->
	lists:all(Predicate, [whereis(Child) || Child <- RegNames]).

all_are_live(RegNames)->
	true_for_all_pids(fun(Child) -> is_pid(Child) end, RegNames).

all_are_dead(RegNames)->
	true_for_all_pids(fun(Child) -> Child =:= undefined end, RegNames).

test_stopping_one_for_one_sup_kills_all_children()->
	Children = [a, b, c],
	Sup = start(Children, one_for_one),
	unlink(Sup),
	sup:stop(Sup),
	timer:sleep(300),
	true = all_are_dead(Children).	

	
