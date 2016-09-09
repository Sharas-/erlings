-module(tests).
-compile(export_all).

start_worker(Name)->
	io:format("worker ~p started~n", [Name]),
	register(Name, Pid = spawn_link(fun worker_loop1/0)),
	Pid.

worker_loop1()->
	receive {From, ping} ->
			From ! {self(), pong},
			worker_loop1();
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



worker_loop()->
	worker_loop([]).
worker_loop(Exits)->
	receive {'EXIT', Pid, Reason} ->
			worker_loop([{Pid, Reason} | Exits]);
		{Pid, get_exits} ->
			rpcc:send(Pid, Exits),
			worker_loop(Exits);
		{Pid, {trap_exits, Switch}}->
			rpcc:send(Pid, process_flag(trap_exit, Switch)),
			worker_loop(Exits);
		stop->
			ok
	end.



spawn_tree({Name, Children}) when is_list(Children) -> 
	register(Name, spawn_link(fun()-> spawn_children(Children), worker_loop([]) end)).

spawn_children([])-> ok;
spawn_children([{_Name, _Children} = Parent | T])->
	spawn_tree(Parent),
	spawn_children(T);
spawn_children([Name | T]) when is_atom(Name) ->
	register(Name, spawn_link(fun worker_loop/0)),
	spawn_children(T).

	
kill(RegName)->
	case whereis(RegName) of 
		undefined -> 
			ok;
		Pid ->
			exit(Pid, kill)
	end.

true_for_all_pids(Predicate, RegNames)->
	lists:all(Predicate, [whereis(Child) || Child <- RegNames]).

all_are_live(RegNames)->
	true_for_all_pids(fun(Child) -> is_pid(Child) end, RegNames).

all_are_dead(RegNames)->
	true_for_all_pids(fun(Child) -> Child =:= undefined end, RegNames).

get_exits(RegName)->
	timer:sleep(300),
	rpcc:rpc(RegName, get_exits).

test_system_process_gets_exit_signal()->
	try
		spawn_tree({a, [aa]}),
		rpcc:rpc(a, {trap_exits, true}),
		KillTarget = whereis(aa),
		exit(KillTarget, exit),
		Exits = get_exits(a),
		true = Exits =:= [{KillTarget, exit}]
	after
		kill(a)
	end.


test_system_process_gets_exit_signal_from_directly_linked_process_only()->
	try
		spawn_tree({a, [aa, ab, {ac, [ca, cb, cc]}, ad]}),
		rpcc:rpc(a, {trap_exits, true}),
		SubRoot = whereis(ac),
		exit(whereis(cb), exit),
		Exits = get_exits(a),
		true = Exits =:= [{SubRoot, exit}]
	after
		kill(a)
	end.

test_system_process_stops_exit_signals()->
	try
		spawn_tree({a, [aa, ab, {ac, [ca, cb, cc]}, ad]}),
		rpcc:rpc(a, {trap_exits, true}),
		exit(whereis(cb), exit),
		timer:sleep(300),
		true = all_are_dead([ac, ca, cb, cc]),
		true = all_are_live([a, aa, ab, ad])
	after
		kill(a)
	end.

test_all_linked_tree_dies()->
	try
		spawn_tree({a, [aa, ab, {ac, [ca, cb, cc]}, ad]}),
		timer:sleep(300),
		exit(whereis(cb), exit),
		timer:sleep(300),
		true = all_are_dead([a, aa, ab, ad, ac, ca, cb, cc])
	after
		kill(a)
	end.

test_killed_parent_doesnt_kill_trapping_children()->
	Children=[aa, ab, ac],
	try
		spawn_tree({a, Children}),
		timer:sleep(300),
		lists:foreach(fun(Child)-> rpcc:rpc(Child, {trap_exits, true}) end, Children),
		rpcc:rpc(a, {trap_exits, true}),
		exit(whereis(a), kill),
		timer:sleep(300),
		undefined = whereis(a),
		true = all_are_live(Children)
	after
		lists:foreach(fun tests:kill/1, Children)	
	end.
