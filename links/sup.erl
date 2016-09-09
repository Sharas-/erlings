-module(sup).

-export([start_link/2, stop/1]).


-spec start_link(Funs, Strategy) -> Pid when
	  Funs :: [fun(()-> pid())],
	  Strategy :: one_for_one | all_for_one,
	  Pid :: pid().
start_link(Funs, all_for_one) ->
	restart_on_exit(fun()-> link_workers(Funs) end);
start_link(Funs, one_for_one) ->
	link_workers([fun()-> restart_on_exit(F) end || F <- Funs]).

stop(Sup) when is_pid(Sup) ->
	kill_em_all([Sup]).

kill_em_all([])->
	ok;
kill_em_all([Pid|Rem])->
	Linked = get_linked(process_info(Pid, links)),
	exit(Pid, kill),
	kill_em_all(Linked),
	kill_em_all(Rem).

get_linked(undefined)->
	[];
get_linked({links, Siblings})->
	Siblings.

link_workers(StartFuns)->
	spawn_link(fun()->
			lists:foreach(fun(F)-> F() end, StartFuns),
			receive after infinity -> ok end
		   end).

restart_on_exit(StartFun)->
	spawn_link(fun()->
		      process_flag(trap_exit, true),
		      restart_loop(StartFun)
	      end).

restart_loop(StartFun)->
	Pid = StartFun(),
	receive {'EXIT', Pid, Reason} ->
		error_logger:error_msg("Process ~p exited with reason ~p~n", [Pid, Reason]),
		restart_loop(StartFun)
	end.




