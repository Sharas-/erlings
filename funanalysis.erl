-module(funanalysis).

-compile(export_all).

names_in_mods(ModFuns) ->
	funs:pivot(ModFuns).

most_used_name(ModFuns)->
	maps:fold(fun(K, V, {_, Mods}=Acc) -> if V > Mods -> {K, V}; true -> Acc end end, {x, []}, names_in_mods(ModFuns)).

most_used_name_loaded()->
	most_used_name([{Mod, Mod:module_info(exports)} || {Mod, _} <- code:all_loaded()]).

unique_names(ModFuns)->
	lists:filter(fun({_, Mods})->length(Mods) =:= 1 end, names_in_mods(ModFuns)).

