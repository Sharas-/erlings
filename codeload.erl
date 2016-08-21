-module(codeload).

-compile(export_all).

hot_code({a, {_, _}=A}=B, [H1,H2|_]=C) ->
	io:format("~p", ["version1"]),
	{inner_tuple, A, all_tuple, B, head1, H1, head2, H2, all_list, C}.
