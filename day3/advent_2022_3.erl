-module(advent_2022_3).

-compile(export_all).

group_sum(File) ->
    GroupSize = 3,
    {ok, B} = file:read_file(File),
    Backpacks = [Y || X <- binary:split(B, <<"\n">>,  [global, trim_all]),
                      Y <- [binary_to_list(X)]],
    SumFun = fun(Bp, {X, ThisGroupSets, PrioSum}) when X == GroupSize ->
                     Sets = [gb_sets:from_list(Bp)|ThisGroupSets],
                     [Letter] = gb_sets:to_list(gb_sets:intersection(Sets)),
                     {1, [], priority(Letter) + PrioSum};
                
                (Bp, {X, ThisGroupSets, PrioSum}) ->
                     {X+1, [gb_sets:from_list(Bp)|ThisGroupSets], PrioSum}
             end,
    {_, _, Sum} = lists:foldl(SumFun, {1, [], 0}, Backpacks),
    Sum.

duplicate_sum(File) ->
    {ok, B} = file:read_file(File),
    Prio = fun(L) -> priority(find_duplicate(L)) end,
    P = [Prio(Y) ||
            X <- binary:split(B, <<"\n">>,  [global, trim_all]),
            Y <- [binary_to_list(X)]
        ],
    lists:sum(P).

find_duplicate(L) -> find_duplicate(L, L, maps:new()).

find_duplicate([Letter|L1], [_,_|Half], L1Map) ->
    find_duplicate(L1, Half, maps:put(Letter, exists, L1Map));
find_duplicate([Letter|L2], [], L1Map) ->
    case maps:get(Letter, L1Map, non_existent) of
        non_existent ->
            find_duplicate(L2, [], L1Map);
        exists ->
            Letter
    end.

priority(Letter) when Letter >= $a andalso Letter =< $z ->
    Letter - $a + 1;
priority(Letter) when Letter >= $A andalso Letter =< $Z ->
    Letter - $A + 27.
