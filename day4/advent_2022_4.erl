-module(advent_2022_4).

-compile(export_all).

contained_sum(File, Part) ->
    {ok, B} = file:read_file(File),
    AllNumbers =
        [H || D <- binary:split(B, <<"\n">>, [global, trim_all]),
              E <- binary:split(D, <<",">>, [global, trim_all]),
              F <- binary:split(E, <<"-">>, [global, trim_all]),
              G <- [binary_to_list(F)],
              H <- [erlang:list_to_integer(G)]],
    case Part of
        1 -> do_sum(AllNumbers, 0, r1_fully_contains_r2_fun());
        2 -> do_sum(AllNumbers, 0, r1_overlap_r2_fun())
    end.

r1_fully_contains_r2_fun() ->
    fun(L1, H1, L2, H2) ->
            ((L1 =< L2) andalso (H1 >= H2)) orelse
            ((L2 =< L1) andalso (H2 >= H1))
    end.

r1_overlap_r2_fun() ->
    fun(L1, H1, L2, H2) ->
            ((L1 =< L2) andalso (H1 >= L2)) orelse
            ((L2 =< L1) andalso (H2 >= L1))
    end.

do_sum([L1,H1,L2,H2|Ranges], Sum, RangeCheck) ->
    case RangeCheck(L1,H1,L2,H2) of
        true ->
            do_sum(Ranges, Sum+1, RangeCheck);
        false ->
            do_sum(Ranges, Sum, RangeCheck)
    end;
do_sum([], Sum, _RangeCheck) ->
    Sum.
