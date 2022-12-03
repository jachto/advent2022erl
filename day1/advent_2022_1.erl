-module(advent_2022_1).

-compile(export_all).

run(File) ->
    {ok, Fd} = file:open(File, [read]),
    {ok, NthSumSorted} = total_calories(Fd, 0, [], 3),
    file:close(Fd),
    lists:sum(NthSumSorted).


total_calories(Fd, Sum, AllSumSorted, Nth) ->
    case file:read_line(Fd) of
        {ok, [D|_] = Data} when D >= $0 andalso D =< $9 ->
            Number = list_to_integer(lists:droplast(Data)),
            total_calories(Fd, Sum + Number, AllSumSorted, Nth);
        
        {ok, "\n"} ->
            NewAllSumSorted = 
                if length(AllSumSorted) >= Nth ->
                        tl(lists:merge([Sum], AllSumSorted));
                   true ->
                        lists:merge([Sum], AllSumSorted)
                end,
            total_calories(Fd, 0, NewAllSumSorted, Nth);

        eof ->
            {ok, AllSumSorted}
    end.
            
