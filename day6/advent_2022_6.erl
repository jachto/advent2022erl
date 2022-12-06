-module(advent_2022_6).
-compile(export_all).

do(File, MLen, Algorithm)
  when MLen =< 16 andalso
       (Algorithm == split orelse Algorithm == sets) ->
    Pid = erlang:spawn(?MODULE, time, [File, MLen, self(), Algorithm]),
    receive
        {Pid, {N, {reductions, Ans}}} ->
            {N, {reductions, Ans}}
    end.

time(File, MLen, From, Algorithm) ->
    {reductions, R1} = lists:keyfind(reductions, 1, erlang:process_info(self())),
    {N, _} = run(File, MLen, Algorithm),
    {reductions, R2} = lists:keyfind(reductions, 1, erlang:process_info(self())),
    From ! {self(), {N, {reductions, R2 - R1}}}.

run(File, MarkerLength, Algorithm) ->
    {ok, B} = file:read_file(File),

    case Algorithm of
        sets ->
            %% MLen:reductions, 4:45096, 8:61549, 14:195317
            %%                 16:{4085,{reductions,235704}}
            m_chars(binary_to_list(B), queue:new(), 0, MarkerLength, 0, 0);

        split ->
            %% MLen:reductions, 4:10438, 8:10547 14:27524
            %%                 16:{4085,{reductions,29543}}
            marker_chars(binary_to_list(B), [], 0, MarkerLength)
    end.

marker_chars(L, Unique, NChars, MLen) when length(Unique) == MLen -> {NChars, L};
marker_chars([C|L], Unique, NChars, MLen) ->
    {Remains, _Throw} = split(C, Unique, [], Unique),
    marker_chars(L, [C|Remains], NChars + 1, MLen).
        
split(Char, [Char|_] = L, Acc, _OrigL) -> {lists:reverse(Acc), L};
split(Char, [CharX|L]   , Acc,  OrigL) -> split(Char, L, [CharX|Acc], OrigL);
split(_Char, []         , _Acc, OrigL) -> {OrigL, []}.

%% Alternative solution - slower but shorter.
%%
m_chars(L, _Q, N, MLen, MLen, MLen) ->
    {N, L};
m_chars([C|L], Q0, N, MLen, QLength, _SetLength) ->
    Q1 = queue:in(C, Q0),
    {_, Q2} = queue:out(Q1),
    Q3 = if QLength == MLen -> Q2; true -> Q1 end,
    S3 = ordsets:from_list(queue:to_list(Q3)),
    m_chars(L, Q3, N + 1, MLen, queue:len(Q3), ordsets:size(S3)).


    

    
    
