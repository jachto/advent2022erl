-module(advent_2022_6).
-compile(export_all).

run(File, MarkerLength) when MarkerLength == 4 orelse MarkerLength == 14->
    {ok, B} = file:read_file(File),
    marker_chars(binary_to_list(B), [], 0, MarkerLength).

marker_chars(L, Unique, NChars, MLen) when length(Unique) == MLen -> {NChars, L};
marker_chars([C|L], Unique, NChars, MLen) ->
    {Remains, _Throw} = split(C, Unique, [], Unique),
    marker_chars(L, [C|Remains], NChars + 1, MLen).
        
split(Char, [Char|_] = L, Acc, _OrigL) -> {lists:reverse(Acc), L};
split(Char, [CharX|L]   , Acc,  OrigL) -> split(Char, L, [CharX|Acc], OrigL);
split(_Char, []         , _Acc, OrigL) -> {OrigL, []}.
