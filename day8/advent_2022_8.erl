-module(advent_2022_8).

-compile(export_all).

run(File) ->
    {ok, Fd} = file:open(File, [read]),
    {ok, Top} = vt(Fd),
    file:close(Fd),
    Top.

vt(Fd) ->
    Row = [],
    Col = [],
    {Rs, Cs} = get_from_file(Fd, Row, Col),
    RowLen = length(hd(Rs)),
    ColLen = length(hd(Cs)),
    MaxRowInx = get_max_indesis(Rs),
    MaxColInx = get_max_indesis(Cs),
    sum_visible(MaxRowInx, RowLen, 0) + sum_visible(MaxColInx, ColLen, 0).

%% [ 3 4 5 5 6 6 5 5 4  3 ]
sum_visible([{MaxThisSide, MaxOtherSide} | MaxInx], Length, Sum) ->
    DSum = MaxThisSide + (Length - MaxOtherSide + 1),
    sum_visible(MaxInx, Length, Sum + DSum);
sum_visible([], _Length, Sum) ->
    Sum.


    
    

get_max_indesis([Line|Ls]) ->
    [max_inx_pair(Line, 1, {1, 1}, {-1, -2}) | get_max_indesis(Ls)];
get_max_indesis([]) ->
    [].

max_inx_pair([E|Es], I, {IL, IR}, {MaxL, MaxR}) when MaxR > -1 ->
    if E >= MaxR ->
            max_inx_pair(Es, I+1, {IL, I}, {MaxL, E});
       true ->
            max_inx_pair(Es, I+1, {IL, IR}, {MaxL, MaxR})
    end;
max_inx_pair([E|Es], I, {IL, IR}, {MaxL, MaxR}) ->
    if E > MaxL ->
            max_inx_pair(Es, I+1, {I, IR}, {E, MaxR});
       true ->
            max_inx_pair(Es, I+1, {IL, IR}, {MaxL, MaxR})
    end;
max_inx_pair([], _I, {IL, IR}, {_MaxL, _MaxR}) ->
    {IL, IR}.

add_to_columns([C|TheRow], [Col|Cs]) -> [[C|Col] | add_to_columns(TheRow, Cs)];
add_to_columns(Row, []) -> add_to_columns(Row, new_column(Row, []));
add_to_columns([], []) ->  [].

new_column([_C|CsInR], Col) -> new_column(CsInR, [[]|Col]);
new_column([], Col) -> Col.

reverse_columns([C|Cs]) -> [lists:reverse(C)|reverse_columns(Cs)];
reverse_columns([]) -> [].


get_from_file(Fd, Rs, Cs) ->
    case file:read_line(Fd) of
        {ok, Row} ->
            get_from_file(Fd, [Row|Rs], add_to_columns(Row, Cs));

        eof ->
            {ok, {lists:reverse(Rs), reverse_columns(Cs)}}
    end.
