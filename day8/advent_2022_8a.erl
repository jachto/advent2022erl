-module(advent_2022_8a).

-compile(export_all).

%% Attempt holding rows and columns in lists.
%% It became too complex to adjust for all
%% overlapping cases where trees are counted more than once.
%% Abandon implementation.

run(File) ->
    {ok, Fd} = file:open(File, [read]),
    Sum = vt(Fd),
    file:close(Fd),
    Sum.

vt(Fd) ->
    Row = [],
    Col = [],
    {ok, {Rows, Cols}} = get_from_file(Fd, Row, Col),
    Nth = 1,
    Rs = tl(lists:droplast(Rows)),
    Cs = tl(lists:droplast(Cols)),
    {SumR, SingleMaxInxR} = sum_visible(Rs, 0, Nth, row, []),
    {SumC, SingleMaxInxC} = sum_visible(Cs, 0, Nth, col, []),
    CrossMax = ordsets:intersection(ordsets:from_list(SingleMaxInxR),
                                    ordsets:from_list(SingleMaxInxC)),
    SubCrossMax = 3 * length(CrossMax),
    SubSingleMax = 1 * (length(SingleMaxInxR -- CrossMax) +
                            length(SingleMaxInxC -- CrossMax)),
    SumR + SumC - SubCrossMax - SubSingleMax.
    
sum_visible([Line|Lines], Sum, Nth, LineType, Inxs) ->
    LSum = sum_line(Line, lists:reverse(Line), 0, -1, -1),
    InxsNew = case {LineType, max_single_inx(Line)} of
                  {_, 0}     -> Inxs;
                  {row, Inx} -> [{Inx, Nth} | Inxs];
                  {col, Inx} -> [{Nth, Inx} | Inxs]
              end,
    sum_visible(Lines, Sum + LSum, Nth+1, LineType, InxsNew);
sum_visible([], Sum, _Nth, _LineType, Inxs) ->
    {Sum, Inxs}.

max_single_inx(Line) -> max_single_inx(Line, -1, 1, 0).

max_single_inx([Max|Xs], Max, Nth, _Inx) ->
    max_single_inx(Xs, Max, Nth+1, 0);
max_single_inx([X|Xs], Max, Nth, _Inx) when X > Max ->
    max_single_inx(Xs, X, Nth+1, Nth);
max_single_inx([X|Xs], Max, Nth, Inx) when X < Max ->
    max_single_inx(Xs, X, Nth+1, Inx);
max_single_inx([], _, _Nth, Inx) ->
    Inx.

sum_line([L|Ls], [R|Rs], Sum, MaxL, MaxR) ->
    {AddL, MaxLNew} = if L > MaxL -> {1, L}; true -> {0, MaxL} end,
    {AddR, MaxRNew} = if R > MaxR -> {1, R}; true -> {0, MaxR} end,
    sum_line(Ls, Rs, Sum+AddL+AddR, MaxLNew, MaxRNew);
sum_line([], [], Sum, _MaxL, _MaxR) ->
    Sum.

get_from_file(Fd, Rs, Cs) ->
    case file:read_line(Fd) of
        {ok, ARow} ->
            Row = lists:droplast(ARow),
            io:format("ROW:~p~n", [Row]),
            get_from_file(Fd, [Row|Rs], add_to_columns(Row, Cs));

        eof ->
            {ok, {reverse_row(Rs), reverse_col(Cs)}}
    end.

add_to_columns([C|TheRow], [Col|Cs]) ->
    [[C|Col] | add_to_columns(TheRow, Cs)];
add_to_columns([], []) ->
    [];
add_to_columns(Row, []) ->
    Cs = lists:map(fun(E) -> [E] end, Row),
    add_to_columns(Row, Cs).

reverse_col([C|Cs]) -> [lists:reverse(C)|reverse_col(Cs)];
reverse_col([]) -> [].


