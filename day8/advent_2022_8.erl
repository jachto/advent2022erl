-module(advent_2022_8a).

-compile(export_all).

-define(is_digit(D), ((D >= $0) andalso (D =< $9))).

%%
%% A solution using lists storing only the numbers of the tree hight
%% became too difficult to handle due to some cases were
%% there are overlapping viewings of a tree.
%%
%% When the stored entity were changed to
%%   {TreeHight, <1 for seen, 0 for not seen>}
%% part 1 was solvable.
%%
%% Part 2 however became too difficult as here you had to be able to traverse
%% the two X and Y directions for the same considered midpoint-tree.
%% To solve part 2 I hade to start using a maps with the key {X, Y}.
%%

run(File, Part) when Part == 1 orelse Part == 2 ->
    {ok, Fd} = file:open(File, [read]),
    Ans = case Part of
              1 ->
                  sum_seen_trees(Fd);
              2 ->
                  to_be_done
          end,
    file:close(Fd),
    Ans.



%%
%% Part 1 solved by using lists.
%%
-define(hight(Entity), element(1, Entity)).
-define(seen(Entity),  erlang:setelement(2, Entity, 1)).

sum_seen_trees(Fd) ->
    Row = [],
    {ok, Rs} = get_from_file(Fd, Row),
    Rs,
    Nth = 1,
    LinesRev = [],
    Rows = sum_visible(Rs, Nth, LinesRev),
    Cs = add_to_columns(Rows, []),
    Cols = sum_visible(Cs, Nth, LinesRev),
    lists:foldl(fun({_Hight, Seen}, Sum) -> Sum + Seen end,
                0,
                lists:flatten(Cols)).
    
get_from_file(Fd, Rs) ->
    case file:read_line(Fd) of
        {ok, ARow} ->
            Row = lists:droplast(ARow),
            io:format("ROW:~s~n", [Row]),
            RowNew = row_convert(Row),
            get_from_file(Fd, [RowNew|Rs]);

        eof ->
            {ok, lists:reverse(Rs)}
    end.

sum_visible([Line|Lines], Nth, LinesRev) ->
    Line1 = sum_line(Line, -1, []),
    Line2 = lists:reverse(sum_line(lists:reverse(Line1), -1, [])),
    sum_visible(Lines, Nth+1, [Line2|LinesRev]);
sum_visible([], _Nth, LinesRev) ->
    lists:reverse(LinesRev).

sum_line([L|Ls], Max, LsRev) ->
    if ?hight(L) > Max ->
            sum_line(Ls, ?hight(L), [?seen(L)|LsRev]);
       true ->
            sum_line(Ls, Max, [L|LsRev])
    end;
sum_line([], _Max, LsRev) ->
    lists:reverse(LsRev).

row_convert([Digit|Line]) when ?is_digit(Digit) ->
    [{Digit - $0, 0} | row_convert(Line)];
row_convert(_) ->
    [].

update_to_tuple([C|Cs]) -> [update_to_tuple_2(C)|update_to_tuple(Cs)];
update_to_tuple([]) -> [].

update_to_tuple_2([E|Line]) -> [{E,0}|update_to_tuple_2(Line)];
update_to_tuple_2([]) -> [].

add_to_columns(Row, []) ->
    Cs = lists:map(fun(_E) -> [] end, hd(Row)),
    add_to_columns_1(Row, Cs).

add_to_columns_1([Row], Cs) ->
    add_to_columns_2(Row, Cs, rev);
add_to_columns_1([Row|Rs], Cs) ->
    CsNew = add_to_columns_2(Row, Cs, no_rev),
    add_to_columns_1(Rs, CsNew);
add_to_columns_1([], []) ->
    [].

add_to_columns_2([C|Row], [Col|Cs], no_rev) ->
    [[C|Col] | add_to_columns_2(Row, Cs, no_rev)];
add_to_columns_2([C|Row], [Col|Cs], rev) ->
    [lists:reverse([C|Col]) | add_to_columns_2(Row, Cs, rev)];
add_to_columns_2([], _, _) ->
    [].

    



