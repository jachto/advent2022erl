-module(advent_2022_5).

-compile(export_all).

run(File, CrateMoverType)
  when CrateMoverType == 9000 orelse
       CrateMoverType == 9001 ->
    {ok, Fd} = file:open(File, [read]),
    {ok, Top} = top_of_stacks(Fd, CrateMoverType),
    file:close(Fd),
    Top.


top_of_stacks(Fd, CrateMoverType) ->
    {ok, {Moves, Stacks}} = get_from_file(Fd),
    StacksNew = do_moves(Moves, Stacks, CrateMoverType),
    {ok, tops(erlang:tuple_to_list(StacksNew))}.

tops([[Top|_] | StackList]) -> [Top|tops(StackList)];
tops([]) -> [].

do_moves([{N, From, To}|Moves], Stacks, CrateMoverType) ->
    FromStack = element(From, Stacks),
    {NCrate, FromStackNew} = lists:split(N, FromStack),
    ToStackNew =
        case CrateMoverType of
            9000 -> lists:reverse(NCrate, element(To, Stacks));
            9001 -> NCrate ++ element(To, Stacks)
        end,
    Stacks2 = erlang:setelement(To, Stacks, ToStackNew),
    Stacks3 = erlang:setelement(From, Stacks2, FromStackNew),
    do_moves(Moves, Stacks3, CrateMoverType);
do_moves([], Stacks, _CrateMoverType) ->
    Stacks.

get_from_file(Fd) ->
    Moves = [],
    Stacks = {[],[],[],[],[],[],[],[],[]},
    get_from_file(Fd, Moves, Stacks).

get_from_file(Fd, Moves, Stacks) ->
    case file:read_line(Fd) of
        {ok, "move" ++ _Data = Move} ->
            get_from_file(Fd, [get_move(Move)|Moves], Stacks);

        {ok, [Char|_] = StackAny}
          when Char == $\s orelse Char == $[ ->
            get_from_file(Fd, Moves, get_stacks(StackAny, Stacks));

        {ok, _OtherLines} ->
            get_from_file(Fd, Moves, Stacks);

        eof ->
            {ok, {lists:reverse(Moves), Stacks}}
    end.

get_move(Move) ->
    MoveTriple = [],
    get_move(Move, MoveTriple).

get_move("move" ++ Move, T) ->
    get_move(Move, T);
get_move("from" ++ Move, T) ->
    get_move(Move, T);
get_move("to" ++ Move, T) ->
    get_move(Move, T);
get_move([$\s|Move], T) ->
    get_move(Move, T);
get_move([Digit|_] = Move, T)
  when Digit >= $0 andalso Digit =< $9 ->
    {Integer, MoveNew} = get_integer(Move),
    TripleRev = [Integer|T],
    case TripleRev of
        [D3,D2,D1] ->
            {D1,D2,D3};
        _NoFullTriple ->
            get_move(MoveNew, TripleRev)
    end.

get_integer(Move) -> get_integer(Move, []).

get_integer([Digit|Ds], DigitsRev)
  when Digit >= $0 andalso Digit =< $9 ->
    get_integer(Ds, [Digit|DigitsRev]);
get_integer(Move, DigitsRev) ->
    {erlang:list_to_integer(lists:reverse(DigitsRev)),
     Move}.

get_stacks(StackAny, Stacks) ->
    Nth = 1,
    get_stacks(StackAny, Nth, Stacks).

get_stacks([$[,Letter,$]|StackAny], Nth, Stacks) ->
    StackNew = erlang:element(Nth, Stacks) ++ [Letter],
    StacksNew = erlang:setelement(Nth, Stacks, StackNew),
    get_stacks(StackAny, Nth+1, StacksNew);
get_stacks([$\s,$\s,$\s,$\s|StackAny], Nth, Stacks) ->
    get_stacks(StackAny, Nth+1, Stacks);
get_stacks([$\n|StackAny], _Nth, Stacks) ->
    get_stacks(StackAny, 1, Stacks);
get_stacks([_Char|StackAny], Nth, Stacks) ->
    get_stacks(StackAny, Nth, Stacks);
get_stacks([], _Nth, Stacks) ->
    Stacks.
