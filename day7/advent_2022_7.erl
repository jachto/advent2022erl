-module(advent_2022_7).

-compile(export_all).

-define(is_digit(D), ((D >= $0) andalso (D =< $9))).

run(File) ->
    {ok, B} = file:read_file(File),
    {ok, Dirs} = dir_size(binary_to_list(B), maps:new()),
    Dirs.

dir_size(L, Map) ->
    Path = [],
    parse(L, Map, Path).

parse("$ cd /" ++ L, Map, Path) ->
    parse(L, maps:put(Path, 0, Map), Path);
parse("$ ls" ++ L, Map, Path) ->
    parse_ls(L, Map, Path).


parse_ls("dir" ++ L, Map, Path) ->
    parse_ls(L, Map, Path); %% skip now
parse_ls([Digit|_] = L, Map, Path) when ?is_digit(Digit) ->
    Map1 = update_dir_size(L, Map, Path),
    parse_ls(L, Map1, Path);
parse_ls(_L, _Map, _Path) ->
    apa.

update_dir_size(L, Map, Path) ->
    DirSizeCurrent = maps:get(Path, Map),
    {Size, FileName} = get_file_info(L),
    Map1 = maps:put(FileName, Size + DirSizeCurrent, Map),
    maps:put(Path, Size, Map1).
    
get_file_info(L) ->
    {IntStr, FileName} = lists:splitwith(char_pred($\s), L),
    {erlang:list_to_integer(IntStr), FileName}.

char_pred(Char) -> fun(C) -> C =:= Char end.
    


%%    case file:read_line(Fd) of
%%        {ok, "move" ++ _Data = Move} ->
%%            get_from_file(Fd, [get_move(Move)|Moves], Stacks);
%%
%%        {ok, [Char|_] = StackAny}
%%          when Char == $\s orelse Char == $[ ->
%%            get_from_file(Fd, Moves, get_stacks(StackAny, Stacks));
%%
%%        {ok, _OtherLines} ->
%%            get_from_file(Fd, Moves, Stacks);
%%
%%        eof ->
%%            {ok, {lists:reverse(Moves), Stacks}}
%%    end.

