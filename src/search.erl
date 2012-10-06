-module(search).
-compile(export_all).

-import(board, []).

%% Wait function
wait(Sec) ->
    receive
    after (1000 * Sec) -> ok
    end.

%% Index of the minimum
minimum([]) -> error;
minimum([Min]) -> {Min, 1};
minimum([Head|Tail]) -> minimum(Head, Tail, 1, 2).
minimum(Min, [Head|Tail], Index, CIndex) ->
    if
        Head < Min ->
            minimum(Head, Tail, CIndex, CIndex + 1);
        true ->
            minimum(Min, Tail, Index, CIndex + 1)
    end;
minimum(Min, [], Index, _) -> {Min, Index}.

%% Greedy process
greedy_proc({Fi, Fj}, FringeCell, FringeHeu) ->

    %% Do wait
    wait(1),

    %% Check if found
    board ! {get_pos, self()},
    receive
        {I, J} when (abs(I - Fi) =< 1) and (abs(J - Fj) =< 1) ->
            found;
        {_, _} ->
            %% Jump
            board ! {get_neighbors, self()},
            receive
                {[], _} ->
                    io:format("Problems, got stuck :S~n"),
                    %% Move
                    {Val, Min} = minimum(FringeHeu),
                    Next = lists:nth(Min, FringeCell),
                    io:format("Will try to backtrack to ~w~n", [Next]),
                    board ! {move, Next},
                    %% Remove new search point from Fringe
                    NewFringeCell = lists:delete(Next, FringeCell),
                    NewFringeHeu =  lists:delete(Val, FringeHeu),
                    greedy_proc({Fi, Fj}, NewFringeCell, NewFringeHeu);
                {Cells, Heuristics} ->
                    %% Move
                    {Val, Min} = minimum(Heuristics),
                    Next = lists:nth(Min, Cells),
                    board ! {move, Next},
                    %% Add to Fringe
                    NewFringeCell = lists:append(FringeCell,
                                                 lists:delete(Next, Cells)),
                    NewFringeHeu =  lists:append(FringeHeu,
                                                lists:delete(Val, Heuristics)),
                    greedy_proc({Fi, Fj}, NewFringeCell, NewFringeHeu)
            end
    end.

%% Spawn greedy
greedy() ->
    board ! {get_finish, self()},
    receive
        {Fi, Fj} ->
            spawn(search, greedy_proc, [{Fi, Fj}, [], []])
    end.
