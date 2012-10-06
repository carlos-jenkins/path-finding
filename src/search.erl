-module(search).
-compile(export_all).

-import(board, []).

%% Wait function
wait(Sec) ->
    receive
    after (1000 * Sec) -> ok
    end.

%% Index of the minimum
min_index([]) -> error;
min_index([_]) -> 1;
min_index([Head|Tail]) -> min_index(Head, Tail, 1, 2).
min_index(Min, [Head|Tail], Index, CIndex) ->
    if
        Head < Min ->
            min_index(Head, Tail, CIndex, CIndex + 1);
        true ->
            min_index(Min, Tail, Index, CIndex + 1)
    end;
min_index(_, [], Index, _) -> Index.

%% Greedy process
greedy_proc(Fringe) ->

    %% Do wait
    wait(1),

    %% Check destination point
    board ! {get_finish, self()},
    receive
        {Fi, Fj} ->
            %% Get current position
            board ! {get_pos, self()},
            receive
                {I, J} when (abs(I - Fi) =< 1) and (abs(J - Fj) =< 1) ->
                    wait(3),
                    greedy_proc(Fringe);
                {_, _} ->
                    %% Jump
                    board ! {get_neighbors, self()},
                    receive
                        {[], _} ->
                            io:format("Problems, got stuck :S~n"),
                            error;
                        {Cells, Heuristics} ->
                            %% Move
                            Min = min_index(Heuristics),
                            Next = lists:nth(Min, Cells),
                            board ! {move, Next},
                            %% Update Fringe
                            NewFringe = Fringe, %% TODO
                            greedy_proc(NewFringe)
                    end
            end
    end.

%% Spawn greedy
greedy() ->
    spawn(search, greedy_proc, [[]]).
