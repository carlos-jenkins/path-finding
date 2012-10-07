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
        {I, J} when (abs(I - Fi) == 0) and (abs(J - Fj) == 0) ->
            found;
        {_, _} ->
            %% Jump
            board ! {get_neighbors, self()},
            receive
                {[], _} ->
                    if
                        FringeCell == [] ->
                            io:format("Unable to find the path :(~n"),
                            fail;
                        true ->
                            io:format("Problems, got stuck :S~n"),
                            %% Move
                            {Val, Min} = minimum(FringeHeu),
                            Next = lists:nth(Min, FringeCell),
                            io:format("Will try to backtrack to ~w~n", [Next]),
                            board ! {move, Next},
                            %% Remove new search point from Fringe
                            NewFringeCell = lists:delete(Next, FringeCell),
                            NewFringeHeu =  lists:delete(Val, FringeHeu),
                            greedy_proc({Fi, Fj}, NewFringeCell, NewFringeHeu)
                    end;
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

%% A* greedy
astar() ->
    board ! {get_start_finish, self()},
    receive
        {{Si,Sj},{Fi, Fj}} ->
            spawn(search, a_star_proc, [{Si,Sj},{Fi, Fj}, [], []])
    end.


%% A* process
a_star_proc({Si,Sj},{Fi, Fj}, FringeCell, FringeHeu) ->

    %% Do wait
    wait(1),
    
    %% Check if found
    board ! {get_pos, self()},
    receive
        {I, J} when (abs(I - Fi) == 0) and (abs(J - Fj) == 0) ->
            found;
        {_, _} ->
            %% Jump
            board ! {get_neighbors, self()},
            receive
                {[], _} ->
                    if
                        FringeCell == [] ->
                            io:format("Unable to find the path :(~n"),
                            fail;
                        true ->
                            io:format("Problems, got stuck :S~n"),
                            %% Move
                            {Val, Min} = minimum(FringeHeu),
                            Next = lists:nth(Min, FringeCell),
                            io:format("Will try to backtrack to ~w~n", [Next]),
                            board ! {move, Next},
                            %% Remove new search point from Fringe
                            NewFringeCell = lists:delete(Next, FringeCell),
                            NewFringeHeu =  lists:delete(Val, FringeHeu),
                            a_star_proc({Si,Sj},{Fi, Fj}, NewFringeCell, NewFringeHeu)
                    end;
                {Cells, Heuristics} ->
                    A_Star_Heuristics = a_star(Heuristics,Cells,{Si,Sj}),
                    %% Add to Fringe
                    NewFringeCell = lists:append(FringeCell, Cells),
                    NewFringeHeu =  lists:append(FringeHeu, A_Star_Heuristics),
                    %% Move
                    {Val, Min} = minimum(NewFringeHeu),
                    Next = lists:nth(Min, NewFringeCell),
                    board ! {move, Next},
                    %%io:format("~p~n", [A_Star_Heuristics]),
                    a_star_proc({Si,Sj},{Fi, Fj}, lists:delete(Next, NewFringeCell), lists:delete(Val, NewFringeHeu))
            end
    end.
    
%% A* calc for distance traveled plus Heuristic.
a_star([F|R],[{Pi,Pj}|RC],{Si,Sj}) ->
	[(F+math:sqrt((Pi-Si)*(Pi-Si)+(Pj-Sj)*(Pj-Sj)))|a_star(R,RC,{Si,Sj})];
a_star([],_,_) -> [].
    
