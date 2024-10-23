
maze_size1(5, 5).
% CHANGE MAZE SIZE

%FIRST MAZE(5 x 5)
wall(2,2).
wall(3,2).
wall(4,2).
wall(2,3).
wall(2,4).
wall(4,3).

%SECOND MAZE(10 x 10)
%wall(6,2).
%wall(6,3).
%wall(6,4).
%wall(6,5).
%wall(6,6).
%wall(6,7).
%wall(7,4).
%wall(8,4).
%wall(9,4).
%wall(2,7).
%wall(3,7).
%wall(4,7).
%wall(5,7).
%wall(7,7).
%wall(8,7).
%wall(9,7).
%wall(10,7).


% CHECK IS SQUARE IS WITHIN MAZE BOUNDS AND NOT A WALL
valid_position(X, Y) :-
    maze_size1(MaxX, MaxY),
    between(1, MaxX, X),
    between(1, MaxY, Y).

free_position((X, Y)) :-
    valid_position(X, Y),
    \+ wall(X, Y).

adjacent((X1, Y), (X2, Y)) :-
    (X2 is X1 - 1 ; X2 is X1 + 1),
    valid_position(X2, Y).

adjacent((X, Y1), (X, Y2)) :-
    (Y2 is Y1 - 1 ; Y2 is Y1 + 1),
    valid_position(X, Y2).


%DEPTH FIRST SEARCH
dfs_solve(Start, Goal, Path) :-
    depthfirst(Start, Goal, [Start], Path),
    reverse(Path, Solution),
    print_solution(Solution).

depthfirst(Goal, Goal, Path, Path) :-
    !.
depthfirst(Current, Goal, Visited, Path) :-
    adjacent(Current, Next),
    free_position(Next),
    not(member(Next, Visited)),
    depthfirst(Next, Goal,[Next|Visited], Path).

%ITERATIVE DEEPENING SEARCH
ids_solve(Start, Goal, Path) :-
    maze_size1(MaxX, MaxY),
    MaxDepth is MaxX * MaxY,
    between(1, MaxDepth, Depth),
    ids(Start, Goal, Depth, Path).

ids(Start, Goal, Depth, Path) :-
    ids_alg(Start, Goal, [Start], Depth, Path),
    reverse(Path, Solution),
    print_solution(Solution).

ids_alg(_, Goal, Path, _, Path) :-
     member(Goal, Path), !.
ids_alg(_, _, _, 0, _) :- !, fail.
ids_alg(Node, Goal, Path, Depth, Solution) :-
    Depth > 0,
    Depth1 is Depth - 1,
    adjacent(Node, Next),
    free_position(Next),
    not(member(Next, Path)),
    depth_limited_dfs(Next, Goal, [Next|Path], Depth1, Solution).


% MANHATTAN DISTANCE HEURISTIC
manhattan_distance((X1, Y1), (X2, Y2), Distance) :-
    Distance is abs(X1 - X2) + abs(Y1 - Y2).


% DISTANCE CALCULATION FOR A*
path_cost([], 0).
path_cost([_], 0).
path_cost([(X1,Y1),(X2,Y2)|T], Cost) :-
    manhattan_distance((X1,Y1), (X2,Y2), Dist),
    path_cost([(X2,Y2)|T], RemainingCost),
    Cost is Dist + RemainingCost.

% A* SEARCH
solve_astar(Start, Goal, Path) :-
    a_star([[0, Start, []]], Goal, [], Path),
    reverse(Path, Solution),
    print_solution(Solution).

a_star([[_, Node, Path]|_], Goal, _, [Node|Path]) :-
    Node = Goal.
a_star([[_, Node, Path]|RestOpen], Goal, Closed, FinalPath) :-
    findall([NewCost, NewCell, [Node|Path]],
            (adjacent(Node, NewCell),
             free_position(NewCell),
             not(member(NewCell, Closed)),
             not(member([_, NewCell, _], RestOpen)),
             path_cost([NewCell|Path], NewCost)),
            Children),
    append(Children, RestOpen, UpdatedOpen),
    sort(1, @=<, UpdatedOpen, SortedOpen),
    a_star(SortedOpen, Goal, [Node|Closed], FinalPath).


% PRINT SOLOUTION PATH
print_solution([]).
print_solution([(X,Y)|T]) :-
    write('('), write(X), write(','), write(Y), write(') '),
    print_solution(T).
