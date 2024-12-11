%  erlc puzzle.erl && erl -noshell -s puzzle main -s init stop
-module(puzzle).
-export([main/0, dijkstra_trail_scores/1, dfs_rating/1]).

main() ->
    {ok, BinaryContent} = file:read_file("input.txt"),
    Lines = string:split(binary_to_list(BinaryContent), "\n", all),
    Grid = parse_input(Lines),

    Scores = dijkstra_trail_scores(Grid),
    ScoreSum = lists:sum(Scores),
    io:format("Part 1: Sum of trailhead scores = ~w~n", [ScoreSum]),

    Ratings = dfs_rating(Grid),
    RatingSum = lists:sum(Ratings),
    io:format("Part 2: Sum of trailhead ratings = ~w~n", [RatingSum]).

parse_input(Lines) ->
    [ [list_to_integer([Char]) || Char <- Line ] || Line <- Lines, Line =/= ""].

dijkstra_trail_scores(Grid) ->
    Trailheads = find_heights(Grid, 0),
    [dijkstra_score(Grid, Pos) || Pos <- Trailheads].

find_heights(Grid, Value) ->
    [ {Row, Col} || Row <- lists:seq(1, length(Grid)),
                    Col <- lists:seq(1, length(hd(Grid))),
                    lists:nth(Col, lists:nth(Row, Grid)) == Value].

dijkstra_score(Grid, StartPos) ->
    TrailEnds = find_heights(Grid, 9),
    dijkstra(Grid, StartPos, TrailEnds, #{}, [{0, StartPos}], 0).

dijkstra(_Grid, _Start, _TrailEnds, _Visited, [], Score) ->
    Score;
dijkstra(Grid, Start, TrailEnds, Visited, [{Distance, {Row, Col}} | RestQueue], Score) ->
    VisitedKey = maps:is_key({Row, Col}, Visited),
    case VisitedKey of
        true ->
            dijkstra(Grid, Start, TrailEnds, Visited, RestQueue, Score);
        false ->
            NewVisited = maps:put({Row, Col}, true, Visited),
            IsTrailEnd = lists:member({Row, Col}, TrailEnds),
            NewScore = if IsTrailEnd -> Score + 1; true -> Score end,
            Neighbors = valid_neighbors(Grid, {Row, Col}),
            ValidNeighbors = [ {Distance + 1, Neighbor} || Neighbor <- Neighbors,
                                not maps:is_key(Neighbor, NewVisited),
                                is_valid_move(Grid, {Row, Col}, Neighbor)],
            dijkstra(Grid, Start, TrailEnds, NewVisited, lists:merge(ValidNeighbors, RestQueue), NewScore)
    end.

dfs_rating(Grid) ->
    Trailheads = find_heights(Grid, 0),
    [dfs_count_paths(Grid, Pos, #{}) || Pos <- Trailheads].

dfs_count_paths(Grid, {Row, Col}, Visited) ->
    VisitedKey = maps:is_key({Row, Col}, Visited),
    case VisitedKey of
        true ->
            0;
        false ->
            Value = lists:nth(Col, lists:nth(Row, Grid)),
            case Value == 9 of
                true ->
                    1;
                false ->
                    NewVisited = maps:put({Row, Col}, true, Visited),
                    Neighbors = valid_neighbors(Grid, {Row, Col}),
                    lists:sum([dfs_count_paths(Grid, Neighbor, NewVisited) || Neighbor <- Neighbors,
                                is_valid_move(Grid, {Row, Col}, Neighbor)])
            end
    end.

valid_neighbors(Grid, {Row, Col}) ->
    Rows = length(Grid),
    Cols = length(hd(Grid)),
    Directions = [{0, 1}, {1, 0}, {0, -1}, {-1, 0}],
    [ {Row + RShift, Col + CShift} || {RShift, CShift} <- Directions,
      Row + RShift >= 1, Col + CShift >= 1, Row + RShift =< Rows, Col + CShift =< Cols ].

is_valid_move(Grid, {Row1, Col1}, {Row2, Col2}) ->
    Val1 = lists:nth(Col1, lists:nth(Row1, Grid)),
    Val2 = lists:nth(Col2, lists:nth(Row2, Grid)),
    Val2 == Val1 + 1.