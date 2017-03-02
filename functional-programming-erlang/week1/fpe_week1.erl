-module(fpe_week1).

-export([perimeter/1, area/1, enclose/1, bits/1]).


%%% Data Types

-type point() :: {integer(), integer()}.
-type rect() :: {rect, point(), point()}.
-type circle() :: {circle, point(), integer()}.
-type triangle() :: {triangle, point(), point(), point()}.
-type shape() :: rect() | circle() | triangle().


%%% Module API

-spec perimeter(shape()) -> number().
perimeter({rect, {X1,Y1}, {X2,Y2}}) ->
    Width = X2 - X1,
    Height = Y2 - Y1,
    (Width + Height) * 2;

perimeter({circle, _Center, Radius}) ->
    2 * math:pi() * Radius;

perimeter({triangle, P1, P2, P3}) ->
    distance(P1, P2) + distance(P2, P3) + distance(P3, P1).


-spec area(shape()) -> number().
area({rect, {X1,Y1}, {X2,Y2}}) ->
    Width = X2 - X1,
    Height = Y2 - Y1,
    Width * Height;

area({circle, _Center, Radius}) ->
    math:pi() * math:pow(Radius, 2);

area({triangle, P1, P2, P3}) ->
    S1 = distance(P1, P2),
    S2 = distance(P2, P3),
    S3 = distance(P3, P1),
    HP = (S1 + S2 + S3) / 2,
    math:sqrt(HP * (HP - S1) * (HP - S2) * (HP - S3)).


-spec enclose(shape()) -> rect().
enclose({rect, _, _} = R) -> R;

enclose({circle, {X, Y}, R}) ->
    {rect, {X-R, Y-R}, {X+R, Y+R}};

enclose({triangle, {X1,Y1}, {X2,Y2}, {X3,Y3}}) ->
    {MinX, MaxX} = minAndMax(X1, X2, X3),
    {MinY, MaxY} = minAndMax(Y1, Y2, Y3),
    {rect, {MinX,MinY}, {MaxX,MaxY}}.


-spec bits(integer()) -> integer().
bits(V) ->
    lists:foldl(
      fun
          ($0, Acc) -> Acc;
          ($1, Acc) -> Acc + 1
      end,
      0,
      integer_to_list(V, 2)).


%%% Inner functions

-spec distance(point(), point()) -> number().
distance({X1,Y1}, {X2,Y2}) ->
    C1 = X1 - X2,
    C2 = Y1 - Y2,
    math:sqrt(C1 * C1 + C2 * C2).


-spec minAndMax(integer(), integer(), integer()) -> {integer(), integer()}.
minAndMax(V1, V2, V3) ->
    Min = min(V1, min(V2, V3)),
    Max = max(V1, max(V2, V3)),
    {Min, Max}.
