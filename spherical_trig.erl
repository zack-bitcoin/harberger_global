-module(spherical_trig).
-export([test/0, test2/0, quadrance/2, area/1]).

%todo. maybe we should calculate the direction to walk to get from one point to another.

-record(point, {x, y, z}).%3 integers
-record(vector3, {x, y, z}).%3 rationals
-record(triangle, {x, y, z}).%3 points

point_to_3vector(
  #point{x = X, y = Y, z = Z}) ->
    %gnomonic projection
    % https://en.wikipedia.org/wiki/Gnomonic_projection
    #vector3{x = rat:make(X, 1), 
             y = rat:make(Y, 1), 
             z = rat:make(Z, 1)}.
polar(T = #triangle{}) ->
    T2 = proj:triangle_to_trilateral(T),
    proj:dual(T2).
quadrance(P1, P2) ->
    V1 = point_to_3vector(P1),
    V2 = point_to_3vector(P2),
    trig:spread(V1, V2).
quadrances(T = #triangle{x = X, y = Y, z = Z}) ->
    [quadrance(Y, Z),
     quadrance(Z, X),
     quadrance(Y, X)].
spreads(T) -> quadrances(polar(T)).

small_triangle_trio(
  [{A1, A2}, {B1, B2}, {C1, C2}]) ->
    %looking at the pairs of possible angles, it finds the smallest triangle.
    Combinations = 
        [{A1, B1, C1},
         {A1, B1, C2},
         {A1, B2, C1},
         {A1, B2, C2},
         {A2, B1, C1},
         {A2, B1, C2},
         {A2, B2, C1},
         {A2, B2, C2}],
    Combinations2 = 
        lists:filter(fun({A, B, C}) ->
                             (A+B+C) > math:pi()
                     end, Combinations),
    hd(lists:sort(fun({A, B, C}, {A2, B2, C2}) ->
                          (A+B+C) < (A2+B2+C2)
                  end, Combinations2)).
planar_area([{A1, _}, {A2, _}, {A3, _}]) ->
    S = (A1 + A2 + A3) / 2,
    math:sqrt(
      S 
      * (S - A1) 
      * (S - A2)
      * (S - A3)).
area(T = #triangle{x = X, y = Y, z = Z}) ->
    %excess internal angle strategy.
    F = fun(X) -> trig:spread_to_angle(X) end,
    S2 = lists:map(F, spreads(T)),
    Q2 = lists:map(F, quadrances(T)),
    {A1C, A2C, A3C} = 
        small_triangle_trio(S2),
    Area1 = A1C + A2C + A3C - (math:pi()),
    %planar estimation strategy.
    Area2 = planar_area(Q2),
    if
        (Area1 < 0.00001) -> Area2;
        true -> Area1
    end.


test() ->
    P1 = proj:make_point(1, -1, 2),
    P2 = proj:make_point(3, 1, 1),
    P3 = proj:make_point(1, 2, -2),
    T = proj:make_triangle(P1, P2, P3),
    B = polar(T),
    {spreads(T), B}.

test2() -> 
    Ns = [1, 1000, 1000000, 1000000000, 1000000000000],
    B = 100000000,
    F = fun(N) ->
                P1 = proj:make_point(B, B+N, B),
                P2 = proj:make_point(B+N, B, B),
                P3 = proj:make_point(B, B, B),
                area(proj:make_triangle(P1, P2, P3))
        end,
    lists:map(F, Ns).
