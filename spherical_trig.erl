-module(spherical_trig).
%trigonometry on the surface of a sphere.
-export([test/0, test2/0, test3/0,
         test4/0,
         quadrance/2, area/1,
         direction/2,
         quadrances/1,

        angles/1]).
-record(point, {x, y, z}).%3 integers
-record(triangle, {x, y, z}).%3 points

polar(T = #triangle{}) ->
    T2 = proj:triangle_to_trilateral(T),
    proj:dual(T2).
quadrance(P1, P2) ->
    V1 = trig:point_to_3vector(P1),
    V2 = trig:point_to_3vector(P2),
    trig:spread(V1, V2).
quadrances(T = #triangle{x = X, y = Y, z = Z}) ->
    [quadrance(Y, Z),
     quadrance(Z, X),
     quadrance(Y, X)].
spreads(T) -> quadrances(polar(T)).
    
small_triangle_trio(
  [{A1, A2}, {B1, B2}, {C1, C2}]) ->
  %X) ->
    %looking at the pairs of possible angles, it finds the smallest triangle.
    Combinations = 
        [[A1, B1, C1],
         [A1, B1, C2],
         [A1, B2, C1],
         [A1, B2, C2],
         [A2, B1, C1],
         [A2, B1, C2],
         [A2, B2, C1],
         [A2, B2, C2]],
    %io:fwrite({Combinations, Combinations2}),
    Combinations2 = 
        lists:filter(fun([A, B, C]) ->
                             (A+B+C) > math:pi()
                     end, Combinations),
    hd(lists:sort(fun([A, B, C], [A2, B2, C2]) ->
                          (A+B+C) < (A2+B2+C2)
                  end, Combinations2)).
planar_area([{A1, _}, {A2, _}, {A3, _}]) ->
    S = (A1 + A2 + A3) / 2,
    math:sqrt(
      S 
      * (S - A1) 
      * (S - A2)
      * (S - A3)).
area(T = #triangle{}) ->
    %excess internal angle strategy.
    [A1C, A2C, A3C] = angles(T),
    Area1 = A1C + A2C + A3C - (math:pi()),
    %planar estimation strategy.
    F = fun(X) -> trig:spread_to_angle(X) end,
    Q2 = lists:map(F, quadrances(T)),
    Area2 = planar_area(Q2),
    if
        (Area1 < 0.00001) -> Area2;
        true -> Area1
    end.
angles(T = #triangle{}) ->
    small_triangle_trio(
      lists:map(
        fun(X) -> trig:spread_to_angle(X) end,
        spreads(T))).
direction(P1, P2) ->
    North = proj:make_point(0,0,1),
    Collinear = proj:collinear(P1, P2, North),
    if
        Collinear ->
            Q1 = trig:quadrance_to_distance(quadrance(P1, North)),
            Q2 = trig:quadrance_to_distance(quadrance(P2, North)),
            if
                Q2 > Q1 -> 180;
                true -> 0
            end;
        true ->
            T = proj:make_trilateral(
                  proj:dual(P1), 
                  proj:join(P1, North), 
                  proj:join(P1, P2)),
            T2 = proj:trilateral_to_triangle(T),
            %#triangle{x = X, y = Y, z = Z} = T2,
            Clockwise = trig:clockwise(P1, North, P2),
            _Obtuse = rat:less_than(
                       quadrance(P1, North),
                       quadrance(P2, North)),
            Obtuse = false,
            %this is not correctly measuring if the angle is obtuse. TODO
    %if it is not clockwise, we are turning westeward. 
    %if it is clockwise, we are turning eastward.
            %A = element(1, angles(T2)),
            A0 = hd(angles(T2)),%error! not always this angle.
            A = [A0, math:pi() - A0],%second possibility is for if we are in different hemispheres.
            F = fun(Angle) ->
                        B = case Clockwise of
                                false -> ((math:pi() * 2) - Angle);
                                true -> Angle
                            end,
                        B * 180 / math:pi()
                end,
            lists:map(F, A)
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

test3() ->
    P1 = proj:make_point(1000, 1000, 1000), 
    P2a = proj:make_point(-1000, 999, 1000),
    P2o = proj:make_point(-1000, 1001, 1000),
    P3 = proj:make_point(4509,5307,5000),
    P4 = proj:make_point(-6517,3501,5000),
    P5 = proj:make_point(0, -1000, 1000), 
    P6a = proj:make_point(0, -800, 1000),
    P6b = proj:make_point(1, -800, 1000),
    P7 = proj:make_point(0,-1000, 1000),
    P8 = proj:make_point(-1, -1005, 1000),
    {
      direction(P1, P2a),
     direction(P2a, P1),
      direction(P1, P2o),
     direction(P2o, P1),
     % direction(P3, P4),
     % direction(P4, P3),
      direction(P5, P6a),
      direction(P6a, P5),
      direction(P5, P6b),
      direction(P6b, P5),
      direction(P7, P8),
      direction(P8, P7)
    }.
test4() ->
%{globe:gps_to_point({50,0}), globe:gps_to_point({50,10})}.
%{{point,0,-8391,10000},{point,1457,-8264,10000}}
    %spherical_trig:direction({point,0,-8391,10000},{point,1457,-8264,10000}).%93.8? should be like 87.2
    %spherical_trig:direction({point,0,-9,10},{point,1,-8,10}).%93.8? should be like 87.2
    spherical_trig:direction(
      {point,-5446,3459,5000},
      {point,-6517,3501,5000}).%is 43. should be like 140.
    
    
    
