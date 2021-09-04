-module(globe).
%distances, areas, and directions between points on earth.
-export([
         gps_to_point/1, point_to_gps/1,
         distance/2, area/3, seperation/2,
         region/1,
         test/0, test2/0, test3/0
]).
-define(radius, 6371000). 
%-define(max, 4294967295).
-define(max, 10000).%useful for testing, so the numbers are small enough to be readable.

-record(spoint, {point, s}).
-record(sline, {line, s}).
-record(point, {x, y, z}).
-record(line, {x, y, z}).
-record(srat, {rat, s}).
-record(triangle, {x, y, z}).
point_to_gps(#spoint{
                point = #point{x = X, 
                               y = Y, z = Z}, 
                s = S}) ->
    A1 = math:sqrt((X*X) + (Y*Y)),
    AN = math:atan(A1/Z),
    Lat = ((math:pi()/2) - AN) * 180 / math:pi(),
    PNc = math:acos(-Y / A1),
    PN = if
             X < 0 -> -PNc;
             true -> PNc
         end,
    %PN = PNc,
    Long = PN * 180 / math:pi(),
    if
        S -> {Lat, Long};
        true -> {-Lat, Long+180}
    end.
gps_to_point({Lat0, Long0}) ->
    %P={x = X, y = Y, z = Z}
    {S, Lat, Long} = 
        if
            Lat0 < 0 -> {false, -Lat0, Long0+180};
            true -> {true, Lat0, Long0}
        end,
    Z = ?max,
    AN = (math:pi()/2) - 
        (Lat * math:pi() / 180),
    %tan(AN) = sqrt(X*X + Y*Y)/Z
    A1 = math:tan(AN)*Z,%sqrt(X*X + Y*Y)

    %radians from prime meridian
    PN = Long * math:pi() / 180,
    %sin(PN) = X / sqrt(X*X + Y*Y)
    %cos(PN) = -Y / sqrt(X*X + Y*Y)
    X_over_A1 = math:sin(PN),
    Y_over_A1 = -math:cos(PN),

    X = A1 * X_over_A1,
    Y = A1 * Y_over_A1,
    P = #point{
      x = round(X), 
      y = round(Y), 
      z = round(Z)},
    #spoint{point = simplify(P), s = S}.
simplify(L = #point{}) ->
    proj:dual(simplify(proj:dual(L)));
simplify(L) when is_record(L, line) ->
    X = L#line.x,
    Y = L#line.y,
    Z = L#line.z,
    if
        ((abs(X) > ?max) 
         or (abs(Y) > ?max)
         or (abs(Z) > ?max)) ->
            X2 = div2(X),
            Y2 = div2(Y),
            Z2 = div2(Z),
            simplify(proj:make_line(X2, Y2, Z2));
        true -> proj:make_line(X, Y, Z)
    end.
div2(X) ->
    case X of
        1 -> 1;
        -1 -> -1;
        X -> X div 2
    end.

distance(P1, P2) ->
    #srat{rat = R, s = S} = 
        spherical_trig:quadrance(P1, P2),
    {A1, A2} = trig:spread_to_angle(R),
    A3 = if
             S -> A1;
             true -> A2
         end,
    A3 * ?radius.
area(P1, P2, P3) ->
    T = #triangle{x = P1, y = P2, z = P3},
    A = spherical_trig:area(T),
    A * ?radius * ?radius.
area([P1, P2, P3]) -> area(P1, P2, P3);
area([H|T]) ->
    area2(H, T).
area2(_, [_]) -> 0;
area2(H, [A, B|R]) -> 
    area(H, A, B) + area2(H, [B|R]).
    
    
seperation(P1, P2) ->
    Dir = (spherical_trig:direction(P1, P2)),
    Dis = distance(P1, P2),
    Dir0 = if
               Dir > 180 ->
                   Dir - 360;
               true -> Dir
           end,
    {Dir0, Dis}.
same_hemispheres([], _) -> [];
same_hemispheres([H|T], P) ->
    B = spherical_trig:same_hemisphere(H, P),
    [{B, H}|same_hemispheres(T, P)].
all_enclosed([]) -> true;
all_enclosed([{true, _}|R]) -> 
    all_enclosed(R);
all_enclosed([{false, _}|_]) -> 
    false.
         
region(L) ->
    SS = slope_sort(L),
    region2(slope_sort(L)).
slope_sort(L) ->
    lists:sort(fun(A, B) ->
                       rat:less_than(
                         slope(B), slope(A))
               end, L).
slope(#sline{line = #line{x = X, y = Y}, 
             s = S}) ->
    One = rat:make(1, 1),
    Slope1 = rat:make(-X, Y),
    B = rat:positive(Slope1),
    Slope2 = 
        %monatonic from rationals -> positive rationals
        if
            B -> rat:add(One, Slope1);
            true -> 
                rat:inverse(
                  rat:sub(One, Slope1))
        end,
    if
        S -> Slope2;
        true -> rat:negative(
                   Slope2)
    end.
region2([A, B, C, D|T]) ->
    region_helper([spherical_trig:meet(B, A)], 
                  [B, C, D|T], A).
region_helper(Ps, [L], A) ->
    [spherical_trig:meet(A, L)] ++ Ps;
region_helper(Ps, [L1, L2|L], A) ->
    region_helper(
      Ps ++ [spherical_trig:meet(L2, L1)],
      [L2|L], A).

test() ->
    B = 1000000,
    S = 1,
    P1 = #spoint{point = proj:make_point(B, B, B),
                 s = true},
    P2 = #spoint{point = proj:make_point(B, B+S, B),
                 s = true},
    P3 = #spoint{point = proj:make_point(B+S, B, B),
                 s = false},
    {area(P1, P2, P3), distance(P1, P2), 
     distance(P2, P3), distance(P3, P1),
     seperation(P1, P3)}.
test2() ->
    Tokyo = gps_to_point({35.6762, 139.65}),
    Melbourne = gps_to_point({-37.8136, 144.96}),
    Sydney = gps_to_point({-33.9, 151}),
    Rio = gps_to_point({-22.9, -43.2}),
    SF = gps_to_point({37.7749, -122.4194}),
    LosAngeles = gps_to_point({34.0522, -118.2437}),
    %LosAngeles = gps_to_point({35.6762, -118.2437}),
    F = {
      (Tokyo),
      (Melbourne),
      %point_to_gps(SF),
      %point_to_gps(LosAngeles),
      seperation(LosAngeles, Tokyo),
      seperation(Tokyo, LosAngeles),
      seperation(LosAngeles, SF),
      seperation(SF, LosAngeles),
      seperation(Tokyo, Melbourne),
      seperation(Melbourne, Tokyo)
      %angles are good, but maybe I am adding the angle the wrong way in the southern hemisphere it is flipped over the y axis.

     }.
    
test3() ->    
%gps {north/south, east/west}
    P1 = gps_to_point({-0.1,0}),
    P2 = gps_to_point({0.1, -0.2}),
    P3 = gps_to_point({0.2, -0.1}),
    P4 = gps_to_point({0.2, 0.1}),
    P5 = gps_to_point({0.1, 0.2}),
    L1 = spherical_trig:join(P1, P2),
    L2 = spherical_trig:join(P2, P3),
    L3 = spherical_trig:join(P3, P4),
    L4 = spherical_trig:join(P4, P5),
    L5 = spherical_trig:join(P5, P1),
    %La = [L1, L2, L3, L4, L5],
    La = [L2, L3, L4, L5, L1],
    Ls = region(La),%TODO. need a way to sort the lines into a cycle of increasing slope.
    Ls = region([L4, L3, L5, L2, L1]),%TODO. need a way to sort the lines into a cycle of increasing slope.
    Ls = [P2, P3, P4, P5, P1],
    {area(Ls) / 1000000,
     area(P1, P2, P3)/ 1000000,
     area(P1, P3, P4)/ 1000000,
     area(P1, P4, P5)/ 1000000,
     distance(P1, P2)/1000}.
    
