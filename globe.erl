-module(globe).
%distances, areas, and directions between points on earth.
-export([
         gps_to_point/1, point_to_gps/1,
         distance/2, area/3, seperation/2,
         gpsify/1,
         test/0, test2/0, test3/0
]).
-define(radius, 6371000). 
-define(max, 4294967295).%2^32 - 1
%-define(max, 10000).%useful for testing, so the numbers are small enough to be readable.
-record(spoint, {point, s}).
-record(sline, {line, s}).
-record(point, {x, y, z}).
-record(line, {x, y, z}).
-record(srat, {rat, s}).
-record(triangle, {x, y, z}).
point_to_gps(
  #spoint{point = #point{x = X, y = Y, z = Z}, 
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
gpsify(L) -> lists:map(fun point_to_gps/1, L).
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
    P1 = gps_to_point({0.01,2}),
    P2 = gps_to_point({1, -0.01}),
    P3 = gps_to_point({2, 1}),
    P4 = gps_to_point({2, 3}),
    P5 = gps_to_point({1, 4}),
    L1 = spherical_trig:join(P1, P2),
    L2 = spherical_trig:join(P2, P3),
    L3 = spherical_trig:join(P3, P4),
    L4 = spherical_trig:join(P4, P5),
    L5 = spherical_trig:join(P5, P1),
    %L6 = spherical_trig:dual(P1#spoint{s = not(P3#spoint.s)}),
    L6 = spherical_trig:dual(P1),
    L9 = spherical_trig:dual(P2),
    L7 = spherical_trig:join(#spoint{point = #point{x = 1,y = 0,z = 100}, s = true}, P2),
    L7b = spherical_trig:join(P2, #spoint{point = #point{x = 1,y = 0,z = 100}, s = true}),
    io:fwrite("\n"),
    %L2 = slope_sort(L),
    L8 = spherical_trig:join(#spoint{point = #point{x = 1,y = 0,z = 100}, s = true}, P5),
    L8b = spherical_trig:join(P5, #spoint{point = #point{x = 1,y = 0,z = 100}, s = true}),
    %io:fwrite(concurrent(L7, L1, L2)),
    %io:fwrite({L7, L7b, slope(L7), slope(L7b), slope(L8), slope(L8b)}),
    L10 = L6#sline{s = not(L6#sline.s)},
    %La = [L1, L2, L3, L4, L5],
    %La2 = slope_sort(remove_excess_lines(La2)),
    %Lb2 = slope_sort(La),
    %Lb2 = slope_sort(remove_excess_lines(slope_sort(Lb))),
    %io:fwrite(lists:map(fun(X) -> point_to_gps(X) end, pointify(slope_sort(La2)))),
    %io:fwrite(lists:map(fun(X) -> point_to_gps(X) end, pointify(La))),
    io:fwrite("region 2\n"),
    %io:fwrite({La2}),
    %Ps = region(La),
    %Ps = region(Lb),
    Ps = [P2, P3, P4, P5, P1],
    %Lb2 = slope_sort(Lb),
    %Ls2 = region([L4, L3, L5, L2, L1, L6]),
    %io:fwrite("region 2\n"),
    %Ls = region(La),
    %Ls = region([L4, L3, L5, L2, L1]),
    %Lsb = [P2, P3, P4, P5, P1],
    %io:fwrite({Ls, Lsb}),
    %io:fwrite("region 2\n"),
    %Ls = region(La),
    %io:fwrite("region 3\n"),
    %Ls = region([L4, L3, L5, L2, L1]),
    %Ls = region([L4, L3, L5, L2, L1, L6]),
    {
      %Ls,
      [P2, P3, P4, P5, P1],
      %area(Ls) / 1000000,
     area(P1, P2, P3)/ 1000000,
     area(P1, P3, P4)/ 1000000,
     area(P1, P4, P5)/ 1000000,
     distance(P1, P2)/1000}.
    
