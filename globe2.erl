-module(globe2).
%distances, areas, and directions between points on earth.
-export([
         gps_to_point/1, point_to_gps/1,
         distance/2, area/3, seperation/2,
         test/0, test2/0
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
point_to_gps(P = #spoint{
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
    #spoint{point = globe:simplify(P), s = S}.

distance(P1, P2) ->
    #srat{rat = R, s = S} = 
        spherical_trig2:quadrance(P1, P2),
    {A1, A2} = trig:spread_to_angle(R),
    A3 = if
             S -> A1;
             true -> A2
         end,
    A3 * ?radius.
area(P1, P2, P3) ->
    %T = proj:make_triangle(P1, P2, P3),
    T = #triangle{x = P1, y = P2, z = P3},
    A = spherical_trig2:area(T),
    A * ?radius * ?radius.
seperation(P1, P2) ->
    Dir = (spherical_trig2:direction(P1, P2)),
    Dis = distance(P1, P2),
    %Dis2 = (math:pi() * ?radius) - Dis,
    %Dir2 = Dir - 180,
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
      seperation(Tokyo, Melbourne),%bad
      seperation(Melbourne, Tokyo)%bad
      %angles are good, but maybe I am adding the angle the wrong way in the southern hemisphere. it is flipped over the y axis.
      % TODO! it is because spherical_trig:clockwise is reversed for each element in the southern hemisphere.
     }.
    
    
                 
    
