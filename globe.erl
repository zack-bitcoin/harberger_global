-module(globe).
%distances, areas, and directions between points on earth.
-export([
         gps_to_point/1, point_to_gps/1,
         simplify/1,
         distance/2, area/3, seperation/2,
         test/0, test2/0
]).

-define(radius, 6371000). 
%-define(max, 4294967295).
-define(max, 10000).%useful for testing, so the numbers are small enough to be readable.

-record(point, {x, y, z}).
-record(line, {x, y, z}).

point_to_gps(P) when is_record(P, point) ->
    X = P#point.x,
    Y = P#point.y,
    Z = P#point.z,
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
    {Lat, Long}.
    
gps_to_point({Lat0, Long0}) when (Lat0 < 0) ->
    %lat is north-south. -90 to 90.
    %long is east west. -180 to 180
    gps_to_point({-Lat0, Long0 + 180});
gps_to_point({Lat, Long}) ->
    %radians from north pole.
    %P={x = X, y = Y, z = Z}
    %Z = ?radius * ?radius * ?radius,
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
    simplify(proj:make_point(
               round(X), round(Y), round(Z))).

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
    %angle * radius
    Q = spherical_trig:quadrance(P1, P2),
    {A, _} = trig:spread_to_angle(Q),
    A * ?radius.
area(P1, P2, P3) ->
    T = proj:make_triangle(P1, P2, P3),
    A = spherical_trig:area(T),
    A * ?radius * ?radius.
seperation(P1, P2) ->
    Dir = (spherical_trig:direction(P1, P2)),
    Dis = distance(P1, P2),
    Dis2 = (math:pi() * ?radius) - Dis,
    %Dir2 = Dir - 180,
    %Dir0 = if
    %           Dir > 180 ->
    %               Dir - 360;
    %           true -> Dir
    %       end,
    {{Dir, Dis}, {Dir, Dis2}}.
test() ->
    B = 1000000,
    S = 1,
    P1 = proj:make_point(B, B, B),
    P2 = proj:make_point(B, B+S, B),
    P3 = proj:make_point(B+S, B, B),
    {area(P1, P2, P3), distance(P1, P2), 
     distance(P2, P3), distance(P3, P1),
     seperation(P1, P3)}.

test2() ->
    Tokyo = gps_to_point({35.6762, 139.65}),
    Melbourne = gps_to_point({-37.8136, 144.96}),
    SF = gps_to_point({37.7749, -122.4194}),
    LosAngeles = gps_to_point({34.0522, -118.2437}),

    Greenwich = gps_to_point({51.5, 0}),
    Toronto = gps_to_point({43.7, -79.4}),
    Sydney = gps_to_point({-33.8, 151.2}),
    NewDelhi = gps_to_point({28.6, 77}),

    %seperation(Tokyo, LosAngeles).
    F = {
     % Tokyo,
     point_to_gps(Tokyo),
     point_to_gps(Melbourne),
     %SF,
     point_to_gps(SF),
     point_to_gps(LosAngeles),
     point_to_gps(Greenwich),
     point_to_gps(Toronto),
     point_to_gps(Sydney),
     point_to_gps(NewDelhi)
     %point_to_gps(gps_to_point({1,1}))
    },
    
    F2 = {
      %LosAngeles,
      SF, Toronto,
      seperation(LosAngeles, SF),
    seperation(SF, LosAngeles),
    seperation(SF, Toronto),
    seperation(Toronto, SF),
    seperation(LosAngeles, Toronto),
    seperation(Toronto, LosAngeles),
    seperation(SF, Tokyo),
    seperation(Tokyo, SF),
    seperation(Tokyo, Melbourne),
      seperation(Melbourne, Tokyo)
     },
    F3 = {
    %  Tokyo, LosAngeles,
    %seperation(Tokyo, LosAngeles),
    %seperation(LosAngeles, Tokyo),
    %  seperation(Tokyo, Melbourne),
    %  seperation(Melbourne, Tokyo),%bad
      seperation(LosAngeles, SF),
    seperation(SF, LosAngeles)%bad
     },
    F3.
     

