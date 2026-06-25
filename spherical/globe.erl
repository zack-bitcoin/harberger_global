-module(globe).
%distances, areas, and directions between points on earth.
-export([gps_to_point/1, point_to_gps/1,
         distance/2, distance_point_line/2,
	 line_dist_try/3,
	 area/3, seperation/2,
         gpsify/1, area/1,
         test/0, test2/0, test3/0,
	 estimate_line/2, simplify/1,
	 radius/0,
         test/1]).

-define(radius, 6371000). %in meters
-define(area, 510000000000000).%in square meters
% 4294967296 2^32
%
%-define(max, ).
%-define(max, 4294967295).%2^32 - 1
%-define(max, 16777216).
%-define(max, 67108863).
%-define(max, 268435455).
%-define(max, 134217727).
-define(max, 32767).%2^15 - 1
%-define(max, 8191).
%-define(max, 16383).%2^14 - 1
%-define(max, 2047).
%-define(max, 65535).
%-define(max, 1048575).
%-define(max, 131071).
%-define(max, 63).
-record(point, {x, y, z}).
-record(line, {x, y, z}).
-record(srat, {rat, s}).
-record(rat, {t, b}).
-record(triangle, {x, y, z}).

radius() -> ?radius.
point_to_gps(
  #point{x = X, y = Y, z = Z}) ->
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
    Long1 = Long - 90,
    Long2 = if
		Long1 < -180 ->
		    Long1 + 360;
		true -> Long1
	    end,
    {Lat, Long2}.
gps_to_point({Lat, Long0}) ->
    Long1 = Long0 + 90,
    Long = if
	       Long1 > 360 -> Long1 - 360;
	       true -> Long1
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
    P3 = if
             (Lat < 0) -> 
                 spherical_trig:flip_hemisphere(P);
             true -> P
         end,
    simplify(P3).
    
gpsify(L) -> lists:map(fun point_to_gps/1, L).
simplify(L = #point{}) ->
    dproj:dual(simplify(dproj:dual(L)));
simplify(L0) when is_record(L0, line) ->
    %approximate
    L = dproj:simplify(L0),
    X = L#line.x,
    Y = L#line.y,
    Z = L#line.z,
    if
        ((abs(X) > ?max) 
         or (abs(Y) > ?max)
         or (abs(Z) > ?max)) ->
	    Max = max(max(abs(X), abs(Y)), abs(Z)),
	    Fold = rat:make(?max, Max),%this scales to use all available space.
	    X2 = divg(X, Fold),
	    Y2 = divg(Y, Fold),
	    Z2 = divg(Z, Fold),
            simplify(dproj:make_line(X2, Y2, Z2));
        true -> dproj:make_line(X, Y, Z)
    end.
divg(X, G) ->
    A = X*G#rat.t div G#rat.b,
    if
	(X == 0) -> 0;
	(A == 0) and (X > 0) -> 1;
	(A == 0) and (X < 0) -> -1;
	true -> A
    end.
distance(_P1, {point, 0, 0, 0}) ->
    1000000000000000000000000000;
distance(P1, P2) ->
    #srat{rat = R, s = S} = 
        spherical_trig:quadrance(P1, P2),
    RB = R == {rat, 0, 0},
    if
	RB -> io:fwrite({P1, P2});
	true -> ok
    end,
    {A1, A2} = trig:spread_to_angle(R),
    A3 = if
             S -> A1;
             true -> A2
         end,
    A3 * ?radius.
area(P1, P2, P3) ->
    T = #triangle{x = P1, y = P2, z = P3},
    A = spherical_trig:det_area(T),
    A * ?radius * ?radius.
area(L) ->
    A = spherical_trig:det_area(L),
    A * ?radius * ?radius.

seperation(P1, P2) ->
    Dir = (spherical_trig:direction(P1, P2)),
    Dis = distance(P1, P2),
    Dir0 = if
               Dir > 180 -> Dir - 360;
               true -> Dir
           end,
    {Dir0, Dis}.

specificity(B, Max) ->
     {%B,
      distance({point, 0,0,1},
               {point, B,0,Max}),
     %then the points are this far apart, or nearer.
     distance({point, B, 0, ?max},
              {point, B-1, 0, 
               (?max * (B-1) div B)-1})}.

test() ->
    B = 0,
    C = 10,
    S = 1,
    P1 = dproj:make_point(B, B, B+S),
    P2 = dproj:make_point(B, B+S, B),
    P3 = dproj:make_point(B+S, B, B),
    P4 = dproj:make_point(C, C, C+S),
    P5 = dproj:make_point(C, C+S, C),
    _P6 = dproj:make_point(C+S, C, C),
    {?area / area(P1, P2, P3), %should be approximately 8, since this is 1/8th of the world.
     ?area / area(P3, P4, P5) / 8,
     distance(P1, P2), 
     distance(P2, P3), distance(P3, P1)%,i
   %  seperation(P1, P3)
    }.
test2() ->
    Tokyo = gps_to_point({35.6762, 139.65}),
    Melbourne = gps_to_point({-37.8136, 144.96}),
    _Sydney = gps_to_point({-33.9, 151}),
    _Rio = gps_to_point({-22.9, -43.2}),
    SF = gps_to_point({37.7749, -122.4194}),
    LosAngeles = gps_to_point({34.0522, -118.2437}),
    %LosAngeles = gps_to_point({35.6762, -118.2437}),
    _F = {
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
    _L1 = spherical_trig:join(P1, P2),
    _L2 = spherical_trig:join(P2, P3),
    _L3 = spherical_trig:join(P3, P4),
    _L4 = spherical_trig:join(P4, P5),
    _L5 = spherical_trig:join(P5, P1),
    %L6 = spherical_trig:dual(P1#spoint{s = not(P3#spoint.s)}),
    L6 = dproj:dual(P1),
    _L9 = dproj:dual(P2),
    _L7 = spherical_trig:join(#point{x = 1,y = 0,z = 100}, P2),
    _L7b = spherical_trig:join(P2, #point{x = 1,y = 0,z = 100}),
    io:fwrite("\n"),
    %L2 = slope_sort(L),
    _L8 = spherical_trig:join(#point{x = 1,y = 0,z = 100}, P5),
    _L8b = spherical_trig:join(P5, #point{x = 1,y = 0,z = 100}),
    %io:fwrite(concurrent(L7, L1, L2)),
    %io:fwrite({L7, L7b, slope(L7), slope(L7b), slope(L8), slope(L8b)}),
    _L10 = spherical_trig:flip_hemisphere(L6),
    %L10 = L6#sline{s = not(L6#sline.s)},
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
    Ps = [P2, P3, P4, P5, P1],
    {
      %Ls,
      [P2, P3, P4, P5, P1],
      %area(Ls) / 1000000,
     area(P1, P2, P3)/ 1000000,
     area(P1, P3, P4)/ 1000000,
     area(P1, P4, P5)/ 1000000,
      area(Ps)/1000000,
      area(lists:reverse(Ps))/1000000,
      area([P1, P2, P3, P4, P5, P4, P3, P2])/1000000,
     distance(P1, P2)/1000}.
test(4) -> 
    %testing to see how finely detailed the measurements can be. Units are in meters.
    %seems like there is always a point nearer than ?radius / ?max meters.
    %P1 = {point, 1, 0, ?max},
    %P2 = {point, 0, 0, 1},
    _M2 = ?max div 2,

    %P3 = {point, M2, M2, M2 + 1},
    %P4 = {point, M2 + 1, M2 + 1, M2 + 2},

    %20 kilometers
    BoundConstant = max(1, ?max * 20000 div ?radius),
    %200 kilometers
    BoundConstant2 = max(1, ?max * 200000 div ?radius),
    %2000 kilometers
    BoundConstant3 = max(1, ?max * 2000000 div ?radius),
    %10000 kilometers
    BoundConstant4 = max(1, ?max * 10000000 div ?radius),
    %20000 kilometers
    BoundConstant5 = max(1, ?max * 20000000 div ?radius),

    %these 3 distances, gaps between the pole and it's nearest points, and the prime meridian. They are the worst case. Every other point has neighbors nearer to them than these spots.
    {{polar, 
      distance({point, 1, 0, ?max}, 
               {point, 0,0,1})},
     {equitorial1, 
      distance({point, 1, 0, 1},
               {point, ?max-1, 0, ?max})},
     {equitorial2,
      distance({point, 1,0,1},
               {point, ?max, 1, ?max})},
     %here is the nearest 2 points, for the best possible case.
      {best,
       distance({point, ?max-1, ?max-1, ?max-2},
                {point, ?max-2, ?max-2, ?max-3})},
     {at_different_radius_from_polar,
     specificity(BoundConstant, ?max),
     specificity(BoundConstant2, ?max),
     specificity(BoundConstant3, ?max),
     specificity(BoundConstant4, ?max),
     specificity(BoundConstant5, ?max)
     },
     {radius_over_max, ?radius / ?max}
    };
test(6) ->
    X = (5*?max) - (?max div 7),
    Y = (?max) + (?max div 11),
    Z = (?max) + (?max div 19),
    X2 = X-1,
    Y2 = Y+3,
    Z2 = Z+2,
    _L1 = dproj:make_line(X, Y, Z),
    %L2 = dproj:make_line(X2, Y2, Z2),
    P2 = dproj:make_point(X2, Y2, Z2),
    P1 = dproj:make_point(X, Y, Z),
    {{p1, P1}, {p2, P2},
     {p1_p2_dist, distance(P1, P2)},
     {dumb_method, line_dist_try(P1, P2, simplify(dproj:join(P1, P2)))},
     {method1, estimate_line(P1, P2)},
     ok};
test(7) ->
    Ps = [gps_to_point({10, 10}),
	  %gps_to_point({11, 10}),
	  gps_to_point({12, 11}),
	  %gps_to_point({12, 12}),
	  gps_to_point({11, 13}),
	  %gps_to_point({10, 13}),
	  gps_to_point({9, 12})%,
	  %gps_to_point({9, 11})
	 ],
    %io:fwrite({Ps}),
    Ls = spherical_trig:linify(Ps),
    R = spherical_trig:region(Ls),
    A = spherical_trig:area2(Ps),
    {spherical_trig:too_squished(A, Ps), A, R, Ls, Ps}.

    
estimate_line(P1, P2) ->
    %find the line in the system that most nearly intersects these two points.
    L = dproj:simplify(dproj:join(P1, P2)),%this is the exactly line we want, but it is probably too big to fit in the system.
    X = abs(L#line.x),
    Y = abs(L#line.y),
    Z = abs(L#line.z),
    Max = max(X, max(Y, Z)),
    Min = min(X, min(Y, Z)),
    estimate_line2(P1, P2, L, Min, Max, 0, 0, 0, 0, 1000000000000000000.0).
estimate_line2(_P1, _P2, L, _Min, _Max, T, X, Y, Z, Distance) when T > (?max-1) ->
    {{exact, L}, 
     {estimate, #line{x = X, y = Y, z = Z}}, 
     {distance, Distance}};
estimate_line2(P1, P2, L, Min, Max, T, X, Y, Z, Distance) ->
    #line{x = X0, y = Y0, z = Z0} = L,
    X1 = round(X0 * T / Max),
    Y1 = round(Y0 * T / Max),
    Z1 = round(Z0 * T / Max),
    if
	((T rem 100) == 99) ->
	    %io:fwrite("trial " ++ integer_to_list(T) ++ " X: " ++ integer_to_list(X1) ++ " Y: " ++ integer_to_list(Y1) ++ " Z: " ++ integer_to_list(Z1) ++ " " ++ float_to_list(Distance) ++ "\n");
	    ok;
	true -> ok
    end,
    D1 = line_dist_try(P1, P2, {line, X1, Y1, Z1}),
    Shortest = min(D1, Distance),
    {X2, Y2, Z2} = case Shortest of
		       Distance ->
			   {X, Y, Z};
		       D1 -> {X1, Y1, Z1}
		   end,
    estimate_line2(P1, P2, L, Min, Max, T+1, X2, Y2, Z2, Shortest).
line_dist_try(_P1, _P2, {line, 0,0,0}) ->
    10000000000000000000000000000000000000.0;
line_dist_try(P1, P2, Line) ->
%    Dist = distance_point_line(P1, Line) +
%	distance_point_line(P2, Line),
    Dist = math:sqrt(math:pow(distance_point_line(P1, Line), 2) +
			 math:pow(distance_point_line(P2, Line), 2)),
    if
	%(Dist == 0.0) ->
	%    10000000000000000000000000000000.0;
	(Dist < 0.0) ->
	    io:fwrite({Dist, P1, P2, Line}),
	    throw("negative distance error");
	true -> Dist
    end.
distance_point_line(P, L) ->
    P2 = dproj:dual(L),
    L2 = dproj:join(P, P2),
    P3 = dproj:meet(L, L2),
    distance(P, P3).


