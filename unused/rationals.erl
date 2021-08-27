-module(rationals).
-export([make_line/3, make_point/3, 
         intersection/2, mid_point/2, 
         perpendicular_bisector/2,
         gps_to_rat/2,
         rat_to_gps/1,
         simplify/1, div2/1,
         angle/2, distance/2,
         meets/2, test/0, test2/0, 
         test3/0]).

-define(radius, 6371000). 

%for a triangle on a sphere.
%angle1 + angle2 + angle3 - pi = excess angle.
%excess angle * r^2 is area.

%to calculate angles, consider the angle between the planes. which is the angle between the normal to each plane.


%if a point is on a line, then
%ax + by + zc = 0
-record(line, {a,b,c}).
-record(point, {x, y, z}).

-define(max, 4294967295).

make_line(A, B, C) ->
    #line{a = A, b = B, c = C}.
make_point(A, B, C) ->
    #point{x = A, y = B, z = C}.

rat_to_gps2(P) when is_record(P, point) ->
    %spread = (sin(angle))^2
    X = P#point.x,
    Y = P#point.y,
    Z = P#point.z,
    R1 = (X*X) + (Y*Y),
    R2 = R1 + (Z*Z),
    S = X*X/(R1),
    Q = R1/R2,
    Lat = ((math:pi()/2) - 
               math:asin(math:sqrt(Q)))
        / math:pi() * 180,
    Long = math:asin(math:sqrt(S))
        / math:pi() * 180,
    {Lat, Long}.

rat_to_gps(P) when is_record(P, point) ->
    X = P#point.x,
    Y = P#point.y,
    Z = P#point.z,
    X_over_Y = 
        case Y of
            0 -> ?max;
            _ -> X/Y
        end,
    Long = math:atan(-X_over_Y),
    A = math:sqrt(math:pow(X_over_Y, 2) + 1),
    Den = if
              (Y == 0) -> ?max;
              true -> Z/Y
          end,
    Lat = math:atan(Den/A),
    {Lat2, Long2} = 
        if
            Lat < 0 -> {-Lat, Long + math:pi()};
            true -> {Lat, Long}
        end,
    {Lat3, Long3} = {Lat2 * 180 / math:pi(), 
                     Long2 * 180 / math:pi()},
    Long4 = if
                Long3 > 180 ->
                    Long3 - 360;
                true -> Long3
            end,
    {Lat3, Long4}.

gps_to_rat2(Lat, Long) ->
    Lat2 = (math:pi()/2) - (Lat * math:pi() / 180),
    Lat3 = math:sin(Lat2),
    Q = Lat3*Lat3,
    Long2 = Long * math:pi() / 180,
    Long3 = math:sin(Long2),
    S = Long3*Long3,
    %Q = (X2 + Y2)/(X2 + Y2 + Z2) = (X2 + Y2)/?radius2
    %S = X2/(X2+Y2)
    X2 = S/Q/(?radius * ?radius),
    Y2 = if
             S == 0.0 -> ?max*X2;
             true -> X2*((1/S)-1)
         end,
    %Y2 = (X2/S) - X2,
    Z2 = ((X2 + Y2) / Q) - X2 - Y2,
    X3 = X2 * ?radius * ?radius * ?radius * ?radius,
    Y3 = Y2 * ?radius * ?radius * ?radius * ?radius,
    Z3 = Z2 * ?radius * ?radius * ?radius * ?radius,
    X4 = math:sqrt(X3),
    Y4 = math:sqrt(Y3),
    Z4 = math:sqrt(Z3),
    simplify(#point{x = round(X4), 
                    y = round(Y4), 
                    z = round(Z4)}).
    
    
 

gps_to_rat(Lat0, Long0) when (Lat0 < 0) ->
    gps_to_rat(-Lat0, Long0 + 180);
gps_to_rat(Lat0, Long0) ->
    Long1 = if
                %Long0 > 180 -> Long0 - 180;
                Long0 > 360 -> Long0 - 360;
                true -> Long0
            end,
    Long = Long1 / 180 * math:pi(),
    Lat = Lat0 / 180 * math:pi(),
    %lat is between -90 and 90. 90 is north pole.
    %long is between -180 and 180. 0 is London.
    %the line through the north pole and london is lat=0. includes points {0, 1, 1} and {0, 2, 1}
    %Z = 4294967295,
    Z = ?max * ?max,

    %tan(latitude) = Z/D
    %tan(latitude) = Z/sqrt((X)^2 + (Y)^2)
    %tan(latitude) = Z^2/((X)^2 + (Y)^2)
    %(X)^2 + (Y)^2 = (Z/(tan(latitude)))^2
    %(X/Y)^2 + 1 = (Z/(Y*tan(latitude)))^2 = A^2
    %sqrt((X/Y)^2 + 1) = (Z/(Y*tan(latitude)))
    %Y = Z/(tan(latitude)*sqrt((X/Y)^2 + 1))

    X_over_Y = -math:tan(Long),
    A0 = math:sqrt(math:pow(X_over_Y, 2) + 1),
    Pi = math:pi(),
    A = if
            (Long < 0) -> -A0;
            (Long > Pi) -> -A0;
            true -> A0
        end,
    TanLat = math:tan(Lat),
    Den = A*TanLat,%Z/Y
    Y = if
            TanLat == 0 -> ?max*?max;
            (A == 0) -> ?max;
            true -> Z / Den
        end,
    X = Y * X_over_Y,
    
    simplify(#point{x = round(X), 
                    y = round(Y), 
                    z = round(Z)}).

test() ->
    P1 = make_point(0,10,10),
    P2 = make_point(0,20,10),
    LondonLat = intersection(P1, P2),
    LondonLat = {line, 0,1,0},
    
    P3 = make_point(1, 0, 0),%equator around india
    P4 = make_point(0, 1, 0),%equator south of london
    Equator = intersection(P3, P4),
    Equator = {line, 0, 0, 1},%circle at infinity
    Main = intersection(LondonLat, Equator),

    %{LondonLat, Equator, Main},
    
    {rat_to_gps(gps_to_rat(0,0)),
     rat_to_gps(gps_to_rat(0,25)),
     rat_to_gps(gps_to_rat(25,0)),
     rat_to_gps(gps_to_rat(25,25)),
     rat_to_gps(gps_to_rat(15,30)),
     rat_to_gps(gps_to_rat(15,210)),
     rat_to_gps(gps_to_rat(-15,30)),
     rat_to_gps(gps_to_rat(-15,210)),
     (gps_to_rat(0,0)),
     (gps_to_rat(15,30)),%A
     (gps_to_rat(15,210)),%B
     (gps_to_rat(-15,30)),%B
     (gps_to_rat(-15,210))%A
    }.
    
    %{distance(gps_to_rat(1, 1), 
    %          gps_to_rat(1, 1.0000001)),
    % short_distance(gps_to_rat(1, 1), 
    %                gps_to_rat(1, 1.00000010))
    %}.

test3() ->
    
    F = fun(X) ->
                N = 10,
               D = short_distance(gps_to_rat(N,N),
                            gps_to_rat(N,N+X)),
               D2 = short_distance(gps_to_rat(N,N),
                             gps_to_rat(N+X,N)),
                Area = area(gps_to_rat(N,N),
                         gps_to_rat(N,N+X),
                         gps_to_rat(N+X,N)),
               {D, 
                 Area,
                ((D*D2/2) - Area)/Area,
               X}
       end,
    {
      F(80),
      F(40),
      F(20),
      F(10),
      F(1),
      F(0.1),
      F(0.01),
      F(0.009),
      F(0.008),
      F(0.007),
      F(0.005),
      F(0.001),
      F(0.0001),
      F(0.00001),
      F(0.000001),
      F(0.0000001),
      F(0.00000001),
      F(0.000000001)
    }.

test2() ->
    {
     rat_to_gps(gps_to_rat(-2,1))
      %rat_to_gps2(gps_to_rat(-2,1))
     %(gps_to_rat(15,30)),%A
     %(gps_to_rat(15,210)),%B
     %(gps_to_rat(-15,30)),%B
     %(gps_to_rat(-15,210))%A
    }.
    

     

simplify(P) when is_record(P, line) ->
    dual(simplify(dual(P)));
simplify(P) when is_record(P, point) ->
    X = P#point.x,
    Y = P#point.y,
    Z = P#point.z,
    G = gcf(gcf(X, Y), Z),
    X2 = X div G,
    Y2 = Y div G,
    Z2 = Z div G,
    if
        ((X2 > ?max) 
         or (Y2 > ?max)
         or (Z2 > ?max)) ->
            X3 = div2(X2),
            Y3 = div2(Y2),
            Z3 = div2(Z2),
            simplify(#point{x = X3, y = Y3, z = Z3});
        true -> #point{x = X2, y = Y2, z = Z2}
    end.
div2(X) ->
    R = X rem 2,
    case X of
        1 -> 1;
        -1 -> -1;
        X -> X div 2
    end.
            
       
gcf(X, Y) when (abs(Y) > abs(X)) ->         
    gcf(Y, X);
gcf(X, 0) -> X;
gcf(X, Y) -> 
    Z = X rem Y,
    gcf(Y, Z).

meets(L1, P1) when
      (is_record(L1, line) and
       is_record(P1, point)) ->
    A = L1#line.a,
    B = L1#line.b,
    C = L1#line.c,
    X = P1#point.x,
    Y = P1#point.y,
    Z = P1#point.z,
    ((A*X) + (B*Y) + (C*Z)) == 0;
meets(P1, L1) when
      (is_record(L1, line) and
       is_record(P1, point)) ->
    meets(L1, P1).
is_perpendicular(L1, L2) -> %in blue geometry
    %unused
    (L1#line.a * L1#line.b) +
        (L2#line.a * L2#line.b)
        == 0.
equal(X1, X2)
  when (is_record(X1, point) 
        and is_record(X2, point)) ->
    %unused
    A1 = X1#point.x,
    A2 = X1#point.y,
    A3 = X1#point.z,
    B1 = X2#point.x,
    B2 = X2#point.y,
    B3 = X2#point.z,
    proportion_equal(A1, A2, A3, B1, B2, B3);
equal(X1, X2) when is_record(X1, line) ->
    equal(dual(X1), dual(X2)).
proportion_equal(A1, B1, C1, A2, B2, C2) ->
    %unused
    A3 = (A1 * B2) - (A2 * B1),
    B3 = (B1 * C2) - (C1 * B2),
    C3 = (C1 * A2) - (C2 * A1),
    (A3 == 0) 
        and (B3 == 0) 
        and (C3 == 0).
dual(L) when is_record(L, line) ->
    %pole of this great circle.
    #point{x = L#line.a,
           y = L#line.b,
           z = L#line.c};
dual(P) when is_record(P, point) ->
    %great circle of this point.
    #line{a = P#point.x,
          b = P#point.y,
          c = P#point.z}.
spherical_perpendicular(L, L2) ->
    %for lines and points.
    P = dual(L),
    meets(P, L2).
is_parallel(L1, L2) ->
    %in the projective plane.
    L1#line.a*L2#line.b ==
        L1#line.b * L2#line.a.
intersection(L1, L2) when (is_record(L1, line) and
                           is_record(L2, line))->
    E = equal(L1, L2), 
    P = is_parallel(L1, L2),
    if
        E -> equal;
        P -> 
            %io:fwrite("parallel\n"),
            dual(simplify(L1#line{c = 0}));
        true ->
            A1 = L1#line.a,
            B1 = L1#line.b,
            C1 = L1#line.c,
            A2 = L2#line.a,
            B2 = L2#line.b,
            C2 = L2#line.c,
            P2 = #point{
              x = (B1*C2)-(B2*C1),
              y = (C1*A2)-(C2*A1),
              z = (A1*B2)-(A2*B1)},
            simplify(P2)
    end;
intersection(P1, P2) when (is_record(P1, point) and
                   is_record(P2, point)) ->
    dual(intersection(dual(P1), dual(P2))).
collinear(P1, P2, P3) ->
    %unused
    L = intersection(P1, P2),
    meets(L, P3).
concurrent(L1, L2, L3) ->
    %unused
    P = intersection(L1, L2),
    meets(L3, P).
altitude_to(L, P) ->
    %unused
    A = L#line.a,
    B = L#line.b,
    %C = L#line.c,
    X = P#point.x,
    Y = P#point.y,
    Z = P#point.z,
    L2 = #line{a = -B*Z,
               b = A*Z,
               c = ((B*X)-(A*Y))},
    simplify(L2).
altitude_foot(L, P) ->
    %unused
    A = altitude_to(L, P),
    intersection(A, L).
mid_point(P1, P2) ->
    %unused
    X1 = P1#point.x,
    Y1 = P1#point.y,
    Z1 = P1#point.z,
    X2 = P2#point.x,
    Y2 = P2#point.y,
    Z2 = P2#point.z,
    P3 = #point{x = (X1*Z2) + (X2*Z1),
                y = (Y1*Z2) + (Y2*Z1),
                z = 2*Z1*Z2},
    simplify(P3).
%spherical midpoint
%S = (a+b+c)/2
%sin(a/S) = sqrt(-cos(S)cos(S-A)/(sin(B)sin(C))
%

perpendicular_bisector(P1, P2) ->
    %unused
    L1 = intersection(P1, P2),
    M = mid_point(P1, P2),
    A = altitude_to(L1, M),
    simplify(A).

angle(P1, P2) ->
    %cos = u dot v / ||u|| * ||v||
    X1 = P1#point.x,
    X2 = P2#point.x,
    Y1 = P1#point.y,
    Y2 = P2#point.y,
    Z1 = P1#point.z,
    Z2 = P2#point.z,
    T = (X1 * X2) + (Y1 * Y2) + (Z1 * Z2),
    B1 = (X1 * X1) + (Y1 * Y1) + (Z1 * Z1),
    B2 = (X2 * X2) + (Y2 * Y2) + (Z2 * Z2),
    B3 = math:sqrt(B1 * B2),
    min(math:acos(T/B3),
        math:acos(-T/B3)).

area(P1, P2, P3) ->
    %accounting for the spherical curviture.
    %angle from center of sphere.
    A1 = angle(P2, P3),
    A2 = angle(P3, P1),
    A3 = angle(P1, P2),
    if
        (A1 < 0.0000001) 
        or (A2 < 0.0000001) 
        or (A3 < 0.0000001) ->
            D1 = short_distance(P2, P3),
            D2 = short_distance(P1, P3),
            D3 = short_distance(P1, P2),
            planar_area(D1, D2, D3);
        true ->
    %spherical cosine law to know angle on the surface.
            SA1 = math:acos(min(1, 
                        ((math:cos(A1) - (math:cos(A2)*math:cos(A3)))
                         / (math:sin(A2)*math:sin(A3))))),
    %spherical sine law to find the other 2 angles.
            SA2 = math:asin(min(1, (math:sin(SA1)*math:sin(A2))/math:sin(A1))),
            SA3 = math:asin(min(1, (math:sin(SA1)*math:sin(A3))/math:sin(A1))),

            Excess = SA1 + SA2 + SA3 - math:pi(),
            View = if
                       Excess < 0.0000001 ->
      %for very small triangles the planar algorithm is better.
                           planar_area(A1, A2, A3);
                       true -> Excess
                   end,
            View * ?radius * ?radius%in square meters
    end.

planar_area(A1, A2, A3) ->
    S = (A1 + A2 + A3) / 2,
    math:sqrt(
      S 
      * (S - A1) 
      * (S - A2)
      * (S - A3)).
    
   
short_distance(P1, P2) -> 
     %for very very short distances we can't measure angles from the center of the sphere, so we use the haversine technique to turn gps coordinates into distances.
    {Lat1, Long1} = rat_to_gps(P1),
    {Lat2, Long2} = rat_to_gps(P2),

    T1 = Lat1 * math:pi() / 180,
    T2 = Lat2 * math:pi() / 180,
    DT = (Lat2 - Lat1) * math:pi() / 180,
    DL = (Long2 - Long1) * math:pi() / 180,

    S1 = math:sin(DT/2),
    A = (S1 * S1) + ((math:cos(T1) * math:cos(T2))
        * (math:sin(DL/2) * math:sin(DL/2))),
    C = 2*math:atan(math:sqrt(A)/math:sqrt(1-A)),
    ?radius * C.


distance(P1, P2) ->
    A = angle(P1, P2),
    A * ?radius.
