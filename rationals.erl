-module(rationals).
-export([make_line/3, make_point/3, 
         intersection/2, mid_point/2, 
         perpendicular_bisector/2,
         gps_to_rat/2,
         rat_to_gps/1,
         simplify/1,
         angle/2, distance/2,
         meets/2, test/0]).

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
    {Lat2 * 180 / math:pi(), 
     Long2 * 180 / math:pi()}.

gps_to_rat(Lat0, Long0) when (Lat0 < 0) ->
    gps_to_rat(-Lat0, Long0 + 180);
gps_to_rat(Lat0, Long0) ->
    Long1 = if
                Long0 > 180 -> Long0 - 180;
                true -> Long0
            end,
    Long = Long1 / 180 * math:pi(),
    Lat = Lat0 / 180 * math:pi(),
    %lat is between -90 and 90. 90 is north pole.
    %long is between -180 and 180. 0 is London.
    %the line through the north pole and london is lat=0. includes points {0, 1, 1} and {0, 2, 1}
    Z = 4294967295,

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
    },
    
    {distance(gps_to_rat(1, 1), 
              gps_to_rat(1, 1.0001))
    },

    {area(gps_to_rat(0.01, 0.01),
          gps_to_rat(0, 0.01),
          gps_to_rat(0.01, 0))}.
    

     

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
    #point{x = L#line.a,
           y = L#line.b,
           z = L#line.c};
dual(P) when is_record(P, point) ->
    #line{a = P#point.x,
          b = P#point.y,
          c = P#point.z}.
is_parallel(L1, L2) ->
    L1#line.a*L2#line.b ==
        L1#line.b * L2#line.a.
intersection(L1, L2) when (is_record(L1, line) and
                           is_record(L2, line))->
    %io:fwrite("intersection\n"),
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

    A1 = angle(P2, P3),
    A2 = angle(P1, P3),
    A3 = angle(P1, P2),
   
    T = math:cos(A1) - (math:cos(A2)*math:cos(A3)),
    B = math:sin(A2)*math:sin(A3),
    
    SA1 = math:acos(T/B),
    
    %math:sin(SA1) / math:sin(A1) = math:sin(SA2) / math:sin(A2)
    SA2 = math:asin((math:sin(SA1)*math:sin(A2))/math:sin(A1)),
    SA3 = math:asin((math:sin(SA1)*math:sin(A3))/math:sin(A1)),

    Excess = SA1 + SA2 + SA3 - math:pi(),
    Excess * ?radius * ?radius.%in square meters
    

distance(P1, P2) ->
    A = angle(P1, P2),
    A * ?radius.
