-module(rationals).
-export([make_line/3, make_point/3, 
         intersection/2, mid_point/2, 
         perpendicular_bisector/2,
         gps_to_rat/2,
         rat_to_gps/2,
         meets/2, test/0]).

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
    Long = math:atan(Y/X),
    A = math:sqrt(math:pow(X/Y, 2) + 1),
    Den = Z / Y,
    Lat = math:atan(Den/A),
    {Lat, Long}.

    

gps_to_rat(Lat, Long) ->
    %Lat = (math:pi() / 2) - Lat0,
    %lat is between -90 and 90. 90 is north pole.
    %long is between -180 and 180. 0 is London.
    %the line through the north pole and london is lat=0. includes points {0, 1, 1} and {0, 2, 1}
    Z = 4294967295 * 2,

    %(X/Z)^2 + (Y/Z)^2 = (1/(tan(latitude)))^2
    %(X)^2 + (Y)^2 = (Z/(tan(latitude)))^2
    %(X/Y)^2 + 1 = (Z/(Y*tan(latitude)))^2
    %sqrt((X/Y)^2 + 1) = (Z/(Y*tan(latitude)))
    %Y = Z/(tan(latitude)*sqrt((X/Y)^2 + 1))

    Y_over_X = math:tan(Long),
    X_over_Y = 
        case Y_over_X of
            0.0 -> ?max;
            _ ->
                1/Y_over_X
        end,
    A = math:sqrt(math:pow(X_over_Y, 2) + 1),
    Den = A*math:tan(Lat),
    Y = if
            (Den == 0) -> ?max;
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

    {LondonLat, Equator, Main},
    
    {rat_to_gps(gps_to_rat(0,0)),
     rat_to_gps(gps_to_rat(math:pi()/4,math:pi()/4))
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
            X3 = max(1, X2 div 2),
            Y3 = max(1, Y2 div 2),
            Z3 = max(1, Z2 div 2),
            simplify(#point{x = X3, y = Y3, z = Z3});
        true -> #point{x = X2, y = Y2, z = Z2}
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
%central_line(L1) ->
%    L1#line.c == 0.
is_parallel(L1, L2) ->
    L1#line.a*L2#line.b ==
        L1#line.b * L2#line.a.
is_perpendicular(L1, L2) -> %in blue geometry
    (L1#line.a * L1#line.b) +
        (L2#line.a * L2#line.b)
        == 0.
equal(X1, X2) 
  when (is_record(X1, point) 
        and is_record(X2, point)) ->
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
    L = intersection(P1, P2),
    meets(L, P3).
concurrent(L1, L2, L3) ->
    P = intersection(L1, L2),
    meets(L3, P).
parallel_to(L, P) ->
    A = L#line.a,
    B = L#line.b,
    %C = L#line.c,
    X = P#point.x,
    Y = P#point.y,
    Z = P#point.z,
    L2 = #line{a = A*Z,
               b = B*Z,
               c = -((A*X) + (B*Y))},
    simplify(L2).
altitude_to(L, P) ->
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
    A = altitude_to(L, P),
    intersection(A, L).
mid_point(P1, P2) ->
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
    L1 = intersection(P1, P2),
    M = mid_point(P1, P2),
    A = altitude_to(L1, M),
    simplify(A).
