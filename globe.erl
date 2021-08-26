-module(globe).
-export([
         gps_to_rat/2,
         rat_to_gps/1,
         simplify/1,
         distance/2,
         area/3,
         test/0
]).

-define(radius, 6371000). 
-define(max, 4294967295).

-record(point, {x, y, z}).
-record(line, {x, y, z}).

point_to_gps(P) when is_record(P, point) ->
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
gps_to_point(Lat, Long) ->
    Lat2 = (math:pi()/2) - (Lat * math:pi() / 180),
    Lat3 = math:sin(Lat2),
    Q = Lat3*Lat3,
    Long2 = Long * math:pi() / 180,
    Long3 = math:sin(Long2),
    S = Long3*Long3,
    %Q = (X2 + Y2)/(X2 + Y2 + Z2) = (X2 + Y2)/?radius2
    %S = X2/(X2+Y2)
    R2 = ?radius * ?radius,
    X2 = S/Q/(R2),
    Y2 = if
             S == 0.0 -> ?max*X2;
             true -> X2*((1/S)-1)
         end,
    %Y2 = (X2/S) - X2,
    Z2 = ((X2 + Y2) / Q) - X2 - Y2,
    R4 = R2 * R2,
    X3 = X2 * R4,
    Y3 = Y2 * R4,
    Z3 = Z2 * R4,
    X4 = math:sqrt(X3),
    Y4 = math:sqrt(Y3),
    Z4 = math:sqrt(Z3),
    proj:make_point(round(X4), round(Y4), round(Z4)).
simplify(L) when is_record(L, line) ->
    X = P#point.x,
    Y = P#point.y,
    Z = P#point.z,
    if
        ((X > ?max) 
         or (Y > ?max)
         or (Z > ?max)) ->
            X2 = div2(X),
            Y2 = div2(Y),
            Z2 = div2(Z),
            simplify(proj:make_line(X2, Y2, Z2));
        true -> proj:make_line(X, Y, Z)
    end.
div2(X) ->
    R = X rem 2,
    case X of
        1 -> 1;
        -1 -> -1;
        X -> X div 2
    end.
distance(P1, P2) ->
    %angle * radius
    Q = spherical_trig:quadrance(P1, P2),
    rat:to_float(Q) * ?radius.
area(P1, P2, P3) ->
    T = proj:make_triangle(P1, P2, P3),
    A = spherical_trig:area(T),
    A * ?radius * ?radius.

test() ->
    B = 1000000,
    S = 1,
    P1 = proj:make_point(B, B, B),
    P2 = proj:make_point(B, B+S, B),
    P3 = proj:make_point(B+S, B, B),
    {area(P1, P2, P3), distance(P1, P2), 
     distance(P2, P3), distance(P3, P1)}.
