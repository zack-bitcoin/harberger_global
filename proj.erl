-module(proj).
%projective geometry library.
-export([dual/1, join/2, make_point/3, 
         make_line/3]).

-record(point, {x, y, z}).
-record(line, {x, y, z}).
-record(triangle, {p1, p2, p3}).%3 points
-record(trilateral, {l1, l2, l3}).%3 lines

%greatest common divisor. For the projective notion of points and lines.
gcd(0, 0) -> 0;
gcd(0, A) -> A;
gcd(A, 0) -> A;
gcd(A, B) when (B>A) -> gcd2(B, A);
gcd(A, B) -> gcd2(A, B).
gcd2(A, 0) -> A;
gcd2(A, B) -> gcd2(B, A rem B).
gcd(0, 0, 0) -> 
    io:fwrite("point/line (0,0,0) is undefined"),
    1=2;
gcd(A, B, C) -> 
    G = gcd(gcd(A, B), C),
    if
        ((C*G)<0) -> -G;%third number always positive, so there is only one way to make each point/line
        true -> G
    end.

%rules for generating the basic data structures
make_point(X, Y, Z) ->
    P = #point{x = X, y = Y, z = Z},
    simplify(P).
make_line(X, Y, Z) ->
    dual(make_point(X, Y, Z)).
make_triangle(P1 = #point{}, P2 = #point{}, 
              P3 = #point{}) ->
    #triangle{p1 = P1, p2 = P2, p3 = P3}.
make_trilateral(L1, L2, L3) ->
    dual(make_triangle(
           dual(L1), dual(L2), dual(L3))).
simplify(#point{x = X, y = Y, z = Z}) ->
    G = gcd(X, Y, Z),
    #point{x = X div G, 
           y = Y div G, 
           z = Z div G};
simplify(L = #line{}) ->
    dual(simplify(dual(L))).
    
dual(#line{x = X, y = Y, z = Z}) ->
    #point{x = X, y = Y, z = Z};
dual(#point{x = X, y = Y, z = Z})->
    #line{x = X, y = Y, z = Z};
dual(#triangle{p1 = X, p2 = Y, p3 = Z}) ->
    #trilateral{l1 = dual(X), l2 = dual(Y), 
                l3 = dual(Z)};
dual(#trilateral{l1 = X, l2 = Y, l3 = Z})->
    #triangle{p1 = dual(X), p2 = dual(Y), 
              p3 = dual(Z)}.
 %operations on lines and points
equal(P1, P2) -> simplify(P1) == simplify(P2).
meet(#line{x = X1, y = Y1, z = Z1}, 
     #line{x = X2, y = Y2, z = Z2}) ->
    make_point((Y1 * Z2) - (Y2 * Z1),
               (Z1 * X2) - (Z2 * X1),
               (X1 * Y2) - (X2 * Y1)).
join(P1, P2) ->
    %points can be joined to form a line.
    dual(meet(dual(P1), dual(P2))).
incident(#point{x = A, y = B, z = C},
        #line{x = X, y = Y, z = Z}) ->
    ((A*X) + (B*Y) + (C*Z)) == 0;
incident(L = #line{}, P = #point{}) ->
    incident(P, L).
determinate(#point{x = X1, y = Y1, z = Z1},
            #point{x = X2, y = Y2, z = Z2},
            #point{x = X3, y = Y3, z = Z3}) ->
    ((X1*Y2*Z3) + (Y1*Z2*X3) + (Z1*X2*Y3)) - 
        ((X3*Y2*Z1) + (Y3*Z2*X1) + (Z3*X2*Y1));
determinate(L1 = #line{}, L2 = #line{}, 
            L3 = #line{}) ->
    determinate(dual(L1), dual(L2), dual(L3)).
concurrent(L, M, N) ->%if 3 lines are concurrent, returns true.
    %if the determinant is 0, then the lines are concurrent.
    determinate(L, M, N) == 0.
collinear(L, M, N) ->%if 3 points are on the same line, returns true.
    %concurrent(dual(L), dual(M), dual(N)).
    determinate(L, M, N) == 0.
perpendicular(L1, L2)-> incident(dual(L1), L2).
triangle_to_trilateral(
  #triangle{p1 = X, p2 = Y, p3 = Z}) ->
    %switch to encoding based on lines instead of points.
    #trilateral{l1 = join(Y, Z), 
                l2 = join(Z, X), 
                l3 = join(X, Y)}.
trilateral_to_triangle(T) ->
    %switch to encoding based on points instead of lines.
    dual(triangle_to_trilateral(dual(T))).
altitudes(T = #triangle{p1 = X, p2 = Y, p3 = Z}) ->
    #trilateral{l1 = A, l2 = B, l3 = C} = 
        triangle_to_trilateral(T),
    F = fun(P, L) -> join(P, dual(L)) end,
    [F(X, A), F(Y, B), F(Z, C)].
altitude_points(T) ->
    lists:map(dual, altitudes(T)).
base_points(
  T = #triangle{p1 = X, p2 = Y, p3 = Z})->
    [A1, A2, A3] = altitudes(T),
    [meet(A1, X), meet(A2, Y), meet(A3, Z)].
orthic_triangle(T = #triangle{}) ->
    [X, Y, Z] = base_points(T),
    make_triangle(X, Y, Z).
dual_orthic_triangle(T) ->
    X = orthic_triangle(T),
    trilateral_to_triangle(dual(X)).
base_center(T) ->
    %base center lies on the ortho_axis
    C = dual_orthic_triangle(T),
    %C and T are perspective to the base center
    A = join(T#triangle.p1, C#triangle.p1),
    B = join(T#triangle.p2, C#triangle.p2),
    meet(A, B).

orthocenter(T = #triangle{}) ->
    %altitudes of the triangle are concurrent at a point.
    [A1, A2, _] = altitudes(T),
    meet(A1, A2).
ortholine(T) ->
    dual(orthocenter(T)).
dual_triangle(
  T = #triangle{p1 = X, p2 = Y, p3 = Z}) ->
    #trilateral{l1 = A, l2 = B, l3 = C} =
        triangle_to_trilateral(T),
    equal(dual(X), A) or
        equal(dual(Y), B) or
        equal(dual(Z), C).
desargues_polar(T, P) ->
    %calculating the polar of a triangle (a line) with respect to the pole P (a point).
    %Cevian lines
    C1 = join(T#triangle.p1, P),
    C2 = join(T#triangle.p2, P),
    C3 = join(T#triangle.p3, P),

    %sides of triangle T
    #trilateral{l1 = S1, l2 = S2, l3 = S3} = 
        triangle_to_trilateral(T),
    B1 = meet(C1, S1),
    B2 = meet(C2, S2),
    B3 = meet(C3, S3),
    %triangle B is perspective to T via point P, so by desargues theorem, they must be perspective to a line. that line is the "desargues polar"
    join(meet(S3,join(B1, B2)),
         meet(S2,join(B1, B3))).
         
orthic_axis(T) ->
    desargues_polar(T, orthocenter(T)).
orthostar(T) -> dual(orthic_axis(T)).
ortho_axis(T) ->
    %most important line in hyperbolic triangle geometry.
    join(orthocenter(T), orthostar(T)).
    %ortho_axis passes through the base_center of T.
ortho_axis_point(T) ->
    dual(ortho_axis(T)).
