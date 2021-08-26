-module(proj).
-export([dual/1, make_triangle/3, 
         make_trilateral/3,
         triangle_to_trilateral/1,
         trilateral_to_triangle/1,
         join/2, meet/2,
         make_point/3]).

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
    G = gcd(X, Y, Z),
    #point{x = X div G, 
           y = Y div G, 
           z = Z div G}.
make_line(X, Y, Z) ->
    dual(make_point(X, Y, Z)).
make_triangle(P1, P2, P3) ->
    true = is_record(P1, point),
    true = is_record(P2, point),
    true = is_record(P3, point),
    #triangle{p1 = P1, p2 = P2, p3 = P3}.
make_trilateral(L1, L2, L3) ->
    dual(make_triangle(
           dual(L1), 
           dual(L2), 
           dual(L3))).

%concept of duality from projective geometry
dual(X) when is_record(X, line) ->
    #point{x = X#line.x,
           y = X#line.y,
           z = X#line.z};
dual(X) when is_record(X, point)->
    #line{x = X#point.x,
          y = X#point.y,
          z = X#point.z};
dual(X) when is_record(X, triangle)->
    #trilateral{l1 = dual(X#triangle.p1),
                l2 = dual(X#triangle.p2),
                l3 = dual(X#triangle.p3)};
dual(X) when is_record(X, trilateral)->
    #triangle{p1 = dual(X#trilateral.l1),
              p2 = dual(X#trilateral.l2),
              p3 = dual(X#trilateral.l3)}.

 %operations on lines and points
meet(L1, L2) ->
    %lines meet at a point.
    X = (L1#line.y * L2#line.z) - 
        (L2#line.y * L1#line.z),
    Y = (L1#line.z * L2#line.x) - 
        (L2#line.z * L1#line.x),
    Z = (L1#line.x * L2#line.y) -
        (L2#line.x * L1#line.y),
    %Z = (L2#line.x * L1#line.y) - 
    %    (L1#line.x * L2#line.y),
    make_point(X,Y,Z).
join(P1, P2) ->
    %points can be joined to form a line.
    dual(meet(dual(P1), dual(P2))).
lies_on(A, L) ->
    %rule to know if a point is inside of a line.
    (L#line.x * A#point.x) +
        (L#line.y * A#point.y) -
        (L#line.z * A#point.z).
incident(A, B) when is_record(A, point) ->
    lies_on(A, B);
incident(A, B) when is_record(B, point) ->
    lies_on(B, A).
concurrent(L, M, N) ->%if 3 lines are concurrent, returns true.
    %if the determinant is 0, then the lines are concurrent.
    (L#line.z * ((M#line.x * N#line.y) - 
                     (M#line.y * N#line.x))) +
        (M#line.z * ((N#line.x * L#line.y) - 
                         (N#line.y * L#line.x))) -
        (N#line.z * ((M#line.x * L#line.y) - 
                         (M#line.y * L#line.x))) 
        == 0.
collinear(L, M, N) ->%if 3 points are on the same line, returns true.
    concurrent(dual(L), dual(M), dual(N)).
perpendicular(L1, L2)->
    incident(dual(L1), L2).


%operations on triangles
others(1) -> {2, 3};
others(2) -> {3, 1};
others(3) -> {1, 2}.
triangle_point(1, T) -> T#triangle.p1;
triangle_point(2, T) -> T#triangle.p2;
triangle_point(3, T) -> T#triangle.p3.
triangle_line(N, T) ->
    %one of the lines made by joining 2 points of a triangle.
    {A, B} = others(N),
    join(triangle_point(A, T),
         triangle_point(B, T)).
triangle_to_trilateral(T) ->
    %switch to encoding based on lines instead of points.
    make_trilateral(
      triangle_line(1, T),
      triangle_line(2, T),
      triangle_line(3, T)).
trilateral_to_triangle(T) ->
    %switch to encoding based on points instead of lines.
    dual(triangle_to_trilateral(dual(T))).
%triangle_side(N, T) ->
%    {A, B} = others(N),
%    make_side(triangle_point(A, T),
%              triangle_point(B, T)).
%triangle_vertex(N, T) ->
%    dual(triangle_side(N, T)).

altitude(N, T) ->
    join(triangle_point(N, T), 
         dual(triangle_line(N, T))).
altitude_point(N, T) ->
    dual(altitude(N, T)).
base_point(N, T) -> %base of an altitude
    meet(altitude(N, T),
         triangle_line(N, T)).
orthic_triangle(T) ->
    X = base_point(1, T),
    Y = base_point(2, T),
    Z = base_point(3, T),
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

orthocenter(T) ->
    %altitudes of the triangle are concurrent at a point.
    A1 = altitude(1, T),
    A2 = altitude(2, T),
    meet(A1, A2).
ortholine(T) ->
    dual(orthocenter(T)).
   
dual_triangle_helper(N, T) ->
    %if point N of a triangle is dual the opposite side, return true.
    {A, B} = others(N),
    dual(triangle_point(N, T)) == 
        join(triangle_point(A, T),
             triangle_point(B, T)).
dual_triangle(T) ->
    %if any point is dual the opposite side, returns true.
    dual_triangle_helper(1, T)
        or dual_triangle_helper(2, T)
        or dual_triangle_helper(3, T).
desargues_polar(T, P) ->
    %calculating the polar of a triangle (a line) with respect to the pole P (a point).
    %Cevian lines
    C1 = join(T#triangle.p1, P),
    C2 = join(T#triangle.p2, P),
    C3 = join(T#triangle.p3, P),

    %sides of triangle T
    S1 = triangle_line(1, T),
    S2 = triangle_line(2, T),
    S3 = triangle_line(3, T),
    
    B1 = meet(C1, S1),
    B2 = meet(C2, S2),
    B3 = meet(C3, S3),
    %triangle B is perspective to T via point P, so by desargues theorem, they must be perspective to a line. that line is the "desargues polar"
    join(meet(S3,join(B1, B2)),
         meet(S2,join(B1, B3))).
         
orthic_axis(T) ->
    C = orthocenter(T),
    desargues_polar(T, C).
orthostar(T) ->
    dual(orthic_axis(T)).
ortho_axis(T) ->
    %most important line in hyperbolic triangle geometry.
    C = orthocenter(T),
    join(C, orthostar(T)).
    %ortho_axis passes through the base_center of T.
ortho_axis_point(T) ->
    dual(ortho_axis(T)).


