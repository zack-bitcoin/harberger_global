-module(hyperbolic).
-export([test/0, join/2]).

%basic data structures for projective hyperbolic geometry
-record(point, {x, y, z}).
-record(line, {x, y, z}).
-record(side, {p1, p2}).%2 points
-record(vertex, {l1, l2}).%2 lines
-record(couple, {p, l}).%point and a line
-record(triangle, {p1, p2, p3}).%3 points
-record(trilateral, {l1, l2, l3}).%3 lines
-record(null_point, {a, b}).
-record(null_line, {a, b}).

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
    #point{x = X div G, y = Y div G, z = Z div G}.
make_line(X, Y, Z) ->
    dual(make_point(X, Y, Z)).
make_side(P1, P2) ->
    true = is_record(P1, point),
    true = is_record(P2, point),
    #side{p1 = P1, p2 = P2}.
make_vertex(L1, L2) ->
    dual(make_side(dual(L1), dual(L2))).
make_couple(P, L) ->
    true = is_record(P, point),
    true = is_record(L, line),
    #couple{p = P, l = L}.
make_triangle(P1, P2, P3) ->
    true = is_record(P1, point),
    true = is_record(P2, point),
    true = is_record(P3, point),
    #triangle{p1 = P1, p2 = P2, p3 = P3}.
make_trilateral(L1, L2, L3) ->
    dual(make_triangle(dual(L1), dual(L2), dual(L3))).
make_null_point(A, B) ->
    #null_point{a = A, b = B}.
make_null_line(A, B) ->
    #null_line{a = A, b = B}.

%concept of duality from projective geometry
dual(X) when is_record(X, line) ->
    #point{x = X#line.x,
           y = X#line.y,
           z = X#line.z};
dual(X) when is_record(X, point)->
    #line{x = X#point.x,
          y = X#point.y,
          z = X#point.z};
dual(X) when is_record(X, side) ->
    #vertex{l1 = dual(X#side.p1),
            l2 = dual(X#side.p2)};
dual(X) when is_record(X, vertex)->
    #side{p1 = dual(X#vertex.l1),
          p2 = dual(X#vertex.l2)};
dual(X) when is_record(X, couple)->
    #couple{p = dual(X#couple.l),
            l = dual(X#couple.p)};
dual(X) when is_record(X, triangle)->
    #trilateral{l1 = dual(X#triangle.p1),
                l2 = dual(X#triangle.p2),
                l3 = dual(X#triangle.p3)};
dual(X) when is_record(X, trilateral)->
    #triangle{p1 = dual(X#trilateral.l1),
              p2 = dual(X#trilateral.l2),
              p3 = dual(X#trilateral.l3)};
dual(X) when is_record(X, null_point)->
    #null_line{a = X#null_point.a, b = X#null_point.b};
dual(X) when is_record(X, null_line) ->
    #null_point{a = X#null_line.a, b = X#null_line.b}.

 %operations on lines and points
meet(L1, L2) ->
    %lines meet at a point.
    X = (L1#line.y * L2#line.z) - (L2#line.y * L1#line.z),
    Y = (L1#line.z * L2#line.x) - (L2#line.z * L1#line.x),
    Z = (L2#line.x * L1#line.y) - (L1#line.x * L2#line.y),
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
dual_couple(C) ->
    (is_record(C, couple) and
     (C == dual(C))).


%operations on triangles
others(1) -> {2, 3};
others(2) -> {1, 3};
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
triangle_side(N, T) ->
    {A, B} = others(N),
    make_side(triangle_point(A, T),
              triangle_point(B, T)).
triangle_vertex(N, T) ->
    dual(triangle_side(N, T)).

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


%operations involving the unit circle
null(X) ->
    %check if a point is on the unit cirle, or if a line is tangent to the unit circle.
    incident(X, dual(X)).
null_point2point(X) ->
    A = X#null_point.a,
    B = X#null_point.b,
    A2 = A*A,
    B2 = B*B,
    make_point(A2-B2, 2*A*B, A2+B2).
join_null_points(N1, N2) when (is_record(N1, null_point) and is_record(N2, null_point)) ->
    make_line((N1#null_point.b*N2#null_point.b) - 
                  (N1#null_point.a*N2#null_point.a),
              (N1#null_point.a*N2#null_point.b) + 
                  (N2#null_point.a*N1#null_point.b),
              (N1#null_point.b*N2#null_point.b) + 
                  (N1#null_point.a*N2#null_point.b)).
meet_null_lines(L1, L2) ->
    dual(join_null_points(dual(L1), dual(L2))).

point_to_matrix(X) when is_record(X, point) ->
    %projective matrix of the point A.
    {{X#point.y, X#point.x + X#point.z},
     {X#point.x - X#point.z, -X#point.y}};
point_to_matrix(X) when is_record(X, null_point)->
    point_to_matrix(null_point2point(X)).
%trace(matrix) = 0. (sum over the diagonal i,i)
%det(matrix) = z*z - x*x - y*y
%so null points have a determinant of 0.
matrix_to_point({{P,Q},{R,P2}}) ->
    P2 = -P,
    make_point((Q+R), P*2, (Q-R)).

mul_mat({{A1, B1}, {C1, D1}}, 
        {{A2, B2}, {C2, D2}}) ->
    {{(A1*A2) + (B1*C2),
      (A1*B2) + (B1*D2)},
     {(C1*A2) + (D1*C2),
      (C1*B2) + (D1*D2)}}.

reflect_matrix(M, A) ->
    %reflecting M over point A.
    mul_mat(mul_mat(A, M), A).

reflect_in(B, A) when is_record(B, null_point)->
    %A is the center of reflection.

    % a type of matrix multiplication
    %[t1 u1] [[y  x+z][x-z -y]]

    X = (B#null_point.a*A#point.y) +
        (B#null_point.b*(A#point.x - A#point.z)),
    Y = B#null_point.a * (A#point.x + A#point.z) - 
        (B#null_point.b * A#point.y),
    make_null_point(X, Y);
reflect_in(B, A) when is_record(B, point) ->            
    B2 = point_to_matrix(B),
    A2 = point_to_matrix(A),
    C = reflect_matrix(B2, A2),
    matrix_to_point(C);
reflect_in(B, A) when is_record(B, line) ->
    dual(reflect_in(dual(B), A)).
   
test() ->
    true = concurrent(make_line(38, -3, 17),
                      make_line(34, -27, 37),
                      make_line(1, 6, -5)),
    ST1 = make_triangle(
            make_point(-3, 3, 5),
            make_point(4, 0, 5),
            make_point(2, -4, 5)),
    {line,49,74,15} = altitude(1, ST1),
    {line,125,-199,100} = altitude(2, ST1),
    {line,223,-51,130} = altitude(3, ST1),
    {point,10385,-3025,19001} = orthocenter(ST1),
    {point,667,-242,985} = base_point(1, ST1),
    {point,653,-2125,5045} = base_point(2, ST1),
    {point,2581,363,4285} = base_point(3, ST1),
    {trilateral,{line,7430,-6945,3887},
      {line,11525,2610,7163},
      {line,3195,-9970,4613}} = triangle_to_trilateral(orthic_triangle(ST1)),
     {line,7290,-415,7889} = orthic_axis(ST1),
     {line,187986,-665765,208735} = ortho_axis(ST1),
     {point,3095,-2610,11112} = base_center(ST1),
    %success.
    H1 = {point,2770131775,-2244686655,13010262679},
    H1 = orthocenter(
           make_triangle(
             triangle_point(1, ST1),
             base_point(2, ST1),
             base_point(3, ST1))),
    H2 = {point,710857435,-38213010,994620217},
    H2 = orthocenter(
           make_triangle(
             triangle_point(2, ST1),
             base_point(3, ST1),
             base_point(1, ST1))),
    H3 = {point,445198925,-623584030,1364495423},
    H3 = orthocenter(
           make_triangle(
             triangle_point(3, ST1),
             base_point(1, ST1),
             base_point(2, ST1))),
    Base = orthocenter(
             make_triangle(H1, H2, H3)),
    Base = base_center(ST1),%confirmation of base triple orthocenter theorem


    %next testing circle stuff
    ST2 = make_triangle(
            make_point(-1, 0, 1),
            make_point(3, 4, 5),
            make_point(-7, -24, 25)),%all 3 points are null points. they are on the unit circle.
    
    A1 = {line,-5,-12,5},
    A1 = altitude(1, ST2),
    A2 = {line,-31,32,7},
    A2 = altitude(2, ST2),
    A3 = {line,-37,-9,19},
    A3 = altitude(3, ST2),
    true = concurrent(A1, A2, A3),
    {point,-61,-30,133} = 
        orthocenter(ST2),
    {line,-61,-30,133} =
        orthic_axis(ST2),
    {triangle,{point,-1,-60,145},
     {point,-149,-96,221},
     {point,-47,18,83}} =
        orthic_triangle(ST2),
    {point,-61,-30,133} = 
        orthostar(ST2),
    {point,-61,-30,133} =
        base_center(ST2),

    reflect_in(make_point(0, 1, 1),make_point(10,5,1)).
    %success.

