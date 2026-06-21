-module(trig).
%planar trigonometry over the rationals.
-export([spread_to_angle/1,
         spread/2, clockwise/3, 
	 point_to_3vector/1,
	 dot/2,
	 det_spread_to_angle/1,
	 maclaurin_asin/1,
	 solid_spread/3, solid_spread/1,
	 det_sqrt/1,
         test/1]).
%-record(line, {x, y, z}).%3 integers
-record(point, {x, y, z}).%3 integers
-record(vector, {x, y}).%2 rationals
-record(vector3, {x, y, z}).%3 rationals
-record(triangle, {x, y, z}).%3 (s)points
-define(rat_pi, {rat, 1146408, 364913}).%312689/99532 https://www.johndcook.com/blog/2018/05/22/best-approximations-for-pi/
-define(bits32, 4294967296). %32
-define(bits64, 18446744073709551616).
-define(bits128, 340282366920938463463374607431768211456).


point_to_3vector(
  #point{x = X, y = Y, z = Z}) ->
    %gnomonic projection
    % https://en.wikipedia.org/wiki/Gnomonic_projection
    #vector3{x = rat:make(X, 1), 
             y = rat:make(Y, 1), 
             z = rat:make(Z, 1)}.
clockwise(P1, P2, P3) ->
    V1 = point_to_vector(P1),
    V2 = point_to_vector(P2),
    V3 = point_to_vector(P3),
    W1 = sub(V3, V2),%vector from 2 to 3
    %W2 = sub(V1, V3),%from 3 to 1
    W3 = sub(V2, V1),%from 1 to 2
    rat:positive(determinate(W1, W3)).

point_to_vector(#point{x = X, y = Y, z = Z}) ->
    #vector{x = rat:make(X, Z),
            y = rat:make(Y, Z)}.

dot(P1 = #point{},
    P2 = #point{}) ->
    dot(point_to_vector(P1),
        point_to_vector(P2));
dot(#vector{x = X1, y = Y1},
    #vector{x = X2, y = Y2}) ->
    rat:add(rat:mul(X1, X2),
            rat:mul(Y1, Y2));
dot(#vector3{x = X1, y = Y1, z = Z1},
    #vector3{x = X2, y = Y2, z = Z2}) ->
    rat:add(
      rat:mul(X1, X2),
      rat:mul(Y1, Y2),
      rat:mul(Z1, Z2)).
cross(P1 = #point{}, P2 = #point{}) ->
    %unused
    cross(point_to_3vector(P1),
          point_to_3vector(P2));
cross(#vector3{x = X1, y = Y1, z = Z1},
      #vector3{x = X2, y = Y2, z = Z2}) ->
    %unused
    #vector3{x = rat:sub(rat:mul(Y1, Z2),
                         rat:mul(Y2, Z1)),
             y = rat:sub(rat:mul(Z1, X2),
                         rat:mul(Z2, X1)),
             z = rat:sub(rat:mul(X1, Y2),
                         rat:mul(X2, Y1))}.
add(#vector{x = X1, y = Y1},
    #vector{x = X2, y = Y2}) ->
    #vector{x = rat:add(X1, X2),
            y = rat:add(Y1, Y2)};
add(#vector3{x = X1, y = Y1, z = Z1},
    #vector3{x = X2, y = Y2, z = Z2}) ->
    #vector3{x = rat:add(X1, X2),
             y = rat:add(Y1, Y2),
             z = rat:add(Z1, Z2)}.
rat_mul(N, V) when is_integer(N) ->
    rat_mul(rat:make(N, 1), V);
rat_mul(N, #vector{x = X, y = Y}) ->
    #vector{x = rat:mul(X, N),
            y = rat:mul(Y, N)};
rat_mul(N, #vector3{x = X, y = Y, z = Z}) ->
    #vector3{x = rat:mul(X, N),
             y = rat:mul(Y, N),
             z = rat:mul(Z, N)}.
negative(#vector{x = X1, y = Y1}) ->
    #vector{x = rat:negative(X1), 
            y = rat:negative(Y1)};
negative(#vector3{x = X1, y = Y1, z = Z1}) ->
    #vector3{x = rat:negative(X1),
             y = rat:negative(Y1),
             z = rat:negative(Z1)}.
sub(V1, V2) ->
    add(V1, negative(V2)).
perpendicular(V1, V2) ->
    %unused
    %v3 = v2 - v1
    %Q1 + Q2 = Q3
    %spread is one.
    rat:zero(dot(V1, V2)).
parallel(V1 = #vector{}, 
         V2 = #vector{}) ->
    %v3 = v2 - v1
    %(Q1 + Q2 + Q3)^2 = 2*(Q1^2 + Q2^2 + Q3^2)
    %spread is zero for rationals.
    %perpendicular(V1, make_perp(V2)).
    rat:zero(determinate(V1, V2));
parallel(#vector3{x = X1, y = Y1, z = Z1},
         #vector3{x = X2, y = Y2, z = Z2}) ->
    %v3 = v2 - v1
    %(Q1 + Q2 + Q3)^2 = 2*(Q1^2 + Q2^2 + Q3^2)
    %spread is zero for rationals.
    X = rat:divide(X1, X2),
    Y = rat:divide(Y1, Y2),
    Z = rat:divide(Z1, Z2),
    (X == Y) and (X == Z).
    %every pair of dimentions is parallel in 2d.
make_perp(#vector{x = X, y = Y}) ->
    %#vector{x = rat:negative(Y), y = X}.
    %90 degree turn right.
    #vector{x = Y, y = rat:negative(X)}.
determinate(V1 = #vector{}, V2 = #vector{}) ->
    dot(V1, make_perp(V2)).
determinate(
  #vector3{x = X1, y = Y1, z = Z1},
  #vector3{x = X2, y = Y2, z = Z2},
  #vector3{x = X3, y = Y3, z = Z3}) ->
    %unused
    rat:sub(
      rat:add(rat:mul(X1, Y2, Z3),
              rat:mul(Y1, Z2, X3),
              rat:mul(Z1, X2, Y3)),
      rat:add(rat:mul(X3, Y2, Z1),
              rat:mul(Y3, Z2, X1),
              rat:mul(Z3, X2, Y1))).
quadrance_to_distance(R) -> 
    %unused
    math:sqrt(rat:to_float(R)).
quadrance(V) -> dot(V, V).
spread(V1 = #point{}, V2 = #point{}) ->
    spread(point_to_3vector(V1),
           point_to_3vector(V2));
spread(V1, V2) ->
%    D = determinate(V1, V2),
%    rat:make(D*D,
%             quadrance(V1) *
%                 quadrance(V2)).
    D = dot(V1, V2),
    rat:sub(
      rat:make(1, 1),
      rat:divide(rat:mul(D, D), 
                 rat:mul(quadrance(V1),
                         quadrance(V2)))).
% 1 - ((d*d)/(q(v1) * q(v2)))

    %   https://www.youtube.com/watch?v=TuaInpbbsg4   23:45
solid_spread(T = #triangle{x = X, y = Y, z = Z}) ->
    solid_spread(X, Y, Z).
solid_spread(V1 = #point{}, V2, V3) ->
    solid_spread(point_to_3vector(V1),
		 point_to_3vector(V2),
		 point_to_3vector(V3));
solid_spread(V1, V2, V3) ->
    D = determinate(V1, V2, V3),
	    rat:divide(rat:mul(D, D),
		       rat:mul(quadrance(V1),
			       quadrance(V2),
			       quadrance(V3))).
spread_to_angle(R) ->
    X = math:sqrt(
          rat:to_float(R)),
    Y = math:asin(X),
    Y2 = math:pi() - Y,
    {Y, Y2}.
-record(rat, {t, b}).
det_spread_to_angle(R) ->

    %if R > 1 or R < 0, fail.
    %if R > 3/4ths, use the cg_asin, otherwise use the maclaurin_asin.

    X = rat:est_simplify(det_sqrt(R), ?bits64),
    %io:fwrite({rat:to_float(X), math:sqrt(rat:to_float(R))}),
    B = rat:less_than({rat, 3, 4}, R),
    Y = if
	    B -> rat:est_simplify(cg_asin(X), ?bits32);
	    true -> rat:est_simplify(maclaurin_asin(X), ?bits64)
	end,

    %works best for nearly perpendicular lines.
    %io:fwrite({"sqrt", math:sqrt(R#rat.t / R#rat.b), rat:to_float(X)}),
    %Y = rat:est_simplify(maclaurin_asin(X), ?bits32),
    %Z = rat:est_simplify(cg_asin(X), ?bits32),
    %io:fwrite({"asin", math:asin(X#rat.t / X#rat.b), rat:to_float(Y), rat:to_float(Z)}),
    %io:fwrite(X, Y),
    %io:fwrite({?rat_pi, Y}),
    Y2 = rat:est_simplify(rat:sub(?rat_pi, Y), ?bits32),
    {rat:to_float(Y), rat:to_float(Y2)}.


cg_asin(X) ->
    %fast branchless asin(x) approximation.
    %Based on Abramowitz and Stegun formula 4.4.45
    % https://developer.download.nvidia.com/cg/asin.html
    % https://personal.math.ubc.ca/~cbm/aands/page_81.htm

    % Original Minimax coefficients from Abramowitz and Stegun
    A0 = {rat, 15707288, 10000000},
    A1 = {rat, -2121144, 10000000},
    A2 = {rat,    74261,  1000000},
    A3 = {rat,   187293, 10000000},
    AbsX = rat:rabs(X),
    Sign = rat:sign(X),

    % Evaluate polynomial using Horner's method

    P1 = rat:add(rat:mul(A3, AbsX), A2),
    P2 = rat:add(rat:mul(P1, AbsX), A1),
    P3 = rat:add(rat:mul(P2, AbsX), A0),

    R = rat:sub(rat:divide(?rat_pi, 2),
		rat:mul(det_sqrt(rat:sub(1, AbsX)), P3)),

    rat:est_simplify(rat:mul(R,Sign), ?bits32).

maclaurin_asin(X) -> %more accurate when x is smaller.

    %sum from n=0 -> infinity of ((2n)! / ((2^(2n))*(n!^2))) * (x^(2n+1) / (2n+1))
    % x + x^3 / 6 + 3 * x^5 / 40 + 5 * x^7 / 112 + ...
    %n=0 : 0!/((2^0) * (0!^2) * (x^1)/1) : x
    %n=1 : 2!/((2^(2)*(1))) * x^3 / 3 : x^3/6
    %n=2 : 4!/((2^4 * 2^2)) * x^5 / 5 : x^5 * 3/40
    %n=3 : 6!/((2^6 * 6^2)) * x^7 / 7 : x^7 * 6*5*4*3*2/(2*2*2*2*2*2*6*6*7) : x^7 * 5/(2*2*2*2*7)
    %n=4 : 8!/((2^8 * 4!^2)) * x^9 / (9) : x^9 * 8*7*6*5/(2^8 * 4!) / 9 : x^9 * 7*3*5/(2^4 * 4! * 9) : x^9 * 7 * 5/(2^7 * 9) : x^9 35/1152
    %n=5 : 10!/((2^10 * 5!^2)) * x^11 / (11) : x^11 * 10*9*8*7*6/(2^10 * 5! * 11) : x^11 * 5*9*7*3/(2^5 * 5! * 11) : x^11 * 9*7/(2^5 * 4*2*11) : x^11 63/2816
    X2 = rat:est_simplify(rat:mul(X, X), ?bits64),
    X3 = rat:est_simplify(rat:mul(X2, X), ?bits64),
    X5 = rat:est_simplify(rat:mul(X3, X2), ?bits64),
    X7 = rat:est_simplify(rat:mul(X5, X2), ?bits64),
    X9 = rat:est_simplify(rat:mul(X7, X2), ?bits64),
    X11 = rat:est_simplify(rat:mul(X9, X2), ?bits64),
    %io:fwrite({rat:to_float(X), rat:to_float(X3), rat:to_float(X5), rat:to_float(X7)}),
    F1 = X,
    F2 = rat:est_simplify(rat:mul(X3, {rat, 1, 6}), ?bits64),
    F3 = rat:est_simplify(rat:mul(X5, {rat, 3, 40}), ?bits64),
    F4 = rat:est_simplify(rat:mul(X7, {rat, 5, 112}), ?bits64),
    F5 = rat:est_simplify(rat:mul(X9, {rat, 35, 1152}), ?bits64),
    F6 = rat:est_simplify(rat:mul(X11, {rat, 63, 2816}), ?bits64),

    A1 = rat:est_simplify(rat:add(F1, F2), ?bits64),
    A2 = rat:est_simplify(rat:add(A1, F3), ?bits64),
    A3 = rat:est_simplify(rat:add(A2, F4), ?bits64),
    A4 = rat:est_simplify(rat:add(A3, F5), ?bits64),
    A5 = rat:est_simplify(rat:add(A4, F6), ?bits64),

    A5.
    
      %rat:mul(X5, {rat, 3, 40})).
det_sqrt(R) ->
    %babylon method
    %x_n+1 = (x_n + R/x_n)/2
    %X0 = 1,
    %Est = {rat, 1, 1},
    Est = R,
    {rat, T, B} = R,
    %io:fwrite({det_log2(T), det_log2(B)}),
    %Est = {rat, det_log2(T), det_log2(B)},
    X1 = rat:est_simplify(rat:divide(rat:add(Est, R), 2), ?bits64),
    %det_sqrt2(X1, R, 27).
    det_sqrt2(X1, R, 35).
det_sqrt2(X, R, 0) -> X;
det_sqrt2(X, R, N) -> 
    X2 = rat:est_simplify(rat:divide(rat:add(X, rat:divide(R, X)), 2), ?bits64),
    det_sqrt2(X2, R, N-1).

det_log2(M) when M < 3 -> 1;
det_log2(N) when is_integer(N) -> 
    1 + det_log2(N div 2).
    
			  
    
cross_law(Q1, Q2, Q3) ->
    %returns spread3
    %unused
    L = rat:square(
          rat:sub(
            rat:add(Q1, Q2),
            Q3)),
    B = rat:mul(
          %rat:scalar_mul(4, Q1),
          rat:mul(rat:make(4, 1), Q1),
          Q2),
    rat:sub(rat:make(1, 1),
            rat:divide(L, B)).

spread_law_s(Q1, S2, Q2) ->
    %returns spread1
    %unused
    rat:divide(rat:mul(Q1, S2), Q2).
spread_law_q(S1, S2, Q2) ->
    %returns quadrance1
    %unused
    rat:divide(rat:mul(Q2, S1), S2).

test(1) ->
    P1 = #point{x = 2, y = 3, z = 4},
    P2 = #point{x = 4, y = 3, z = 1},
    V1 = point_to_vector(P1),
    V2 = point_to_vector(P2),

    %{V1, V2,
    % sub(V1, V2)},
    
    V3 = #vector3{x = rat:make(1, 2),
                  y = rat:make(5, 3),
                  z = rat:make(-8, 1)},
    V4 = rat_mul(rat:make(1, 2), V3),
    V5 = rat_mul(rat:make(-3, 7), V3),

    V6 = #vector3{x = rat:make(2, 1),
                  y = rat:make(1, 1),
                  z = rat:make(-1, 5)},

    {parallel(V3, V4),
     parallel(V3, V5),
     parallel(V4, V5),
     parallel(V6, V3),
     parallel(V6, V4),
     parallel(V6, V5)};
test(2) ->
    R1 = {rat, 1, 10000000000000000000},
    %R1 = {rat, 1, 1},
    {spread_to_angle(R1), 
     1/2000000000,
     det_spread_to_angle(R1)};

%    {rat:to_float(cg_asin(R1)), 
%     rat:to_float(maclaurin_asin(R1)), 
%     math:asin(rat:to_float(R1))}.
test(3) ->
    A = 1000000000,
    B = 1,
    P1 = {point, B+A, A, A},
    P2= {point, A, A+B, A},
    P3 = {point, A, A, A+B},
    T1 = #triangle{x = P1, y = P2, z = P3},
    {math:sqrt(rat:to_float(solid_spread(T1)))/2, spherical_trig:area(T1)}.
    

