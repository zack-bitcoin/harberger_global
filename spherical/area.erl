%standalone implementation of calculating area. no libraries. for consideration to add to the blockchain.

-module(area).
-export([area/1,  test/1]).

-define(radius, 6371000). 
-record(triangle, {x, y, z}).
-define(bits32, 4294967296). %32
-define(bits64, 18446744073709551616).
-define(bits128, 340282366920938463463374607431768211456).
-record(rat, {t, b}).
-record(point, {x, y, z}).%3 integers.

area(T = #triangle{}) ->
    {rat, R1, R2} = det_area(T),
    {rat, R1 * ?radius * ?radius, R2}.
    
det_area(T = #triangle{x = X, y = Y, z = Z}) ->
    SS = rat_est_simplify(solid_spread(T), ?bits128),
    SR = rat_est_simplify(det_sqrt(SS), ?bits64), 
    rat_divide(SR, 2).

det_sqrt(R) ->
    det_sqrt2(R, R, 60).
det_sqrt2(X, R, 0) -> X;
det_sqrt2(X, R, N) ->
    {rat, T, B} = rat_divide(rat_sub(R, rat_mul(X, X)), R),
    X2 = rat_est_simplify(rat_divide(rat_add(X, rat_divide(R, X)), 2), ?bits128),
    det_sqrt2(X2, R, N-1).

rat_est_simplify(R0, Max) ->
    R1 = rat_simplify(R0),
    #rat{t = T, b = B} = R1,
    if
	((abs(T) > Max) or (abs(B) > Max)) ->
	    M2 = max(abs(T), abs(B)),
	    Fold = {rat, Max, M2},
	    T2 = divg(T, Fold),
	    B2 = divg(B, Fold),
	    rat_simplify({rat, T2, B2});
	true -> R1
    end.
divg(X, G) ->
    A = X*G#rat.t div G#rat.b,
    if
	(X == 0) -> 0;
	(A == 0) and (X > 0) -> 1;
	(A == 0) and (X < 0) -> -1;
	true -> A
    end.
rat_rabs({rat, X, Y}) ->
    {rat, abs(X), abs(Y)}.

solid_spread(T = #triangle{x = X, y = Y, z = Z}) ->
    D = determinate(X, Y, Z),
    rat_divide(rat_mul(D, D),
	       rat_mul(dot3(X, X),
		       dot3(Y, Y),
		       dot3(Z, Z))).
dot3(#point{x = X1, y = Y1, z = Z1}, 
    #point{x = X2, y = Y2, z = Z2}) ->
    (X1 * X2) + (Y1 * Y2) + (Z1 * Z2).

determinate({point, X1, Y1, Z1},
	    {point, X2, Y2, Z2},
	    {point, X3, Y3, Z3}) ->
    rat_sub(
      rat_add(rat_mul(X1, Y2, Z3),
              rat_mul(Y1, Z2, X3),
              rat_mul(Z1, X2, Y3)),
      rat_add(rat_mul(X3, Y2, Z1),
              rat_mul(Y3, Z2, X1),
              rat_mul(Z3, X2, Y1))).

rat_negative({rat, T, B}) ->
    {rat, -T, B}.
rat_divide({rat, T, B}, I) when is_integer(I)->
    rat_simplify({rat, T, B*I});
rat_divide(R1, {rat, T2, B2}) ->
    rat_mul(R1, {rat, B2, T2}).
rat_mul(A, B, C) ->
    rat_mul(A, rat_mul(B, C)).
rat_mul(A, B) when is_integer(A) ->
    rat_mul(B, {rat, A, 1});
rat_mul({rat, T1, B1}, {rat, T2, B2}) ->
    rat_simplify({rat, T1*T2, B1*B2}).
rat_add(A, B, C) -> rat_add(rat_add(A, B), C).
rat_add(A, B) -> rat_sub(A, rat_negative(B)).
rat_sub({rat, T1, B1}, {rat, T2, B2}) ->
    rat_simplify({rat, ((T1*B2) - (T2*B1)), B1*B2}).
rat_simplify(R = #rat{t = T, b = B}) ->
    %exact.
    G = gcf(T, B),
    case G of
        0 -> R;
        _ ->
            #rat{t = T div G, b = B div G}
    end.
gcf(X, Y) when (abs(Y) > abs(X)) -> 
    gcf(Y, X);
gcf(X, 0) -> X;
gcf(X, Y) -> gcf(Y, X rem Y).

test(1) ->
    B = 10000000,
    S = 1,
    P1 = dproj:make_point(B, B, B+S),
    P2 = dproj:make_point(B, B+S, B),
    P3 = dproj:make_point(B+S, B, B),
    area({triangle, P1, P2, P3}).
