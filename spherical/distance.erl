%given 2 points on a globe, what is the distance between them?
%self-contained implementation without libraries, for consideration to be put into the blockchain.

-module(distance).
-export([distance/2]).
-define(radius, 6371000). 
-record(srat, {rat, s}).%rat is a rational. s is for whether we are talking about the big or small angle. true indicates the short distance.
-record(rat, {t, b}).
-record(point, {x, y, z}).%3 integers.
-record(line, {x, y, z}).%3 integers.
-define(bits32, 4294967296). %32
-define(bits64, 18446744073709551616).
-define(bits128, 340282366920938463463374607431768211456).
-define(rat_pi, {rat, 1146408, 364913}).% https://www.johndcook.com/blog/2018/05/22/best-approximations-for-pi/


distance(P1, {point, 0, 0, 0}) ->
    1000000000000000000000000000000000000000;
distance(P1, P2) ->
    #srat{rat = R, s = S} = 
        spherical_quadrance(P1, P2),
    RB = (R == {rat, 0, 0}),
    if
	RB -> io:fwrite({P1, P2});
	true -> ok
    end,
    {A1, A2} = det_spread_to_angle(R),
    A3 = if
             S -> A1;
             true -> A2
         end,
    rat_mul(A3, {rat, ?radius, 1}).

spherical_quadrance(P1, P2) ->
    SBig = same_hemisphere(P1, P2),
    Rat = trig_spread(P1, P2),
    #srat{rat = Rat, s = SBig}.
same_hemisphere(P1, P2) ->
    dot3(P1, P2) > 0.
dot3(#point{x = X1, y = Y1, z = Z1}, 
    #point{x = X2, y = Y2, z = Z2}) ->
    (X1 * X2) + (Y1 * Y2) + (Z1 * Z2).
%dot3(P1 = #line{}, P2 = #line{}) ->
%    dot3(dual(P1), dual(P2)).
%dual(#line{x = X, y = Y, z = Z}) ->
%    #point{x = X, y = Y, z = Z};
%dual(#point{x = X, y = Y, z = Z})->
%    #line{x = X, y = Y, z = Z};

trig_spread(P1 = #point{}, P2 = #point{}) ->
    D = dot3(P1, P2),
    rat_sub(
      {rat, 1, 1},
      rat_divide(rat_mul(D, D), 
                 rat_mul(dot3(P1, P1),
                         dot3(P2, P2)))).

rat_sub({rat, T1, B1}, {rat, T2, B2}) ->
    rat_simplify({rat, ((T1*B2) - (T2*B1)), B1*B2}).
rat_add(A, B) -> rat_sub(A, rat_negative(B)).
rat_mul(A, B) when is_integer(A) ->
    rat_mul(B, {rat, A, 1});
rat_mul({rat, T1, B1}, {rat, T2, B2}) ->
    rat_simplify({rat, T1*T2, B1*B2}).
rat_divide(R1, B) when is_integer(B)->
    rat_mul(R1, {rat, 1, B});
rat_divide(R1, {rat, T2, B2}) ->
    rat_mul(R1, {rat, B2, T2}).
rat_negative({rat, T, B}) ->
    {rat, -T, B}.

det_spread_to_angle(R = {rat, T, B}) when (abs(T) < abs(B)) and ((T*B) > 0) ->
    X = det_sqrt(R),
    Bool = rat_less_than({rat, 3, 4}, R),
    Y = if
	    Bool -> rat_est_simplify(cg_asin(X), ?bits32);
	    true -> rat_est_simplify(maclaurin_asin(X), ?bits64)
	end,
    Y2 = rat_est_simplify(rat_sub(?rat_pi, Y), ?bits32),
    {Y, Y2}.
det_sqrt(R) ->
    det_sqrt2(R, R, 60).
det_sqrt2(X, R, 0) -> X;
det_sqrt2(X, R, N) ->
    {rat, T, B} = rat_divide(rat_sub(R, rat_mul(X, X)), R),
    X2 = rat_est_simplify(rat_divide(rat_add(X, rat_divide(R, X)), 2), ?bits128),
    det_sqrt2(X2, R, N-1).
    

rat_less_than(#rat{t = T1, b = B1},
          #rat{t = T2, b = B2}) ->
    (T1 * B2) < (T2 * B1).
    
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
rat_sign({rat, X, Y}) ->
    if
	(X*Y) > 0 -> 1;
	true -> -1
    end.
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
    AbsX = rat_rabs(X),
    Sign = rat_sign(X),

    % Evaluate polynomial using Horner's method

    P1 = rat_add(rat_mul(A3, AbsX), A2),
    P2 = rat_add(rat_mul(P1, AbsX), A1),
    P3 = rat_add(rat_mul(P2, AbsX), A0),

    R = rat_sub(rat_divide(?rat_pi, 2),
		rat_mul(det_sqrt(rat_sub(1, AbsX)), P3)),

    rat_est_simplify(rat_mul(R,Sign), ?bits32).
maclaurin_asin(X) -> %more accurate when x is smaller.

    %sum from n=0 -> infinity of ((2n)! / ((2^(2n))*(n!^2))) * (x^(2n+1) / (2n+1))
    % x + x^3 / 6 + 3 * x^5 / 40 + 5 * x^7 / 112 + ...
    X2 = rat_est_simplify(rat_mul(X, X), ?bits64),
    X3 = rat_est_simplify(rat_mul(X2, X), ?bits64),
    X5 = rat_est_simplify(rat_mul(X3, X2), ?bits64),
    X7 = rat_est_simplify(rat_mul(X5, X2), ?bits64),
    X9 = rat_est_simplify(rat_mul(X7, X2), ?bits64),
    X11 = rat_est_simplify(rat_mul(X9, X2), ?bits64),

    F1 = X,
    F2 = rat_est_simplify(rat_mul(X3, {rat, 1, 6}), ?bits64),
    F3 = rat_est_simplify(rat_mul(X5, {rat, 3, 40}), ?bits64),
    F4 = rat_est_simplify(rat_mul(X7, {rat, 5, 112}), ?bits64),
    F5 = rat_est_simplify(rat_mul(X9, {rat, 35, 1152}), ?bits64),
    F6 = rat_est_simplify(rat_mul(X11, {rat, 63, 2816}), ?bits64),

    A1 = rat_est_simplify(rat_add(F1, F2), ?bits64),
    A2 = rat_est_simplify(rat_add(A1, F3), ?bits64),
    A3 = rat_est_simplify(rat_add(A2, F4), ?bits64),
    A4 = rat_est_simplify(rat_add(A3, F5), ?bits64),
    A5 = rat_est_simplify(rat_add(A4, F6), ?bits64),

    A5.
    
