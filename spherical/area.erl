%standalone implementation of calculating area. no libraries. for consideration to add to the blockchain.

-module(area).
-export([area/1,  test/1]).

-define(radius, 6371000). 
-record(triangle, {x, y, z}).
-define(bits30, 1073741824).
-define(bits32, 4294967296). %32
-define(bits64, 18446744073709551616).
-define(bits128, 340282366920938463463374607431768211456).
-record(rat, {t, b}).
-record(point, {x, y, z}).%3 integers.

area(T = #triangle{}) ->
    {rat, R1, R2} = det_area(T),
    {rat, R1 * ?radius * ?radius, R2}.
    
det_area(T = #triangle{}) ->
    SS = solid_spread(T),
    %SS = {rat, 3939292383828, 2932932932},
    SR = det_sqrt(SS, 2),
    %SR = rat_est_simplify(SS, ?bits64), 
    rat_divide(SR, 2).

det_sqrt(R, Rounds) ->
    {rat, T, B} = R,
    %L2T = log2(T),
    T2 = det_pow(2, log2(T) div 2), 
    B2 = det_pow(2, log2(B) div 2),
    det_sqrt2({rat, T2, B2}, R, Rounds).
det_sqrt2(X, _R, 0) -> X;
det_sqrt2(X, R, N) ->
    D1 = rat_est_simplify(rat_divide(R, X), ?bits64),
    A1 = rat_add(X, D1),
    D2 = rat_divide(A1, 2),
    X2 = rat_est_simplify(D2, ?bits64),
    det_sqrt2(X2, R, N-1).
log2(N) when N < 3 ->
    1;
log2(N) -> 1 + log2(N div 2).
det_pow(0, _) -> 0;
det_pow(_, 0) -> 1;
det_pow(N, 1) -> N;
det_pow(N, M) when (M div 2 == 0)-> 
    X = det_pow(N, M div 2),
    X*X;
det_pow(N, M) -> 
    N * det_pow(N, M-1).
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
%rat_rabs({rat, X, Y}) ->
%    {rat, abs(X), abs(Y)}.

solid_spread(#triangle{x = X, y = Y, z = Z}) ->
    D = determinate(X, Y, Z),
    rat_est_simplify({rat, D*D, dot3(X, X)*dot3(Y,Y)*dot3(Z,Z)}, ?bits128).
dot3(#point{x = X1, y = Y1, z = Z1}, 
    #point{x = X2, y = Y2, z = Z2}) ->
    (X1 * X2) + (Y1 * Y2) + (Z1 * Z2).
determinate({point, X1, Y1, Z1},
	    {point, X2, Y2, Z2},
	    {point, X3, Y3, Z3}) ->
    ((X1 * Y2 * Z3) + (Y1 * Z2 * X3) + (Z1 * X2 * Y3)) - 
	((X3 * Y2 * Z1) + (Y3 * Z2 * X1) + (Z3 * X2 * Y1)).
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
    rat:to_float(area({triangle, P1, P2, P3})); %0.11717184150516298

test(2) ->
    B = 100 * ?radius,
    S = 10000,
    P1 = {point, B, B, B+S},
    P2 = {point, B, B+S, B},
    P3 = {point, B+S, B, B},
    Times = 1000,
    T1 = erlang:timestamp(),
    doit_times(Times, fun() ->
		      area({triangle, P1, P2, P3})
	      end),
    T2 = erlang:timestamp(),
    {verify, timer:now_diff(T2, T1) / Times, "millionths of a second per calculation."}.
    
doit_times(0, _) -> ok;
doit_times(N, F) -> 
    F(),
    doit_times(N-1, F).
