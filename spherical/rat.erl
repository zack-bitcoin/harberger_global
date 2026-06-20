-module(rat).
%rational numbers library.
-export([make/2, add/2, sub/2, mul/2, 
         divide/2, add/3, mul/3, 
         inverse/1, less_than/2,
         negative/1, zero/1, to_float/1,
	 est_simplify/2, rabs/1, sign/1,
         positive/1]).

-record(rat, {t, b}).

make(T, B) -> #rat{t = T, b = B}.
to_float(Z) -> Z#rat.t / Z#rat.b.
zero(X) -> X#rat.t == 0.
positive(#rat{t = T, b = B}) -> (T*B) > 0.
equal(R1, R2) ->
    %unused
    (R1#rat.t * R2#rat.b) ==
        (R2#rat.t * R1#rat.b).
gcf(X, Y) when (abs(Y) > abs(X)) -> 
    gcf(Y, X);
gcf(X, 0) -> X;
gcf(X, Y) -> gcf(Y, X rem Y).
est_simplify(R0, Max) ->
    R1 = simplify(R0),
    #rat{t = T, b = B} = R1,
    if
	((abs(T) > Max) or (abs(B) > Max)) ->
	    M2 = max(abs(T), abs(B)),
	    Fold = make(Max, M2),
	    T2 = divg(T, Fold),
	    B2 = divg(B, Fold),
	    simplify(make(T2, B2));
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

rabs({rat, X, Y}) ->
    {rat, abs(X), abs(Y)}.

sign({rat, X, Y}) ->
    if
	(X*Y) > 0 -> 1;
	true -> -1
    end.
	    
    
simplify(R = #rat{t = T, b = B}) ->
    %exact.
    G = gcf(T, B),
    case G of
        0 -> R;
        _ ->
            #rat{t = T div G, b = B div G}
    end.
mul(A, B) when is_integer(A) and is_integer(B) -> A*B;
mul(#rat{t = T, b = B}, A) when is_integer(A) ->
    make(T*A, B);
mul(A, #rat{t = T, b = B}) when is_integer(A) ->
    make(T*A, B);
mul(#rat{t = T1, b = B1}, 
    #rat{t = T2, b = B2}) ->
    simplify(make(T1*T2, B1*B2)).
mul(N1, N2, N3) -> mul(N1, mul(N2, N3)).
sub(A, R) when is_integer(A)-> 
    sub(make(A, 1), R);
sub(R = #rat{}, A) when is_integer(A)-> 
    sub(R, make(A, 1));
sub(#rat{t = T1, b = B1}, 
    #rat{t = T2, b = B2}) -> 
    simplify(make((T1*B2) - (T2*B1), B1*B2)).
add(A, B) -> sub(A, negative(B)).
add(A, B, C) -> add(A, add(B, C)).
divide(A, B) -> mul(A, inverse(B)).
inverse(A) when is_integer(A) ->
    make(1, A);
inverse(#rat{t = T, b = B}) ->
    make(B, T).
negative(N) when is_integer(N) ->
    make(-N, 1);
negative(N = #rat{t = T}) ->
    N#rat{t = -T}.
less_than(#rat{t = T1, b = B1},
          #rat{t = T2, b = B2}) ->
    (T1 * B2) < (T2 * B1).
