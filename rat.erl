-module(rat).
%rational numbers library.
-export([make/2, add/2, sub/2, mul/2, 
         divide/2, add/3, mul/3, 
         negative/1, zero/1, to_float/1,
         positive/1]).

-record(rat, {t, b}).

make(T, B) -> #rat{t = T, b = B}.
to_float(Z) -> Z#rat.t / Z#rat.b.
zero(X) -> X#rat.t == 0.
positive(#rat{t = T, b = B}) -> (T*B) > 0.
equal(R1, R2) ->
    (R1#rat.t * R2#rat.b) ==
        (R2#rat.t * R1#rat.b).
%gcf(#rat{t = T, b = B}) -> gcf(T, B).
gcf(X, Y) when (abs(Y) > abs(X)) -> 
    gcf(Y, X);
gcf(X, 0) -> X;
gcf(X, Y) -> gcf(Y, X rem Y).
simplify(#rat{t = T, b = B}) ->
    G = gcf(T, B),
    #rat{t = T div G, b = B div G}.
mul(#rat{t = T1, b = B1}, 
    #rat{t = T2, b = B2}) ->
    simplify(#rat{t = T1*T2, b = B1 * B2}).
mul(N1, N2, N3) -> mul(N1, mul(N2, N3)).
sub(#rat{t = T1, b = B1}, 
    #rat{t = T2, b = B2}) -> 
    simplify(#rat{t = (T1*B2) - (T2*B1), 
                     b = B1*B2}).
add(A, B) -> sub(A, negative(B)).
add(A, B, C) -> add(A, add(B, C)).
divide(A, #rat{t = T, b = B}) ->
    mul(A, #rat{t = B, b = T}).
negative(N = #rat{t = T}) ->
    N#rat{t = -T}.
less_than(#rat{t = T1, b = B1},
          #rat{t = T2, b = B2}) ->
    (T1 * B2) < (T2 * B1).
