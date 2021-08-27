-module(rat).
%rational numbers library.
-export([make/2, top/1, bottom/1, 
         add/2, sub/2, mul/2, divide/2,
         add/3, mul/3, negative/1,
         scalar_mul/2, gcf/2,
         zero/1, to_float/1,
         positive/1,
         equal/2]).

-record(number, {t, b}).

make(T, B) -> #number{t = T, b = B}.
top(X) -> X#number.t.
bottom(X) -> X#number.b.
to_float(Z) -> Z#number.t / Z#number.b.
zero(X) -> X#number.t == 0.
positive(#number{t = T, b = B}) -> (T*B) > 0.
equal(R1, R2) ->
    (R1#number.t * R2#number.b) ==
        (R2#number.t * R1#number.b).
gcf(#number{t = T, b = B}) -> gcf(T, B).
gcf(X, Y) when (abs(Y) > abs(X)) -> 
    gcf(Y, X);
gcf(X, 0) -> X;
gcf(X, Y) -> gcf(Y, X rem Y).
simplify(#number{t = T, b = B}) ->
    G = gcf(T, B),
    #number{t = T div G, b = B div G}.
square(N) -> mul(N, N).
mul(#number{t = T1, b = B1}, 
    #number{t = T2, b = B2}) ->
    simplify(#number{t = T1*T2, b = B1 * B2}).
mul(N1, N2, N3) -> mul(N1, mul(N2, N3)).
scalar_mul(S, N) -> mul(N, make(S, 1)).
sub(#number{t = T1, b = B1}, 
    #number{t = T2, b = B2}) -> 
    simplify(#number{t = (T1*B2) - (T2*B1), 
                     b = B1*B2}).
add(A, B, C) -> add(A, add(B, C)).
add(A, B) -> sub(A, negative(B)).
divide(A, #number{t = T, b = B}) ->
    mul(A, #number{t = B, b = T}).
negative(N = #number{t = T}) ->
    N#number{t = -T}.
