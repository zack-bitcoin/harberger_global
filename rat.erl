-module(rat).
-export([make/2, top/1, bottom/1, 
         add/2, sub/2, mul/2, divide/2,
         add/3, mul/3, negative/1,
         scalar_mul/2, gcf/2,
         zero/1, to_float/1,
         positive/1,
         equal/2]).

-record(number, {t, b}).

make(T, B) ->
    #number{t = T,
            b = B}.
top(X) ->
    X#number.t.
bottom(X) ->
    X#number.b.
to_float(Z) ->
    Z#number.t / Z#number.b.
zero(X) ->
    X#number.t == 0.

positive(X) ->
    (X#number.t * X#number.b) > 0.

equal(R1, R2) ->
    (R1#number.t * R2#number.b) ==
        (R2#number.t * R1#number.b).
   
gcf(N) when is_record(N, number) -> 
    gcf(N#number.t, N#number.b).

gcf(X, Y) when (abs(Y) > abs(X)) ->         
    gcf(Y, X);
gcf(X, 0) -> X;
gcf(X, Y) -> 
    Z = X rem Y,
    gcf(Y, Z).

simplify(N) when is_record(N, number) ->
    G = gcf(N#number.t, N#number.b),
    #number{t = N#number.t div G,
            b = N#number.b div G}.

square(N) ->    
    mul(N, N).
mul(N1, N2) ->
    simplify(
      #number{
         t = (N1#number.t * N2#number.t),
         b = (N1#number.b * N2#number.b)}).
mul(N1, N2, N3) ->
    mul(N1, mul(N2, N3)).
scalar_mul(S, N) ->
    mul(N, make(S, 1)).
sub(A, B) -> 
    DB = B#number.b,
    DA = A#number.b,
    simplify(
      #number{
         t = (A#number.t * DB) - 
             (B#number.t * DA),
         b = DB * DA}).
add(A, B, C) ->
    add(A, add(B, C)).
add(A, B) ->
    B2 = negative(B),
    sub(A, B2).
divide(A, B) ->
    B2 = #number{t = B#number.b,
                 b = B#number.t},
    mul(A, B2).

negative(N) ->
    N#number{t = -N#number.t}.
