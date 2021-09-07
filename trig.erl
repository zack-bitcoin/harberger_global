-module(trig).
%planar trigonometry over the rationals.
-export([spread_to_angle/1,
         spread/2, clockwise/3, 
         test/0]).
%-record(line, {x, y, z}).%3 integers
-record(point, {x, y, z}).%3 integers
-record(vector, {x, y}).%2 rationals
-record(vector3, {x, y, z}).%3 rationals

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
solid_spread(V1, V2, V3) ->
    %unused
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

test() ->
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
     parallel(V6, V5)}.
