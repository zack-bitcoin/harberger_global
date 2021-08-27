-module(trig).
-export([test/0, spread/2, spread_to_angle/1,
         clockwise/3, quadrance_to_distance/1]).
-record(line, {x, y, z}).%3 integers
-record(point, {x, y, z}).%3 integers
-record(vector, {x, y}).%2 rationals
-record(vector3, {x, y, z}).%3 rationals

test() ->
    P1 = #point{x = 2, y = 3, z = 4},
    P2 = #point{x = 4, y = 3, z = 1},
    V1 = point_to_vector(P1),
    V2 = point_to_vector(P2),

    {V1, V2,
     sub(V1, V2)}.

slope(#vector{x = X, y = Y}) ->
    D = rat:divide(Y, X),
    B = rat:bottom(D),
    T = rat:top(D),
    if
        ((B == 0) and T > 0) -> 10000000000000;
        (B == 0) -> -1000000000000000;
        true ->
            rat:to_float(rat:divide(Y, X))
    end.

clockwise(P1, P2, P3) ->
    V1 = point_to_vector(P1),
    V2 = point_to_vector(P2),
    V3 = point_to_vector(P3),
    W1 = sub(V3, V2),%vector from 2 to 3
    %W2 = sub(V1, V3),%from 3 to 1
    W3 = sub(V2, V1),%from 1 to 2
    rat:positive(determinate(W1, W3)).

vector_to_point(#vector{x = X, y = Y}) ->
    YB = rat:bottom(Y),
    XB = rat:bottom(X),
    #point{x = rat:top(X)*YB,
           y = rat:top(Y)*XB,
           z = YB * XB}.

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
add(#vector{x = X1, y = Y1},
    #vector{x = X2, y = Y2}) ->
    #vector{x = rat:add(X1, X2),
            y = rat:add(Y1, Y2)};
add(#vector3{x = X1, y = Y1, z = Z1},
    #vector3{x = X2, y = Y2, z = Z2}) ->
    #vector3{x = rat:add(X1, X2),
             y = rat:add(Y1, Y2),
             z = rat:add(Z1, Z2)}.

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
    %v3 = v2 - v1
    %Q1 + Q2 = Q3
    %spread is one.
    rat:zero(dot(V1, V2)).

make_perp(#vector{x = X, y = Y}) ->
    %#vector{x = rat:negative(Y), y = X}.
    #vector{x = Y, y = rat:negative(X)}.

determinate(V1 = #vector{}, 
            V2 = #vector{}) ->
    dot(V1, make_perp(V2)).
    %dot(make_perp(V1), V2).

determinate(V1 = #vector3{x = X1, y = Y1, z = Z1},
            V2 = #vector3{x = X2, y = Y2, z = Z2},
            V3 = #vector3{x = X3, y = Y3, z = Z3}) ->
    rat:sub(
      rat:add(rat:mul(X1, Y2, Z3),
              rat:mul(Y1, Z2, X3),
              rat:mul(Z1, X2, Y3)),
      rat:add(rat:mul(X3, Y2, Z2),
              rat:mul(Y3, Z2, X1),
              rat:mul(Z3, X2, Y1))).


project_z(#vector3{x = X, y = Y}) ->
    #vector{x = X, y = Y}.
project_y(#vector3{x = X, z = Z}) ->
    #vector{x = Z, y = X}.
project_x(#vector3{y = Y, z = Z}) ->
    #vector{x = Y, y = Z}.
parallel(V1 = #vector{}, 
         V2 = #vector{}) ->
    %v3 = v2 - v1
    %(Q1 + Q2 + Q3)^2 = 2*(Q1^2 + Q2^2 + Q3^2)
    %spread is zero for rationals.

    %perpendicular(V1, make_perp(V2)).
    rat:zero(determinate(V1, V2));
parallel(V1 = #vector3{},
         V2 = #vector3{}) ->
    %v3 = v2 - v1
    %(Q1 + Q2 + Q3)^2 = 2*(Q1^2 + Q2^2 + Q3^2)
    %spread is zero for rationals.
    B1 = parallel(project_x(V1),
                  project_x(V2)),
    B2 = parallel(project_y(V1),
                  project_y(V2)),
    B3 = parallel(project_z(V1),
                  project_z(V2)),
    B1 and B2 and B3.
     
quadrance_to_distance(R) -> 
    math:sqrt(rat:to_float(R)).

quadrance(V) ->
    dot(V, V).
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
%min(math:asin(X),
%        math:asin(X2)).
    
cross_law(Q1, Q2, Q3) ->
    %returns spread3
    L = rat:square(
          rat:sub(
            rat:add(Q1, Q2),
            Q3)),
    B = rat:mul(
          rat:scalar_mul(4, Q1),
          Q2),
    rat:sub(rat:make(1, 1),
            rat:divide(L, B)).

spread_law_s(Q1, S2, Q2) ->
    %returns spread1
    rat:divide(rat:mul(Q1, S2),
               Q2).
spread_law_q(S1, S2, Q2) ->
    %returns quadrance1
    rat:divide(rat:mul(Q2, S1),
               S2).

triple_spread(S1, S2) ->
    %(s1 + s2 + s3)^2 = 
    %  2*(s1^2 + s2^2 + s3^2) + 4*s1*s2*s3

    %(s1 + s2)^2 + s3^2 + 2*(s1+s2)*s3 =
    % 2*s1^2 + 2*s2^2 + 2*s3^2 + 4*s1*s2*s3

    %(s1 + s2)^2 + 2*(s1+s2)*s3 =
    % 2*s1^2 + 2*s2^2 + s3^2 + 4*s1*s2*s3
    
    %(s1^2 + s2^2) + 2*s1*s2 + 2*(s1+s2)*s3 =
    % 2*s1^2 + 2*s2^2 + s3^2 + 4*s1*s2*s3
    
    %2*s1*s2 + 2*(s1+s2)*s3 =
    % s1^2 + s2^2 + s3^2 + 4*s1*s2*s3
    
    %2*s1*s2 + 2*s1*s3 + 2*s2*s3 =
    % s1^2 + s2^2 + s3^2 + 4*s1*s2*s3

    % 2*s1*s3 + 2*s2*s3 - s3^2 - 4*s1*s2*s3 = 
    % s1^2 + s2^2 - 2*s1*s2
            
    % -s3^2 + s3(2*s1 + 2*s2 - 4*s1*s2) = 
    % (s1 - s2)^2

    % -s3^2 + s3(2*s1 + 2*s2 - 4*s1*s2) -
    % (s1 - s2)^2 = 0

    % s3^2 + s3(-2*s1 - 2*s2 + 4*s1*s2) +
    % (s1 - s2)^2 = 0

    %s3 = (2*s1 + 2*s2 - 4*s1*s2) +- 
    %  sqrt((4*s1^2 + 4*s2^2 + 16*s1^2*s2^2 + 4*s1*s2 - 8*s1^2*s2 - 8*s1*s2^2) - 4*(s1 - s2)^2)

    %s3 = (2*s1 + 2*s2 - 4*s1*s2) +- 
    %  2*sqrt((s1^2 + s2^2 + 4*s1^2*s2^2 + s1*s2 - 2*s1^2*s2 - 2*s1*s2^2) - (s1 - s2)^2)

    %s3 = (2*s1 + 2*s2 - 4*s1*s2) +- 
    %  2*sqrt((s1^2 + s2^2 + 4*s1^2*s2^2 + s1*s2 - 2*s1^2*s2 - 2*s1*s2^2) - (s1^2 + s2^2 - 2*s1*s2))
    
    %s3 = (2*s1 + 2*s2 - 4*s1*s2) +- 
    %  2*sqrt((4*s1^2*s2^2 + s1*s2 - 2*s1^2*s2 - 2*s1*s2^2) - (- 2*s1*s2))

    %s3 = (2*s1 + 2*s2 - 4*s1*s2) +- 
    %  2*sqrt((4*s1^2*s2^2 + s1*s2 - 2*s1^2*s2 - 2*s1*s2^2) + 2*s1*s2)
    
    %s3 = (2*s1 + 2*s2 - 4*s1*s2) +- 
    %  2*sqrt((4*s1^2*s2^2 + 3*s1*s2 - 2*s1^2*s2 - 2*s1*s2^2))

    %s3 = (2*s1 + 2*s2 - 4*s1*s2) +- 
    %  2*sqrt(s1*s2*(4*s1*s2 + 3 - 2*s1 - 2*s2))

    %s3 = 2*(s1 + s2 - 2*s1*s2) +- 
    %  2*sqrt(s1*s2*(4*s1*s2 + 3 - 2*s1 - 2*s2))

    %s3 = 2*(s1 + s2 - 2*s1*s2) +- 
    %  2*sqrt(s1*s2*(4*s1*s2 + 3 - 2*(s1 + s2)))
    ok.
