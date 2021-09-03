-module(spherical_trig).
-export([area/1, quadrance/2,direction/2, 
         test/0, test2/0]).

-record(spoint, {point, s}).%3 integers, and a boolean, for which hemisphere the point is in. s is true if the point is in the northern hemisphere.
-record(sline, {line, s}).
-record(point, {x, y, z}).%3 integers.
-record(line, {x, y, z}).%3 integers.
-record(triangle, {x, y, z}).%3 (s)points
-record(trilateral, {x, y, z}).%3 spoints
-record(srat, {rat, s}).%rat is a rational. s is for whether we are talking about the big or small angle. true indicates the short distance.

%s is for whether this indicates an quadrance that crosses the equator, or a lune that contains a north or south pole.
-record(rat, {top, bottom}).

quadrance(P1 = #point{}, P2 = #point{}) ->
    trig:spread(P1, P2);
quadrance(SP1 = #spoint{point = P1, s = S1}, 
          SP2 = #spoint{point = P2, s = S2}) ->
    SBig = same_side_is_small(SP1, SP2),
    Rat = quadrance(P1, P2),
    #srat{rat = Rat, s = SBig}.
same_side_is_small(
  P1 = #spoint{}, P2 = #spoint{}) ->
    dot2(P1, P2) > 0.
dot2(#sline{line = P1, s = S1}, #sline{line = P2, s = S2}) ->
    D = dot(P1, P2),
    if
        (S1 xor S2) -> -D;
        true -> D
    end;
dot2(P1 = #spoint{}, P2 = #spoint{}) ->
    dot2(dual(P1), dual(P2)).
dot(P1 = #point{x = X1, y = Y1, z = Z1}, 
    P2 = #point{x = X2, y = Y2, z = Z2}) ->
    (X1 * X2) + (Y1 * Y2) + (Z1 * Z2);
dot(P1 = #line{}, P2 = #line{}) ->
    dot(proj:dual(P1), proj:dual(P2)).
quadrances(T = #triangle{x = X, y = Y, z = Z}) ->
    [quadrance(Y, Z),
     quadrance(Z, X),
     quadrance(Y, X)].
dual(#trilateral{x = L1, y = L2, z = L3}) ->
    #triangle{x = dual(L1), 
              y = dual(L2), 
              z = dual(L3)};
dual(#sline{line = L, s = S}) ->
    #spoint{point = proj:dual(L), s = S};
dual(#spoint{point = P, s = S}) ->
    #sline{line = proj:dual(P), s = S}.

join(#spoint{point = P1, s = S1},
     #spoint{point = P2, s = S2}) ->
    %for S, take the cross product of the vector starting at the center of the globe, and passing through that point.
%    North = #point{x = 0,y = 0,z = 1},
%    Orthogonal = cross(P1, P2),
%    CircleContainsNorth = 
%        Orthogonal#point.z > 0,
        %dot(North, Orthogonal) > 0,
    CircleContainsNorth = 
        (P1#point.x * P2#point.y) > 
        (P2#point.x * P1#point.y),
    #sline{line = proj:join(P1, P2),
           s = CircleContainsNorth xor S1 xor S2}.
triangle_to_trilateral(
  #triangle{x = X, y = Y, z = Z}) ->
    #trilateral{x = join(Y, Z),
                y = join(Z, X),
                z = join(X, Y)}.
spreads(T = #triangle{}) ->
    quadrances(
      dual(triangle_to_trilateral(T))).
planar_area([A1, A2, A3]) ->
    S = (A1 + A2 + A3) / 2,
    Area2 = 
      S 
      * (S - A1) 
      * (S - A2)
      * (S - A3),
    math:sqrt(max(0, Area2)).
area(T = #triangle{}) ->
    [A1C, A2C, A3C] = angles(T),
    Area1 = A1C + A2C + A3C - (math:pi()),
    Q2 = spreads_to_angles(quadrances(T)),
    Area2 = planar_area(Q2),
    if
        (Area1 < 0.00001) -> Area2;
        true -> Area1
    end.
spreads_to_angles([]) -> [];
spreads_to_angles([H|T]) -> 
    [spread_to_angle(H)|
     spreads_to_angles(T)].
spread_to_angle(#srat{rat = R, s = Big}) ->
    {A1, A2} = trig:spread_to_angle(R),
    if
        Big -> A1;
        true -> A2
    end.
angles(T = #triangle{}) ->
    spreads_to_angles(spreads(T)).
direction(P1 = #spoint{point = U1, s = S1}, 
          P2 = #spoint{point = U2, s = S2}) ->
    NP = proj:make_point(0,0,1),
    North = #spoint{point = NP, s = true},
    T = #triangle{x = P1, y = North, z = P2},
    Angle = spread_to_angle(hd(spreads(T))),
    A2 = Angle*180/math:pi(),
    Clockwise = trig:clockwise(U1, NP, U2),
    if
        (Clockwise xor S1 xor S2) -> A2;
        true -> 360-A2
    end.

test() ->
    B = 1000,
    P1 = #spoint{point = #point{x = 0, y = -B, z = B}, s = true},
    P2 = #spoint{point = #point{x = 0-1, y = -B - 5, z = B}, s = true},
    P3 = #spoint{point = #point{x = 0+1, y = -B-5, z = B}, s = true},
    Tokyo = #spoint{point = #point{x = 4509, y = 5307, z = 5000}, s = true},
    Mel = #spoint{point = #point{x = -3699, y = -5275, z = 5000}, s = false},
    Sydney = #spoint{point = #point{x = -3607, y = -6508, z = 5000}, s = false},
    T = #triangle{x = P1, y = P2, z = P3},
    NP = proj:make_point(0,0,1),
    North = #spoint{point = NP, s = true},
    ToDegree = fun(R) -> R*180/math:pi() end,
    MapDegree = fun(L) -> 
                        lists:map(
                          fun(X) ->
                                  X*180/math:pi()
                          end, L)
                end,
    {
     %quadrances(T),
      spreads(#triangle{x = Mel, y =North, z = Tokyo}),
      spreads(#triangle{x = Tokyo, y =North, z = Mel}),
      spreads(#triangle{x = P2, y =North, z = P1}),
      %spreads2(#triangle{x = P1, y =North, z = P2}),
      %spreads(#triangle{x = P2, y =North, z = P1}),
      %MapDegree(angles(#triangle{x = Mel, y =North, z = Sydney})),%should be 53, 6, 130
      MapDegree(angles(#triangle{x = P1, y =North, z = P2})),%should be 165,0,15  change:1,0,0
      %{111, 111
      MapDegree(angles(#triangle{x = Tokyo, y =North, z = Mel})),%should be 
      %110 111
      MapDegree(angles(#triangle{x = Mel, y =North, z = Tokyo})),%should be 4.5, 5, 175
      %011, 101
      MapDegree(angles(#triangle{x = P2, y =North, z = P1})),%should be 15,0,165  change:0,0,1
      %111 101
     direction(P2, P1),%should be 15
     direction(P1, P3),%
     direction(P1, P2),%should be 195
     direction(Tokyo, Mel),%
     direction(Sydney, Mel),%
     direction(Mel, Tokyo),%
     area(T)
    }.
    
test2() ->
    B = 1000,
    P1 = #spoint{point = #point{x = B, y = -B, z = B}, s = true},
    P2 = #spoint{point = #point{x = B, y = -B-1, z = B}, s = true},
    P3 = #spoint{point = #point{x = B+1, y = -B-1, z = B}, s = true},
    {join(P1, P2),
     join(P1, P3),
     join(P2, P1),
     join(P3,P1)
    }.
    
    
    
               
    
    
