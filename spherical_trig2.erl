-module(spherical_trig2).
-export([test/0, area/1, quadrance/2,
        direction/2, quadrances/1, angles/1
        ]).

-record(spoint, {point, s}).%3 integers, and a boolean, for which hemisphere the point is in. s is true if the point is in the northern hemisphere.
-record(sline, {line, s}).
-record(point, {x, y, z}).%3 integers.
-record(line, {x, y, z}).%3 integers.
-record(vector3, {x, y, z}).%3 rationals
-record(triangle, {x, y, z}).%3 (s)points
-record(trilateral, {x, y, z}).%3 spoints
-record(srat, {rat, s}).%rat is a rational. s is for whether we are talking about the big or small angle. true indicates the short distance.

%s is for whether this indicates an quadrance that crosses the equator, or a lune that contains a north or south pole.
-record(rat, {top, bottom}).

xor_rat(R = #srat{s = S}, V) ->
    R#srat{s = S xor V}.
 
quadrance(P1 = #point{}, P2 = #point{}) ->
    V1 = trig:point_to_3vector(P1),
    V2 = trig:point_to_3vector(P2),
    trig:spread(V1, V2);
quadrance(#spoint{point = P1, s = S1}, 
          #spoint{point = P2, s = S2}) ->
    S = S1 xor S2,%indicates that the quadrance is for the portion that does not cross the equator.
    S3 = same_side_is_small(P1, P2),
    SBig = S xor S3,%indicates that we are talking about the bigger distance/angle.
    Rat = quadrance(P1, P2),
    #srat{rat = Rat, s = SBig}.
same_side_is_small(
  P1 = #point{x = X1, y = Y1, z = Z1}, 
  P2 = #point{x = X2, y = Y2, z = Z2}) ->
    V1 = trig:point_to_3vector(P1),
    V2 = trig:point_to_3vector(P2),
    V3 = trig:negative(V2),
    Qsame = trig:quadrance(trig:sub(V1, V2)),
    Qother = trig:quadrance(trig:sub(V1, V3)),
    rat:less_than(Qsame, Qother).
quadrances(T = #triangle{x = X, y = Y, z = Z}) ->
    [quadrance(Y, Z),
     quadrance(Z, X),
     quadrance(Y, X)].
dual(#trilateral{x = L1, y = L2, z = L3}) ->
    #triangle{x = dual(L1), y = dual(L2), z = dual(L3)};
dual(#sline{line = L, s = S}) ->
    #spoint{point = proj:dual(L), s = S}.
join(#spoint{point = P1},
     #spoint{point = P2}) ->
    %for S, take the cross product of the vector starting at the center of the globe, and passing through that point.
    North = #point{x = 0,y = 0,z = 1},
    B = rat:positive(
          trig:dot(trig:point_to_3vector(North), 
                   trig:cross(P1, P2))),
    #sline{line = proj:join(P1, P2),
           s = B}.
triangle_to_trilateral(
  #triangle{x = X, y = Y, z = Z}) ->
    #trilateral{x = join(Y, Z),
                y = join(Z, X),
                z = join(X, Y)}.
v_xor([H]) -> H;
v_xor([H|[I|T]]) -> 
    v_xor([v_xor(H, I)|T]).
v_xor([A, B, C], [D, E, F]) ->
    [A xor D, B xor E, C xor F].
flip([]) -> [];
flip([B|R]) -> [not(B)|flip(R)].
flip_if(true, V) -> flip(V);
flip_if(false, V) -> V.
    
tri_to_signs(#triangle{
                x = #spoint{s = A},
                y = #spoint{s = B},
                z = #spoint{s = C}}) ->
    [A, B, C];
tri_to_signs(#trilateral{
                x = #sline{s = A},
                y = #sline{s = B},
                z = #sline{s = C}}) ->
    [A, B, C].
even_tri([A, B, C]) ->
    not(A xor B xor C).
spreads(T = #triangle{}) ->
    Trilat = triangle_to_trilateral(T),
    Polar = dual(Trilat),
    [Q1, Q2, Q3] = quadrances(Polar),
    LMN = tri_to_signs(T),
    DEF = v_xor([LMN, 
                 tri_to_signs(
                   triangle_to_trilateral(Polar)),
                 tri_to_signs(Trilat)
                ]),
    [D2, E2, F2] = flip_if(even_tri(LMN), DEF),
    [xor_rat(Q1, D2),
     xor_rat(Q2, E2),
     xor_rat(Q3, F2)].
    
planar_area([A1, A2, A3]) ->
    S = (A1 + A2 + A3) / 2,
    Area2 = 
      S 
      * (S - A1) 
      * (S - A2)
      * (S - A3),
    if
        (Area2 > 0) -> math:sqrt(Area2);
        true -> 0
    end.
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
                                            
    {%spherical_trig:quadrances(remove_sign(T)),
     %quadrances(T),
      %pole_side_is_small(proj:join(remove_sign(P3), remove_sign(P1)),
      %                   proj:join(remove_sign(P1), remove_sign(P2))),
     %spherical_trig:angles(remove_sign(T)),
     %angles(T),
      %spreads(#triangle{x = P1, y =North, z = P2}),
      %spreads(#triangle{x = P2, y =North, z = P1}),
      %MapDegree(angles(#triangle{x = Mel, y =North, z = Sydney})),%should be 53, 6, 130
      MapDegree(angles(#triangle{x = Tokyo, y =North, z = Mel})),%should be 
      
      %110, 100, 100, 010,
      MapDegree(angles(#triangle{x = Mel, y =North, z = Tokyo})),%should be 4.5, 5, 175
      %011, 100, 110, 000,
      MapDegree(angles(#triangle{x = P2, y =North, z = P1})),%should be 15,0,165  change:0,0,1
      %{000  100,  101, 000
      MapDegree(angles(#triangle{x = P1, y =North, z = P2})),%should be 165,0,15  change:1,0,0
      %{000  100,  010,  010,
     direction(P2, P1),%should be 15
     direction(P1, P3),%
     direction(P1, P2),%should be 195
     direction(Tokyo, Mel),%
     direction(Sydney, Mel),%
     direction(Mel, Tokyo),%
      %spherical_trig:direction(remove_sign(P1), remove_sign(P2)),
     %spherical_trig:direction(remove_sign(P2), remove_sign(P1))
      %spherical_trig:area(remove_sign(T)),
     area(T)
    }.
    
    
    
    
               
    
    
