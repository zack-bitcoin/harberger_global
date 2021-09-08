-module(spherical_trig).
-export([
         join/2, dual/1, area/1,
         quadrance/2, direction/2, region/1,
         test/0, test2/0, test3/0, test4/0,
         test5/0, test6/0, test8/0,
         test9/0]).

-record(spoint, {point, s}).%3 integers, and a boolean, for which hemisphere the point is in. s is true if the point is in the northern hemisphere.
-record(sline, {line, s}).
-record(point, {x, y, z}).%3 integers.
-record(line, {x, y, z}).%3 integers.
-record(triangle, {x, y, z}).%3 (s)points
-record(trilateral, {x, y, z}).%3 spoints
-record(srat, {rat, s}).%rat is a rational. s is for whether we are talking about the big or small angle. true indicates the short distance.

%s is for whether this indicates an quadrance that crosses the equator, or a lune that contains a north or south pole.

make_trilateral(X, Y, Z) ->
    #trilateral{x = X, y = Y, z = Z}.
make_triangle(X, Y, Z) ->
    #triangle{x = X, y = Y, z = Z}.
            

quadrance(P1 = #point{}, P2 = #point{}) ->
    trig:spread(P1, P2);
quadrance(SP1 = #spoint{point = P1}, 
          SP2 = #spoint{point = P2}) ->
    SBig = same_hemisphere(SP1, SP2),
    Rat = quadrance(P1, P2),
    #srat{rat = Rat, s = SBig}.
same_hemisphere(
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
dot(#point{x = X1, y = Y1, z = Z1}, 
    #point{x = X2, y = Y2, z = Z2}) ->
    (X1 * X2) + (Y1 * Y2) + (Z1 * Z2);
dot(P1 = #line{}, P2 = #line{}) ->
    dot(proj:dual(P1), proj:dual(P2)).
quadrances(#triangle{x = X, y = Y, z = Z}) ->
    [quadrance(Y, Z),
     quadrance(Z, X),
     quadrance(Y, X)].
dual(#trilateral{x = L1, y = L2, z = L3}) ->
    #triangle{x = dual(L1), 
              y = dual(L2), 
              z = dual(L3)};
dual(#sline{line = (L = #line{}), 
            s = S}) ->
    #spoint{point = proj:dual(L), s = S};
dual(#spoint{point = (P = #point{}), 
             s = S}) ->
    #sline{line = proj:dual(P), s = S}.

cross(#point{x = X1, y = Y1, z = Z1},
      #point{x = X2, y = Y2, z = Z2}) ->
    %notice we do NOT simplify this point by removing the greatest common denominator.
    #point{x = (Y1 * Z2) - (Z1 * Y2),
           y = (Z1 * X2) - (X1 * Z2),
           z = (X1 * Y2) - (Y1 * X2)}.
%meet(L1 = #sline{}, L2 = #sline{}) ->
%    dual(join(dual(L1), dual(L2))).
join(#spoint{point = P1, s = S1},
     #spoint{point = P2, s = S2}) ->
    %for S, take the cross product of the vector starting at the center of the globe, and passing through that point.
%    North = #point{x = 0,y = 0,z = 1},
%    Orthogonal = cross(P1, P2),
%    CircleContainsNorth = 
%        Orthogonal#point.z > 0,
        %dot(North, Orthogonal) > 0,
    %A = (P1#point.x * P2#point.y),
    %B = (P2#point.x * P1#point.y),
    %CircleContainsNorth = not(A < B),
    #point{x = X, y = Y, z = Z} = cross(P1, P2),
    CircleContainsNorth = 
        if
            (Z == 0) and (X == 0) -> Y > 0;
            Z == 0 -> X > 0;
            true -> Z > 0
        end,
    #sline{line = proj:join(P1, P2),
           s = CircleContainsNorth xor S1 xor S2}.
flip_hemisphere(#trilateral{
                   x = X, y = Y, z = Z}) ->
    #trilateral{x = X#sline{s = not(X#sline.s)},
                y = Y#sline{s = not(Y#sline.s)},
                z = Z#sline{s = not(Z#sline.s)}}.
clockwise(#triangle{x = X, y = Y, z = Z}) ->
    true xor
    X#spoint.s xor 
        Y#spoint.s xor 
        Z#spoint.s xor
        trig:clockwise(
          X#spoint.point, 
          Y#spoint.point, 
          Z#spoint.point).
    
triangle_to_trilateral(Tri = #triangle{
                         x = X, y = Y, z = Z}) ->
    Clockwise = clockwise(Tri),
    T2 = make_trilateral(
           join(Y, Z),join(Z, X),join(X, Y)),
    if
        Clockwise -> T2;
        true -> 
            flip_hemisphere(T2)
    end.
trilateral_to_triangle(T) ->
    dual(triangle_to_trilateral(dual(T))).
spreads(T = #triangle{}) ->
    Qs = quadrances(
           dual(triangle_to_trilateral(T))),
    lists:map(fun(R) ->
                      R#srat{s = not(R#srat.s)}
              end, Qs).
planar_area([A1, A2, A3]) ->
    S = (A1 + A2 + A3) / 2,
    Area2 = S * (S - A1) * 
        (S - A2) * (S - A3),
    math:sqrt(max(0, Area2)).
area2([A, B, C|T]) -> 
    %also works for convex regions.
    Tri = make_triangle(A, B, C),
    S = clockwise(Tri),
    Area = area(Tri),
    A2 = if
             S -> Area;
             true -> -Area
         end,
    A2 + area([A, C|T]);
area2([_, _]) -> 0.
area(T = #triangle{}) ->
    [A1C, A2C, A3C] = angles(T),
    Area1 = A1C + A2C + A3C - (math:pi()),
    Q2 = spreads_to_angles(quadrances(T)),
    Area2 = planar_area(Q2),
    if
        (Area1 < 0.00001) -> Area2;
        true -> Area1
    end;
area(L) ->
    min(area2(L),
        area2(tl(L) ++ [hd(L)])).
spreads_to_angles(S) ->
    lists:map(fun spread_to_angle/1, S).
spread_to_angle(#srat{rat = R, s = Big}) ->
    {A1, A2} = trig:spread_to_angle(R),
    if
        Big -> A1;
        true -> A2
    end.
angles(T = #triangle{}) ->
    spreads_to_angles(spreads(T)).
direction(P1 = #spoint{}, P2 = #spoint{}) ->
    %NP = proj:make_point(1,1,1000000),
    NP = proj:make_point(0,0,1),
    North = #spoint{point = NP, s = true},
    T = #triangle{x = P1, y = North, z = P2},
    S1 = hd(spreads(T)),
    S2 = S1#srat{s = not(S1#srat.s)},
    Angle = spread_to_angle(S2),
    A2 = Angle*180/math:pi(),
    Clockwise2 = clockwise(T),
    if
        Clockwise2 -> 180 - A2;
        true -> A2 - 180
    end.
contains(_, [], E, C) -> 
    {lists:reverse(E), lists:reverse(C)};
contains(New = #spoint{}, 
         [Sp = #spoint{}|Points],
         E, C) ->
    B = same_hemisphere(New, Sp),
    {E2, C2} = if B -> {[Sp|E], C};
                  true -> {E, [Sp|C]} end,
    contains(New, Points, E2, C2).
remove_excess_lines([A, B, C|L]) ->
    Tri = make_trilateral(A, B, C),
    Tri2 = trilateral_to_triangle(Tri),
    Clockwise = clockwise(Tri2),
    if
        (Clockwise) ->
            remove_excess_lines2([A, B, C], L);
        true ->
            remove_excess_lines2([A, C, B], L)
    end.
    %start with a trilateral, keep trying to add more lines constraining the space.
remove_excess_lines2(Lines, []) -> Lines;
remove_excess_lines2(Lines, T) ->
    %Points = pointify(slope_sort(Lines)),
    Points = pointify(Lines),
    {E, C} = contains(dual(hd(T)), 
                      Points, [], []),
    if
        (C == []) -> {error, empty_region};
        (E == []) ->
   %If a new line contains all existing points, then we should drop that line.
            io:fwrite("contains all the points\n"),
            remove_excess_lines2(Lines, tl(T));
        (length(E) == 1) ->
            io:fwrite("cut off one point\n"),
            Lines2 = rotate_to_meet(Lines, hd(E), length(Lines)),
            remove_excess_lines2([hd(T)|Lines2], tl(T));
        true -> %remove_excess_lines3(Lines, C, E, T)
            io:fwrite("cut off more than one point\n"),
            Lines2 = remove_if_touch_2(Lines, E),%todo. maybe we don't need this
            Lines3 = rotate_to_unlisted(Lines2, C, length(Lines)),
            Lines4 = [hd(T)|Lines3],
            remove_excess_lines2(Lines4, tl(T))
    end.
rotate_to_meet(_, _, -2) ->
    io:fwrite("rotate to meet error\n"),
    error;
rotate_to_meet([A, B, C|T], Meet, Limit) ->
    Lat = make_trilateral(A, B, C),
    Tri = trilateral_to_triangle(Lat),
    #triangle{z = M} = Tri,
    if
        M == Meet ->
            [B, C|T]++[A]; 
        true ->
            rotate_to_meet([B, C|T]++[A], Meet, Limit - 1)
    end.
rotate_to_unlisted(_, _, -2) ->
    io:fwrite("rotate to unlisted error\n"),
    error;
rotate_to_unlisted([A, B, C|T], L, Limit) ->
    Lat = make_trilateral(A, B, C),
    Tri = trilateral_to_triangle(Lat),
    #triangle{z = M} = Tri,
    Bool = is_in(M, L),
    if
        not(Bool) -> [B|T]++[A];
        true -> rotate_to_unlisted(
                  [B|T]++[A], L, Limit - 1)
    end.
is_in(_, []) -> false;
is_in(X, [X|_]) -> true;
is_in(X, [_|T]) -> is_in(X, T).
    
remove_if_touch_2(Lines, Ps) 
%currently unused, but  maybe it is an error and we need it.
  when (length(Ps) < 2) -> Lines;
remove_if_touch_2(Lines, Ps) ->
    lists:filter(
      fun(#sline{line = L}) -> 
              length(
                lists:filter(
                  fun(#spoint{point = P}) -> 
                          proj:incident(L, P) 
                  end, Ps)) < 2
      end, 
      Lines).
linify([H|T]) ->
    %starts with point preceding first line.
    %used for tests.
    X = join(lists:last(T), H),
    linify2([H|T]) ++ [X].
linify2([_]) -> [];
linify2([A|[B|T]]) -> [join(A, B)|linify2([B|T])].
new_linify(P) ->
    linify2(P ++ [hd(P), hd(tl(P))]).
new_linify2([_, _]) -> [];
new_linify2([A, B, C|T]) -> 
    T1 = make_triangle(A, B, C),
    T2 = triangle_to_trilateral(T1),
    #trilateral{x = Z} = T2,
    [Z|linify2([B, C|T])].
pointify(L) -> pointify2(L ++ [hd(L), hd(tl(L))]).
pointify2([_, _]) -> [];
pointify2([A, B, C|T]) ->
    T1 = make_trilateral(A, B, C),
    T2 = trilateral_to_triangle(T1),
    #triangle{x = Z} = T2,
    [Z|pointify2([B, C|T])].
concurrent(#sline{line = X}, 
           #sline{line = Y},
           #sline{line = Z}) ->
    proj:concurrent(X, Y, Z).
remove_concurrents([A, B, C|T]) -> 
    Bool = concurrent(A, B, C),
    case {Bool, T} of
        {true, []} -> [];
        {_, []} -> [A, B, C];
        {true, _} -> remove_concurrents([A, C|T]);
        _ -> [A|remove_concurrents([B, C|T])]
    end.
%region operations
region(L) ->
    %given a list of lines, return the points at the corner of the enclosed region.
%todo: if a region has no space in it, we need to realize this and say so.
    if
        length(L) < 3 -> empty_region;
        true ->
            L2 = L,
            L4 = remove_excess_lines(L2),
            L5 = remove_concurrents(L4),
            if
                (L5 == L4) -> pointify(L4);
                true -> region(L5)
            end
    end.
    

test() ->
    B = 1000,
    P1 = #spoint{point = #point{x = 0, y = -B, z = B}, s = true},
    P2 = #spoint{point = #point{x = 0-1, y = -B - 5, z = B}, s = true},
    P3 = #spoint{point = #point{x = 0+1, y = -B-5, z = B}, s = true},
    %Tokyo = #spoint{point = #point{x = 45091, y = 53075, z = 50000}, s = true},
    %Mel = #spoint{point = #point{x = -36991, y = -52750, z = 50000}, s = false},
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
    P1 = #spoint{point = #point{x = B, y = -B+1, z = B}, s = true},
    P2 = #spoint{point = #point{x = B, y = -B-1, z = B}, s = true},
    P3 = #spoint{point = #point{x = B+1, y = -B-1, z = B}, s = true},
    Trib = make_triangle(P1, P3, P2),
    Trib2 = trilateral_to_triangle(
             triangle_to_trilateral(Trib)),
    Tri = make_triangle(P1, P2, P3),
    Tri2 = trilateral_to_triangle(
             triangle_to_trilateral(Tri)),
    Tri = Tri2,
%    io:fwrite({triangle_to_trilateral(Tri), 
%               triangle_to_trilateral(Trib)}),
    Trib = Trib2,
   

    { 
      join(P1, P2),
     join(P2, P1),
     join(P1, P3),
     join(P3,P1)
    }.
    
test3() ->    
    Trilat = {trilateral,
              {sline,
               {line,8953,-156,3},
               false},
              {sline,
               {line,0,312,8947},
               true},
              {sline,
               {line,-8953,-156,3},
               false}},
    Trilat = triangle_to_trilateral(
               trilateral_to_triangle(
                 Trilat)).
test4() ->
    B = 10,
    P1 = #spoint{point = #point{x = B, y = -B+1, z = B}, s = true},
    P2 = #spoint{point = #point{x = B, y = -B-1, z = B}, s = true},
    P3 = #spoint{point = #point{x = B-1, y = -B-1, z = B}, s = true},
    Tri = make_triangle(P1, P2, P3),
    Tril = triangle_to_trilateral(Tri),
    Tri = trilateral_to_triangle(Tril),
    %io:fwrite({Tri, Tri2}),
    
    {Tri,
     Tril,
     dual(Tril),
     spreads(Tri),
     angles(Tri),
     area(Tri)}.
    
test5() ->
    P1 = globe:gps_to_point({42, 10}),
    P2 = globe:gps_to_point({43, 8}),
    P3 = globe:gps_to_point({45, 7}),
    P4 = globe:gps_to_point({47, 8}),
    P5 = globe:gps_to_point({48, 10}),
    P6 = globe:gps_to_point({47, 12}),
    P7 = globe:gps_to_point({45, 13}),
    P8 = globe:gps_to_point({43, 12}),
    Ps = [P1, P2, P3, P4,
          P5, P6, P7, P8],
    [L1, L2, L3, L4,
     L5, L6, L7, L8] = linify(Ps),

    UL1 = dual(P1),
    UL2 = dual(P2),
    UL3 = dual(P3),
    UL4 = dual(P4),
    
    UL1b = UL1#sline{s = not(UL1#sline.s)},
    UL0b = dual(
             globe:gps_to_point({1, -190})),
    UL0c = dual(
             %globe:gps_to_point({-85, 45})),
             globe:gps_to_point({-1, -190})),
   
    NP = globe:gps_to_point({89, -80}),%left of north pole
    NB1 = join(P3, NP),%should include all.
    NB1b = join(NP, P3),%should exclude all.
    false = (NB1 == NB1b),
    NB2 = join(NP, P7),
    %L = [UL1, UL2, UL3, UL4, L1, L2, L3, L4, L5, L6, L7, L8],
    L = [L1, L2, L3, L4, L5, L6, L7, L8, %borders
         NB2, NB1, %tangent at a point
         %NB1b,
         UL0b, UL0c, UL1b],%external points that touch nowhere
    %L = [L1, L2, L3, L4, L5, L6, L7, L8],
    {
      {bad_lines, {NB1}},
      {region, globe:gpsify(region(L))}
      %lists:map(fun(X) -> point_to_gps(X) end,
      %           region(L))}
      %linify(region(L))
    }.
   
test6() -> 
    P1 = globe:gps_to_point({-48, 10}),
    P2 = globe:gps_to_point({-47, 8}),
    P3 = globe:gps_to_point({-45, 7}),
    P4 = globe:gps_to_point({-43, 8}),
    P5 = globe:gps_to_point({-42, 10}),
    P6 = globe:gps_to_point({-43, 12}),
    P7 = globe:gps_to_point({-45, 13}),
    P8 = globe:gps_to_point({-47, 12}),
    Ps = [P1, P2, P3, P4,
          P5, P6, P7, P8],
    %draw a circle like test 4, but this time in the southern hemisphere.
    [L1, L2, L3, L4,
     L5, L6, L7, L8] = linify(Ps),
    UL1 = dual(P1),
    UL2 = dual(P2),
    UL3 = dual(P3),
    UL4 = dual(P4),
    UL1b = UL1#sline{s = not(UL1#sline.s)},
    L = linify(Ps) ++ [UL1b],
    {
      %{pointified1, globe:gpsify(Ps)},
      {pointified, globe:gpsify(pointify(L))},
      {region, globe:gpsify(region(L))}
    }.
test8() ->    
    %looks like it fails if any point is on the lines x=0 or y=0 or z=0
    XD = -45,
    YD = 0,
    P1 = globe:gps_to_point({0+YD, -6+XD}),
    P2 = globe:gps_to_point({4+YD, -4+XD}),
    P3 = globe:gps_to_point({6+YD,  0+XD}),
    P4 = globe:gps_to_point({4+YD,  4+XD}),
    P5 = globe:gps_to_point({0+YD,  6+XD}),
    P6 = globe:gps_to_point({-4+YD, 4+XD}),
    P7 = globe:gps_to_point({-6+YD, 0+XD}),
    P8 = globe:gps_to_point({-4+YD, -4+XD}),
    Ps = [%P1, P2, 
          P3, P4,
          P5, P6, P7, P8,
          P1, P2
         ],
    
    [L1, L2, L3, L4,
     L5, L6, L7, L8] = linify(Ps),
    L = linify(Ps),
    %Tri = make_triangle(P2, P4, P7),
    %Tri = make_triangle(P1, P7, P3),
    Tri = make_triangle(P1, P3, P7),
    Tri2 = trilateral_to_triangle(
            triangle_to_trilateral(Tri)),
    Region = region(L),
    Region2 = region(lists:reverse(L)),
    { 
      Tri, Tri2,
      %Ps,
      L,
      remove_excess_lines(L),
      %remove_concurrents(remove_excess_lines(L)),
      %Ps,
      Region,
      Region2,
      %pointify(L),
      %{points, globe:gpsify(Ps)},
      %{pointified, globe:gpsify(pointify(L))},
      %{region, globe:gpsify(region(L))},
      {area_points, area(Ps)},
      {region, area(pointify(L))},
      {region, area(Region)},
      {region, area(region(lists:reverse(L)))}
    }.
    
test9() ->
    T = {triangle,{spoint,{point,-188,-37254,65},true},
         {spoint,{point,100,-57701,3125},true},
         {spoint,{point,-100,57701,3125},false}},
    Tb = {triangle,{spoint,{point,-188,-37254,65},true},
         {spoint,{point,100,-57701,3125},true},
         {spoint,{point,-101,57701,3125},false}},
    Trilat = triangle_to_trilateral(T),
    Trilatb = triangle_to_trilateral(Tb),
    T2 = trilateral_to_triangle(
          Trilat),
    T2b = trilateral_to_triangle(
          Trilatb),
    Trilat3 = #trilateral{
      x = #sline{
        line = #line{x = 1, y = 1, z = 0},
        s = true},
      y = #sline{
        line = #line{x = 1, y = 1, z = 1},
        s = true},
      z = #sline{
        line = #line{x = 2, y = 1, z = 1},
        s = true}},
    {area(T2),
     T, T2, T2b, Trilat, Trilatb}.
    
    
    
