-module(spherical_trig).
-export([
         join/2, meet/2, dual/1,
         area/1, quadrance/2,direction/2, 
         same_hemisphere/2, 
         make_trilateral/3,
         trilateral_to_triangle/1,
         region/1,
         test/0, test2/0, test3/0, test4/0,
         test5/0, test6/0, test7/0]).

-record(spoint, {point, s}).%3 integers, and a boolean, for which hemisphere the point is in. s is true if the point is in the northern hemisphere.
-record(sline, {line, s}).
-record(point, {x, y, z}).%3 integers.
-record(line, {x, y, z}).%3 integers.
-record(triangle, {x, y, z}).%3 (s)points
-record(trilateral, {x, y, z}).%3 spoints
-record(srat, {rat, s}).%rat is a rational. s is for whether we are talking about the big or small angle. true indicates the short distance.

%s is for whether this indicates an quadrance that crosses the equator, or a lune that contains a north or south pole.
-record(rat, {top, bottom}).

make_trilateral(X, Y, Z) ->
    #trilateral{x = X, y = Y, z = Z}.
make_triangle(X, Y, Z) ->
    #triangle{x = X, y = Y, z = Z}.

quadrance(P1 = #point{}, P2 = #point{}) ->
    trig:spread(P1, P2);
quadrance(SP1 = #spoint{point = P1, s = S1}, 
          SP2 = #spoint{point = P2, s = S2}) ->
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

meet(L1 = #sline{}, L2 = #sline{}) ->
    dual(join(dual(L1), dual(L2))).
join(#spoint{point = P1, s = S1},
     #spoint{point = P2, s = S2}) ->
    %for S, take the cross product of the vector starting at the center of the globe, and passing through that point.
%    North = #point{x = 0,y = 0,z = 1},
%    Orthogonal = cross(P1, P2),
%    CircleContainsNorth = 
%        Orthogonal#point.z > 0,
        %dot(North, Orthogonal) > 0,
    A = (P1#point.x * P2#point.y),
    B = (P2#point.x * P1#point.y),
    CircleContainsNorth = A > B,
    #sline{line = proj:join(P1, P2),
           s = CircleContainsNorth xor S1 xor S2}.
flip_hemisphere(Tl = #trilateral{
                  x = X, y = Y, z = Z}) ->
    #trilateral{x = X#sline{s = not(X#sline.s)},
                y = Y#sline{s = not(Y#sline.s)},
                z = Z#sline{s = not(Z#sline.s)}}.
clockwise(Tri = #triangle{x = X, y = Y, z = Z}) ->
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
    T2 = #trilateral{x = join(Y, Z),
                     y = join(Z, X),
                     z = join(X, Y)},
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
area(T = #triangle{}) ->
    [A1C, A2C, A3C] = angles(T),
    Area1 = A1C + A2C + A3C - (math:pi()),
    Q2 = spreads_to_angles(quadrances(T)),
    Area2 = planar_area(Q2),
    if
        (Area1 < 0.00001) -> Area2;
        true -> Area1
    end.
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
    NP = proj:make_point(0,0,1),
    North = #spoint{point = NP, s = true},
    T = #triangle{x = P1, y = North, z = P2},
    Angle = spread_to_angle(hd(spreads(T))),
    A2 = Angle*180/math:pi(),
    Clockwise2 = clockwise(T),
    A3 = if
             Clockwise2 -> A2;
             true -> 360-A2
         end,
    180-A3.
slope_sort(L) ->
    lists:sort(
      fun(A, B) ->
              rat:less_than(
                slope(B), slope(A))
      end, L).
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
    remove_excess_lines2([A, B, C], L).
    %start with a trilateral, keep trying to add more lines constraining the space.
remove_excess_lines2(Lines, []) -> Lines;
remove_excess_lines2(Lines, T) ->
    Points = pointify(slope_sort(Lines)),
    {E, C} = contains(dual(hd(T)), 
                      Points, [], []),
    case C of
        [] -> {error, empty_region};
        _ -> remove_excess_lines3(Lines, E, T)
    end.
remove_excess_lines3(Lines, E, [New|T]) ->
    Lines2 = remove_if_touch_2(Lines, E),
    Lines3 = if %If a new line contains all existing points, then we should drop that line.
                 (E == []) -> Lines2;
                 true -> [New|Lines2]
             end,
    remove_excess_lines2(Lines3, T).
remove_if_touch_2(Lines, Ps) 
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
slope(#sline{line = #line{x = X, y = Y}, 
             s = S}) ->
    One = rat:make(1, 1),
    Slope1 = rat:make(-X, Y),
    B = rat:positive(Slope1),
    Slope2 = 
        %monatonic from rationals -> positive rationals
        if
            B -> 
                %from positives to rationals > 1.
                rat:add(One, Slope1);
            true ->
                %from negatives to rationals < 1 and > 0.
                rat:inverse(rat:sub(One, Slope1))
        end,
    if
        %lower hemisphere is negatives.
        S -> Slope2;
        true -> rat:inverse(rat:negative(Slope2))
    end.
linify([H|T]) ->
    %starts with point preceding first line.
    %used for tests.
    X = join(lists:last(T), H),
    linify2([H|T]) ++ [X].
linify2([_]) -> [];
linify2([A|[B|T]]) -> [join(A, B)|linify2([B|T])].
pointify(L) -> pointify2(L ++ [hd(L), hd(tl(L))]).
pointify2([_, _]) -> [];
pointify2([A, B, C|T]) ->
    T1 = make_trilateral(A, B, C),
    T2 = trilateral_to_triangle(T1),
    #triangle{x = Z} = T2,
    [Z|pointify2([B, C|T])].
concurrent(#sline{line = X}, #sline{line = Y},
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
            L2 = slope_sort(L),
            L4 = slope_sort(remove_excess_lines(L2)),
            L5 = slope_sort(remove_concurrents(L4)),
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
      {slope_sorted, slope_sort(L)},
      %{pointify, globe:gpsify(pointify(slope_sort(L)))},
      %{slope_sorted, slope_sort(remove_concurrents2(slope_sort(L)))},
      {slope_sorted, slope_sort(remove_concurrents(slope_sort(L)))},
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
      {slope_sorted, slope_sort(L)},
      %{pointified1, globe:gpsify(Ps)},
      %{pointified, globe:gpsify(pointify(lists:reverse(slope_sort(L))))},
      {pointified, globe:gpsify(pointify(slope_sort(L)))},
      {region, globe:gpsify(region(L))}
    }.
test7() ->
{{bad_lines,
     {{sline,{line,9895000,1391000,174367},false}}},
 {slope_sorted,
     [{sline,{line,-93000,2500,23361},false},
      {sline,{line,-280000,188750,260401},false},
      {sline,{line,241,-1367,1250},false},
      {sline,{line,-1036,-5877,104},false},
      {sline,{line,198750,272500,259687},false},
      {sline,{line,165000,65000,44399},false},
      {sline,{line,9895000,1391000,174367},false},
      {sline,{line,-1727500,197500,406601},true},
      {sline,{line,-367000,266000,293261},true},
      {sline,{line,1036,5877,104},true},
      {sline,{line,2540000,3750000,2927869},true},
      {sline,{line,3115000,1555000,814317},true},
      {sline,{line,24285000,6055000,435867},true}]},
 {slope_sorted,
     [{sline,{line,-93000,2500,23361},false},
      {sline,{line,-280000,188750,260401},false},
      {sline,{line,241,-1367,1250},false},
      {sline,{line,-1036,-5877,104},false},
      {sline,{line,198750,272500,259687},false},
      {sline,{line,165000,65000,44399},false},
      {sline,{line,-1727500,197500,406601},true},
      {sline,{line,-367000,266000,293261},true},
      {sline,{line,1036,5877,104},true},
      {sline,{line,2540000,3750000,2927869},true},
      {sline,{line,3115000,1555000,814317},true},
      {sline,{line,24285000,6055000,435867},true}]},
 {region,
     [{42.00378125178645,9.998415361247561},
      {43.00366316436866,7.998610516694492},
      {45.001205267428006,7.002065746177191},
      {47.001130335943564,8.001496416171403},
      {48.00051999413438,10.003187674182703},
      {47.000998541442726,12.001632366643221},
      {44.998848974037486,13.00234658179361},
      {43.00328334443247,12.003694177689724}]}}
        = test5(),
    {{slope_sorted,
      [{sline,{line,1727500,-197500,406601},false},
       {sline,{line,367000,-266000,293261},false},
       {sline,{line,-2540000,-3750000,2927869},false},
       {sline,{line,-3115000,-1555000,814317},false},
       {sline,{line,93000,-2500,23361},true},
       {sline,{line,280000,-188750,260401},true},
       {sline,{line,-1564,8867,10000},true},
       {sline,{line,-198750,-272500,259687},true},
       {sline,{line,-165000,-65000,44399},true}]},
     {pointified,
      [{-48.00051999413438,10.003187674182698},
       {-47.000998541442726,12.001632366643207},
       {-44.998848974037486,13.002346581793574},
      {-43.00328334443247,12.00369417768971},
       {22.49795443122679,-52.61017534869952},
       {-22.550136948296064,252.54002783719403},
       {-43.00366316436866,7.99861051669447},
       {-45.001205267428006,7.0020657461771805},
       {-47.001130335943564,8.001496416171392}]},
 {region,
  [{-48.00051999413438,10.003187674182698},
   {-47.000998541442726,12.001632366643207},
   {-44.998848974037486,13.002346581793574},
   {-43.00328334443247,12.00369417768971},
   {-42.00378125178645,9.998415361247567},
   {-43.00366316436866,7.99861051669447},
   {-45.001205267428006,7.0020657461771805},
   {-47.001130335943564,8.001496416171392}]}}
        = test6(),
    ok.
    
    
