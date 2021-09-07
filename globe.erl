-module(globe).
%distances, areas, and directions between points on earth.
-export([
         gps_to_point/1, point_to_gps/1,
         distance/2, area/3, seperation/2,
         region/1,
         test/0, test2/0, test3/0, test4/0, test5/0,
         test6/0
]).
-define(radius, 6371000). 
%-define(max, 4294967295).%2^32 - 1
-define(max, 10000).%useful for testing, so the numbers are small enough to be readable.
-record(spoint, {point, s}).
-record(sline, {line, s}).
-record(point, {x, y, z}).
-record(line, {x, y, z}).
-record(srat, {rat, s}).
-record(triangle, {x, y, z}).
point_to_gps(
  #spoint{point = #point{x = X, y = Y, z = Z}, 
          s = S}) ->
    A1 = math:sqrt((X*X) + (Y*Y)),
    AN = math:atan(A1/Z),
    Lat = ((math:pi()/2) - AN) * 180 / math:pi(),
    PNc = math:acos(-Y / A1),
    PN = if
             X < 0 -> -PNc;
             true -> PNc
         end,
    %PN = PNc,
    Long = PN * 180 / math:pi(),
    if
        S -> {Lat, Long};
        true -> {-Lat, Long+180}
    end.
gps_to_point({Lat0, Long0}) ->
    %P={x = X, y = Y, z = Z}
    {S, Lat, Long} = 
        if
            Lat0 < 0 -> {false, -Lat0, Long0+180};
            true -> {true, Lat0, Long0}
        end,
    Z = ?max,
    AN = (math:pi()/2) - 
        (Lat * math:pi() / 180),
    %tan(AN) = sqrt(X*X + Y*Y)/Z
    A1 = math:tan(AN)*Z,%sqrt(X*X + Y*Y)

    %radians from prime meridian
    PN = Long * math:pi() / 180,
    %sin(PN) = X / sqrt(X*X + Y*Y)
    %cos(PN) = -Y / sqrt(X*X + Y*Y)
    X_over_A1 = math:sin(PN),
    Y_over_A1 = -math:cos(PN),

    X = A1 * X_over_A1,
    Y = A1 * Y_over_A1,
    P = #point{
      x = round(X), 
      y = round(Y), 
      z = round(Z)},
    #spoint{point = simplify(P), s = S}.
gpsify(L) -> lists:map(fun point_to_gps/1, L).
simplify(L = #point{}) ->
    proj:dual(simplify(proj:dual(L)));
simplify(L) when is_record(L, line) ->
    X = L#line.x,
    Y = L#line.y,
    Z = L#line.z,
    if
        ((abs(X) > ?max) 
         or (abs(Y) > ?max)
         or (abs(Z) > ?max)) ->
            X2 = div2(X),
            Y2 = div2(Y),
            Z2 = div2(Z),
            simplify(proj:make_line(X2, Y2, Z2));
        true -> proj:make_line(X, Y, Z)
    end.
div2(X) ->
    case X of
        1 -> 1;
        -1 -> -1;
        X -> X div 2
    end.

distance(P1, P2) ->
    #srat{rat = R, s = S} = 
        spherical_trig:quadrance(P1, P2),
    {A1, A2} = trig:spread_to_angle(R),
    A3 = if
             S -> A1;
             true -> A2
         end,
    A3 * ?radius.
area(P1, P2, P3) ->
    T = #triangle{x = P1, y = P2, z = P3},
    A = spherical_trig:area(T),
    A * ?radius * ?radius.
area([P1, P2, P3]) -> area(P1, P2, P3);
area([H|T]) ->
    area2(H, T).
area2(_, [_]) -> 0;
area2(H, [A, B|R]) -> 
    area(H, A, B) + area2(H, [B|R]).
seperation(P1, P2) ->
    Dir = (spherical_trig:direction(P1, P2)),
    Dis = distance(P1, P2),
    Dir0 = if
               Dir > 180 ->
                   Dir - 360;
               true -> Dir
           end,
    {Dir0, Dis}.
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
    B = spherical_trig:same_hemisphere(New, Sp),
    {E2, C2} = if B -> {[Sp|E], C};
                  true -> {E, [Sp|C]} end,
    contains(New, Points, E2, C2).
remove_excess_lines([A, B, C|L]) ->
    remove_excess_lines2([A, B, C], L).
    %start with a trilateral, keep trying to add more lines constraining the space.
remove_excess_lines2(Lines, []) -> Lines;
remove_excess_lines2(Lines, T) ->
    Points = pointify(slope_sort(Lines)),
    {E, C} = contains(
               spherical_trig:dual(hd(T)), 
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
  when (length(Ps) < 2) ->
    Lines;
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
                rat:inverse(
                  rat:sub(One, Slope1))
        end,
    if
        S -> Slope2;
        true -> rat:inverse(rat:negative(Slope2))
    end.
linify([H|T]) ->
    %starts with point preceding first line.
    %used for tests.
    X = spherical_trig:join(lists:last(T), H),
     linify2([H|T]) ++ [X].
linify2([_]) -> [];
linify2([A|[B|T]]) -> 
    [spherical_trig:join(A, B)|
     linify2([B|T])].
pointify(L) ->
    pointify2(L ++ [hd(L), hd(tl(L))]).
pointify2([_, _]) -> [];
pointify2([A, B, C|T]) ->
    T1 = spherical_trig:make_trilateral(A, B, C),
    T2 = spherical_trig:trilateral_to_triangle(T1),
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
test() ->
    B = 1000000,
    S = 1,
    P1 = #spoint{point = proj:make_point(B, B, B),
                 s = true},
    P2 = #spoint{point = proj:make_point(B, B+S, B),
                 s = true},
    P3 = #spoint{point = proj:make_point(B+S, B, B),
                 s = false},
    {area(P1, P2, P3), distance(P1, P2), 
     distance(P2, P3), distance(P3, P1),
     seperation(P1, P3)}.
test2() ->
    Tokyo = gps_to_point({35.6762, 139.65}),
    Melbourne = gps_to_point({-37.8136, 144.96}),
    Sydney = gps_to_point({-33.9, 151}),
    Rio = gps_to_point({-22.9, -43.2}),
    SF = gps_to_point({37.7749, -122.4194}),
    LosAngeles = gps_to_point({34.0522, -118.2437}),
    %LosAngeles = gps_to_point({35.6762, -118.2437}),
    F = {
      (Tokyo),
      (Melbourne),
      %point_to_gps(SF),
      %point_to_gps(LosAngeles),
      seperation(LosAngeles, Tokyo),
      seperation(Tokyo, LosAngeles),
      seperation(LosAngeles, SF),
      seperation(SF, LosAngeles),
      seperation(Tokyo, Melbourne),
      seperation(Melbourne, Tokyo)
      %angles are good, but maybe I am adding the angle the wrong way in the southern hemisphere it is flipped over the y axis.

     }.
test3() ->    
%gps {north/south, east/west}
    P1 = gps_to_point({0.01,2}),
    P2 = gps_to_point({1, -0.01}),
    P3 = gps_to_point({2, 1}),
    P4 = gps_to_point({2, 3}),
    P5 = gps_to_point({1, 4}),
    L1 = spherical_trig:join(P1, P2),
    L2 = spherical_trig:join(P2, P3),
    L3 = spherical_trig:join(P3, P4),
    L4 = spherical_trig:join(P4, P5),
    L5 = spherical_trig:join(P5, P1),
    %L6 = spherical_trig:dual(P1#spoint{s = not(P3#spoint.s)}),
    L6 = spherical_trig:dual(P1),
    L9 = spherical_trig:dual(P2),
    L7 = spherical_trig:join(#spoint{point = #point{x = 1,y = 0,z = 100}, s = true}, P2),
    L7b = spherical_trig:join(P2, #spoint{point = #point{x = 1,y = 0,z = 100}, s = true}),
    io:fwrite("\n"),
    %L2 = slope_sort(L),
    L8 = spherical_trig:join(#spoint{point = #point{x = 1,y = 0,z = 100}, s = true}, P5),
    L8b = spherical_trig:join(P5, #spoint{point = #point{x = 1,y = 0,z = 100}, s = true}),
    %io:fwrite(concurrent(L7, L1, L2)),
    %io:fwrite({L7, L7b, slope(L7), slope(L7b), slope(L8), slope(L8b)}),
    L10 = L6#sline{s = not(L6#sline.s)},
    %La = [L1, L2, L3, L4, L5],
    La = [L2, L3, L4, L5, L1],
    Lb = [L2, L3, L4, L5, L1, L6],
    La2 = slope_sort(La),
    %La2 = slope_sort(remove_excess_lines(La2)),
    %Lb2 = slope_sort(La),
    io:fwrite("region 1\n"),
    %Lb2 = slope_sort(remove_excess_lines(slope_sort(Lb))),
    %io:fwrite(lists:map(fun(X) -> point_to_gps(X) end, pointify(slope_sort(La2)))),
    %io:fwrite(lists:map(fun(X) -> point_to_gps(X) end, pointify(La))),
    io:fwrite("region 2\n"),
    %io:fwrite({La2}),
    Ps = region(La),
    Ps = region(Lb),
    Ps = [P2, P3, P4, P5, P1],
    %Lb2 = slope_sort(Lb),
    %Ls2 = region([L4, L3, L5, L2, L1, L6]),
    %io:fwrite("region 2\n"),
    %Ls = region(La),
    %Ls = region([L4, L3, L5, L2, L1]),
    %Lsb = [P2, P3, P4, P5, P1],
    %io:fwrite({Ls, Lsb}),
    %io:fwrite("region 2\n"),
    %Ls = region(La),
    %io:fwrite("region 3\n"),
    %Ls = region([L4, L3, L5, L2, L1]),
    %Ls = region([L4, L3, L5, L2, L1, L6]),
    {
      %Ls,
      [P2, P3, P4, P5, P1],
      %area(Ls) / 1000000,
     area(P1, P2, P3)/ 1000000,
     area(P1, P3, P4)/ 1000000,
     area(P1, P4, P5)/ 1000000,
     distance(P1, P2)/1000}.
    
test4() ->
    P1 = gps_to_point({42, 10}),
    P2 = gps_to_point({43, 8}),
    P3 = gps_to_point({45, 7}),
    P4 = gps_to_point({47, 8}),
    P5 = gps_to_point({48, 10}),
    P6 = gps_to_point({47, 12}),
    P7 = gps_to_point({45, 13}),
    P8 = gps_to_point({43, 12}),
    Ps = [P1, P2, P3, P4,
          P5, P6, P7, P8],
    [L1, L2, L3, L4,
     L5, L6, L7, L8] = linify(Ps),

    UL1 = spherical_trig:dual(P1),
    UL2 = spherical_trig:dual(P2),
    UL3 = spherical_trig:dual(P3),
    UL4 = spherical_trig:dual(P4),
    
    UL1b = UL1#sline{s = not(UL1#sline.s)},
    UL0b = spherical_trig:dual(
             gps_to_point({1, -190})),
    UL0c = spherical_trig:dual(
             %gps_to_point({-85, 45})),
             gps_to_point({-1, -190})),
   
    NP = gps_to_point({89, -80}),%left of north pole
    NB1 = spherical_trig:join(P3, NP),%should include all.
    NB1b = spherical_trig:join(NP, P3),%should exclude all.
    false = (NB1 == NB1b),
    NB2 = spherical_trig:join(NP, P7),
    %L = [UL1, UL2, UL3, UL4, L1, L2, L3, L4, L5, L6, L7, L8],
    L = [L1, L2, L3, L4, L5, L6, L7, L8, %borders
         NB2, NB1, %tangent at a point
         %NB1b,
         UL0b, UL0c, UL1b],%external points that touch nowhere
    %L = [L1, L2, L3, L4, L5, L6, L7, L8],
    {
      {bad_lines, {NB1}},
      {slope_sorted, slope_sort(L)},
      %{pointify, gpsify(pointify(slope_sort(L)))},
      %{slope_sorted, slope_sort(remove_concurrents2(slope_sort(L)))},
      {slope_sorted, slope_sort(remove_concurrents(slope_sort(L)))},
      {region, gpsify(region(L))}
      %lists:map(fun(X) -> point_to_gps(X) end,
      %           region(L))}
      %linify(region(L))
    }.
   
test5() -> 
    P1 = gps_to_point({-48, 10}),
    P2 = gps_to_point({-47, 8}),
    P3 = gps_to_point({-45, 7}),
    P4 = gps_to_point({-43, 8}),
    P5 = gps_to_point({-42, 10}),
    P6 = gps_to_point({-43, 12}),
    P7 = gps_to_point({-45, 13}),
    P8 = gps_to_point({-47, 12}),
    Ps = [P1, P2, P3, P4,
          P5, P6, P7, P8],
    %draw a circle like test 4, but this time in the southern hemisphere.
    [L1, L2, L3, L4,
     L5, L6, L7, L8] = linify(Ps),
    UL1 = spherical_trig:dual(P1),
    UL2 = spherical_trig:dual(P2),
    UL3 = spherical_trig:dual(P3),
    UL4 = spherical_trig:dual(P4),
    UL1b = UL1#sline{s = not(UL1#sline.s)},
    L = linify(Ps) ++ [UL1b],
    {
      {slope_sorted, slope_sort(L)},
      %{pointified1, gpsify(Ps)},
      %{pointified, gpsify(pointify(lists:reverse(slope_sort(L))))},
      {pointified, gpsify(pointify(slope_sort(L)))},
      {region, gpsify(region(L))}
    }.
test6() ->
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
        = test4(),
    {42505372659149.01,3.0033172008537066,
     20015081.59412259,20015083.79270337,
     {120.0,20015083.79270337}} = test(),

    {{spoint,{point,4509,5307,5000},true},
     {spoint,{point,-3699,-5275,5000},false},
     {-53.92593122552037,8819267.671984639},
     {55.52594559032647,8819267.671984639},
     {-41.055113516686106,559093.0266066218},
     {136.4931427730047,559093.0266066218},
     {175.62804606867869,8190268.169845447},
     {-4.495628861091916,8190268.169845447}} 
        = test2(),
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
        = test5(),
    ok.
