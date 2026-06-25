-module(spherical_trig).
-export([
         join/2, dual/1, area/1,
         quadrance/2, direction/2, region/1,
         flip_hemisphere/1,
	 det_area/1, linify/1, area2/1, too_squished/2,
         test/1, test2/0, test3/0, test4/0,
         test5/0, test6/0, test8/0]).

-record(spoint, {point, s}).%3 integers, and a boolean, for which hemisphere the point is in. s is true if the point is in the northern hemisphere.
-record(sline, {line, s}).
-record(point, {x, y, z}).%3 integers.
-record(line, {x, y, z}).%3 integers.
-record(triangle, {x, y, z}).%3 (s)points
-record(trilateral, {x, y, z}).%3 spoints
-record(srat, {rat, s}).%rat is a rational. s is for whether we are talking about the big or small angle. true indicates the short distance.
-record(rat, {t, b}).
-define(bits32, 4294967296). %32
-define(bits64, 18446744073709551616).
-define(bits128, 340282366920938463463374607431768211456).

%s is for whether this indicates an quadrance that crosses the equator, or a lune that contains a north or south pole.

make_trilateral(X, Y, Z) ->
    #trilateral{x = X, y = Y, z = Z}.
make_triangle(X, Y, Z) ->
    #triangle{x = X, y = Y, z = Z}.
            
quadrance(P1, P2) ->
    SBig = same_hemisphere(P1, P2),
    Rat = trig:spread(P1, P2),
    #srat{rat = Rat, s = SBig}.
same_hemisphere(P1, P2) ->
    dot(P1, P2) > 0.
dot(#point{x = X1, y = Y1, z = Z1}, 
    #point{x = X2, y = Y2, z = Z2}) ->
    (X1 * X2) + (Y1 * Y2) + (Z1 * Z2);
dot(P1 = #line{}, P2 = #line{}) ->
    dot(dproj:dual(P1), dproj:dual(P2)).
%quadrances(L) when is_list(L) ->
%    quadrances_list(L++[hd(L)]);
quadrances(#triangle{x = X, y = Y, z = Z}) ->
    [quadrance(Y, Z),
     quadrance(Z, X),
     quadrance(Y, X)].
%quadrances_list([A, B]) ->
%    [quadrance(A, B)];
%quadrances_list([A, B|T]) ->
%    [quadrance(A, B)|quadrances_list([B|T])].
quadrances_all_pairs([_]) -> [];
quadrances_all_pairs([H|T]) -> 
    quadrances_all_pairs2(H, T) ++
	quadrances_all_pairs(T).
quadrances_all_pairs2(_, []) -> [];
quadrances_all_pairs2(X, [H|T]) -> 
    [quadrance(X, H)|
     quadrances_all_pairs2(X, T)].
dual(#trilateral{x = L1, y = L2, z = L3}) ->
    #triangle{x = dproj:dual(L1), 
              y = dproj:dual(L2), 
              z = dproj:dual(L3)}.
join(P1, P2) -> 
    dproj:join(P1, P2).
meet(L1, L2) ->
    dproj:meet(L1, L2).
flip_hemisphere(#point{x = X, y = Y, z = Z}) ->
    #point{x = -X, y = -Y, z = -Z};
flip_hemisphere(#line{x = X, y = Y, z = Z}) ->
    #line{x = -X, y = -Y, z = -Z};
flip_hemisphere(#trilateral{
                   x = X, y = Y, z = Z}) ->
    #trilateral{x = flip_hemisphere(X),
                y = flip_hemisphere(Y),
                z = flip_hemisphere(Z)}.
clockwise(T = #triangle{x = X, y = Y, z = Z}) ->
    true
        xor (X#point.z > 0) 
        xor (Y#point.z > 0) 
        xor (Z#point.z > 0) 
        xor trig:clockwise(X, Y, Z).
triangle_to_trilateral(Tri = #triangle{
                         x = X, y = Y, z = Z}) ->
    Clockwise = clockwise(Tri),
    T2 = make_trilateral(
           join(Y, Z),join(Z, X),join(X, Y)),
    if
        (Clockwise) -> 
            flip_hemisphere(T2);
        true -> 
	    T2
    end.
trilateral_to_triangle(T = #trilateral{}) ->
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
    %Area = area(Tri),
    Area = area:area(Tri),
    A2 = if
             S -> Area;
             true -> rat:negative(Area)
         end,
    rat:est_simplify(rat:add(A2, area2([A, C|T])), ?bits32);
area2([_, _]) -> {rat, 0, 1}.
det_area(#triangle{x = X, y = Y, z = Z}) ->
    1=2, %use area:area instead.
    SS = rat:est_simplify(trig:solid_spread(X, Y, Z), ?bits128),
    %io:fwrite({SS, rat:to_float(SS)}),
    SR = rat:est_simplify(trig:det_sqrt(SS, 2), ?bits64), 
    %SR.
    %io:fwrite({SS, SR, rat:to_float(SS), rat:to_float(SR)}),
    rat:divide(SR, 2).
area(T = #triangle{}) ->
    [A1C, A2C, A3C] = angles(T),
    Area1 = A1C + A2C + A3C - (math:pi()),
    Q2 = spreads_to_angles(quadrances(T)),
    Area2 = planar_area(Q2),
    if
        (Area1 < 0.00001) -> Area2;
        true -> Area1
    end;
area(L) -> area2(L).

old_det_area(T = #triangle{x = X, y = Y, z = Z}) ->
    [Q1, Q2, Q3] = quadrances(T),
    A1 = trig:spread_to_angle(Q1#srat.rat),
    A2 = trig:spread_to_angle(Q2#srat.rat),
    A3 = trig:det_spread_to_angle(Q3#srat.rat),

    %L1 = trig:det_sqrt(Q1#srat.rat),
    %L2 = trig:det_sqrt(Q2#srat.rat),
    %L3 = trig:det_sqrt(Q3#srat.rat),
    %L1 = point_distance(X, Y),
    %L2 = point_distance(Y, Z),
    %L3 = point_distance(Z, X),
    S = rat:divide(rat:add(rat:add(A1, A2), A3), 2),
    Area2 = rat:mul(
	      rat:mul(S, rat:sub(S, A1)),
	      rat:mul(rat:sub(S, A2), rat:sub(S, A3))),
    rat:est_simplify(Area2, ?bits32).
    %Area3 = rat:mul(Area2, 1000000),%convert to square cm.
    %(Area3#rat.t div Area3#rat.b).
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
direction(P1, P2) ->
    NP = dproj:make_point(0,0,1),
    T = #triangle{x = P1, y = NP, z = P2},
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
contains(New = #point{}, 
         [Sp = #point{}|Points],
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
    Points = pointify(Lines),
    {E, C} = contains(dproj:dual(hd(T)), 
                      Points, [], []),
    if
        (C == []) -> {error, empty_region};
        (E == []) ->
   %If a new line contains all existing points, then we should drop that line.
            remove_excess_lines2(Lines, tl(T));
        (length(E) == 1) ->
            Lines2 = rotate_to_meet(Lines, hd(E), length(Lines)),
            remove_excess_lines2([hd(T)|Lines2], tl(T));
        true -> 
            Lines2 = remove_if_touch_2(Lines, E),
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
rotate_to_unlisted([A, B], L, Limit) ->
    [A, B];
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
  when (length(Ps) < 2) -> Lines;
remove_if_touch_2(Lines, Ps) ->
    lists:filter(
      fun(L) -> 
              length(
                lists:filter(
                  fun(P) -> 
                          dproj:incident(L, P) 
                  end, Ps)) < 2
      end, 
      Lines).
max_quadrance(L) ->
    max_quadrance2(L, {rat, 0, 1}).
max_quadrance2([], M) -> M;
max_quadrance2([{srat, H, _}|T], M) -> 
    B = rat:less_than(M, H),
    if
	B -> max_quadrance2(T, H);
	true -> max_quadrance2(T, M)
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
concurrent(X, Y, Z) ->
    dproj:concurrent(X, Y, Z).
remove_concurrents([A, B, C|T]) -> 
    Bool = concurrent(A, B, C),
    if
	Bool -> "removed concurrent\n";
	true -> ok
    end,
    case {Bool, T} of
        {true, []} -> [];
        {_, []} -> [A, B, C];
        {true, _} -> remove_concurrents([A, C|T]);
        _ -> [A|remove_concurrents([B, C|T])]
    end.
%region operations
region(L) ->
    %given a list of lines, return the points at the corner of the enclosed region.
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

too_squished(Area, Region) ->
    Qs = quadrances_all_pairs(Region),
    Q = max_quadrance(Qs),
    rat:less_than(rat:mul(16, Area), Q).
%Area * 16 < rat:to_float(Q).
    
    

test() ->
    B = 1000,
    P1 = #point{x = 0, y = -B, z = B},
    P2 = #point{x = 0-1, y = -B - 5, z = B},
    P3 = #point{x = 0+1, y = -B-5, z = B},
    %Tokyo = #spoint{point = #point{x = 45091, y = 53075, z = 50000}, s = true},
    %Mel = #spoint{point = #point{x = -36991, y = -52750, z = 50000}, s = false},
    Tokyo = #point{x = 4509, y = 5307, z = 5000},
    Mel = #point{x = 3699, y = 5275, z = -5000},
    Sydney = #point{x = 3607, y = 6508, z = -5000},
    T = #triangle{x = P1, y = P2, z = P3},
    NP = dproj:make_point(0,0,1),
    %North = #spoint{point = NP, s = true},
    North = NP,
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
    P1 = #point{x = B, y = -B+1, z = B},
    P2 = #point{x = B, y = -B-1, z = B},
    P3 = #point{x = B+1, y = -B-1, z = B},
    Trib = make_triangle(P1, P3, P2),
    Trib2 = trilateral_to_triangle(
             triangle_to_trilateral(Trib)),
    Tri = make_triangle(P1, P2, P3),
    Tri2 = trilateral_to_triangle(
             triangle_to_trilateral(Tri)),
    Tri = Tri2,
    Trib = Trib2,

    { 
      join(P1, P2),
     join(P2, P1),
     join(P1, P3),
     join(P3,P1)
    }.
    
test3() ->    
    Trilat = {trilateral,
               {line,8953,-156,3},
               {line,0,312,-8947},
               {line,-8953,-156,3}
             },
    Trilat = triangle_to_trilateral(
               trilateral_to_triangle(
                 Trilat)).
test4() ->
    B = 10000,
    F = 1,
    P1 = #point{x = B, y = B+F, z = B},
    P2 = #point{x = B+F, y = B, z = B},
    P3 = #point{x = B, y = B, z = B+F},
    Tri = make_triangle(P1, P2, P3),%1/8th of globe.
    Tril = triangle_to_trilateral(Tri),
    Tri = trilateral_to_triangle(Tril),
    {Tri,
     Tril,
     dual(Tril),
     spreads(Tri),
     angles(Tri),
     area(Tri)}.
   
point_fix(#spoint{point = P, s = S}) -> 
    if
        S -> P;
        true -> flip_hemisphere(P)
    end.
point_break(P = #point{z = Z}) ->
    {P2, S} = if
            Z < 0 -> {flip_hemisphere(P),
                      false};
            true -> {P, true}
        end,
    #spoint{point = P2, s = S}.
            
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

    UL1 = dproj:dual(P1),
    UL2 = dproj:dual(P2),
    UL3 = dproj:dual(P3),
    UL4 = dproj:dual(P4),
    
    %UL1b = UL1#sline{s = not(UL1#sline.s)},
    %UL1b = UL1#line{z = -UL1#line.z},
    UL1b = flip_hemisphere(UL1),
    UL0b = dproj:dual(globe:gps_to_point({1, -190})),
    UL0c = dproj:dual(globe:gps_to_point({-1, -190})),
   
    NP = globe:gps_to_point({89, -80}),%left of north pole
    NB1 = join(P3, NP),%should include all.
    NB1b = join(NP, P3),%should exclude all.
    false = (NB1 == NB1b),
    NB2 = join(NP, P7),
    %L = [UL1, UL2, UL3, UL4, L1, L2, L3, L4, L5, L6, L7, L8],
    L = [L1, L2, L3, L4, L5, L6, L7, L8, %borders
         UL1b,
         NB2, NB1 %tangent at a point
         %UL1b %{line,241,-1367,-1250}]},
         %NB1b,
         %UL0b, UL0c, UL1b
        ],%external points that touch nowhere
    %L = [L1, L2, L3, L4, L5, L6, L7, L8],
    {
      %{bad_lines, {NB1}},
      same_hemisphere(P1, P2),
      {points, Ps},
      %{points, pointify(linify(Ps))},
      %{points, linify(pointify(L))},
      %{lines, L},
      %{remove_excess, remove_excess_lines(L)},
      %{remove_concurrents, remove_concurrents(
      %                       remove_excess_lines(L))},
      %{remove_excess, pointify(remove_excess_lines(L))},
      %{remove_concurrents, pointify(
      %                       remove_concurrents(
      %                         remove_excess_lines(L)))},
      {region, region(L)}
      %{region, globe:gpsify(
     %            lists:map(
     %              fun(X) -> point_break(X)
     %              end, region(L)))}
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
    UL1 = dproj:dual(P1),
    UL2 = dproj:dual(P2),
    UL3 = dproj:dual(P3),
    UL4 = dproj:dual(P4),
    UL1b = flip_hemisphere(UL1),%UL1#sline{s = not(UL1#sline.s)},
    L = linify(Ps),% ++ [UL1b],
    {
      %same_hemisphere(P1, P2)
      %{pointified1, globe:gpsify(Ps)},
      %{pointified, globe:gpsify(pointify(L))},
      {points, Ps},
      %{pointified, pointify(L)},
      %{remove_excess, pointify(remove_excess_lines(L))},
      {region, region(L)}
    }.
test8() ->    
    %looks like it fails if any point is on the lines x=0 or y=0 or z=0
    XD = 0,
    YD = -10,

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
      remove_excess_lines(lists:reverse(L)),%todo. missing 2 lines.
      %remove_concurrents(remove_excess_lines(L)),
      %Ps,
      Region,
      Region2,
      %pointify(L),
      %{points, globe:gpsify(Ps)},
      %{pointified, globe:gpsify(pointify(L))},
      %{region, globe:gpsify(region(L))},
      {area_points, area(Ps)},
      %{region, area(pointify(L))},
      {region, area(Region)},
      {region, area(Region2)}
    }.
   
test(1) -> 
    A = 63710000,%radius*10
    %A = 10,
    B = 1,
    %10-cm x 10-cm
    P1 = #point{x = A+B, y = A, z = A},
    P2 = #point{x = A, y = A+B, z = A},
    P3 = #point{x = A, y = A, z = A+B},
    T = #triangle{x = P1, y = P2, z = P3},
    DA = area:area(T),
    {%rat:to_float(det_area(T)),
     area(T),
     DA,
     rat:to_float(DA),
     T};
test(2) -> 
    %test to see if a region is too squished to be valid.
    A = 32768,
    B = 1,
    Stretch = 0,
    P1 = #point{x = A+3, y = A, z = A},
    P2 = #point{x = A+2, y = A+2, z = A},
    %P2 = #point{x = 2+A, y = 2+A, z = A},
    P3 = #point{x = A, y = A+3, z = A},
    P4 = #point{x = A-2, y = A+2, z = A},
    %P4 = #point{x = -2+A, y = 2+A, z = A},
    P5 = #point{x = A-3, y = A, z = A},
    P6 = #point{x = A-2, y = A-2, z = A},
    %P6 = #point{x = -2+A, y = -2+A, z = A},
    P7 = #point{x = A, y = A-3, z = A},
    P8 = #point{x = A+2, y = A-2, z = A},
    %P8 = #point{x = 2+A, y = -2+A, z = A},
    Ps = [P1, P2, P3, P4, P5, P6, P7, P8],
    Ls = linify([P1, P2, P3, P4]),
    R = region(Ls),
    io:fwrite({Ps, R}),
    %io:fwrite(R),
    %find the 2 points with the biggest spread.
    % must be true: (D^2)/20 < Area
    %io:fwrite({Area}),
    %Area = rat:rabs(area2(Ps)),
    Area = rat:est_simplify(rat:rabs(area2(Ps)), ?bits32),
    Times = 10000,
    T1 = erlang:timestamp(),
    doit_times(Times, fun() ->
			      Area = rat:est_simplify(rat:rabs(area2(Ps)), ?bits32),
			      false = too_squished(Area, Ps)
	      end),
    T2 = erlang:timestamp(),
    %Qs = quadrances_all_pairs(R),
    %Q = max_quadrance(Qs),
    %true = (Area * 16 > rat:to_float(Q)),
    %{Area/rat:to_float(Q), Qs, R}.
    {verify, timer:now_diff(T2, T1) / Times, "millionths of a second per calculation.", Area}.
%success.
doit_times(0, _) -> ok;
doit_times(N, F) -> 
    F(),
    doit_times(N-1, F).
