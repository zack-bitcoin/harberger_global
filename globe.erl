-module(globe).
%distances, areas, and directions between points on earth.
-export([
         gps_to_point/1, point_to_gps/1,
         distance/2, area/3, seperation/2,
         region/1,
         test/0, test2/0, test3/0, test4/0, test5/0
]).
-define(radius, 6371000). 
%-define(max, 4294967295).
-define(max, 10000).%useful for testing, so the numbers are small enough to be readable.

-record(spoint, {point, s}).
-record(sline, {line, s}).
-record(point, {x, y, z}).
-record(line, {x, y, z}).
-record(srat, {rat, s}).
-record(triangle, {x, y, z}).
point_to_gps(#spoint{
                point = #point{x = X, 
                               y = Y, z = Z}, 
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
same_hemispheres([], _) -> [];
same_hemispheres([H|T], P) ->
    B = spherical_trig:same_hemisphere(H, P),
    [{B, H}|same_hemispheres(T, P)].
all_enclosed([]) -> true;
all_enclosed([{true, _}|R]) -> 
    all_enclosed(R);
all_enclosed([{false, _}|_]) -> 
    false.
       
adjacent_lines(NewLine, [NewLine|T]) ->
    {lists:last(T), hd(T)};
    %{hd(T), lists:last(T)};
adjacent_lines(Newline, [A|T]) ->
    adjacent_lines(Newline, T ++ [A]).
 
adjacent_points(NewLine, Lines) ->
    Lines2 = slope_sort([NewLine|Lines]),
    {L1, L2} = adjacent_lines(NewLine, Lines2),
    %P1 = spherical_trig:meet(L1, NewLine),
    %P2 = spherical_trig:meet(NewLine, L2),
    P1 = spherical_trig:meet(NewLine, L1),
    P2 = spherical_trig:meet(L2, NewLine),
    {P1, P2}.
    
 
region(L) ->
    %L2 = lists:reverse(slope_sort(L)),
    if
        length(L) < 3 -> empty_region;
        true ->
            L2 = slope_sort(L),
            L3 = remove_excess_lines(L2),
            L4 = slope_sort(L3),
            L5 = slope_sort(remove_concurrents(L4)),
            if
                (L5 == L4) -> new_pointify(L4);
                true -> region(L5)
            end
    end.
    
    %looking at the intersections, we need to keep turning left. If we ever turn right, then remove the _previous_ line.
        %pointify(L4).
%region2(L3).

%todo: if a region has no space in it, we need to realize this and say so.


slope_sort(L) ->
    lists:sort(fun(A, B) ->
                       rat:less_than(
                         slope(B), slope(A))
               end, L).
contains(_, [], C, E) -> 
    {lists:reverse(C), 
     lists:reverse(E)};
contains(New = #spoint{}, 
         [Sp = #spoint{}|Points],
         C, E) ->
    B = spherical_trig:same_hemisphere(New, Sp),
    if
        B -> contains(New, Points, [Sp|C], E);
        true -> contains(New, Points, C, [Sp|E])
    end.
%intermediate_lines([_]) -> [];
%intermediate_lines([A, B|T]) -> 
%    L = spherical_trig:join(A, B),
%    [L|intermediate_lines([B|T])].
remove_excess_lines([A, B, C|L]) ->
    Trilateral = spherical_trig:make_trilateral(
                   A, B, C),
    Triangle = spherical_trig:trilateral_to_triangle(Trilateral),
    #triangle{x = X, y = Y, z = Z} = Triangle,
    remove_excess_lines2([A, B, C], [X, Y, Z], L).
    %start with a trilateral, keep trying to add more lines.
    %if a new line leaves 1 point out, then add that line, and replace the one point with 2 points.
    %if a new line leaves 2 or more points out, then remove those lines and points, and add the 1 new line and 2 new points.
remove_excess_lines2(Lines, Points, []) ->
    %io:fwrite(Lines, gpsify(Points)),
    Lines;
remove_excess_lines2(Lines, _Points, [New|T]) ->
    io:fwrite("many lines "),
    io:fwrite(integer_to_list(length(Lines))),
    io:fwrite("\n"),
    Points = new_pointify(slope_sort(Lines)),
    {C, E} = contains(
    %{E, C} = contains(
               spherical_trig:dual(New), 
               Points, [], []),
    Lc = length(E),
    Lp = length(Points),
    if
        (E == []) -> 
            io:fwrite("contains no points\n"),
            %io:fwrite({spherical_trig:dual(New), 
            %           Points});
            {error, empty_region};
        (Lp == Lc) ->
    %If a new line contains all existing points, then we should drop that line.
            io:fwrite("contains all points \n"),
            remove_excess_lines2(Lines, Points, T);
        (Lp == (Lc+1)) ->
    %if a new line leaves 1 point out, then add that line, and replace the one point with 2 points.
            io:fwrite("contains all but one points \n"),
            {P1, P2} = adjacent_points(New, Lines),
            %io:fwrite({E, C}),
            %[L1, L2] = passes_through(Lines, hd(E)),
            %P1 = spherical_trig:meet(L1, New),
            %P2 = spherical_trig:meet(New, L2),
            remove_excess_lines2(
              [New|Lines], [P1, P2|E], T);
        (Lp > Lc) ->
    %if a new line leaves 2 or more points out, then remove those lines and points, and add the 1 new line and 2 new points.
           io:fwrite("excludes more than one\n"),
            Lines2 = remove_if_touch_2(Lines, C),
            {P1, P2} = adjacent_points(New, Lines2),
            remove_excess_lines2(
              [New|Lines2], [P1, P2|E], T)
    end.

passes_through([], _) ->
    [];
passes_through([SL = #spoint{point = P}|T], 
               E = #sline{line = L}) ->
    B = proj:incident(L, P),
    if
        B -> [SL|passes_through(T, E)];
        true -> passes_through(T, E)
    end;
passes_through([SL = #sline{line = L}|T], 
               E = #spoint{point = P}) ->
    B = proj:incident(L, P),
    if
        B -> [SL|passes_through(T, E)];
        true -> passes_through(T, E)
    end.

remove_if_touch_2([], Ps) -> [];
remove_if_touch_2([L|T], Ps) -> 
    M = passes_through(Ps, L),
    case M of
        [] -> [L|remove_if_touch_2(T, Ps)];
        [_] -> [L|remove_if_touch_2(T, Ps)];
        %[_,_] -> 
        _ -> 
            %io:fwrite({M, L, Ps}),
            remove_if_touch_2(T, Ps)
    end.
            
            
   
%remove_element([X|T], X) -> T;
%remove_element([A|T], X) -> 
%    [A|remove_element(T, X)].
                                     


%lines_minus([], _) -> [];
%lines_minus([H|T], R) -> 
%    B = is_in(H, R),
%    if
%        B -> lines_minus(T, R);
%        true -> [H|lines_minus(T, R)]
%    end.
%is_in(X, [X|_]) -> true;
%is_in(X, [_|T]) -> is_in(X, T);
%is_in(_, []) -> false.
            
    
            
    
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
%region2([A, B, C, D|T]) ->
%    region_helper([spherical_trig:meet(B, A)], 
%                  [B, C, D|T], A).
%region_helper(Ps, [L], A) ->
%    [spherical_trig:meet(A, L)] ++ Ps;
%region_helper(Ps, [L1, L2|L], A) ->
%    region_helper(
%      Ps ++ [spherical_trig:meet(L2, L1)],
%      [L2|L], A).

linify([H|T]) ->
    %starts with point preceding first line.
    %used for tests.
    X = spherical_trig:join(lists:last(T), H),
     linify2([H|T]) ++ [X].
linify2([_]) -> [];
linify2([A|[B|T]]) -> 
    [spherical_trig:join(A, B)|
     linify2([B|T])].

new_pointify(L) ->
    new_pointify2(L ++ [hd(L), hd(tl(L))]).
new_pointify2([_, _]) -> [];
new_pointify2([A, B, C|T]) ->
    io:fwrite("new_pointify2\n"),
    T1 = spherical_trig:make_trilateral(A, B, C),
    T2 = spherical_trig:trilateral_to_triangle(T1),
    #triangle{x = Z} = T2,
    [Z|new_pointify2([B, C|T])].
    

     
    

%pointify([H|T]) ->
    %starts with point preceding first line.
%    [spherical_trig:meet(H, lists:last(T))|
    %[spherical_trig:meet(lists:last(T), H)|
%     pointify2([H|T])].
%pointify2([_]) -> [];
%pointify2([A|[B|T]]) -> 
%    [spherical_trig:meet(B, A)|
    %[spherical_trig:meet(A, B)|
%     pointify2([B|T])].
%pointifyr([H|T]) ->%todo. this version works for the southern hemisphere. we need to figure out how to combine the 2 versions so it always works. 
    %if we are going clockwise, then just always turn right. 
    %starts with point preceding first line.
    %[spherical_trig:meet(H, lists:last(T))|
%    [spherical_trig:meet(lists:last(T), H)|
%     pointifyr2([H|T])].
%pointifyr2([_]) -> [];
%pointifyr2([A|[B|T]]) -> 
    %[spherical_trig:meet(B, A)|
%    [spherical_trig:meet(A, B)|
%     pointifyr2([B|T])].
%turnify([A|[B|T]]) ->
%    T2 = lists:reverse([A|[B|T]]),
%    Last = turnify2(hd(tl(T2)), hd(T2), A),

%    [turnify2(hd(T2), A, B)|
%     turnify3([A|[B|T]])] ++
%        [Last].
    %[turnify2(hd(tl(T2)), hd(T2), A),
    % turnify2(hd(T2), A, B)|
    % turnify3([A|[B|T]])].
%direction_vector(#spoint{point = P, s = S}) ->
%    V1 = trig:point_to_vector(P),
%    if
%        S -> V1;
%        true -> trig:negative(V1)
%    end.
%turnify2(A, B, C) ->
%    Tri = spherical_trig:make_triangle(A, B, C),
%    spherical_trig:clockwise(Tri).
%    V1 = direction_vector(A),
%    V2 = direction_vector(B),
%    V3 = direction_vector(C),
%    Va = trig:sub(V2, V1),
%    Vb = trig:sub(V3, V2),
%    not(rat:positive(trig:determinate(Va, Vb))).
    
%turnify3([_, _])  -> [];
%turnify3([A, B, C|T]) -> 
%    [turnify2(A, B, C)|
%     turnify3([B, C|T])].
gpsify([]) -> [];
gpsify([H|T]) -> 
    [point_to_gps(H)|
     gpsify(T)].
    

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
concurrent(#sline{line = X},
           #sline{line = Y},
           #sline{line = Z}) ->
    proj:concurrent(X, Y, Z).
remove_concurrents(L) when length(L) > 2 ->
    RL = lists:reverse(L),
    B1 = concurrent(
          hd(RL), hd(L), hd(tl(L))),
    B2 = concurrent(
           hd(tl(RL)), hd(RL), hd(L)),
    if
        B1 -> 
            io:fwrite("remove concurrent1\n"),
            remove_concurrents(tl(L));
        B2 -> 
            io:fwrite("remove concurrent2\n"),
            %io:fwrite({spherical_trig:meet(hd(RL), hd(L)), 
            %           lists:map(fun(X) -> {X, slope(X)} end, RL)}),
            remove_concurrents(
                lists:reverse(tl(RL)));
        true -> remove_concurrents2(L)
    end;
remove_concurrents(L) -> L.

remove_concurrents2([A, B, C|T]) ->
    Bool = concurrent(A, B, C),
    if
        Bool -> 
            io:fwrite("remove concurrent3\n"),
            remove_concurrents2([A, C|T]);
        true -> [A|remove_concurrents2([B, C|T])]
    end;
remove_concurrents2([A, B]) -> [A, B].

            
    
%remove_bad_turns(L) ->
%    Sl0 = slope_sort(L),
%    Sl = remove_concurrents(Sl0),
    %Ts = turnify(pointify(Sl)),
    %remove_bad_turns2(
    %  Sl, [lists:last(Ts)] ++ Ts, Sl, []).
%remove_bad_turns2([], _, _, A) -> 
%    slope_sort(A);
%remove_bad_turns2([L|Lt], [R1|[R2|Rt]], Sl, A) -> 
    %B = rat:positive(R),
%    Sl2 = remove_element(Sl, L),
%    if
%        (not(R1) and not(R2)) -> 
            %remove_bad_turns2(Lt, Rt, Sl);
            %io:fwrite(L),
%            remove_bad_turns(Sl2);
%        true -> remove_bad_turns2(
%                  Lt, [R2|Rt], Sl, [L|A])
%    end.
            
    
test3() ->    
%gps {north/south, east/west}
    P1 = gps_to_point({0.01,2}),
    P2 = gps_to_point({1, -0.01}),
    P3 = gps_to_point({2, 1}),
    P4 = gps_to_point({2, 3}),%instead of this, 2 bads.
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
      {slope_sorted, slope_sort(remove_concurrents2(slope_sort(L)))},
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
      {pointified, gpsify(new_pointify(slope_sort(L)))},
      {region, gpsify(region(L))}
    }.
