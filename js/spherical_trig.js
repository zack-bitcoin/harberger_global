var spherical_trig = (function(){

    function join(p1, p2){
        var s3 = (p1.x * p2.y) > (p2.x * p1.y);
        return({type: "sline",
                line: proj.join(p1.point,
                                p2.point),
                s: p1.s ^ p2.s ^ s3});
    };
    function triangle_to_trilateral(tri){
        return({type: "trilateral",
                x: join(tri.y, tri.z),
                y: join(tri.z, tri.x),
                z: join(tri.x, tri.y)});
    };
    function dual(a){
        var t = a.type;
        if(t === "trilateral"){
            return({type: "triangle",
                    x: dual(a.x),
                    y: dual(a.y),
                    z: dual(a.z)});
        } else if(t === "sline"){
            return({type: "spoint",
                    point: proj.dual(a.line)});
        } else {
            console.log("unsupported dual");
        };
    };
    function spreads(tri){
        return(quadrances(
            dual(triangle_to_trilateral(tri))));
    };
    function angles(tri){
        return(spreads_to_angles(
            spreads(tri)));
    };
    function spread_to_angle(srat){
        var [a1, a2] =
            tri.spread_to_angle(srat.rat);
        if(srat.s){
            return(a1);
        } else {
            return(a2);
        };
    };
    function spreads_to_angles(l){
        return(l.map(spread_to_angle));
    };
    function dot(p1, p2){
        return((p1.x * p2.x) +
               (p1.y * p2.y) +
               (p1.z * p2.z));
    };
    function same_side_is_small(sp1, sp2){
        var d = dot(sp1.point, sp2.point);
        var s = sp1.s ^ sp2.s;
        if(s === 0){
            d = -d;
        };
        return(d > 0);
    };
    function quadrance(sp1, sp2){
        var rat = trig.spread(
            sp1.point, sp2.point);
        var s = same_side_is_small(sp1, sp2);
        return({type: "srat",
                rat: rat,
                s: s});
    };
    function quadrances(tri){
        return([quadrance(tri.y, tri.z),
                quadrance(tri.z, tri.x),
                quadrance(tri.y, tri.x)]);
    };
    function planar_area(l){
        //list of 3 distances
        var [d1, d2, d3] = l;
        var s = (d1 + d2 + d3) / 2;
        var area =
            s * (s - d1) * (s - d2) * (s - d3);
        return(Math.sqrt(Math.max(0, area)));
    };
    function area(tri){
        var [a1, a2, a3] = angles(tri);
        var area1 = a1 + a2 + a3 - Math.pi;
        var q2 = spreads_to_angles(quadrances(t));
        var area2 = planar_area(q2);
        if(area1 < 0.00001){
            return(area2);
        } else {
            return(area1);
        }
    };
    function direction(sp1, sp2){
        var np = proj.make_point(0,0,1);
        var north = {type: "spoint",
                     point: np,
                     s: 0};
        var t = {type: "triangle",
                 x: sp1, y: north, z: sp2};
        var angle = spread_to_angle(spreads(t)[0]);
        var a2 = angle * 180 / Math.pi;
        var clockwise = trig.clockwise(
            sp1.point, np, sp2.point);
        var s = clockwise ^ sp1.s ^ sp2.s;
        if(s) {
            return(a2);
        } else {
            return(360 - a2);
        };
    };
    return({
        area: area,
        quadrance: quadrance,
        direction: direction
    });
})();
