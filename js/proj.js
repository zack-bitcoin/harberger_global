var proj = (function(){
    function make_point(x, y, z){
        var p ={type: "point",
                x: x,
                y: y,
                z: z};
        return(simplify(p));
    };
    function make_line(x, y, z){
        var l = {type: "line",
                 x: x,
                 y: y,
                 z: z};
        return(simplify(l));
    };
    function gcd2(a, b){
        if(b === 0){
            return(a);
        } else {
            return(gcd2(b, a % b));
        };
    };
    function gcd(a, b){
        if((a === 0) &&
           (b === 0)){
            return(0);
        } else if(a === 0){
            return(b);
        } else if(b === 0){
            return(a);
        } else if(b > a){
            return(gcd2(b, a));
        } else {
            return(gcd2(a, b));
        };
    };
    function gcd3(a, b, c){
        var g = gcd(gcd(a, b), c);
        if((c*g)<0){
            return(-g);
        } else {
            return(g);
        };
    };
    function dual(a) {
        var t = a.type;
        if(t === "line"){
            var y = JSON.parse(JSON.stringify(a));
            y.type = "point";
            return(y);
        } else if(t === "point"){
            var y = JSON.parse(JSON.stringify(a));
            y.type = "line";
            return(y);
        } else if(t === "triangle"){
            return(make_trilateral(
                dual(a.x), dual(a.y), dual(a.z)));
        } else if(t === "trilateral"){
            return(make_triangle(
                dual(a.x), dual(a.y),
                dual(a.z)));
        } else {
            console.log(JSON.stringify(a));
            console.log("dual: impossible error");
        };
    };
    function make_triangle(x, y, z) {
        return({type: "triangle",
                x: x, y: y, z: z});
    };
    function make_trilateral(x, y, z){
        return({type: "trilateral",
                x: x, y: y, z: z});
    };
    function meet(l1, l2){
        return(make_point(
            (l1.y * l2.z) - (l2.y * l1.z),
            (l1.z * l2.x) - (l2.z * l1.x),
            (l1.x * l2.y) - (l2.x * l1.y)));
    };
    function join(p1, p2){
        return(dual(meet(dual(p1), dual(p2))));
    };
    function triangle_to_trilateral(tri){
        return(make_trilateral(
            join(tri.y, tri.z),
            join(tri.z, tri.x),
            join(tri.x, tri.y)));
    };
    function trilateral_to_triangle(tri){
        return(dual(triangle_to_trilateral(
            dual(tri))));
    };
    return({
        make_point: make_point,
        make_line: make_line,
        dual: dual,
        make_triangle: make_triangle,
        make_trilateral: make_trilateral,
        triangle_to_trilateral: triangle_to_trilateral,
        trilateral_to_triangle: trilateral_to_triangle,
        join: join
    });
})();
