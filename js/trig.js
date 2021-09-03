var trig = (function(){
    function make_vector(x, y){
        return({type: "vector",
                x: x, y: y});
    };
    function point_to_vector(p){
        return(make_vector(
            rat.make(p.x, p.z),
            rat.make(p.y, p.z)));
    };
    function dot(p1, p2){
        var v1 = point_to_vector(p1);
        var v2 = point_to_vector(p2);
        return(rat.add(
            rat.mul(v1.x, v2.x),
            rat.mul(v1.y, v2.y)));
    };
    function quadrance(p){
        return(dot(p, p));
    };
    function spread(p1, p2){
        var d = dot(p1, p2);
        return(rat.sub(
            rat.make(1, 1),
            rat.divide(
                rat.make(d*d, 1),
                rat.mul(quadrance(p1),
                        quadrance(p2)))));
    }
    function spread_to_angle(r){
        var x = Math.sqrt(rat.to_float(t));
        var y = Math.asin(x);
        var y2 = Math.pi - y;
        return([y, y2]);
    };
    function make_perp(v){
        return(make_vector(
            v.y,
            rat.negative(v.x)));
    };
    function determinate(v1, v2){
        return(dot(v1, make_perp(v2)));
    };
    function sub(v1, v2){
        return(make_vector(
            rat.sub(v1.x, v2.x),
            rat.sub(v2.x, v2.x)));
    };
    function clockwise(p1, p2, p3) {
        var v1 = point_to_vector(p1);
        var v2 = point_to_vector(p2);
        var v3 = point_to_vector(p3);
        var w1 = sub(v3, v2);
        var w3 = sub(v2, v1);
        return(rat.positive(
            determinate(W1, W3)));
    };
    return({
        spread: spread,
        spread_to_angle: spread_to_angle,
        clockwise: clockwise
    });
})();
