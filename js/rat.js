var rat = (function(){
    function make(t, b) {
        return({type: "rat",
                t: t,
                b: b});
    };
    function to_float(rat) {
        return(rat.t / rat.b);
    };
    function zero(rat) {
        return(rat.t === 0);
    };
    function positive(rat) {
        return((rat.t * rat.b) > 0);
    };
    function equal(r1, r2){
        return((r1.t * r2.b) ===
               (r2.t * r1.b));
    };
    function gcf(x, y){
        if(Math.abs(y) > Math.abs(x)){
            return(gcf(y, x));
        } else if(y ===0){
            return(x);
        } else {
            return(gcf(y, x % y));
        }
    };
    function simplify(rat){
        var g = gcf(rat.t, rat.b);
        return(make(rat.t/g, rat.b/g));
    };
    function mul(a, b){
        return(simplify(make(
            a.t * b.t,
            a.b * b.b)));
    };
    function mul3(a, b, c){
        return(mul(mul(a, b),
                   c));
    };
    function sub(a, b){
        return(simplify(make(
            ((a.t * b.b) - (b.t * a.b)),
            a.b * b.b)));
    };
    function add(a, b){
        return(sub(a, negative(b)));
    };
    function add3(a, b, c){
        return(add(add(a, b)
                   c));
    };
    function inverse(rat){
        return(make(rat.b, rat.t));
    };
    function divide(a, b){
        return(mul(a, inverse(b)));
    };
    function negative(rat){
        return(make(-rat.t, rat.b));
    };
    function less_than(a, b){
        return((a.t * b.b) <
               (b.t * a.b));
    };

    return({
        make: make,
        to_float: to_float,
        zero: zero,
        positive: positive,
        mul: mul,
        mul3: mul3,
        add: add,
        add3: add3,
        sub: sub,
        divide: divide,
        negative: negative,
        less_than: less_than
    });
})();
