const R = (function() {
    function seq_by(from, to, by = 1) {
    	let x = [];
      for (i = 0; Math.sign(by) * (from + i * by) <=  Math.sign(by) * to; i++) {
    		x.push(from + i * by);
    	}
    	return x;
    }

    function c() {
        var x = [];
        for (var i = 0; i < arguments.length; i++) {
            if (Array.isArray(arguments[i])) {
                for (var item of arguments[i]) {
                    x.push(item);
                }
            } else {
                x.push(arguments[i]);
            }
        }
        return x;
    }

    function matrix(x, nrow, ncol, byrow = false) {
        if (byrow) {
            return math.transpose(math.matrix(x).reshape([ncol, nrow]));
        } else {
            return math.matrix(x).reshape([nrow, ncol]);
        }
    }

    /*
    function list(key, value) {
        x = {};
        for (var ind = 0; ind < key.length; ind++) {
            x[key[ind]] = value[ind];
        }
        return x;
    }
    */

    function print(x) {
        if (x instanceof Array) {
            return printArray(x);
        }

        if (x instanceof math.Matrix) {
            return print(x._data);
        }

        if (x instanceof dfjs.DataFrame) {
            return x.show();
        }

        console.log(x);
    }


    function printArray(x) {
        function isNestedArray(x) { return Array.isArray(x[0]); }
        function addSpace(x)      { return "    " + x; }
        function paste(s1, s2)    { return String(s1) + String(s2); }

        if (isNestedArray(x)) {
            x.map(print);
        } else {
            let line = addSpace(x.reduce((s1, s2) => paste(s1, addSpace(s2))));
            console.log(line);
        }
    }


    // Basic R functions
    function length(x) { return x.length; }
    function map(x, f) { return x.map(f); }
    function reduce(x, f) { return x.reduce(f); }

    function runif(n, min = 0, max = 1) {
        var res = [];
        for (var i = 1; i <= n; i++) {
            res.push(min + Math.random() * (max - min));
        }
        return res;
    }

    // genericGroup
    function signif(x, n) {
        function signif0(x) {
            return(parseFloat(x.toPrecision(n)));
        }

        if (typeof(x) == "number") {
            return(signif0(x));
        }

        else if (x instanceof Array) {
            return(map(x, signif0));
        }

        else {
            throw "Currently only supports scalar and array.";
        }
    }

    function cospi(x) { return math.cos(math.multiply(x, math.pi)); }
    function sinpi(x) { return math.sin(math.multiply(x, math.pi)); }
    function tanpi(x) { return math.tan(math.multiply(x, math.pi)); }

    function range(x) { return [math.min(x), math.max(x)]; }

    function all(x) {
      for (let i of x) {
        if (!i) return(false);
      }
      return(true);
    }

    function any(x) {
      for (let i of x) {
        if (i) return(true);
      }
      return(false);
    }

    function lgamma(x) { return math.log(math.gamma(x)); }

    function lchoose(n, k) {
      return math.log(math.combination(n, k));
    }

    return {
        seq_by: seq_by,
        c: c,
        matrix: matrix,
        print: print,
        length: length,
        map: map,
        reduce: reduce,

        runif: runif,
        // genericGroup
        signif: signif,
        cospi: cospi,
        sinpi: sinpi,
        tanpi: tanpi,
        all: all,
        any: any,
        range: range,
        lgamma: lgamma,
        lchoose: lchoose
    };
})();
