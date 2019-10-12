// R.js
// Random Variates
function runif(n, min = 0, max = 1) {
    var res = [];
    for (var i = 1; i <= n; i++) {
        res.push(min + Math.random() * (max - min));
    }
    return res;
}

function polar() {
    var u = 2*Math.random()-1;
    var v = 2*Math.random()-1;
    var r = u*u + v*v;
    while (r === 0 || r > 1) {
        u = 2*Math.random()-1;
        v = 2*Math.random()-1;
        r = u*u + v*v;
    }
    var c = Math.sqrt(-2*Math.log(r)/r);
    return [u*c, v*c];
}

function rnorm(n, mean = 0, sd = 1) {
    var res = [];
    for (var i = 0; i < n; i+=2) {
        var s = polar().map(x => mean + x * sd);
        res = res.concat(s);
    }
    if (n % 2 === 1) {
        res.pop();
    }
    return res;
}

function dnorm(x, mean = 0, sd = 1, log = false) {
    var sd2 = sd ** 2;
    if (!log) {
        return 1 / Math.sqrt(2 * Math.PI * sd2) * Math.exp(-((x - mean)**2) / (2 * sd2));
    } else {
        return -0.5 * Math.log(2 * Math.PI * sd2) - (x - mean)**2 / (2 * sd2);
    }
}

// Array
function c() {
    var x = [];
    for (var i = 0; i < arguments.length; i++) {
        x.push(arguments[i]);
    }
    return(x);
}

function seq(from, to, by, length_out) {
    if (by && length_out) {
        throw("Too many arguments. You can only specify one of the third and the fourth argument.");
    }
    var res = [];
    if (!to) {
        to = from;
        from = 1;
    }
    if (length_out) {
        by = (to - from) / (length_out - 1);
    }
    for (var i = from; i <= to; i = i + by) {
        res.push(i);
    }
    return res;
}

function rep(x, n) {
    var res = [];
    for (var i = 0; i < n; i++) {
        res.push(x);
    }
    return res;
}

function View(data0) {
    data0.map(x => console.log(x));
}

function head(x, n = 6) {
    if (n === 0) {
        return [];
    } else if (n < 0) {
        n = x.length + n;
    }
    var res = [];
    for (var i = 0; i < n; i++) {
        res.push(x[i]);
    }
    return res;
}

function tail(x, n = 6) {
    if (n === 0) {
        return [];
    } else if (n < 0) {
        n = x.length + n;
    }
    var res = [];
    for (var i = 0; i < n; i++) {
        res.push(x[x.length - n + i]);
    }
    return res;
}

function unique(x) {
    var y = new Set(x);
    return [...y];
}

function rev(x) {
    return x.reverse();
}

function rbind() {
    res = [];
    var l = arguments.length;
    for (var i = 0; i < l; i++) {
        res.push(arguments[i]);
    }
    return res;
}

function cbind(...args) {
    var res = rbind(...args);
    return t(res);
}

function t(data0) {
    // transpose
    var res = [];
    for (var i = 0; i < data0[0].length; i++) {
        res.push(data0.map(x => x[i]));
    }
    return res;
}

function sum(x) {
    var res = 0;
    for (var i = 0; i < x.length; i++) {
        res = res + x[i];
    }
    return res;
}

function map(x, f) {
    return x.map(f);
}

function map2(x, y, f) {
    var res = [];
    for (var i = 0; i < x.length; i++) {
        res.push(f(x[i], y[i]));
    }
    return res;
}

function range(x) {
    return [Math.min(...x), Math.max(...x)];
}

function diff(x) {
    res = [];
    for (i = 1; i < x.length; i++) {
        res.push(x[i] - x[i-1]);
    }
    return res;
}

function extendrange(x, f = 0.05) {
    var r = range(x);
    var ext = diff(r)[0] * f;
    return [r[0] - ext, r[1] + ext];
}

// Matrices
function matrix(x, n, m, byrow = false) {
    if (x.length != n * m) {
        throw("The number of elements provided doesn't match the matrix dimension desired.");
    }
    if (!byrow) { return t(matrix(x, m, n, true)); }
    var res = [];
    for (var i = 0; i < n; i++) {
        var ind = seq(i*m, (i+1)*m - 1);
        res.push(ind.map(l => x[l]));
    }
    return res;
}

function dim(m0) {
    var r = m0.length;
    var c = m0[0].length;
    return [r, c];
}

function nrow(m0) {
    return m0.length;
}

function ncol(m0) {
    return m0[0].length;
}

// List
row_view = function(data0) {
    // This function transposes a list, e.g.
    //   input: {x:[1,2,3], y:[4,5,6]}
    //   output: [{x:1, y:4}, {x:2, y:5}, {x:3, y:6}]
    var res = [];
    var obj_keys = Object.keys(data0);
    var data_len = data0[obj_keys[0]].length;
    var key_len = obj_keys.length;
    for (var j = 0; j < data_len; j++) {
        datum = {};
        for (var i = 0; i < key_len; i++) {
            var k = obj_keys[i];
            datum[k] = data0[k][j];
        }
        res.push(datum);
    }
    return res;
};

col_view = function(data0) {
    // This function transposes a list, e.g.
    //   input: [{x:1, y:4}, {x:2, y:5}, {x:3, y:6}]
    //   output: {x:[1,2,3], y:[4,5,6]}
    var res = {};
    var obj_keys = Object.keys(data0[0]);
    var data_len = data0.length;
    var key_len = obj_keys.length;
    for (var i = 0; i < key_len; i++) {
        var k = obj_keys[i];
        res[k] = [];
        for (var j = 0; j < data0.length; j++) {
            res[k].push(data0[j][k]);
        }
    }
    return res;
}

// Data.frame
filter = function(df0, pred_fun0) {
    return col_view(row_view(df0).filter(pred_fun0));
};
