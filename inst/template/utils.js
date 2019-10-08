function seq_by(from, to, by = 1) {
	let x = [];
    for (i = 0; from + i * by <= to; i++) {
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

function list(key, value) {
    x = {};
    for (var ind = 0; ind < key.length; ind++) {
        x[key[ind]] = value[ind];
    }
    return x;
}


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
