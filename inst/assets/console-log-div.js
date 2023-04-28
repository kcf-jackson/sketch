/*
Source: https://github.com/bahmutov/console-log-div
License: MIT License

Copyright (c) 2015 Gleb Bahmutov

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

(function initConsoleLogDiv() {
    'use strict';


    if (console.log.toDiv) {
        return;
    }

    function toString(x) {
        if (x instanceof Error) {
            return x.message;
        }
        return typeof x === 'string' ? x : JSON.stringify(x);
    }

    var log = console.log.bind(console);
    var error = console.error.bind(console);
    var warn = console.warn.bind(console);
    var table = console.table ? console.table.bind(console) : null;
    var id = 'console-log-div';

    function createOuterElement() {
        var outer = document.getElementById(id);
        if (!outer) {
            outer = document.createElement('fieldset');
            outer.id = id;
            document.body.appendChild(outer);
        }
        outer.classList.add(id);

        var style = outer.style;
        style.width = '95%';
        // style.minHeight = '200px';
        style.fontFamily = 'monospace';
        style.marginTop = '20px';
        style.marginBottom = '20px';  // Modified (CFK)
        style.marginLeft = '10px';
        style.marginRight = '10px';
        style.whiteSpace = 'pre';
        style.border = '1px solid black';
        style.borderRadius = '5px';
        style.padding = '5px 10px';
        return outer;
    }

    var logTo = (function createLogDiv() {

        var outer = createOuterElement();

        var caption = document.createTextNode('console output');
        var legend = document.createElement('legend');
        legend.appendChild(caption);
        outer.appendChild(legend);

        var div = document.createElement('div');
        div.id = 'console-log-text';
        outer.appendChild(div);

        return div;
    }());

    function printToDiv() {
        var msg = Array.prototype.slice.call(arguments, 0)
        .map(toString)
        .join(' ');
        var text = logTo.textContent;
        logTo.textContent = text + msg + '\n';
    }

    function logWithCopy() {
        log.apply(null, arguments);
        printToDiv.apply(null, arguments);
    }

    console.log = logWithCopy;
    console.log.toDiv = true;

    console.error = function errorWithCopy() {
        error.apply(null, arguments);
        var args = Array.prototype.slice.call(arguments, 0);
        args.unshift('ERROR:');
        printToDiv.apply(null, args);
    };

    console.warn = function logWarning() {
        warn.apply(null, arguments);
        var args = Array.prototype.slice.call(arguments, 0);
        args.unshift('WARNING:');
        printToDiv.apply(null, args);
    };

    function printTable(objArr, keys) {

        var numCols = keys.length;
        var len = objArr.length;
        var $table = document.createElement('table');
        $table.style.width = '100%';
        $table.setAttribute('border', '1');
        var $head = document.createElement('thead');
        var $tdata = document.createElement('td');
        $tdata.innerHTML = 'Index';
        $head.appendChild($tdata);

        for (var k = 0; k < numCols; k++) {
            $tdata = document.createElement('td');
            $tdata.innerHTML = keys[k];
            $head.appendChild($tdata);
        }
        $table.appendChild($head);

        for (var i = 0; i < len; i++) {
            var $line = document.createElement('tr');
            $tdata = document.createElement('td');
            $tdata.innerHTML = i;
            $line.appendChild($tdata);

            for (var j = 0; j < numCols; j++) {
                $tdata = document.createElement('td');
                $tdata.innerHTML = objArr[i][keys[j]];
                $line.appendChild($tdata);
            }
            $table.appendChild($line);
        }
        var div = document.getElementById('console-log-text');
        div.appendChild($table);
    }

    console.table = function logTable() {
        if (typeof table === 'function') {
            table.apply(null, arguments);
        }

        var objArr = arguments[0];
        var keys;

        if (typeof objArr[0] !== 'undefined') {
            keys = Object.keys(objArr[0]);
        }
        printTable(objArr, keys);
    };

    window.addEventListener('error', function (err) {
        printToDiv( 'EXCEPTION:', err.message + '\n  ' + err.filename, err.lineno + ':' + err.colno);
    });

}());
