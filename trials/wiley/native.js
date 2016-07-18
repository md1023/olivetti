// getSums with reduce
(function () {
    arr = [1, 2, 3, 4, 5];
    var qqq = [];
    for (var i=0; i < arr.length; i++) {
        qqq.push(
            arr.slice(0, i + 1).reduce(function(p, c) {
                return p + c;
            })
        );
    }
    console.log(qqq);
})();


// getSums without reduce
(function () {
    var qqq = [arr[0]];
    for (var i=1; i < arr.length; i++) {
        qqq.push(arr[i] + qqq[qqq.length - 1]);
    }
    console.log(qqq);
})();


// ArmyShooters
(function () {
    arr = [1, 2, 3, 4, 5];
    var list = [];
    for (var i=0; i < arr.length; i++) {
        list.push(
            (function (j) {
                return function() {
                    return j;
                };
            })(i)
        );
    }
    list.forEach(function(f) { console.log(f()); });
})();


// replace simple for with while
// for (var i = 0; i < 3; i++) {
//     console.log( "номер " + i + "!" );
// }
(function () {
    var i = 0;
    while (++i < 4) {
        console.log( "номер " + (i - 1) + "!" );
    }
})();
