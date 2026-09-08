// SS1Mean
// Author: Wan Nor Arifin
function doCalculate() {
    sd = document.SS1Mean.sd.value;
    precision = document.SS1Mean.precision.value;
    ci = document.SS1Mean.ci.value/100;
    z = jStat.normal.inv(ci + (1 - ci)/2, 0, 1)    
    drop = document.SS1Mean.drop.value;
    n = Math.ceil( Math.pow(z * sd / precision, 2) );
    n_drop = Math.ceil( n / ((100 - drop) / 100) );
    document.SS1Mean.n.value = n;
    document.SS1Mean.n_drop.value = n_drop;
    document.getElementById("drop_").innerHTML = drop;
    return;
}
