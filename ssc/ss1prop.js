// SS1Prop
// Author: Wan Nor Arifin
function doCalculate() {
    p = document.SS1Prop.p.value;
    sd = Math.sqrt(p*(1-p));
    precision = document.SS1Prop.precision.value;
    ci = document.SS1Prop.ci.value/100;
    z = jStat.normal.inv(ci + (1 - ci)/2, 0, 1)    
    drop = document.SS1Prop.drop.value;
    n = Math.ceil( Math.pow(z * sd / precision, 2) );
    n_drop = Math.ceil( n / ((100 - drop) / 100) );
    document.SS1Prop.n.value = n;
    document.SS1Prop.n_drop.value = n_drop;
    document.getElementById("drop_").innerHTML = drop;
    return;
}
