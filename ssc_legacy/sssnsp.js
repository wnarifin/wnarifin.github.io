// SSSnSp
// Author: Wan Nor Arifin
function doCalculate() {
// input
    sn = document.SSSnSp.sn.value;
    sp = document.SSSnSp.sp.value;
    p = document.SSSnSp.p.value;
    precision = document.SSSnSp.precision.value;
    ci = document.SSSnSp.ci.value/100;
    z = jStat.normal.inv(ci + (1 - ci)/2, 0, 1)    
    drop = document.SSSnSp.drop.value;
// calculate
    n1 = Math.ceil( (z**2 * sn*(1-sn)/precision**2)/p );
    n2 = Math.ceil( (z**2 * sp*(1-sp)/precision**2)/(1-p) );
    n = Math.max(n1, n2);
    n_drop = Math.ceil( n / ((100 - drop) / 100) );
// return
    document.SSSnSp.n1.value = n1;
    document.SSSnSp.n2.value = n2;
    document.SSSnSp.n.value = n;
    document.SSSnSp.n_drop.value = n_drop;
    document.getElementById("drop_").innerHTML = drop;
    return;
}
