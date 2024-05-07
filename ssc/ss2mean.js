// SS2Mean
// Author: Wan Nor Arifin
function doCalculate() {
    // inputs
    sd = document.SS2Mean.sd.value;
    diff = document.SS2Mean.diff.value;
    es = diff/sd;
    m = +document.SS2Mean.m.value; // add + to ensure numeric
    alpha = document.SS2Mean.alpha.value;
    power = document.SS2Mean.power.value/100;
    drop = document.SS2Mean.drop.value;
    //calculate
    z_alpha = jStat.normal.inv(1 - alpha/2, 0, 1);
    z_beta = jStat.normal.inv(power, 0, 1);
    n1 = (1 + m) / m * (z_alpha + z_beta)**2 / es**2 + z_alpha**2 / (2 * (1 + m));
    n1 = Math.ceil(n1);
    n0 = m * n1;
    n0 = Math.ceil(n0);
    n1_drop = Math.ceil( n1 / ((100 - drop) / 100) );
    n0_drop = Math.ceil( n0 / ((100 - drop) / 100) );
    // results
    document.SS2Mean.n1.value = n1;
    document.SS2Mean.n1_drop.value = n1_drop;
    document.SS2Mean.n0.value = n0;
    document.SS2Mean.n0_drop.value = n0_drop;
    document.getElementById("drop_1").innerHTML = drop;
    document.getElementById("drop_2").innerHTML = drop;
    return;
}
