// SS2Mean
// Author: Wan Nor Arifin
function doCalculate() {
    // inputs
    sd = document.SS2Mean.sd.value;
    diff = document.SS2Mean.diff.value;
    alpha = document.SS2Mean.alpha.value;
    power = document.SS2Mean.power.value/100;
    drop = document.SS2Mean.drop.value;
    //calculate
    z_alpha = jStat.normal.inv(1 - alpha/2, 0, 1);
    z_beta = jStat.normal.inv(power, 0, 1);
    n = Math.ceil( (2 * sd**2 * (z_alpha + z_beta)**2) / diff**2 );
    n_drop = Math.ceil( n / ((100 - drop) / 100) );
    // results
    document.SS2Mean.n.value = n;
    document.SS2Mean.n_drop.value = n_drop;
    document.getElementById("drop_").innerHTML = drop;
    return;
}
