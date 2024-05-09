// SS2MeanRM
// Author: Wan Nor Arifin
function doCalculate() {
    // inputs
    sd = document.SS2MeanRM.sd.value;
    diff = document.SS2MeanRM.diff.value;
    es = diff/sd;
    r = +document.SS2MeanRM.r.value; // repetitions, add + to ensure numeric
    base = +document.SS2MeanRM.base.value;  // 1 = yes, 0 = no
    rho = +document.SS2MeanRM.rho.value; // correlation between repetitions, 0.6 - 0.75 common (Machin et al, 2009)
    alpha = document.SS2MeanRM.alpha.value;
    power = document.SS2MeanRM.power.value/100;
    drop = document.SS2MeanRM.drop.value;
    //calculate
    z_alpha = jStat.normal.inv(1 - alpha/2, 0, 1);
    z_beta = jStat.normal.inv(power, 0, 1);
    if (base == 1) {
      r = r - 1; // minus 1 baseline
      R = (1 + (r - 1) * rho) / r - rho**2; // fix v = 1
    }
    else {
      R = (1 + (r - 1) * rho) / r;
    }
    n = R * ( (2 * (z_alpha + z_beta)**2 / es**2) + (z_alpha**2 / 4) );
    n = Math.ceil(n);
    n_drop = Math.ceil( n / ((100 - drop) / 100) );
    // results
    document.SS2MeanRM.n.value = n;
    document.SS2MeanRM.n_drop.value = n_drop;
    document.getElementById("drop_").innerHTML = drop;
    return;
}
