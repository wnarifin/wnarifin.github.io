// SSCorr
// Author: Wan Nor Arifin

// SSCorr_Hx
function doCalculate_hx() {
    // inputs
    corr = Number(document.SSCorr_Hx.corr.value);
    alpha = document.SSCorr_Hx.alpha.value;
    power = document.SSCorr_Hx.power.value/100;
    drop = document.SSCorr_Hx.drop.value;
    //calculate
    z_alpha = jStat.normal.inv(1 - alpha/2, 0, 1);
    z_beta = jStat.normal.inv(power, 0, 1);
    u_corr_zero = 0.5 * Math.log((1+corr)/(1-corr));
    n_zero = Math.ceil( Math.pow(z_alpha + z_beta, 2)/Math.pow(u_corr_zero, 2) + 3);
    u_corr = 0.5 * Math.log((1+corr)/(1-corr)) + corr/(2*(n_zero-1));
    n = Math.ceil( Math.pow(z_alpha + z_beta, 2)/Math.pow(u_corr, 2) + 3);
    n_drop = Math.ceil( n / ((100 - drop) / 100) );
    //results
    document.SSCorr_Hx.n.value = n;
    document.SSCorr_Hx.n_drop.value = n_drop;
    document.getElementById("drop_hx_").innerHTML = drop;
    return;
}
// SSCorr_Est
function doCalculate_est() {
    // inputs
    corr = document.SSCorr_Est.corr.value;
    precision = document.SSCorr_Est.precision.value;   
    ci = document.SSCorr_Est.ci.value/100;
    drop = document.SSCorr_Est.drop.value;
    //calculate
    z = jStat.normal.inv(ci + (1 - ci)/2, 0, 1)
    n = 0; // unfinished
    n_drop = 0;  // unfinished
    //results
    document.SSCorr_Est.n.value = n;
    document.SSCorr_Est.n_drop.value = n_drop;
    document.getElementById("drop_est_").innerHTML = drop;
    return;
}
