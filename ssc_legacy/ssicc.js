// SSICC
// Author: Wan Nor Arifin

// SSICC_Hx
function doCalculate_hx() {
    // inputs
    icc0 = document.SSICC_Hx.icc0.value;
    icc1 = document.SSICC_Hx.icc1.value;
    alpha = document.SSICC_Hx.alpha.value;
    power = document.SSICC_Hx.power.value/100;
    rater = document.SSICC_Hx.rater.value;
    drop = document.SSICC_Hx.drop.value;
    //calculate
    z_alpha = jStat.normal.inv(1 - alpha/2, 0, 1);
    z_beta = jStat.normal.inv(power, 0, 1);
    theta0 = icc0/(1-icc0);
    theta = icc1/(1-icc1);
    C0 = (1 + rater*theta0)/(1 + rater*theta);
    n = Math.ceil( 1 + (2*((z_alpha + z_beta)**2)*rater) / (((Math.log(C0))**2)*(rater-1)) );
    n_drop = Math.ceil( n / ((100 - drop) / 100) );
    //results
    document.SSICC_Hx.n.value = n;
    document.SSICC_Hx.n_drop.value = n_drop;
    document.getElementById("drop_").innerHTML = drop;
    return;
}

// SSICC_Est
function doCalculate_est() {
    // inputs
    icc = document.SSICC_Est.icc.value;
    precision = document.SSICC_Est.precision.value;
    ci = document.SSICC_Est.ci.value/100;
    rater = document.SSICC_Est.rater.value;
    drop = document.SSICC_Est.drop.value;
    //calculate
    z = jStat.normal.inv(ci + (1 - ci)/2, 0, 1)
    w = precision*2
    n = Math.ceil( (8*(z**2)*((1-icc)**2)*((1+(rater-1)*icc)**2))/(rater*(rater-1)*w**2) + 1 );
    n_drop = Math.ceil( n / ((100 - drop) / 100) );
    //results
    document.SSICC_Est.n.value = n;
    document.SSICC_Est.n_drop.value = n_drop;
    document.getElementById("drop_").innerHTML = drop;
    return;
}
