// SSKappa
// Author: Wan Nor Arifin

// SSKappa_Hx
function doCalculate_hx() {
    // inputs
    k0 = document.SSKappa_Hx.k0.value;
    k1 = document.SSKappa_Hx.k1.value;
    p = document.SSKappa_Hx.p.value;
    alpha = document.SSKappa_Hx.alpha.value;
    power = document.SSKappa_Hx.power.value/100;
    drop = document.SSKappa_Hx.drop.value;
    //calculate
    z_alpha = jStat.normal.inv(1 - alpha/2, 0, 1);
    z_beta = jStat.normal.inv(power, 0, 1);
    ncp = (z_alpha + z_beta)**2;
    n = Math.ceil( ncp*( ((p*(1-p)*(k1-k0))**2 / (p**2+p*(1-p)*k0)) + (2*(p*(1-p)*(k1-k0))**2 / (p*(1-p)*(1-k0))) + ((p*(1-p)*(k1-k0))**2 / ((1-p)**2+p*(1-p)*k0)) )**(-1) );
    n_drop = Math.ceil( n / ((100 - drop) / 100) );
    //results
    document.SSKappa_Hx.n.value = n;
    document.SSKappa_Hx.n_drop.value = n_drop;
    document.getElementById("drop_").innerHTML = drop;
    return;
}
