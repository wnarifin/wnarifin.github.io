// SSAlpha
// Author: Wan Nor Arifin

// SSAlpha_Hx
function doCalculate_hx() {
    // inputs
    cronbach0_hx = document.SSAlpha_Hx.cronbach0_hx.value;
    cronbach1_hx = document.SSAlpha_Hx.cronbach1_hx.value;
    alpha_hx = document.SSAlpha_Hx.alpha_hx.value;
    power_hx = document.SSAlpha_Hx.power_hx.value/100;
    item_hx = document.SSAlpha_Hx.item_hx.value;
    drop_hx = document.SSAlpha_Hx.drop_hx.value;
    //calculate
    z_alpha = jStat.normal.inv(1 - alpha_hx/2, 0, 1);
    z_beta = jStat.normal.inv(power_hx, 0, 1);
    delta = (1-cronbach0_hx)/(1-cronbach1_hx);
    n_hx = Math.ceil( ( 2*item_hx/(item_hx-1) ) * Math.pow(z_alpha+z_beta, 2) / Math.pow(Math.log(delta), 2) + 2 );
    n_drop_hx = Math.ceil( n_hx / ((100 - drop_hx) / 100) );
    //results
    document.SSAlpha_Hx.n_hx.value = n_hx;
    document.SSAlpha_Hx.n_drop_hx.value = n_drop_hx;
    document.getElementById("drop_hx_").innerHTML = drop_hx;
    return;
}

// SSAlpha_Est
function doCalculate_est() {
    //inputs
    cronbach_est = document.SSAlpha_Est.cronbach_est.value;
    precision_est = document.SSAlpha_Est.precision_est.value;   
    ci_est = document.SSAlpha_Est.ci_est.value/100;
    item_est = document.SSAlpha_Est.item_est.value;
    drop_est = document.SSAlpha_Est.drop_est.value;
    //calculate
    z = jStat.normal.inv(ci_est + (1 - ci_est)/2, 0, 1)
    epsilon2 = precision_est*2;
    epsilon1 = (1 - (cronbach_est - precision_est) ) / (1 - (Number(cronbach_est) + Number(precision_est)) )
    n_zero = (8*item_est/(item_est-1)) * Math.pow(z/Math.log(epsilon1), 2) + 2;
    w_ul = 1 - Math.exp( Math.log(1-cronbach_est) + z*Math.sqrt( 2*item_est/((item_est-1)*(n_zero-2)) ) );
    w_ll = 1 - Math.exp( Math.log(1-cronbach_est) - z*Math.sqrt( 2*item_est/((item_est-1)*(n_zero-2)) ) );
    w_zero = w_ul - w_ll;
    n_est = Math.ceil( (n_zero - 2) * Math.pow(w_zero / epsilon2, 2) + 2 );
    n_drop_est = Math.ceil( n_est / ((100 - drop_est) / 100) );
    //results
    document.SSAlpha_Est.n_est.value = n_est;
    document.SSAlpha_Est.n_drop_est.value = n_drop_est;
    document.getElementById("drop_est_").innerHTML = drop_est;
    return;
}
