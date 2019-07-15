// SS2MeanPaired & SSSDDiff
// Author: Wan Nor Arifin

// SS2MeanPaired
function doCalculate_hx() {
    // inputs
    sd = document.SS2MeanPaired.sd.value;
    diff = document.SS2MeanPaired.diff.value;
    alpha = document.SS2MeanPaired.alpha.value;
    power = document.SS2MeanPaired.power.value/100;
    drop = document.SS2MeanPaired.drop.value;
    //calculate
    z_alpha = jStat.normal.inv(1 - alpha/2, 0, 1);
    z_beta = jStat.normal.inv(power, 0, 1);
    n = Math.ceil( (sd**2 * (z_alpha + z_beta)**2) / diff**2 );
    n_drop = Math.ceil( n / ((100 - drop) / 100) );
    // results
    document.SS2MeanPaired.n.value = n;
    document.SS2MeanPaired.n_drop.value = n_drop;
    document.getElementById("drop_").innerHTML = drop;
    return;
}

// SSSDDiff
function doCalculate_sd() {
    // inputs
    sd_pre = document.SSSDDiff.sd_pre.value;
    sd_post = document.SSSDDiff.sd_post.value;
    r_pre_post = document.SSSDDiff.r_pre_post.value;
    //calculate
    var_d = sd_pre**2 + sd_post**2 - 2*r_pre_post*sd_pre*sd_post;
    sd_d = Math.sqrt(var_d);
    // results
    document.SSSDDiff.sd_d.value = sd_d.toFixed(3);
    return;
}
