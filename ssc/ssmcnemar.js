// SSMcNemar
// Author: Wan Nor Arifin
function doCalculate() {
    // inputs
    p0 = +document.SSMcNemar.p0.value;
    p1 = +document.SSMcNemar.p1.value;
    p_discordant = p1*(1-p0) + p0*(1-p1);
    or = (p1*(1-p0)) / (p0*(1-p1));
    alpha = document.SSMcNemar.alpha.value;
    power = document.SSMcNemar.power.value/100;
    drop = document.SSMcNemar.drop.value;
    //calculate
    z_alpha = jStat.normal.inv(1 - alpha/2, 0, 1);
    z_beta = jStat.normal.inv(power, 0, 1);
    n_ = (z_alpha*(or+1) + z_beta*Math.sqrt((or+1)**2-((or-1)**2)*p_discordant))**2 / (((or-1)**2)*p_discordant);
    n = Math.ceil(n_);
    n_drop = Math.ceil( n / ((100 - drop) / 100) );
    // results
    document.SSMcNemar.n.value = n;
    document.SSMcNemar.n_drop.value = n_drop;
    document.getElementById("drop_").innerHTML = drop;
    return;
}
