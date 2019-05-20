// SS2Prop
// Author: Wan Nor Arifin
function doCalculate() {
    // inputs
    p0 = +document.SS2Prop.p0.value;
    p1 = +document.SS2Prop.p1.value;
    p_bar = (p0 + p1) / 2;
    alpha = document.SS2Prop.alpha.value;
    power = document.SS2Prop.power.value/100;
    drop = document.SS2Prop.drop.value;
    //calculate
    z_alpha = jStat.normal.inv(1 - alpha/2, 0, 1);
    z_beta = jStat.normal.inv(power, 0, 1);
    //n = p_bar;
    n_ = (z_alpha * Math.sqrt(2 * p_bar * (1 - p_bar)) + z_beta * Math.sqrt(p0 * (1 - p0) + p1 * (1 - p1)))**2 / (p0 - p1)**2;
    n = Math.ceil(n_);
    n_drop = Math.ceil( n / ((100 - drop) / 100) );
    // results
    document.SS2Prop.n.value = n;
    document.SS2Prop.n_drop.value = n_drop;
    document.getElementById("drop_").innerHTML = drop;
    return;
}
