// SS2Prop
// Author: Wan Nor Arifin
function doCalculate() {
    sel = document.getElementById("design").value;
    // inputs
    p0 = +document.SS2Prop.p0.value;
    p1 = +document.SS2Prop.p1.value;
    p_bar = (p0 + p1) / 2;
    if(sel=="cross-sectional") {
      p = +document.SS2Prop.m.value;
      m = (1 - p) / p;
    }
    else {
      m = +document.SS2Prop.m.value;
    }
    alpha = document.SS2Prop.alpha.value;
    power = document.SS2Prop.power.value/100;
    drop = document.SS2Prop.drop.value;
    //calculate
    z_alpha = jStat.normal.inv(1 - alpha/2, 0, 1);
    z_beta = jStat.normal.inv(power, 0, 1);
    n1 = (z_alpha * Math.sqrt((1 + m) * p_bar * (1 - p_bar)) + z_beta * Math.sqrt(m * p0 * (1 - p0) + p1 * (1 - p1)))**2 / (m * (p0 - p1)**2)
    n1 = Math.ceil(n1);
    n0 = m * n1;
    n0  = Math.ceil(n0);
    n1_drop = Math.ceil( n1 / ((100 - drop) / 100) );
    n0_drop = Math.ceil( n0 / ((100 - drop) / 100) );
    // results
    document.SS2Prop.n1.value = n1;
    document.SS2Prop.n1_drop.value = n1_drop;
    document.SS2Prop.n0.value = n0;
    document.SS2Prop.n0_drop.value = n0_drop;
    document.getElementById("drop_1").innerHTML = drop;
    document.getElementById("drop_2").innerHTML = drop;
    if(sel=="cross-sectional") {
      document.SS2Prop.n.value = n1 + n0;
      document.SS2Prop.n_drop.value = n1_drop + n0_drop;
      document.getElementById("drop_3").innerHTML = drop;
    }
    return;
}
