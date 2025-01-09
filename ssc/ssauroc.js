// SSAUROC
// Author: Wan Nor Arifin
// Last update: 9/1/25

// SSAUROC_Hx
function doCalculate_hx() {
    // inputs
    A0 = +document.SSAUROC_Hx.A0.value; // + ensures number is detected as number
    A = +document.SSAUROC_Hx.A.value;
    p = +document.SSAUROC_Hx.p.value;
    alpha = document.SSAUROC_Hx.alpha.value;
    power = document.SSAUROC_Hx.power.value/100;
    drop = document.SSAUROC_Hx.drop.value;
    //calculate
    one = new Decimal(1); // use decimal.js
    z_alpha = jStat.normal.inv(1 - alpha/2, 0, 1);
    z_beta = jStat.normal.inv(power, 0, 1);
    R = ((1 - p) / p >= 1) ? (1 - p) / p : p / (one.minus(p)); // ifelse, use decimal.js for p / 1 - p d/t issue with floating pt number
    var_A = A * (1 - A); // eq 6.6 in Zhou's Statistical methods in diagnostic medicine
    var_A0 = A0 * (1 - A0); 
    n_ = Math.ceil( ( z_alpha * Math.sqrt(var_A0) + z_beta * Math.sqrt(var_A))**2 / (A0 - A)**2 ); // eq 6.8 in Zhou's Statistical methods in diagnostic medicine
    n = Math.ceil( n_ * (1 + R) );
    n_drop = Math.ceil( n / ((100 - drop) / 100) );
    //results
    document.SSAUROC_Hx.n.value = n;
    document.SSAUROC_Hx.n_drop.value = n_drop;
    document.getElementById("drop_hx_").innerHTML = drop;
    return;
}
// SSAUROC_Est
function doCalculate_est() {
    // inputs
    A = +document.SSAUROC_Est.A.value;
    p = +document.SSAUROC_Est.p.value;
    precision = +document.SSAUROC_Est.precision.value;   
    ci = document.SSAUROC_Est.ci.value/100;
    drop = document.SSAUROC_Est.drop.value;
    //calculate
    one = new Decimal(1); // use decimal.js
    z = jStat.normal.inv(ci + (1 - ci)/2, 0, 1)
    R = ((1 - p) / p >= 1) ? (1 - p) / p : p / (one.minus(p)); // ifelse, use decimal.js for p / 1 - p d/t issue with floating pt number
    var_A = A * (1 - A); // eq 6.6 in Zhou's Statistical methods in diagnostic medicine
    n_ = Math.ceil( ( z * Math.sqrt(var_A) )**2 / precision**2 ); // eq 6.2 in Zhou's Statistical methods in diagnostic medicine
    n = Math.ceil( n_ * (1 + R) );
    n_drop = Math.ceil( n / ((100 - drop) / 100) );
    //results
    document.SSAUROC_Est.n.value = n;
    document.SSAUROC_Est.n_drop.value = n_drop;
    document.getElementById("drop_est_").innerHTML = drop;
    return;
}
