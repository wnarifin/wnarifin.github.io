// SSRMSEA
// Author: Wan Nor Arifin
// AI Help: Gemini 3.1 Pro

function ssrmseaCalculate() {
    var rmsea = parseFloat(document.SSRMSEA1.rmsea.value);
    var alpha = parseFloat(document.SSRMSEA1.alpha.value);
    var power = parseFloat(document.SSRMSEA1.power.value) / 100;
    var df = parseInt(document.SSRMSEA1.df.value);
    var drop = parseFloat(document.SSRMSEA1.drop.value);

    var delta = ncp(alpha, power, df);
    var N_e = (delta / (Math.pow(rmsea, 2) * df)) + 1;

    var n = Math.ceil(N_e);
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    document.SSRMSEA1.n.value = n;
    document.SSRMSEA1.n_drop.value = n_drop;
    document.getElementById("drop_").innerHTML = drop;
}
