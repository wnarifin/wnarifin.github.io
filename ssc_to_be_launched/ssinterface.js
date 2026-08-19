/*
===================================== 
ssinterface.js
Author: Wan Nor Arifin
github: github.com/wnarifin/
updated date: 2026-08-04

Interface code for html display control
===================================== 
*/

function doCalculate_ss1mean() {
    var sd = document.SS1Mean.sd.value;
    var precision = document.SS1Mean.precision.value;
    var ci = document.SS1Mean.ci.value / 100;
    var drop = document.SS1Mean.drop.value;

    var res = calc_ss1mean(sd, precision, ci, drop);

    document.SS1Mean.n.value = res.n;
    document.SS1Mean.n_drop.value = res.n_drop;
    document.getElementById("drop_").innerHTML = drop;
}

function doCalculate_ss1prop() {
    var p = document.SS1Prop.p.value;
    var precision = document.SS1Prop.precision.value;
    var ci = document.SS1Prop.ci.value / 100;
    var drop = document.SS1Prop.drop.value;

    var res = calc_ss1prop(p, precision, ci, drop);

    document.SS1Prop.n.value = res.n;
    document.SS1Prop.n_drop.value = res.n_drop;
    document.getElementById("drop_").innerHTML = drop;
}

function doCalculate_ss2mean() {
    var sd = document.SS2Mean.sd.value;
    var diff = document.SS2Mean.diff.value;
    var m = +document.SS2Mean.m.value;
    var alpha = document.SS2Mean.alpha.value;
    var power = document.SS2Mean.power.value / 100;
    var drop = document.SS2Mean.drop.value;

    var res = calc_ss2mean(sd, diff, m, alpha, power, drop);

    document.SS2Mean.n1.value = res.n1;
    document.SS2Mean.n1_drop.value = res.n1_drop;
    document.SS2Mean.n0.value = res.n0;
    document.SS2Mean.n0_drop.value = res.n0_drop;
    document.getElementById("drop_1").innerHTML = drop;
    document.getElementById("drop_2").innerHTML = drop;
}

function doCalculate_hx_ss2mean_paired() {
    var sd = document.SS2MeanPaired.sd.value;
    var diff = document.SS2MeanPaired.diff.value;
    var alpha = document.SS2MeanPaired.alpha.value;
    var power = document.SS2MeanPaired.power.value / 100;
    var drop = document.SS2MeanPaired.drop.value;

    var res = calc_hx_ss2mean_paired(sd, diff, alpha, power, drop);

    document.SS2MeanPaired.n.value = res.n;
    document.SS2MeanPaired.n_drop.value = res.n_drop;
    document.getElementById("drop_").innerHTML = drop;
}

function doCalculate_sd_ss2mean_paired() {
    var sd_pre = document.SSSDDiff.sd_pre.value;
    var sd_post = document.SSSDDiff.sd_post.value;
    var r_pre_post = document.SSSDDiff.r_pre_post.value;

    var res = calc_sd_ss2mean_paired(sd_pre, sd_post, r_pre_post);

    document.SSSDDiff.sd_d.value = res.sd_d;
}

function doCalculate_ss2mean_ratio1() {
    var sd = document.SS2Mean.sd.value;
    var diff = document.SS2Mean.diff.value;
    var alpha = document.SS2Mean.alpha.value;
    var power = document.SS2Mean.power.value / 100;
    var drop = document.SS2Mean.drop.value;

    var res = calc_ss2mean_ratio1(sd, diff, alpha, power, drop);

    document.SS2Mean.n.value = res.n;
    document.SS2Mean.n_drop.value = res.n_drop;
    document.getElementById("drop_").innerHTML = drop;
}

function doCalculate_ss2mean_rm() {
    var sd = document.SS2MeanRM.sd.value;
    var diff = document.SS2MeanRM.diff.value;
    var r = +document.SS2MeanRM.r.value;
    var base = +document.SS2MeanRM.base.value;
    var rho = +document.SS2MeanRM.rho.value;
    var alpha = document.SS2MeanRM.alpha.value;
    var power = document.SS2MeanRM.power.value / 100;
    var drop = document.SS2MeanRM.drop.value;

    var res = calc_ss2mean_rm(sd, diff, r, base, rho, alpha, power, drop);

    document.SS2MeanRM.n.value = res.n;
    document.SS2MeanRM.n_drop.value = res.n_drop;
    document.getElementById("drop_").innerHTML = drop;
}

function doCalculate_ss2prop() {
    var sel = document.getElementById("design").value;
    var p0 = +document.SS2Prop.p0.value;
    var p1 = +document.SS2Prop.p1.value;
    var m = (sel == "cross-sectional") ? (1 - (+document.SS2Prop.m.value)) / (+document.SS2Prop.m.value) : +document.SS2Prop.m.value;
    var alpha = document.SS2Prop.alpha.value;
    var power = document.SS2Prop.power.value / 100;
    var drop = document.SS2Prop.drop.value;

    var res = calc_ss2prop(p0, p1, m, alpha, power, drop);

    document.SS2Prop.n1.value = res.n1;
    document.SS2Prop.n1_drop.value = res.n1_drop;
    document.SS2Prop.n0.value = res.n0;
    document.SS2Prop.n0_drop.value = res.n0_drop;
    document.getElementById("drop_1").innerHTML = drop;
    document.getElementById("drop_2").innerHTML = drop;
    if (sel == "cross-sectional") {
        document.SS2Prop.n.value = res.n;
        document.SS2Prop.n_drop.value = res.n_drop;
        document.getElementById("drop_3").innerHTML = drop;
    }
}

function doCalculate_ss2prop_ratio1() {
    var p0 = +document.SS2Prop.p0.value;
    var p1 = +document.SS2Prop.p1.value;
    var alpha = document.SS2Prop.alpha.value;
    var power = document.SS2Prop.power.value / 100;
    var drop = document.SS2Prop.drop.value;

    var res = calc_ss2prop_ratio1(p0, p1, alpha, power, drop);

    document.SS2Prop.n.value = res.n;
    document.SS2Prop.n_drop.value = res.n_drop;
    document.getElementById("drop_").innerHTML = drop;
}

function doCalculate_hx_ssalpha() {
    var cronbach0_hx = document.SSAlpha_Hx.cronbach0_hx.value;
    var cronbach1_hx = document.SSAlpha_Hx.cronbach1_hx.value;
    var alpha_hx = document.SSAlpha_Hx.alpha_hx.value;
    var power_hx = document.SSAlpha_Hx.power_hx.value / 100;
    var item_hx = document.SSAlpha_Hx.item_hx.value;
    var drop_hx = document.SSAlpha_Hx.drop_hx.value;

    var res = calc_hx_ssalpha(cronbach0_hx, cronbach1_hx, alpha_hx, power_hx, item_hx, drop_hx);

    document.SSAlpha_Hx.n_hx.value = res.n_hx;
    document.SSAlpha_Hx.n_drop_hx.value = res.n_drop_hx;
    document.getElementById("drop_hx_").innerHTML = drop_hx;
}

function doCalculate_est_ssalpha() {
    var cronbach_est = document.SSAlpha_Est.cronbach_est.value;
    var precision_est = document.SSAlpha_Est.precision_est.value;
    var ci_est = document.SSAlpha_Est.ci_est.value / 100;
    var item_est = document.SSAlpha_Est.item_est.value;
    var drop_est = document.SSAlpha_Est.drop_est.value;

    var res = calc_est_ssalpha(cronbach_est, precision_est, ci_est, item_est, drop_est);

    document.SSAlpha_Est.n_est.value = res.n_est;
    document.SSAlpha_Est.n_drop_est.value = res.n_drop_est;
    document.getElementById("drop_est_").innerHTML = drop_est;
}

function doCalculate_ssanimal() {
    var k = +document.SSAnimal.k.value;
    var r = +document.SSAnimal.r.value;
    var sacrifice = +document.SSAnimal.sacrifice.value;

    var res = calc_ssanimal(k, r, sacrifice);

    document.SSAnimal.n_min.value = res.n_min;
    document.SSAnimal.n_max.value = res.n_max;
    document.getElementById("design").innerHTML = res.design;
    document.getElementById("k_").innerHTML = k;
    document.getElementById("r_").innerHTML = r;
    document.getElementById("sacrifice_req").innerHTML = res.sacrifice_req;
    document.getElementById("N_min").innerHTML = res.N_min;
    document.getElementById("N_max").innerHTML = res.N_max;
}

function doReset_ssanimal() {
    document.SSAnimal.n_min.value = "";
    document.SSAnimal.n_max.value = "";
    document.getElementById("design").innerHTML = "";
    document.getElementById("k_").innerHTML = "2";
    document.getElementById("r_").innerHTML = "1";
    document.getElementById("sacrifice_req").innerHTML = "not required";
    document.getElementById("N_min").innerHTML = "0";
    document.getElementById("N_max").innerHTML = "0";
}

function doCalculate_hx_ssauroc() {
    var A0 = +document.SSAUROC_Hx.A0.value;
    var A = +document.SSAUROC_Hx.A.value;
    var p = +document.SSAUROC_Hx.p.value;
    var alpha = document.SSAUROC_Hx.alpha.value;
    var power = document.SSAUROC_Hx.power.value / 100;
    var drop = document.SSAUROC_Hx.drop.value;

    var res = calc_hx_ssauroc(A0, A, p, alpha, power, drop, typeof Decimal !== 'undefined' ? Decimal : null);

    document.SSAUROC_Hx.n.value = res.n;
    document.SSAUROC_Hx.n_drop.value = res.n_drop;
    document.getElementById("drop_hx_").innerHTML = drop;
}

function doCalculate_est_ssauroc() {
    var A = +document.SSAUROC_Est.A.value;
    var p = +document.SSAUROC_Est.p.value;
    var precision = +document.SSAUROC_Est.precision.value;
    var ci = document.SSAUROC_Est.ci.value / 100;
    var drop = document.SSAUROC_Est.drop.value;

    var res = calc_est_ssauroc(A, p, precision, ci, drop, typeof Decimal !== 'undefined' ? Decimal : null);

    document.SSAUROC_Est.n.value = res.n;
    document.SSAUROC_Est.n_drop.value = res.n_drop;
    document.getElementById("drop_est_").innerHTML = drop;
}

function doCalculate_hx_sscorr() {
    var corr = Number(document.SSCorr_Hx.corr.value);
    var alpha = document.SSCorr_Hx.alpha.value;
    var power = document.SSCorr_Hx.power.value / 100;
    var drop = document.SSCorr_Hx.drop.value;

    var res = calc_hx_sscorr(corr, alpha, power, drop);

    document.SSCorr_Hx.n.value = res.n;
    document.SSCorr_Hx.n_drop.value = res.n_drop;
    document.getElementById("drop_hx_").innerHTML = drop;
}

function doCalculate_est_sscorr() {
    var corr = document.SSCorr_Est.corr.value;
    var precision = document.SSCorr_Est.precision.value;
    var ci = document.SSCorr_Est.ci.value / 100;
    var drop = document.SSCorr_Est.drop.value;

    var res = calc_est_sscorr(corr, precision, ci, drop);

    document.SSCorr_Est.n.value = res.n;
    document.SSCorr_Est.n_drop.value = res.n_drop;
    document.getElementById("drop_est_").innerHTML = drop;
}

function doCalculate_hx_ssicc() {
    var icc0 = document.SSICC_Hx.icc0.value;
    var icc1 = document.SSICC_Hx.icc1.value;
    var alpha = document.SSICC_Hx.alpha.value;
    var power = document.SSICC_Hx.power.value / 100;
    var rater = document.SSICC_Hx.rater.value;
    var drop = document.SSICC_Hx.drop.value;

    var res = calc_hx_ssicc(icc0, icc1, alpha, power, rater, drop);

    document.SSICC_Hx.n.value = res.n;
    document.SSICC_Hx.n_drop.value = res.n_drop;
    document.getElementById("drop_").innerHTML = drop;
}

function doCalculate_est_ssicc() {
    var icc = document.SSICC_Est.icc.value;
    var precision = document.SSICC_Est.precision.value;
    var ci = document.SSICC_Est.ci.value / 100;
    var rater = document.SSICC_Est.rater.value;
    var drop = document.SSICC_Est.drop.value;

    var res = calc_est_ssicc(icc, precision, ci, rater, drop);

    document.SSICC_Est.n.value = res.n;
    document.SSICC_Est.n_drop.value = res.n_drop;
    document.getElementById("drop_").innerHTML = drop;
}

function doCalculate_hx_sskappa() {
    var k0 = document.SSKappa_Hx.k0.value;
    var k1 = document.SSKappa_Hx.k1.value;
    var p = document.SSKappa_Hx.p.value;
    var alpha = document.SSKappa_Hx.alpha.value;
    var power = document.SSKappa_Hx.power.value / 100;
    var drop = document.SSKappa_Hx.drop.value;

    var res = calc_hx_sskappa(k0, k1, p, alpha, power, drop);

    document.SSKappa_Hx.n.value = res.n;
    document.SSKappa_Hx.n_drop.value = res.n_drop;
    document.getElementById("drop_").innerHTML = drop;
}

function doCalculate_est_sskappa() {
    var k = document.SSKappa_Est.k.value;
    var precision = document.SSKappa_Est.precision.value;
    var p = document.SSKappa_Est.p.value;
    var ci = document.SSKappa_Est.ci.value / 100;
    var drop = document.SSKappa_Est.drop.value;

    var res = calc_est_sskappa(k, precision, p, ci, drop);

    document.SSKappa_Est.n.value = res.n;
    document.SSKappa_Est.n_drop.value = res.n_drop;
    document.getElementById("drop_").innerHTML = drop;
}

window.onload = function () {
    if (document.getElementById("varTbody")) {
        addVariableRow();
    }
};

function addVariableRow() {
    var tbody = document.getElementById("varTbody");
    var tr = document.createElement("tr");

    var tdName = document.createElement("td");
    tdName.style.padding = "5px";
    var inputName = document.createElement("input");
    inputName.type = "text";
    inputName.placeholder = "e.g. Age";
    tdName.appendChild(inputName);

    var tdScale = document.createElement("td");
    tdScale.style.padding = "5px";
    var selScale = document.createElement("select");
    var optNum = document.createElement("option");
    optNum.value = "numerical";
    optNum.text = "Numerical";
    var optCat = document.createElement("option");
    optCat.value = "categorical";
    optCat.text = "Categorical";
    selScale.appendChild(optNum);
    selScale.appendChild(optCat);
    selScale.onchange = function () { updateRow(tr); };
    tdScale.appendChild(selScale);

    var tdCat = document.createElement("td");
    tdCat.style.padding = "5px";
    var inputCat = document.createElement("input");
    inputCat.type = "number";
    inputCat.min = "2";
    inputCat.value = "2";
    inputCat.style.display = "none";
    inputCat.oninput = function () { updateRow(tr); };
    tdCat.appendChild(inputCat);

    var tdCount = document.createElement("td");
    tdCount.style.padding = "5px";
    var inputCount = document.createElement("input");
    inputCount.type = "number";
    inputCount.value = "1";
    inputCount.readOnly = true;
    tdCount.appendChild(inputCount);

    var tdRem = document.createElement("td");
    tdRem.style.padding = "5px";
    var btnRem = document.createElement("input");
    btnRem.type = "button";
    btnRem.value = "X";
    btnRem.onclick = function () {
        tbody.removeChild(tr);
        updateTotalK();
    };
    tdRem.appendChild(btnRem);

    tr.appendChild(tdName);
    tr.appendChild(tdScale);
    tr.appendChild(tdCat);
    tr.appendChild(tdCount);
    tr.appendChild(tdRem);

    tbody.appendChild(tr);
    updateTotalK();
}

function updateRow(tr) {
    var selScale = tr.cells[1].getElementsByTagName("select")[0];
    var inputCat = tr.cells[2].getElementsByTagName("input")[0];
    var inputCount = tr.cells[3].getElementsByTagName("input")[0];

    if (selScale.value === "numerical") {
        inputCat.style.display = "none";
        inputCount.value = "1";
    } else {
        inputCat.style.display = "inline-block";
        var m = parseInt(inputCat.value);
        if (isNaN(m) || m < 2) m = 2;
        inputCount.value = (m - 1).toString();
    }
    updateTotalK();
}

function updateTotalK() {
    var tbody = document.getElementById("varTbody");
    if (!tbody) return;
    var trs = tbody.getElementsByTagName("tr");
    var totalK = 0;
    for (var i = 0; i < trs.length; i++) {
        var inputCount = trs[i].cells[3].getElementsByTagName("input")[0];
        totalK += parseInt(inputCount.value) || 0;
    }
    document.SSLogistic.k.value = totalK;
}

function doCalculate_sslogistic() {
    var k = Number(document.SSLogistic.k.value);
    var epp = Number(document.SSLogistic.epp.value);
    var p = Number(document.SSLogistic.p.value);
    var drop = Number(document.SSLogistic.drop.value);

    var res = calc_sslogistic(k, epp, p, drop);

    document.SSLogistic.n1.value = res.n1;
    document.SSLogistic.n.value = res.n;
    document.SSLogistic.n_drop.value = res.n_drop;
    document.getElementById("drop_").innerHTML = drop;
}

function copyToClipboard() {
    var text = "Sample Size Calculation:\n\n";
    text += "Logistic Regression - Rule-of-thumb\n\n";
    text += "https://wnarifin.github.io/ssc/sslogistic.html\n\n";

    text += "Variables:\n";
    var tbody = document.getElementById("varTbody");
    var trs = tbody.getElementsByTagName("tr");
    for (var i = 0; i < trs.length; i++) {
        var name = trs[i].cells[0].getElementsByTagName("input")[0].value || "Unnamed";
        var scale = trs[i].cells[1].getElementsByTagName("select")[0].value;
        var count = trs[i].cells[3].getElementsByTagName("input")[0].value;
        if (scale === "categorical") {
            var m = trs[i].cells[2].getElementsByTagName("input")[0].value;
            text += "- " + name + " (Categorical, " + m + " categories) -> Count: " + count + "\n";
        } else {
            text += "- " + name + " (Numerical) -> Count: " + count + "\n";
        }
    }

    var k = document.SSLogistic.k.value;
    var epp = document.SSLogistic.epp.value;
    var p = document.SSLogistic.p.value;
    var drop = document.SSLogistic.drop.value;

    text += "\nInputs:\n";
    text += "- Total independent variables count (k): " + k + "\n";
    text += "- Events (outcomes) per variable (EPV): " + epp + "\n";
    text += "- Proportion with outcome (p): " + p + "\n";
    text += "- Expected dropout rate: " + drop + "%\n";

    var n1 = document.SSLogistic.n1.value;
    var n = document.SSLogistic.n.value;
    var n_drop = document.SSLogistic.n_drop.value;

    var p_num = Number(p);
    var p_text = p_num > 0.5 ? "1 - " + p : p;

    text += "\nResults:\n";
    text += "- Number of subjects with outcome, n1 = (k + 1) x EPV = (" + k + " + 1) x " + epp + " = " + n1 + "\n";
    text += "- Sample size, n = n1 / " + (p_num > 0.5 ? "(1 - p)" : "p") + " = " + n1 + " / " + p_text + " = " + n + "\n";
    text += "- Sample size (with " + drop + "% dropout) = " + n_drop + "\n";

    text += "\nReferences for formula:\n";
    text += "Hosmer, D. W., Lemeshow, S., & Sturdivant, R. X. (2013). Applied logistic regression (3rd ed.). New Jersey: John Wiley & Sons, Inc.\n";
    text += "Peduzzi, P., Concato, J., Kemper, E., Holford, T. R., & Feinstein, A. R. (1996). A simulation study of the number of events per variable in logistic regression analysis. Journal of clinical epidemiology, 49(12), 1373-1379.\n";
    text += "Vittinghoff, E., & McCulloch, C. E. (2007). Relaxing the rule of ten events per variable in logistic and Cox regression. American journal of epidemiology, 165(6), 710-718.\n";

    var year = new Date().getFullYear();
    text += "\nSuggested reference:\n";
    text += "Arifin, W. N. (" + year + "). Sample size calculator (web). Retrieved from http://wnarifin.github.io\n";

    if (navigator.clipboard) {
        navigator.clipboard.writeText(text).then(function () {
            alert("Copied to clipboard!");
        }, function (err) {
            alert("Could not copy text: " + err);
        });
    } else {
        var textArea = document.createElement("textarea");
        textArea.value = text;
        document.body.appendChild(textArea);
        textArea.select();
        try {
            document.execCommand('copy');
            alert("Copied to clipboard!");
        } catch (err) {
            alert("Could not copy text: " + err);
        }
        document.body.removeChild(textArea);
    }
}

function doCalculate_ssmcnemar() {
    var p0 = +document.SSMcNemar.p0.value;
    var p1 = +document.SSMcNemar.p1.value;
    var alpha = document.SSMcNemar.alpha.value;
    var power = document.SSMcNemar.power.value / 100;
    var drop = document.SSMcNemar.drop.value;

    var res = calc_ssmcnemar(p0, p1, alpha, power, drop);

    document.SSMcNemar.n.value = res.n;
    document.SSMcNemar.n_drop.value = res.n_drop;
    document.getElementById("drop_").innerHTML = drop;
}

function doCalculate_ssrmsea1() {
    var rmsea = parseFloat(document.SSRMSEA1.rmsea.value);
    var alpha = parseFloat(document.SSRMSEA1.alpha.value);
    var power = parseFloat(document.SSRMSEA1.power.value) / 100;
    var df = parseInt(document.SSRMSEA1.df.value);
    var drop = parseFloat(document.SSRMSEA1.drop.value);

    var delta = ncp(alpha, power, df);
    var res = calc_ssrmsea1(rmsea, alpha, power, df, drop, delta);

    document.SSRMSEA1.n.value = res.n;
    document.SSRMSEA1.n_drop.value = res.n_drop;
    document.getElementById("drop_").innerHTML = drop;
}

function doCalculate_sssnsp() {
    var sn = document.SSSnSp.sn.value;
    var sp = document.SSSnSp.sp.value;
    var p = document.SSSnSp.p.value;
    var precision = document.SSSnSp.precision.value;
    var ci = document.SSSnSp.ci.value / 100;
    var drop = document.SSSnSp.drop.value;

    var res = calc_sssnsp(sn, sp, p, precision, ci, drop);

    document.SSSnSp.n1.value = res.n1;
    document.SSSnSp.n2.value = res.n2;
    document.SSSnSp.n.value = res.n;
    document.SSSnSp.n_drop.value = res.n_drop;
    document.getElementById("drop_").innerHTML = drop;
}

function doCalculate_ssncp() {
    var alpha = Number(document.NCP.alpha.value);
    var power = Number(document.NCP.power.value) / 100;
    var df = Number(document.NCP.df.value);

    var out = ncp(alpha, power, df);
    document.NCP.ncp.value = out.toFixed(6);
}

function doCalculate_pncchisquare() {
    var q = Number(document.NCX2.q.value);
    var df = Number(document.NCX2.df.value);
    var ncp_val = Number(document.NCX2.ncp.value);

    var out = pncchisq(q, df, ncp_val);
    document.NCX2.pncx2.value = out;
}
