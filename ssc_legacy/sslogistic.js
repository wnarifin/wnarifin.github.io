// SSLogistic
// Author: Wan Nor Arifin
// AI Help: Gemini 3.1 Pro

window.onload = function () {
    // Add one initial row when page loads
    addVariableRow();
};

function addVariableRow() {
    var tbody = document.getElementById("varTbody");
    var tr = document.createElement("tr");

    // Variable Name
    var tdName = document.createElement("td");
    tdName.style.padding = "5px";
    var inputName = document.createElement("input");
    inputName.type = "text";
    inputName.placeholder = "e.g. Age";
    tdName.appendChild(inputName);

    // Scale
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

    // Categories (m)
    var tdCat = document.createElement("td");
    tdCat.style.padding = "5px";
    var inputCat = document.createElement("input");
    inputCat.type = "number";
    inputCat.min = "2";
    inputCat.value = "2";
    inputCat.style.display = "none";
    inputCat.oninput = function () { updateRow(tr); };
    tdCat.appendChild(inputCat);

    // Count
    var tdCount = document.createElement("td");
    tdCount.style.padding = "5px";
    var inputCount = document.createElement("input");
    inputCount.type = "number";
    inputCount.value = "1";
    inputCount.readOnly = true;
    tdCount.appendChild(inputCount);

    // Remove
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

function doCalculate() {
    k = Number(document.SSLogistic.k.value);
    epp = Number(document.SSLogistic.epp.value);
    p = Number(document.SSLogistic.p.value);
    drop = Number(document.SSLogistic.drop.value);

    n1 = (k + 1) * epp;
    p_event = p > 0.5 ? 1 - p : p;
    n = Math.ceil(n1 / p_event);
    n_drop = Math.ceil(n / ((100 - drop) / 100));

    document.SSLogistic.n1.value = n1;
    document.SSLogistic.n.value = n;
    document.SSLogistic.n_drop.value = n_drop;
    document.getElementById("drop_").innerHTML = drop;
    return;
}

function copyToClipboard() {
    var text = "Sample Size Calculation:\n\n";
    text += "Logistic Regression - Rule-of-thumb\n\n";
    text += "https://wnarifin.github.io/ssc/sslogistic.html\n\n";

    // Variables table
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

    // for run in github
    if (navigator.clipboard) {
        navigator.clipboard.writeText(text).then(function () {
            alert("Copied to clipboard!");
        }, function (err) {
            alert("Could not copy text: " + err);
        });
        // to allow local testing
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
