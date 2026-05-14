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
