// SSAnimal
// Author: Wan Nor Arifin
function doCalculate() {
    // Get values from form
    k = document.SSAnimal.k.value;
    r = document.SSAnimal.r.value;
    sacrifice = document.SSAnimal.sacrifice.value;

    // Calculator
    if (k > 1 && r == 1) {
        n_min = 10/k + 1;
        n_max = 20/k + 1;
        design = "The ANOVA design is one-way ANOVA, applied for group comparison. ";
    } else if (k == 1 && r > 1) {
        n_min = 10/(r-1) + 1;
        n_max = 20/(r-1) + 1;
        design = "The ANOVA design is one within factor, repeated measures ANOVA. This is applied for within group comparison of repeated measurements. ";
    } else if (k > 1 && r > 1) {
        n_min = 10/(k*r) + 1;
        n_max = 20/(k*r) + 1;
        design = "The ANOVA design is one-between, one within factor, repeated measures ANOVA. This is applied for between and within group comparison of repeated measurements. ";
    } else {
        n_min = null;
        n_max = null;
        design = "The ANOVA design is inappropriate";
    }
    n_min = Math.ceil( n_min );
    n_max = Math.max(n_min, Math.floor( n_max )); //rounded down, see pg.102
    if (sacrifice == 0) {
        sacrifice_req = "not required";
        N_min = n_min * k;
        N_max = n_max * k;
    } else {
        sacrifice_req = "required";
        N_min = n_min * k * r;
        N_max = n_max * k * r;            
    }

    // Return values
    document.SSAnimal.n_min.value = n_min;
    document.SSAnimal.n_max.value = n_max;
    document.getElementById("design").innerHTML = design;
    document.getElementById("k_").innerHTML = k;
    document.getElementById("r_").innerHTML = r;
    document.getElementById("sacrifice_req").innerHTML = sacrifice_req;
    document.getElementById("N_min").innerHTML = N_min;
    document.getElementById("N_max").innerHTML = N_max;
    return;
}

// has to include this reset function to reset <span> contents
// otherwise, easier by <input type="reset">
function doReset() {
    document.SSAnimal.n_min.value = "";
    document.SSAnimal.n_max.value = "";
    document.getElementById("design").innerHTML = "";
    document.getElementById("k_").innerHTML = "2";
    document.getElementById("r_").innerHTML = "1";
    document.getElementById("sacrifice_req").innerHTML = "not required";
    document.getElementById("N_min").innerHTML = "0";
    document.getElementById("N_max").innerHTML = "0";
    return;
}
