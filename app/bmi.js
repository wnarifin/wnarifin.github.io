// BMI
// Author: Wan Nor Arifin
function doCalculate() {
    weight = document.BMI.weight.value;
    height = document.BMI.height.value;
    height_m = height/100.0
    target_bmi = document.BMI.target_bmi.value;

    bmi = weight/height_m**2;
    target_weight = target_bmi*(height_m**2);

    document.BMI.bmi.value = bmi.toFixed(2);
    document.BMI.target_weight.value = target_weight.toFixed(2);

    return;
}
