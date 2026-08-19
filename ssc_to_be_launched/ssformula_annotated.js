/*
======================================= 
ssformula_annotated.js
Author: Wan Nor Arifin
github: github.com/wnarifin/
updated date: 2026-08-04

Core sample size calculation formulas
Annotated for LLM agent use.
https://wnarifin.github.io/ssc_web.html
======================================= 
*/

/* SS1Mean
Calculates sample size for one mean estimation.
Inputs:
- sd: Expected standard deviation.
- precision: Desired precision (margin of error).
- ci: Confidence level in percentage (e.g., 95 for 95%).
- drop: Expected dropout rate in percentage (e.g., 10 for 10%).
Outputs an object with:
- n: Required sample size.
- n_drop: Required sample size accounting for dropout.

References:
1. Arifin, W. N. (2013). Introduction to sample size calculation. Education in Medicine Journal, 5(2), e89-e96.
2. Naing, N. N. (2003). Determination of Sample Size. The Malaysian Journal of Medical Sciences: MJMS, 10(2), 84-86.
*/
function calc_ss1mean(sd, precision, ci, drop) {
    var z = jStat.normal.inv(ci + (1 - ci) / 2, 0, 1);
    var n = Math.ceil(Math.pow(z * sd / precision, 2));
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n: n, n_drop: n_drop };
}

/* SS1Prop
Calculates sample size for one proportion estimation.
Inputs:
- p: Expected proportion.
- precision: Desired precision (margin of error) as a proportion.
- ci: Confidence level in percentage (e.g., 95 for 95%).
- drop: Expected dropout rate in percentage.
Outputs an object with:
- n: Required sample size.
- n_drop: Required sample size accounting for dropout.

References:
1. Arifin, W. N. (2013). Introduction to sample size calculation. Education in Medicine Journal, 5(2), e89-e96.
2. Naing, N. N. (2003). Determination of Sample Size. The Malaysian Journal of Medical Sciences: MJMS, 10(2), 84-86.
*/
function calc_ss1prop(p, precision, ci, drop) {
    var sd = Math.sqrt(p * (1 - p));
    var z = jStat.normal.inv(ci + (1 - ci) / 2, 0, 1);
    var n = Math.ceil(Math.pow(z * sd / precision, 2));
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n: n, n_drop: n_drop };
}

/* SS2Mean
Calculates sample size for testing the difference between two independent means.
Inputs:
- sd: Expected standard deviation (pooled).
- diff: Expected difference in means.
- m: Ratio of sample size (Group 0 to Group 1).
- alpha: Significance level (e.g., 0.05).
- power: Statistical power (e.g., 0.8, 0.9).
- drop: Expected dropout rate in percentage.
Outputs an object with:
- n1: Sample size for Group 1.
- n0: Sample size for Group 0.
- n: Total sample size.
- n1_drop, n0_drop, n_drop: Corresponding sample sizes accounting for dropout.

References:
1. Machin, D., Campbell, M. J., Tan, S. B., & Tan, S. H. (2009). Sample size tables for clinical studies (3rd ed.). John Wiley & Sons.
*/
function calc_ss2mean(sd, diff, m, alpha, power, drop) {
    var es = diff / sd;
    var z_alpha = jStat.normal.inv(1 - alpha / 2, 0, 1);
    var z_beta = jStat.normal.inv(power, 0, 1);
    var n1 = (1 + m) / m * Math.pow(z_alpha + z_beta, 2) / Math.pow(es, 2) + Math.pow(z_alpha, 2) / (2 * (1 + m));
    n1 = Math.ceil(n1);
    var n0 = m * n1;
    n0 = Math.ceil(n0);
    var n1_drop = Math.ceil(n1 / ((100 - drop) / 100));
    var n0_drop = Math.ceil(n0 / ((100 - drop) / 100));
    return { n1: n1, n1_drop: n1_drop, n0: n0, n0_drop: n0_drop, n: n1 + n0, n_drop: n1_drop + n0_drop };
}

/* SS2MeanPaired - Hypothesis Testing
Calculates sample size for testing the difference between two paired means (dependent t-test).
Inputs:
- sd: Standard deviation of the differences.
- diff: Expected mean difference.
- alpha: Significance level (e.g., 0.05).
- power: Statistical power (e.g., 0.8, 0.9).
- drop: Expected dropout rate in percentage.
Outputs an object with:
- n: Required sample size (pairs).
- n_drop: Required sample size accounting for dropout.

References:
1. Naing, N. N. (2011). A practical guide on determination of sample size in health sciences research. Kelantan: Pustaka Aman Press.
2. Arifin, W. N. (2014). Calculating standard deviation of difference for determination of sample size for planned paired t-test analysis. Education in Medicine Journal, 6(2), e62-e64.
*/
function calc_hx_ss2mean_paired(sd, diff, alpha, power, drop) {
    var z_alpha = jStat.normal.inv(1 - alpha / 2, 0, 1);
    var z_beta = jStat.normal.inv(power, 0, 1);
    var n = Math.ceil((Math.pow(sd, 2) * Math.pow(z_alpha + z_beta, 2)) / Math.pow(diff, 2));
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n: n, n_drop: n_drop };
}

/* SS2MeanPaired - Standard Deviation of Difference Calculation
Helper function to calculate the standard deviation of differences from pre/post SDs and correlation.
Inputs:
- sd_pre: Standard deviation before intervention.
- sd_post: Standard deviation after intervention.
- r_pre_post: Correlation coefficient between pre and post measurements.
Outputs an object with:
- sd_d: Calculated standard deviation of the difference.

References:
1. Arifin, W. N. (2014). Calculating standard deviation of difference for determination of sample size for planned paired t-test analysis. Education in Medicine Journal, 6(2), e62-e64.
*/
function calc_sd_ss2mean_paired(sd_pre, sd_post, r_pre_post) {
    var var_d = Math.pow(sd_pre, 2) + Math.pow(sd_post, 2) - 2 * r_pre_post * sd_pre * sd_post;
    var sd_d = Math.sqrt(var_d);
    return { sd_d: sd_d.toFixed(3) };
}

/* SS2MeanRatio1
Calculates sample size for testing the difference between two independent means assuming equal sample sizes (ratio 1:1).
Inputs:
- sd: Expected pooled standard deviation.
- diff: Expected difference in means.
- alpha: Significance level (e.g., 0.05).
- power: Statistical power (e.g., 0.8, 0.9).
- drop: Expected dropout rate in percentage.
Outputs an object with:
- n: Sample size per group.
- n_drop: Sample size per group accounting for dropout.

References:
1. Lemeshow, S., Hosmer Jr, D. W., Klar, J., Lwanga, S. K. (1990). Adequacy of sample size in health studies. England: John Wiley & Sons Ltd.
2. Naing, N. N. (2011). A practical guide on determination of sample size in health sciences research. Kelantan: Pustaka Aman Press.
*/
function calc_ss2mean_ratio1(sd, diff, alpha, power, drop) {
    var z_alpha = jStat.normal.inv(1 - alpha / 2, 0, 1);
    var z_beta = jStat.normal.inv(power, 0, 1);
    var n = Math.ceil((2 * Math.pow(sd, 2) * Math.pow(z_alpha + z_beta, 2)) / Math.pow(diff, 2));
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n: n, n_drop: n_drop };
}

/* SS2MeanRM
Calculates sample size for testing the difference between means with repeated measures design.
Inputs:
- sd: Standard deviation.
- diff: Expected difference.
- r: Number of repetitions/measurements.
- base: 1 if baseline measurement is included, 0 otherwise.
- rho: Correlation between repetitions.
- alpha: Significance level.
- power: Statistical power (e.g., 0.8, 0.9).
- drop: Dropout rate in percentage.
Outputs an object with:
- n: Total sample size.
- n_drop: Total sample size accounting for dropout.

References:
1. Machin, D., Campbell, M. J., Tan, S. B., & Tan, S. H. (2009). Sample size tables for clinical studies (3rd ed.). John Wiley & Sons.
*/
function calc_ss2mean_rm(sd, diff, r, base, rho, alpha, power, drop) {
    var es = diff / sd;
    var z_alpha = jStat.normal.inv(1 - alpha / 2, 0, 1);
    var z_beta = jStat.normal.inv(power, 0, 1);
    var R;
    if (base == 1) {
        r = r - 1; // minus 1 baseline
        R = (1 + (r - 1) * rho) / r - Math.pow(rho, 2);
    }
    else {
        R = (1 + (r - 1) * rho) / r;
    }
    var n = R * ((2 * Math.pow(z_alpha + z_beta, 2) / Math.pow(es, 2)) + (Math.pow(z_alpha, 2) / 4));
    n = Math.ceil(n);
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n: n, n_drop: n_drop };
}

/* SS2Prop
Calculates sample size for testing the difference between two independent proportions.
Inputs:
- p0: Proportion in group 0.
- p1: Proportion in group 1.
- m: Ratio of sample sizes (n0/n1).
- alpha: Significance level.
- power: Statistical power (e.g., 0.8, 0.9).
- drop: Dropout rate in percentage.
Outputs an object with:
- n1, n0: Sample sizes for groups 1 and 0.
- n: Total sample size.
- n1_drop, n0_drop, n_drop: Corresponding sizes with dropout.

References:
1. Machin, D., Campbell, M. J., Tan, S. B., & Tan, S. H. (2009). Sample size tables for clinical studies (3rd ed.). John Wiley & Sons.
*/
function calc_ss2prop(p0, p1, m, alpha, power, drop) {
    var p_bar = (p0 + p1) / 2;
    var z_alpha = jStat.normal.inv(1 - alpha / 2, 0, 1);
    var z_beta = jStat.normal.inv(power, 0, 1);
    var n1 = Math.pow(z_alpha * Math.sqrt((1 + m) * p_bar * (1 - p_bar)) + z_beta * Math.sqrt(m * p0 * (1 - p0) + p1 * (1 - p1)), 2) / (m * Math.pow(p0 - p1, 2));
    n1 = Math.ceil(n1);
    var n0 = m * n1;
    n0 = Math.ceil(n0);
    var n1_drop = Math.ceil(n1 / ((100 - drop) / 100));
    var n0_drop = Math.ceil(n0 / ((100 - drop) / 100));
    return { n1: n1, n1_drop: n1_drop, n0: n0, n0_drop: n0_drop, n: n1 + n0, n_drop: n1_drop + n0_drop };
}

/* SS2PropRatio1
Calculates sample size for testing the difference between two independent proportions assuming equal sample sizes (ratio 1:1).
Inputs:
- p0: Proportion in group 0.
- p1: Proportion in group 1.
- alpha: Significance level.
- power: Statistical power (e.g., 0.8, 0.9).
- drop: Dropout rate in percentage.
Outputs an object with:
- n: Sample size per group.
- n_drop: Sample size per group accounting for dropout.

References:
1. Lemeshow, S., Hosmer Jr, D. W., Klar, J., Lwanga, S. K. (1990). Adequacy of sample size in health studies. England: John Wiley & Sons Ltd.
*/
function calc_ss2prop_ratio1(p0, p1, alpha, power, drop) {
    var p_bar = (p0 + p1) / 2;
    var z_alpha = jStat.normal.inv(1 - alpha / 2, 0, 1);
    var z_beta = jStat.normal.inv(power, 0, 1);
    var n_ = Math.pow(z_alpha * Math.sqrt(2 * p_bar * (1 - p_bar)) + z_beta * Math.sqrt(p0 * (1 - p0) + p1 * (1 - p1)), 2) / Math.pow(p0 - p1, 2);
    var n = Math.ceil(n_);
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n: n, n_drop: n_drop };
}

/* SSAlpha_Hx
Calculates sample size for testing the difference between two Cronbach's alpha coefficients (Hypothesis Testing).
Inputs:
- cronbach0_hx: Null hypothesis Cronbach's alpha.
- cronbach1_hx: Alternative hypothesis Cronbach's alpha.
- alpha_hx: Significance level.
- power_hx: Statistical power (e.g., 0.8, 0.9).
- item_hx: Number of items in the scale.
- drop_hx: Expected dropout rate in percentage.
Outputs an object with:
- n_hx: Required sample size.
- n_drop_hx: Required sample size accounting for dropout.

References:
1. Bonett, D. G. (2002). Sample size requirements for testing and estimating coefficient alpha. Journal of educational and behavioral statistics, 27(4), 335-340.
*/
function calc_hx_ssalpha(cronbach0_hx, cronbach1_hx, alpha_hx, power_hx, item_hx, drop_hx) {
    var z_alpha = jStat.normal.inv(1 - alpha_hx / 2, 0, 1);
    var z_beta = jStat.normal.inv(power_hx, 0, 1);
    var delta = (1 - cronbach0_hx) / (1 - cronbach1_hx);
    var n_hx = Math.ceil(((2 * item_hx / (item_hx - 1)) * Math.pow(z_alpha + z_beta, 2)) / (Math.pow(Math.log(delta), 2)) + 2);
    var n_drop_hx = Math.ceil(n_hx / ((100 - drop_hx) / 100));
    return { n_hx: n_hx, n_drop_hx: n_drop_hx };
}

/* SSAlpha_Est
Calculates sample size for estimating Cronbach's alpha with desired precision.
Inputs:
- cronbach_est: Expected Cronbach's alpha.
- precision_est: Desired precision (margin of error).
- ci_est: Confidence level in percentage.
- item_est: Number of items in the scale.
- drop_est: Dropout rate in percentage.
Outputs an object with:
- n_est: Required sample size.
- n_drop_est: Required sample size accounting for dropout.

References:
1. Bonett, D. G. (2002). Sample size requirements for testing and estimating coefficient alpha. Journal of educational and behavioral statistics, 27(4), 335-340.
*/
function calc_est_ssalpha(cronbach_est, precision_est, ci_est, item_est, drop_est) {
    var z = jStat.normal.inv(ci_est + (1 - ci_est) / 2, 0, 1)
    var epsilon2 = precision_est * 2;
    var epsilon1 = (1 - (cronbach_est - precision_est)) / (1 - (Number(cronbach_est) + Number(precision_est)))
    var n_zero = (8 * item_est / (item_est - 1)) * Math.pow(z / Math.log(epsilon1), 2) + 2;
    var w_ul = 1 - Math.exp(Math.log(1 - cronbach_est) + z * Math.sqrt(2 * item_est / ((item_est - 1) * (n_zero - 2))));
    var w_ll = 1 - Math.exp(Math.log(1 - cronbach_est) - z * Math.sqrt(2 * item_est / ((item_est - 1) * (n_zero - 2))));
    var w_zero = w_ul - w_ll;
    var n_est = Math.ceil((n_zero - 2) * Math.pow(w_zero / epsilon2, 2) + 2);
    var n_drop_est = Math.ceil(n_est / ((100 - drop_est) / 100));
    return { n_est: n_est, n_drop_est: n_drop_est };
}

/* SSAnimal
Calculates sample size for animal studies using the resource equation approach (ANOVA).
Inputs:
- k: Number of groups.
- r: Number of repeated measurements (if applicable).
- sacrifice: Whether animal sacrifice is required (1) or not (0).
Outputs an object with:
- n_min, n_max: Minimum and maximum sample sizes per group.
- design: Text description of ANOVA design.
- sacrifice_req: "required" or "not required".
- N_min, N_max: Total minimum and maximum number of animals needed.

References:
1. Arifin, W. N, Zahiruddin, W. M. (2017). Sample size calculation in animal studies using resource equation approach. Malaysian Journal of Medical Sciences, 24(5), 101-105. https://doi.org/10.21315/mjms2017.24.5.11
*/
function calc_ssanimal(k, r, sacrifice) {
    var n_min = null;
    var n_max = null;
    var design = "";
    if (k > 1 && r == 1) {
        n_min = 10 / k + 1;
        n_max = 20 / k + 1;
        design = "The ANOVA design is one-way ANOVA, applied for group comparison. ";
    } else if (k == 1 && r > 1) {
        n_min = 10 / (r - 1) + 1;
        n_max = 20 / (r - 1) + 1;
        design = "The ANOVA design is one within factor, repeated measures ANOVA. This is applied for within group comparison of repeated measurements. ";
    } else if (k > 1 && r > 1) {
        n_min = 10 / (k * r) + 1;
        n_max = 20 / (k * r) + 1;
        design = "The ANOVA design is one-between, one within factor, repeated measures ANOVA. This is applied for between and within group comparison of repeated measurements. ";
    } else {
        design = "The ANOVA design is inappropriate";
    }
    n_min = Math.ceil(n_min);
    n_max = Math.max(n_min, Math.floor(n_max));

    var sacrifice_req, N_min, N_max;
    if (sacrifice == 0) {
        sacrifice_req = "not required";
        N_min = n_min * k;
        N_max = n_max * k;
    } else {
        sacrifice_req = "required";
        N_min = n_min * k * r;
        N_max = n_max * k * r;
    }
    return { n_min: n_min, n_max: n_max, design: design, sacrifice_req: sacrifice_req, N_min: N_min, N_max: N_max };
}

/* SSAUROC_Hx
Calculates sample size for testing the Area Under the Receiver Operating Characteristic (AUROC) curve against a null value.
Inputs:
- A0: Null hypothesis AUROC.
- A: Alternative hypothesis expected AUROC.
- p: Prevalence (proportion of disease).
- alpha: Significance level.
- power: Statistical power (e.g., 0.8, 0.9).
- drop: Dropout rate in percentage.
- Decimal: Decimal.js constructor for high precision math.
Outputs an object with:
- n: Total required sample size.
- n_drop: Total required sample size accounting for dropout.

References:
1. Zhou, X. H., Obuchowski, N. A., & McClish, D. K. (2011). Statistical methods in diagnostic medicine (2nd ed.). Hoboken, New Jersey: John Wiley & Sons. [Equation 6.6, 6.8]
*/
function calc_hx_ssauroc(A0, A, p, alpha, power, drop, Decimal) {
    var one = new Decimal(1);
    var z_alpha = jStat.normal.inv(1 - alpha / 2, 0, 1);
    var z_beta = jStat.normal.inv(power, 0, 1);
    var R = ((1 - p) / p >= 1) ? (1 - p) / p : p / (one.minus(p));
    var var_A = A * (1 - A);
    var var_A0 = A0 * (1 - A0);
    var n_ = Math.ceil(Math.pow(z_alpha * Math.sqrt(var_A0) + z_beta * Math.sqrt(var_A), 2) / Math.pow(A0 - A, 2));
    var n = Math.ceil(n_ * (1 + R));
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n: n, n_drop: n_drop };
}

/* SSAUROC_Est
Calculates sample size for estimating the Area Under the Receiver Operating Characteristic (AUROC) curve with a specific precision.
Inputs:
- A: Expected AUROC.
- p: Prevalence (proportion of disease).
- precision: Desired precision (margin of error).
- ci: Confidence level in percentage.
- drop: Dropout rate in percentage.
- Decimal: Decimal.js constructor for high precision math.
Outputs an object with:
- n: Total required sample size.
- n_drop: Total required sample size accounting for dropout.

References:
1. Zhou, X. H., Obuchowski, N. A., & McClish, D. K. (2011). Statistical methods in diagnostic medicine (2nd ed.). Hoboken, New Jersey: John Wiley & Sons. [Equation 6.2, 6.6]
*/
function calc_est_ssauroc(A, p, precision, ci, drop, Decimal) {
    var one = new Decimal(1);
    var z = jStat.normal.inv(ci + (1 - ci) / 2, 0, 1)
    var R = ((1 - p) / p >= 1) ? (1 - p) / p : p / (one.minus(p));
    var var_A = A * (1 - A);
    var n_ = Math.ceil(Math.pow(z * Math.sqrt(var_A), 2) / Math.pow(precision, 2));
    var n = Math.ceil(n_ * (1 + R));
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n: n, n_drop: n_drop };
}

/* SSCorr_Hx
Calculates sample size for testing a correlation coefficient against a null value.
Inputs:
- corr: Expected correlation coefficient (r).
- alpha: Significance level.
- power: Statistical power (e.g., 0.8, 0.9).
- drop: Dropout rate in percentage.
Outputs an object with:
- n: Required sample size.
- n_drop: Required sample size accounting for dropout.

References:
1. Machin, D., Campbell, M. J., Tan, S. B, & Tan, S. H. (2009). Sample size tables for clinical studies (3rd eds.). West Sussex, UK: John Wiley & Sons Ltd.
*/
function calc_hx_sscorr(corr, alpha, power, drop) {
    var z_alpha = jStat.normal.inv(1 - alpha / 2, 0, 1);
    var z_beta = jStat.normal.inv(power, 0, 1);
    var u_corr_zero = 0.5 * Math.log((1 + corr) / (1 - corr));
    var n_zero = Math.ceil(Math.pow(z_alpha + z_beta, 2) / Math.pow(u_corr_zero, 2) + 3);
    var u_corr = 0.5 * Math.log((1 + corr) / (1 - corr)) + corr / (2 * (n_zero - 1));
    var n = Math.ceil(Math.pow(z_alpha + z_beta, 2) / Math.pow(u_corr, 2) + 3);
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n: n, n_drop: n_drop };
}

/* SSCorr_Est
Calculates sample size for estimating a correlation coefficient with desired precision.
Inputs:
- corr: Expected correlation coefficient (r).
- precision: Desired precision (margin of error).
- ci: Confidence level in percentage.
- drop: Expected dropout rate in percentage.
Outputs an object with:
- n: Required sample size.
- n_drop: Required sample size accounting for dropout.

References:
1. Moinester, M., & Gottfried, R. (2014). Sample size estimation for correlations with pre-specified confidence interval. The Quantitative Methods for Psychology, 10(2), 124-130. [Equation 8]
*/
function calc_est_sscorr(corr, precision, ci, drop) {
    var z = jStat.normal.inv(ci + (1 - ci) / 2, 0, 1)
    var n = Math.ceil((Math.pow(z, 2) * Math.pow(1 - Math.pow(corr, 2), 2)) / Math.pow(precision, 2) + 1 + 6 * Math.pow(corr, 2));
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n: n, n_drop: n_drop };
}

/* SSICC_Hx
Calculates sample size for testing the intraclass correlation coefficient (ICC).
Inputs:
- icc0: Null hypothesis ICC.
- icc1: Alternative hypothesis ICC.
- alpha: Significance level.
- power: Statistical power (e.g., 0.8, 0.9).
- rater: Number of raters or replicates per subject.
- drop: Expected dropout rate in percentage.
Outputs an object with:
- n: Required number of subjects.
- n_drop: Required number of subjects accounting for dropout.

References:
1. Walter, S.D., Eliasziw, M., & Donner, A. (1998). Sample size and optimal designs for reliability studies. Statistics in medicine, 17, 101-110.
*/
function calc_hx_ssicc(icc0, icc1, alpha, power, rater, drop) {
    var z_alpha = jStat.normal.inv(1 - alpha / 2, 0, 1);
    var z_beta = jStat.normal.inv(power, 0, 1);
    var theta0 = icc0 / (1 - icc0);
    var theta = icc1 / (1 - icc1);
    var C0 = (1 + rater * theta0) / (1 + rater * theta);
    var n = Math.ceil(1 + (2 * Math.pow(z_alpha + z_beta, 2) * rater) / (Math.pow(Math.log(C0), 2) * (rater - 1)));
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n: n, n_drop: n_drop };
}

/* SSICC_Est
Calculates sample size for estimating the intraclass correlation coefficient (ICC) with desired precision.
Inputs:
- icc: Expected ICC.
- precision: Desired precision (margin of error).
- ci: Confidence level in percentage.
- rater: Number of raters or replicates.
- drop: Expected dropout rate in percentage.
Outputs an object with:
- n: Required number of subjects.
- n_drop: Required number of subjects accounting for dropout.

References:
1. Bonett, D. G. (2002). Sample size requirements for estimating intraclass correlations with desired precision. Statistics in medicine, 21(9), 1331-1335.
*/
function calc_est_ssicc(icc, precision, ci, rater, drop) {
    var z = jStat.normal.inv(ci + (1 - ci) / 2, 0, 1)
    var w = precision * 2
    var n = Math.ceil((8 * Math.pow(z, 2) * Math.pow(1 - icc, 2) * Math.pow(1 + (rater - 1) * icc, 2)) / (rater * (rater - 1) * Math.pow(w, 2)) + 1);
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n: n, n_drop: n_drop };
}

/* SSKappa_Hx
Calculates sample size for testing Cohen's Kappa agreement statistic.
Inputs:
- k0: Null hypothesis Kappa.
- k1: Alternative hypothesis Kappa.
- p: Prevalence of the trait.
- alpha: Significance level.
- power: Statistical power (e.g., 0.8, 0.9).
- drop: Expected dropout rate in percentage.
Outputs an object with:
- n: Required sample size.
- n_drop: Required sample size accounting for dropout.

References:
1. Donner, A., Eliasziw, M. (1992). A goodness-of-fit approach to inference procedures for the kappa statistic: Confidence interval construction, significance-testing and sample size estimation. Statistics in Medicine, 11, 1511-1519.
2. Shoukri, M. M., Asyali, M. H., Donner, A. (2004). Sample size requirements for the design of reliability study: review and new results. Statistical Methods in Medical Research, 13, 1-21.
*/
function calc_hx_sskappa(k0, k1, p, alpha, power, drop) {
    var z_alpha = jStat.normal.inv(1 - alpha / 2, 0, 1);
    var z_beta = jStat.normal.inv(power, 0, 1);
    var ncp = Math.pow(z_alpha + z_beta, 2);
    var t1 = Math.pow(p * (1 - p) * (k1 - k0), 2) / (Math.pow(p, 2) + p * (1 - p) * k0);
    var t2 = 2 * Math.pow(p * (1 - p) * (k1 - k0), 2) / (p * (1 - p) * (1 - k0));
    var t3 = Math.pow(p * (1 - p) * (k1 - k0), 2) / (Math.pow(1 - p, 2) + p * (1 - p) * k0);
    var n = Math.ceil(ncp * Math.pow(t1 + t2 + t3, -1));
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n: n, n_drop: n_drop };
}

/* SSKappa_Est
Calculates sample size for estimating Cohen's Kappa with a desired precision.
Inputs:
- k: Expected Kappa value.
- precision: Desired precision (margin of error).
- p: Prevalence of the trait.
- ci: Confidence level in percentage.
- drop: Expected dropout rate in percentage.
Outputs an object with:
- n: Required sample size.
- n_drop: Required sample size accounting for dropout.

References:
1. Donner, A., Eliasziw, M. (1992). A goodness-of-fit approach to inference procedures for the kappa statistic: Confidence interval construction, significance-testing and sample size estimation. Statistics in Medicine, 11, 1511-1519.
2. Shoukri, M. M., Asyali, M. H., Donner, A. (2004). Sample size requirements for the design of reliability study: review and new results. Statistical Methods in Medical Research, 13, 1-21.
*/
function calc_est_sskappa(k, precision, p, ci, drop) {
    var z = jStat.normal.inv(ci + (1 - ci) / 2, 0, 1);
    var w = precision * 2;
    var a = (4 * Math.pow(z, 2) / Math.pow(w, 2));
    var b = (1 - k);
    var c = (1 - k) * (1 - 2 * k);
    var d = (k * (2 - k)) / (2 * p * (1 - p));
    var n = Math.ceil(a * (b * (c + d)));
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n: n, n_drop: n_drop };
}

/* SSLogistic
Calculates sample size for Logistic Regression based on the rule-of-thumb Events Per Variable (EPV).
Inputs:
- k: Total independent variables count.
- epp: Events (outcomes) per variable (EPV).
- p: Proportion of subjects with the outcome.
- drop: Expected dropout rate in percentage.
Outputs an object with:
- n1: Number of subjects with outcome.
- n: Total sample size required.
- n_drop: Total sample size accounting for dropout.

References:
1. Hosmer, D. W., Lemeshow, S., & Sturdivant, R. X. (2013). Applied logistic regression (3rd ed.). New Jersey: John Wiley & Sons, Inc.
2. Peduzzi, P., Concato, J., Kemper, E., Holford, T. R., & Feinstein, A. R. (1996). A simulation study of the number of events per variable in logistic regression analysis. Journal of clinical epidemiology, 49(12), 1373-1379.
3. Vittinghoff, E., & McCulloch, C. E. (2007). Relaxing the rule of ten events per variable in logistic and Cox regression. American journal of epidemiology, 165(6), 710-718.
*/
function calc_sslogistic(k, epp, p, drop) {
    var n1 = (k + 1) * epp;
    var p_event = p > 0.5 ? 1 - p : p;
    var n = Math.ceil(n1 / p_event);
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n1: n1, n: n, n_drop: n_drop };
}

/* SSMcNemar
Calculates sample size for testing equality of paired proportions using McNemar's test.
Inputs:
- p0: Proportion in group 0.
- p1: Proportion in group 1.
- alpha: Significance level.
- power: Statistical power (e.g., 0.8, 0.9).
- drop: Expected dropout rate in percentage.
Outputs an object with:
- n: Required sample size (pairs).
- n_drop: Required sample size accounting for dropout.

References:
1. Machin, D., Campbell, M. J., Tan, S. B, & Tan, S. H. (2009). Sample size tables for clinical studies (3rd eds.). West Sussex, UK: John Wiley & Sons Ltd.
*/
function calc_ssmcnemar(p0, p1, alpha, power, drop) {
    var p_discordant = p1 * (1 - p0) + p0 * (1 - p1);
    var or = (p1 * (1 - p0)) / (p0 * (1 - p1));
    var z_alpha = jStat.normal.inv(1 - alpha / 2, 0, 1);
    var z_beta = jStat.normal.inv(power, 0, 1);
    var n_ = Math.pow(z_alpha * (or + 1) + z_beta * Math.sqrt(Math.pow(or + 1, 2) - Math.pow(or - 1, 2) * p_discordant), 2) / (Math.pow(or - 1, 2) * p_discordant);
    var n = Math.ceil(n_);
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n: n, n_drop: n_drop };
}

/* SSRMSEA
Calculates sample size for Structural Equation Modeling (SEM) based on Root Mean Square Error of Approximation (RMSEA).
Inputs:
- rmsea: Desired RMSEA value.
- alpha: Significance level.
- power: Statistical power (e.g., 0.8, 0.9).
- df: Degrees of freedom.
- drop: Expected dropout rate in percentage.
- delta: Noncentrality parameter (computed via ncp() function).
Outputs an object with:
- n: Required sample size.
- n_drop: Required sample size accounting for dropout.

References:
1. Kim, K. H. (2005). The relation among fit indexes, power, and sample size in structural equation modeling. Structural Equation Modeling, 12(3), 368-390.
*/
function calc_ssrmsea1(rmsea, alpha, power, df, drop, delta) {
    var N_e = (delta / (Math.pow(rmsea, 2) * df)) + 1;
    var n = Math.ceil(N_e);
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n: n, n_drop: n_drop };
}

/* SSSnSp
Calculates sample size for estimating sensitivity and specificity of a diagnostic test.
Inputs:
- sn: Expected sensitivity.
- sp: Expected specificity.
- p: Prevalence of the disease.
- precision: Desired precision (margin of error).
- ci: Confidence level in percentage.
- drop: Expected dropout rate in percentage.
Outputs an object with:
- n1: Sample size based on sensitivity.
- n2: Sample size based on specificity.
- n: Overall required sample size (maximum of n1 and n2).
- n_drop: Overall sample size accounting for dropout.

References:
1. Buderer, N. M. F. (1996). Statistical methodology: I. Incorporating the prevalence of disease into the sample size calculation for sensitivity and specificity. Academic Emergency Medicine, 3(9), 895-900.
*/
function calc_sssnsp(sn, sp, p, precision, ci, drop) {
    var z = jStat.normal.inv(ci + (1 - ci) / 2, 0, 1)
    var n1 = Math.ceil((Math.pow(z, 2) * sn * (1 - sn) / Math.pow(precision, 2)) / p);
    var n2 = Math.ceil((Math.pow(z, 2) * sp * (1 - sp) / Math.pow(precision, 2)) / (1 - p));
    var n = Math.max(n1, n2);
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n1: n1, n2: n2, n: n, n_drop: n_drop };
}

/* NCP (Kim 2005)
Calculates the non-centrality parameter (NCP) given alpha, power and df for SEM models.
Inputs:
- alpha: Significance level.
- power: Statistical power (e.g., 0.8, 0.9).
- df: Degrees of freedom.
Outputs:
- delta: Computed non-centrality parameter.

References:
1. Kim, K. H. (2005). The relation among fit indexes, power, and sample size in structural equation modeling. Structural Equation Modeling, 12(3), 368-390.
*/
function ncp(alpha, power, df) {
    var crit = jStat.chisquare.inv(1 - alpha, df);
    var delta = Math.round(crit - df);
    var times = 1;
    var direc = 1;
    var amount = 10;

    while (times < 9) {
        delta = delta + direc * amount;
        var pow = 1 - pncchisq(crit, df, delta);
        if (direc * (power - pow) < 0) {
            times = times + 1;
            direc = -1 * direc;
            amount = amount / 10;
        }
    }
    return delta;
}

/* Pr NC chi-square (Ding 1992, Algorithm AS 275)
AI help: Gemini 3.1 Pro (to handle ncp >=1000 & df >100000 issues)

Calculates c.d.f. i.e Pr for non-central chi-square distribution
given quartile q, df and ncp. Tested against R function pchisq(q, df, ncp) & Ross (1999) Table V.

References:
1. Ding, C. G. (1992). Algorithm AS 275: computing the non-central χ 2 distribution function. Journal of the Royal Statistical Society. Series C (Applied Statistics), 41(2), 478-482.
2. Pearson, E. S. (1959). Note on an approximation to the distribution of non-central χ2. Biometrika, 46(3/4), 364.
3. Wilson, E. B., & Hilferty, M. M. (1931). The distribution of chi-square. Proceedings of the National Academy of Sciences of the United States of America, 17(12), 684–688.
4. Ross, A. H. (1999). Algorithm for calculating the noncentral chi-square distribution. IEEE Transactions on Information Theory, 45(4), 1327-1333.
*/
function pncchisq(q, df, ncp) {
    // Pearson 3-moment approximation for large NCP
    // Reduces large Non-Central Chi-Square values to Central Chi-Square equivalents
    // Prevents double precision underflow of Math.exp(-ncp/2) in Ding's algorithm
    // which occurs when ncp > ~1490
    if (ncp >= 1000) {
        let f_star = Math.pow(df + 2 * ncp, 3) / Math.pow(df + 3 * ncp, 2);
        // f_star: The Equivalent Degrees of Freedom required so that the Skewness (3rd moment) 
        // of the new central chi-square perfectly matches the skewness of your original 
        // non-central chi-square.
        let b = (df + 3 * ncp) / (df + 2 * ncp);
        // b: The Scaling Factor applied to the variance.
        // Because the non-central distribution spreads out differently, b acts as a multiplier. 
        // It stretches or compresses the new central distribution to ensure that its Variance (2nd moment) 
        // perfectly matches the variance of your original non-central distribution.
        let a = (df + ncp) - b * f_star;
        // a: The Location Shift.
        // After scaling, the shape (skewness) and width (variance) match, but the distribution 
        // is still slightly off in its center point (mean). 'a' calculates exactly how far left or right 
        // to slide the entire distribution so that its Mean (1st moment) perfectly aligns with the 
        // original non-central mean (which is $df + ncp$), ensuring the tail probabilities align.
        let x_cent = (q - a) / b;
        // x_cent: The Central Quantile
        // Now we need to calculate the probability up to your original quantile, q ($X \le q$). 
        // If $X \approx b \cdot \chi^2(f^*) + a$, we just use basic algebra to isolate the 
        // central chi-square variable:
        // 1. $b \cdot \chi^2(f^*) + a \le q$
        // 2. $b \cdot \chi^2(f^*) \le q - a$
        // $\chi^2(f^*) \le (q - a) / b$
        // Therefore, to get the probability at q for your complex non-central distribution, 
        // you just ask jStat for the probability of (q - a) / b on a standard central 
        // chi-square distribution with f_star degrees of freedom!

        if (x_cent <= 0) return 0;

        if (f_star > 100000) {
            // Wilson-Hilferty transformation for huge central chi-square to avoid jStat hang
            // May also refer https://en.wikipedia.org/wiki/Chi-squared_distribution
            let z = (Math.pow(x_cent / f_star, 1 / 3) - (1 - 2 / (9 * f_star))) / Math.sqrt(2 / (9 * f_star));
            return jStat.normal.cdf(z, 0, 1);
        }

        return jStat.chisquare.cdf(x_cent, f_star);
    }

    errmax = 1e-64;
    itrmax = 1e6;

    // rename to original names in Ding 1992
    x = q;
    f = df;
    theta = ncp;

    // preliminary checks
    if (f < 0 || theta < 0) {  // changed to f < 0 vs f <= 0 in Ding
        return NaN;
    }
    if (x <= 0) {
        return 0;  // follows pchisq R behavior, chi2nc = 0
    }

    // once passed the checks, assign
    lam = theta / 2;

    // evaluate first term
    n = 1;
    u = Math.exp(-lam);
    v = u;
    x2 = x / 2;
    f2 = f / 2;
    // t = x2 ** f2 * Math.exp(-x2) / Math.exp(jStat.gammaln(f2 + 1));  // original Ding 1992
    t = Math.exp(f2 * Math.log(x2) - x2 - jStat.gammaln(f2 + 1));  // solve Inf value issue

    term = v * t;
    chi2nc = term;

    // evaluate term
    f2nx = -1;  // so while loop will be run at least once
    chi2nc_t1 = chi2nc;  // Pr at t-1
    while (f2nx <= 0) {
        u = u * lam / n;
        v = v + u;
        // t = t * x / (f + 2 * n);
        t = Math.exp(Math.log(t * x) - Math.log(f + 2 * n));  // but, log this one does not solve issues
        // with ncp() function, see issues in ncp0.js
        term = v * t;
        chi2nc = chi2nc + term;  // Pr at t

        n = n + 1;
        f2nx = (f + 2 * n - x);
        // check for convergence
        // defined as diff = p_t - p_{t-1} as difference between successive iterations
        // instead of error bound in Ding 1992
        // diff is faster to run than error bound
        diff = chi2nc - chi2nc_t1;
        if (diff > errmax) {
            chi2nc_t1 = chi2nc;  // update Pr at t-1 for next iteration
            f2nx = -1;  // to restart while loop
        }
        if (n > itrmax - 1) {
            return NaN;
        }
    }
    return chi2nc;
}
