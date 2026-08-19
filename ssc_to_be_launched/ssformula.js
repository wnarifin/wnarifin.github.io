/*
=======================================
ssformula.js
Author: Wan Nor Arifin
github: github.com/wnarifin/
updated date: 2026-08-04

Core sample size calculation formulas
https://wnarifin.github.io/ssc_web.html
======================================= 
*/

// SS1Mean
function calc_ss1mean(sd, precision, ci, drop) {
    var z = jStat.normal.inv(ci + (1 - ci) / 2, 0, 1);
    var n = Math.ceil(Math.pow(z * sd / precision, 2));
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n: n, n_drop: n_drop };
}

// SS1Prop
function calc_ss1prop(p, precision, ci, drop) {
    var sd = Math.sqrt(p * (1 - p));
    var z = jStat.normal.inv(ci + (1 - ci) / 2, 0, 1);
    var n = Math.ceil(Math.pow(z * sd / precision, 2));
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n: n, n_drop: n_drop };
}

// SS2Mean
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

// SS2MeanPaired - Hx
function calc_hx_ss2mean_paired(sd, diff, alpha, power, drop) {
    var z_alpha = jStat.normal.inv(1 - alpha / 2, 0, 1);
    var z_beta = jStat.normal.inv(power, 0, 1);
    var n = Math.ceil((Math.pow(sd, 2) * Math.pow(z_alpha + z_beta, 2)) / Math.pow(diff, 2));
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n: n, n_drop: n_drop };
}

// SS2MeanPaired - SD
function calc_sd_ss2mean_paired(sd_pre, sd_post, r_pre_post) {
    var var_d = Math.pow(sd_pre, 2) + Math.pow(sd_post, 2) - 2 * r_pre_post * sd_pre * sd_post;
    var sd_d = Math.sqrt(var_d);
    return { sd_d: sd_d.toFixed(3) };
}

// SS2MeanRatio1
function calc_ss2mean_ratio1(sd, diff, alpha, power, drop) {
    var z_alpha = jStat.normal.inv(1 - alpha / 2, 0, 1);
    var z_beta = jStat.normal.inv(power, 0, 1);
    var n = Math.ceil((2 * Math.pow(sd, 2) * Math.pow(z_alpha + z_beta, 2)) / Math.pow(diff, 2));
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n: n, n_drop: n_drop };
}

// SS2MeanRM
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

// SS2Prop
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

// SS2PropRatio1
function calc_ss2prop_ratio1(p0, p1, alpha, power, drop) {
    var p_bar = (p0 + p1) / 2;
    var z_alpha = jStat.normal.inv(1 - alpha / 2, 0, 1);
    var z_beta = jStat.normal.inv(power, 0, 1);
    var n_ = Math.pow(z_alpha * Math.sqrt(2 * p_bar * (1 - p_bar)) + z_beta * Math.sqrt(p0 * (1 - p0) + p1 * (1 - p1)), 2) / Math.pow(p0 - p1, 2);
    var n = Math.ceil(n_);
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n: n, n_drop: n_drop };
}

// SSAlpha_Hx
function calc_hx_ssalpha(cronbach0_hx, cronbach1_hx, alpha_hx, power_hx, item_hx, drop_hx) {
    var z_alpha = jStat.normal.inv(1 - alpha_hx / 2, 0, 1);
    var z_beta = jStat.normal.inv(power_hx, 0, 1);
    var delta = (1 - cronbach0_hx) / (1 - cronbach1_hx);
    var n_hx = Math.ceil(((2 * item_hx / (item_hx - 1)) * Math.pow(z_alpha + z_beta, 2)) / (Math.pow(Math.log(delta), 2)) + 2);
    var n_drop_hx = Math.ceil(n_hx / ((100 - drop_hx) / 100));
    return { n_hx: n_hx, n_drop_hx: n_drop_hx };
}

// SSAlpha_Est
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

// SSAnimal
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

// SSAUROC_Hx
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

// SSAUROC_Est
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

// SSCorr_Hx
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

// SSCorr_Est
function calc_est_sscorr(corr, precision, ci, drop) {
    var z = jStat.normal.inv(ci + (1 - ci) / 2, 0, 1)
    var n = Math.ceil((Math.pow(z, 2) * Math.pow(1 - Math.pow(corr, 2), 2)) / Math.pow(precision, 2) + 1 + 6 * Math.pow(corr, 2));
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n: n, n_drop: n_drop };
}

// SSICC_Hx
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

// SSICC_Est
function calc_est_ssicc(icc, precision, ci, rater, drop) {
    var z = jStat.normal.inv(ci + (1 - ci) / 2, 0, 1)
    var w = precision * 2
    var n = Math.ceil((8 * Math.pow(z, 2) * Math.pow(1 - icc, 2) * Math.pow(1 + (rater - 1) * icc, 2)) / (rater * (rater - 1) * Math.pow(w, 2)) + 1);
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n: n, n_drop: n_drop };
}

// SSKappa_Hx
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

// SSKappa_Est
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

// SSLogistic
function calc_sslogistic(k, epp, p, drop) {
    var n1 = (k + 1) * epp;
    var p_event = p > 0.5 ? 1 - p : p;
    var n = Math.ceil(n1 / p_event);
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n1: n1, n: n, n_drop: n_drop };
}

// SSMcNemar
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

// SSRMSEA
function calc_ssrmsea1(rmsea, alpha, power, df, drop, delta) {
    var N_e = (delta / (Math.pow(rmsea, 2) * df)) + 1;
    var n = Math.ceil(N_e);
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n: n, n_drop: n_drop };
}

// SSSnSp
function calc_sssnsp(sn, sp, p, precision, ci, drop) {
    var z = jStat.normal.inv(ci + (1 - ci) / 2, 0, 1)
    var n1 = Math.ceil((Math.pow(z, 2) * sn * (1 - sn) / Math.pow(precision, 2)) / p);
    var n2 = Math.ceil((Math.pow(z, 2) * sp * (1 - sp) / Math.pow(precision, 2)) / (1 - p));
    var n = Math.max(n1, n2);
    var n_drop = Math.ceil(n / ((100 - drop) / 100));
    return { n1: n1, n2: n2, n: n, n_drop: n_drop };
}

// NCP (Kim 2005)
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

// PNCCHISQ (Ding 1992, Algorithm AS 275)
function pncchisq(q, df, ncp) {
    if (ncp >= 1000) {
        let f_star = Math.pow(df + 2 * ncp, 3) / Math.pow(df + 3 * ncp, 2);
        let b = (df + 3 * ncp) / (df + 2 * ncp);
        let a = (df + ncp) - b * f_star;
        let x_cent = (q - a) / b;

        if (x_cent <= 0) return 0;

        if (f_star > 100000) {
            let z = (Math.pow(x_cent / f_star, 1 / 3) - (1 - 2 / (9 * f_star))) / Math.sqrt(2 / (9 * f_star));
            return jStat.normal.cdf(z, 0, 1);
        }

        return jStat.chisquare.cdf(x_cent, f_star);
    }

    errmax = 1e-64;
    itrmax = 1e6;

    x = q;
    f = df;
    theta = ncp;

    if (f < 0 || theta < 0) {
        return NaN;
    }
    if (x <= 0) {
        return 0
    }

    lam = theta / 2;

    n = 1;
    u = Math.exp(-lam);
    v = u;
    x2 = x / 2;
    f2 = f / 2;
    t = Math.exp(f2 * Math.log(x2) - x2 - jStat.gammaln(f2 + 1));

    term = v * t;
    chi2nc = term;

    f2nx = -1;
    chi2nc_t1 = chi2nc;
    while (f2nx <= 0) {
        u = u * lam / n;
        v = v + u;
        t = Math.exp(Math.log(t * x) - Math.log(f + 2 * n));
        term = v * t;
        chi2nc = chi2nc + term;

        n = n + 1;
        f2nx = (f + 2 * n - x);
        diff = chi2nc - chi2nc_t1;
        if (diff > errmax) {
            chi2nc_t1 = chi2nc;
            f2nx = -1;
        }
        if (n > itrmax - 1) {
            return NaN;
        }
    }
    return chi2nc;
}
