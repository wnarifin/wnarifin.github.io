/* Pr NC chi-square
================================
Author: Wan Nor Arifin
AI help: Gemini 3.1 Pro (to handle ncp >=1000 & df >100000 issues)
github: github.com/wnarifin/
Updated: 2026/5/14

Calculates c.d.f. i.e Pr for non-central chi-square distribution
given quartile q, df and ncp. Tested against R function pchisq(q, df, ncp) & Ross (1999) Table V.

1. Ding, C. G. (1992). Algorithm AS 275: computing the non-central χ 2 distribution function.
Journal of the Royal Statistical Society. Series C (Applied Statistics), 41(2), 478-482.
2. Pearson, E. S. (1959). Note on an approximation to the distribution of non-central χ2. Biometrika, 46(3/4), 364.
3. Wilson, E. B., & Hilferty, M. M. (1931). The distribution of chi-square. Proceedings of the National Academy of Sciences of the United States of America, 17(12), 684–688.
4. Ross, A. H. (1999). Algorithm for calculating the noncentral chi-square distribution. IEEE Transactions on Information Theory, 45(4), 1327-1333.

================================*/

// for interface
function pncchisquare() {
    q = Number(document.NCX2.q.value);
    df = Number(document.NCX2.df.value);
    ncp = Number(document.NCX2.ncp.value);

    out = pncchisq(q, df, ncp);
    document.NCX2.pncx2.value = out;
    return;
}

// Ding 1992 Algorithm AS 275
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
// refer test results in test_results_all.xlsx
