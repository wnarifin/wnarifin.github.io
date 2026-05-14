/* NCP
================================
Author: Wan Nor Arifin
github: github.com/wnarifin/
Updated: 2023/01/04

Calculates non-centrality parameter
given alpha, power and df.

Kim, K. H. (2005). The relation among fit indexes, power, and sample size in structural equation modeling.
Structural Equation Modeling, 12(3), 368-390.
================================*/

// for interface
function ncpCalculate() {
    alpha = Number(document.NCP.alpha.value);
    power = Number(document.NCP.power.value)/100;
    df = Number(document.NCP.df.value);
        
    out = ncp(alpha, power, df);
    document.NCP.ncp.value = out.toFixed(6);
    return;
}

// Kim 2005
function ncp(alpha, power, df) {
    crit = jStat.chisquare.inv(1 - alpha, df);
    delta = Math.round(crit - df);
    times = 1;
    direc = 1;
    amount = 10;
  
    while (times < 9) {  // reasonable to increase up to 15 for df = 1 million
        delta = delta + direc * amount;
        pow = 1 - pncchisq(crit, df, delta);
        // relies on Ding 1992 Algo
        if (direc * (power - pow) < 0) {
            times = times + 1;
            direc = -1 * direc;
            amount = amount / 10;
        }
    }
    return delta;
}

