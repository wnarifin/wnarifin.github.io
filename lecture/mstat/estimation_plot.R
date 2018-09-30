# basic z, when population sd known
#==================================
# say parameters, u = 120, sd = 10, var = 100
# simulation setup
n = 30
m = 10  # replicated samples
u = 120
sd = 10
alpha = 0.05

# construct 10 CIs for 10 sample means
xbar = replicate(m, mean(rnorm(n, u, sd)))
sd_xbar = sd / sqrt(n)
z = qnorm(1-alpha/2)
ci_xbar = c(xbar - z * sd_xbar, xbar + z * sd_xbar)
dim(ci_xbar) = c(length(xbar), 2)
ci_xbar
xbar_ci = data.frame(mean = xbar, ll = ci_xbar[, 1], ul = ci_xbar[, 2])
xbar_ci

# coverage probability
cp = 1 - sum(xbar_ci$ll < u & xbar_ci$ul < u | xbar_ci$ll > u & xbar_ci$ul > u)/m
cp  # 95% of constructed 95% CIs include parameter

# plotrix
library(plotrix)  # install.packages("plotrix")

plotCI(xbar_ci$mean, 1:length(xbar_ci$mean), ui = xbar_ci$ul, li = xbar_ci$ll,
       ylab = "Sample number", xlab = "Means and CIs",
       err = "x", lwd = 2, scol = "red", col = "blue")
abline(v = 120, col = "blue", lwd = 2)
