########################
# ESTIMATION
# Author: Wan Nor Arifin
########################

# calculation of CIs
####################

# one mean, z
#============
mean = 120
sd = 15
n = 30
se = sd / sqrt(n)
alpha = 0.05
z = qnorm(1 - alpha/2)

ci = mean + c(-1, 1) * z * se
cbind(mean = mean, ll = ci[1], ul = ci[2])

# one mean, t
#============
mean = 120
sd = 15
n = 30
se = sd / sqrt(n)
alpha = 0.05
t = qt(1 - alpha/2, df = n - 1)

ci = mean + c(-1, 1) * t * se
cbind(mean = mean, ll = ci[1], ul = ci[2])

# one proportion
#===============
p = 0.35
n = 200
(n*p > 5)
(n*(1-p) > 5)
se = sqrt(p*(1-p)/n)
alpha = 0.05
z = qnorm(1 - alpha/2)

ci = p + c(-1, 1) * z * se
cbind(p = p, ll = ci[1], ul = ci[2])
cbind(p = p, ll = ci[1], ul = ci[2]) * 100

# one variance
#=============
sd = 15
var = sd^2
n = 30
alpha = 0.05
x2_ll = qchisq(1 - alpha/2, df = n - 1)
x2_ul = qchisq(alpha/2, df = n - 1)

ci_ll = (n-1)*var / x2_ll
ci_ul = (n-1)*var / x2_ul
cbind(variance = var, ll = ci_ll, ul = ci_ul)
sqrt(cbind(sd = var, ll = ci_ll, ul = ci_ul))

# coverage probility of confidence interval
###########################################

# basic z, when population sd known
#==================================
# say parameters, u = 120, sd = 10, var = 100
# simulation setup
n = 30
m = 10000  # replicated samples
u = 120
sd = 10
alpha = 0.05

# setup repetition of simulation
rep = 10  # repetition
cp = numeric(length = rep)  # set coverage probability cp vector of length = rep

for (i in 1:rep) {  # repeat simulation 10 times
  # --- start cycle
  
  # construct 10000 CIs for 10000 sample means
  xbar = replicate(m, mean(rnorm(n, u, sd)))
  sd_xbar = sd / sqrt(n)
  z = qnorm(1-alpha/2)
  ci_xbar = c(xbar - z * sd_xbar, xbar + z * sd_xbar)
  dim(ci_xbar) = c(length(xbar), 2)
  ci_xbar
  xbar_ci = data.frame(mean = xbar, ll = ci_xbar[, 1], ul = ci_xbar[, 2])
  xbar_ci
  
  # coverage probability
  cp[i] = 1 - sum(xbar_ci$ll < u & xbar_ci$ul < u | xbar_ci$ll > u & xbar_ci$ul > u)/m
  
  # print
  print(cp[i])
  # --- end
}
head(xbar_ci)
cp  # 95% of constructed 95% CIs include parameter
mean(cp)

# what happens when we use z when sd unknown?
#============================================
# say parameters, u = 120, sd = sample sd
# simulation setup
n = 5  # try with smaller n, e.g. 5 vs 30
m = 10000  # replicated samples
u = 120
alpha = 0.05

# setup repetition of simulation
rep = 10  # repetition
cp = numeric(length = rep)  # set coverage probability cp vector of length = rep

for (i in 1:rep) {  # repeat simulation 100 times
  # --- start cycle
  
  # construct 10000 CIs for 10000 sample means
  x = replicate(m, rnorm(n, u, sd))
  xbar = colMeans(x)
  sd_x = apply(x, 2, sd)
  sd_xbar = sd_x / sqrt(n)
  z = qnorm(1-alpha/2)
  ci_xbar = c(xbar - z * sd_xbar, xbar + z * sd_xbar)
  dim(ci_xbar) = c(length(xbar), 2)
  ci_xbar
  xbar_ci = data.frame(mean = xbar, ll = ci_xbar[, 1], ul = ci_xbar[, 2])
  xbar_ci
  
  # coverage probability
  cp[i] = 1 - sum(xbar_ci$ll < u & xbar_ci$ul < u | xbar_ci$ll > u & xbar_ci$ul > u)/m
  
  # print
  print(cp[i])
  # --- end
}
head(xbar_ci)
cp  # 95% of constructed 95% CIs include parameter
mean(cp)  # dependent on n

# use t distrib for sd unknown
#=============================
# say parameters, u = 120, sd = sample sd
# simulation setup
n = 40  # try with smaller n, e.g. 5 vs 30
m = 10000  # replicated samples
u = 120
alpha = 0.05

# setup repetition of simulation
rep = 10  # repetition
cp = numeric(length = rep)  # set coverage probability cp vector of length = rep

for (i in 1:rep) {  # repeat simulation 100 times
  # --- start cycle
  
  # construct 10000 CIs for 10000 sample means
  x = replicate(m, rnorm(n, u, sd))
  xbar = colMeans(x)
  sd_x = apply(x, 2, sd)
  sd_xbar = sd_x / sqrt(n)
  t = qt(1-alpha/2, df = n - 1)  # reliability coeff using t
  ci_xbar = c(xbar - t * sd_xbar, xbar + t * sd_xbar)
  dim(ci_xbar) = c(length(xbar), 2)
  ci_xbar
  xbar_ci = data.frame(mean = xbar, ll = ci_xbar[, 1], ul = ci_xbar[, 2])
  xbar_ci
  
  # coverage probability
  cp[i] = 1 - sum(xbar_ci$ll < u & xbar_ci$ul < u | xbar_ci$ll > u & xbar_ci$ul > u)/m
  
  # print
  print(cp[i])
  # --- end
}
head(xbar_ci)
cp  # 95% of constructed 95% CIs include parameter
mean(cp)  # adjusted for small n
