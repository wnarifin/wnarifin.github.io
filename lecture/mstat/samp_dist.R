# Sampling distributions
# Author: Wan Nor Arifin

# sampling distribution of one mean

# simple example
# ----

# population, N = 3
pop = c(3, 6, 9)
plot(table(pop))
plot(table(pop), xlab = "X", ylab = "Frequency", main = "Population")
mean(pop)
# define population variance (i.e divide by n not n-1)
varp = function(x) sum((x - mean(x))^2)/length(x)
varp(pop)

# n = 2

# sampling with replacement
# permutation
# N^r = 9
samp_w = expand.grid(pop, pop)
lengths(samp_w)[[1]]
samp_w$mean = apply(samp_w, 1, mean)
plot(table(samp_w$mean))
plot(table(samp_w$mean), xlab = expression(bar(X)), ylab = "Frequency", main = "Sample")
mean(samp_w$mean)  # sampling distrib mean
varp(samp_w$mean)  # sampling distrib var
varp(samp_w$mean)*2  # = pop var

# sampling w/out replacement
# combination
# NCr = 3
choose(3, 2)
combn(pop, 2)
samp_wo = as.data.frame(t(combn(pop, 2)))
samp_wo$mean = apply(samp_wo, 1, mean)
plot(table(samp_wo$mean), type = "h")  # n not large enough
mean(samp_wo$mean)  # sampling distrib mean
varp(samp_wo$mean)  # sampling distrib var
varp(samp_wo$mean)*(3-1)*2/(3-2)  # = pop var

# larger population and sample
# ----

# simulation settings
N = 10^6  # population 1 million
n = 30
time = 10^4  # 10K times
mu = 120
sigma = 15

# population
x = rnorm(N, mean = mu, sd = sigma)  # generate x values of variable X
hist(x, xlab = expression(X %~% N(120, 15^2)), main = "Population")
mean(x)
varp(x)

# w replacement
samp_xw =  replicate(time, sample(x, n, replace = TRUE))
str(samp_xw)
samp_xw_mean = colMeans(samp_xw)
hist(samp_xw_mean, xlab = expression(bar(X)), main = "With replacement")
mean(samp_xw_mean)
varp(samp_xw_mean)
varp(samp_xw_mean)*n

# w/out replacement
samp_xwo =  replicate(time, sample(x, n, replace = FALSE))
str(samp_xwo)
samp_xwo_mean = colMeans(samp_xwo)
hist(samp_xwo_mean, xlab = expression(bar(X)), main = "Without replacement")
mean(samp_xwo_mean)
varp(samp_xwo_mean)
varp(samp_xwo_mean)*(N-1)*n/(N-n)
