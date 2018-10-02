# hypothesis testing lecture

# ex 1, mean, sd known, z
mu = 120
mean = 125
sd = 15
n = 30
se = sd/sqrt(n)
z = (mean - mu)/se
p_z = 1 - pnorm(z, 0, 1)
p_value = 1 - pnorm(mean, mu, se)
# 2-tailed
qnorm(0.975)  # c, 1 - alpha/2
z
p_z*2
p_value*2
# >
qnorm(0.95)  # c, one-tailed
z
p_z
p_value
# <
qnorm(0.05)  # c, one-tailed, lower
z
pnorm(z)  # lower tail!
1 - p_z
1 - p_value

# ex 2, mean, sd unknown, t
mu = 120
mean = 125
sd = 15
n = 25
se = sd/sqrt(n)
df = n - 1
t = (mean - mu)/se
p_t = 1 - pt(t, df)
# 2-tailed
qt(0.975, df)  # c, 1 - alpha/2
t
p_t*2
# >
qt(0.95, df)  # c, one-tailed
t
p_t
# <
qt(0.05, df)  # c, one-tailed, lower
t
pt(t, df)  # lower tail
1 - p_t

# ex 3, p, z
p = 0.3
x = 25
n = 60
phat = x/n
var = p*(1-p)
se = sqrt(var/n)
z = (phat - p)/se
p_z = 1 - pnorm(z)
p_value = 1 - pnorm(phat, p, se)
# 2-tailed
qnorm(0.975)  # c
z
p_z*2
p_value*2
# >
qnorm(0.95)  # c, one-tailed
z
p_z
p_value
# <
qnorm(0.05)  # c, one-tailed, lower
z
pnorm(z)  # lower tail!
1 - p_z
1 - p_value

# ex 4, var, X^2
sigma2 = 15^2
var = 18.5^2
n = 30
df = n - 1
x2 = df*var/sigma2
# 2-tailed
qchisq(0.025, df)  # c_l
qchisq(0.975, df)  # c_u
x2
# x2_alpha/2 < x2 < x2_1-alpha/2
# no difference
pchisq(x2, df)  # p that it is lower
1 - pchisq(x2, df)  # p that it is higher
# cannot multiply by 2 for asymmetric distribution!
# report one-sided p-values.
# >
qchisq(0.95, df)  # c, one-tailed
x2
# x2 > x2_1-alpha
1 - pchisq(x2, df)
p_x2u
# <
qchisq(0.05, df)  # c, one-tailed, lower
x2
# x2 not < x2_alpha
pchisq(x2, df)
