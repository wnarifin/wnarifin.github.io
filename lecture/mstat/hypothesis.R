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

# ex 2, p, z
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