# Comparison of numerical data

# ----
# independent t-test
library(foreign)
cholest = read.spss("cholest.sav", to.data.frame = T)
str(cholest)

head(cholest)

by(cholest$chol, cholest$sex, mean)
by(cholest$chol, cholest$sex, sd)

table(cholest$sex)

# graphical
library(lattice)
histogram(~ chol | sex, data = cholest, layout = c(1, 2))
bwplot(chol ~ sex, data = cholest)

# equal variance
var.test(chol ~ sex, data = cholest)

# t-test
# unequal variance, Welch version
t.test(chol ~ sex, data = cholest)
# equal variance
t.test(chol ~ sex, data = cholest, var.equal = T)

# Mann-Whitney test
by(cholest$chol, cholest$sex, median)
by(cholest$chol, cholest$sex, IQR)

wilcox.test(chol ~ sex, data = cholest, exact = FALSE)

# ----
# paired t-test
sbp = read.csv("sbp.csv")
str(sbp)

mean(sbp$S1)
mean(sbp$S2)

mean(sbp$S2 - sbp$S1)

histogram(~ (S2 - S1), data = sbp)
s2_s1 = sbp$S2 - sbp$S1
hist(s2_s1, probability = T)
lines(density(s2_s1, adjust = 1.5))

bwplot(~ (S2 - S1), data = sbp)

# paired-t test
t.test(sbp$S2, sbp$S1, paired = T)

# wilcoxon signed-rank test
median(s2_s1)
median(sbp$S1)
median(sbp$S2)

wilcox.test(sbp$S2, sbp$S1, paired = T, exact = F)

# ----
by(cholest$chol, cholest$categ, mean)
histogram(~ chol | categ, data = cholest, layout = c(1,3))
bwplot(chol ~ categ, data = cholest)

bartlett.test(chol ~ categ, data = cholest)

# equal variance
aov_chol = aov(chol ~ categ, data = cholest)
summary(aov_chol)
# unequal variance, use Welch version
oneway.test(chol ~ categ, data = cholest)

pairwise.t.test(cholest$chol, cholest$categ, p.adj = "bonferroni")

str(aov_chol)

residuals_chol = residuals(aov_chol)
residuals_chol = as.numeric(residuals_chol)

histogram(residuals_chol)
bwplot(residuals_chol)

# Kruskal-Wallis
by(cholest$chol, cholest$categ, median)
kruskal.test(chol ~ categ, data = cholest)

pairwise.wilcox.test(cholest$chol, cholest$categ, p.adj = "bonferroni")
