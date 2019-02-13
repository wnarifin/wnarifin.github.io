# ================================================
# A Short Course on Data Analysis Using R Software
# Wan Nor Arifin
# 
# Basic Statistics
# ================================================

# Comparison of numerical data

# ----
# independent t-test
library(foreign)
cholest = read.spss("cholest.sav", to.data.frame = TRUE)
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
t.test(chol ~ sex, data = cholest, var.equal = TRUE)

# Mann-Whitney test
by(cholest$chol, cholest$sex, median)
by(cholest$chol, cholest$sex, IQR)

wilcox.test(chol ~ sex, data = cholest, exact = FALSE)

# ----
# paired t-test
sbp = read.csv("sbp.csv")
str(sbp)
sbp

mean(sbp$S1); sd(sbp$S1)
mean(sbp$S2); sd(sbp$S2)

mean(sbp$S2 - sbp$S1); sd(sbp$S2 - sbp$S1)

lengths(sbp)

histogram(~ (S2 - S1), data = sbp)
bwplot(~ (S2 - S1), data = sbp)

# paired-t test
t.test(sbp$S2, sbp$S1, paired = TRUE)

# wilcoxon signed-rank test
median(sbp$S1); IQR(sbp$S1)
median(sbp$S2); IQR(sbp$S2)

wilcox.test(sbp$S2, sbp$S1, paired = TRUE, exact = FALSE)

# ----
# one-way ANOVA
by(cholest$chol, cholest$categ, mean)
by(cholest$chol, cholest$categ, sd)
table(cholest$categ)
histogram(~ chol | categ, data = cholest, layout = c(1,3))
bwplot(chol ~ categ, data = cholest)

bartlett.test(chol ~ categ, data = cholest)

# equal variance
aov_chol = aov(chol ~ categ, data = cholest)
summary(aov_chol)
# if unequal variance, can use Welch version
oneway.test(chol ~ categ, data = cholest)

pairwise.t.test(cholest$chol, cholest$categ, p.adj = "bonferroni")

residuals_chol = residuals(aov_chol)
residuals_chol = as.numeric(residuals_chol)

histogram(residuals_chol)
bwplot(residuals_chol)

# Kruskal-Wallis
by(cholest$chol, cholest$categ, median)
kruskal.test(chol ~ categ, data = cholest)

pairwise.wilcox.test(cholest$chol, cholest$categ, p.adj = "bonferroni")


# Comparison of categorical data

# chi-squared test
lung = read.csv("lung.csv")
str(lung)
head(lung)
tab_lung = table(Smoking = lung$Smoking, Cancer = lung$Cancer)
str(lung)
tab_lung
addmargins(tab_lung)
chisq.test(tab_lung)
chisq.test(lung$Smoking, lung$Cancer)

chisq.test(tab_lung)$expected

# Fisher's exact test
fisher.test(tab_lung)

# McNemar's test
tab_pm = read.table(header = FALSE, text = "
794 150
86 570
                    ")
tab_pm
str(tab_pm)
tab_pm = as.matrix(tab_pm)
tab_pm = as.table(tab_pm)
str(tab_pm)

dimnames(tab_pm) = list(first = c("approve", "disapprove"),
                        second = c("approve", "disapprove"))
tab_pm
addmargins(tab_pm)
mcnemar.test(tab_pm)


# Correlation

# data
library(foreign)
cholest = read.spss("cholest.sav", to.data.frame = TRUE)
str(cholest)

library(lattice)
histogram(~ chol, data = cholest)
bwplot(~ chol, data = cholest)
histogram(~ age, data = cholest)
bwplot(~ age, data = cholest)
xyplot(chol ~ age, data = cholest)
cor.test(~ chol + age, data = cholest)

# Spearman's correlation
cor.test(~ chol + age, data = cholest, method = "spearman")