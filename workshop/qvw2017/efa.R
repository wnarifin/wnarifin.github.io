#------------------------------------------------------------------
# Exploratory factor analysis and Cronbach's alpha
#------------------------------------------------------------------
# Author: Wan Nor Arifin
#------------------------------------------------------------------

# Preliminaries

## Load libraries
library(foreign)
library(psych)  # for psychometrics
library(MVN)  # for multivariate normality

## Load data set
data = read.spss("Attitude_Statistics v3.sav", use.value.labels = F, to.data.frame = T)
dim(data)  # 13 variables
names(data)  # list variable names
head(data)  # the first 6 observations
data1 = data[-1]  # remove ID
dim(data1)
names(data1)
head(data1)

# Exploratory factor analysis

## Preliminary steps

# **Descriptive statistics**
describe(data1)
response.frequencies(data1)

# **Normality of data**

# _Univariate normality_

# 1. Histograms
par(mfrow = c(3,4))  # set view to 3 rows & 4 columns
apply(data1, 2, hist)
par(mfrow = c(1,1))  # set to default full view
# multi.hist(data1)  # at times, error

# 2. Shapiro Wilk's test
apply(data1, 2, shapiro.test)

# _Multivariate normality_
mardiaTest(data1, qqplot = TRUE)

## Step 1

# **Check suitability of data for analysis**

# 1. Kaiser-Meyer-Olkin (KMO) Measure of Sampling Adequacy (MSA)
KMO(data1)  # middling

# 2. Bartlet's test of sphericity
cortest.bartlett(data1)  # < 0.05

# **Determine the number of factors**

# 1. Kaiser's eigenvalue > 1 rule.
# 2. Cattell's scree test.
scree = scree(data1)
print(scree)  # factor = 2

# 3. Parallel analysis.
parallel = fa.parallel(data1, fm = "pa", fa = "fa")
print(parallel)  # factor = 2

# 4. Very simple structure (VSS) criterion.
# 5. Velicer's minimum average partial (MAP) criterion.
vss(data1)  # VSS = 3/5, MAP = 2

## Step 2

# **Run EFA**

# 1. fixing the number of factors as decided from previous step. Two factors are reasonable.
# 2. choosing an appropriate extraction method. We use PAF, `fm = "pa"`.
# 3. choosing an appropriate oblique rotation method. We use oblimin, `rotate = "oblimin"`.
fa = fa(data1, nfactors = 2, fm = "pa", rotate = "oblimin")
print(fa, cut = .3, digits = 3)
# use `print(fa, digits = 3)` to view FLs < .3

# **Results**

# 1. Factor loadings (FL).
# Low FLs? Q1 < .3, Q12 < .4, Q2 & Q3 < .5
# Cross-loadings? Q3 & Q12

# 2. Communalities.
# Low communalities? Q1 < Q12 < Q2 < .25 (.004 / .177 / .207 respectively)

# 3. Factor correlations.
# PA1 <-> PA2 = .087 < .85

## Step 3

# **Repeat**

# Remove Q1? Low communality and FL:
fa1 = fa(data1[-1], nfactors = 2, fm = "pa", rotate = "oblimin")
print(fa1, cut = .3, digits = 3)

# Remove Q12? Low communality & FL
fa2 = fa(data1[-c(1,12)], nfactors = 2, fm = "pa", rotate = "oblimin")
print(fa2, cut = .3, digits = 3)

# Remove Q2? Low communality & FL
fa3 = fa(data1[-c(1,2,12)], nfactors = 2, fm = "pa", rotate = "oblimin")
print(fa3, cut = .3, digits = 3)

# Remove Q3? Low communality & FL. High item complexity indicates cross-loading.
fa4 = fa(data1[-c(1,2,3,12)], nfactors = 2, fm = "pa", rotate = "oblimin")
print(fa4, cut = .3, digits = 3)

# **Stop**

## Summary

# > PA1: Q4, Q5, Q6, Q7, Q11
# > PA2: Q8, Q9, Q10

# Name the factor.
# > PA1 -- Affinity
# > PA2 -- Importance

# Internal consistency reliability

# **Cronbach's alpha**

# Determine the reliability for each factor separately by including the selected items only.

names(data1)  # list
# Group items
PA1 = c("Q4","Q5","Q6","Q7","Q11")
PA2 = c("Q8","Q9","Q10")

# **PA1**
alpha.pa1 = alpha(data1[PA1])
print(alpha.pa1, digits = 3)
# 1. Cronbach's alpha
# 2. Corrected item-total correlation" `r.cor` & `r.drop`
# 3. Cronbach's alpha if item deleted: `raw_alpha` under `Reliability if an item is dropped:`

# **PA2**
alpha.pa2 = alpha(data1[PA2])
print(alpha.pa2, digits = 3)
