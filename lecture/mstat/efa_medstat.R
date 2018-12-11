# =================================
# Exploratory factor analysis (EFA)
# Author: Wan Nor Arifin
# Updated: 2018-12-11
# =================================

# Preliminaries

## Load libraries
library(foreign)  # for importing SPSS data
library(psych)  # for psychometrics
library(lattice)  # easy to plot multivariate plots

## Load data set
data = read.spss("Attitude_Statistics v3.sav", use.value.labels = F, to.data.frame = T)
str(data)
data = data[-1]  # exclude ID, we are not going to use the variable
dim(data)  # 12 variables
names(data)  # list variable names
head(data)  # the first 6 observations

# Exploratory factor analysis

## Preliminary steps

# **Descriptive statistics**
describe(data)
response.frequencies(data)
print(response.frequencies(data), digit = 2)

# **Normality of data**

# _Univariate normality_

# 1. Histograms
cat(names(data), sep = " + ")
histogram(~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10 + Q11 + Q12,
          data = data)
# graphically looks normal

# 2. Shapiro Wilk's test
mapply(shapiro.test, data)  # all p.value < 0.05, not normal

# _Multivariate (MV) normality_
mardia(data)  # at start and end, away from the line, not MV normal
# kurtosis =  10.8, p < 0.05, not MV normal.

## Step 1

# **Check suitability of data for analysis**

# 1. Kaiser-Meyer-Olkin (KMO) Measure of Sampling Adequacy (MSA)
KMO(data)  # Overall MSA = 0.76, middling

# 2. Bartlet's test of sphericity
cortest.bartlett(data)  # p < 0.05, items are correlated

# **Determine the number of factors**

# 1. Kaiser's eigenvalue > 1 rule.
# 2. Cattell's scree test.
scree = scree(data)  # look at --o-- FA
print(scree)  # Eigen values of factors > 1 = 2, then number of factor = 2

# 3. Parallel analysis.
parallel = fa.parallel(data, fm = "pa", fa = "fa")  # 2 dots above dashed line
# means factor = 2
print(parallel)  # Parallel analysis suggests that the number of factors =  2

# 4. Very simple structure (VSS) criterion.
# 5. Velicer's minimum average partial (MAP) criterion.
vss(data)  # VSS complexity 1 = 3/5 factors, mininum MAP = 2 factors

## Step 2

# **Run EFA**

# 1. fixing the number of factors as decided from previous step. Two factors are reasonable.
# 2. choosing an appropriate extraction method. We use PAF, `fm = "pa"`.
# 3. choosing an appropriate oblique rotation method. We use oblimin, `rotate = "oblimin"`.
fa = fa(data, nfactors = 2, fm = "pa", rotate = "oblimin")
print(fa, cut = .3, digits = 3)  # cut = .3 to view only FLs > 0.3
print(fa, digits = 3)  # to view FLs < .3

# **Results**

# 1. Factor loadings (FL).
# Low FLs? Q1 < .3, Q12 < .4, Q2 & Q3 < .5
# Cross-loadings? Q3 & Q12, indicated by relatively high FLs across factors
# and high complexity, close to 2

# 2. Communalities.
# Low communalities (h2)? Q1 < Q12 < Q2 < .25 (.004 / .177 / .207 respectively)

# 3. Factor correlations.
# PA1 <-> PA2 = .087 < .85

## Step 3

# **Repeat**

# Remove Q1? Low communality, low FL:
fa1 = fa(subset(data, select = -Q1), nfactors = 2, fm = "pa", rotate = "oblimin")
print(fa1, cut = .3, digits = 3)

# Remove Q12? Low communality, (relatively) low FL, high complexity (cross loading)
fa2 = fa(subset(data, select = -c(Q1, Q12)), nfactors = 2, fm = "pa", 
         rotate = "oblimin")
print(fa2, cut = .3, digits = 3)

# Remove Q3? Low communality, (relatively) low FL, high complexity (cross loading)
fa3 = fa(subset(data, select = -c(Q1, Q12, Q3)), nfactors = 2, fm = "pa", 
         rotate = "oblimin")
print(fa3, cut = .3, digits = 3)

# Remove Q2? Low communality, (relatively) low FL
fa4 = fa(subset(data, select = -c(Q1, Q12, Q3, Q2)), nfactors = 2, fm = "pa", 
         rotate = "oblimin")
print(fa4, cut = .3, digits = 3)

# **Stop**

## Summary

# > PA1: Q4, Q5, Q6, Q7, Q11
# > PA2: Q8, Q9, Q10

# Name the factor.
# > PA1 -- Affinity
# > PA2 -- Importance
