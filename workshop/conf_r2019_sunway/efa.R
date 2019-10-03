# ===================================
# Exploratory factor analysis (short)
# Author: Wan Nor Arifin
# Updated: 2019-10-05
# ===================================


# --------------
# Load libraries
library(foreign)  # for importing SPSS data
library(psych)  # for psychometrics


# -------------
# Load data set
data = read.spss("Attitude_Statistics v3.sav", use.value.labels = F, to.data.frame = T)
str(data)
data = data[-1]  # exclude ID, we are not going to use the variable
dim(data)  # 12 variables
names(data)  # list variable names
head(data)  # the first 6 observations


# -------------
# Preliminaries

## Descriptive statistics
describe(data)
response.frequencies(data)
print(response.frequencies(data), digit = 2)

## Normality of data

# **Univariate normality**

# 1. Histograms
multi.hist(data)
boxplot(data)
# graphically looks normal

# 2. Shapiro Wilk's test
mapply(shapiro.test, data)  # all p.value < 0.05, not normal

# **Multivariate (MV) normality**
mardia(data)  # at start and end, away from the line, not MV normal
# kurtosis =  10.8, p < 0.05
# skewness = 735.01, p < 0.05
# not MV normal.


# ---------------------------
# Exploratory factor analysis

## Step 1: Check data suitability for analysis

# 1. Kaiser-Meyer-Olkin (KMO) Measure of Sampling Adequacy (MSA)
KMO(data)  # Overall MSA = 0.76, > 0.7.

# 2. Bartlet's test of sphericity
cortest.bartlett(data)  # p < 0.05, items are correlated

## Determine the number of factors

# 1. Kaiser's eigenvalue > 1 rule & 2. Cattell's scree test.
scree = scree(data)
# look at --o-- FA --> 2 factors.
print(scree)
# Eigen values of factors > 1 = 2, then number of factor = 2

# 3. Parallel analysis.
parallel = fa.parallel(data, fa = "both")
# 2 triangles above dashed line, factor = 2
print(parallel)
# Parallel analysis suggests:
# number of factors =  2

## Step 2: Perform EFA
# 1. fix the number of factors as decided from previous step. Two factors are reasonable.
# 2. choose an appropriate extraction method. We use PAF, `fm = "pa"`.
# 3. choose an appropriate oblique rotation method. We use oblimin, `rotate = "oblimin"`.

## 2 factors
fa = fa(data, nfactors = 2, fm = "pa", rotate = "oblimin")
print(fa, digits = 3)
print(fa, cut = .3, digits = 3)  # cut = .3 to view only FLs > 0.3
## Interpretation
# Q1 FL < .3

## Step 3: Repeat EFA

## Remove Q1
fa_1 = fa(data[-c(1)], nfactors = 2, fm = "pa", rotate = "oblimin")
print(fa_1, cut = .3, digits = 3)
## Interpretation
# Q12 com = 1.98
# complexity > 1, item not specific to a factor

## Remove Q12
fa_2 = fa(data[-c(1, 12)], nfactors = 2, fm = "pa", rotate = "oblimin")
print(fa_2, cut = .3, digits = 3)
## Interpretation
# all FLs > .3
# Cumulative variance = 48.1%
# Factor correlation PA1 <-> PA2 = 0.1

## Factor scores
fa_2$scores

## Summary
# Factors:
# > PA1: Q4, Q5, Q6, Q7, Q11
# > PA2: Q2, Q3, Q8, Q9, Q10
# Name the factor:
# > PA1 -- Affinity
# > PA2 -- Importance


# ------------------------------
# Reliability - Cronbach's alpha

# Determine the reliability for each factor separately by including the selected items only.

## Group items
names(data)  # item list
PA1 = c("Q4","Q5","Q6","Q7","Q11")
PA2 = c("Q2", "Q3", "Q8","Q9","Q10")

## PA1
alpha.pa1 = alpha(data[PA1])
print(alpha.pa1, digits = 3)
# 1. Cronbach's alpha
# 2. Corrected item-total correlation" `r.cor` & `r.drop`
# 3. Cronbach's alpha if item deleted: `raw_alpha` under `Reliability if an item is dropped:`

## PA2
alpha.pa2 = alpha(data[PA2])
print(alpha.pa2, digits = 3)
