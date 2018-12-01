# ====================================================
# Principal components and Exploratory factor analysis
# Wan Nor Arifin
# ====================================================

# --------------
# Load libraries
library(foreign)  # for importing SPSS data
library(psych)  # for psychometrics
library(lattice)  # easy to plot multivariate plots

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

#-Univariate normality

#--1. Histograms
multi.hist(data)
boxplot(data)
# graphically looks normal

#--2. Shapiro Wilk's test
mapply(shapiro.test, data)  # all p.value < 0.05, not normal

#-Multivariate (MV) normality
mardia(data)  # at start and end, away from the line, not MV normal
# kurtosis =  10.8, p < 0.05
# skewness = 735.01, p < 0.05
# not MV normal.

## Check suitability of data for analysis

#--1. Kaiser-Meyer-Olkin (KMO) Measure of Sampling Adequacy (MSA)
KMO(data)  # Overall MSA = 0.76, > 0.7.

#--2. Bartlet's test of sphericity
cortest.bartlett(data)  # p < 0.05, items are correlated

## Determine the number of factors

#--1. Kaiser's eigenvalue > 1 rule.
#--2. Cattell's scree test.
scree = scree(data)
# look at --o-- FA --> 2 factors.
# look at --*-- PC --> 3 factors
print(scree)
# Eigen values of factors > 1 = 2, then number of factor = 2
# Eigen values of principal components > 1 = 3, then number of component = 3
#--3. Parallel analysis.
parallel = fa.parallel(data, fa = "both")
# 2 triangles above dashed line, factor = 2
# 2 x above dashed line, components = 2
print(parallel)
# Parallel analysis suggests:
# number of factors =  2
# number of components = 2


# -----------------------------
# Principal components analysis
## Perform the analysis
#-1. fix the number of components as decided from previous step. Two/three factors are reasonable.
## 2 components
pc = principal(data, nfactors = 2)
print(pc, digits = 3)
print(pc, cut = .3, digits = 3)  # cut = .3 to view only CLs > 0.3
## Interpret
# 2 components
# Q1 CL < 0.3 --> can remove
# Variance explained, RC1 = 26.6%, RC2 = 22.9%

## 3 components
pc1 = principal(data, nfactors = 3)
print(pc1, digits = 3)
print(pc1, cut = .3, digits = 3)
## Interpret
# 3 components
# All CL > 0.3
# Variance explained, RC1 = 25.9%, RC2 = 22.8%, RC3 = 9.5%
# last component small % of variance, may be 2 components are better.

## Remove Q1
pc_1 = principal(data[-c(1)], nfactors = 2)
print(pc_1, cut = .3, digits = 3)
# Q12 has 2 high loadings in 2 components, can remove
# can decide based on 'com' complexity too
# com = 2 means Q12 belongs to both components
# not good for data reduction
# Cumulative variance = 54.0%

## Remove Q12
pc_2 = principal(data[-c(1, 12)], nfactors = 2)
print(pc_2, cut = .3, digits = 3)
# Cumulative variance = 57.3%

## Component scores
pc_2$scores

## Summary
# Components:
# > RC1: Q4, Q5, Q6, Q7, Q11
# > RC2: Q2, Q3, Q8, Q9, Q10
# Name the component:
# > RC1 -- Affinity
# > RC2 -- Importance


# ---------------------------
# Exploratory factor analysis

## Perform the analysis
#-1. fix the number of factors as decided from previous step. Two factors are reasonable.
#-2. choose an appropriate extraction method. We use PAF, `fm = "pa"`.
#-3. choose an appropriate oblique rotation method. We use oblimin, `rotate = "oblimin"`.
## 2 factors
fa = fa(data, nfactors = 2, fm = "pa", rotate = "oblimin")
print(fa, digits = 3)
print(fa, cut = .3, digits = 3)  # cut = .3 to view only FLs > 0.3
## Interpret
# Q1 FL < .3

## Remove Q1
fa_1 = fa(data[-c(1)], nfactors = 2, fm = "pa", rotate = "oblimin")
print(fa_1, cut = .3, digits = 3)
## Interpretation
# Q12 com = 1.98
# item not specific to a factor

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
