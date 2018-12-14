# ====================================
# Confirmatory Factor Analysis (short)
# Author: Wan Nor Arifin
# Updated: 2018-12-15
# ====================================

# Preliminaries

## Load libraries
library(foreign)
library(psych)
library(lavaan)  # for CFA
library(semTools)  # for additional functions in SEM
library(semPlot)  # for path diagram

## Load data set
data = read.spss("Attitude_Statistics v3.sav", F, T)  # Shortform
# Include selected items from PA1 & PA2 in "data.cfa"
data.cfa = data[c("Q2", "Q3", "Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11")]
str(data.cfa)
dim(data.cfa)
names(data.cfa)
head(data.cfa)

# Confirmatory factor analysis

## Preliminary steps

# **Descriptive statistics**
describe(data.cfa)
response.frequencies(data.cfa)

# **Multivariate normality**
mardia(data.cfa)  # `kurtosis` > 5, _P_ < 0.05
# use MLR

## Step 1

# **Specify the measurement model**
model = "
PA1 =~ Q4 + Q5 + Q6 + Q7 + Q11
PA2 =~ Q2 + Q3 + Q8 + Q9 + Q10
"

## Step 2

# **Fit the model**
cfa.model = cfa(model, data = data.cfa, estimator = "MLR")
summary(cfa.model, fit.measures = T, standardized = T)

# **Results**

# Overall model fit - by fit indices.
# Fail all cutoff points.
# ! There is no CFit _P_-value for robust RMSEA.
# 
# Parameter estimates
# Remember to read under `Std.all`
# All FLs > 0.3
# and the factor correlation < 0.85.
# There is no problem with the item quality and the factors are distinct.

mi = modificationIndices(cfa.model)
subset(mi, mi > 3.84) # look at 'mi'
sr = residuals(cfa.model, type = "standardized")
sr
# Localized areas of misfit
# Modification indices:
# The highest `Q4 ~~  Q3`
# Ignore `PA1 =~  Q3``, `PA1 =~  Q8` and `PA2 =~  Q5`,
# not justifiable to allow these items specified under other factors.
# Residuals:
# Q3<->Q4, Q5<->Q8 SR > |2.58|

## Step 3

# **Model revision**

# _Revision 1_: Based on MI, `Q4 ~~  Q3`?
model1 = "
PA1 =~ Q4 + Q5 + Q6 + Q7 + Q11
PA2 =~ Q2 + Q3 + Q8 + Q9 + Q10
Q4 ~~  Q3
"
cfa.model1 = cfa(model1, data = data.cfa, estimator = "MLR")
summary(cfa.model1, fit.measures=T, standardized=T)
# Improved, but still do not satisfy the cutoff points
mi1 = modificationIndices(cfa.model1)
subset(mi1, mi > 3.84) # look at 'mi'
sr1 = residuals(cfa.model1, type = "standardized")
sr1
# Based on MI, `Q3 ~~ Q10` is highest

# _Revision_ 2: Based on MI, `Q3 ~~ Q10`?
model2 = "
PA1 =~ Q4 + Q5 + Q6 + Q7 + Q11
PA2 =~ Q2 + Q3 + Q8 + Q9 + Q10
Q4 ~~  Q3
Q3 ~~ Q10
"
cfa.model2 = cfa(model2, data = data.cfa, estimator = "MLR")
summary(cfa.model2, fit.measures=T, standardized=T)
# Only SRMR & upper 90% of RMSEA do not fit
# Q3 FL very low, remove?
mi2 = modificationIndices(cfa.model2)
subset(mi2, mi > 3.84)
sr2 = residuals(cfa.model2, type="standardized"); sr2
# Based on MI, `Q9 ~~ Q10` is highest

# _Revision_ 3: Based on MI, `Q9 ~~ Q10`?
model3 = "
PA1 =~ Q4 + Q5 + Q6 + Q7 + Q11
PA2 =~ Q2 + Q3 + Q8 + Q9 + Q10
Q4 ~~  Q3
Q3 ~~ Q10
Q9 ~~ Q10
"
cfa.model3 = cfa(model3, data = data.cfa, estimator = "MLR")
summary(cfa.model3, fit.measures=T, standardized=T)
# Model fit perfect!
# SRMR and upper 90% of RMSEA are off by 0.01 and 0.02 respectively only!
# Q3 FL gets even lower, remove?
mi3 = modificationIndices(cfa.model3)
subset(mi3, mi > 3.84)
sr3 = residuals(cfa.model3, type="standardized"); sr3

# _Revision_ 4: Remove Q3? Also remomber to remove `~~ Q3` too.
model4 = "
PA1 =~ Q4 + Q5 + Q6 + Q7 + Q11
PA2 =~ Q2 + Q8 + Q9 + Q10
Q9 ~~ Q10
"
cfa.model4 = cfa(model4, data = data.cfa, estimator = "MLR")
summary(cfa.model4, fit.measures=T, standardized=T)
# Perfect!

# **Model-to-model comparison**

# Compare models that do not involve item removal
# -- Nested model
anova(cfa.model, cfa.model1, cfa.model2, cfa.model3, method = "satorra.bentler.2010")
# X2 difference P-values: significant improvement from model -> model3
# AIC & BIC: reduction in both from model -> model3 indicates model improvement.

# Compare models that involves item removal
# -- Non-nested model
# Compare by AIC and BIC only!
# X2 difference is not accurate for non-nested model.
# lavaan will issue warining!
anova(cfa.model3, cfa.model4, method = "satorra.bentler.2010")
# AIC & BIC: reduction in both from model3 -> model4 indicates model improvement.

# Path diagram
semPaths(cfa.model, 'path', 'std', style = 'lisrel', edge.color = 'black', intercepts = F)
semPaths(cfa.model4, 'path', 'std', style = 'lisrel', edge.color = 'black', intercepts = F)
# Nice diagrams!

