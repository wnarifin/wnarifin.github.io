#-------------------------------------------
# Confirmatory Factor Analysis & Reliability
# Author: Wan Nor Arifin
# Updated: 2019-07-29
#-------------------------------------------

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
data.cfa = data[c("Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11")]
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
PA2 =~ Q8 + Q9 + Q10
"

## Step 2

# **Fit the model**
cfa.model = cfa(model, data = data.cfa, estimator = "MLR")
# cfa.model = cfa(model, data = data.cfa, std.lv = 1)  # factor variance = 1
summary(cfa.model, fit.measures = T, standardized = T)
parameterEstimates(cfa.model)  # unstandardized, to view the 95% CI
standardizedSolution(cfa.model)  # standardized, to view the SE of FL
varTable(cfa.model)  # mean and variance for each item from the cfa model, and
# summary

# **Results**

# 1. Overall model fit - by fit indices.
# Good model fit based on all indices,
# with the exception of the upper 90% CI of robust RMSEA = 0.112.
# There is no CFit _P_-value for robust RMSEA.
# 
# 3. Parameter estimates
# Remember `Std.all`
# All FLs > 0.5
# and the factor correlation < 0.85.
# There is no problem with the item quality and the factors are distinct.

mi = modificationIndices(cfa.model)
subset(mi, mi > 3.84)
sr = residuals(cfa.model, type = "standardized")
sr
# 2. Localized areas of misfit
# Modification indices:
# Four suggested specifications with MIs > 3.84.
# Ignore `PA1 =~  Q8` and `PA2 =~  Q5` based on content,
# not justifiable to allow these two items specified under other factors.
# `Q9 ~~ Q10` is justifiable, based on the wording "is important".
# `Q5 ~~ Q11` is not justifiable.
# Residuals:
# Q5 has an SR with Q8 (SR = 3.397).
# May also focus on Q5. Avoid Q8 because there are only three items in the factor and FL Q8 > Q5.

## Step 3

# **Model revision**

# _Revision 1_: Based on MI, `Q9 ~~ Q10`?

# Both from PA2, reasonable by the wording of the questions.
model1 = "
PA1 =~ Q4 + Q5 + Q6 + Q7 + Q11
PA2 =~ Q8 + Q9 + Q10
Q9 ~~ Q10
"
cfa.model1 = cfa(model1, data = data.cfa, estimator = "MLR")
summary(cfa.model1, fit.measures=T, standardized=T)
# The upper 90% CI of RMSEA is smaller, but
# we have a serious Heywood case here! Q8 FL = 1.522.
# Thus this solution is not acceptable.

# _Revision_ 2: Remove Q5?

# Because Q5 has an SR with Q8.
model2 = "
PA1 =~ Q4 + Q6 + Q7 + Q11
PA2 =~ Q8 + Q9 + Q10
"
cfa.model2 = cfa(model2, data = data.cfa, estimator = "MLR")
summary(cfa.model2, fit.measures=T, standardized=T)
# The upper 90% CI of RMSEA has reduced from 0.112 to 0.104.
# The FLs and factor correlation are acceptable. No Heywood's case.
mi2 = modificationIndices(cfa.model2)
subset(mi2, mi.scaled > 3.84)
sr2 = residuals(cfa.model2, type="standardized"); sr2
# There is no SR > 2.58.
# So we may stop at "model2",
# although the upper 90% CI of RMSEA is still > 0.08,
# but there is no more localized areas of misfit by SR.

# **Model-to-model comparison**

# Because "model2" is not nested in "model",
# we compare mainly by AIC and BIC,
# and additionally by X2 difference
# (in our case scaled X2 difference),
anova(cfa.model, cfa.model2, method = "satorra.bentler.2010")
# Clearly, the AIC and BIC are reduced
# ("model2" [without Q5] vs "model" [with Q5]).
# The X2 difference is significant, which indicates an improvement in model fit.

# Construct reliability

# **Raykov's rho**

# Look at the `omega` row in the output,
rel.model2 = reliability(cfa.model2)
print(rel.model2, digits = 3)
# Raykov's rho (the `omega`):
# PA1 = 0.808,
# PA2 = 0.836.
# Both factors are reliable.

# Path diagram

semPaths(cfa.model2, 'path', 'std', style = 'lisrel', edge.color = 'black', intercepts = F)
