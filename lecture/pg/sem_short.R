# ====================================
# Structural Equation Modeling (short)
# Author: Wan Nor Arifin
# Updated: 2018-12-15
# ====================================

# Preliminaries

# Load libraries
library(foreign)
library(psych)
library(lavaan)
library(semTools)
library(semPlot)

# Load data set
data = read.spss("Attitude_Statistics v3.sav", F, T)
str(data)
head(data)  # the first 6 observations
describe(data[-1])


# Correlation

# Correlations between:
## Observed variables
# Q4 & Q11
model.c = "
Q4 ~~ Q11
"
corr.c = sem(model.c, data = data, meanstructure = T)  # meanstructure -> display mean
summary(corr.c, fit.measures = T, standardized = T)
semPaths(corr.c, what = "path", whatLabels = "par", edge.color = "black")
cor(data$Q4, data$Q11)
## Latent variables
model.cl = "
F1 =~ Q4 + Q6 + Q7 + Q11
F2 =~ Q8 + Q9 + Q10
"
corr.cl = sem(model.cl, data = data)
summary(corr.cl, fit.measures = T, standardized = T)
semPaths(corr.cl, what = "path", whatLabels = "par", edge.color = "black", layout = "tree2")


# Causal

# Causal effects:
## Observed variables
# Q4 & Q11
model.cs = "
Q4 ~ Q11
"
cause.cs = sem(model.cs, data = data)
summary(cause.cs, fit.measures = T, standardized = T)
lm(formula = Q4 ~ Q11, data = data)  # compare with SLR
semPaths(cause.cs, what = "path", whatLabels = "par", edge.color = "black", rotation = 2)
## Latent variables
model.csl = "
F1 =~ Q4 + Q6 + Q7 + Q11
F2 =~ Q8 + Q9 + Q10
F2 ~ F1
"
cause.csl = sem(model.csl, data = data)
summary(cause.csl, fit.measures = T, standardized = T)
semPaths(cause.csl, what = "path", whatLabels = "par", style = "lisrel", edge.color = "black", 
         rotation = 2, sizeMan = 4, sizeLat = 6, residuals = F)
## Multiple variables
model.cs1 = "
Q4 ~ Q7 + Q11 + Q6
"
cause.cs1 = sem(model.cs1, data = data)
summary(cause.cs1, fit.measures = T, standardized = T)
semPaths(cause.cs1, what = "path", whatLabels = "par", edge.color = "black", rotation = 2)
lm(Q4 ~ Q7 + Q11 + Q6, data = data)  # compare with MLR


# Mediation
model.me = "
Q4 ~ c*Q7 + b*Q11
Q11 ~ a*Q7  # mediator
ab := a*b  # indirect effect
total := c + a*b  # total effect
"
med.me = sem(model.me, data = data)
summary(med.me, fit.measures = T, standardized = T)
semPaths(med.me, what = "path", whatLabels = "name", edge.color = "black", layout = "spring")
semPaths(med.me, what = "path", whatLabels = "par", edge.color = "black", layout = "spring")
model.me1 = "
Q4 ~ c*Q7
"
med.me1 = sem(model.me1, data = data)
summary(med.me1, fit.measures = T, standardized = T)


# Moderation/Interraction
data$Q7.Q8 = data$Q7*data$Q8  # create interraction
head(data)
model.mo = "
Q4 ~ Q7 + Q8 + Q7.Q8
"
mod.mo = sem(model.mo, data = data)
summary(mod.mo, fit.measures = T, standardized = T)  # complete moderation
semPaths(mod.mo, what = "path", whatLabels = "par", edge.color = "black", residuals = F)
lm(Q4 ~ Q7 + Q8 + Q7*Q8, data = data)
