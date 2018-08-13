# 02-logistic

# library
library(foreign)
library(epiDisplay)
library(psych)
library(lattice)
library(rsq)
library(MASS)
library(car)

# data
coronary = read.dta("coronary.dta")
str(coronary)

# slogr, cad ~ gender
codebook(coronary[c("cad", "gender")])
table(coronary$gender, coronary$cad)
cc(coronary$cad, coronary$gender)  # plain OR

slg_cad = glm(cad ~ gender, data = coronary, family = binomial)
summary(slg_cad)
Confint(slg_cad)  # coeff.
exp(Confint(slg_cad))  # OR

# mlogr, cad ~ sbp + chol + age + bmi + gender + race
str(coronary)
coronary = subset(coronary, select = -id) # remove id

# descriptive, by CAD
by(subset(coronary, select = c(sbp, dbp, chol, age, bmi)), coronary$cad, summ)
by(subset(coronary, select = c(race, gender)), coronary$cad, codebook)

# univariable
# again, you can do one-by-one univariable on your own
slg_cad0 = glm(cad ~ 1, data = coronary, family = binomial)
summary(slg_cad0)
names(coronary)
add1(slg_cad0, scope = ~ sbp + dbp + chol + age + bmi + race + gender, 
     test = "LRT")
# all sig. except bmi & race
# choose only vars p-value < 0.25 to proceed in MLogR

# multivariable
# race not included, p-value > 0.25
mlg_cad = glm(cad ~ sbp + dbp + chol + age + bmi + gender, 
              data = coronary, family = binomial)
summary(mlg_cad)

# stepwise
mlg_cad_stepboth = step(mlg_cad, direction = "both")
summary(mlg_cad_stepboth)  # cad ~ dbp + gender
mlg_cad_stepforward = step(slg_cad0, 
                           scope = ~ sbp + dbp + chol + age + bmi + gender, 
                           direction = "forward")
summary(mlg_cad_stepforward)  # cad ~ sbp + gender
mlg_cad_stepback = step(mlg_cad, direction = "backward")
summary(mlg_cad_stepback)  # cad ~ dbp + gender
# compare AICs
AIC(mlg_cad_stepboth, mlg_cad_stepforward)
# cad ~ dbp + gender, gives the lowest AIC
# cad ~ sbp + gender, gives insig. p-value to gender
# mlg_cad1: cad ~ dbp + gender
mlg_cad1 = glm(cad ~ dbp + gender, data = coronary, family = binomial)
summary(mlg_cad1)

# Confounder:
# If we include a variable, and it causes notable change (> 20%) in the
# coefficients of other vars, it is a confounder
# when the confounder is sig. and the main effect var is also sig.
# keep confounder in the model
# 
# Add back possible vars & vars removed before.
#
# Formula for % change = 100 * (model_small - model_large) / model_large
# Hosmer, Lemeshow & Sturdivant (2013)
#
# + age, common demographic confounder
summary(update(mlg_cad1, . ~ . + age))  # longer codes
coef(update(mlg_cad1, . ~ . + age))  # but no need to save into objects
coef(mlg_cad1)
100 * (coef(mlg_cad1) - coef(update(mlg_cad1, . ~ . + age))[1:3]) / 
  coef(update(mlg_cad1, . ~ . + age))[1:3]
# < 20% change
# + chol
summary(update(mlg_cad1, . ~ . + chol))
coef(update(mlg_cad1, . ~ . + chol))
coef(mlg_cad1)
100 * (coef(mlg_cad1) - coef(update(mlg_cad1, . ~ . + chol))[1:3]) / 
  coef(update(mlg_cad1, . ~ . + chol))[1:3]  # [1:3] select vars, exclude new var
# < 20% change
# + bmi
summary(update(mlg_cad1, . ~ . + bmi))
coef(update(mlg_cad1, . ~ . + bmi))
coef(mlg_cad1)
100 * (coef(mlg_cad1) - coef(update(mlg_cad1, . ~ . + bmi))[1:3]) / 
  coef(update(mlg_cad1, . ~ . + bmi))[1:3]
# < 20% change. Again ignore intercept.
# + race
summary(update(mlg_cad1, . ~ . + race))
coef(update(mlg_cad1, . ~ . + race))
coef(mlg_cad1)
100 * (coef(mlg_cad1) - coef(update(mlg_cad1, . ~ . + race))[1:3]) / 
  coef(update(mlg_cad1, . ~ . + race))[1:3]
# < 20% change
# + sbp, bcs it is known to relate to dbp
summary(update(mlg_cad1, . ~ . + sbp))
coef(update(mlg_cad1, . ~ . + sbp))
coef(mlg_cad1)
100 * (coef(mlg_cad1) - coef(update(mlg_cad1, . ~ . + sbp))[1:3]) / coef(update(mlg_cad1, . ~ . + sbp))[1:3]
# > 20% change in dbp, possible confounder!
# cause insig. p-values for both.
# why?
cor(coronary$sbp, coronary$dbp)
# both are highly correlated, so may choose either one in the model,
# redundant variable,
# this actually may fall under multicollinearity issue below.
# Not a confounding issue.

# Chosen model, mlg_cad1: cad ~ dbp + gender
summary(mlg_cad1)
Confint(mlg_cad1)  # 95% CI of the coefficients

# Compare this model with the no-variable model and all-variable model by LR test and AIC comparison,
# LR test
anova(slg_cad0, mlg_cad1, test = "LRT")  # sig. better than no var at all,
# i.e. the Null Model
anova(mlg_cad, mlg_cad1, test = "LRT")  # no sig. dif with all vars model,
# model with 2 vars (dbp & gender) is just as good as full model (with all the vars),
# i.e. the Saturated Model

# AIC
AIC(slg_cad0, mlg_cad1, mlg_cad)
# our final model has the lowest AIC

# multicollinearity, MC
# by looking at the estimates and standard errors, SEs
# when SE > Estimate -- MC problem
# How large? Relatively large... not specific in book.
# Sometimes, the estimates are unusually large, i.e. large ORs
# illogical -- also indicates MC problem
#
# mlg_cad1: cad ~ dbp + gender
summary(mlg_cad1)  # all SEs < Estimates/Coefficients
# Now we have a relook at sbp problem above
# mlg_cad1 + sbp : cad ~ dbp + gender + sbp
summary(update(mlg_cad1, . ~ . + sbp))
# sbp: SE > Estimate
0.01672/0.01386  # = SE 1.2 times > Estimate
# at this point, it is resonable to choose
# mlg_cad1: cad ~ dbp + gender

# interaction, *
summary(glm(cad ~ dbp*gender, data = coronary, family = binomial))
# insig. *

# model fit
# 1. Hosmer-Lemeshow test
# install.packages("ResourceSelection")
library(ResourceSelection)
hl_cad1 = hoslem.test(mlg_cad1$y, mlg_cad1$fitted.values)
hl_cad1  # does not fit, slightly... ideally > 0.05
# usually because small number of variables in the model
cbind(hl_cad1$observed, hl_cad1$expected)
#
# 2. classification table
coronary$cad_prob = mlg_cad1$fitted.values
head(coronary[c("cad", "cad_prob")])
coronary$cad_pred = cut(coronary$cad_prob, breaks = c(-Inf, 0.5, Inf), 
                        labels = c("no cad", "cad"))
head(coronary[c("cad", "cad_prob", "cad_pred")])
table(coronary$cad, coronary$cad_pred)
# correctly classified %
100 * (157 + 3) / length(coronary$cad)  # = 80%
#
# 3. Receiver Operating Characteristic (ROC) curve
roc_cad1 = lroc(mlg_cad1)
roc_cad1$auc
# we assume the model fit based on 2/3 criteria

# rename the selected model to final model
mlg_cad_final = mlg_cad1

# present the results and interpret
summary(mlg_cad_final)
exp(Confint(mlg_cad_final))  # ORs and the 95% CIs
rsq(mlg_cad_final, adj = T)  # R-squared is usually reported for linear regression.
# But R-squared is also available for GLM, in our case logistic regression.
# This is usually known as pseudo-R-squared. In GLM, it is made possible by the
# work of Zhang (2016), the author of "rsq" package.

# predict
coronary$cad_prob1 = predict(mlg_cad_final, type = "response")  # in probability
# converted from logit, by adding type = "response"
head(coronary$cad_prob1)
# you can also use mlg_cad_final$fitted.values
# but as we will see below, we need predict() for new data
str(coronary[c("dbp", "gender")])
# simple, dbp = 110, gender = man
predict(mlg_cad_final, list(dbp = 110, gender = "man"), type = "response")
# probability > 0.5 = cad
# more data points
new_data = data.frame(dbp = c(100, 110, 120, 100, 110, 120),
                      gender = c("man", "man", "man", "woman", "woman", "woman"))
new_data
predict(mlg_cad_final, new_data, type = "response")
new_data$prob_cad = predict(mlg_cad_final, new_data, type = "response")
new_data
new_data$pred_cad = cut(new_data$prob_cad, breaks = c(-Inf, 0.5, Inf),
                        labels = c("no cad", "cad"))
new_data

# Exercise ==============================================#
# use "coronary_large.sav" dataset and repeat the analysis
# =======================================================#
