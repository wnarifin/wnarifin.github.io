# 03-cox

# 

# analysis

# library
library(survival)
library(epiDisplay)
library(coin)
library(TH.data)
library(car)

# Before we start with Cox PH model,
# we want to obtain the cumulative hazard and survival
# until time t using coxph() on one group
# Following our previous one-group survival analysis `sur_aml` on `aml`
acute = aml
cox_aml = coxph(Surv(time, status) ~ 1, data = acute)
sur_cox_aml = survfit(cox_aml)
summary(sur_cox_aml)
basehaz(cox_aml)  # also try -log(sur_cox_aml$surv) = H_t

# data
?lung  # about the dataset
lca = na.omit(lung)  # omit subjects with missing data
str(lca)
table(lca$status)  # status: 1=censored, 2=dead
lca$status = lca$status - 1  # turn to our usual 0/1
lca$sex = factor(lca$sex, levels = 1:2, labels = c("male", "female"))
table(lca$ph.ecog)  # only one obs. = 3, set to 2
lca[lca$ph.ecog == 3, ]$ph.ecog = 2
lca$ph.ecog = factor(lca$ph.ecog)
str(lca$ph.ecog)

# explore
codebook(lca)
table(lca$status)  # number of events

# univariable
cox_lca0 = coxph(Surv(time, status) ~ 1, data = lca)  # empty model
summary(cox_lca0)  # remember, no intercept in Cox PH, so there's nothing here
names(lca)
cat(names(lca), sep = " + ")  # makes our life easier to copy all the var names
# for current analysis, we are not using `ph.karno` and `pat.carno`
add1(cox_lca0, scope = ~ age + sex + ph.ecog + meal.cal + wt.loss, 
     test = "Chisq")  # LR test
# since `meal.cal` and `wt.loss`'s P-values < 0.25, we skip these variables
# from multivariable model

# multivariable model
# we include the selected variables into our multivariable model
cox_lca = coxph(Surv(time, status) ~ age + sex + ph.ecog, data = lca)
cox_lca  # basic results
# focus on the sig. of the included vars
# `age` not sig.
# the results also give the HRs i.e. `exp(coef)`
# `likelihood ratio test`, i.e. LR test of the present model vs empty model.
# more details
summary(cox_lca)

# stepwise
cox_lca_stepboth = step(cox_lca, direction = "both")
cox_lca_stepboth
cox_lca_stepforward = step(cox_lca0, scope = ~ age + sex + ph.ecog, direction = "forward")
cox_lca_stepforward
cox_lca_stepback = step(cox_lca, direction = "backward")
cox_lca_stepback
# all gives same set of variables
# sex + ph.ecog
# name it as cox_lca1
cox_lca1 = cox_lca_stepboth
AIC(cox_lca1)

# we skip confounder checking this time, you may do as an exercise

# compare with empty and full model
AIC(cox_lca0, cox_lca1, cox_lca)  # lower in AIC than 3 vars model
# no AIC for empty model in Cox PH because there's no intercept
anova(cox_lca0, cox_lca1, cox_lca, test = "Chisq")  # no diff. from 3 vars full model

# multicollinearity, MC
cox_lca1
# there's no large SE for all the vars

# # interaction, *
add1(cox_lca1, scope = ~ . + sex * ph.ecog, test = "Chisq")
# insig. *

# Proportional hazards assumption
# test for constant coefficients over time
cox.zph(cox_lca1)  # pg.ecog2 hazard not proportionate
# global test of constant coefficient over time not sig. -- proportionate

# plots
# plot of coefficient over time
# sex=female
plot(cox.zph(cox_lca1), var = 1)
abline(coef(cox_lca1)[1], 0, lty = 2)
# ph.ecog=1
plot(cox.zph(cox_lca1), var = 2)
abline(coef(cox_lca1)[2], 0, lty = 2)
# ph.ecog=2
plot(cox.zph(cox_lca1), var = 3)
abline(coef(cox_lca1)[3], 0, lty = 2)
# the points scattered fairly equally above and below the estimated coefficient
# lines over time. The points jumping above and below maybe because of 
# categorical predictors, may look better if we have numerical predictors

# KM curve plot
# sex
sur_lca_sex = survfit(Surv(time, status) ~ sex, data = lca)
sur_lca_sex
plot(sur_lca_sex, xlab = "Time (days)", 
     ylab = "Survival", lty = 1:2)
legend(800, 1, c("male", "female"), lty = 1:2)
# clearly parallel lines = proportionate hazards
# ph.ecog
sur_lca_ph.ecog = survfit(Surv(time, status) ~ ph.ecog, data = lca)
sur_lca_ph.ecog
plot(sur_lca_ph.ecog, xlab = "Time (days)", 
     ylab = "Survival", lty = 1:3)
legend(900, 0.5, c("0", "1", "2"), lty = 1:3)
# ph.ecog = 2 looks less parallel to the other two

# Log-minus-log plot
# sex
plot(sur_lca_sex, fun = "cloglog", xlab = "Time (days)", 
     ylab = "Log minus log survival function", lty = 1:2)
legend(5, 0.5, c("male", "female"), lty = 1:2)
# parallel, esp for time period of > 50 days
# ph.ecog
plot(sur_lca_ph.ecog, fun = "cloglog", xlab = "Time (days)", 
     ylab = "Log minus log survival function", lty = 1:3)
legend(5, 0.5, c("0", "1", "2"), lty = 1:3)
# at longer time period > 100 days, the lines look more parallel

# proportionate? so-so

# Additional plot: Cumulative hazard, H(t) plot
# sex
plot(sur_lca_sex, fun = "cumhaz", xlab = "Time (days)", 
     ylab = "Cumulative hazard", lty = 1:2)
legend(5, 3.0, c("male", "female"), lty = 1:2)
# ph.ecog
plot(sur_lca_ph.ecog, fun = "cumhaz", xlab = "Time (days)", 
     ylab = "Cumulative hazard", lty = 1:3)
legend(5, 2.5, c("0", "1", "2"), lty = 1:3)

# Interpretation
# final model
cox_lca_final = cox_lca1
summary(cox_lca_final)
# Female has lower hazard with HR = 0.60 (40% lower) than male, controlling for other predictors.
# Ecog score 1 has higher hazard with HR = 1.38 (38% higher) than ECOG score 0, controlling for other predictors.
# Ecog score 2 has higher hazard with HR = 2.55 (155% higher) than ECOG score 0, controlling for other predictors.

# Model equation
# log(h(t) / h0(t)) = -0.51 * sex (female) + 0.32 * ph.ecog (= 1) + 0.94 * ph.ecog (= 2)
log_hr0 = -0.51 * 0 + 0.32 * 0 + 0.94 * 0; log_hr0  # = 0; baseline logHR0
exp(log_hr0)  # HR0 = 1
log_hr1 = -0.51 * 1 + 0.32 * 0 + 0.94 * 1; log_hr1  # for sexfemale=1, ph.ecog=2
exp(log_hr1)  # HR1 = 1.54

# prediction
# HR & hazard
# We start by adding predicted hazard to our sample,
lca$hazard = predict(cox_lca_final, type = "risk")
# to add HR, we need to find h0(t), the baseline hazard by predicting for
# sex = 0 ("male"), ph.ecog = 0 (i.e. dummy variables ph.ecog1 = 0, ph.ecog2 = 0)
h0_t = predict(cox_lca_final, list(sex = "male", ph.ecog = "0"), type = "risk")
h0_t  # h0(t)
# HR = h(t)/h0(t)
lca$hr = lca$hazard/h0_t
# view the first 20 observations in the sample,
head(lca[c("sex", "ph.ecog", "time", "status", "hazard", "hr")], 20)
# extra...
# alternative formula h0(t) = h(t)/e^HR
coef(cox_lca_final)
e_hr_fem = exp(coef(cox_lca_final)[1]*1 + coef(cox_lca_final)[2]*0 + coef(cox_lca_final)[3]*0)
hr_fem = predict(cox_lca_final, list(sex = "female", ph.ecog = "0"), type = "risk")
hr_fem/e_hr_fem  # = 0.8355948

# simple, sex = "female", ph.ecog = "2"
predict(cox_lca_final, list(sex = "female", ph.ecog = "2"), type = "risk")  # hazard
exp(coef(cox_lca_final)[1]*1 + coef(cox_lca_final)[2]*0 + coef(cox_lca_final)[3]*1)  # HR
predict(cox_lca_final, list(sex = "female", ph.ecog = "2"), type = "risk")/h0_t  # HR
# we utilize the baseline hazard we had before
# data frame
new_data = data.frame(sex = c("male", "male", "male", "female", "female", "female"),
                      ph.ecog = c("0", "1", "2", "0", "1", "2"))
new_data
new_hazard = hazard = predict(cox_lca_final, new_data, type = "risk")
new_hr = new_hazard/h0_t
data.frame(new_data, hazard = round(new_hazard, 3), hr = round(new_hr, 3))
# by formula
new_data$sex1 = 0
new_data$ph.ecog1 = 0
new_data$ph.ecog2 = 0
new_data[new_data$sex == "female", "sex1"] = 1
new_data[new_data$ph.ecog == 1, "ph.ecog1"] = 1
new_data[new_data$ph.ecog == 2, "ph.ecog2"] = 1
new_data
betas = coef(cox_lca_final)
hr = exp(betas[1]*new_data$sex1 + betas[2]*new_data$ph.ecog1 + betas[3]*new_data$ph.ecog2)
data.frame(new_data, hr = hr)

# Median survival times and survival probabilities
# http://www.drizopoulos.com/courses/emc/ep03_%20survival%20analysis%20in%20r%20companion
# simple, sex = "female", ph.ecog = "2"
new_cox1 = survfit(cox_lca_final, newdata = list(sex = "female", ph.ecog = "2"))
new_cox1  # median survival time
summary(new_cox1, times = 100)  # survival at 100 days
summary(new_cox1, times = c(100, 200, 300))  # survival at 100, 200 and 300 days
# more data points
new_data = data.frame(sex = c("male", "male", "male", "female", "female", "female"),
                      ph.ecog = c("0", "1", "2", "0", "1", "2"))
new_data
new_cox2 = survfit(cox_lca_final, newdata = new_data)
new_cox2  # median survival times
summary(new_cox2, times = 100)
summary(new_cox2, times = c(100, 200, 300))

# exercise
# 1. Now, include `ph.kano` and `pat.karno` in the multivariable analysis.
# What do you get? *Hint: Interaction?
# 1. Perform Cox PH on builtin `GBSG2` dataset (PH.data package). 