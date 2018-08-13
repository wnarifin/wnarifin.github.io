# 01-linear

# library
library(foreign)
library(epiDisplay)
library(psych)
library(lattice)
library(rsq)
library(MASS)
library(car)

# data prelim
"
coronary = read.spss('slog.sav', T, T)
str(coronary)
codebook(coronary)
"

# edit data
"
multi.hist(coronary[3:7])
plot(chol ~ age, data = coronary)
table(coronary$bmi)

subset(coronary, bmi < 30, bmi)
coronary[coronary$bmi < 30, 'bmi'] = rnorm(13, 36.8, 3.8)
hist(coronary$bmi)
coronary_ed = coronary

coronary = coronary_ed
hist(coronary$age)
table(coronary$age < 45)
sel_age = sample(which(coronary$age < 45), 50)
coronary[sel_age, 'age'] = coronary[sel_age, 'age'] + sample(3:7, 1)
hist(coronary$age)
coronary_ed2 = coronary
write.dta(coronary, 'coronary.dta')
str(read.dta('coronary.dta'))
"
"
hist(coronary$sbp)
table(coronary$sbp)
lm_coronary = lm(sbp ~ dbp, data = coronary)
summary(lm_coronary)
sbp_new = round(predict(lm_coronary, list(dbp = coronary[coronary$sbp > 170, 'dbp'])))
coronary[coronary$sbp > 170, 'sbp'] = sbp_new
hist(coronary$sbp)
write.dta(coronary, 'coronary.dta')
"

# data
coronary = read.dta("coronary.dta")
str(coronary)

# slr, chol ~ dbp
summ(coronary[c("chol", "dbp")])
multi.hist(coronary[c("chol", "dbp")])
par(mfrow = c(1, 2))
mapply(boxplot, coronary[c("chol", "dbp")], 
       main = colnames(coronary[c("chol", "dbp")]))
par(mfrow = c(1, 1))
slr_chol = glm(chol ~ dbp, data = coronary)
summary(slr_chol)
Confint(slr_chol)  # 95% CI
rsq(slr_chol, adj = T)
plot(chol ~ dbp, data = coronary)
abline(slr_chol)

# mlr, chol ~ sbp + dbp + race + gender
str(coronary)
coronary = subset(coronary, select = -c(id, cad, age)) # remove id, cad, age from our data
# we're not going to use them, easier to specifiy model

# descriptive
summ(coronary[c("chol", "sbp", "dbp", "bmi")])
codebook(coronary[c("race", "gender")])

plot(coronary)
multi.hist(coronary[c("chol", "sbp", "dbp", "bmi")])
par(mfrow = c(2, 2))
mapply(boxplot, coronary[c("chol", "sbp", "dbp", "bmi")], 
       main = colnames(coronary[c("chol", "sbp", "dbp", "bmi")]))
par(mfrow = c(1, 1))
par(mfrow = c(1, 2))
boxplot(chol ~ race, data = coronary)
boxplot(chol ~ gender, data = coronary)
par(mfrow = c(1, 1))

# univariable
# can do univariable one-by-one on your own
slr_chol0 = glm(chol ~ 1, data = coronary)
summary(slr_chol0)
names(coronary)
add1(slr_chol0, scope = ~ sbp + dbp + bmi + race + gender, test = "LRT")
# all sig. except gender
# choose only vars p-value < 0.25 to proceed in MLR

# multivariable
mlr_chol = glm(chol ~ sbp + dbp + bmi + race, data = coronary)
#mlr_chol = glm(chol ~ ., data = coronary)  # shortcut
summary(mlr_chol)
rsq(mlr_chol, adj = T)

# stepwise
mlr_chol_stepboth = step(mlr_chol, direction = "both")
summary(mlr_chol_stepboth)  # racechinese marginally sig.
mlr_chol_stepforward = step(slr_chol0, scope = ~ sbp + dbp + bmi + race + gender, 
                            direction = "forward")
summary(mlr_chol_stepforward)  # same with both
mlr_chol_stepback = step(mlr_chol, direction = "backward")
summary(mlr_chol_stepback)  # same with both
# choose: chol ~ dbp + race, has the lowest AIC
mlr_chol1 = glm(chol ~ dbp + race, data = coronary)
summary(mlr_chol1)

# Confounder:
# If we include a variable, and it causes notable change (> 20%) in the
# coefficients of other vars, it is a confounder
# when the confounder is sig. and the main effect var is also sig.
# keep confounder in the model
#
# Formula for % change = 100 * (model_small - model_large) / model_large
# Hosmer, Lemeshow & Sturdivant (2013)
#
# start by including common demo adjustment, + gender
mlr_chol2 = glm(chol ~ dbp + race + gender, data = coronary)
summary(mlr_chol2)  # higher AIC, gender insig.
coef(mlr_chol2); coef(mlr_chol1)
100 * (coef(mlr_chol1) - coef(mlr_chol2)[1:4])/coef(mlr_chol2)[1:4]  # change < 20%
# no notable change in coeffs, gender is not a confounder
# can you try by adding sbp & bmi to mlr_chol1 & see what happens to the coeffs?
# we will use `update()` function here
mlr_chol3 = update(mlr_chol1, . ~ . + sbp)
summary(mlr_chol3)  # higher AIC, sbp insig.
coef(mlr_chol3); coef(mlr_chol1)
100 * (coef(mlr_chol1) - coef(mlr_chol3)[1:4])/coef(mlr_chol3)[1:4]  # change < 20%
# no notable change in coeffs, sbp is not a confounder
mlr_chol4 = update(mlr_chol1, . ~ . + bmi)
summary(mlr_chol4)  # slighly higher AIC, bmi insig.
coef(mlr_chol4); coef(mlr_chol1)
100 * (coef(mlr_chol1) - coef(mlr_chol4)[1:4])/coef(mlr_chol4)[1:4]  # change < 20%
# no notable change in coeffs of other vars (ignore intercept!)
# bmi is not a confounder
# Our chosen model
# mlr_chol1: chol ~ dbp + race
summary(mlr_chol1)
Confint(mlr_chol1)  # 95% CI of the coefficients
rsq(mlr_chol1)
# compare with the no var model
# LR test
anova(slr_chol0, mlr_chol1, test = "LRT")  # sig. better than no var at all!
# model with no var at all is called Null Model
anova(mlr_chol, mlr_chol1, test = "LRT")  # no sig. dif with all vars model,
# model with 2 vars (dbp & race) is just as good as full model (with all the vars)
# model with all vars is called Saturated Model
# AIC
AIC(slr_chol0, mlr_chol1, mlr_chol)
# our final model has the lowest AIC

# Multicollinearity, MC
# by Variance Inflation Factor (VIF)
vif(mlr_chol1)  # all < 10

# Interaction, *
summary(glm(chol ~ dbp*race, data = coronary))  # dbp*race not sig.
# in R, it is easy to fit interaction by *
# dbp*race will automatically include all vars involved i.e. equal to
# glm(chol ~ dbp + race + dbp:race, data = coronary)
# use : to just include interaction

# model fit: residuals
rraw_chol = resid(mlr_chol1)  # unstandardized
multi.hist(rraw_chol)

rstd_chol = rstandard(mlr_chol1)  # standardized residuals
pstd_chol = scale(predict(mlr_chol1))  # standardized predicted values
plot(rstd_chol ~ pstd_chol, xlab = "Std predicted", ylab = "Std residuals")
abline(0, 0)  # normal, linear, equal variance

plot(rraw_chol ~ coronary$dbp, xlab = "DBP", ylab = "Raw Residuals")
abline(0, 0)

# rename the selected model
mlr_chol_final = mlr_chol1

# present the results and interpret
summary(mlr_chol_final)
Confint(mlr_chol_final)  # 95% CI of the coefficients
rsq(mlr_chol_final, adj = T)

# predict
coronary$pred_chol = predict(mlr_chol_final)
head(coronary)
str(coronary[c("dbp", "race")])
# simple, dbp = 90, race = indian
predict(mlr_chol_final, list(dbp = 90, race = "indian"))
# more data points
new_data = data.frame(dbp = c(90, 90, 90), race = c("malay", "chinese", "indian"))
new_data
predict(mlr_chol_final, new_data)
new_data$pred_chol = predict(mlr_chol_final, new_data)
new_data

## Exercise ====================##
## add age in model as exercise ##
# can do univariable one-by-one (if you want)
# reload the original data, minus id & cad
coronary = read.dta("coronary.dta")
coronary = subset(coronary, select = -c(id, cad))
slr_chol0 = glm(chol ~ 1, data = coronary)
summary(slr_chol0)
names(coronary)
add1(slr_chol0, scope = ~ sbp + dbp + age + bmi + race + gender, test = "LRT")
# all sig.

mlr_chol = glm(chol ~ sbp + dbp + age + bmi + race + gender, data = coronary)
#mlr_chol = glm(chol ~ ., data = coronary)  # shortcut
summary(mlr_chol)
rsq(mlr_chol, adj = T)
# stepwise
mlr_chol_stepboth = step(mlr_chol, direction = "both")
summary(mlr_chol_stepboth)  # racechinese marginally sig.
mlr_chol_stepforward = step(slr_chol0, 
                            scope = ~ sbp + dbp + age + bmi + race + gender, 
                            direction = "forward")
summary(mlr_chol_stepforward)  # highly sig.
mlr_chol_stepback = step(mlr_chol, direction = "backward")
summary(mlr_chol_stepback)  # same with both
# compare models
anova(mlr_chol_stepboth, mlr_chol_stepforward, test = "LRT")
# no dif. between the models, both are comparable
# compare AIC
AIC(mlr_chol_stepboth, mlr_chol_stepforward)
# choose mlr_model_stepforward: chol ~ dbp + age, has the lowest AIC
mlr_chol1 = glm(chol ~ dbp + age, data = coronary)
summary(mlr_chol1)
rsq(mlr_chol1, adj = T)
# try including race, bcs it was also chosen in mlr_stepboth & _stepback
mlr_chol2 = glm(chol ~ dbp + age + race, data = coronary)
summary(mlr_chol2)

# Confounder:
# If we include a variable, and it causes notable change (>20%) in the
# coefficients of other vars, it is a confounder
# race is a confounder, why?
by(coronary$age, coronary$race, mean)
# clearly age dif. by race
# so modeling needs clinical judgement, so we need to justify
# adding causes insig. in race
rsq(mlr_chol2, adj = T)
# try including common demo adjustment, age & gender
mlr_chol3 = glm(chol ~ dbp + age + gender, data = coronary)
summary(mlr_chol3)  # gender bcm insig., higher AIC, not a confounder
rsq(mlr_chol3, adj = T)
# add1, final look at change in AIC
add1(mlr_chol1, scope = ~ . + sbp + bmi + gender + race, test = "LRT")
# none sig. on adding
# final decision
mlr_chol_final = glm(chol ~ dbp + race, data = coronary)
summary(mlr_chol_final)
Confint(mlr_chol_final)  # 95% CI
# chose race instead of age bcs there seem to be imbalance in age in the sample
# may not be fair to add age given the data set
## ========================================= ##
