# 02-km

# KM one group

# analysis

# library
library(survival)
library(epiDisplay)
library(coin)

# data
?aml  # about the dataset
acute = aml
str(acute)
#knitr::kable(acute)

# explore
codebook(acute)
table(acute$status)  # number of events

# KM
sur_aml = survfit(Surv(time, status) ~ 1, data = acute)
sur_aml  # median survival time = 27 (95%CI: 18, 45)
summary(sur_aml)  # noted the `survival`` column, the cumulative survival
plot(sur_aml, conf.int = F, 
     main = "Kaplan-Meier survival curve for AML", 
     xlab = "Time (months)", ylab = "Survival (probability)")
# `conf.int = F` to supress 95% CI line, can remove the argument
abline(0.5, 0, lty = 2)

# KM for groups

# data
?glioma  # about the dataset
gli = glioma
str(gli)
gli4 = subset(gli, histology == "GBM")  # grade 4 glioma
str(gli4)
#knitr::kable(gli4[c("time", "event", "group")])

# explore
codebook(gli4)

# KM
sur_gli4 = survfit(Surv(time, event) ~ group, data = gli4)
sur_gli4  # median survival times/group, 0.95UCL cannot be estimated
summary(sur_gli4)
plot(sur_gli4, main = "Kaplan-Meier survival curve for glioma grade 4", 
     xlab = "Time (months)", ylab = "Survival (probability)", lty = c(1, 2))
#, col = c("blue", "red"))
abline(0.5, 0, lty = 2, col = "blue")
legend(45, 0.9, c("Control", "RIT"), lty = c(1, 2))
#, col = c("blue", "red"))

# Log-rank test
survdiff(Surv(time, event) ~ group, data = gli4)

# exercise
# 1. Analyze gli data for histology == "Grade3".
gli3 = subset(gli, histology == "Grade3")
codebook(gli3)
sur_gli3 = survfit(Surv(time, event) ~ group, data = gli3)
sur_gli3  # median survival times/group, 0.95UCL cannot be estimated
summary(sur_gli3)
plot(sur_gli3, main = "Kaplan-Meier survival curve for glioma grade 4", 
     xlab = "Time (months)", ylab = "Survival (probability)", lty = c(1, 2))
#, col = c("blue", "red"))
abline(0.5, 0, lty = 2, col = "blue")
legend(40, 0.3, c("Control", "RIT"), lty = c(1, 2))
survdiff(Surv(time, event) ~ group, data = gli3)

# 2. Again, analyze using `aml` data from `survival` package. This time compare
# the groups (`x` variable in the dataset).
# KM
sur_aml2 = survfit(Surv(time, status) ~ x, data = acute)
sur_aml2  # median survival time = 27 (95%CI: 18, 45)
summary(sur_aml2)  # noted the `survival`` column, the cumulative survival
plot(sur_aml2, conf.int = F, lty = 1:2,
     main = "Kaplan-Meier survival curve for AML", 
     xlab = "Time (months)", ylab = "Survival (probability)")
# `conf.int = F` to supress 95% CI line, can remove the argument
abline(0.5, 0, lty = 2)
legend(100, 1, c("Maintained", "Nonmaintained"), lty = 1:2)
survdiff(Surv(time, status) ~ x, data = acute)
