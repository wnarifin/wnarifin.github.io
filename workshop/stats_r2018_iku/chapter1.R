# Descriptive statistics

# data
library(foreign)

cholest = read.spss("cholest.sav", to.data.frame = TRUE)
str(cholest)
summary(cholest)

# central tendency & dispersion
mean(cholest$chol)
mean(cholest$age)

sd(cholest$chol)
sd(cholest$age)

median(cholest$chol)
median(cholest$age)

IQR(cholest$chol)
IQR(cholest$age)

# proportion
tab_sex = table(cholest$sex)
tab_categ = table(cholest$categ)
tab_sex
tab_categ
str(tab_sex)
str(tab_categ)

prop.table(tab_sex)
prop.table(tab_categ)

prop.table(tab_sex)*100
prop.table(tab_categ)*100

# using sapply
mean_all = sapply(cholest[c("age", "chol", "exercise")], mean)
mean_all
sd_all = sapply(cholest[c("age", "chol", "exercise")], sd)
sd_all
median_all = sapply(cholest[c("age", "chol", "exercise")], median)
median_all
iqr_all = sapply(cholest[c("age", "chol", "exercise")], IQR)
iqr_all

cbind(Mean = mean_all, SD = sd_all, Median = median_all, IQR = iqr_all)
rbind(Mean = mean_all, SD = sd_all, Median = median_all, IQR = iqr_all)

tab_sc = sapply(cholest[c("sex", "categ")], table)
tab_sc
prop_sc = sapply(tab_sc, prop.table)
prop_sc
per_sc = sapply(tab_sc, function(x) prop.table(x)*100)
per_sc

# using codebook
library(epiDisplay)
codebook(cholest)
codebook(cholest[c("chol", "age", "exercise")])

# using describe
library(psych)
describe(cholest[c("chol", "age", "exercise")])

# by groups
by(cholest$chol, cholest$sex, mean)
by(cholest$chol, cholest$sex, sd)

by(cholest$chol, cholest$categ, mean)
by(cholest$chol, cholest$categ, sd)

by(cholest$chol, cholest$sex, median)
by(cholest$chol, cholest$sex, IQR)

by(cholest$chol, cholest$categ, median)
by(cholest$chol, cholest$categ, IQR)

# cross-tabulation
tab_sex_categ = table(Gender = cholest$sex, Category = cholest$categ)
tab_sex_categ

prop_sex_categ = prop.table(tab_sex_categ)
prop_sex_categ

per_sex_categ = prop_sex_categ*100
per_sex_categ

margin_sex_categ = addmargins(tab_sex_categ)
margin_sex_categ

addmargins(prop_sex_categ)
addmargins(per_sex_categ)

# extra, by "sex"
addmargins(tbl$male)
addmargins(tbl$female)
prop.table(tbl$male)*100
prop.table(tbl$female)*100
addmargins(prop.table(tbl$male)*100)
addmargins(prop.table(tbl$female)*100)
