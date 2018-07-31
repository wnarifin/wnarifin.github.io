# Correlation

# data
library(foreign)
cholest = read.spss("cholest.sav", to.data.frame = TRUE)
str(cholest)
head(cholest)

library(lattice)
xyplot(chol ~ age, data = cholest)
cor.test(~ chol + age, data = cholest)

# Spearman's correlation
cor.test(~ chol + age, data = cholest, method = "spearman")
