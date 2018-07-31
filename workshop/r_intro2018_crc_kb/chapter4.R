# Graphical exploration of data

# data
library(foreign)
cholest = read.spss("cholest.sav", to.data.frame = TRUE)
str(cholest)
head(cholest)

# library
library(lattice)

# histogram
histogram(~ chol, data = cholest)
histogram(~ chol, data = cholest, col = "yellow")  # col = color

histogram(~ chol | sex,  data = cholest)
histogram(~ chol | categ,  data = cholest)

histogram(~ chol | sex,  data = cholest, layout = c(1, 2))
histogram(~ chol | categ,  data = cholest, layout = c(1, 3))

# boxplot
bwplot(~ chol, data = cholest)
bwplot(sex ~ chol, data = cholest)
bwplot(categ ~ chol, data = cholest)

bwplot(chol ~ sex, data = cholest)  # dif. orientation
bwplot(chol ~ categ, data = cholest)

# bar chart
## prepare the count & %
table_sex_categ = table(cholest$sex, cholest$categ)
df_sex_categ = as.data.frame(table_sex_categ)
colnames(df_sex_categ) = c("Sex", "Category", "Count")
df_sex_categ$Percent = as.data.frame(prop.table(table_sex_categ))$Freq*100

barchart(Count ~ Category | Sex, df_sex_categ, layout = c(2, 1))
barchart(Percent ~ Category | Sex, df_sex_categ, layout = c(2, 1))

barchart(Category ~ Count, groups = Sex, df_sex_categ, 
         auto.key = TRUE)  # auto.key -> auto legend
barchart(Category ~ Percent, groups = Sex, df_sex_categ, auto.key = TRUE)

# scatter plot
xyplot(chol ~ age, groups = sex, data = cholest)
xyplot(chol ~ age, groups = sex, data = cholest, auto.key = TRUE)

# using `graphics` (R base)
hist(cholest$chol)
hist(cholest$chol, col = "yellow")

hist(cholest$chol, freq = F)
lines(density(cholest$chol))  # overlay density curve

par(mfrow = c(1, 2))  # set plot to 2 rows and 1 column
by(cholest$chol, cholest$sex, hist)
par(mfrow = c(1, 1))  # reset back to 1 row and 1 column

boxplot(cholest$chol)
boxplot(cholest$chol ~ cholest$categ)

barplot(df_sex_categ$Percent)
barplot(df_sex_categ$Percent, horiz = T)

plot(cholest$chol ~ cholest$age)
abline(line(cholest$chol ~ cholest$age))

# ggplot2
# install ggplot2 then use `library(ggplot2)`
# can learn from https://ggplot2.tidyverse.org/
