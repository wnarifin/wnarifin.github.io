# ======================
# Mutivariate statistics
# Wan Nor Arifin
# ======================

# library
library(psych)  # for most functions we're going to use
library(epiDisplay)  # for codebook
library(car)  # for Salaries dataset
library(lattice)  # for bwplot

# builtin data
num = attitude
mix = iris
sal = Salaries
str(num)
str(mix)
str(sal)

# descriptive stats
describe(num)
describe(mix[1:4])
codebook(mix[5])

# graphical

## numerical
hist(mix[,1])
multi.hist(mix[1])  # with curve
boxplot(mix[,1])
qqnorm(mix[,1])
qqline(mix[,1])

## categorical
barplot(table(mix[5]), col = rainbow(3, alpha = 0.5))
pie(table(mix[5]), col = rainbow(3, alpha = 0.5))

# normality

## univariate
#-graphical
multi.hist(num)
boxplot(num)
#-stat
describe(num)  # skewness & kurtosis
n = 30
s_se = sqrt(6/n)
k_se = sqrt(24/n)
cbind(skewness = skew(num), "2 x se" = s_se*2, 
      kurtosis = kurtosi(num), "2 x se" = k_se*2)
lapply(num, shapiro.test)

## bivariate
plot(num)

## MV
mardia(num) # note the Mardia's skewness & kurtosis
# or X2 vs Mahalanobis distance
mah2 = mahalanobis(num, colMeans(num), cov(num))
chisq = qchisq(ppoints(30), df = 7)
plot(chisq ~ sort(mah2), xlab = "Mahalanobis distance", ylab = "Chi-square")
abline(0, 1, col = "blue")

# linearity
plot(num[1:3])
plot(num$rating ~ num$complaints)
abline(lm(num$rating ~ num$complaints))

# homoscedasticity

## categorical IV
#-2 levels
bwplot(salary ~ sex, data = sal)
histogram(~ salary | sex, data = sal)
var.test(salary ~ sex, data = sal)  # 2 levels only
#-3 levels
bwplot(Sepal.Width ~ Species, data = mix)
histogram(~ Sepal.Width | Species, data = mix)
bartlett.test(Sepal.Width ~ Species, data = mix)  # > 2 levels allowed

## numerical IV, graphical only
plot(num[1:3])

# transformation
slry = sal$salary

#-right skew
hist(slry)
## try each method for right skew
hist( sqrt(slry) )
hist( log(slry) )
hist( log10(slry) )
hist( 1/slry )

#-create a left skew data, reverse slry
summary(slry)
slry_rev = (max(slry) + min(slry)) - slry
hist(slry_rev)

## try each method for left skew
hist( slry_rev^2 )
hist( slry_rev^3 )
