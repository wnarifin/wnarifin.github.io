#-----------------------------------
# Repeated Categorical Data Analysis
#-----------------------------------

# Revision
#---------
# DV: Binary, Measure: 2x, Analysis: McNemar
# DV: Binary, Measure: >2x, Analysis: Cochran's Q
# DV: Multinomial, Measure: 2x, Analysis: Marginal Homogeneity Test
# DV: Binary/Multinominal, Measure: 2x & >, Analysis: Generalized Estimating Equations (GEE)

#-McNemar
# DV: Binary, Measure: 2x
# PM rating (Agresti, pg409), n=1600
"Data:
            second
first        approve disapprove
  approve        794        150
  disapprove      86        570
"
first = factor(rep(c(1,2), c(794+150, 86+570)), labels = c("approve", "disapprove"))
second = factor(rep(c(1,2,1,2), c(794, 150, 86, 570)), labels = c("approve", "disapprove"))
tab = table(first, second); tab
mcnemar.test(tab)

#-Cochran's Q
# DV: Binary, Measure: >2x
lect = read.csv("lect.csv")
lect$student = as.factor(lect$student)
lect$lecturer = as.factor(lect$lecturer)
str(lect)
lect
library(RVAideMemoire)
cochran.qtest(understanding ~ lecturer | student, data = lect)
# set outcome as factor for mh_test() function
lect$understanding = as.factor(lect$understanding)
str(lect)
lect
library(coin)
mh_test(understanding ~ lecturer | student, data = lect)


#-Marginal Homogeneity Test (Stuart-Maxwell test) Agresti, pg422
# DV: Multinomial, Measure: 2x
# My stats lecture understanding level, n=200
"Data:
              after.lecture
before.lecture confused so-so understand
    confused         12     8         80
    so-so            10    10         20
    understand        5     8         47
"
before.lecture = factor(rep(c(1,2,3), c(12+8+80, 10+10+20, 5+8+47)), labels = c("confused", "so-so", "understand"))
after.lecture = factor(rep(c(1,2,3,1,2,3,1,2,3), c(12,8,80, 10,10,20, 5,8,47)), labels = c("confused", "so-so", "understand"))
tabq = table(before.lecture, after.lecture); tabq
library(coin)
mh_test(tabq)  # nominal
mh_test(tabq, scores = list(response = 1:3))  # ordinal

#-GEE
# DV: Binary/Multinominal, Measure: 2x & >
# Depression, Agresti, pg459
ID = rep(1:340, each = 3)
Diagnosis = factor( rep(c(1,2), c(150*3, 190*3)), labels = c("Mild", "Severe") )
Treatment = factor( rep(c(1,2,1,2), c(80*3, 70*3, 100*3, 90*3)), labels = c("Standard", "New Drug") )
Time = rep(0:2, times = 340)
Response = unlist( c(
  rep( list( c(1,1,1), c(1,1,0), c(1,0,1), c(1,0,0), c(0,1,1), c(0,1,0), c(0,0,1), c(0,0,0) ), c(16,13,9,3, 14,4,15,6) ),
  rep( list( c(1,1,1), c(1,1,0), c(1,0,1), c(1,0,0), c(0,1,1), c(0,1,0), c(0,0,1), c(0,0,0) ), c(31,0,6,0, 22,2,9,0) ),
  rep( list( c(1,1,1), c(1,1,0), c(1,0,1), c(1,0,0), c(0,1,1), c(0,1,0), c(0,0,1), c(0,0,0) ), c(2,2,8,9, 9,15,27,28) ),
  rep( list( c(1,1,1), c(1,1,0), c(1,0,1), c(1,0,0), c(0,1,1), c(0,1,0), c(0,0,1), c(0,0,0) ), c(7,2,5,2, 31,5,32,6) )
) )
# 1 = normal, 0 = abnormal
depress = data.frame(ID, Diagnosis, Treatment, Time, Response)
head(depress); tail(depress)
str(depress)

# GEE
library(geepack)  # geeglm
library(MuMIn)  # QIC
library(doBy)  # esticon(), confint doesn't work for GEE
# intercept only model
gee.model0 = geeglm(Response ~ 1, family = binomial, data = depress, id = ID, corstr = "exchangeable")
summary(gee.model0)
QIC(gee.model0)

# 1 var models
gee.model1 = geeglm(Response ~ Diagnosis, family = binomial, data = depress, id = ID, corstr = "exchangeable")
summary(gee.model1)
QIC(gee.model1)

gee.model1a = geeglm(Response ~ Treatment, family = binomial, data = depress, id = ID, corstr = "exchangeable")
summary(gee.model1a)
QIC(gee.model1a)

gee.model1b = geeglm(Response ~ Time, family = binomial, data = depress, id = ID, corstr = "exchangeable")
summary(gee.model1b)
QIC(gee.model1b)

# All 3 vars sig.; 3 var model
gee.model3 = geeglm(Response ~ Diagnosis + Treatment + Time, family = binomial, data = depress, id = ID, corstr = "exchangeable")
summary(gee.model3)
QIC(gee.model3)
Confint(gee.model3)
gee.est3 = esticon(gee.model3, diag(4)); gee.est3

# Tx*Time interaction
gee.model4 = geeglm(Response ~ Diagnosis + Treatment*Time, family = binomial, data = depress, id = ID, corstr = "exchangeable")
summary(gee.model4)
QIC(gee.model4)
gee.est4 = esticon(gee.model4, diag(5)); gee.est4
with(gee.est4, cbind(OR = exp(Estimate), LL = exp(Lower), UL = exp(Upper)))
    