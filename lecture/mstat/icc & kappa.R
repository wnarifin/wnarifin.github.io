# ICC

# libraries
library(foreign)
library(irr)  # icc, kappa2
library(psych)  # ICC, cohen.kappa

# data set
data1 = read.spss("ICC_BP Lecture.sav", F, T)
head(data1)
data.sbp = data1[c(2,4,6,8,10)]
data.dbp = data1[c(3,5,7,9,11)]

# package irr
# ICC 1-way, agreement only, ICC(1/k) Fleiss
icc(data.sbp, "oneway", "agreement")  # ICC(1)
icc(data.sbp, "oneway", "agreement", "average")  # ICC(k)

# ICC 2-way, random & fixed, agreement, ICC(A, 1/k) Mcgraw case 2/3, ICC(2, 1/k) Fleiss
icc(data.sbp, "twoway", "agreement")
icc(data.sbp, "twoway", "agreement", "average")

# ICC 2-way, random & fixed, consistency, ICC(C, 1/k) Mcgraw case 2/3, ICC(3, 1/k) Fleiss
icc(data.sbp, "twoway", "consistency")
icc(data.sbp, "twoway", "consistency", "average")

# for Hx testing, have to include r0 = cutoff, e.g. .6, .75 etc
icc(data.sbp, "twoway", "agreement", r0 = .6)
icc(data.sbp, "twoway", "agreement", r0 = .75)

# package psych
# Using Fleiss' nomenclature
ICC(data.sbp)
ICC(data.dbp)

# Please read Weir, J. P. (2005). Quantifying Test-Retest Reliability Using the Intraclass Correlation Coefficient and the SEM, Table 5

# ===

# kappa

# aggregate data in my note, Table 2
source("https://raw.githubusercontent.com/wnarifin/medicalstats-in-R/master/functions/tbl2raw_fun.R")
# tbl2raw function
tbl_12 = matrix(c(30, 15, 
                  5, 30), 2)  # enter data
dimnames(tbl_12) = list(doc1 = c("1", "2"), doc2 = c("1", "2"))  # give labels
tbl_12  # resulting matrix
data2 = tbl2raw(tbl_12)  # convert matrix to raw data
table(data2)  # confirm cross-tabulation of the raw data = matrix
kappa2(data2)  # irr package
cohen.kappa(data2)  # psych package

# data in my note Table 3
data3 = read.csv("kappa.csv")
head(data3)
table(data3)
kappa2(data3)  # unweighted
# Please read on weighting in Altman, D. G. (1991). Practical statistics for medical research. London: Chapman and Hall.
kappa2(data3, "equal")  # linear weight
kappa2(data3, "squared")  # quadratic weight
cohen.kappa(data3)  # psych package
