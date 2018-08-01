# Handling data in R

# reading data
data = read.csv("cholest.csv")
data

library(foreign)
data = read.dta("cholest.dta")
data

data = read.spss("cholest.sav", to.data.frame = TRUE)
data

head(data)
tail(data)

?head
head(data, 3)
head(data, 10)

str(data)
str(x)

# data structure

## variable types
data_num = c(1, 2, 3, 4, 5)
str(data_num)

data_cat = c("M", "F", "M", "F", "M")
str(data_cat)

data_cat = factor( c("M", "F", "M", "F", "M") )
str(data_cat)
levels(data_cat)

## containers
data_frame = data.frame(data_num, data_cat)
data_frame
str(data_frame)

data_list = list(num = data_num, cat = data_cat)
data_list
str(data_list)

data_matrix = matrix(data = c(data_num, data_cat),
                     nrow = 5, ncol = 2)
data_matrix
str(data_matrix)

# subset
data$age
data[ , 2]
data[ , "age"]
data["age"]
data[2]

data[10:20, ]
data[c(3, 7, 9, 11, 13), ]

data[10:20, c("age", "categ")]
data[10:20, c(2, 5)]
data[c(1:10, 20:30), c("age", "categ")]

subset(data, age > 35, c(age, categ))
# > < <= >= == !=
# & |
subset(data, age >= 35 & categ == "Grp C", c(age, categ))
subset(data, chol > 8.5 & age >= 40
       & sex == "female" & categ == "Grp C", 
       c(chol, age, sex, categ))
subset(data, age > 35 & age < 40, c(age, chol))

data1 = subset(data,
               chol > 8.5 & age >= 40
               & sex == "female" & categ == "Grp C",
               c(chol, age, sex, categ))

# editing

## new variable
data$age_month = data$age * 12
data$age_month

## recode
data$age_cat = cut(data$age,
                   breaks = c(-Inf, 35, 45, Inf),
                   labels = c("<35", "35-45", ">45"))
table(data$age_cat)
library(car)
data$age_cat1 = recode(data$age_cat,
                       "c('35-45', '>45') = '35 and above'")
table(data$age_cat1)

data$age_cat = cut(data$age, 
                   breaks = c(-Inf, 30, 35, 40, 45, 50, Inf),
                   labels = c("<30", "30-35", "36-40", "41-45", "46-50", ">50"))
table(data$age_cat)
data$age_cat1 = recode(data$age_cat,
                       "c('<30', '30-35') = '<36';
                       c('36-40', '41-45') = '36-45';
                       c('46-50', '>50') = '>45'")
table(data$age_cat1)
