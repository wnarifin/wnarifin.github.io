#-----------------------------------
# Introduction to R
#-----------------------------------
# Author: Wan Nor Arifin
#-----------------------------------

# Outlines

# - RStudio Interface
# - Function, Library & Object
# - Read data
# - Handle data
# - Basic analysis

# RStudio Interface

## The windows
# 1. Script
# 2. Console
# 3. Environment & History
# 4. Files & others

## Tasks
# - Set the working directory (Files)
# - Install packages a.k.a libraries (Packages)
#   - psych, lavaan, MVN, semTools, semPlot
# - Open a new R script
#   - type all commands/functions here
#   - comments, start with "#"
#   - run all commands by Ctrl+Enter

# Function, Library, Object

## Function
#function(), think of MS Excel function

## Library
library(psych)

##Object
# - name assigned on left side of "<-" / "="
# - variable, data (data frame, matrix, list)
x <- 1
y = 2
z = x + y
z  #type object name, you'll get the value

# Read data

#We have these files:
# - cholest.csv
# - cholest.sav
# - cholest.dta
# - cholest.xlsx
#Always make sure that you set the working directory first!
data.csv = read.csv("cholest.csv")  #most natural way to open data in R
library(foreign)  #library to read .sav (SPSS) and .dta (STATA) files
data.sav = read.spss("cholest.sav", to.data.frame = TRUE)  #SPSS
data.dta = read.dta("cholest.dta")  #STATA
library(readxl)  #library to read excel files, must install first
data.xls = read_excel("cholest.xlsx", sheet = 1)

# Handle data

## Basics
str(data.csv)  #Basic info
dim(data.csv)  #Dimension (row/case column/variable)
names(data.csv)  #Variable names

## View data
head(data.csv)  #View data, first 6 rows
tail(data.csv)  #View data, last 6 rows
data.csv  #View all
View(data.csv)  #View, graphical way

## Select specific parts of data (subsetting)
data.csv$age  #View "age" only
data.csv["age"]
data.csv[2]
#In general, syntax data[row(number/name), col(number/name)]
data.csv[1:10, 2:4]  #Row 1 to 10; col 2 to 4
data.csv[c(1,3,5,7,9), c("age", "chol")]  #Row 1,3,5,7,9; col age & chol
data.csv[data.csv["age"] == 38, c("age", "chol")]  #Row age = 38; col age & chol
data.csv[data.csv["sex"] == 1, c("sex", "chol")]  #Row Sex = 1; col sex & chol
#Can also use subset(), syntax subset(data, condition, variable)
subset(data.csv, age == 38)
subset(data.csv, age == 38, age:sex)

# Basic analysis

#We use data.sav, with category labels
str(data.sav)  #numerical = num, categorical = Factor
summary(data.sav)

## Numerical
library(psych)  #to use describe
describe(data.sav[c("chol","age", "exercise")])

## Categorical
table(data.sav$sex)
table(data.sav$categ)

## Plots
#Histogram
hist(data.sav$chol)
#Boxplot
boxplot(data.sav$chol)
#Scatter plot
plot(data.sav$age, data.sav$chol)
abline(lm(chol ~ age, data = data.sav))  #need two lines of codes
#Bar chart
count = table(data.sav$sex)
barplot(count, col = c("blue", "red"))

# Q&A