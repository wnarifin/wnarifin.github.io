# =================================
# Introduction to R and RStudio IDE
# Wan Nor Arifin
# 19/12/2018
# =================================

# R & RStudio

## Installation
# - R @ https://cran.r-project.org/
# - RStudio @ http://www.rstudio.com/

# R to PDF
# - Windows & MacOS -- MikTeX @ https://miktex.org/download
# - Linux -- texlive.
# Important for R-markdown session later.

# R interface
# ...

# RStudio Interface

## The windows
# 1. Script
# 2. Console
# 3. Environment & History
# 4. Files & others

# R script

# - type all commands/functions here
# - comments, start with "#"
# - run all commands by Ctrl+Enter

# Function & Object

## Function
# - function(), think of MS Excel function
# function(argument1 = value, argument2 = value)

## Object
# - name assigned on left side of "<-" / "="
# - variable, data (data frame, matrix, list)
x <- 1
y = 2
z = x + y
z  # type object name, you'll get the value

# R packages

## Install packages a.k.a libraries

# - Graphically
# - Command
# e.g. psych, car
install.packages("psych")  # by command
install.packages("car")

## Load libraries
library(psych)
library(car)

# Set the working directory (Files)
# ...

# Help
?psych
?library
??mean
??survey
