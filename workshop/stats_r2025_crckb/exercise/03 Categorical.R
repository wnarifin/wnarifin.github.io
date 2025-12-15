#' ---
#' title: "Categorical data analysis using R"
#' author: "Dr. Wan Nor Arifin"
#' date: "Updated: `r format(Sys.time(), '%d %B %Y')`"
#' output:
#'   html_document:
#'     theme: united
#'     toc: true
#'     toc_depth: 4
#'     toc_float: true
#'     number_sections: true
#' ---

#+ include=FALSE
knitr::opts_chunk$set(echo = TRUE, comment = "#>", warning = TRUE, message = TRUE)  # detailed

#+ include=FALSE
# Chi-square ----

#' # Two independent samples - Chi-squared test for association

#' ## About the test:
#' - Non-parametric test.
#' - Association between TWO categorical variables.
#' - Cross-tabulation between the variables, usually 2 x 2, but can be any levels.
#' - The association between the variables are made by comparing the **observed** cell counts with the **expected** cell counts if the variables are not associated to each other.
#' - Requirement -- < 25% **expected** cell counts < 5.
#' - $\chi^2$ statistics.

#'
#' ## Analysis:
#' 1. The data, load `categorical data [x2].sav`,

library(haven)
catdata = read_sav("categorical data [x2].sav")
str(catdata)
head(catdata)

library(gtsummary)
theme_gtsummary_eda()
tbl_summary(catdata)

#' Now, we create cross-tabulation of the categorical variables,

tab_catdata = table(Gender = catdata$sex, Smoking = catdata$smoke)

#' and view the table,

tab_catdata
addmargins(tab_catdata)

#' 2. Perform chi-squared test for association. Two ways to do,

#' by using the table,

chisq.test(tab_catdata)

#' or by using the variables directly,

chisq.test(catdata$sex, catdata$smoke)

#' But remember, for chi-squared test, you must review the table to get an idea about the association.

#'
#' 3. Check assumption -- < 25% **expected** cell counts < 5.

#'
#' The expected cell counts,

chisq.test(tab_catdata)$expected

#' No count < 5, thus we can rely on chi-squared test.

#'
#' ## Presentation
#' [Guide: Reporting Statistical Results in Medical Journals](http://www.mjms.usm.my/MJMS23052016/01MJMS23052016_ED.pdf)

#+ include=FALSE
# Fisher's exact ----

#' # Two independent samples - Fisher's exact test

#' ## About the test:
#' - Alternative of chi-squared test.
#' - Usually small cell counts, i.e. chi-squared test requirement is not fulfilled.
#' - Gives exact _P_-value, no statistical distribution involved.

#'
#' ## Analysis:
#' 1. Perform Fisher's exact test,

tab_catdata1 = table(Gender = catdata$sex, Smoking = catdata$Smoking3)
tab_catdata1
chisq.test(tab_catdata1)$expected
fisher.test(tab_catdata1)
fisher.test(tab_catdata1) |> broom::tidy()

#'
#' ## Presentation
#' [Guide: Reporting Statistical Results in Medical Journals](http://www.mjms.usm.my/MJMS23052016/01MJMS23052016_ED.pdf)


#+ include=FALSE
# McNemar ----

#' # Two dependent samples - McNemar's test

#' ## About the test:
#' - Non-parametric test.
#' - Association between TWO repeated categorical outcomes.
#' - Cross-tabulation is limited to 2 x 2 only.
#' - The concern is whether the subjects still have the same outcomes (concordant) or different outcomes (discordant) upon repetition (pre-post).
#' - The association is determined by looking at the discordant cells. 
#' - $\chi^2$ statistics.

#'
#' ## Analysis:
#' 1. The data, `catdata`,

#' Now we view the table,

tab_chol = table(catdata$Chol_pre, catdata$Chol_post)
addmargins(tab_chol)

#' 2. Perform McNemar's test,

mcnemar.test(tab_chol)

#'
#' ## Presentation
#' [Guide: Reporting Statistical Results in Medical Journals](http://www.mjms.usm.my/MJMS23052016/01MJMS23052016_ED.pdf)

#'
#' # Exercise
#' - use dataset from "Practice Datasets" folder 