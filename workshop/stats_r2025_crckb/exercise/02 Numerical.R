#' ---
#' title: "Numerical data analysis using R - Exercise"
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
# Independent t ----

#' # Two independent samples - Independent _t_-test
  
#' ## About the test:
#' - Parametric test.
#' - Normally distributed data per group.
#' - Comparison of means between TWO groups.
#' - _t_-statistics.
#'

#' ## Analysis:
#' 1. Load `asthma_pefr [indep t].sav` dataset,

library(haven)
asthma = read_sav("asthma_pefr [indep t].sav")
str(asthma)
head(asthma)
asthma$drug_grp = factor(asthma$drug_grp,
                         levels = c(1,2,3),
                         labels = c("drug A", "drug B", "drug C"))

#' Explore the data. Obtain the basic descriptive statistics.

by(asthma$pefr, asthma$drug_grp, mean)
by(asthma$pefr, asthma$drug_grp, sd)
table(asthma$drug_grp)

library(gtsummary)
theme_gtsummary_eda()
tbl_summary(asthma)

#' 2. Check the **normality assumption** of the data by group,

library(lattice)
histogram(~ pefr | drug_grp, data = asthma)
bwplot(pefr ~ drug_grp, data = asthma)

library(GGally)
asthma |> ggpairs()

#' 3. Check the **equality of variance assumption**,

asthma_ab = subset(asthma, drug_grp != "drug C")
var.test(pefr ~ drug_grp, data = asthma_ab)  # equal*

#' *Choose:
  
#' - Equal variance = Standard Two Sample _t_-test.
#' - Unequal variance = Welch Two Sample _t_-test.

#' 4. Perform independent _t_-test,

t.test(pefr ~ drug_grp, data = asthma_ab)  # significant difference
t.test(pefr ~ drug_grp, data = asthma_ab) |> broom::tidy()

#' The function default is **Welch Two Sample _t_-test** (takes car the unequal variance).

#' You can also obtain the standard _t_-test (equal variance assumed),

t.test(pefr ~ drug_grp, data = asthma_ab, var.equal = T) |> broom::tidy()

#' ## Presentation
#' [Guide: Reporting Statistical Results in Medical Journals](http://www.mjms.usm.my/MJMS23052016/01MJMS23052016_ED.pdf)


#+ include=FALSE
# Paired t ----

#' # Two dependent samples - Paired _t_-test

#' ## About the test:
#' - Parametric test.
#' - Normally distributed DIFFERENCES between TWO paired observations).
#' - Compares the observations between TWO time points for any changes (e.g. any changes after treatment?).
#' - If there are changes, the differences between the time points $\neq 0$.
#' - _t_-statistics.
#'

#' ## Analysis:
#' 1. Load `CD4 [paired t].sav` dataset,

library(haven)
cd4 = read_sav("CD4 [paired t].sav")
str(cd4)
cd4

#' Explore the data. Obtain the basic descriptive statistics.

#' Mean and SD,

cd4$diff = cd4$cd4post - cd4$cd4pre
mean(cd4$cd4pre); sd(cd4$cd4pre)
mean(cd4$cd4post); sd(cd4$cd4post)
mean(cd4$diff); sd(cd4$diff)


#' and the number of subjects,

lengths(cd4)

#' 2. Check the **normality assumption** of the differences,

histogram(~ diff, data = cd4)
bwplot(~ diff, data = cd4)

#' 3. Perform paired _t_-test,

t.test(cd4$cd4pre, cd4$cd4post, paired = TRUE)

#' ## Presentation
#' [Guide: Reporting Statistical Results in Medical Journals](http://www.mjms.usm.my/MJMS23052016/01MJMS23052016_ED.pdf)


#+ include=FALSE
# One-way ANOVA ----

#' # More than two independent samples - One-way ANOVA

#' ## About the test:
#' - Parametric test.
#' - Comparison of means for THREE/MORE groups.
#' - _F_-statistics.
#'

#' ## Analysis:
#' 1. Explore the data `asthma` again. Obtain basic descriptive statistics,

by(asthma$pefr, asthma$drug_grp, mean)
by(asthma$pefr, asthma$drug_grp, sd)

table(asthma$drug_grp)
table(asthma$drug_grp) |> prop.table() |> round(3) * 100

#' 2. Check the **normality assumption** of the data per group,

library(lattice)
histogram(~ pefr | drug_grp, data = asthma)
bwplot(pefr ~ drug_grp, data = asthma)

#' However, we will mainly rely on **residuals** for the normality assessment.

#' 3. Check the **equality of variance assumption**,

bartlett.test(pefr ~ drug_grp, data = asthma)

#' 4. Perform one-way ANOVA test,

aov_pefr = aov(pefr ~ drug_grp, data = asthma)
summary(aov_pefr)  # significant difference between three groups

#' Notice here we save the output of `aov()` into `aov_chol` first. This allows further extraction of full output from `aov_chol` ANOVA object.

#' 5. Post-hoc test, to look for significant group pairs,

pairwise.t.test(asthma$pefr, asthma$drug_grp, p.adj = "bonferroni")
#' all pairs significant difference

#' Here, it works as if we do multiple independent _t_-tests. We adjust for multiple comparison by Bonferroni correction.

#' 6. Check the **normality of the residuals**,

#' Save the residuals as `residual_chol`. We also need to use `as.numeric()` to extract proper numerical data from `aov_chol` ANOVA object, and save it again to `residuals_chol`

residuals_pefr = residuals(aov_pefr)
residuals_pefr = as.numeric(residuals_pefr)

#' Then, check the normality,

histogram(~ residuals_pefr)  # normal
# bwplot(~ residuals_pefr)
boxplot(residuals_pefr)

#' ## Presentation
#' [Guide: Reporting Statistical Results in Medical Journals](http://www.mjms.usm.my/MJMS23052016/01MJMS23052016_ED.pdf)

#'
#' # Exercise
#' - use dataset from "Practice Datasets" folder 