# Comparison of categorical data

# chi-squared test
lung = read.csv("lung.csv")
tab_lung = table(Smoking = lung$Smoking, Cancer = lung$Cancer)
tab_lung
addmargins(tab_lung)
chisq.test(tab_lung)

chisq.test(tab_lung)$expected
chisq.test(tab_lung)$observed

# Fisher's exact test
fisher.test(tab_lung)

# ----
tab_pm = read.table(header = FALSE, text = "
794 150
86 570
                    ")
str(tab_pm)
tab_pm = as.matrix(tab_pm)
tab_pm = as.table(tab_pm)
str(tab_pm)

dimnames(tab_pm) = list(first = c("approve", "disapprove"),
                        second = c("approve", "disapprove"))
tab_pm
addmargins(tab_pm)
mcnemar.test(tab_pm)
