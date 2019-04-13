# =====================
# Log Linear Regression 
# Wan Nor Arifin
# Updated: 14/04/2019 
# =====================

# Steps in log-linear modeling (von Eye & Mun (2013) pg 81-84)
# ============================================================
# Step 1: Specify models to be tested
# Step 2: Estimate the models; parameters, expected freqs, residuals
# Step 3: Hypothesis testing; 1. Overall GOF 2. Parameters sig. 3. Standardized residuals (~z)
# Step 4: Model interpretation; model fit, parameters, ORs

library(epiDisplay)  # to use poisgof

# Data in von Eye & Mun (2013). Log-linear modeling. pg.162
# =========================================================
penaltyTab = read.table(header = T, text = "
victim defendant  penalty freq
black     black death_no   593
black     black death_yes   14
black     white death_no   284
black     white death_yes   38
white     black death_no    25
white     black death_yes    1
white     white death_no   272
white     white death_yes   23"
); penaltyTab

# Step 1: Specify models to be tested
# ===================================
# 3 way table
# 3 variables, D, P, V
# Fit hiarchical models
# No interaction, Independence model
# D, P, V
# One 2-way
# D, PV
# P, DV
# V, DP
# Two 2-way
# DP, DV
# DP, PV
# DV, PV
# Three 2-way
# DP, DV, PV
# 3-way, Saturated model
# DPV
# Total 9 models

# Step 2: Estimate the models; parameters, expected freqs, residuals
# Step 3: Hypothesis testing; 1. Overall GOF 2. Parameters sig. 3. Standardized residuals (~z)
# ============================================================================================
# GOF, Model-model comparison, make table:
# ----------------------------------------
# List down X2, G2 for GOF, AIC
# Model comparisons:
# 1. List down G2, model_n vs model_n-1; G2_n-1 - G2_n -> LR Test
# 2. List down delta_AIC, AIC_n - AIC_n-1
# ---------------------------------------

# No interaction, Independence model
# D, P, V
ll.model0 = glm(freq ~ defendant + penalty + victim, data = penaltyTab, family = poisson)  # Step 2
summary(ll.model0)  # Step 2 & 3
names(ll.model0)  # detailed results options
# Residuals, obs vs pred
penaltyTab$pred = ll.model0$fitted.values; penaltyTab
cbind(penaltyTab, rawres = with(penaltyTab, freq - pred), stdres = rstandard(ll.model0, type = "pearson"))
## stdres  = standardized pearson residual: Agresti 3.13, pg81
# GOF
poisgof(ll.model0)  # G2
penaltyTab$x2 = with(penaltyTab, (freq - pred)^2/pred)  # X2 formula: von Eye & Mun, pg25; Agresti 3.10, pg79
list(results = "X2 GOF", chisq = sum(penaltyTab$x2), df = ll.model0$df.residual, p.value = pchisq(sum(penaltyTab$x2), ll.model0$df.residual, lower.tail = F) )  # X2
AIC(ll.model0)
# Parameters
cbind(round(summary(ll.model0)$coefficients, 3), round(confint(ll.model0), 3))
idr.display(ll.model0)  # ORs
# Model-model comparison
AIC(ll.model0) - AIC(ll.model_ <- glm(freq ~ 1, data = penaltyTab, family = poisson))  # vs empty model
anova(ll.model_, ll.model0, test = "LRT")  # vs empty model

# One 2-way
# D, PV
ll.model1 = glm(freq ~ defendant + penalty*victim, data = penaltyTab, family = poisson)  # Step 2
summary(ll.model1)
poisgof(ll.model1)
penaltyTab$pred = ll.model1$fitted.values; penaltyTab
penaltyTab$x2 = with(penaltyTab, (freq - pred)^2/pred)
list(results = "X2 GOF", chisq = sum(penaltyTab$x2), df = ll.model1$df.residual, p.value = pchisq(sum(penaltyTab$x2), ll.model1$df.residual, lower.tail = F) )
AIC(ll.model1)
anova(ll.model0, ll.model1, test = "LRT")
# Now rpt same commands for all models...
# P, DV
# V, DP
# Two 2-way
# DP, DV
# DP, PV
# DV, PV
# Three 2-way
# DP, DV, PV
# 3-way, Saturated model
# DPV

# Write model fit results in a table...

# Step 4: Model interpretation; model fit, parameters, ORs
# ========================================================
# Interpret your results