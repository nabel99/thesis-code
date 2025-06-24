library(tidyverse)
library(TOSTER)
library(DoubleML)

df <- read.csv("./data/df_joined.csv")
str(df)
colSums(is.na(df))

mean_crime <- mean(df$violent_crime_100k)
mean_crime * 0.05

# mean crime *0.05 is 18.8
# one percentage point increase in immigrants --> +1000 immigrants per 100k lead to 18.8 change
# 1 immigrant per 100k --> 0.0188
# see reasoning in Section 3.1

eqbound_upper <- round((mean_crime * 0.05) / (100000 * 0.01), 4)
eqbound_lower <- -1 * eqbound_upper

# SIMPLE EQUIVALENCE TEST -------------------------------------------------
# linear regression, equivalence test of the estimate
# same confounders used as in Green (2016)
simple_eq_vars <- c("male", "age_18_24", "hs_grad_plus", 
                    "unemployment_rate", "median_household_income",
                    "poverty_rate", "unauthorized_total_100k",
                    "violent_crime_100k")

df_simple_eq <- df %>% select(all_of(simple_eq_vars))

linear_model <- lm(violent_crime_100k ~ ., data = df_simple_eq)
summary_lm <- summary(linear_model)


coef_estimate <- summary_lm$coefficients["unauthorized_total_100k", "Estimate"]
coef_se <- summary_lm$coefficients["unauthorized_total_100k", "Std. Error"]
n <- nobs(linear_model)

#tsum_TOST for summary statistics
res_simple_eqtuest <- TOSTER::tsum_TOST(
  m1 = coef_estimate,
  mu = 0,
  sd1 = coef_se * sqrt(n),
  n1 = n,
  low_eqbound = eqbound_lower,
  high_eqbound = eqbound_upper,
  eqbound_type = "raw",
  alpha = 0.05,
  hypothesis = "EQU"
)

res_simple_eqtuest

# what would be the equivalence bound needed to reject?

sufficient_lower <- coef_estimate - (qt(0.95, n-1) * coef_se)
sufficient_upper <- coef_estimate - (-qt(0.95, n-1) * coef_se)


# DML based eqtest --------------------------------------------------------


