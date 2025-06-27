library(tidyverse)
library(TOSTER)

# convert to 1k dollars

df <- read.csv("./data/df_joined.csv")
str(df)
colSums(is.na(df))

# convert to 1k dollars
df <- df %>% mutate(across(c("median_household_income", "per_capita_income"), function(x) x/ 1000))

# multiply proportions
df <- df %>% mutate(across(c("male", "female", "age_18_24", "hs_grad_plus", "unemployment_rate",
                             "uninsured", "poverty_rate", "unauthorized_total_pct"), function(x) x * 100))

mean_crime <- mean(df$violent_crime_100k)

# see reasoning for equivalence bounds in Section 3.1

eqbound_upper <- round((mean_crime * 0.05), 4)
eqbound_lower <- -1 * eqbound_upper

# SIMPLE EQUIVALENCE TEST -------------------------------------------------
# model with unauthorized immigrant proportion
# eq bounds set at 5% change compared to mean, taken from Light and Miller (2018)

prop_eq_vars <- c("male", "age_18_24", "hs_grad_plus", 
                    "unemployment_rate", "median_household_income",
                    "poverty_rate", "unauthorized_total_pct",
                    "violent_crime_100k")

df_prop_eq <- df %>% select(all_of(prop_eq_vars))

linear_model <- lm(violent_crime_100k ~ ., data = df_prop_eq)
summary_lm <- summary(linear_model)


coef_estimate <- summary_lm$coefficients["unauthorized_total_pct", "Estimate"]
coef_se <- summary_lm$coefficients["unauthorized_total_pct", "Std. Error"]
n <- nobs(linear_model)

# eqbounds defined above

res_eqtuest <- TOSTER::tsum_TOST(
  m1 = coef_estimate,
  mu = 0,
  sd1 = coef_se * sqrt(n), # convert standard error back to SD
  n1 = n,
  low_eqbound = eqbound_lower,
  high_eqbound = eqbound_upper,
  eqbound_type = "raw",
  alpha = 0.05,
  hypothesis = "EQU"
)
res_eqtuest



# DML based eqtest --------------------------------------------------------
library(DoubleML)
library(mlr3)
library(mlr3learners)
library(data.table)
set.seed(9999)

# unauthorized_total_pct
y_col <- "violent_crime_100k"
d_col <- "unauthorized_total_pct"
x_cols <- setdiff(names(df_prop_eq), c(y_col, d_col))

# initialize data object
dml_data <- DoubleMLData$new(
  data = df_prop_eq,
  y_col = y_col,
  d_cols = d_col,
  x_cols = x_cols
)

# ADJUST LEARNERS
# standard random forest
#learner <- lrn("regr.ranger", num.trees = 500, mtry = 3, min.node.size = 5)
learner <- lrn("regr.cv_glmnet", alpha = 1)

# set up partial linear model
dml_plr <- DoubleMLPLR$new(
  data = dml_data,
  ml_g = learner,  # E[Y|X]
  ml_m = learner,   # E[D|X]
  n_folds = 2
)

# fit
dml_plr$fit()

dml_plr$summary()

# DML eq test
# eqbounds the same
# degrees of freedom irrelevant, because sample large enough for asymptotic normality

dml_coef <- dml_plr$coef
dml_se <- dml_plr$se

# tsum_TOST calculates SE as SD/sqrt(n), so even though SE in DML is not calculated like that,
# for the purpose of the equivalence test, this workaround is sufficient

res_dml_eqtuest <- TOSTER::tsum_TOST(
  m1 = dml_coef,
  mu = 0,
  sd1 = dml_se * sqrt(n), # convert standard error to SD based on behaviour of tsum_TOST
  n1 = n,
  low_eqbound = eqbound_lower,
  high_eqbound = eqbound_upper,
  eqbound_type = "raw",
  alpha = 0.05,
  hypothesis = "EQU"
)
res_dml_eqtuest


