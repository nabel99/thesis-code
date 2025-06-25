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
                             "uninsured", "poverty_rate", "unauthorized_total_pct"), function(x) x* 100))

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


# model with unauth immigrant proportion
# eq bounds set at 5% change compared to mean, based on same logic as before, taken from Light and Miller (2018)
prop_eq_vars <- c("male", "age_18_24", "hs_grad_plus", 
                    "unemployment_rate", "median_household_income",
                    "poverty_rate", "unauthorized_total_pct",
                    "violent_crime_100k")

df_prop_eq <- df %>% select(all_of(prop_eq_vars))
# for a unit increase to make sense
prop_linear_model <- lm(violent_crime_100k ~ ., data = df_prop_eq)
summary_prop_lm <- summary(prop_linear_model)


prop_coef_estimate <- summary_prop_lm$coefficients["unauthorized_total_pct", "Estimate"]
prop_coef_se <- summary_prop_lm$coefficients["unauthorized_total_pct", "Std. Error"]

prop_eqbound_lower <- -1 * (0.05 * mean(df_prop_eq$violent_crime_100k))
prop_eqbound_upper <- 0.05 * mean(df_prop_eq$violent_crime_100k)

res_prop_eqtuest <- TOSTER::tsum_TOST(
  m1 = prop_coef_estimate,
  mu = 0,
  sd1 = prop_coef_se * sqrt(n), # convert standard error back to SD
  n1 = n,
  low_eqbound = prop_eqbound_lower,
  high_eqbound = prop_eqbound_upper,
  eqbound_type = "raw",
  alpha = 0.05,
  hypothesis = "EQU"
)
res_prop_eqtuest



# DML based eqtest --------------------------------------------------------
library(DoubleML)
library(mlr3)
library(mlr3learners)
library(data.table)
set.seed(9999)

# unauthorized_total_100k

y_col <- "violent_crime_100k"
d_col <- "unauthorized_total_100k"
x_cols <- setdiff(names(df_simple_eq), c(y_col, d_col))

# initialize data object
dml_data_v1 <- DoubleMLData$new(
  data = df_simple_eq,
  y_col = y_col,
  d_cols = d_col,
  x_cols = x_cols
)

# standard random forest
#learner_v1 <- lrn("regr.ranger", num.trees = 500, mtry = 3, min.node.size = 5)
# lasso
learner_v1 <- lrn("regr.cv_glmnet", alpha = 1)

# set up partial linear model
dml_plr_v1 <- DoubleMLPLR$new(
  data = dml_data_v1,
  ml_g = learner_v1,  # E[Y|X]
  ml_m = learner_v1,   # E[D|X]
  n_folds = 2
)

# fit
dml_plr_v1$fit()

dml_plr_v1$summary()


# unauthorized_total_pct
y_col_2 <- "violent_crime_100k"
d_col_2 <- "unauthorized_total_pct"
x_cols_2 <- setdiff(names(df_prop_eq), c(y_col_2, d_col_2))

# initialize data object
dml_data_v2 <- DoubleMLData$new(
  data = df_prop_eq,
  y_col = y_col_2,
  d_cols = d_col_2,
  x_cols = x_cols_2
)

# standard random forest
learner_v2 <- lrn("regr.ranger", num.trees = 500, mtry = 3, min.node.size = 5)
#learner_v2 <- lrn("regr.cv_glmnet", alpha = 1)

# set up partial linear model
dml_plr_v2 <- DoubleMLPLR$new(
  data = dml_data_v2,
  ml_g = learner_v2,  # E[Y|X]
  ml_m = learner_v2,   # E[D|X]
  n_folds = 2
)

# fit
dml_plr_v2$fit()

dml_plr_v2$summary()



