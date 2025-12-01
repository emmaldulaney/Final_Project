library(dplyr)
library(glmnet)

merged_data <- read.csv("/Users/abhip/Downloads/merged_data.csv")

model_df <- merged_data %>%
  select(
    SOVI_SCORE, # target we want to predict
    pct_owner_occupied, # % of homes that are owner-occupied
    pct_renter_occupied,# % of homes that are renter-occupied
    pct_hs_or_higher, # % of adults with HS diploma or higher
    pct_ba_or_higher, # % of adults with bachelor’s degree or higher
    median_hh_income, # median household income
    poverty_rate, # % of people in poverty
    pct_in_labor_force, # % of adults in the labor force
    pct_unemployed,# % of the labor force that is unemployed
    pct_armed_forces # % of the labor force in the armed forces
  ) %>%
  # dropping any rows that have missing values
  na.omit()

# y = what we want to predict (SOVI score)
y <- model_df$SOVI_SCORE

# X = all the predictors we use to predict SOVI
# turn them into a matrix because glmnet needs that format
X <- as.matrix(model_df %>% select(-SOVI_SCORE))


#fitting a lasso model with cross valid
set.seed(542)

cv_lasso <- cv.glmnet(
  X, y,
  alpha = 1,   
  nfolds = 10  
)

# This plot shows test error for different values of lambda
# (lambda controls how strong the penalty/shrinkage is)
plot(cv_lasso)

# Two standard choices for lambda:
lambda_min  <- cv_lasso$lambda.min   # gives the lowest CV error (best fit)
lambda_1se  <- cv_lasso$lambda.1se   # slightly larger lambda, simpler model,
# still within 1 standard error of the best

lambda_min
lambda_1se

#fitting the LASSO models at those two lambdas

lasso_min <- glmnet(
  X, y,
  alpha  = 1,
  lambda = lambda_min
)

lasso_1se <- glmnet(
  X, y,
  alpha  = 1,
  lambda = lambda_1se
)

pred_min <- predict(lasso_min, newx = X)
pred_1se <- predict(lasso_1se, newx = X)

#RMSE 
rmse_min <- sqrt(mean((y - pred_min)^2))
rmse_1se <- sqrt(mean((y - pred_1se)^2))

rmse_min
rmse_1se

# Coefficients for the more flexible model (lambda_min)
coef_min <- as.matrix(coef(lasso_min))
coef_min

# Keep only the coefficients that are not zero; these are the variables LASSO decided are important
nonzero_min <- coef_min[coef_min[, 1] != 0, , drop = FALSE]
nonzero_min

# same for the simpler 1-SE model
coef_1se     <- as.matrix(coef(lasso_1se))
nonzero_1se  <- coef_1se[coef_1se[, 1] != 0, , drop = FALSE]
nonzero_1se

# These nonzero coefficients tell us:
#  - direction: positive = higher value → higher SOVI, negative = higher value → lower SOVI
#  - size: how strong the relationship is (on the SOVI scale, after standardization)