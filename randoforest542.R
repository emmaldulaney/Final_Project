#### full MAIN code
library(dplyr)
library(randomForest)

# 1. Hazard exposure columns (no *_EAL, no *_RISKS)
haz_exposure_cols <- grep("_(EVNTS|AFREQ|EXP_AREA)$",
                          names(merged_data),
                          value = TRUE)

haz_exposure_cols

# Columns that are basically all NA (from your colSums output)
mostly_na_cols <- c("CFLD_EVNTS", "ERQK_EVNTS", "WFIR_EVNTS")

# Keep hazard exposure columns that are NOT mostly NA
haz_exposure_keep <- setdiff(haz_exposure_cols, mostly_na_cols)

haz_exposure_keep

# 2. Build modeling data for RISK_SCORE
risk_model_df <- merged_data %>%
  select(
    RISK_SCORE,          # target
    SOVI_SCORE,          # social vulnerability
    region,              # region factor you created
    coastal,             # coastal / inland factor
    log_pop_density,     # log of population density
    log_build_value,     # log of total building value
    all_of(haz_exposure_keep)
  ) %>%
  filter(
    !is.na(RISK_SCORE),
    !is.na(log_pop_density)
  )

# Check NAs now (should be 0 for all remaining columns)
colSums(is.na(risk_model_df))

# 3. Train / test split (80/20)
set.seed(123)
n <- nrow(risk_model_df)
train_idx <- sample(seq_len(n), size = floor(0.8 * n))

train_df <- risk_model_df[train_idx, ]
test_df  <- risk_model_df[-train_idx, ]

# Just to confirm we still have rows
dim(train_df)
dim(test_df)

# 4. Baseline model: always predict mean RISK_SCORE from training data
baseline_pred <- mean(train_df$RISK_SCORE)
baseline_rmse <- sqrt(mean((test_df$RISK_SCORE - baseline_pred)^2))
baseline_rmse

# 5. Random Forest model (no *_RISKS)
set.seed(123)
rf_model <- randomForest(
  RISK_SCORE ~ .,
  data       = train_df,
  ntree      = 500,
  mtry       = floor(sqrt(ncol(train_df) - 1)),  # rule of thumb
  importance = TRUE
)

rf_model   # shows OOB MSE and % Var explained

# 6. Test RMSE
rf_pred <- predict(rf_model, newdata = test_df)
rf_rmse <- sqrt(mean((test_df$RISK_SCORE - rf_pred)^2))
rf_rmse

# 7. Variable importance
importance(rf_model)
varImpPlot(rf_model)



library(dplyr)
library(ggplot2)

# Get importance table and turn into a data frame
imp_df <- as.data.frame(importance(rf_model))
imp_df$variable <- rownames(imp_df)

# Keep only %IncMSE and sort by it
imp_top10 <- imp_df %>%
  arrange(desc(`%IncMSE`)) %>%
  slice(1:10)

ggplot(imp_top10, aes(x = reorder(variable, `%IncMSE`), y = `%IncMSE`)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Most Important Predictors for RISK_SCORE",
    x = "Predictor",
    y = "% Increase in MSE if Variable is Removed"
  ) +
  theme_minimal()

