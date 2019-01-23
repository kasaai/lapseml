library(h2o)
library(recipes)
h2o.init(nthreads = 1)

rec <- recipe(training_data) %>%
  step_string2factor(gender, risk_class, face_amount, premium_mode) %>%
  prep(strings_as_factors = FALSE)

training_h2o <- as.h2o(bake(rec, training_data), "training_data")
validation_h2o <- as.h2o(bake(rec, validation_data), "validation_data")

predictors <- c(
  "gender", "risk_class", "face_amount", "premium_mode", "avg_issue_age", "duration",
  "avg_premium_jump_ratio"
)
response <- "lapse_count_rate"
automl_model <- h2o.automl(
  predictors, response, training_frame = training_h2o, nfolds = 10,
  weights_column = "exposure_count",
  stopping_metric = "RMSE",
  max_runtime_secs = 300
)

# check for reasonability

predictions <- predict(automl_model, validation_h2o) %>%
  as.data.frame()

validation_data %>%
  select(lapse_count_rate, exposure_count) %>%
  cbind(predictions) %>%
  weighted_rmse(truth = "lapse_count_rate", estimate = "predict", weights = "exposure_count")

# [1] 0.1619634

h2o.shutdown()
