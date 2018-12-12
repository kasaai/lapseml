library(h2o)
h2o.init(nthreads = 1)
training_h2o <- as.h2o(training_data, "training_data")
training_h2o
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
