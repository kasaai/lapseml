library(keras)
library(recipes)

source("analysis/data-prep.R")

predictors <- c(
  "gender", "avg_issue_age", "face_amount",
  "risk_class",
  "premium_jump_ratio",
  # "avg_premium_jump_ratio",
  # "risk_class_mapped",
  "premium_mode", "duration"
)
responses <- c("lapse_count_rate", "lapse_amount_rate")

weights <- "exposure_count"

f <- as.formula(paste(
  paste(responses, collapse = " + "),
  paste(c(predictors, weights), collapse = " + "),
  sep = "~"
))

rec_nn <- recipe(f, data = training_data) %>%
  update_role(!!weights, new_role = "sample_weight") %>%
  step_integer(
    risk_class,
    premium_jump_ratio,
    duration,
    gender,
    # risk_class_mapped,
    face_amount,
    premium_mode,
    zero_based = TRUE
  ) %>%
  step_center(avg_issue_age) %>% # , avg_premium_jump_ratio
  step_scale(avg_issue_age) %>% # , avg_premium_jump_ratio
  prep(retain = TRUE, stringsAsFactors = FALSE)

# Helper function to create a keras data model,
# might move to be util.R once model is finalized

prep_keras_data <- function(data, predictors, responses) {
  data <- data %>%
    map_at(
      "gender",
      ~ keras::to_categorical(.x, 2) %>% array_reshape(c(length(.x), 2))
    ) %>%
    # map_at(
    #   "premium_mode",
    #   ~ keras::to_categorical(.x, 6) %>%
    #     array_reshape(c(length(.x), 6))
    # ) %>%
    map_at(
      "duration",
      ~ keras::to_categorical(.x, 3) %>%
        array_reshape(c(length(.x), 3))
    )

  list(
    x = data[predictors],
    y = data[responses],
    weights = data[weights]
  )
}

keras_training <- prep_keras_data(juice(rec_nn), predictors, responses)
keras_validation <- bake(rec_nn, validation_data) %>%
  prep_keras_data(predictors, responses)

make_keras_model <- function() {
  input_gender <- layer_input(shape = 2, name = "gender")
  input_issue_age_group <- layer_input(shape = 1, name = "avg_issue_age")
  input_face_amount_band <- layer_input(shape = 1, name = "face_amount")

  # input_avg_premium_jump_ratio <- layer_input(shape = 1, name = "avg_premium_jump_ratio")
  input_premium_jump_ratio <- layer_input(shape = 1, name = "premium_jump_ratio")
  # input_risk_class_mapped <- layer_input(shape = 1, name = "risk_class_mapped")
  input_risk_class <- layer_input(shape = 1, name = "risk_class")

  input_premium_mode <- layer_input(shape = 1, name = "premium_mode")
  input_duration <- layer_input(shape = 3, name = "duration")

  embedding_risk_class <- input_risk_class %>%
    layer_embedding(9, 2, name = "embedding_risk_class") %>%
    layer_flatten()
  embedding_premium_jump_ratio <- input_premium_jump_ratio %>%
    layer_embedding(24, 2, name = "embedding_premium_jump_ratio") %>%
    layer_flatten()
  embedding_face_amount_band <- input_face_amount_band %>%
    layer_embedding(4, 2, name = "embedding_face_amount_band") %>%
    layer_flatten()
  embedding_premium_mode <- input_premium_mode %>%
    layer_embedding(6, 2, name = "embedding_premium_mode") %>%
    layer_flatten()

  concat_inputs <- layer_concatenate(list(
    input_duration,
    input_gender,
    input_issue_age_group,
    embedding_face_amount_band,
    # input_avg_premium_jump_ratio,
    embedding_premium_jump_ratio,
    embedding_risk_class,
    embedding_premium_mode
  ))

  main_layer <- concat_inputs %>%
    layer_dense(units = 32, activation = "relu")


  output_count_rate <- main_layer %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 1, activation = "sigmoid", name = "lapse_count_rate")

  output_amount_rate <- main_layer %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 1, activation = "sigmoid", name = "lapse_amount_rate")

  keras_model(
    inputs = c(
      input_duration,
      input_gender,
      input_issue_age_group,
      input_face_amount_band,
      # input_avg_premium_jump_ratio,
      input_premium_jump_ratio,
      input_risk_class,
      input_premium_mode
    ),
    outputs = c(output_count_rate, output_amount_rate)
  )
}

model <- make_keras_model()

model %>%
  compile(
    optimizer = optimizer_adam(lr = 0.0001),
    loss = "mse",
    loss_weights = c(0.9, 0.1)
  )

history <- model %>%
  fit(
    x = keras_training$x,
    y = keras_training$y,
    batch_size = 256,
    epochs = 20,
    validation_split = 0.2,
    sample_weight = keras_training$weights
  )

plot(history)
predictions <- predict(
  model,
  keras_validation$x
)

validation_data_with_preds <- validation_data %>% bind_cols(
  predictions %>%
    setNames(c("predicted_count_rate", "predicted_amount_rate")) %>%
    as.data.frame()
)

validation_data_with_preds %>%
  weighted_rmse(truth = "lapse_count_rate", estimate = "predicted_count_rate", weights = "exposure_count")

# [1] 0.1695261
