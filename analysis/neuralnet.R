library(keras)
library(recipes)

source("analysis/data-prep.R")

predictors <- c(
  "gender", "avg_issue_age", "face_amount", "post_level_premium_structure",
  "avg_premium_jump_ratio", "risk_class_mapped", "premium_mode", "duration"
)
responses <- c("lapse_count_rate", "lapse_amount_rate")

f <- as.formula(paste(
  paste(responses, collapse = " + "),
  paste(predictors, collapse = " + "),
  sep = "~"
))

rec_nn <- recipe(f, data = training_data) %>%
  step_string2factor(gender, risk_class_mapped, face_amount, premium_mode) %>%
  step_num2factor(duration) %>%
  # step_num2factor(all_numeric()) %>%
  step_center(avg_issue_age, avg_premium_jump_ratio) %>%
  step_scale(avg_issue_age, avg_premium_jump_ratio) %>%
  prep(retain = TRUE, stringsAsFactors = FALSE)

# Helper function to create a keras data model

make_keras_data <- function(data) {
  data <- data %>%
    map_if(is.factor, ~ as.integer(.x) - 1) %>%
    map_at("gender", ~ keras::to_categorical(.x, 2) %>% array_reshape(c(length(.x), 2))) %>%
    # map_at("post_level_premium_structure",
    #        ~ keras::to_categorical(.x, 2) %>% array_reshape(c(length(.x), 2))) %>%
    map_at("premium_mode", ~ keras::to_categorical(.x, 6) %>%
             array_reshape(c(length(.x), 6))) %>%
  map_at("duration", ~ keras::to_categorical(.x, 3) %>%
           array_reshape(c(length(.x), 3)))

  list(x = data[predictors],
       y = data[responses])
}

keras_training <- make_keras_data(juice(rec_nn))
keras_testing <- bake(rec_nn, testing_data) %>%
  make_keras_data()
# validation_data <- bake(rec_nn, validation) %>%
#   make_keras_data()

# Build network.
# Note that the one-hot encoded inputs will have shape > 1. The rest
#  will be fed to embedding layers so we keep the original representation.
input_gender <- layer_input(shape = 2, name = "gender")
input_issue_age_group <- layer_input(shape = 1, name = "avg_issue_age")
input_face_amount_band <- layer_input(shape = 1, name = "face_amount")
input_prem_jump_d11_d10 <- layer_input(shape = 1, name = "avg_premium_jump_ratio")
input_risk_class <- layer_input(shape = 1, name = "risk_class_mapped")
input_premium_mode <- layer_input(shape = 6, name = "premium_mode")
input_duration <- layer_input(shape = 3, name = "duration")
#
# output_issue_age_group <- input_issue_age_group %>%
#   layer_embedding(7, 6) %>%
#   layer_flatten()

concat_inputs <- layer_concatenate(list(
  input_duration,
  input_gender,
  input_issue_age_group,
  input_face_amount_band %>%
    layer_embedding(4, 2) %>%
    layer_flatten(),
  input_prem_jump_d11_d10,
  input_risk_class %>%
    layer_embedding(9, 2) %>%
    layer_flatten(),
  input_premium_mode
))

main_layer <- concat_inputs %>%
  layer_dense(units = 32, activation = "relu")


output_count_rate <- main_layer %>%
  layer_dropout(0.2) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid", name = "lapse_count_rate")

output_amount_rate <- main_layer %>%
  layer_dropout(0.2) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid", name = "lapse_amount_rate")

model <- keras_model(
  inputs = c(input_duration, input_gender, input_issue_age_group, input_face_amount_band,
             input_prem_jump_d11_d10,
             input_risk_class, input_premium_mode),
  outputs = c(output_count_rate, output_amount_rate)
)

model %>%
  compile(
    optimizer = optimizer_adam(amsgrad = TRUE),
    loss = "mse",
    loss_weights = c(0.8, 0.2)
  )

history <- model %>%
  fit(
    x = keras_training$x,
    y = keras_training$y,
    batch_size = 256,
    epochs = 100,
    validation_split = 0.2,
    sample_weight = keras_training$exposure_count
    )

predictions <- predict(
  model,
  keras_testing$x
)

# WIP. Here `validation_summary` isn't a summary (yet), we're just
#  cbinding the predictions to the original data.
validation_summary <- testing_data %>% bind_cols(
  predictions %>%
    setNames(c("predicted_count_rate", "predicted_amount_rate")) %>%
    as.data.frame()
)

matrices <- validation_summary %>%
  weighted_rmse(truth = "lapse_count_rate", estimate = "predicted_count_rate", weights = "exposure_count")

validation_summary %>%
  arrange(predicted_count_rate) %>%
  select(predicted_count_rate, lapse_count_rate) %>%
  ggplot(aes(x = predicted_count_rate, y = lapse_count_rate)) +
  geom_point()

validation_summary %>%
  arrange(predicted_count_rate) %>%
  select(predicted_count_rate, lapse_count_rate) %>%
  mutate(decile = cut(predicted_count_rate, quantile(
    predicted_count_rate, probs = seq(0, 1, 0.1)
  ), include.lowest = TRUE)) %>%
  group_by(decile) %>%
  summarize(mean_predicted = mean(predicted_count_rate),
            mean_actual = mean(lapse_count_rate)) %>%
  gather("key", "value", -"decile") %>%
  ggplot(aes(x = decile, y = value, fill = key)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Average Actual vs. Predicted Lapse Rates") +
  coord_flip()


