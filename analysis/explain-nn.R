library(DALEX)

make_custom_predict_nn <- function(rec) {
  function(model, new_data) {
    keras_data <- bake(rec, new_data) %>%
      prep_keras_data()
    predict(model, keras_data$x)[[1]] %>%
      as.vector()
  }
}

explainer_nn <- DALEX::explain(
  model, data = select(validation_data, predictors),
  y = validation_data$lapse_count_rate,
  predict_function = make_custom_predict_nn(rec_nn),
  label = "neural_net"
)

new_data <- validation_data[53,] %>% select(predictors)
pb_nn <- prediction_breakdown(explainer_nn, observation = new_data)
plot(pb_nn)

vi_nn <- variable_importance(explainer_nn)
plot(vi_nn)

pdp_nn <- variable_response(explainer_nn, variable = "avg_issue_age")
plot(pdp_nn)

## embedding weights

model$get_layer("embedding_premium_jump_ratio")$get_weights() %>%
  as.data.frame() %>%
  cbind(rec_nn$steps[[1]]$key$premium_jump_ratio) %>%
  mutate(value = gsub("^[A-Z]\\.[ ]+", "", value)) %>%
  ggplot(aes(x = X1, y = X2, color = integer)) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = value)) +
  theme_bw() +
  guides(color = FALSE) +
  ggtitle("Embedding weights for premium jump ratio")
