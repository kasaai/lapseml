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
