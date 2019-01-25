source("analysis/data-prep.R")

mod_form <- lapse_count ~
  risk_class_mapped + face_amount + premium_mode_mapped +
  avg_issue_age +
  I(avg_issue_age ^ 2) + log(avg_issue_age) +
  I((duration - 9) ^ (-1)) + I((duration - 9) ^ (-2)) + I((duration - 9) ^ (-3)) +
  I(avg_premium_jump_ratio ^ (-1)) + I(avg_premium_jump_ratio ^ (-2)) + I(avg_premium_jump_ratio ^ (-3)) +
  avg_issue_age:I(avg_premium_jump_ratio ^ (-1)) + avg_issue_age:I(duration - 9) +
  offset(log(exposure_count))


soa2015_model <- glm(mod_form, family = poisson(), data = training_data)

validation_data %>%
  mutate(
    predicted_lapse_rate = predict(soa2015_model,
                                   validation_data,
                                   type = "response") /
      exposure_count
  ) %>%
  select(lapse_count_rate, predicted_lapse_rate, exposure_count) %>%
  weighted_rmse("lapse_count_rate", "predicted_lapse_rate", "exposure_count")

# [1] 0.1720286
