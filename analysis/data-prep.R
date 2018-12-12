library(insurance)
library(tidyverse)
# glimpse(insurance::lapse_study)

issue_age_mapping <- tribble(
  ~ age_band, ~ average_issue_age,
  "0-19",     10,
  "20-29",    25,
  "30-39",    35,
  "40-49",    45,
  "50-59",    55,
  "60-69",    65,
  "70+",      75,
)

data <- insurance::lapse_study %>%
  # Keep only duration 10 - 12 data.
  filter(duration %in% c("10", "11", "12")) %>%
  # Keep only Premium Jump to ART
  filter(post_level_premium_structure == "1. Premium Jump to ART") %>%
  # Remove empty exposures.
  filter(exposure_count > 0, exposure_amount > 0) %>%
  # Join with issue age mapping
  left_join(issue_age_mapping, by = c(issue_age = "age_band")) %>%
  mutate(
    lapse_count_rate = lapse_count / exposure_count,
    lapse_amount_rate = lapse_amount / exposure_amount,
    duration = as.integer(duration)
  )

training_data <- filter(data, policy_year < 2011)
testing_data <- filter(data, policy_year == 2011)
