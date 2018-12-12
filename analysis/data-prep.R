library(insurance)
library(tidyverse)
# glimpse(insurance::lapse_study)

issue_age_mapping <- tribble(
  ~ age_band, ~ average_issue_age,
  "0-19",     9.5,
  "20-29",    26.3,
  "30-39",    34.7,
  "40-49",    44.2,
  "50-59",    53.7,
  "60-69",    63.3,
  "70+",      72.3
)

data <- insurance::lapse_study %>%
  # Keep only duration 10 data.
  filter(duration == "10") %>%
  # Keep only Premium Jump to ART
  filter(post_level_premium_structure == "1. Premium Jump to ART") %>%
  # Remove empty exposures.
  filter(exposure_count > 0, exposure_amount > 0) %>%
  # Join with issue age mapping
  left_join(issue_age_mapping, by = c(issue_age = "age_band")) %>%
  mutate(
    lapse_count_rate = lapse_count / exposure_count,
    lapse_amount_rate = lapse_amount / exposure_amount
  )

training_data <- filter(policy_year < 2011)
testing_data <- filter(policy_year == 2011)
