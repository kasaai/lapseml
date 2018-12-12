library(insurance)
library(tidyverse)
# glimpse(insurance::lapse_study)

issue_age_mapping <- tribble(
  ~ age_band, ~ avg_issue_age,
  "0-19",     10,
  "20-29",    25,
  "30-39",    35,
  "40-49",    45,
  "50-59",    55,
  "60-69",    65,
  "70+",      75,
)

# Map levels to midpoint of bands per SOA2015
premium_jump_ratio_mapping <- tribble(
  ~ premium_jump_ratio_band, ~ avg_premium_jump_ratio,
  "A.  1.01 - 2.00",    1.5,
  "B.  2.01 - 3.00",    2.5,
  "C.  3.01 - 4.00",    3.5,
  "D.  4.01 - 5.00",    4.5,
  "E.  5.01 - 6.00",    5.5,
  "F.  6.01 - 7.00",    6.5,
  "G.  7.01 - 8.00",    7.5,
  "H.  8.01 - 9.00",    8.5,
  "I.  9.01 - 10.00",   9.5,
  "J. 10.01 - 11.00",   10.5,
  "K. 11.01 - 12.00",   11.5,
  "L. 12.01 - 13.00",   12.5,
  "M. 13.01 - 14.00",   13.5,
  "N. 14.01 - 15.00",   14.5,
  "O. 15.01 - 16.00",   15.5,
  "P. 16.01 - 17.00",   16.5,
  "Q. 17.01 - 18.00",   17.5,
  "R. 18.01 - 19.00",   18.5,
  "S. 19.01 - 20.00",   19.5,
  "T. 20.01 - 21.00",   20.5,
  "U. 21.01 - 22.00",   21.5,
  "V. 22.01 - 23.00",   22.5,
  "W. 23.01 - 24.00",   23.5,
  # SOA 2014 p. 18
  "X. 24.01 AND UP",    27.9
)

data <- insurance::lapse_study %>%
  # Keep only duration 10 - 12 data.
  filter(duration %in% c("10", "11", "12")) %>%
  # Keep only Premium Jump to ART
  filter(post_level_premium_structure == "1. Premium Jump to ART") %>%
  # Keep only known premium jump ratios
  filter(premium_jump_ratio != "Y. Unknown") %>%
  # Remove empty exposures.
  filter(exposure_count > 0, exposure_amount > 0) %>%
  # Join with issue age mapping
  left_join(issue_age_mapping, by = c(issue_age = "age_band")) %>%
  # Join with premium jump ratio mapping
  left_join(premium_jump_ratio_mapping, by = c(premium_jump_ratio = "premium_jump_ratio_band")) %>%
  mutate(
    lapse_count_rate = lapse_count / exposure_count,
    lapse_amount_rate = lapse_amount / exposure_amount,
    duration = as.integer(duration)
  )

training_data <- filter(data, policy_year < 2011)
testing_data <- filter(data, policy_year == 2011)
