library(insurance)
library(tidyverse)
# glimpse(insurance::lapse_study)

data <- insurance::lapse_study %>%
  # Keep only duration 10 data.
  filter(duration == "10") %>%
  # Keep only Premium Jump to ART
  filter(post_level_premium_structure == "Premium Jump to ART") %>%
  # Remove empty exposures.
  filter(exposure_count > 0, exposure_amount > 0) %>%
  mutate(
    lapse_count_rate = lapse_count / exposure_count,
    lapse_amount_rate = lapse_amount / exposure_amount
  )
