require(dplyr)
require(ggplot2)
require(tidyr)
require(lubridate)
require(stringr)

violations_data <- read.csv('HOSP2567.csv')

psych <- violations_data %>%
  filter(hospital_type == "Psychiatric") %>%
  select(ï..facility_name, facility_id, state, deficiency_tag, inspection_date, EVENT_ID,
          inspection_text)

psych$inspection_date <- parse_date_time(psych$inspection_date, "%m/%d/%y")

total_violations <- psych %>%
  group_by(facility_id) %>%
  summarize(num_violations = n())

ownership_data <- read.csv('acadia_ownership.csv')

ownership_violations <- select(ownership_data, facility_id, ownership, is_acadia)
ownership_violations <- left_join(total_violations, ownership_violations, by='facility_id')

violations_by_ownership <- ownership_violations %>%
  group_by(ownership) %>%
  summarize(mean_violations = mean(num_violations))

current_ownership_violations <- ownership_violations %>%
  filter(ownership == "proprietary") %>%
  group_by(is_acadia) %>%
  summarize(mean_violations = mean(num_violations))

inspections_data <- select(ownership_data, facility_id, ownership, is_acadia,
                           acadia_purchase_date)
inspections_data$acadia_purchase_date <- parse_date_time(inspections_data$acadia_purchase_date,
                                                         "%m/%d/%y")
inspections_data <- left_join(psych, inspections_data, by='facility_id')
inspections_data <- mutate(inspections_data, was_inspection_acadia =
                             as.numeric(acadia_purchase_date < inspection_date))
inspections_data$was_inspection_acadia[is.na(inspections_data$was_inspection_acadia)] <- 0

x1 <- c(1, 2, 3)
inspections_data$ownership <- x1[match(inspections_data$ownership,
                                       c('government', 'nonprofit', 'proprietary'))]

violations_per_inspection <- inspections_data %>%
  mutate(inspection_type = (ownership + was_inspection_acadia)) %>%
  group_by(inspection_type) %>%
  summarize(violations_per_inspection = (n() / n_distinct(EVENT_ID)))
