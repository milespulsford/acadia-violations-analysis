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


forum_theme <- theme(
  panel.grid.major.y = element_blank(),
  panel.grid.major.x = element_line(color = '#CDCDCD'),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = 'white'),
  axis.ticks = element_blank(),
  plot.margin = unit(c(25, 25, 25, 25), 'point'),
  axis.text = element_text(color = '#333333', size = 10),
  axis.title = element_text(color = '#333333', size = 12),
  plot.title = element_text(hjust = 0, margin = margin(0,0,25,0), size = 16),
  legend.position = "none"
)

vpichart_data <- violations_per_inspection %>%
  mutate(highlight = ifelse(inspection_type == 4, 'yes', 'no'))

violations_chart <- ggplot(vpichart_data, aes(x = inspection_type,
                                              y = violations_per_inspection,
                                              fill = highlight)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c('yes' = '#6B0F1A', 'no' = '#0e0e40')) +
  labs(title = 'Hospital Inspection Violations', x = 'Facility Ownership',
       y = 'Violations Per Inspection') +
  ylim(0, 7) +
  coord_flip() +
  forum_theme

ggsave('violations_chart.pdf', violations_chart)