library(tidyverse)

# read in the data
care_state <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-08/care_state.csv') |> 
  janitor::clean_names()

# let's take a quick peek at the data
care_state %>%
  head()

# what timeframes are we working with?
# some yearly, some quarterly
care_state %>%
  group_by(
    start_date,
    end_date
  ) %>%
  tally()

# make our data nice so we can plot flu vs covid 
# healthcare worker vaccination rates by state
correlation_data <- care_state %>%
  filter(
    condition == 'Healthcare Personnel Vaccination'
  ) %>%
  mutate(
    measure_name = case_when(
      stringr::str_detect(measure_name, 'COVID') ~ 'covid_vac_pct',
      stringr::str_detect(measure_name, 'influenza') ~ 'flu_vac_pct',
      TRUE ~ 'other'
    )
  ) %>%
  pivot_wider(
    id_cols = state,
    names_from = measure_name,
    values_from = score
  ) 

# plot it!
correlation_data %>%
  ggplot(
    aes(
      x = covid_vac_pct,
      y = flu_vac_pct
    )
  ) +
  geom_point() +
  theme_minimal()

# make our data nice so we can plot psych vs reg
# treatment times by state
treat_time_correlation_data <- care_state %>%
  filter(
    measure_id %in% c('OP_18b', 'OP_18c')
  ) %>%
  mutate(
    measure_name = case_when(
      measure_id == 'OP_18b' ~ 'reg_time',
      measure_id == 'OP_18c' ~ 'psych_time',
      TRUE ~ 'other'
    ),
    # convert to hours
    score = score / 60 
  ) %>%
  pivot_wider(
    id_cols = state,
    names_from = measure_name,
    values_from = score
  )

# plot it!
treat_time_correlation_data %>%
  ggplot(
    aes(
      x = reg_time,
      y = psych_time
    )
  ) + 
  geom_abline(
    intercept = 0, 
    slope = 1,
    linetype = 'dashed',
    color = 'grey'
  ) +
  geom_point() +
  theme_minimal() +
  labs(
    x = 'Median Emergency Treatment Time (hours)',
    y = 'Median Psych/Mental Health Treatment Time (hours)',
    title = 'Psychiatric/Mental Health treatment times are longer for all states compared to non-Psychiatric/Mental Health treatment times.'
  ) +
  theme(
    plot.title = ggtext::element_textbox_simple()
  )


