
COVID_cases <- read.csv("~/Desktop/THESIS/Data/COVID_cases.csv")
COVID_hospital <- read.csv("~/Desktop/THESIS/Data/COVID_hospital.csv")

#clean datasets
library(dplyr)
library(lubridate)
# Clean cases/deaths 
COVID_cases <- COVID_cases %>%
  mutate(
    # Convert dates
    date_updated = mdy(date_updated),
    start_date = mdy(start_date),
    week_ending = mdy(end_date),
    # Convert numeric variables (remove commas first)
    tot_cases = as.numeric(gsub(",", "", tot_cases)),
    new_cases = as.numeric(gsub(",", "", new_cases)),
    tot_deaths = as.numeric(gsub(",", "", tot_deaths)),
    new_deaths = as.numeric(gsub(",", "", new_deaths)),
    new_historic_cases = as.numeric(gsub(",", "", new_historic_cases)),
    new_historic_deaths = as.numeric(gsub(",", "", new_historic_deaths))) %>%
  select(-end_date)  # Remove old column name

# Clean COVID_hospital dataset (rates by state)
COVID_hospital <- COVID_hospital %>%
  mutate(
    # Convert date and rename
    week_ending = as.Date(X_WeekendDate),
    # Convert and rename numeric variables
    weekly_hospital_rate = as.numeric(WeeklyRate),
    cumulative_hospital_rate = as.numeric(CumulativeRate)) %>%
  filter(Type == "Crude Rate") %>%  # Only keep crude rates
  select(-X_WeekendDate, -WeeklyRate, -CumulativeRate)  # Remove old column names

# Create rates in cases and deaths
us_cases_deaths <- COVID_cases %>%
  group_by(week_ending) %>%
  summarize(
    total_new_cases = sum(new_cases, na.rm = TRUE),
    total_new_deaths = sum(new_deaths, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(
    # Calculate rates per 100,000 (US population ~332 million)
    weekly_case_rate = (total_new_cases / 331900000) * 100000,
    weekly_death_rate = (total_new_deaths / 331900000) * 100000)

# Aggregate hospitalizations to US weekly average
us_hospital <- COVID_hospital %>%
  mutate(week_ending = week_ending - 3) %>% #line up dates with other dataset
  group_by(week_ending) %>%
  summarize(weekly_hospital_rate = mean(weekly_hospital_rate, na.rm = TRUE),
    .groups = "drop")


# Merge all three measures: 167 weeks of data from march 4 2020
combined_data <- us_cases_deaths %>%
  inner_join(us_hospital, by = "week_ending") %>%
  select(week_ending, weekly_case_rate, weekly_death_rate, weekly_hospital_rate) %>%
  pivot_longer(
    cols = c(weekly_case_rate, weekly_death_rate, weekly_hospital_rate),
    names_to = "measure",
    values_to = "rate"
  ) %>%
  mutate(
    measure = case_when(
      measure == "weekly_case_rate" ~ "Cases",
      measure == "weekly_death_rate" ~ "Deaths",
      measure == "weekly_hospital_rate" ~ "Hospitalizations"
    )
  ) %>%
  filter(!is.na(rate))  # Remove any missing values

# Create time series plot
ggplot(combined_data, aes(x = week_ending, y = rate, color = measure)) +
  geom_line(linewidth = 1) +
  labs(
    title = "COVID-19 Weekly Rates in the United States",
    subtitle = "Cases, Hospitalizations, and Deaths Over Time",
    x = "Date",
    y = "Rate per 100,000 Population",
    color = "Measure"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c(
    "Cases" = "#2E86AB", 
    "Hospitalizations" = "#A23B72", 
    "Deaths" = "#F18F01"
  ))


# Faceted time series plot (better for different scales)
ggplot(combined_data, aes(x = week_ending, y = rate, color = measure)) +
  geom_line(linewidth = 1) +
  facet_wrap(~measure, scales = "free_y", ncol = 1) +
  labs(
    title = "COVID-19 Weekly Rates in the United States",
    x = "Date",
    y = "Rate per 100,000 Population"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 11)
  ) +
  scale_color_manual(values = c(
    "Cases" = "#2E86AB", 
    "Hospitalizations" = "#A23B72", 
    "Deaths" = "#F18F01"
  ))

range.Date(combined_data$week_ending)