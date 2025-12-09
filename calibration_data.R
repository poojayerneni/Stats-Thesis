
# choose first major peak in cases
calibration_data <- combined_data %>%
  filter(week_ending >= as.Date("2020-03-01") & 
           week_ending <= as.Date("2020-06-15")) %>%
  pivot_wider(names_from = measure, values_from = rate)

# Create time variable (days since start)
calibration_data <- calibration_data %>%
  mutate(
    time_days = as.numeric(week_ending - min(week_ending))
  )

print(calibration_data)

# Visualize calibration period
calibration_long <- calibration_data %>%
  pivot_longer(cols = c(Cases, Deaths, Hospitalizations),
               names_to = "measure", values_to = "rate")

ggplot(calibration_long, aes(x = time_days, y = rate, color = measure)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~measure, scales = "free_y", ncol = 1) +
  labs(title = "Calibration Data: Spring 2020 Wave",
       subtitle = "March 1 - June 15, 2020",
       x = "Days since March 1, 2020", 
       y = "Rate per 100,000") +
  theme_minimal()


