
#initial plots for compartmental
library(deSolve)
library(ggplot2)
library(reshape2)
#PROPORTION-BASED PLOT FOR 100 YEARS, USE OUT
#simple time series
matplot(out$time, out[,-1], type = "l", lty = 1,
        xlab = "Time (years)", ylab = "Proportion",
        main = "Compartmental Model")
legend("right", legend = colnames(out)[-1],
       col = 1:8, lty = 1, cex = 0.6)
#panels of individual compartments

long <- melt(out, id = "time")

ggplot(long, aes(x = time, y = value, colour = variable)) +
  geom_line(size = 1) +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  labs(x = "Time (days)", y = "Proportion",
       title = "Separate Compartments") +
  theme(legend.position = "none")

# New symptomatic cases per time step
incidence <- diff(out$I)  # change in I between steps
plot(times[-1], incidence, type = "l",
     xlab = "Time (days)", ylab = "New symptomatic cases",
     main = "Incidence of Symptomatic Cases")
#new deaths 
new_deaths <- diff(out$D)
plot(times[-1], new_deaths, type = "l",
     xlab = "Time (days)", ylab = "New deaths",
     main = "Daily Deaths")

#heatplot
library(viridis)

ggplot(long, aes(x = time, y = variable, fill = value)) +
  geom_tile() +
  scale_fill_viridis() +
  theme_minimal() +
  labs(x = "Time (days)", y = "Compartment",
       fill = "Proportion",
       title = "Heatmap of Compartments Over Time")

#peak infections
out$Infectious <- out$A + out$I
ggplot(out, aes(x = time)) +
  geom_line(aes(y = A, color = "A")) +
  geom_line(aes(y = I, color = "I")) +
  geom_line(aes(y = Infectious, color = "A + I"), size = 1.2) +
  theme_minimal() +
  labs(x = "Time (days)", y = "Proportion", color = "Compartment",
       title = "Asymptomatic, Symptomatic, and Total Infectious")


### FOR ONE YEAR USE OUT_YEAR
#time series
matplot(out_year$time, out_year[,-1], type = "l", lty = 1,
        xlab = "Time (days)", ylab = "Proportion",
        main = "SEAIHCRD Model over 1 Year")
legend("right", legend = colnames(out_year)[-1],
       col = 1:8, lty = 1, cex = 0.6)

#final deaths after 1 year
final_deaths = tail(out_year$D, 1)
cat("Final proportion of deaths after 1 year:", final_deaths, "\n")

#% of total population that died (death prevalence)
death_prevalence = final_deaths * 100
print(death_prevalence)
cat("Death prevalence (%):", death_prevalence, "%\n")

#mean time to death= (weighted average of time until cumulative deaths reach 50%)
cum_deaths = out_year$D
median_time_index = which.min(abs(cum_deaths - max(cum_deaths)/2))
median_time_to_death = out_year$time[median_time_index]
cat("Median time to death (days):", median_time_to_death, "\n")

#death rate per person-year where each person contributes roughly proportion alive * time
person_years = trapz(out_year$time, 1 - out$D) / 365  # integrate (alive fraction) over time
death_rate = final_deaths / person_years
cat("Deaths per person-year:", death_rate, "\n")

out_year$active_infections <- out_year$A + out_year$I  #infectious prevalence (A + I)
peak_prevalence <- max(out_year$active_infections)
avg_prevalence  <- mean(out_year$active_infections)
peak_prevalence
avg_prevalence

library(ggplot2)

# Compute cumulative infections and active infections
out_year$cum_infections <- out_year$S[1] - out_year$S
out_year$day <- floor(out_year$time)

# Aggregate to daily data
daily <- aggregate(cbind(cum_infections, active_infections) ~ day, data = out_year, FUN = max)

# Plot cumulative and active infections over 1 year in proportions
ggplot(daily, aes(x = day)) +
  geom_col(aes(y = cum_infections), fill = "steelblue", alpha = 0.5) +
  geom_line(aes(y = active_infections), color = "red", size = 1.2) +
  labs(
    title = "Active vs. Cumulative Infections Over 1 Year",
    x = "Day",
    y = "Proportion of Population",
    caption = "Blue bars= cumulative infections; Red line= active infections (A+I)"
  ) +
  theme_minimal(base_size = 13)

### FOR 1 YEAR AND 100,000 PPL USE OUT_SCALED
# Calculate peak values for each compartment
peaks <- sapply(out_scaled[, -1], max)
peak_text <- paste(names(peaks), ": ", format(round(peaks), big.mark = ","), sep = "")
caption <- paste("Peak values -", paste(peak_text, collapse = " | "))

#time series: COME BACK TO THIS BC NUMBERS ARE CRAZY
matplot(out_scaled$time, out_scaled[,-1], type = "l", lty = 1,
        xlab = "Time (days)", ylab = "Number of People",
        main = "SEAIHCRD Model over 1 Year (N=100,000)",
        yaxt = "n")  # Suppress default y-axis

# Add custom y-axis with comma formatting
y_axis_vals <- axTicks(2)
axis(2, at = y_axis_vals, labels = format(y_axis_vals, big.mark = ",", scientific = FALSE))

legend("right", legend = colnames(out_scaled)[-1],
       col = 1:8, lty = 1, cex = 0.6)

# Add caption below the plot
mtext(caption, side = 1, line = 4, cex = 0.7, adj = 0)

# Print peak values to console
cat("\nPeak values for each compartment:\n")
for (i in 1:length(peaks)) {
  cat(sprintf("%s: %s individuals\n", names(peaks)[i], format(round(peaks[i]), big.mark = ",")))
  
###ONE YEAR FOR 350 FOR MSM COMPARISON
  # Compute cumulative infections and active infections (scaled to population)
  out_year$cum_infections <- (out_year$S[1] - out_year$S) * 350
  out_year$active_infections <- (out_year$A + out_year$I) * 350
  out_year$day <- floor(out_year$time)
  
  # Aggregate to daily data
  daily_350 <- aggregate(cbind(cum_infections, active_infections) ~ day, data = out_year, FUN = max)
  
  # Plot with scaled population
  ggplot(daily_350, aes(x = day)) +
    geom_col(aes(y = cum_infections), fill = "steelblue", alpha = 0.5) +
    geom_line(aes(y = active_infections), color = "red", size = 1.2) +
    labs(
      title = "Active vs. Cumulative Infections Over 1 Year (N=350)",
      x = "Day",
      y = "Number of People",
      caption = "Blue bars = cumulative infections; Red line = active infections (A+I)"
    ) +
    theme_minimal(base_size = 13)
}