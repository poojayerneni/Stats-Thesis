
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
  labs(x = "Time (years)", y = "Proportion",
       title = "Proportions of Separated Compartments") +
  theme(legend.position = "none")

# New symptomatic cases per time step
incidence <- diff(out$I)  # change in I between steps
plot(times[-1], incidence, type = "l",
     xlab = "Time (years)", ylab = "New symptomatic cases",
     main = "Incidence of Symptomatic Cases")
#new deaths 
new_deaths <- diff(out$D)
plot(times[-1], new_deaths, type = "l",
     xlab = "Time (years)", ylab = "New deaths",
     main = "Daily Deaths")

#heatplot
library(viridis)

ggplot(long, aes(x = time, y = variable, fill = value)) +
  geom_tile() +
  scale_fill_viridis() +
  theme_minimal() +
  labs(x = "Time (years)", y = "Compartment",
       fill = "Proportion",
       title = "Heatmap of Compartments Over Time")

#peak infections
out$Infectious <- out$A + out$I
ggplot(out, aes(x = time)) +
  geom_line(aes(y = A, color = "A")) +
  geom_line(aes(y = I, color = "I")) +
  geom_line(aes(y = Infectious, color = "A + I"), size = 1.2) +
  theme_minimal() +
  labs(x = "Time (years)", y = "Proportion", color = "Compartment",
       title = "Asymptomatic, Symptomatic, and Total Infectious")


### FOR ONE YEAR USE OUT_YEAR
#time series
plot_data <- out_year %>% 
  select(-Daily_New_Infections, -Cumulative_Infections)

matplot(plot_data$time, plot_data[,-1], type = "l", lty = 1,
        xlab = "Time (days)", ylab = "Proportion",
        main = "SEAIHCRD Model over 1 Year")
legend("right", legend = colnames(plot_data)[-1],
       col = 1:length(colnames(plot_data)[-1]), lty = 1, cex = 0.6)
matplot(out_year$time, out_year[,-1], type = "l", lty = 1,
        xlab = "Time (days)", ylab = "Proportion",
        main = "SEAIHCRD Model over 1 Year")
legend("right", legend = colnames(out_year)[-1],
       col = 1:8, lty = 1, cex = 0.6)

#panels of individual compartments

long_yr <- melt(plot_data, id = "time")

ggplot(long_yr, aes(x = time, y = value, colour = variable)) +
  geom_line(size = 1) +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  labs(x = "Time (days)", y = "Proportion",
       title = "Proportions of Separated Compartments for 1 Year") +
  theme(legend.position = "none")

#infections plot
out_year$active_infections <- out_year$A + out_year$I  #infectious prevalence (A + I)
peak_prevalence <- max(out_year$active_infections)
avg_prevalence  <- mean(out_year$active_infections)
peak_prevalence
avg_prevalence
ggplot(out_year, aes(x = time)) +
  geom_line(aes(y = A, color = "A")) +
  geom_line(aes(y = I, color = "I")) +
  geom_line(aes(y = active_infections, color = "A + I"), size = 1.2) +
  theme_minimal() +
  labs(x = "Time (days)", y = "Proportion", color = "Compartment",
       title = "Asymptomatic, Symptomatic, and Total Infectious for 1 Year")

#mean time to death= (weighted average of time until cumulative deaths reach 50%)
cum_deaths = out_year$D
median_time_index = which.min(abs(cum_deaths - max(cum_deaths)/2))
median_time_to_death = out_year$time[median_time_index]
cat("Median time to death (days):", median_time_to_death, "\n")

#death rate per person-year where each person contributes roughly proportion alive * time
library(pracma)
person_years = trapz(out_year$time, 1 - out$D) / 365  # integrate (alive fraction) over time
death_rate = final_deaths / person_years
cat("Deaths per person-year:", death_rate, "\n")


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
# Find peak values and their timing
# Find peaks for each compartment
compartments_peak <- c("S", "E", "A", "I", "H", "C", "R", "D")

peaks <- data.frame(
  Compartment = compartments_peak,
  Peak_Time = sapply(compartments_peak, function(x) out_year$time[which.max(out_year[[x]])]),
  Peak_Value = sapply(compartments_peak, function(x) max(out_year[[x]]))
)

print(peaks)

# With population scaling
peaks_scaled <- peaks
peaks_scaled$Peak_Value <- peaks$Peak_Value * pop_size
print(peaks_scaled)

# Time series plot
matplot(out_scaled$time, out_scaled[,-1], type = "l", lty = 1,
        xlab = "Time (days)", ylab = "Number of People",
        main = "SEAIHCRD Model over 1 Year (N=100,000)",
        ylim = c(0, 100000), yaxt = "n")
axis(2, at = seq(0, 100000, by = 20000), 
     labels = format(seq(0, 100000, by = 20000), big.mark = ","))
  

# Add y-axis with ticks at peak values
#axis(2, at = peaks_scaled$Peak_Value, labels = round(peaks_scaled$Peak_Value), 
     #las = 2, cex.axis = 0.7)

# Add points at peaks using peaks_scaled dataframe
for(i in 1:nrow(peaks_scaled)) {
  points(peaks_scaled$Peak_Time[i], peaks_scaled$Peak_Value[i], 
         pch = 10, col = i, cex = 1.2)
}

# Add legend
legend("right", legend = names(out_scaled)[-1], col = 1:8, lty = 1, lwd = 2, cex = 0.8)


#new infections and deaths
# First, calculate cumulative infections for PROPORTIONS
out_year$Cumulative_Infections <- 1 - out_year$S  # Total who left S compartment

# Then calculate daily new infections (proportions)
out_year$Daily_New_Infections <- c(0, diff(out_year$Cumulative_Infections))

# Now scale to population
out_scaled$Cumulative_Infections <- out_year$Cumulative_Infections * pop_size
out_scaled$Daily_New_Infections <- out_year$Daily_New_Infections * pop_size

# Plot epidemic curve
plot(out_scaled$time, out_scaled$Daily_New_Infections, 
     type = "l", lwd = 2, col = "darkblue",
     xlab = "Time (days)", ylab = "New Cases per Day",
     main = "Epidemic Curve - Daily New Infections")

# Daily new deaths
out_scaled$Daily_New_Deaths <- c(0, diff(out_scaled$D))

ggplot(out_scaled, aes(x = time, y = Daily_New_Deaths)) +
  geom_line(linewidth = 1, color = "darkred") +
  labs(title = "Daily Deaths",
       x = "Time (days)", y = "Deaths per Day") +
  theme_minimal()


  
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
  
