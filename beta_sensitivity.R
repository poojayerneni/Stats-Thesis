
beta_values <- seq(0.5, 1.0, by = 0.1)
peak_infections <- numeric(length(beta_values))

for(i in seq_along(beta_values)) {
  parms_temp <- parms
  parms_temp["beta"] <- beta_values[i]
  out_temp <- as.data.frame(ode(startconds, times_1yr, seaihcrdmod, parms_temp))
  peak_infections[i] <- max(out_temp$I) * pop_size
}

sensitivity_df <- data.frame(beta = beta_values, peak = peak_infections)
# beta of peak symptomatic infectionss
ggplot(sensitivity_df, aes(x = beta, y = peak)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "Sensitivity Analysis: Effect of Transmission Rate",
       x = "Beta", 
       y = "Peak Symptomatic Infections") +
  theme_minimal()

#proportion of peak symptomatic infections

for(i in seq_along(beta_values)) {
  parms_temp <- parms
  parms_temp["beta"] <- beta_values[i]
  out_temp <- as.data.frame(ode(startconds, times_1yr, seaihcrdmod, parms_temp))
  peak_infections[i] <- max(out_temp$I) 
}

sensitivity_df_prob <- data.frame(beta = beta_values, peak = peak_infections)

ggplot(sensitivity_df, aes(x = beta, y = peak)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "Sensitivity Analysis: Effect of Transmission Rate",
       x = "Beta", 
       y = "Peak Symptomatic Infections") +
  theme_minimal()

# Sensitivity analysis for all compartments
beta_values <- seq(0.5, 1.0, by = 0.1)
n_beta <- length(beta_values)

# Initialize storage for all compartments
sensitivity_all <- data.frame(
  beta = rep(beta_values, each = 1),
  peak_S = numeric(n_beta),
  peak_E = numeric(n_beta),
  peak_A = numeric(n_beta),
  peak_I = numeric(n_beta),
  peak_H = numeric(n_beta),
  peak_C = numeric(n_beta),
  peak_R = numeric(n_beta),
  peak_D = numeric(n_beta),
  peak_Total_Infected = numeric(n_beta),  # A + I combined
  peak_Healthcare = numeric(n_beta)        # H + C combined
)

# Run simulations
for(i in seq_along(beta_values)) {
  parms_temp <- parms
  parms_temp["beta"] <- beta_values[i]
  out_temp <- as.data.frame(ode(startconds, times_1yr, seaihcrdmod, parms_temp))
  
  # Store peak values for each compartment (proportions)
  sensitivity_all$peak_S[i] <- max(out_temp$S)
  sensitivity_all$peak_E[i] <- max(out_temp$E)
  sensitivity_all$peak_A[i] <- max(out_temp$A)
  sensitivity_all$peak_I[i] <- max(out_temp$I)
  sensitivity_all$peak_H[i] <- max(out_temp$H)
  sensitivity_all$peak_C[i] <- max(out_temp$C)
  sensitivity_all$peak_R[i] <- max(out_temp$R)
  sensitivity_all$peak_D[i] <- max(out_temp$D)
  sensitivity_all$peak_Total_Infected[i] <- max(out_temp$A + out_temp$I)
  sensitivity_all$peak_Healthcare[i] <- max(out_temp$H + out_temp$C)
}

# Scale to population size
sensitivity_scaled <- sensitivity_all
sensitivity_scaled[, -1] <- sensitivity_all[, -1] * pop_size

# PLOT 1: All compartments on one plot (proportions)

sensitivity_long <- sensitivity_all %>%
  pivot_longer(cols = starts_with("peak_"), 
               names_to = "Compartment", 
               values_to = "Peak_Value") %>%
  mutate(Compartment = gsub("peak_", "", Compartment))

ggplot(sensitivity_long, aes(x = beta, y = Peak_Value, color = Compartment)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Sensitivity Analysis: Effect of Beta on All Compartments",
       x = "Beta", 
       y = "Peak Proportion",
       color = "Compartment") +
  theme_minimal() +
  theme(legend.position = "right")

# PLOT 2: Combined infections (A + I) - scaled to population
ggplot(sensitivity_scaled, aes(x = beta, y = peak_Total_Infected)) +
  geom_line(linewidth = 1.5, color = "darkred") +
  geom_point(size = 3, color = "darkred") +
  labs(title = "Sensitivity Analysis: Combined Infections (Asymptomatic + Symptomatic)",
       x = "Beta", 
       y = "Peak Total Infected Individuals") +
  theme_minimal()

# PLOT 3: Key compartments only (I, H, C, D) - scaled
sensitivity_key <- sensitivity_scaled %>%
  select(beta, peak_I, peak_H, peak_C, peak_D) %>%
  pivot_longer(cols = -beta, names_to = "Compartment", values_to = "Peak_Value") %>%
  mutate(Compartment = gsub("peak_", "", Compartment))

ggplot(sensitivity_key, aes(x = beta, y = Peak_Value, color = Compartment)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  labs(title = "Sensitivity Analysis: Key Health Outcomes",
       x = "Beta", 
       y = "Peak Number of Individuals",
       color = "Compartment") +
  theme_minimal() +
  scale_color_manual(values = c("I" = "orange", "H" = "red", 
                                "C" = "darkred", "D" = "black"),
                     labels = c("Symptomatic", "Hospitalized", "ICU", "Deaths"))

# PLOT 4: Healthcare burden (H + C)
ggplot(sensitivity_scaled, aes(x = beta, y = peak_Healthcare)) +
  geom_line(linewidth = 1.5, color = "purple") +
  geom_point(size = 3, color = "purple") +
  labs(title = "Sensitivity Analysis: Peak Healthcare Burden",
       x = "Beta", 
       y = "Peak Healthcare Patients (Hospital + ICU)") +
  theme_minimal()

# PLOT 5: Faceted view of all compartments (scaled)
sensitivity_scaled_long <- sensitivity_scaled %>%
  pivot_longer(cols = starts_with("peak_"), 
               names_to = "Compartment", 
               values_to = "Peak_Value") %>%
  mutate(Compartment = gsub("peak_", "", Compartment))

ggplot(sensitivity_scaled_long, aes(x = beta, y = Peak_Value)) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  facet_wrap(~ Compartment, scales = "free_y", ncol = 3) +
  labs(title = "Sensitivity Analysis: Effect of Beta on All Compartments",
       x = "Beta", 
       y = "Peak Value (N = 100,000)") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

# Print summary table
print("Peak values at different beta values (scaled to population):")
print(sensitivity_scaled)


###MONTE CARLO SIMULATIONS
#basic symptomatic infections
# Run model multiple times with parameter uncertainty
set.seed(123)
n_sim <- 100

# Define parameter uncertainty (±20%)
mc_results <- data.frame()

for(sim in 1:n_sim) {
  parms_mc <- parms
  
  # Add uncertainty to key parameters
  parms_mc["beta"] <- parms["beta"] * runif(1, 0.8, 1.2)
  parms_mc["rho"] <- pmax(0, pmin(1, parms["rho"] * runif(1, 0.8, 1.2)))
  parms_mc["eta"] <- parms["eta"] * runif(1, 0.8, 1.2)
  parms_mc["deltaH"] <- parms["deltaH"] * runif(1, 0.8, 1.2)
  parms_mc["deltaC"] <- parms["deltaC"] * runif(1, 0.8, 1.2)
  
  out_mc <- as.data.frame(ode(startconds, times_1yr, seaihcrdmod, parms_mc))
  out_mc$Simulation <- sim
  
  mc_results <- rbind(mc_results, out_mc)
}

mc_results[, 2:9] <- mc_results[, 2:9] * pop_size

# Calculate confidence intervals
mc_summary <- mc_results %>%
  group_by(time) %>%
  summarize(
    I_median = median(I),
    I_lower = quantile(I, 0.025),
    I_upper = quantile(I, 0.975),
    D_median = median(D),
    D_lower = quantile(D, 0.025),
    D_upper = quantile(D, 0.975)
  )

# Plot with uncertainty bands
ggplot(mc_summary, aes(x = time)) +
  geom_ribbon(aes(ymin = I_lower, ymax = I_upper), fill = "lightblue", alpha = 0.5) +
  geom_line(aes(y = I_median), color = "blue", linewidth = 1.5) +
  labs(title = "Symptomatic Infections with Parameter Uncertainty",
       subtitle = "Shaded area = 95% confidence interval from 100 simulations",
       x = "Time (days)", 
       y = "Symptomatic Infections") +
  theme_minimal()

# Summary statistics with uncertainty
mc_outcomes <- mc_results %>%
  group_by(Simulation) %>%
  summarize(
    Peak_I = max(I),
    Peak_H = max(H),
    Total_D = max(D)
  )

cat("Outcome Uncertainty (95% CI):\n")
cat("Peak Infections:", 
    round(quantile(mc_outcomes$Peak_I, c(0.025, 0.5, 0.975))), "\n")
cat("Peak Hospitalizations:", 
    round(quantile(mc_outcomes$Peak_H, c(0.025, 0.5, 0.975))), "\n")
cat("Total Deaths:", 
    round(quantile(mc_outcomes$Total_D, c(0.025, 0.5, 0.975))), "\n")

##WHOLE MODEL
# Comprehensive Monte Carlo Simulation with Multiple Visualizations
set.seed(123)
n_simulations <- 100

# Run model multiple times with parameter uncertainty
mc_results <- data.frame()

for(sim in 1:n_simulations) {
  parms_mc <- parms
  
  # Add ±20% uncertainty to key parameters
  parms_mc["beta"]   <- parms["beta"] * runif(1, 0.8, 1.2)
  parms_mc["theta"]  <- parms["theta"] * runif(1, 0.8, 1.2)
  parms_mc["rho"]    <- pmax(0, pmin(1, parms["rho"] * runif(1, 0.8, 1.2)))
  parms_mc["eta"]    <- parms["eta"] * runif(1, 0.8, 1.2)
  parms_mc["kappa"]  <- parms["kappa"] * runif(1, 0.8, 1.2)
  parms_mc["deltaH"] <- parms["deltaH"] * runif(1, 0.8, 1.2)
  parms_mc["deltaC"] <- parms["deltaC"] * runif(1, 0.8, 1.2)
  parms_mc["rH"]     <- parms["rH"] * runif(1, 0.8, 1.2)
  parms_mc["rC"]     <- parms["rC"] * runif(1, 0.8, 1.2)
  
  out_mc <- as.data.frame(ode(startconds, times_1yr, seaihcrdmod, parms_mc))
  out_mc$Simulation <- sim
  
  mc_results <- rbind(mc_results, out_mc)
}

# Scale to population
mc_results[, 2:9] <- mc_results[, 2:9] * pop_size

# Create derived metrics
mc_results <- mc_results %>%
  mutate(
    Total_Infections = A + I,
    Healthcare_Burden = H + C,
    Health_Outcomes = I + H + C
  )

# Calculate confidence intervals for all metrics
mc_summary <- mc_results %>%
  group_by(time) %>%
  summarize(
    # Symptomatic infections
    I_median = median(I), I_lower = quantile(I, 0.025), I_upper = quantile(I, 0.975),
    # Total infections (A + I)
    TotalInf_median = median(Total_Infections), 
    TotalInf_lower = quantile(Total_Infections, 0.025), 
    TotalInf_upper = quantile(Total_Infections, 0.975),
    # Healthcare burden (H + C)
    HC_median = median(Healthcare_Burden), 
    HC_lower = quantile(Healthcare_Burden, 0.025), 
    HC_upper = quantile(Healthcare_Burden, 0.975),
    # Individual compartments
    S_median = median(S), S_lower = quantile(S, 0.025), S_upper = quantile(S, 0.975),
    E_median = median(E), E_lower = quantile(E, 0.025), E_upper = quantile(E, 0.975),
    A_median = median(A), A_lower = quantile(A, 0.025), A_upper = quantile(A, 0.975),
    H_median = median(H), H_lower = quantile(H, 0.025), H_upper = quantile(H, 0.975),
    C_median = median(C), C_lower = quantile(C, 0.025), C_upper = quantile(C, 0.975),
    R_median = median(R), R_lower = quantile(R, 0.025), R_upper = quantile(R, 0.975),
    D_median = median(D), D_lower = quantile(D, 0.025), D_upper = quantile(D, 0.975)
  )

# 1. Plot: Symptomatic Infections Only
p1 <- ggplot(mc_summary, aes(x = time)) +
  geom_ribbon(aes(ymin = I_lower, ymax = I_upper), fill = "lightblue", alpha = 0.5) +
  geom_line(aes(y = I_median), color = "blue", linewidth = 1.2) +
  labs(title = "Symptomatic Infections with Parameter Uncertainty",
       subtitle = "95% confidence interval from 100 simulations",
       x = "Time (days)", y = "Symptomatic Infections (I)") +
  theme_minimal()

# 2. Plot: All Infections (Symptomatic + Asymptomatic)
p2 <- ggplot(mc_summary, aes(x = time)) +
  geom_ribbon(aes(ymin = TotalInf_lower, ymax = TotalInf_upper), 
              fill = "orange", alpha = 0.4) +
  geom_line(aes(y = TotalInf_median, color = "Total (A+I)"), linewidth = 1.2) +
  geom_ribbon(aes(ymin = I_lower, ymax = I_upper), fill = "red", alpha = 0.3) +
  geom_line(aes(y = I_median, color = "Symptomatic (I)"), linewidth = 1) +
  geom_ribbon(aes(ymin = A_lower, ymax = A_upper), fill = "yellow", alpha = 0.3) +
  geom_line(aes(y = A_median, color = "Asymptomatic (A)"), linewidth = 1) +
  scale_color_manual(values = c("Total (A+I)" = "darkorange", 
                                "Symptomatic (I)" = "red", 
                                "Asymptomatic (A)" = "gold")) +
  labs(title = "All Infections: Symptomatic vs Asymptomatic",
       subtitle = "Shaded areas = 95% CI",
       x = "Time (days)", y = "Number of Infections", color = "Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

# 3. Plot: Healthcare Burden (Hospital + ICU)
p3 <- ggplot(mc_summary, aes(x = time)) +
  geom_ribbon(aes(ymin = HC_lower, ymax = HC_upper), 
              fill = "purple", alpha = 0.4) +
  geom_line(aes(y = HC_median, color = "Total (H+C)"), linewidth = 1.2) +
  geom_ribbon(aes(ymin = H_lower, ymax = H_upper), fill = "blue", alpha = 0.3) +
  geom_line(aes(y = H_median, color = "Hospital (H)"), linewidth = 1) +
  geom_ribbon(aes(ymin = C_lower, ymax = C_upper), fill = "darkred", alpha = 0.3) +
  geom_line(aes(y = C_median, color = "ICU (C)"), linewidth = 1) +
  scale_color_manual(values = c("Total (H+C)" = "purple", 
                                "Hospital (H)" = "blue", 
                                "ICU (C)" = "darkred")) +
  labs(title = "Healthcare Burden: Hospitalizations & ICU",
       subtitle = "Shaded areas = 95% CI",
       x = "Time (days)", y = "Number of Patients", color = "Facility") +
  theme_minimal() +
  theme(legend.position = "bottom")

# 4. Plot: Health Outcomes (I, H, C, D)
mc_summary_long <- mc_summary %>%
  select(time, I_median, I_lower, I_upper, 
         H_median, H_lower, H_upper,
         C_median, C_lower, C_upper,
         D_median, D_lower, D_upper) %>%
  pivot_longer(cols = -time, 
               names_to = c("Compartment", ".value"),
               names_pattern = "(.*)_(.*)")

p4 <- ggplot(mc_summary_long %>% filter(Compartment %in% c("I", "H", "C", "D")), 
             aes(x = time, color = Compartment, fill = Compartment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(aes(y = median), linewidth = 1) +
  scale_color_manual(values = c("I" = "orange", "H" = "blue", 
                                "C" = "darkred", "D" = "black"),
                     labels = c("Symptomatic", "Hospital", "ICU", "Deaths")) +
  scale_fill_manual(values = c("I" = "orange", "H" = "blue", 
                               "C" = "darkred", "D" = "black"),
                    labels = c("Symptomatic", "Hospital", "ICU", "Deaths")) +
  labs(title = "Health Outcomes: Infections, Hospitalizations, ICU, Deaths",
       subtitle = "Shaded areas = 95% CI",
       x = "Time (days)", y = "Count", color = "Outcome", fill = "Outcome") +
  theme_minimal() +
  theme(legend.position = "bottom")

# 5. Plot: All Compartments (SEAIHCRD)
p5 <- ggplot(mc_summary_long, aes(x = time, color = Compartment, fill = Compartment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(aes(y = median), linewidth = 0.8) +
  scale_color_brewer(palette = "Set1",
                     labels = c("Susceptible", "Exposed", "Asymptomatic", 
                                "Symptomatic", "Hospital", "ICU", "Recovered", "Deaths")) +
  scale_fill_brewer(palette = "Set1",
                    labels = c("Susceptible", "Exposed", "Asymptomatic", 
                               "Symptomatic", "Hospital", "ICU", "Recovered", "Deaths")) +
  labs(title = "All Compartments (SEAIHCRD Model)",
       subtitle = "Shaded areas = 95% CI",
       x = "Time (days)", y = "Population Count", 
       color = "Compartment", fill = "Compartment") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Display plots
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)

# Summary statistics with uncertainty
mc_outcomes <- mc_results %>%
  group_by(Simulation) %>%
  summarize(
    Peak_I = max(I),
    Peak_TotalInf = max(Total_Infections),
    Peak_H = max(H),
    Peak_C = max(C),
    Peak_HC = max(Healthcare_Burden),
    Total_D = max(D),
    Attack_Rate = (max(R) + max(D)) / pop_size * 100
  )

# Print comprehensive summary
cat("\n=== OUTCOME UNCERTAINTY (95% CI) ===\n\n")

cat("INFECTIONS:\n")
cat("Peak Symptomatic (I):", 
    sprintf("%s [%s - %s]",
            format(round(median(mc_outcomes$Peak_I)), big.mark=","),
            format(round(quantile(mc_outcomes$Peak_I, 0.025)), big.mark=","),
            format(round(quantile(mc_outcomes$Peak_I, 0.975)), big.mark=",")), "\n")
cat("Peak Total Infections (A+I):", 
    sprintf("%s [%s - %s]",
            format(round(median(mc_outcomes$Peak_TotalInf)), big.mark=","),
            format(round(quantile(mc_outcomes$Peak_TotalInf, 0.025)), big.mark=","),
            format(round(quantile(mc_outcomes$Peak_TotalInf, 0.975)), big.mark=",")), "\n\n")

cat("HEALTHCARE BURDEN:\n")
cat("Peak Hospitalizations (H):", 
    sprintf("%s [%s - %s]",
            format(round(median(mc_outcomes$Peak_H)), big.mark=","),
            format(round(quantile(mc_outcomes$Peak_H, 0.025)), big.mark=","),
            format(round(quantile(mc_outcomes$Peak_H, 0.975)), big.mark=",")), "\n")
cat("Peak ICU (C):", 
    sprintf("%s [%s - %s]",
            format(round(median(mc_outcomes$Peak_C)), big.mark=","),
            format(round(quantile(mc_outcomes$Peak_C, 0.025)), big.mark=","),
            format(round(quantile(mc_outcomes$Peak_C, 0.975)), big.mark=",")), "\n")
cat("Peak Total Healthcare (H+C):", 
    sprintf("%s [%s - %s]",
            format(round(median(mc_outcomes$Peak_HC)), big.mark=","),
            format(round(quantile(mc_outcomes$Peak_HC, 0.025)), big.mark=","),
            format(round(quantile(mc_outcomes$Peak_HC, 0.975)), big.mark=",")), "\n\n")

cat("MORTALITY:\n")
cat("Total Deaths (D):", 
    sprintf("%s [%s - %s]",
            format(round(median(mc_outcomes$Total_D)), big.mark=","),
            format(round(quantile(mc_outcomes$Total_D, 0.025)), big.mark=","),
            format(round(quantile(mc_outcomes$Total_D, 0.975)), big.mark=",")), "\n\n")

cat("ATTACK RATE:\n")
cat("Final Attack Rate (%):", 
    sprintf("%.2f%% [%.2f%% - %.2f%%]",
            median(mc_outcomes$Attack_Rate),
            quantile(mc_outcomes$Attack_Rate, 0.025),
            quantile(mc_outcomes$Attack_Rate, 0.975)), "\n")