
# Inputs
hospital_capacity <- 262.94   # beds per 100k
icu_capacity      <- 29.4
hospital_cost_per_day <- 1772
icu_cost_per_day <- 2902

dt <- 1/120  # timestep used in your ode simulation
pop_size <- 100000

# function to compute metrics from out_scaled (data.frame with columns time,
#S,E,A,I,H,C,R,D)
hosp_metrics <- function(df, hosp_cap, icu_cap, dt) {
  H <- df$H
  C <- df$C
  hosp_overflow <- pmax(0, H - hosp_cap)
  icu_overflow  <- pmax(0, C - icu_cap)
  
  hosp_bed_days <- sum(H) * dt
  icu_bed_days  <- sum(C) * dt
  hosp_overflow_bed_days <- sum(hosp_overflow) * dt
  icu_overflow_bed_days  <- sum(icu_overflow) * dt
  
  hosp_days_above <- sum(hosp_overflow > 0) * dt
  icu_days_above  <- sum(icu_overflow > 0) * dt
  
  hosp_calendar_days <- length(unique(floor(df$time[hosp_overflow > 0])))
  icu_calendar_days  <- length(unique(floor(df$time[icu_overflow > 0])))
  
  t_peak_h <- df$time[which.max(H)]
  t_peak_c <- df$time[which.max(C)]
  total_deaths <- tail(df$D, 1)
  
  total_cost <- hosp_bed_days * hospital_cost_per_day + icu_bed_days * 
    icu_cost_per_day
  
  list(
    hosp_bed_days = hosp_bed_days,
    icu_bed_days = icu_bed_days,
    hosp_overflow_bed_days = hosp_overflow_bed_days,
    icu_overflow_bed_days = icu_overflow_bed_days,
    hosp_days_above = hosp_days_above,
    icu_days_above = icu_days_above,
    hosp_calendar_days = hosp_calendar_days,
    icu_calendar_days = icu_calendar_days,
    t_peak_h = t_peak_h,
    t_peak_c = t_peak_c,
    total_deaths = total_deaths,
    total_cost = total_cost
  )
}

# Baseline:
baseline_metrics <- hosp_metrics(out_scaled, hospital_capacity, icu_capacity, dt)
print(baseline_metrics)

# Beta reduced by 30%
parms_bpi <- parms
parms_bpi["beta"] <- parms["beta"] * 0.7
out_bpi <- as.data.frame(ode(startconds, times, seaihcrdsmod, parms_bpi))
out_bpi_scaled <- out_bpi
out_bpi_scaled[,-1] <- out_bpi[,-1] * pop_size
bpi_metrics <- hosp_metrics(out_bpi_scaled, hospital_capacity, icu_capacity, dt)
print(bpi_metrics)

# Vaccination (30% vaccinated at t=0) 
start_vax <- startconds
start_vax["S"] <- startconds["S"] * (1 - 0.3)
start_vax["R"] <- 0.3 * startconds["S"]  # vaccinate into R
out_vax <- as.data.frame(ode(start_vax, times, seaihcrdsmod, parms))
out_vax_scaled <- out_vax
out_vax_scaled[,-1] <- out_vax[,-1] * pop_size
vax_metrics <- hosp_metrics(out_vax_scaled, hospital_capacity, icu_capacity, dt)
print(vax_metrics)

# ICU capacity expansion (doubled) analysis (at baseline)
expanded_icu_metrics <- hosp_metrics(out_scaled, hospital_capacity, 
                                     icu_capacity*2, dt)
print(expanded_icu_metrics)

# incremental calculations (example) 
inc_cost_bpi <- bpi_metrics$total_cost - baseline_metrics$total_cost
deaths_averted_bpi <- baseline_metrics$total_deaths - bpi_metrics$total_deaths
cost_per_death_averted_bpi <- inc_cost_bpi / deaths_averted_bpi
print(list(inc_cost_bpi=inc_cost_bpi, deaths_averted_bpi=deaths_averted_bpi, 
           cost_per_death_averted_bpi=cost_per_death_averted_bpi))


library(gtsummary)
library(dplyr)
library(gt)

# Combine all metrics into a data frame
metrics_df <- data.frame(
  Metric = c(
    "Hospital bed-days",
    "ICU bed-days",
    "Hospital overflow bed-days",
    "ICU overflow bed-days",
    "Days above hospital capacity",
    "Days above ICU capacity",
    "Calendar days above hospital capacity",
    "Calendar days above ICU capacity",
    "Time to peak hospital demand (days)",
    "Time to peak ICU demand (days)",
    "Total deaths",
    "Total healthcare cost ($)"
  ),
  Baseline = c(
    baseline_metrics$hosp_bed_days,
    baseline_metrics$icu_bed_days,
    baseline_metrics$hosp_overflow_bed_days,
    baseline_metrics$icu_overflow_bed_days,
    baseline_metrics$hosp_days_above,
    baseline_metrics$icu_days_above,
    baseline_metrics$hosp_calendar_days,
    baseline_metrics$icu_calendar_days,
    baseline_metrics$t_peak_h,
    baseline_metrics$t_peak_c,
    baseline_metrics$total_deaths,
    baseline_metrics$total_cost
  ),
  BPI_30pct = c(
    bpi_metrics$hosp_bed_days,
    bpi_metrics$icu_bed_days,
    bpi_metrics$hosp_overflow_bed_days,
    bpi_metrics$icu_overflow_bed_days,
    bpi_metrics$hosp_days_above,
    bpi_metrics$icu_days_above,
    bpi_metrics$hosp_calendar_days,
    bpi_metrics$icu_calendar_days,
    bpi_metrics$t_peak_h,
    bpi_metrics$t_peak_c,
    bpi_metrics$total_deaths,
    bpi_metrics$total_cost
  ),
  Vaccination_30pct = c(
    vax_metrics$hosp_bed_days,
    vax_metrics$icu_bed_days,
    vax_metrics$hosp_overflow_bed_days,
    vax_metrics$icu_overflow_bed_days,
    vax_metrics$hosp_days_above,
    vax_metrics$icu_days_above,
    vax_metrics$hosp_calendar_days,
    vax_metrics$icu_calendar_days,
    vax_metrics$t_peak_h,
    vax_metrics$t_peak_c,
    vax_metrics$total_deaths,
    vax_metrics$total_cost
  ),
  ICU_Expansion = c(
    expanded_icu_metrics$hosp_bed_days,
    expanded_icu_metrics$icu_bed_days,
    expanded_icu_metrics$hosp_overflow_bed_days,
    expanded_icu_metrics$icu_overflow_bed_days,
    expanded_icu_metrics$hosp_days_above,
    expanded_icu_metrics$icu_days_above,
    expanded_icu_metrics$hosp_calendar_days,
    expanded_icu_metrics$icu_calendar_days,
    expanded_icu_metrics$t_peak_h,
    expanded_icu_metrics$t_peak_c,
    expanded_icu_metrics$total_deaths,
    expanded_icu_metrics$total_cost
  )
)

# Create the table with gtsummary/gt
table <- metrics_df %>%
  gt() %>%
  tab_header(
    title = "Healthcare System Impact Analysis",
    subtitle = "SEAIHCRDS Model Scenarios (N=100,000)"
  ) %>%
  cols_label(
    Metric = "Metric",
    Baseline = "Baseline",
    BPI_30pct = "BPI (-30% β)",
    Vaccination_30pct = "Vaccination (30%)",
    ICU_Expansion = "2x ICU Capacity"
  ) %>%
  fmt_number(
    columns = c(Baseline, BPI_30pct, Vaccination_30pct, ICU_Expansion),
    rows = 1:10,
    decimals = 1
  ) %>%
  fmt_number(
    columns = c(Baseline, BPI_30pct, Vaccination_30pct, ICU_Expansion),
    rows = 11,
    decimals = 0
  ) %>%
  fmt_currency(
    columns = c(Baseline, BPI_30pct, Vaccination_30pct, ICU_Expansion),
    rows = 12,
    decimals = 0
  ) %>%
  tab_style(
    style = cell_fill(color = "#f0f0f0"),
    locations = cells_body(rows = c(1, 3, 5, 7, 9, 11))
  ) %>%
  tab_footnote(
    footnote = paste0("Hospital capacity: ", hospital_capacity, " beds per 100k; ICU capacity: ", icu_capacity, " beds per 100k"),
    locations = cells_title()
  ) %>%
  tab_footnote(
    footnote = paste0("Hospital cost: $", hospital_cost_per_day, "/day; ICU cost: $", icu_cost_per_day, "/day"),
    locations = cells_body(rows = 12, columns = Metric)
  )

table

# Optional: Add a summary comparison table
comparison_df <- data.frame(
  Intervention = c("BPI (-30% β)", "Vaccination (30%)", "2x ICU Capacity"),
  Deaths_Averted = c(
    baseline_metrics$total_deaths - bpi_metrics$total_deaths,
    baseline_metrics$total_deaths - vax_metrics$total_deaths,
    baseline_metrics$total_deaths - expanded_icu_metrics$total_deaths
  ),
  Cost_Difference = c(
    bpi_metrics$total_cost - baseline_metrics$total_cost,
    vax_metrics$total_cost - baseline_metrics$total_cost,
    expanded_icu_metrics$total_cost - baseline_metrics$total_cost
  )
) %>%
  mutate(
    Cost_per_Death_Averted = ifelse(Deaths_Averted > 0, 
                                    Cost_Difference / Deaths_Averted, 
                                    NA)
  )

comparison_table <- comparison_df %>%
  gt() %>%
  tab_header(
    title = "Intervention Comparison",
    subtitle = "Relative to Baseline Scenario"
  ) %>%
  cols_label(
    Intervention = "Intervention",
    Deaths_Averted = "Deaths Averted",
    Cost_Difference = "Cost Difference ($)",
    Cost_per_Death_Averted = "Cost per Death Averted ($)"
  ) %>%
  fmt_number(
    columns = Deaths_Averted,
    decimals = 0
  ) %>%
  fmt_currency(
    columns = c(Cost_Difference, Cost_per_Death_Averted),
    decimals = 0
  ) %>%
  tab_style(
    style = cell_fill(color = "#e8f4f8"),
    locations = cells_body(
      columns = Deaths_Averted,
      rows = Deaths_Averted > 0
    )
  )

comparison_table