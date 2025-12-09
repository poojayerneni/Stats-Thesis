
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