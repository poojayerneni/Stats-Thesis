
require("deSolve")

#model creation for 100 years
seaihcrdsmod = function(t, startconds, parms) {
  
  # Compartments
  S = startconds[1]
  E = startconds[2]
  A = startconds[3]
  I = startconds[4]
  H = startconds[5]
  C = startconds[6]
  R = startconds[7]
  D = startconds[8]
  
  #parameters
  beta = parms["beta"]      # transmission rate
  alpha = parms["alpha"]    # relative infectiousness of asymptomatic
  theta = parms["theta"]    # rate exposed -> infectious
  rho = parms["rho"]        # fraction symptomatic
  rA = parms["rA"]          # recovery rate asymptomatic
  rI = parms["rI"]          # recovery rate symptomatic
  rH = parms["rH"]          # recovery rate hospitalized
  rC = parms["rC"]          # recovery rate ICU
  eta = parms["eta"]        # rate symptomatic -> hospitalized
  kappa = parms["kappa"]    # rate hospitalized -> ICU
  deltaH = parms["deltaH"]  # death rate hospitalized
  deltaC = parms["deltaC"]  # death rate ICU
  omega  = parms["omega"]   # waning immunity rate
  mu = parms["mu"]          # natural death rate
  N = S + E + A + I + H + C + R # total population for force of infection
  
  
  Gamma = beta * (I + alpha * A) / N
  
  
  dS = - S * Gamma  + omega* R #- mu * S
  dE = S * Gamma - theta * E #- mu * E
  dA = (1 - rho) * theta * E - rA * A #- mu * A
  dI = rho * theta * E - (eta + rI) * I  #+mu*I
  dH = eta * I - (kappa + rH + deltaH) * H #+ mu*H
  dC = kappa * H - (rC + deltaC) * C #+ mu*C
  dR = rA * A + rI * I + rH * H + rC * C  - omega* R #- mu * R
  dD = deltaH * H + deltaC * C #+ mu * (S + E + A + I + H + C + R) 
  
  
  res = c(dS, dE, dA, dI, H = dH, C = dC, dR, dD)
  list(res)
}


times = seq(0, 100, by = 1/120)
parms <- c(
  beta = 0.75, 
  alpha = 0.497,
  theta = 0.167,         
  rho   = 0.7, 
  rA    = 0.143,         
  rI    = 0.143,    
  eta   = 0.0686, 
  kappa = 0.102, 
  deltaH= 0.209,
  deltaC= 0.343,
  rH    = 0.196, 
  rC    = 0.657,
  omega = 0.00093,
  mu    = 1/70     
)

startconds = c(S = 0.999, E = 0.001, A = 0, I = 0, 
               H = 0, C = 0, R = 0, D = 0)


out = as.data.frame(ode(startconds, times, seaihcrdsmod, parms))
head(out)

#simulate for 1 year in proportions
times = seq(0, 365, by = 1/120)

out_year = as.data.frame(ode(startconds, times, seaihcrdsmod, parms))
head(out_year)

#simulate for 1 year with population of 100,000

# Population size
pop_size <- 100000
out_year_new = as.data.frame(ode(startconds, times, seaihcrdsmod, parms))
head(out_year)
out_scaled <- out_year_new
out_scaled[, -1] <- out_year_new[, -1] * pop_size
head(out_scaled)
