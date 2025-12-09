
# Load or define the nextgenR0 function
nextgenR0 <- function(Istates, Flist, Vlist, parameters, dfe){
  paras = as.list(c(dfe, parameters)) 
  
  k=0
  vl=fl=list(NULL)
  for(i in 1:length(Istates)){
    assign(paste("f", i, sep = "."), lapply(lapply(Flist, deriv, Istates[i]), eval, paras))
    assign(paste("v", i, sep = "."), lapply(lapply(Vlist, deriv, Istates[i]), eval, paras))
    for(j in 1:length(Istates)){
      k=k+1
      fl[[k]]=attr(eval(as.name(paste("f", i, sep=".")))[[j]], "gradient")[1,]
      vl[[k]]=attr(eval(as.name(paste("v", i, sep=".")))[[j]], "gradient")[1,] 
    } 
  }  
  f=matrix(as.numeric(as.matrix(fl)[,1]), ncol=length(Istates)) 
  v=matrix(as.numeric(as.matrix(vl)[,1]), ncol=length(Istates)) 
  R0=max(eigen(f%*%solve(v))$values) 
  return(R0) 
}

# Define parameters
parameters <- c(
  beta = 0.75,
  kappa = 0.497,      # Relative infectiousness of asymptomatics
  theta = 0.167,
  rho = 0.7,
  gamma_A = 0.143,    # Recovery rate from A
  gamma_I = 0.143,    # Recovery rate from I
  gamma_H = 0.196,    # Recovery rate from H
  gamma_C = 0.657,    # Recovery rate from C
  eta = 0.0686,       # Symptomatic -> hospitalized rate
  k = 0.102,          # Hospitalized -> ICU rate
  delta_H = 0.209,    # Death rate in hospital
  delta_C = 0.343,    # Death rate in ICU
  omega = 0.000095,    # Waning immunity rate
  mu = 1/70/365,      # Natural death rate
  N = 1               # Total population at DFE
)

# Define disease-free equilibrium
# At DFE: S = N, all other compartments = 0
dfe <- c(
  S = 1,
  E = 0,
  A = 0,
  I = 0,
  H = 0,
  C = 0,
  R = 0,
  D = 0
)

# Define infected states (only E, A, I)
Istates <- c("E", "A", "I")

# Define F list (new infections entering each infected compartment)
# Only E receives new infections: S * beta * (I + kappa * A) / N
# A and I receive no NEW infections (only progressions from E)
Flist <- list(
  quote(S * beta * (I + kappa * A) / N),  # New infections into E --> COME BACK TO THIS
  quote(0),                                 # No new infections into A
  quote(0)                                  # No new infections into I
)

# Define V list (net transitions = losses (rates out) - gains (rates in), 
#excluding new infections)

Vlist <- list(
  quote(theta * E),                                    # V for E
  quote(gamma_A * A  - (1 - rho) * theta * E),         # V for A
  quote((gamma_I + eta) * I - rho * theta * E)            # V for I
)

# Calculate R0
R0 <- nextgenR0(Istates = Istates, 
                Flist = Flist, 
                Vlist = Vlist, 
                parameters = parameters, 
                dfe = dfe)

cat("R0 calculated using nextgenR0 function:", R0, "\n")

# OPTIONAL: verify with analytical formula
R0_analytical <- parameters["beta"] * (
  parameters["rho"] / (parameters["gamma_I"] + parameters["eta"] + parameters["mu"]) + 
    (1 - parameters["rho"]) * parameters["kappa"] / (parameters["gamma_A"] + parameters["mu"])
)

cat("R0 (Analytical formula):", R0_analytical, "\n")
cat("Difference:", abs(R0 - R0_analytical), "\n")
```