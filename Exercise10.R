# Exercise 10
# Cancer drug resistance growth model - Dynamic Modeling

# load packages
library(ggplot2)

#  variables
#starting point
N0=3
M0=5
# parameters: growth rate and limitations
rN=0.1
rM=0.1
K=100
timesteps = 100
treatment = T

# create vector to store N's
Ns=numeric(length=timesteps)
Ns[1]=N0
Ms=numeric(length=timesteps)
Ms[1]=M0

#for loop
for(t in 1:timesteps){
  if (treatment = T){
    
  }
}

#normal population
N[t+1] = N[t]+rN*N[t](1-(N[t]+M[t]/K))

#mutant population
M[t+1] = M[t]+rM*M[t](1-(N[t]+M[t]/K))