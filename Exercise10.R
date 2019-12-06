# Exercise 10
# Cancer drug resistance growth model - Dynamic Modeling

# load data and 

# time points to simulate over
# a place to stor population sizes (value of state variable through time)
# a loop for simulating

#  variables
#starting point
N0=3
# parameters
#growth rate
rN=0.1
rM=0.1
#limitation
K=100


#normal population
N[t+1] = N[t]+rN*N[t](1-(N[t]+M[t]/K))

#mutant population
M[t+1] = M[t]+rM*M[t](1-(N[t]+M[t]/K))