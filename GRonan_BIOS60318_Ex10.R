#*******************************************************************************************
# Author: George Ronan
# Date Edited: 12 December, 2019
# Imagine a cancer cell in a tumor that spontaneously exhibited a mutation that confers drug
# resistance. The model we will use to represent the growth of the two sub-populations is:
# N_t = N_t + r_N*N_t*(1 ??? (N_t + M_t)/K )
# M_t = M_t + r_M*M_t*(1 ??? (N_t + M_t)/K )
# Generate a script that simulates growth of the two sub-populations in the tumor to
# equilibrium followed by drug treatment. Plot your results using a line graph.
#-Default Parameters------------------------------------------------------------------------
# Initial Growth Rate: r_N = r_M = 0.1
# Carrying Capacity: K = 1,000,000
# Post Treatment Growth Rate: r_N = -0.1; r_M = 0.05
# Mutation Occurance: N_t = 100 (N_t = 100 --> 99; M_t = 0 --> 1)
#-------------------------------------------------------------------------------------------
#*******************************************************************************************
# Initialize
#*******************************************************************************************
K <- 1000000 # Carrying capacity
rN_i <- 0.1; rM_i <- 0.1 # Initial growth rate constant values
rN_d <- -0.1; rM_d <- 0.5 # Growth rate constant values post-drug treatment
rN <- rN_i; rM <- rM_i # Set initial growth rate constants
Mut <- 100 # Cell number at which mutation occurs
timepoints <- 1000 # Duration of simulation
N = numeric(length = timepoints)
M = numeric(length = timepoints)
N[1] <- 1 # Initial number of 'normal' cancer cells
M[1] <- 0 # Initial number of 'mutant' cancer cells
# NOTE - Drug was introduced one the populations reached carrying capacity
#*******************************************************************************************
# Run Simulation
#*******************************************************************************************
for(i in 2:timepoints){
  if(N[i-1] < Mut && M[i-1] == 0){ # Only 'normal' exists
    N[i] = N[i-1] + rN * N[i-1] * (1-(N[i-1]+M[i-1])/K)
  }else if(N[i-1] >= Mut && M[i-1] == 0){ # Formation of first 'mutant' at N_pop >= Mut
    N[i] <- N[i-1] - 1 # Removal of initial mutated cancer cell from 'normal' population
    M[i] <- M[i-1] + 1 # Addition of mutated cancer cell to new 'mutant' population
  }else if(M[i-1] > 0){ # After 'mutant' population exists
    N[i] = N[i-1] + rN * N[i-1] * (1-(N[i-1]+M[i-1])/K)
    M[i] = M[i-1] + rM * M[i-1] * (1-(N[i-1]+M[i-1])/K)
  }
  # Simulate introduction of drug (once carrying capacity has been reached)
  if(round(N[i] + M[i]) == K){
    rN <- rN_d # Update rN_initial to rN_post-drug
    rM <- rM_d # Update rM_initial to rM_post-drug
  }
}
#*******************************************************************************************
# Plot Results
#*******************************************************************************************
library(ggplot2)
library(reshape2)
sim <- data.frame(time = 1:length(N),N = N,M = M) # Convert arrays to single dataframe
sim_p <- melt(data = sim,id = c("time")) # Melt dataframe for plotting
sim_plot <- ggplot(sim_p) + # Create base plot
  geom_line(aes(x = time,y = value,color = variable)) + # Plot melted data
  scale_color_manual(values = c("blue","red"), # Set line colors for each population
                     name = "Legend", # Name of legend
                     labels = c("Normal","Mutant")) + # Names of legend entries
  labs(title = "Normal versus Mutant Cancer Growth without/with Treatment",
       x = "Time",y = "Cell Count") + # Title and x/y labels
  theme_classic()
sim_plot
