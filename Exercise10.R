#clear environment, load library
rm(list = ls())
library(ggplot2)

# set initial values and parameters
K=1000000
N0=99
M0=1
timesteps=500

#create vectors to store populations (N and M)
# where N is the non-mutant sub-population of the tumor and M is the mutant population
Ns <- numeric(length = timesteps)
Ns[1] <- N0
Ms <- numeric(length = timesteps)
Ms[1] <- M0

#simulate
for (t in 1:(timesteps-1)){
  if(t<200){
    r=.1
  Ns[t+1] = Ns[t] + r*Ns[t]*(1-(Ns[t]+Ms[t])/K)
  Ms[t+1] = Ms[t] + r*Ms[t]*(1-(Ns[t]+Ms[t])/K)
  }else{
  Rm=.05
  Rn=-0.1
  Ns[t+1] = Ns[t] + Rn*Ns[t]*(1-(Ns[t]+Ms[t])/K)
  Ms[t+1] = Ms[t] + Rm*Ms[t]*(1-(Ns[t]+Ms[t])/K)
}}

#plot
tumorpop <- data.frame(time=1:length(Ns), N=Ns, M=Ms)
ggplot(data = tumorpop, aes(x=time)) +
  geom_line(aes(y=N, col="blue")) +
  geom_line(aes(y=M, col="red")) +
  ylab("Population") +
  labs(col = "Cell type")  +
  scale_color_manual(labels = c("Non-mutant", "Mutant"), values = c("blue", "red"))


                                                