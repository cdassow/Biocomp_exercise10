# Exercise 10
# Cancer drug resistance growth model - Dynamic Modeling

# load packages
library(ggplot2)

#  variables
#starting point
N0=50
M0=1
# parameters: growth rate and limitations
rN=rM=0.1
K=1000000
timesteps = 500
treatment = F

# create vector to store N's and M's
Ns=numeric(length=timesteps)
Ns[1]=N0
Ms=numeric(length=timesteps)
Ms[100]=M0

#for loop
for(t in 1:timesteps-1){
  if (treatment == F){
    if(Ns[t]<100){
      #normal population
      Ns[t+1]=Ns[t]+(rN*Ns[t]*((1-(Ns[t]+Ms[t])/K)))
    }else if(Ns[t]>=100){
      #normal population
      Ns[t+1] = Ns[t]+(rN*Ns[t]*((1-(Ns[t]+Ms[t])/K)))
      #mutant population
      Ms[t+1] = Ms[t]+(rM*Ms[t]*((1-(Ns[t]+Ms[t])/K)))
    }
  }else if (treatment == T){
    rM=rN*.5
    rN=-0.1
    if(Ns[t]<100){
      #normal population
      Ns[t+1] = Ns[t]+(rN*Ns[t]*(1-((Ns[t]+Ms[t])/K)))
    }else if(Ns[t]>=100){
      #normal population
      Ns[t+1] = Ns[t]+(rN*Ns[t]*(1-((Ns[t]+Ms[t])/K)))
      #mutant population
      Ms[t+1] = Ms[t]+(rM*Ms[t]*(1-((Ns[t]+Ms[t])/K)))
    }
  }
}

# plot simulation
simEvents<-data.frame(time=1:length(Ns),N=Ns,M=Ms)
ggplot(data=simEvents)+
  geom_line(aes(x=time,y=N),col='black')+
  geom_line(aes(x=time,y=M),col='red')+
  theme_classic()

