# Exercise 10
# Cancer drug resistance growth model - Dynamic Modeling

# load packages
library(ggplot2)

#  variables
#starting point
N0=99
M0=1
# parameters: growth rate and limitations
rN = rM = 0.1
rN_drug = -0.1
rM_drug = 0.5
K=1000000
timesteps = 1000
eQ=285

# create vector to store N's and M's
Ns=numeric(length=timesteps)
Ns[1]=N0
Ms=numeric(length=timesteps)
Ms[1]=M0

#for loop
for(t in 1:(timesteps-1)){
  if (t<=eQ){
      #normal population
      Ns[t+1] = Ns[t]+(rN*Ns[t]*((1-(Ns[t]+Ms[t])/K)))
      #mutant population
      Ms[t+1] = Ms[t]+(rM*Ms[t]*((1-(Ns[t]+Ms[t])/K)))
  } else {
      #normal population
      Ns[t+1] = Ns[t]+(rN_drug*Ns[t]*(1-((Ns[t]+Ms[t])/K)))
      #mutant population
      Ms[t+1] = Ms[t]+(rM_drug*Ms[t]*(1-((Ns[t]+Ms[t])/K)))
  }
}

# plot simulation
simEvents<-data.frame(time=1:length(Ns),N=Ns,M=Ms)
ggplot(data=simEvents)+
  geom_line(aes(x=time,y=N,colour="NonMutant"))+
  geom_line(aes(x=time,y=M,colour="Mutant"))+
  theme_classic()+
  theme(legend.position="top")+
  scale_colour_manual(values=c("NonMutant"="black", "Mutant"="red"))+
  xlab("Time (days)")+
  ylab("Number of Cells")

