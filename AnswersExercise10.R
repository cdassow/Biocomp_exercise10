
#Exercise 10 answers
#Setting constants for the simulation
N0=99
M0=1
rn=0.1
rn2=-.1
rm2=.05
rm=0.1
K=1000000
timesteps=500

Ns<-numeric(length = timesteps)
Ms<-numeric(length = timesteps)
Ns[1]=N0
Ms[1]=M0

library(ggplot2)

#simulation for the nonmutant
for(t in 1:(timesteps-1)){
  Ns[t+1]<-Ns[t]+rn*Ns[t]*(1-(Ns[t]+Ms[t])/K)
}

#simlulation for mutant
for(t in 1:(timesteps-1)){
  Ms[t+1]<-Ns[t]+rm*Ms[t]*(1-(Ns[t]+Ms[t])/K)
}

#Plotting
Nonmut<- data.frame(time=1:timesteps,N=Ns)
ggplot(data=Nonmut, aes(x=time, y=N)) +geom_line() +theme_classic()

Mut<- data.frame(time=1:timesteps,M=Ms)
ggplot(data=Mut, aes(x=time, y=M)) +geom_line() +theme_classic()

#FULL SIMULATION
#This is to simulate nonmutants and mutants including the drug information
#final plot includes nonmutant and mutant
for(t in 1:(timesteps-1)){
  if(t<=200){
      Ns[t+1]<-Ns[t]+rn*Ns[t]*(1-((Ns[t]+Ms[t])/K))
      Ms[t+1]<-Ms[t]+rm*Ms[t]*(1-((Ns[t]+Ms[t])/K))
  }else{
    Ns[t+1]<-Ns[t]+rn2*Ns[t]*(1-((Ns[t]+Ms[t])/K))
    Ms[t+1]<-Ms[t]+rm2*Ms[t]*(1-((Ns[t]+Ms[t])/K))
  }
}
Nonmut<- data.frame(time=1:timesteps,N=Ns, M=Ms)

ggplot(data=Nonmut) +
  geom_line(aes(x=time, y=N, color='Nonmutant')) +
  geom_line(aes(x=time, y=M, color='Mutant'))+
  labs(x="Time", y="Cell Abundance", color="Cell Subpopulation") +
  theme_classic()






