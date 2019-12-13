#Exercise 10
#starting info
N0=99
M0=1
r=0.1 #usual growth rate
rm=0.05 #mutant growth rate w/ treatment
rn=-0.1 #normal growth rate w/ treatment
K=1000000
timesteps=750
#vector creation
Ns=numeric(length=timesteps)
Ns[1]=N0
Ms=numeric(length=timesteps)
Ms[1]=M0
#previous simulation established that equilibrium is reached at t=258 with normal growth rates
#set up simulation with these paramaters and treatment starting at equilibrium 
for(t in 1:(timesteps-1)){
  if(t<=258){
  Ns[t+1]=Ns[t]+r*Ns[t]*(1-(Ns[t]+Ms[t])/K)  
  Ms[t+1]=Ms[t]+r*Ms[t]*(1-(Ns[t]+Ms[t])/K)
  }else{
    Ns[t+1]=Ns[t]+rn*Ns[t]*(1-(Ns[t]+Ms[t])/K)  
    Ms[t+1]=Ms[t]+rm*Ms[t]*(1-(Ns[t]+Ms[t])/K)
  }
}
#plotting data
library(ggplot2)
sim=data.frame(time=1:length(Ns), N=Ns, M=Ms)
ggplot(data=sim)+
  geom_line(aes(x=time, y=M), col='blue')+
  geom_line(aes(x=time, y=N), col='green')+
  labs(x="Time", y="Cell Count")+theme_classic()
