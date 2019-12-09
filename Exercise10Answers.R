#Evolution of Drug Resistance in Tumor

rm(list=ls())

#Math
#N(t)=N(t)+rnN(t)*(1-(Nt+Mt)/K)
#M(t)=M(t)+rmM(t)*(1-(Nt+Mt)/K)

#initial conditions
K=1000000
M0=1
N0=99 #at early time (t=0), 1 mutant cell for total of 100 cells
timesteps=1000

#create vector to store M and N, and set initial values
Ns=numeric(length=timesteps)
Ns[1]=N0
Ms=numeric(length=timesteps)
Ms[1]=M0

#simulate
for (t in 1:(timesteps-1)){
  if(t>250){
    rn=-.1  #when equilibrium has been reached (by 250), drug treatment
    rm=.05  #is added
    Ns[t+1]=Ns[t]+rn*Ns[t]*(1-(Ns[t]+Ms[t])/K)
    Ms[t+1]=Ms[t]+rm*Ms[t]*(1-(Ns[t]+Ms[t])/K)
  }else{
    rn=.1
    rm=.1
    Ns[t+1]=Ns[t]+rn*Ns[t]*(1-(Ns[t]+Ms[t])/K)
    Ms[t+1]=Ms[t]+rm*Ms[t]*(1-(Ns[t]+Ms[t])/K)
  }
}

#plot simulation
library(ggplot2)
growth=data.frame(time=1:length(Ns), N=Ns, M=Ms)
ggplot(data=growth)+
  geom_line(aes(x=time, y=N), col='green')+
  geom_line(aes(x=time, y=M), col='red')+
  ylab("Number of Cells")+theme_classic()
