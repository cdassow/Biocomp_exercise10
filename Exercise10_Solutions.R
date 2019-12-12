# set initial values and parameters
N0=99
M0=1
K=1000000
rN=0.1
rM=0.1
rN2=-0.1
rM2=0.05
timesteps=500

# create vector to store N's
Ns=numeric(length=timesteps)
Ns[1]=N0
Ms=numeric(length=timesteps)
Ms[1]=M0

# simulate
for (t in 1:(timesteps-1)){
  if(t<=200){
    Ns[t+1]<-Ns[t]+rN*Ns[t]*(1-((Ns[t]+Ms[t])/K))
    Ms[t+1]<-Ms[t]+rM*Ms[t]*(1-((Ns[t]+Ms[t])/K))
    }else{
    Ns[t+1]<-Ns[t]+rN2*Ns[t]*(1-((Ns[t]+Ms[t])/K))
    Ms[t+1]<-Ms[t]+rM2*Ms[t]*(1-((Ns[t]+Ms[t])/K))
  }
}

# plot simulation
library(ggplot2)

pop<-data.frame(time=1:(timesteps),N=Ns, M=Ms)
ggplot(data=pop)+geom_line(aes(x=time,y=N,color="Non Mutants"))+geom_line(aes(x=time,y=M,color="Mutants"))+
  theme_classic()+labs(x="Time",y="# of Cells",color="Cell Type")


