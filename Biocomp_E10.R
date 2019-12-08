# Biocomputing Exercise 10

install.packages("ggplot2")

library("ggplot2")

rm=0.1
rn=0.1
k=1000000
N0=1
timesteps=250
M0=100

Ns=numeric(length=timesteps)
Ns[1]=N0

Ms=numeric(length=timesteps)
Ms[1]=M0

for(t in 1:(timesteps-1)){
  Ns[t+1]<-Ns[t]+rn*Ns[t]*(1-((Ns[t]+Ms[t])/k))
}

for(t in 1:(timesteps-1)){
  Ms[t+1]<-Ms[t]+rm*Ms[t]*(1-((Ns[t]+Ms[t])/k))
}

challenge<-data.frame(time=1:250)
ggplot(data=challenge)+geom_line(aes(x=time,y=0.5*Ms))+
  geom_line(aes(x=time,y=Ms))+geom_line(aes(x=time,y=Ns))+
  geom_line(aes(x=time,y=-0.1))

