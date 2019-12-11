# Biocomputing Exercise 10

install.packages("ggplot2") 
# mine needed to be reinstalled, not sure why

library("ggplot2")

# parameters

rm=0.1
rn=0.1
k=1000000
N0=99
timesteps=600
M0=1
rmdrug<- 0.05
rndrug<- -0.1

Ns=numeric(length=timesteps)
Ns[1]=N0

Ms=numeric(length=timesteps)
Ms[1]=M0

# if else statement in order to show sub populations before 
# drug treatment and afterwards

for(t in 1:(timesteps-1)){
  if (t<(timesteps/2)){
  Ns[t+1]<-Ns[t]+rn*Ns[t]*(1-((Ns[t]+Ms[t])/k))
  Ms[t+1]<-Ms[t]+rm*Ms[t]*(1-((Ns[t]+Ms[t])/k))
  }else{
    Ms[t+1]<-Ms[t]+rmdrug*Ms[t]*(1-((Ns[t]+Ms[t])/k))
    Ns[t+1]<-Ns[t]+rndrug*Ns[t]*(1-((Ns[t]+Ms[t])/k))
  }
}

# put for loop into dataframe file 

challenge<-data.frame(time=1:600)

# plotting mutant and nonmutant cell populations with 
# drug treatment

ggplot(data=challenge)+xlab('Time')+ylab('Cell Count')+
  geom_line(aes(x=time,y=Ms), col='red')+
  geom_line(aes(x=time,y=Ns), col='blue')

