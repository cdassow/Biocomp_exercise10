## Code for Tutorial 13 - More Dynamic Modeling
library(ggplot2)


### Challenge 3 from Lecture 26: Adding events - immigration of 50 individuals 50 years in,
###             and 90% becoming ill and dying at 150 years

# basic model run
N0=2
r=0.2
K=100
timesteps=250

# create vector to store N's and set initial N
Ns=numeric(length=timesteps)
Ns[1]=N0

# simulate
for(t in 1:(timesteps-1)){
  Ns[t+1] <- Ns[t]+r*Ns[t]*(1-Ns[t]/K)  
}

# model run with events
N0=2
r=0.2
K=100
timesteps=250

# create vector to store N's and set initial N
NsEvents=numeric(length=timesteps)
NsEvents[1]=N0

# simulate
for(t in 1:(timesteps-1)){
  if(t==50){
    # 50 additional new individuals due to immigration
    NsEvents[t+1] <- NsEvents[t]+r*NsEvents[t]*(1-NsEvents[t]/K)+50
  }else if(t==150){
    # 90% of individuals die due to illness
    NsEvents[t+1] <- NsEvents[t]*0.1
  }else{
    NsEvents[t+1] <- NsEvents[t]+r*NsEvents[t]*(1-NsEvents[t]/K)
  }
}

# plot simulation
simEvents<-data.frame(time=1:length(Ns),N=Ns,Nevents=NsEvents)
ggplot(data=simEvents)+
  geom_line(aes(x=time,y=N),col='black')+
  geom_line(aes(x=time,y=Nevents),col='red')+
  theme_classic()


### Lake pollution model

# parameters
QV=0.25
Cin=10
timesteps=50 # just arbitrarily picked, but needs to be long enough to let it get to equilibrium

# create vector to store C's and set initial C
Cs=numeric(length=timesteps)
Cs[1]=0   # assuming the lake was free of pollution when the factory was completed

# simulate
for(t in 1:(timesteps-1)){
  Cs[t+1] <- Cs[t]+QV*(Cin-Cs[t])  
}

# plot results
pollutant<-data.frame(time=1:timesteps,C=Cs)
ggplot(data=pollutant,aes(x=time,y=C))+geom_line()+theme_classic()

# the equilibrium concentration is ~10, but it is a limit so it never gets to 10...
# We'll figure out when it gets to 9.99
sum(Cs<9.99)   # 25 years

# what if the EPA shut it down. How long to get to a concentration of 1 (10% of equilibrium)

# parameters
QV=0.25
Cin=0
timesteps=50 # just arbitrarily picked, but needs to be long enough to let it get to equilibrium

# create vector to store C's and set initial C
Cs=numeric(length=timesteps)
Cs[1]=10   # assuming the lake is starting from a polluted state

# simulate
for(t in 1:(timesteps-1)){
  Cs[t+1] <- Cs[t]+QV*(Cin-Cs[t])  
}

# plot results
pollutant<-data.frame(time=1:timesteps,C=Cs)
ggplot(data=pollutant,aes(x=time,y=C))+geom_line()+theme_classic()

sum(Cs>1)  # 9 years



# what if bacteria degrade 10% per year?
# parameters
QV=0.25
Cin=10
d=0.1
timesteps=50 # just arbitrarily picked, but needs to be long enough to let it get to equilibrium

# create vector to store C's and set initial C
Cs=numeric(length=timesteps)
Cs[1]=0   # assuming the lake was free of pollution when the factory was completed
CsBacteria=Cs

# simulate
for(t in 1:(timesteps-1)){
  Cs[t+1] <- Cs[t]+QV*(Cin-Cs[t])  
  CsBacteria[t+1] <- CsBacteria[t]+QV*(Cin-CsBacteria[t])-d*CsBacteria[t]
}

# plot results
pollutant<-data.frame(time=1:timesteps,C=Cs,C_Bacteria=CsBacteria)
ggplot(data=pollutant)+geom_line(aes(x=time,y=C))+geom_line(aes(x=time,y=C_Bacteria),col="red")+theme_classic()
