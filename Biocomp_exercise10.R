#Function to model the growth of the two populations
#Ms is the population vector of the mutatant cells
#Ns is the population vector of the normal cells
#K is the carrying capacity
#rN is the growth rate of the normal cells, and rM is the growth rate of the mutant cells
#rN2 is the growth rate of the normal cells once the drug takes affect, and rM2 is the growth rate of the mutant cells once the drug takes affect
#timesteps is the number of cell divions 
#Drug takes affect at t=100
#Determine when N[t]=100
  time=100
  N0=1
  K=1000000
  rN=0.1
    for(t in 1:(timesteps-1)){
      Ns[t+1]<-Ns[t]+(rN*Ns[t]*(1-(Ns[t]/K)))
    }
  Ns
  Ns[49]
  Ns[50]
  #Ns reaches 100 between 49 and 50, so the mutation will occur at t=49
TumorPop<-function(No=1, Mo=1, timesteps=750, K=1000000, rN=0.1, rM=0.1, rN2=-0.1, rM2=0.05){
  Ns<-numeric(timesteps)
  Ns[1]<-No
  Ms<-numeric(timesteps)
  Ms[49]<-Mo
  rN2=-0.1
  rM2=0.05
  for(t in 1:(timesteps-1)){
    if(t<49){
      rN=rN
      Ns[t+1]<-Ns[t]+(rN*Ns[t]*(1-(Ns[t]/K)))
    }else{
      rN=rN
      rM=rM
      Ns[t+1]<-Ns[t]+(rN*Ns[t]*(1-((Ns[t]+Ms[t])/K)))
      Ms[t+1]<-Ms[t]+(rM*Ms[t]*(1-((Ms[t]+Ns[t])/K)))
    }
    if(t>=100){
      rN=rN2
      rM=rM2
      Ns[t+1]<-Ns[t]+(rN*Ns[t]*(1-((Ns[t]+Ms[t])/K)))
      Ms[t+1]<-Ms[t]+(rM*Ms[t]*(1-((Ms[t]+Ns[t])/K)))
    }
  }
  NormalPop<<-as.numeric(Ns)
  MutantPop<<-as.numeric(Ms)
}

TumorPop()


Population<-data.frame(time=1:750)
Population$NormalPopulation<-NormalPop
Population$MutantPopulation<-MutantPop

#Graph Data
#Load ggplot
library(ggplot2)
#Create Plot
ggplot(data=Population)+
  geom_line(aes(x=time,y=NormalPopulation), col="blue")+
  geom_line(aes(x=time,y=MutationPopulation), col="red")
