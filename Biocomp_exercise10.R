#Mutation doesn't affect growth rate when cancer drug is absent
#When drug is present, mutant population grows at 50% of regular growth rate
#When drug is present, regular population declines

#Regular population equation
#Nt+1=Nt + rN*Nt*(1-(Nt+Mt)K)

#Mutant Population equation
#Mt+1=Mt + rM*Mt*(1-(Nt+Mt)/K)

#Under normal conditions:
  #rN=rM=0.1
  #K=1,000,000

#Mutation occurred at Nt=100
#rN = -0.1

#Function to model the growth of the two populations
#Ms is the population vector of the mutatant cells
#Ns is the population vector of the normal cells
#K is the carrying capacity
#rN is the growth rate of the normal cells, and rM is the growth rate of the mutant cells
#timesteps is the number of cell divions 
#At a population size of 100 cells, the drug takes affect 
TumorPop<-function(N0=1, M0=1, K=1000000, timesteps=250, rN=0.1, rM=0.1){
  Ns<-numeric(timesteps)
  Ms<-numeric(timesteps)
  Ns[1]<-N0
  Ms[1]<-M0
  for(t in 1:(timesteps-1)){
    if(Ns[t]<100){
      Ns[t+1]<-Ns[t]+rN*Ns[t]*(1-((Ns[t]+Ms[t])/K))
      Ms[t+1]<-Ms[t]+rM*Ms[t]*(1-((Ms[t]+Ns[t])/K))
    }else{
      rN=-rN
      rM=0.5*rM
      Ns[t+1]<-Ns[t]+rN*Ns[t]*(1-((Ns[t]+Ms[t])/K))
      Ms[t+1]<-Ms[t]+rM*Ms[t]*(1-((Ms[t]+Ns[t])/K))
    }
  }
}
#Run the function
TumorPop()

