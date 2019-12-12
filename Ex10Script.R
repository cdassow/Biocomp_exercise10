##### Exercise 10 #####


#make a population model for a bunch of cells
N0= 50# normal cell initial pop
M0= 1 # mutant cell initial pop
rN= 0.1 # normal cell growth rate
rM= 0.1 # mutated cell growth rate
K=1000000 #carrying capacity

# timesteps 
timesteps=500
Ns=numeric(length(timesteps)) # to store normal cell
Ms=numeric(length(timesteps)) # to store mutated cell
Ns[1]=N0
Ms[1]=0

#sub pop 1-- normal cells
#Nt = Nt + rN*Nt*(1-((Nt+Mt)/K))

#sub pop 2-- mutated cells
 #Mt = Mt + rM*Mt*(1-((Nt+Mt)/K))

for(t in 1:(timesteps-1)){
  if(Ns[t]<100){
    # before we start adding mutated cells
    Ns[t+1] <- Ns[t]+rN*Ns[t]*(1-((Ns[t]+Ms[t])/K))
    Ms[t+1] <- Ms[t]+rM*Ms[t]*(1-((Ns[t]+Ms[t])/K))
  }else if(Ms[t]>1000){
    # Cancer drug starts when mutated cells exceed 1000
    Ns[t+1] <- Ns[t]+(-0.1)*Ns[t]*(1-((Ns[t]+Ms[t])/K))
    Ms[t+1] <- Ms[t]+(rM*0.5)*Ms[t]*(1-((Ns[t]+Ms[t])/K))
  }else{
    #mutated subpop begins
    Ms[t+1]<-ifelse(Ms[t-1]==0, M0, Ms[t]+rM*Ms[t]*(1-((Ns[t]+Ms[t])/K)))
    Ns[t+1]<-Ns[t]+rN*Ns[t]*(1-((Ns[t]+Ms[t])/K))
  }
}

sims<-data.frame(timesteps=1:500,Ns=Ns, Ms=Ms)
plot(sims$timesteps, sims$Ns, ylim=c(0, 1000000), xlab="Time step", ylab="Number of cells", pch=16)
points(sims$timesteps, sims$Ms, col="blue", pch=16)
legend(x=400, y=600000, legend=c("Normal Cells", "Mutated Cells"), col=c("black", "blue"), pch=16 )

# when cancer drug is present, new rM = rM*0.5
# mutation occurs when Nt = 100
# drug treatment of N cells, rN =-0.1