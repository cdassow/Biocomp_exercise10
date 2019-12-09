#tutorial 10

#set initial values and parameters
N0 <- 99
M0 <- 1
rN <- rM <- 0.1
K <- 1000000
timesteps <- 400
rMc <- 0.5*rM   #growth rate with drug
rNc <- -0.1     #growth rate with drug

#create vector to store Ns
Ns <- numeric(length=timesteps)
Ns[1] <- N0

#create vector to store Ms
Ms <- numeric(length=timesteps)
Ms[1] <- M0

#N(t) <- N(t) + rN*N(t)*(1-(N(t)+M(t))/K) overall equation for non-mutant sub-pop
#M(t) <- M(t) + rM*M(t)*(1-(N(t)+M(t))/K) overall equation for mutant sub-pop

for(t in 1:timesteps){
  Ns[t+1] <- Ns[t] + rN*Ns[t]*(1-(Ns[t]+Ms[t])/K)
  Ms[t+1] <- Ms[t] + rM*Ms[t]*(1-(Ns[t]+Ms[t])/K)
  if(t>186){     #at t=186, equilibrium is reached (numbers of cells get very close to their limits) 
    Ns[t+1] <- Ns[t] + rNc*Ns[t]*(1-(Ns[t]+Ms[t])/K)
    Ms[t+1] <- Ms[t] + rMc*Ms[t]*(1-(Ns[t]+Ms[t])/K)  
  }
}


#plot simulation
library(ggplot2)
df <- data.frame(time=1:(timesteps+1),NonMutant=Ns,Mutant=Ms)
ggplot(data=df)+
  geom_line(aes(x=time,y=NonMutant),col="red")+
  geom_line(aes(x=time,y=Mutant),col="blue")+
  theme_classic()+
  ylab("number of cells")+
  ggtitle("Mutant and nonmutant cell growth model")



