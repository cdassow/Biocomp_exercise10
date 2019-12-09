#tutorial 10

#set initial values and parameters
N0 <- 1
M0 <- 1
rN <- rM <- 0.1
K <- 1000000
timesteps <- 400
rNc <- 0.5*rN
rMc <- -0.1

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
  if(t>198){   #they hit 4.99 at timestep 198, and 5 is the real equilibrium, but as it's a limit, it's never exactly hit
    Ns[t+1] <- Ns[t] + rNc*Ns[t]*(1-(Ns[t]+Ms[t])/K)
    Ms[t+1] <- Ms[t] + rMc*Ms[t]*(1-(Ns[t]+Ms[t])/K)  
  }
}


#plot simulation
library(ggplot2)
df <- data.frame(time=1:(timesteps+1),NonMutant=Ns,Mutant=Ms)
ggplot(data=df)+
  geom_line(aes(x=time,y=NonMutant))+
  geom_line(aes(x=time,y=Mutant),col="red")+
  theme_classic() 



