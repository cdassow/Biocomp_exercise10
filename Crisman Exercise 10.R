N0=99
M0=1
rn=0.1
rm=0.1
rm_with_drug=0.5*rm
rn_with_drug=-0.1

K=1000000
timesteps=500

#create vector to store N's
Ns=numeric(length=timesteps)
Ns[1]=N0

Ms=numeric(length=timesteps)
Ms[1]=M0

#simulate
for(t in 1:150){
  Ns[t+1]<-Ns[t]+rn*Ns[t]*(1-(Ns[t]+Ms[t])/K)
  Ms[t+1]<-Ms[t]+rm*Ms[t]*(1-(Ns[t]+Ms[t])/K)
}
for(t in 150:499){
  Ns[t+1]<-Ns[t]+rn_with_drug*Ns[t]*(1-(Ns[t]+Ms[t])/K)
  Ms[t+1]<-Ms[t]+rm_with_drug*Ms[t]*(1-(Ns[t]+Ms[t])/K)
}

plot(1:timesteps, Ns, col = "green",xlab="Time",ylab="Population")
lines(1:timesteps, Ms, col = "black")

