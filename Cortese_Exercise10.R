#Problem 1
nsteps<-350
N0<-99 #Intial Population of Normal cancer cells
M0<-1 #Initial Population of Mutant Cells
rN<-0.1 #Growth rate of Normal cells
rDN<--0.1 #Growth rate with treatment
rM<-0.1 #Growth rate of Mutant cells
K<-1000000 #Carrying capacity
Ns=numeric(length=nsteps)
Ns[1]=N0
Ms=numeric(length=nsteps)
Ms[1]=M0
for(i in 1:nsteps){
  while(i<150){
  Ns[i+1]=Ns[i]+rN*Ns[i]*(1-((Ns[i]+Ms[i])/K))
  Ms[i+1]=Ms[i]+rM*Ms[i]*(1-((Ns[i]+Ms[i])/K)) #No treatment
  i<-i+1 #Escape while loop
  }
  Ns[i+1]=Ns[i]+rDN*Ns[i]*(1-((Ns[i]+Ms[i])/K)) #Drug treatment
  Ms[i+1]=Ms[i]+rM*Ms[i]*(1-((Ns[i]+Ms[i])/K))
}
#Equilibrium then add drug to get to new equilibrium. While i is < certain day value?
#library(ggplot2)
sim<-data.frame(time=1:length(Ns),N=Ns,M=Ms)
ggplot(data=sim)+geom_line(aes(x=time,y=N), col='black')+
  geom_line(aes(x=time,y=M), col='red')+
  xlab("Time (days)")+ylab("Population (Num. of Cells)")+theme_classic()
#Black is Normal Cells, Red is Mutant Cells
