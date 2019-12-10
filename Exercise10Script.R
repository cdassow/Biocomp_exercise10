setwd("~/Desktop/biocompshell/Biocomp_exercise10/")
library(ggplot2)

#Make a function (cancersim) to simulate the growth of the two populations after mutation occurs
#timesteps=number of divisions for simulation
#K=carrying capacity
#r=rate of growth before drug treament
#rdrug=rate of growth in nonmutant cells after drug treatment
#rmdrug=rate of growth of mutant cells after drug treatment
#drugtime=number of divisions at which drug treatment occurs 
cancersim <- function(N0=99,M0=1,timesteps=1000,K=1E6,r=0.1,rdrug=-0.1, rmdrug=0.5*r, drugtime=200){
#Make empty vector (N and M) that for loop can fill
  N<-numeric(timesteps)
  M<-numeric(timesteps)
#Assign first number in vector as N0 and M0, N0 is the number of non-mutant cells at the mutation (99), M0 is the number of mutant cells at the first mutation (1) 
  N[1] <- N0
  M[1] <- M0
 #For loop to simulate cell growth, if before drug treatment, both cells will grow at same rate, if after, will grow at different   
  for (t in 1:(timesteps-1)){
  if(t<drugtime){
    N[t+1]<-N[t]+r*N[t]*(1-(N[t]+M[t])/K)
    M[t+1]<-M[t]+r*M[t]*(1-(N[t]+M[t])/K)
  }else{
    N[t+1]<-N[t]+rdrug*N[t]*(1-(N[t]+M[t])/K)
    M[t+1]<-M[t]+rmdrug*M[t]*(1-(N[t]+M[t])/K)
  }}
 #make dataframe to store the number of divisions, and number of cells for N and M over time
   out <- data.frame(divisions=1:timesteps,N,M)
  return(out)
}

#assign dataframe as cancerequil
cancerequil <- cancersim()

#plot simulation
ggplot(data=cancerequil)+
  geom_line(aes(x=divisions,y=N),col='black')+
  geom_line(aes(x=divisions,y=M),col='red')+
  xlab("Number of divisions after mutation")+
  ylab("Number of cells")+
  theme_classic()
  


