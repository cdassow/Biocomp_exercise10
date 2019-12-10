setwd("~/Desktop/biocompshell/Biocomp_exercise10/")
library(ggplot2)

#For first 1-100 divisions, M=0
cancersim <- function(N0=99,M0=1,timesteps=400,K=1E6,r=0.1,rdrug=-0.1, rmdrug=0.5*r, drugtime=200){
  N<-numeric(timesteps)
  M<-numeric(timesteps)
  N[1] <- N0
  M[1] <- M0
    for (t in 1:(timesteps-1)){
  if(t<drugtime){
    N[t+1]<-N[t]+r*N[t]*(1-(N[t]-M[t])/K)
    M[t+1]<-M[t]+r*M[t]*(1-(N[t]-M[t])/K)
  }else{
    N[t+1]<-N[t]+rdrug*N[t]*(1-(N[t]-M[t])/K)
    M[t+1]<-M[t]+rmdrug*M[t]*(1-(N[t]-M[t])/K)
  }}
  out <- data.frame(divisions=1:timesteps,N,M)
  return(out)
}

cancerequil <- cancersim()

ggplot(data=cancerequil)+
  geom_line(aes(x=divisions,y=N),col='black')+
  geom_line(aes(x=divisions,y=M),col='red')+
  xlab("Number of Divisions")+
  ylab("Number of Cells")+
  theme_classic()
  


