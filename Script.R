
#sets up base parameters for this, can be edited for flexibility. 
no_drug_rate <- 0.1
K <- 1000000
N0 <- 100
M0 <- 100
drug_rate_no_mutation <- -0.1
timestep <- 250

#must make location for the  
Nt=numeric(length=timestep)
Mt=numeric(length=timestep)
  
#load ggplot 
library(ggplot2)

#set a value to change once the drug is applied
drug <- F

#begin for loop
for(i in 100:timestep-1){
  #if statement so the first growth only occurs before the system reaches carrying capacity
  if(drug==F){
    Nt=Nt+no_drug_rate*Nt(1-(Nt+Mt)/K)
    Mt=Mt+no_drug_rate*Mt(1-(Nt+Mt)/K)
      if((Nt+Mt)/K==1){
        drug <- T
      }
  }
  else{
    Nt=Nt+drug_rate_no_mutation*Nt(1-(Nt+Mt)/K)
    Mt=Mt+no_drug_rate*0.5*Mt(1-(Nt+Mt)/K)
  }
    return
}
  
