
#sets up base parameters for this, can be edited for flexibility. 
no_drug_rate <- 0.1
K <- 1000000
drug_rate_no_mutation <- -0.1
timestep <- 1000

#must make location for the  
Nt <- numeric(length=timestep)
Mt <- numeric(length=timestep)
Nt[1] <- 99
Mt[1] <- 1  
#load ggplot 
library(ggplot2)

#begin for loop
for(i in 1:(timestep-1)){
  
  #if statement so the first growth only occurs before the system reaches equilibrium, can adjust when the drug is applied by changing 3. 
  if(i<timestep/3){
    Nt[i+1]=Nt[i]+no_drug_rate*Nt[i]*(1-(Nt[i]+Mt[i])/K)
    Mt[i+1]=Mt[i]+no_drug_rate*Mt[i]*(1-(Nt[i]+Mt[i])/K)
  }
  else{
    Nt[i+1]=Nt[i]+drug_rate_no_mutation*Nt[i]*(1-(Nt[i]+Mt[i])/K)
    Mt[i+1]=Mt[i]+no_drug_rate*0.5*Mt[i]*(1-(Nt[i]+Mt[i])/K)
  }
}
#create a dataframe and then plot of the results
cancer <- data.frame(time=1:length(Nt),N_of_Nonmutants=Nt, N_of_Mutants=Mt)
ggplot(data=cancer)+geom_line(mapping=aes(x=time, y=N_of_Nonmutants))+
  geom_line(mapping=aes(x=time, y=N_of_Mutants))+
  labs(y="Population Size", x="Time")+ggtitle("Mutant and Non-Mutant Population Sizes over Time")


