#Stephen Grisoli Exercise 10: Cancer Drug Resistance Modeling
library(ggplot2)
rm(list=ls())

#Initializing variables
N0 = 1
r = 0.1
mutant_r = 0.1
K = 1000000
timesteps = 1000

#Vector to hold wildtype cancer cells
NsCancer=numeric(length=timesteps)
NsCancer[1]=N0

#Vector to hold mutant cancer cells the vector is filled to 1s to satify later NsMutant[t-1] conditional
NsMutant=numeric(length=timesteps)
NsMutant[]=N0

#Simulation
for(t in 1:(timesteps-1)){
  #Simulating all cases except first
  if(t > 1){
    #Simulating before mutation 
    if((NsCancer[t] < 100) && (NsMutant[t]<10)){
      NsCancer[t+1] = NsCancer[t]+r*NsCancer[t]*(1-(NsCancer[t]+NsMutant[t])/K)
    }   else if((NsCancer[t]==NsCancer[t-1])){
        #Simulating condition changes with cancer drug
          NsCancer[t+1] = NsCancer[t]-1
          NsMutant[t+1] = NsMutant[t]-1
          mutant_r = 0.5*mutant_r 
          r = -r
        } else {
          #Simulating conditions after cancer drug
            NsCancer[t+1] = NsCancer[t]+r*NsCancer[t]*(1-((NsCancer[t]+NsMutant[t])/K))
            NsMutant[t+1] = NsMutant[t]+mutant_r*NsMutant[t]*(1-((NsMutant[t]+NsCancer[t])/K))
          }
  } else{
      #Simulating first step (to satisfy [t-1] conditional) 
      NsCancer[t+1] = NsCancer[t]+r*NsCancer[t]*(1-NsCancer[t]/K)
    }
}

#Final vectors to one dataframe
simCancer=data.frame(time=1:timesteps,Ncancer=NsCancer, Nmutant=NsMutant)

#Plot construction
progression_chart = ggplot()+
  geom_line(data=simCancer, aes(x=time, y=Ncancer, color='blue'), size=1)+
  geom_line(data=simCancer, aes(x=time, y=Nmutant, color='red'), size=1)+
  ylab("Number of cells")+
  xlab("Timesteps")+
  scale_color_discrete(name = "Cell Lines", labels = c("Non-Mutant", "Mutant"))+
  ggtitle("Cell Lines over Timesteps")+
  theme_classic()

  



