k = 1000000 # Carrying capacity of the tumor
Nt <- 100 # initial number of cells in the non-mutant tumor
Mt <- 100 # initial number of cells in mutant tumor
# for loop to simulate the tumor growth of each tumor
for (t in 1:1000){
  if (t<200){ # tumor grows to equilibrium before the treatment drug is administered 
    rN = .1 # Rate of non-mutant tumor growth
    rM = .1 # Rate of mutant tumor growth
    Nt[t+1] <- Nt[t]+rN*Nt[t]*(1-((Nt[t]+Mt[t])/k)) # growth of non-mutant tumor
    Mt[t+1] <- Mt[t]+rM*Mt[t]*(1-((Nt[t]+Mt[t])/k)) # growth of non-mutant tumor
  }else{ # starting drug treatment 200 days after tumor started growing
    rN = -0.1 # Rate of non-mutant tumor growth during treatment
    rM = 0.05 # Rate of mutant tumor growth during treatment
    Nt[t+1] <- Nt[t]+rN*Nt[t]*(1-((Nt[t]+Mt[t])/k)) # shrinking tumor size from treatment
    Mt[t+1] <- Mt[t]+rM*Mt[t]*(1-((Nt[t]+Mt[t])/k)) # slower tumor growth from drug resistance
  }
}
# plot simulation
library(ggplot2)
# add the vector data from the tumor growth to a data frame to be able to plot
popN <- data.frame(time=1: length(Nt), N=Nt) 
popM <- data.frame(time=1: length(Mt), M=Mt)
# Use ggplot to make model, and use 2 plots to plot each patient situation
ggplot()+geom_line(data = popN, aes(x=time,y=N),color="blue")+
  geom_line(data = popM, aes(x=time,y=M),color="red")+
  xlab("Time in Days")+ylab("Number of Cancer Cells")+theme_classic()
