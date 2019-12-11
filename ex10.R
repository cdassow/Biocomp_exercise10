library(ggplot2)
#set initial parameters without drug before eq 
N0=99
r=0.1
K=1000000
timesteps=200

M0=1

#create vector to store N's and set initial N 
N=numeric(length=timesteps)
N[1]=N0

M=numeric(length=timesteps)
M[1]=M0

#simulate 
for (t in 1:(timesteps-1)){
  N[t+1] <- N[t]+r*N[t]*(1-((N[t]+M[t])/K))  
  M[t+1] <- M[t]+r*M[t]*(1-((N[t]+M[t])/K)) 

}

#make data frame to determine eq point 
detEq <- data.frame(time= (1:timesteps), N, M)

#plot to determine N eq point
a <- ggplot(data=detEq)+
geom_line(aes(x=time, y=N)
a

#eq determined to be 195 based on graph 

#make data frame before treatment 
befEq<-data.frame(time=(1:194), N[1:194],M[1:194], treatment=c(rep(0,194)))
colnames(befEq)=c("time", "N", "M", "treatment")

#set new parameters 
N20=N[195]
rn2=-0.1
K=1000000
timesteps2=500

M20=M[195]
rm2=0.05


#create vector to store N's and set initial N 
N2=numeric(length=timesteps2)
N2[1]=N20

M2=numeric(length=timesteps2)
M2[1]=M20

#simulate 
for (t in 1:(timesteps2-1)){
  N2[t+1] <- N2[t]+rn2*N2[t]*(1-((N2[t]+M2[t])/K)) 
  M2[t+1] <- M2[t]+rm2*M2[t]*(1-((N2[t]+M2[t])/K))
}

#make data frame after Eq
aftEq<- data.frame(time=(195:694),N2,M2, treatment=c(rep(1,500)))

colnames(aftEq)=c("time", "N", "M", "treatment")

#combine befEq and aftEq
final2<-rbind(befEq,aftEq)

#final graph 
b <- ggplot(data = final2) + 
  geom_line(aes(x = time, y = N), col = "black") +
  geom_line(aes(x = time, y = M), col = "red") +
  xlab("Time") + ylab("Population Size")
b

  

