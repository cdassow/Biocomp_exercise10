#Exercise 10
population=function(N0=99,M0=1,rn=0.1,rm=0.1,rnd=-0.1,rmd=0.05,K=1000000,timesteps=500){
  Nt=numeric(timesteps)
  Mt=numeric(timesteps)
  Nt[1]=N0
  Nt[2]=N0+N0*rn*(1-((N0+M0)/K))
  Mt[1]=M0
  Mt[2]=M0+M0*rm*(1-((N0+M0)/K))
  for(t in 3:(timesteps)){
    if(Nt[t-1]-Nt[t-2]>0.1){
      Nt[t]=Nt[t-1]+Nt[t-1]*rn*(1-((Nt[t-1]+Mt[t-1])/K))
      Mt[t]=Mt[t-1]+Mt[t-1]*rm*(1-((Nt[t-1]+Mt[t-1])/K))
    }else{
      Nt[t]=Nt[t-1]+Nt[t-1]*rnd*(1-((Nt[t-1]+Mt[t-1])/K))
      Mt[t]=Mt[t-1]+Mt[t-1]*rmd*(1-((Nt[t-1]+Mt[t-1])/K))
    }
  }
  pop=cbind(Nt,Mt)
  return(pop)
}

pop=data.frame(population())
pop$time=(1:nrow(pop))
ggplot(data=pop)+
  geom_line(aes(x=time,y=Nt),col='red')+
  geom_line(aes(x=time,y=Mt),col='blue')+
  xlab("Time")+
  ylab("Cells")+
  theme_classic()