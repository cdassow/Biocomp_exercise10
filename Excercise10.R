# Excercise 10

# parameters
rn = 0.1
rm = 0.1
K = 1000000
Nt = 1
Mt = 1
timesteps = 500

# create vector to store N and M and set initial N,M
N = numeric(length=timesteps)
N[1] = 1
M = numeric(length=timesteps)
M[1] = 0

# simulate growth without treatment
for (t in c(1:timesteps-1)){
  if (Nt < 100){
    Nt = Nt + rn*Nt*(1-(Nt+Mt)/K)
    N[t+1] = Nt
    M[t+1] = 0
  }else{
    Nt = Nt + rn*Nt*(1-(Nt+Mt)/K)
    Mt = Mt + rm*Mt*(1-(Nt+Mt)/K)
    N[t+1] = Nt
    M[t+1] = Mt
  }
}


# plot result without treatments
time = c(1:timesteps)
plot(N ~ time, col = 'blue', type = 'l')
lines(M ~ time, col = 'red')


# add treatment sometime
# parameters
rn = 0.1
rm = 0.1
rnD = -0.1 # growth rate under drug treatment
rmD = 0.05 # growth rate under drug treatment
K = 1000000
Nt = 1
Mt = 1
timesteps = 1000
treattime = 300 # we set teatment time at 300 because we found they reach equilibrium after about 250 timestep

# create vector to store N and M and set initial N,M
N = numeric(length=timesteps)
N[1] = 1
M = numeric(length=timesteps)
M[1] = 0

# simulate under treatment at treatment time
for (t in c(1:timesteps-1)){
  if (Nt < 100 & t < treattime){
    Nt = Nt + rn*Nt*(1-(Nt+0)/K)
    N[t+1] = Nt
    M[t+1] = 0
  }else{
    if (t < treattime){
      Nt = Nt + rn*Nt*(1-(Nt+Mt)/K)
      Mt = Mt + rm*Mt*(1-(Nt+Mt)/K)
    }else{
      Nt = Nt + rnD*Nt*(1-(Nt+Mt)/K)
      Mt = Mt + rmD*Mt*(1-(Nt+Mt)/K)
    }
    N[t+1] = Nt
    M[t+1] = Mt
  }
}


# plot result
time = c(1:timesteps)
plot(M ~ time, col = 'blue', type = "l", lty = 1, ylim = c(0,K),
     xlab = 'Time (Step)', ylab = 'Cell Number', main = 'Wild Type vs Mutated Cacner Cells Proliferation')
points(N ~ time, col = 'red',type = "l", lty = 1)
legend("topleft", legend = c('Mutated Cells', 'Wild Type Cells'),
       col = c('blue', 'red'), lty = 1, cex = 0.8)



