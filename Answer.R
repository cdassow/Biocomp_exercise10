# Seth Parker, Kurt Kohler, Chris Gager: Exercise 10 "Fun With Cancer"
# We simulate 1 year of John Doe's cancer growth, with treatment beginning 200 days after cancer growth began

# Set all these variables before beginning the for loop
rN <- 0.1
rM <- 0.1
k <- 10^6
# Nt = Nt + rN * Nt (1 - (Nt + Mt)/k)
# Mt = Mt + rM * Mt (1 - (Nt + Mt)/k)
popN <- c(1:365)
popM <- c(1:365)
Nt <- 2 # Starting Population
Mt <- 2 # Starting population
day <- 0

# This loop is the model
for (cell in 1:365)
{
  day <- day + 1
  if (day >= 200) # Treatment begins
  {
    Nt <- Nt + (rN - 0.2) * Nt * (1 - (Nt + Mt)/k)
    Mt <- Mt + (rM * 0.5) * Mt * (1 - (Nt + Mt)/k)
  }
  else if (Mt + Nt >= 100) # Separate subcolony forms
  {
    if (day < 200) 
    {
      Mt <- Mt + rM * Mt * (1 - (Nt + Mt)/k)
      Nt <- Nt + rN * Nt * (1 - (Nt + Mt)/k)
    }
  }
  else
  {
    if (day < 200)
    {
    Nt <- Nt + rN * Nt * (1 - (Nt + Mt)/k)
    }  
  }
  popN[cell] <- Nt
  popM[cell] <- Mt
}

# Data organization and graph generation

days_with_cancer <- c(1:365)
population_data <- as.data.frame(cbind(popM, popN, days_with_cancer))

library(ggplot2)

ggplot(data = population_data)+
  geom_line(aes(x = days_with_cancer, y = popN, color = "Treatable Cells"), size = 1.2)+
  geom_line(aes(x = days_with_cancer, y = popM, color = "Treatment Resistant Cells"), size = 1.2)+
  theme_classic()+
  labs(x = "Days Since Inception of Cancer", y = "Population of Cancer Cell Subcolonies", title = "John Doe's Year of Cancer (Treatment Began on Day 200)")
  

