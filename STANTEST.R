library(rstan)
scode <- "
parameters {
  real y[2]; 
} 
model {
  y[1] ~ normal(0, 1);
  y[2] ~ double_exponential(0, 2);
} 
"
seed <- 123 # or any other integer 
f1 <- stan(model_code = scode, chains = 1, seed = seed, chain_id = 1) 
f2 <- stan(fit = f1, chains = 2, seed = seed, chain_id = 2:3) 
f12 <- sflist2stanfit(list(f1, f2)) 

library(GridR)
library(GridR)
grid.init(service="condor.local", localTmpDir=".tmp")
grid.apply('g2',
           function(i) stan(fit = f1, seed = seed, chains = 1, chain_id = i, refresh = -1), 
           2:3, 
           wait=TRUE, 
           check=TRUE, 
           batch=c(1))

