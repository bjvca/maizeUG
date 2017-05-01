rm(list=ls())
system("rm myfile")
library(foreign)
library(foreach)
library(doParallel)
registerDoParallel(cores=4)

alpha <- .05
N <- 3000
sims <- 200
minsamp <- 400
bystep <- 50

ptm <- proc.time()
### grid search
## first fix control at 1/3 of N
 
possible.n2 <- seq(minsamp,N-3*minsamp, by=bystep)

#power.fullranking <- array(NA, c(length(possible.n2), length(possible.n2), length(possible.n2)))
power.fullranking <- array(NA, c(length(possible.n2), length(possible.n2)))

res <- foreach (j = 1:(length(possible.n2)) ) %dopar% {
#for (j in 1:(length(possible.n2)-3))  {

#r <- foreach (t = 1:(length(possible.n2)-j -2)) %dopar% {
for (t in 1:(length(possible.n2) - (j-1)) ) {
for (u in 1:(length(possible.n2) - (t -1) - (j - 1)) ) {

g1 <- possible.n2[j]
g2 <- possible.n2[t]
g3 <- possible.n2[u]
Z.sim <- sample(as.factor(c(rep("T4",floor(N-g1-g2-g3)), rep("T3", g3), rep("T2", g2), rep("T1", g1))))



  p.T1vsC <- rep(NA, sims)
  p.T2vsC <- rep(NA, sims)
  p.T2vsT1 <- rep(NA, sims)

 #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <-  rnorm(n=length(Z.sim), mean=60, sd=20)
    tau_1 <- 1
    tau_2 <- 4
   tau_3 <- 5
    Y1 <- Y0 + tau_1
    Y2 <- Y0 + tau_2
Y3 <- Y0 + tau_3
   # Z.sim <- complete_ra(N=N, num_arms=3)
    Y.sim <- Y0*(Z.sim=="T4") + Y3*(Z.sim=="T3") + Y1*(Z.sim=="T1") + Y2*(Z.sim=="T2")
#Y.sim <- Y0*(Z.sim=="T3") + Y1*(Z.sim=="T1") + Y2*(Z.sim=="T2")
    frame.sim <- data.frame(Y.sim, Z.sim)
    
    fit.T2vsC.sim <- lm(Y.sim ~ Z.sim=="T4", data=frame.sim)
    fit.T2vsT1.sim <- lm(Y.sim ~ Z.sim=="T2", data=subset(frame.sim, Z.sim!="T3" &  Z.sim!="T4"))
    


    p.T2vsC[i] <- summary(fit.T2vsC.sim)$coefficients[2,4]
    p.T2vsT1[i] <- summary(fit.T2vsT1.sim)$coefficients[2,4]
  }

#power.fullranking[j,t,u] <- mean(p.T2vsC < alpha/2 & p.T2vsT1 < alpha/2)
#power.fullranking[j,t] <- mean(p.T2vsC < alpha/2 & p.T2vsT1 < alpha/2)
write(c(j,t,u, mean(p.T2vsC < alpha/2 & p.T2vsT1 < alpha/2)),file="myfile",sep = ";",append=TRUE)

}
}
#return(power.fullranking[j,])

}

proc.time() - ptm
 dta <- read.csv("/home/bjvca/data/projects/PASIC/riceRCT/endline/myfile", sep = ";", header = F)



### to run this on amamzon EC
ssh -i "bjornkey.pem" ubuntu@publicIPaddress

sudo apt-get update
sudo apt-get install r-base-core


sudo R
install.packages("foreach")
install.packages("doParallel")

##than copy in code




