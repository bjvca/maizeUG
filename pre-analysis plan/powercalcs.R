### adapt for linear probability model

rm(list=ls())

possible.ns <- seq(from=100, to=2000, by=50)     # The sample sizes we'll be considering
powers <- rep(NA, length(possible.ns))           # Empty object to collect simulation estimates
alpha <- 0.05                                    # Standard significance level
sims <- 500                                      # Number of simulations to conduct for each N

#### Outer loop to vary the number of subjects ####
for (j in 1:length(possible.ns)){
  N <- possible.ns[j]                              # Pick the jth value for N
  
  significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
  
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <-  rnorm(n=N, mean= -.8, sd=1)              # control potential outcome
    tau <- .9
    Y1 <- (Y0 + tau) >0
	Y0 <- Y0>0
    Z.sim <- rbinom(n=N, size=1, prob=.5)          # Do a random assignment
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               # Reveal outcomes according to assignment
    fit.sim <- lm(Y.sim ~ Z.sim)                   # Do analysis (Simple regression)
    p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
    significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
  }
  
  powers[j] <- mean(significant.experiments)       # store average success rate (power) for each N
}
plot(possible.ns, powers, ylim=c(0,1))





















###################################
rm(list=ls())

possible.ns <- seq(from=150, to=1000, by=50)     # The sample sizes we'll be considering
powers <- rep(NA, length(possible.ns))           # Empty object to collect simulation estimates
powers.cov <- rep(NA, length(possible.ns))   
alpha <- 0.05                                    # Standard significance level
sims <- 500                                      # Number of simulations to conduct for each N

#### Outer loop to vary the number of subjects ####
for (j in 1:length(possible.ns)){
  N <- possible.ns[j]                              # Pick the jth value for N
  
  significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
  significant.experiments.cov <- rep(NA, sims)      # Need a second empty vector here too
  
 
  for (i in 1:sims){

### insert one additional dummy for every 55 additional observations



bloc <- 1:N
bloc <- trunc(bloc/55)
binom <- data.frame(bloc)

for(t in unique(bloc)) {
   binom[paste("bloc",t,sep="")] <- ifelse(binom$bloc==t,1,0)
 }

binom <- binom[3:length(binom)]




    
    ## Hypothesize Control Outcome as a function of gender, age, and error
    Y0 <-  rnorm(n=N, mean= -.8, sd=1) +as.matrix(binom)%*%as.matrix(rnorm(length(binom)))
    
    ## This is all the same ##
    tau <- .3
    Y1 <- (Y0 + tau) >0
	Y0 <- Y0>0
    Z.sim <- rbinom(n=N, size=1, prob=.5)
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)
    fit.sim <- lm(Y.sim ~ Z.sim)
    
    ## This is the novel analysis -- including two covariates to increase precision ##
df <- data.frame(Z.sim , binom)
    fit.sim.cov <- lm(Y.sim ~ ., data=df)
    
    ## extract p-values and calculate significance ##
    p.value <- summary(fit.sim)$coefficients[2,4]
    p.value.cov <- summary(fit.sim.cov)$coefficients[2,4]
    significant.experiments[i] <- (p.value <= alpha)
    significant.experiments.cov[i] <- (p.value.cov <= alpha)
  }
  print(j) 
  powers[j] <- mean(significant.experiments)
  powers.cov[j] <- mean(significant.experiments.cov)
}

plot(possible.ns, powers, ylim=c(0,1))
points(possible.ns, powers.cov, col="red")

################# multiple arms

rm(list=ls())
#install.packages("randomizr")
library(randomizr)    # randomizr package for complete random assignment
### prepare yield data (taken from the pasic rice study)
library(foreign)
prod <- read.dta("/home/bjvca/data/projects/PASIC/productivity/data/merged/productivity.dta")
prod_rice <- subset(prod,crop == "rice")
## this is plot level, take averages by hhid
plot_rice <- aggregate(prod_rice$prod,list(prod_rice$hhid), mean, na.rm=T)
names(plot_rice) <- c("hhid","prod")
### some data cleaning
plot_rice <- subset(plot_rice, prod > 200 & prod <4000)


### merge in location data to bloc on village and/or parish level
loc <- read.csv("/home/bjvca/data/projects/digital green/pre-analysis plan/loc_parish.csv")
plot_rice <- merge(loc,plot_rice, by="hhid")
summary(lm(prod~as.factor(parish),data=plot_rice))


possible.ns <- seq(from=100, to=3600, by=55)
power.atleastone <- rep(NA, length(possible.ns))
power.bothtreatments <- rep(NA, length(possible.ns))
power.fullranking <- rep(NA, length(possible.ns))
power.all <- rep(NA, length(possible.ns))
 power.allvsC <- rep(NA, length(possible.ns))
  power.match <- rep(NA, length(possible.ns))
power.Rbothvssingel <- rep(NA, length(possible.ns))
power.Mbothvssingel <- rep(NA, length(possible.ns))

alpha <- 0.05 
sims <- 100
#### Outer loop to vary the number of subjects ####
for (j in 1:length(possible.ns)){
  N <- possible.ns[j]
  p.allvsC <- rep(NA, sims)
  p.matchsvnomatch <- rep(NA, sims)
  p.matchvsC <- rep(NA, sims) 
fit.eachvsC.sim <- matrix(NA,10,sims)
	p.Rbothvssingel <- rep(NA, sims)
	p.Mbothvssingel <- rep(NA, sims)
p.NomatchvsC <- rep(NA, sims)
 # p.YBBvsYMM <- rep(NA, sims)
 # p.YMMvsYFF <- rep(NA, sims)
 # p.YBBvsYFF <- rep(NA, sims)

  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
Y0 <-  plot_rice[sample(1:dim(plot_rice)[1],N, replace=T),]
 #   Y0 <-   rnorm(n=N, mean=1732, sd=920) ### we should sample (with replacement from real data, eg pasic yield data) 

    
    Y0$Z.sim <- complete_ra(N=N, num_arms=10)  ### Need to incorporate blocking here
levels(Y0$Z.sim) <- c("YMM","YMF","YMB","YFB","YFF","YFB","YBM","YBF","YBB", "Ctrl")
### model effects
    tau <- seq(0,.6,length.out=10) ### seven different effects

    Y0$prod[Y0$Z.sim == "YMM"] <- Y0$prod[Y0$Z.sim == "YMM"] * ( 1 + tau[4])
    Y0$prod[Y0$Z.sim == "YMF"] <- Y0$prod[Y0$Z.sim == "YMF"] * ( 1 + tau[3])  
     Y0$prod[Y0$Z.sim == "YMB"] <- Y0$prod[Y0$Z.sim == "YMB"] * ( 1 + tau[5]) 
        Y0$prod[Y0$Z.sim == "YFM"] <- Y0$prod[Y0$Z.sim == "YFM"] * ( 1 + tau[2]) 
        Y0$prod[Y0$Z.sim == "YFF"] <- Y0$prod[Y0$Z.sim == "YFF"] * ( 1 + tau[6]) 
        Y0$prod[Y0$Z.sim == "YFB"] <- Y0$prod[Y0$Z.sim == "YFB"] * ( 1 + tau[9]) 
        Y0$prod[Y0$Z.sim == "YBM"] <- Y0$prod[Y0$Z.sim == "YBM"] * ( 1 + tau[8]) 
        Y0$prod[Y0$Z.sim == "YBF"] <- Y0$prod[Y0$Z.sim == "YBF"] * ( 1 + tau[7]) 
      Y0$prod[Y0$Z.sim == "YBB"] <- Y0$prod[Y0$Z.sim == "YBB"] * ( 1 + tau[10]) 

## tests
    fit.allvsC.sim <- lm(prod ~ Z.sim=="Ctrl", data=Y0)

    fit.matchvsnomatch.sim <- lm(prod ~ (Z.sim=="Ctrl") + as.factor(parish), data=Y0)

#fit.Mbothvssingel.sim <- lm(Y.sim ~ (Z.sim=="T3" | Z.sim == "T6" | Z.sim == "T9"), data=subset(frame.sim, Z.sim=="T7" | Z.sim == "T8" | Z.sim == "T9" | Z.sim=="T1" | Z.sim == "T2" | Z.sim == "T3"  |Z.sim=="T4" | Z.sim == "T5" | Z.sim == "T6"))
#    fit.matchvsC.sim <- lm(Y.sim ~ (Z.sim=="T1" | Z.sim == "T5" | Z.sim == "T9"), data=subset(frame.sim, Z.sim=="T1" | Z.sim == "T5" | Z.sim == "T9" | Z.sim == "T10"))
#fit.NomatchvsC.sim <- lm(Y.sim ~ (Z.sim=="T2" | Z.sim == "T4"), data=subset(frame.sim, Z.sim=="T2" | Z.sim == "T4"  | Z.sim == "T10"))

    #fit.nomatchvsYMM.sim <- lm(Y.sim ~ Z.sim=="T9", data=subset(frame.sim, Z.sim=="T9" | Z.sim=="T1"))
    # fit.YBBvsYFF.sim <- lm(Y.sim ~ Z.sim=="T9", data=subset(frame.sim, Z.sim=="T9" | Z.sim=="T5"))
    #fit.YMMvsYFF.sim <- lm(Y.sim ~ Z.sim=="T1", data=subset(frame.sim, Z.sim=="T1" | Z.sim=="T5"))
    ### Need to capture coefficients and pvalues (one-tailed tests, so signs are important)

    p.allvsC[i] <- summary(fit.allvsC.sim)$coefficients[2,4]
    p.matchsvnomatch[i] <- summary(fit.matchvsnomatch.sim)$coefficients[2,4]
 #  p.matchvsC[i] <- summary(fit.matchvsC.sim)$coefficients[2,4]
#p.NomatchvsC[i] <- summary(fit.NomatchvsC.sim)$coefficients[2,4]
#	p.Rbothvssingel[i] <- summary(fit.Rbothvssingel.sim)$coefficients[2,4]
#	p.Mbothvssingel[i] <- summary(fit.Mbothvssingel.sim)$coefficients[2,4]
    

  }


  power.allvsC[j] <- mean(p.allvsC <= alpha/10)
  power.match[j] <- mean(p.matchsvnomatch <= alpha/10)
#power.Rbothvssingel[j] <-   mean(p.Rbothvssingel <= alpha/10)
#power.Mbothvssingel[j] <-   mean(p.Mbothvssingel <= alpha/10)
  print(j)
}

plot(possible.ns, power.allvsC, ylim=c(0,1), type = "l")
lines(possible.ns, power.match, col="red")
#lines(possible.ns, power.Rbothvssingel, col="blue")
#lines(possible.ns, power.Mbothvssingel, col="green")








