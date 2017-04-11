### this is a program to run the power caluclations for the UGmaize study to be included in the pre-analysis plan
### it is based on Alexander Coppock's example code that goes with: http://egap.org/methods-guides/10-things-you-need-know-about-statistical-power

rm(list=ls())
#install.packages("randomizr")
library(randomizr)    # randomizr package for complete random assignment
### prepare yield data (taken from the pasic rice study)
prod <- read.csv("/home/bjvca/data/projects/digital green/pre-analysis plan/productivity.csv")
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


possible.ns <- seq(from=100, to=4000, by=55)
power.atleastone <- rep(NA, length(possible.ns))
power.bothtreatments <- rep(NA, length(possible.ns))
power.fullranking <- rep(NA, length(possible.ns))
power.all <- rep(NA, length(possible.ns))
 power.allvsC <- rep(NA, length(possible.ns))
  power.match <- rep(NA, length(possible.ns))
power.Rbothvssingel <- rep(NA, length(possible.ns))
power.Mbothvssingel <- rep(NA, length(possible.ns))

alpha <- 0.05 
sims <- 500
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
    tau <- seq(0,.3,length.out=10) ### seven different effects

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
    fit.allvsC.sim <-lm(prod ~ (Z.sim=="Ctrl") + as.factor(parish), data=Y0)

    fit.matchvsnomatch.sim <- lm(prod ~ (Z.sim=="YMM" | Z.sim=="YWW" | Z.sim=="YBB") + as.factor(parish), data=subset(Y0,Z.sim=="YMM" | Z.sim=="YWW" | Z.sim=="YBB"  | Z.sim=="YFM"   | Z.sim=="YMF" ))
 fit.Rbothvssingel.sim <- lm(prod ~ (Z.sim=="YBM" | Z.sim == "YBF" | Z.sim == "YBB")+ as.factor(parish), data=subset(Y0,Z.sim!="Ctrl"))
 fit.Mbothvssingel.sim <- lm(prod ~ (Z.sim=="YMB" | Z.sim == "YFB" | Z.sim == "YBB")+ as.factor(parish), data=subset(Y0,Z.sim!="Ctrl"))

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
	p.Rbothvssingel[i] <- summary(fit.Rbothvssingel.sim)$coefficients[2,4]
	p.Mbothvssingel[i] <- summary(fit.Mbothvssingel.sim)$coefficients[2,4]
    

  }


  power.allvsC[j] <- mean(p.allvsC <= alpha/10 )
  power.match[j] <- mean(p.matchsvnomatch <= alpha/10)
power.Rbothvssingel[j] <-   mean(p.Rbothvssingel <= alpha/10)
power.Mbothvssingel[j] <-   mean(p.Mbothvssingel <= alpha/10)
power.all[j] <-  mean(p.allvsC <= alpha/10 & p.matchsvnomatch <= alpha/10 & p.Rbothvssingel <= alpha/10 & p.Mbothvssingel <= alpha/10)
  print(j)
}
pdf("/home/bjvca/data/projects/digital green/pre-analysis plan/power.pdf")
plot(possible.ns, power.allvsC, ylim=c(0,1), type = "l", lwd=3, xlab = "sample size", ylab="power")
lines(possible.ns, power.match, col="green", lwd=3)
abline(h = .8, lty=2, lwd=3)

lines(possible.ns, power.Rbothvssingel, col="blue" , lwd=3)
lines(possible.ns, power.Mbothvssingel, col="yellow" , lwd=3)
lines(possible.ns, power.all, col="red", lwd=3)
dev.off()








