### this is a program to run the power caluclations for the UGmaize study to be included in the pre-analysis plan
### it is based on Alexander Coppock's example code that goes with: http://egap.org/methods-guides/10-things-you-need-know-about-statistical-power

rm(list=ls())
library(foreign)
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

##### new: base on maize from UNHS2005/06
loc <- read.dta("/home/bjvca/data/data/UGANDA/UNHS/UNHS_2005/UNHS_III_AGRIC_DATA/asec1b.dta")
area <- read.dta("/home/bjvca/data/data/UGANDA/UNHS/UNHS_2005/UNHS_III_AGRIC_DATA/asec4a.dta")
prod <- read.dta("/home/bjvca/data/data/UGANDA/UNHS/UNHS_2005/UNHS_III_AGRIC_DATA/asec7a.dta")

yield <- merge(area, prod, by.x=c("hh","a4aid","a4aq5b"), by.y=c("hh", "a7aq1","a7aq2b"))
#keep only maize
yield <- subset(yield,a4aq5b=="130")
#merge in location
yield <- merge(yield,loc, by="hh")
#keep only maize growing in eastern lowlands
#yield <- subset(yield, district %in% c("iganga", "jinja","kamuli","bugiri","mayuge"))

yield$yld <- yield$a7aq3b*yield$a7aq3d/yield$a4aq3
yield$yld <- yield$yld*2.47105
yield <- subset(yield, yld > 200 & yld < 1300) 

###

possible.ns <- seq(from=500, to=3000, by=50)
alpha <- 0.05 
sims <- 100

power.atleastone <- rep(NA, length(possible.ns))
power.bothtreatments <- rep(NA, length(possible.ns))
power.fullranking <- rep(NA, length(possible.ns))
power.all <- rep(NA, length(possible.ns))
 power.allvsC <- rep(NA, length(possible.ns))
  power.match <- rep(NA, length(possible.ns))
power.Rbothvssingel <- rep(NA, length(possible.ns))
power.Mbothvssingel <- rep(NA, length(possible.ns))
  power.yield <- rep(NA, length(possible.ns))
  power.gap <- rep(NA, length(possible.ns))

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
p.gap1 <- rep(NA, sims)
p.gap2 <- rep(NA, sims)
p.gap3 <- rep(NA, sims)

  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
Y0 <-  yield[sample(1:dim(yield)[1],N, replace=T),]
 #   Y0 <-   rnorm(n=N, mean=1732, sd=920) ### we should sample (with replacement from real data, eg pasic yield data) 

    
Z.sim <- sample(as.factor(c(rep("Ctrl",N/2), rep("YMM", N/2/9),rep("YMF", N/2/9),rep("YMB", N/2/9), rep("YFM", N/2/9), rep("YFF", N/2/9), rep("YFB", N/2/9), rep("YBM", N/2/9), rep("YBF", N/2/9), rep("YBB", N/2/9))))

Y0$Z.sim <- as.factor(c(as.character(Z.sim),rep(NA,N - length(Z.sim))))
Y0 <- subset(Y0, !is.na(Z.sim))

#    Y0$Z.sim <- complete_ra(N=N, num_arms=10)  ### Need to incorporate blocking here
#levels(Y0$Z.sim) <- c("YMM","YMF","YMB","YFB","YFF","YFB","YBM","YBF","YBB", "Ctrl")
### model effects
    tau <- seq(0,.3,length.out=10) ### seven different effects

    Y0$yld[Y0$Z.sim == "YMM"] <- Y0$yld[Y0$Z.sim == "YMM"] * ( 1 + .1)
    Y0$yld[Y0$Z.sim == "YMF"] <- Y0$yld[Y0$Z.sim == "YMF"] * ( 1 + .05)  
     Y0$yld[Y0$Z.sim == "YMB"] <- Y0$yld[Y0$Z.sim == "YMB"] * ( 1 + .15) 
        Y0$yld[Y0$Z.sim == "YFM"] <- Y0$yld[Y0$Z.sim == "YFM"] * ( 1 + .015) 
        Y0$yld[Y0$Z.sim == "YFF"] <- Y0$yld[Y0$Z.sim == "YFF"] * ( 1 + .075) 
        Y0$yld[Y0$Z.sim == "YFB"] <- Y0$yld[Y0$Z.sim == "YFB"] * ( 1 + .125) 
        Y0$yld[Y0$Z.sim == "YBM"] <- Y0$yld[Y0$Z.sim == "YBM"] * ( 1 + .15) 
        Y0$yld[Y0$Z.sim == "YBF"] <- Y0$yld[Y0$Z.sim == "YBF"] * ( 1 + .125) 
      Y0$yld[Y0$Z.sim == "YBB"] <- Y0$yld[Y0$Z.sim == "YBB"] * ( 1 + .20) 
Y0$loc <- as.factor(as.numeric(Y0$district)*100 + Y0$ea)
## tests
    fit.allvsC.sim <-lm(yld ~ (Z.sim=="Ctrl") + loc , data=Y0)

    fit.matchvsnomatch.sim <- lm(yld ~ (Z.sim=="YMM" | Z.sim=="YWW" | Z.sim=="YBB")  + loc , data=subset(Y0,Z.sim=="YMM" | Z.sim=="YWW" | Z.sim=="YBB"  | Z.sim=="YFM"   | Z.sim=="YMF" ))
 fit.Rbothvssingel.sim <- lm(yld ~ (Z.sim=="YBM" | Z.sim == "YBF" | Z.sim == "YBB")  + loc, data=subset(Y0,Z.sim!="Ctrl"))
 fit.Mbothvssingel.sim <- lm(yld ~ (Z.sim=="YMB" | Z.sim == "YFB" | Z.sim == "YBB")  + loc, data=subset(Y0,Z.sim!="Ctrl"))

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


  power.allvsC[j] <- mean(p.allvsC <= alpha )
  power.match[j] <- mean(p.matchsvnomatch <= alpha/10)
power.Rbothvssingel[j] <-   mean(p.Rbothvssingel <= alpha/10)
power.Mbothvssingel[j] <-   mean(p.Mbothvssingel <= alpha/10)
power.all[j] <-  mean(p.allvsC <= alpha/10 & p.matchsvnomatch <= alpha/10 & p.Rbothvssingel <= alpha/10 & p.Mbothvssingel <= alpha/10)
  print(j)
}

save1 <- data.frame(possible.ns,power.allvsC)
pdf("/home/bjvca/data/projects/digital green/pre-analysis plan/power1.pdf")
plot(possible.ns, power.allvsC, ylim=c(0,1), type = "l", lwd=3, xlab = "sample size", ylab="power")

abline(h = .8, lty=2, lwd=3)
dev.off()


### second hypothesis




###



power.atleastone <- rep(NA, length(possible.ns))
power.bothtreatments <- rep(NA, length(possible.ns))
power.fullranking <- rep(NA, length(possible.ns))
power.all <- rep(NA, length(possible.ns))
 power.allvsC <- rep(NA, length(possible.ns))
  power.match <- rep(NA, length(possible.ns))
power.Rbothvssingel <- rep(NA, length(possible.ns))
power.Mbothvssingel <- rep(NA, length(possible.ns))
  power.yield <- rep(NA, length(possible.ns))
  power.gap <- rep(NA, length(possible.ns))
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
p.gap1 <- rep(NA, sims)
p.gap2 <- rep(NA, sims)
p.gap3 <- rep(NA, sims)

  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
Y0 <-  yield[sample(1:dim(yield)[1],N, replace=T),]
 #   Y0 <-   rnorm(n=N, mean=1732, sd=920) ### we should sample (with replacement from real data, eg pasic yield data) 

    

Z.sim <-  sample(as.factor(c(rep("YMM",N/4), rep("YFF", N/4),rep("YMF", N/4),rep("YFM", N/4)))) 
Y0$Z.sim <- as.factor(c(as.character(Z.sim),rep(NA,N - length(Z.sim))))
Y0 <- subset(Y0, !is.na(Z.sim))


#    Y0$Z.sim <- complete_ra(N=N, num_arms=10)  ### Need to incorporate blocking here
#levels(Y0$Z.sim) <- c("YMM","YMF","YMB","YFB","YFF","YFB","YBM","YBF","YBB", "Ctrl")
### model effects
    tau <- seq(0,.3,length.out=10) ### seven different effects

    Y0$yld[Y0$Z.sim == "YMM"] <- Y0$yld[Y0$Z.sim == "YMM"] * ( 1 + .1)
    Y0$yld[Y0$Z.sim == "YMF"] <- Y0$yld[Y0$Z.sim == "YMF"] * ( 1 + .05)  
     Y0$yld[Y0$Z.sim == "YMB"] <- Y0$yld[Y0$Z.sim == "YMB"] * ( 1 + .15) 
        Y0$yld[Y0$Z.sim == "YFM"] <- Y0$yld[Y0$Z.sim == "YFM"] * ( 1 + .015) 
        Y0$yld[Y0$Z.sim == "YFF"] <- Y0$yld[Y0$Z.sim == "YFF"] * ( 1 + .075) 
        Y0$yld[Y0$Z.sim == "YFB"] <- Y0$yld[Y0$Z.sim == "YFB"] * ( 1 + .125) 
        Y0$yld[Y0$Z.sim == "YBM"] <- Y0$yld[Y0$Z.sim == "YBM"] * ( 1 + .15) 
        Y0$yld[Y0$Z.sim == "YBF"] <- Y0$yld[Y0$Z.sim == "YBF"] * ( 1 + .125) 
      Y0$yld[Y0$Z.sim == "YBB"] <- Y0$yld[Y0$Z.sim == "YBB"] * ( 1 + .20) 
Y0$loc <- as.factor(as.numeric(Y0$district)*100 + Y0$ea)
## tests
    fit.allvsC.sim <-lm(yld ~ (Z.sim=="Ctrl") + loc , data=Y0)

    fit.matchvsnomatch.sim <- lm(yld ~ (Z.sim=="YMM" | Z.sim=="YWW")  + loc , data=subset(Y0,Z.sim=="YMM" | Z.sim=="YWW"  | Z.sim=="YFM"   | Z.sim=="YMF"))
 fit.Rbothvssingel.sim <- lm(yld ~ (Z.sim=="YBM" | Z.sim == "YBF" | Z.sim == "YBB")  + loc, data=subset(Y0,Z.sim!="Ctrl"))
 fit.Mbothvssingel.sim <- lm(yld ~ (Z.sim=="YMB" | Z.sim == "YFB" | Z.sim == "YBB")  + loc, data=subset(Y0,Z.sim!="Ctrl"))

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


  power.allvsC[j] <- mean(p.allvsC <= alpha )
  power.match[j] <- mean(p.matchsvnomatch <= alpha)
power.Rbothvssingel[j] <-   mean(p.Rbothvssingel <= alpha/10)
power.Mbothvssingel[j] <-   mean(p.Mbothvssingel <= alpha/10)
power.all[j] <-  mean(p.allvsC <= alpha/10 & p.matchsvnomatch <= alpha/10 & p.Rbothvssingel <= alpha/10 & p.Mbothvssingel <= alpha/10)
  print(j)
}
save2 <- data.frame(possible.ns,power.match)
pdf("/home/bjvca/data/projects/digital green/pre-analysis plan/power2.pdf")
plot(possible.ns, power.match, ylim=c(0,1), type = "l", lwd=3, xlab = "sample size", ylab="power")
abline(h = .8, lty=2, lwd=3)
dev.off()

### third hypothesis





library(foreign)

##### new: base on maize from UNHS2005/06
loc <- read.dta("/home/bjvca/data/data/UGANDA/UNHS/UNHS_2005/UNHS_III_AGRIC_DATA/asec1b.dta")
area <- read.dta("/home/bjvca/data/data/UGANDA/UNHS/UNHS_2005/UNHS_III_AGRIC_DATA/asec4a.dta")
prod <- read.dta("/home/bjvca/data/data/UGANDA/UNHS/UNHS_2005/UNHS_III_AGRIC_DATA/asec7a.dta")

yield <- merge(area, prod, by.x=c("hh","a4aid","a4aq5b"), by.y=c("hh", "a7aq1","a7aq2b"))
#keep only maize
yield <- subset(yield,a4aq5b=="130")
#merge in location
yield <- merge(yield,loc, by="hh")
#keep only maize growing in eastern lowlands
yield <- subset(yield, district %in% c("iganga", "jinja","kamuli","bugiri","mayuge"))

yield$yld <- yield$a7aq3b*yield$a7aq3d/yield$a4aq3
yield$yld <- yield$yld*2.47105
yield <- subset(yield, yld > 100 & yld < 2000) 

###



power.atleastone <- rep(NA, length(possible.ns))
power.bothtreatments <- rep(NA, length(possible.ns))
power.fullranking <- rep(NA, length(possible.ns))
power.all <- rep(NA, length(possible.ns))
 power.allvsC <- rep(NA, length(possible.ns))
  power.match <- rep(NA, length(possible.ns))
power.Rbothvssingel <- rep(NA, length(possible.ns))
power.Mbothvssingel <- rep(NA, length(possible.ns))
  power.yield <- rep(NA, length(possible.ns))
  power.gap <- rep(NA, length(possible.ns))

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
p.gap1 <- rep(NA, sims)
p.gap2 <- rep(NA, sims)
p.gap3 <- rep(NA, sims)

  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
Y0 <-  yield[sample(1:dim(yield)[1],N, replace=T),]
 #   Y0 <-   rnorm(n=N, mean=1732, sd=920) ### we should sample (with replacement from real data, eg pasic yield data) 

Z.sim <- sample(as.factor(c(rep("YMM", N/2/6),rep("YMF", N/2/6),rep("YMB", N/2/6), rep("YFM", N/2/6), rep("YFF", N/2/6), rep("YFB", N/2/6), rep("YBM", N/2/3), rep("YBF", N/2/3), rep("YBB", N/2/3)))) 
Y0$Z.sim <- as.factor(c(as.character(Z.sim),rep(NA,N - length(Z.sim))))
Y0 <- subset(Y0, !is.na(Z.sim))

#    Y0$Z.sim <- complete_ra(N=N, num_arms=10)  ### Need to incorporate blocking here
#levels(Y0$Z.sim) <- c("YMM","YMF","YMB","YFB","YFF","YFB","YBM","YBF","YBB", "Ctrl")
### model effects
    tau <- seq(0,.3,length.out=10) ### seven different effects

    Y0$yld[Y0$Z.sim == "YMM"] <- Y0$yld[Y0$Z.sim == "YMM"] * ( 1 + .1)
    Y0$yld[Y0$Z.sim == "YMF"] <- Y0$yld[Y0$Z.sim == "YMF"] * ( 1 + .05)  
     Y0$yld[Y0$Z.sim == "YMB"] <- Y0$yld[Y0$Z.sim == "YMB"] * ( 1 + .15) 
        Y0$yld[Y0$Z.sim == "YFM"] <- Y0$yld[Y0$Z.sim == "YFM"] * ( 1 + .015) 
        Y0$yld[Y0$Z.sim == "YFF"] <- Y0$yld[Y0$Z.sim == "YFF"] * ( 1 + .075) 
        Y0$yld[Y0$Z.sim == "YFB"] <- Y0$yld[Y0$Z.sim == "YFB"] * ( 1 + .125) 
        Y0$yld[Y0$Z.sim == "YBM"] <- Y0$yld[Y0$Z.sim == "YBM"] * ( 1 + .15) 
        Y0$yld[Y0$Z.sim == "YBF"] <- Y0$yld[Y0$Z.sim == "YBF"] * ( 1 + .125) 
      Y0$yld[Y0$Z.sim == "YBB"] <- Y0$yld[Y0$Z.sim == "YBB"] * ( 1 + .20) 
Y0$loc <- as.factor(as.numeric(Y0$district)*100 + Y0$ea)
## tests
    fit.allvsC.sim <-lm(yld ~ (Z.sim=="Ctrl") + loc , data=Y0)

    fit.matchvsnomatch.sim <- lm(yld ~ (Z.sim=="YMM" | Z.sim=="YWW" )  + loc , data=subset(Y0,Z.sim=="YMM" | Z.sim=="YWW"  | Z.sim=="YFM"   | Z.sim=="YMF" ))
 fit.Rbothvssingel.sim <- lm(yld ~ (Z.sim=="YBM" | Z.sim == "YBF" | Z.sim == "YBB")  + loc, data=subset(Y0,Z.sim!="Ctrl"))
 fit.Mbothvssingel.sim <- lm(yld ~ (Z.sim=="YMB" | Z.sim == "YFB" | Z.sim == "YBB")  + loc, data=subset(Y0,Z.sim!="Ctrl"))

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


  power.allvsC[j] <- mean(p.allvsC <= alpha )
  power.match[j] <- mean(p.matchsvnomatch <= alpha)
power.Rbothvssingel[j] <-   mean(p.Rbothvssingel <= alpha)
power.Mbothvssingel[j] <-   mean(p.Mbothvssingel <= alpha)
power.all[j] <-  mean(p.allvsC <= alpha/10 & p.matchsvnomatch <= alpha/10 & p.Rbothvssingel <= alpha/10 & p.Mbothvssingel <= alpha/10)
  print(j)
}
save3 <- data.frame(possible.ns,power.Rbothvssingel)
pdf("/home/bjvca/data/projects/digital green/pre-analysis plan/power3.pdf")
plot(possible.ns, power.Rbothvssingel, ylim=c(0,1), type = "l", lwd=3, xlab = "sample size", ylab="power")
abline(h = .8, lty=2, lwd=3)
dev.off()



### fourth hypothesis





library(foreign)

##### new: base on maize from UNHS2005/06
loc <- read.dta("/home/bjvca/data/data/UGANDA/UNHS/UNHS_2005/UNHS_III_AGRIC_DATA/asec1b.dta")
area <- read.dta("/home/bjvca/data/data/UGANDA/UNHS/UNHS_2005/UNHS_III_AGRIC_DATA/asec4a.dta")
prod <- read.dta("/home/bjvca/data/data/UGANDA/UNHS/UNHS_2005/UNHS_III_AGRIC_DATA/asec7a.dta")

yield <- merge(area, prod, by.x=c("hh","a4aid","a4aq5b"), by.y=c("hh", "a7aq1","a7aq2b"))
#keep only maize
yield <- subset(yield,a4aq5b=="130")
#merge in location
yield <- merge(yield,loc, by="hh")
#keep only maize growing in eastern lowlands
yield <- subset(yield, district %in% c("iganga", "jinja","kamuli","bugiri","mayuge"))

yield$yld <- yield$a7aq3b*yield$a7aq3d/yield$a4aq3
yield$yld <- yield$yld*2.47105
yield <- subset(yield, yld > 100 & yld < 2000) 

###



power.atleastone <- rep(NA, length(possible.ns))
power.bothtreatments <- rep(NA, length(possible.ns))
power.fullranking <- rep(NA, length(possible.ns))
power.all <- rep(NA, length(possible.ns))
 power.allvsC <- rep(NA, length(possible.ns))
  power.match <- rep(NA, length(possible.ns))
power.Rbothvssingel <- rep(NA, length(possible.ns))
power.Mbothvssingel <- rep(NA, length(possible.ns))
  power.yield <- rep(NA, length(possible.ns))
  power.gap <- rep(NA, length(possible.ns))

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
p.gap1 <- rep(NA, sims)
p.gap2 <- rep(NA, sims)
p.gap3 <- rep(NA, sims)

  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
Y0 <-  yield[sample(1:dim(yield)[1],N, replace=T),]
 #   Y0 <-   rnorm(n=N, mean=1732, sd=920) ### we should sample (with replacement from real data, eg pasic yield data) 

Z.sim <- sample(as.factor(c(rep("YMM", N/2/6),rep("YMF", N/2/6),rep("YMB", N/2/3), rep("YFM", N/2/6), rep("YFF", N/2/6), rep("YFB", N/2/3), rep("YBM", N/2/6), rep("YBF", N/2/6), rep("YBB", N/2/3)))) 
Y0$Z.sim <- as.factor(c(as.character(Z.sim),rep(NA,N - length(Z.sim))))
Y0 <- subset(Y0, !is.na(Z.sim))

#    Y0$Z.sim <- complete_ra(N=N, num_arms=10)  ### Need to incorporate blocking here
#levels(Y0$Z.sim) <- c("YMM","YMF","YMB","YFB","YFF","YFB","YBM","YBF","YBB", "Ctrl")
### model effects
    tau <- seq(0,.3,length.out=10) ### seven different effects

    Y0$yld[Y0$Z.sim == "YMM"] <- Y0$yld[Y0$Z.sim == "YMM"] * ( 1 + .1)
    Y0$yld[Y0$Z.sim == "YMF"] <- Y0$yld[Y0$Z.sim == "YMF"] * ( 1 + .05)  
     Y0$yld[Y0$Z.sim == "YMB"] <- Y0$yld[Y0$Z.sim == "YMB"] * ( 1 + .15) 
        Y0$yld[Y0$Z.sim == "YFM"] <- Y0$yld[Y0$Z.sim == "YFM"] * ( 1 + .015) 
        Y0$yld[Y0$Z.sim == "YFF"] <- Y0$yld[Y0$Z.sim == "YFF"] * ( 1 + .075) 
        Y0$yld[Y0$Z.sim == "YFB"] <- Y0$yld[Y0$Z.sim == "YFB"] * ( 1 + .125) 
        Y0$yld[Y0$Z.sim == "YBM"] <- Y0$yld[Y0$Z.sim == "YBM"] * ( 1 + .15) 
        Y0$yld[Y0$Z.sim == "YBF"] <- Y0$yld[Y0$Z.sim == "YBF"] * ( 1 + .125) 
      Y0$yld[Y0$Z.sim == "YBB"] <- Y0$yld[Y0$Z.sim == "YBB"] * ( 1 + .20) 
Y0$loc <- as.factor(as.numeric(Y0$district)*100 + Y0$ea)
## tests
    fit.allvsC.sim <-lm(yld ~ (Z.sim=="Ctrl") + loc , data=Y0)

    fit.matchvsnomatch.sim <- lm(yld ~ (Z.sim=="YMM" | Z.sim=="YWW" )  + loc , data=subset(Y0,Z.sim=="YMM" | Z.sim=="YWW"  | Z.sim=="YFM"   | Z.sim=="YMF" ))
 fit.Rbothvssingel.sim <- lm(yld ~ (Z.sim=="YBM" | Z.sim == "YBF" | Z.sim == "YBB")  + loc, data=subset(Y0,Z.sim!="Ctrl"))
 fit.Mbothvssingel.sim <- lm(yld ~ (Z.sim=="YMB" | Z.sim == "YFB" | Z.sim == "YBB")  + loc, data=subset(Y0,Z.sim!="Ctrl"))

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


  power.allvsC[j] <- mean(p.allvsC <= alpha )
  power.match[j] <- mean(p.matchsvnomatch <= alpha)
power.Rbothvssingel[j] <-   mean(p.Rbothvssingel <= alpha)
power.Mbothvssingel[j] <-   mean(p.Mbothvssingel <= alpha)
power.all[j] <-  mean(p.allvsC <= alpha/10 & p.matchsvnomatch <= alpha/10 & p.Rbothvssingel <= alpha/10 & p.Mbothvssingel <= alpha/10)
  print(j)
}
save4 <- data.frame(possible.ns,power.Mbothvssingel)
pdf("/home/bjvca/data/projects/digital green/pre-analysis plan/power4.pdf")
plot(possible.ns, power.Mbothvssingel, ylim=c(0,1), type = "l", lwd=3, xlab = "sample size", ylab="power")
abline(h = .8, lty=2, lwd=3)
dev.off()
pdf("/home/bjvca/data/projects/digital green/pre-analysis plan/all.pdf")
par(mfrow=c(2,2))

plot(save1$possible.ns, save1$power.allvsC, ylim=c(0,1), type = "l", lwd=3, xlab = "sample size", ylab="power")
abline(h = .8, lty=2, lwd=3)
plot(save2$possible.ns, save2$power.match, ylim=c(0,1), type = "l", lwd=3, xlab = "sample size", ylab="power")
abline(h = .8, lty=2, lwd=3)
plot(save3$possible.ns, save3$power.Rbothvssingel, ylim=c(0,1), type = "l", lwd=3, xlab = "sample size", ylab="power")
abline(h = .8, lty=2, lwd=3)
plot(save4$possible.ns, save4$power.Mbothvssingel, ylim=c(0,1), type = "l", lwd=3, xlab = "sample size", ylab="power")
abline(h = .8, lty=2, lwd=3)
dev.off()

############################################################################ here power calculations for gender productivity gap

rm(list=ls())
library(foreign)

possible.ns <- seq(from=100, to=1500, by=50)
alpha <- 0.05 
sims <- 200
power.gap1 <- rep(NA, length(possible.ns))

#### Outer loop to vary the number of subjects ####
for (j in 1:length(possible.ns)){
  N <- possible.ns[j]
 
p.gap1 <- rep(NA, sims)


  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
Y1 <- data.frame(rnorm(n=N, mean=42, sd=39.64))
names(Y1) <- "gap"
  


Z.sim <- sample(as.factor(c(rep("YMM", N/6),rep("YMF", N/6),rep("YMB", N/6), rep("YFM", N/6), rep("YFF", N/6), rep("YFB", N/6)))) 
Y1$Z.sim <- as.factor(c(as.character(Z.sim),rep(NA,N - length(Z.sim))))
Y1 <- subset(Y1, !is.na(Z.sim))

### model effects
    Y1$gap[Y1$Z.sim == "YMM"] <- Y1$gap[Y1$Z.sim == "YMM"] * ( 1 + .15)
    Y1$gap[Y1$Z.sim == "YMF"] <- Y1$gap[Y1$Z.sim == "YMF"] * ( 1 + .1)  
     Y1$gap[Y1$Z.sim == "YMB"] <- Y1$gap[Y1$Z.sim == "YMB"] * ( 1 + .025) 
        Y1$gap[Y1$Z.sim == "YFM"] <- Y1$gap[Y1$Z.sim == "YFM"] * ( 1 - .1) 
        Y1$gap[Y1$Z.sim == "YFF"] <- Y1$gap[Y1$Z.sim == "YFF"] * ( 1 - .20) 
        Y1$gap[Y1$Z.sim == "YFB"] <- Y1$gap[Y1$Z.sim == "YFB"] * ( 1 - .15) 
        Y1$gap[Y1$Z.sim == "YBM"] <- Y1$gap[Y1$Z.sim == "YBM"] * ( 1 + .025) 
        Y1$gap[Y1$Z.sim == "YBF"] <- Y1$gap[Y1$Z.sim == "YBF"] * ( 1 - .025) 
      Y1$gap[Y1$Z.sim == "YBB"] <- Y1$gap[Y1$Z.sim == "YBB"] * ( 1 - .33) 




## tests
    fit.gap1.sim <-lm(gap ~ (Z.sim=="YMM" | Z.sim=="YMF" | Z.sim=="YMB") , data=Y1)



    p.gap1[i] <- summary(fit.gap1.sim)$coefficients[2,4]

    

  }


  power.gap1[j] <- mean(p.gap1 <= alpha )
  print(j)
}

save6 <- data.frame(possible.ns,power.gap1)
pdf("/home/bjvca/data/projects/digital green/pre-analysis plan/power5.pdf")
plot(possible.ns, power.gap1, ylim=c(0,1), type = "l", lwd=3, xlab = "sample size", ylab="power")

abline(h = .8, lty=2, lwd=3)
dev.off()
##hyp 2



power.gap1 <- rep(NA, length(possible.ns))

#### Outer loop to vary the number of subjects ####
for (j in 1:length(possible.ns)){
  N <- possible.ns[j]
 
p.gap1 <- rep(NA, sims)


  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
Y1 <- data.frame(rnorm(n=N, mean=42, sd=39.64))
names(Y1) <- "gap"
  


Z.sim <- sample(as.factor(c(rep("YMM", N/2/6),rep("YMF", N/2/6),rep("YMB", N/2/3), rep("YFM", N/2/6), rep("YFF", N/2/6), rep("YFB", N/2/3), rep("YBM", N/2/6), rep("YBF", N/2/6), rep("YBB", N/2/3) ))) 
Y1$Z.sim <- as.factor(c(as.character(Z.sim),rep(NA,N - length(Z.sim))))
Y1 <- subset(Y1, !is.na(Z.sim))

### model effects
    Y1$gap[Y1$Z.sim == "YMM"] <- Y1$gap[Y1$Z.sim == "YMM"] * ( 1 + .15)
    Y1$gap[Y1$Z.sim == "YMF"] <- Y1$gap[Y1$Z.sim == "YMF"] * ( 1 + .1)  
     Y1$gap[Y1$Z.sim == "YMB"] <- Y1$gap[Y1$Z.sim == "YMB"] * ( 1 + .025) 
        Y1$gap[Y1$Z.sim == "YFM"] <- Y1$gap[Y1$Z.sim == "YFM"] * ( 1 - .1) 
        Y1$gap[Y1$Z.sim == "YFF"] <- Y1$gap[Y1$Z.sim == "YFF"] * ( 1 - .20) 
        Y1$gap[Y1$Z.sim == "YFB"] <- Y1$gap[Y1$Z.sim == "YFB"] * ( 1 - .15) 
        Y1$gap[Y1$Z.sim == "YBM"] <- Y1$gap[Y1$Z.sim == "YBM"] * ( 1 + .025) 
        Y1$gap[Y1$Z.sim == "YBF"] <- Y1$gap[Y1$Z.sim == "YBF"] * ( 1 - .025) 
      Y1$gap[Y1$Z.sim == "YBB"] <- Y1$gap[Y1$Z.sim == "YBB"] * ( 1 - .33) 




## tests
    fit.gap1.sim <-lm(gap ~ (Z.sim=="YMB" | Z.sim=="YFB" | Z.sim=="YBB") , data=Y1)



    p.gap1[i] <- summary(fit.gap1.sim)$coefficients[2,4]

    

  }


  power.gap1[j] <- mean(p.gap1 <= alpha )
  print(j)
}

save7 <- data.frame(possible.ns,power.gap1)
pdf("/home/bjvca/data/projects/digital green/pre-analysis plan/power6.pdf")
plot(possible.ns, power.gap1, ylim=c(0,1), type = "l", lwd=3, xlab = "sample size", ylab="power")

abline(h = .8, lty=2, lwd=3)
dev.off()

################################ 
power.gap1 <- rep(NA, length(possible.ns))

#### Outer loop to vary the number of subjects ####
for (j in 1:length(possible.ns)){
  N <- possible.ns[j]
 
p.gap1 <- rep(NA, sims)


  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
Y1 <- data.frame(rnorm(n=N, mean=42, sd=39.64))
names(Y1) <- "gap"
  


Z.sim <- sample(as.factor(c(rep("YMM", N/2/9),rep("YMF", N/2/9),rep("YMB", N/2/9), rep("YFM", N/2/9), rep("YFF", N/2/9), rep("YFB", N/2/9), rep("YBM", N/2/9), rep("YBF", N/2/9), rep("YBB", N/2) ))) 
Y1$Z.sim <- as.factor(c(as.character(Z.sim),rep(NA,N - length(Z.sim))))
Y1 <- subset(Y1, !is.na(Z.sim))

### model effects
    Y1$gap[Y1$Z.sim == "YMM"] <- Y1$gap[Y1$Z.sim == "YMM"] * ( 1 + .15)
    Y1$gap[Y1$Z.sim == "YMF"] <- Y1$gap[Y1$Z.sim == "YMF"] * ( 1 + .1)  
     Y1$gap[Y1$Z.sim == "YMB"] <- Y1$gap[Y1$Z.sim == "YMB"] * ( 1 + .025) 
        Y1$gap[Y1$Z.sim == "YFM"] <- Y1$gap[Y1$Z.sim == "YFM"] * ( 1 - .1) 
        Y1$gap[Y1$Z.sim == "YFF"] <- Y1$gap[Y1$Z.sim == "YFF"] * ( 1 - .20) 
        Y1$gap[Y1$Z.sim == "YFB"] <- Y1$gap[Y1$Z.sim == "YFB"] * ( 1 - .15) 
        Y1$gap[Y1$Z.sim == "YBM"] <- Y1$gap[Y1$Z.sim == "YBM"] * ( 1 + .025) 
        Y1$gap[Y1$Z.sim == "YBF"] <- Y1$gap[Y1$Z.sim == "YBF"] * ( 1 - .025) 
      Y1$gap[Y1$Z.sim == "YBB"] <- Y1$gap[Y1$Z.sim == "YBB"] * ( 1 - .33) 




## tests
    fit.gap1.sim <-lm(gap ~ (Z.sim=="YBB") , data=Y1)



    p.gap1[i] <- summary(fit.gap1.sim)$coefficients[2,4]

    

  }


  power.gap1[j] <- mean(p.gap1 <= alpha )
  print(j)
}

save8 <- data.frame(possible.ns,power.gap1)
pdf("/home/bjvca/data/projects/digital green/pre-analysis plan/power7.pdf")
plot(possible.ns, power.gap1, ylim=c(0,1), type = "l", lwd=3, xlab = "sample size", ylab="power")

abline(h = .8, lty=2, lwd=3)
dev.off()

#### compare all to YMM

################################ 
power.gap1 <- rep(NA, length(possible.ns))

#### Outer loop to vary the number of subjects ####
for (j in 1:length(possible.ns)){
  N <- possible.ns[j]
 
p.gap1 <- rep(NA, sims)


  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
Y1 <- data.frame(rnorm(n=N, mean=42, sd=39.64))
names(Y1) <- "gap"
  


Z.sim <- sample(as.factor(c(rep("YMM", N/2),rep("YMF", N/2/9),rep("YMB", N/2/9), rep("YFM", N/2/9), rep("YFF", N/2/9), rep("YFB", N/2/9), rep("YBM", N/2/9), rep("YBF", N/2/9), rep("YBB", N/2/9) ))) 
Y1$Z.sim <- as.factor(c(as.character(Z.sim),rep(NA,N - length(Z.sim))))
Y1 <- subset(Y1, !is.na(Z.sim))

### model effects
    Y1$gap[Y1$Z.sim == "YMM"] <- Y1$gap[Y1$Z.sim == "YMM"] * ( 1 + .15)
    Y1$gap[Y1$Z.sim == "YMF"] <- Y1$gap[Y1$Z.sim == "YMF"] * ( 1 + .1)  
     Y1$gap[Y1$Z.sim == "YMB"] <- Y1$gap[Y1$Z.sim == "YMB"] * ( 1 + .025) 
        Y1$gap[Y1$Z.sim == "YFM"] <- Y1$gap[Y1$Z.sim == "YFM"] * ( 1 - .1) 
        Y1$gap[Y1$Z.sim == "YFF"] <- Y1$gap[Y1$Z.sim == "YFF"] * ( 1 - .20) 
        Y1$gap[Y1$Z.sim == "YFB"] <- Y1$gap[Y1$Z.sim == "YFB"] * ( 1 - .15) 
        Y1$gap[Y1$Z.sim == "YBM"] <- Y1$gap[Y1$Z.sim == "YBM"] * ( 1 + .025) 
        Y1$gap[Y1$Z.sim == "YBF"] <- Y1$gap[Y1$Z.sim == "YBF"] * ( 1 - .025) 
      Y1$gap[Y1$Z.sim == "YBB"] <- Y1$gap[Y1$Z.sim == "YBB"] * ( 1 - .33) 




## tests
    fit.gap1.sim <-lm(gap ~ (Z.sim=="YMM") , data=Y1)



    p.gap1[i] <- summary(fit.gap1.sim)$coefficients[2,4]

    

  }


  power.gap1[j] <- mean(p.gap1 <= alpha )
  print(j)
}

save5 <- data.frame(possible.ns,power.gap1)
pdf("/home/bjvca/data/projects/digital green/pre-analysis plan/power7.pdf")
plot(possible.ns, power.gap1, ylim=c(0,1), type = "l", lwd=3, xlab = "sample size", ylab="power")

abline(h = .8, lty=2, lwd=3)
dev.off()

pdf("/home/bjvca/data/projects/digital green/pre-analysis plan/all_gap.pdf")
par(mfrow=c(2,2))

plot(save5$possible.ns, save5$power.gap1, ylim=c(0,1), type = "l", lwd=3, xlab = "sample size", ylab="power")
abline(h = .8, lty=2, lwd=3)

plot(save6$possible.ns, save6$power.gap1, ylim=c(0,1), type = "l", lwd=3, xlab = "sample size", ylab="power")
abline(h = .8, lty=2, lwd=3)
plot(save7$possible.ns, save7$power.gap1, ylim=c(0,1), type = "l", lwd=3, xlab = "sample size", ylab="power")
abline(h = .8, lty=2, lwd=3)
plot(save8$possible.ns, save8$power.gap1, ylim=c(0,1), type = "l", lwd=3, xlab = "sample size", ylab="power")
abline(h = .8, lty=2, lwd=3)
dev.off()

