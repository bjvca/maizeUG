### this is a program to run the power caluclations for the UGmaize study to be included in the pre-analysis plan
### it is based on Alexander Coppock's example code that goes with: http://egap.org/methods-guides/10-things-you-need-know-about-statistical-power

rm(list=ls())



plot_maize <- read.csv("maize_yields_pwr.csv")
plot_maize$yield <- plot_maize$yield*2.47105
plot_maize <- subset(plot_maize, yield > 250 & yield < 4000) 



possible.ns <- seq(from=2000, to=4000, by=100)

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
p.gap1 <- rep(NA, sims)
p.gap2 <- rep(NA, sims)
p.gap3 <- rep(NA, sims)

  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
Y0 <-  plot_maize[sample(1:dim(plot_maize)[1],N, replace=T),]
Y1 <- data.frame(rnorm(n=N, mean=42, sd=39.64))
names(Y1) <- "gap"

g1 <- .5
g2 <- .1
g3 <-  .3
g4 <- 0
g5 <- 0
g6 <- .1

Y0$Z.sim <- sample(as.factor(c(rep("Ctrl",g6*N), rep("YMFB", g5*N),rep("YBMF", g4*N),rep("YMF", g3*N), rep("YMMFF", g2*N), rep("YBB", g1*N))))
Y1$Z.sim <- sample(as.factor(c(rep("Ctrl",g6*N), rep("Y1MMMF", N*g2/2 + N*g3/2),rep("Y1FMFF", N*g3/2 + N*g2/2),rep("Y1BMBF", N*g4), rep("Y1MBFB", N*g5), rep("Y1BB", N*g1))))
    
 

### model effects

    Y0$yield[Y0$Z.sim == "YMMFF"] <- Y0$yield[Y0$Z.sim == "YMMFF"] * ( 1 + .075)
    Y0$yield[Y0$Z.sim == "YMF"] <- Y0$yield[Y0$Z.sim == "YMF"] * ( 1 + 0 )  
     Y0$yield[Y0$Z.sim == "YBMF"] <- Y0$yield[Y0$Z.sim == "YBMF"] * ( 1 + .1) 
       Y0$yield[Y0$Z.sim == "YMFB"] <- Y0$yield[Y0$Z.sim == "YMFB"] * ( 1 + .21) 
      Y0$yield[Y0$Z.sim == "YBB"] <- Y0$yield[Y0$Z.sim == "YBB"] * ( 1 + .25) 
 
Y1$gap[Y1$Z.sim == "Y1MMMF"] <- Y1$gap[Y1$Z.sim == "Y1MMMF"]* (1 + .1) 
Y1$gap[Y1$Z.sim == "Y1FMFF"] <- Y1$gap[Y1$Z.sim == "Y1FMFF"]* (1 - .1) 
     Y1$gap[Y1$Z.sim == "Y1BMBF"] <- Y1$gap[Y1$Z.sim == "Y1BMBF"] * ( 1 + 0) 
       Y1$gap[Y1$Z.sim == "Y1MBFB"] <- Y1$gap[Y1$Z.sim == "Y1MBFB"] * ( 1 -.1) 
Y1$gap[Y1$Z.sim == "Y1BB"] <- Y1$gap[Y1$Z.sim == "Y1BB"]* (1 - .5)


## tests
    fit.allvsC.sim <-lm(yield ~ (Z.sim=="Ctrl") + as.factor(region), data=Y0)

fit.matchvsnomatch.sim <- lm(yield ~ (Z.sim=="YMMFF" | Z.sim == "YBB" ) + as.factor(region), data=subset(Y0, Z.sim=="YMMFF"  | Z.sim=="YMF" | Z.sim == "YBB"))
fit.Rbothvssingel.sim <- lm(yield ~ (Z.sim=="YBMF" | Z.sim == "YBB")+ as.factor(region), data=subset(Y0,Z.sim!="Ctrl"))
fit.Mbothvssingel.sim <- lm(yield ~ (Z.sim=="YMFB" | Z.sim == "YBB")+ as.factor(region), data=subset(Y0,Z.sim!="Ctrl"))

fit1.Gap <-  lm(gap ~ (Z.sim=="Y1MMMF"  ), data=subset(Y1, Z.sim=="Y1MMMF"  | Z.sim=="Y1FMFF" ))
fit2.Gap <-  lm(gap ~ (Z.sim=="Y1MBFB" | Z.sim=="Y1BB" ), data=subset(Y1, Z.sim!="Ctrl" ))
fit3.Gap <-  lm(gap ~ (Z.sim=="Y1BB" ), data=Y1 )



    ### Need to capture coefficients and pvalues (one-tailed tests, so signs are important)

    p.allvsC[i] <- summary(fit.allvsC.sim)$coefficients[2,4]
    p.matchsvnomatch[i] <- summary(fit.matchvsnomatch.sim)$coefficients[2,4]
	p.Rbothvssingel[i] <- summary(fit.Rbothvssingel.sim)$coefficients[2,4]
	p.Mbothvssingel[i] <- summary(fit.Mbothvssingel.sim)$coefficients[2,4]

p.gap1[i] <- summary(fit1.Gap)$coefficients[2,4]
p.gap2[i] <- summary(fit2.Gap)$coefficients[2,4]
p.gap3[i] <- summary(fit3.Gap)$coefficients[2,4]
    
    

  }


  power.allvsC[j] <- mean(p.allvsC <= alpha/10 )
  power.match[j] <- mean(p.matchsvnomatch <= alpha/10)
power.Rbothvssingel[j] <-   mean(p.Rbothvssingel <= alpha/10)
power.Mbothvssingel[j] <-   mean(p.Mbothvssingel <= alpha/10)
power.yield[j] <-  mean(p.allvsC <= alpha/10 & p.matchsvnomatch <= alpha/10 & p.Rbothvssingel <= alpha/10 & p.Mbothvssingel <= alpha/10)

power.gap[j] <-  mean(p.gap1 <= alpha/10 & p.gap2 <= alpha/10 & p.gap3 <= alpha/10)

power.all[j] <-  mean(p.allvsC <= alpha/10 & p.matchsvnomatch <= alpha/10 & p.Rbothvssingel <= alpha/10 & p.Mbothvssingel <= alpha/10 & p.gap1 <= alpha/10 & p.gap2 <= alpha/10 & p.gap3 <= alpha/10 )
  print(j)
}
pdf("/home/bjvca/data/projects/digital green/pre-analysis plan/power_gruped.pdf")
plot(possible.ns, power.yield, ylim=c(0,1), type = "l", lwd=3, xlab = "sample size", ylab="power")
lines(possible.ns, power.gap, col="green", lwd=3)
abline(h = .8, lty=2, lwd=3)

lines(possible.ns, power.all, col="blue" , lwd=3)
#lines(possible.ns, power.Mbothvssingel, col="yellow" , lwd=3)
#lines(possible.ns, power.all, col="red", lwd=3)
dev.off()








