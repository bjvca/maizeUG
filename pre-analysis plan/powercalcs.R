### this is a program to run the power caluclations for the UGmaize study to be included in the pre-analysis plan
### it is based on Alexander Coppock's example code that goes with: http://egap.org/methods-guides/10-things-you-need-know-about-statistical-power

### to run this on amamzon EC
ssh -i "bjornkey.pem" ubuntu@34.250.218.247

sudo apt-get update
sudo apt-get install r-base-core


sudo R
install.packages("foreach")
install.packages("doParallel")

wget https://www.dropbox.com/s/jdm4j7rbqpa33ae/productivity.csv?dl=0
mv productivity.csv?dl=0 productivity.csv

wget https://www.dropbox.com/s/h04651jb0zrk1os/loc_parish.csv?dl=0
mv loc_parish.csv?dl=0 loc_parish.csv

wget https://www.dropbox.com/s/ozv4e1cboecql15/maize_yields_pwr.csv?dl=0
mv maize_yields_pwr.csv?dl=0 maize_yields_pwr.csv


#######################3 

rm(list=ls())
library(foreign)
library(foreach)
library(doParallel)
registerDoParallel(cores=detectCores(all.tests = FALSE, logical = TRUE))

alpha <- .05
N <- 1000
sims <- 250
minsamp <- 100
bystep <- 50
res_all <- matrix(NA,1,13)
ptm <- proc.time()

### prepare yield data (taken from the pasic rice study)
prod <- read.csv("productivity.csv")
prod_rice <- subset(prod,crop == "rice")
## this is plot level, take averages by hhid
plot_rice <- aggregate(prod_rice$prod,list(prod_rice$hhid), mean, na.rm=T)
names(plot_rice) <- c("hhid","prod")
### some data cleaning
plot_rice <- subset(plot_rice, prod > 200 & prod <4000)


### merge in location data to bloc on village and/or parish level
loc <- read.csv("loc_parish.csv")
plot_rice <- merge(loc,plot_rice, by="hhid")
summary(lm(prod~as.factor(parish),data=plot_rice))

plot_maize <- read.csv("maize_yields_pwr.csv")
plot_maize$yield <- plot_maize$yield*2.47105
plot_maize <- subset(plot_maize, yield > 250 & yield < 4000) 


summary(lm(yield~as.factor(region),data=plot_maize))

possible.n2 <- seq(minsamp,N-5*minsamp, by=bystep)


#res <- foreach (j1 = 1:(length(possible.n2)) ) %dopar% {
for (j1 in 1:(length(possible.n2)))  {

#r <- foreach (t = 1:(length(possible.n2)-j -2)) %dopar% {
for (j2 in 1:(length(possible.n2) - (j1-1)) ) {
for (j3 in 1:(length(possible.n2) - (j2 -1) - (j1 - 1)) ) {

#res <- foreach(j3 = 1:(length(possible.n2) - (j2 -1) - (j1 - 1)), .combine=rbind) %dopar% {

for (j4 in 1:(length(possible.n2) - (j3 -1) - (j2 -1) - (j1 - 1)) ) {
res <- foreach(j5 = 1:(length(possible.n2)- (j4 -1) - (j3 -1) - (j2 -1) - (j1 - 1)), .combine=rbind) %dopar% {

#for (j5 in 1:(length(possible.n2) - (j4 -1) - (j3 -1) - (j2 -1) - (j1 - 1)) ) {



g1 <- possible.n2[j1]
g2 <- possible.n2[j2]
g3 <- possible.n2[j3]
g4 <- possible.n2[j4]
g5 <- possible.n2[j5]


Z.sim <- sample(as.factor(c(rep("Ctrl",floor(N-g1-g2-g3-g4-g5)), rep("YMFB", g5),rep("YBMF", g4),rep("YMF", g3), rep("YMMFF", g2), rep("YBB", g1))))
Z1.sim <- sample(as.factor(c(rep("Ctrl",floor(N-g1-g2-g3-g4-g5)), rep("Y1MMMF", g2/2 + g3/2),rep("Y1FMFF", g3/2 + g2/2),rep("Y1BMBF", g4), rep("Y1MBFB", g5), rep("Y1BB", g1))))


  p.T1vsC <- rep(NA, sims)
  p.T2vsC <- rep(NA, sims)
  p.T2vsT1 <- rep(NA, sims)

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
 #   Y0 <-   rnorm(n=N, mean=1732, sd=920) ### we should sample (with replacement from real data, eg pasic yield data) 
Y0$Z.sim <- Z.sim
Y1$Z.sim <- Z1.sim
    

### model effects

    Y0$yield[Y0$Z.sim == "YMMFF"] <- Y0$yield[Y0$Z.sim == "YMMFF"] * ( 1 + .075)
    Y0$yield[Y0$Z.sim == "YMF"] <- Y0$yield[Y0$Z.sim == "YMF"] * ( 1 + 0 )  
     Y0$yield[Y0$Z.sim == "YBMF"] <- Y0$yield[Y0$Z.sim == "YBMF"] * ( 1 + .1) 
       Y0$yield[Y0$Z.sim == "YMFB"] <- Y0$yield[Y0$Z.sim == "YMFB"] * ( 1 + .21) 
      Y0$yield[Y0$Z.sim == "YBB"] <- Y0$yield[Y0$Z.sim == "YBB"] * ( 1 + .25) 
 
Y1$gap[Y1$Z.sim == "Y1MMMF"] <- Y1$gap[Y1$Z.sim == "Y1MMMF"]* (1 + .2) 
Y1$gap[Y1$Z.sim == "Y1FMFF"] <- Y1$gap[Y1$Z.sim == "Y1FMFF"]* (1 - .2) 
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
#write(c(j1,j2,j3, mean( p.allvsC < alpha )),file="myfile",ncolumns = 4,sep = ";",append=TRUE)
return(c(j1,j2,j3,j4,j5, mean( p.allvsC < alpha/6),mean(p.matchsvnomatch < alpha/6),mean(p.Rbothvssingel< alpha/6) ,mean(p.Mbothvssingel< alpha/6), mean(p.gap1< alpha/6), mean(p.gap2< alpha/6), mean(p.gap3< alpha/6),  mean( p.allvsC < alpha/6  & p.matchsvnomatch < alpha/6  & p.Rbothvssingel < alpha/6  & p.Mbothvssingel < alpha/6  & p.gap1 < alpha/6  & p.gap2  < alpha/6  & p.gap3 < alpha/6  )))
#print(c(j1,j2,j3))

#write(c(j1,j2,j3,j4,j5,j6,j7,j8,j9, mean( p.allvsC < alpha/10 & p.matchsvnomatch < alpha/10  & p.Rbothvssingel < alpha/10  & p.Mbothvssingel < alpha/10 )),file="myfile",ncolumns = 11,sep = ";",append=TRUE)
#}
#}
#}
#}

}
res_all <- rbind(res_all,res)
print(c(j1,j2,j3,j4))
}
}
}
}


proc.time() - ptm

4,2,2 ,1,1
500,300,300,200,200
7,1,2,1,1
400,100,150,100,100 ctrl 150


1073  4  1  3  1  1 0.8466667 0.9566667 0.8066667 0.8066667 0.6666667
1178  5  1  2  1  1 0.9266667 0.9266667 0.7333333 0.7333333 0.6666667
1188  5  1  3  1  1 0.8100000 0.9700000 0.8633333 0.8633333 0.7000000
1243  6  1  2  1  1 0.9433333 0.9266667 0.7800000 0.7800000 0.6900000
1274  7  1  2  1  1 0.8000000 0.9533333 0.8900000 0.8900000 0.7100000
> possible.n2
[1] 100 150 200 250 300 350 400 450 500




### yield gap



library(foreign)

################################################################################ prepare data for UNPS2013/14  ######################################################################
##get fertilizer use - this is at plot level
agsec3A2013 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2013/AGSEC3A.dta")
agsec3B2013 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2013/AGSEC3B.dta")
sum(duplicated(agsec3A2013[c("HHID","plotID")]))
sum(duplicated(agsec3B2013[c("HHID","plotID")]))

##merge in production - this is at crop level
agsec5A2013 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2013/AGSEC5A.dta")
agsec5B2013 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2013/AGSEC5B.dta")
##get quantity at productionID level
agsec5A2013$prod <- agsec5A2013$a5aq6a*agsec5A2013$a5aq6d
agsec5B2013$prod <- agsec5B2013$a5bq6a*agsec5B2013$a5bq6d
##aggregate to product level

prodA2013 <- aggregate(agsec5A2013$prod, list(agsec5A2013$HHID, agsec5A2013$plotID, agsec5A2013$cropID), sum, na.rm=T)
names(prodA2013) <- c("HHID","plotID","cropID","prod")
prodB2013 <- aggregate(agsec5B2013$prod, list(agsec5B2013$HHID, agsec5B2013$plotID, agsec5B2013$cropID), sum, na.rm=T)
names(prodB2013) <- c("HHID","plotID","cropID","prod")
prodA2013$prod[prodA2013$prod > 200000] <- NA
prodB2013$prod[prodB2013$prod > 200000] <- NA

##merge in plot area - this is at crop level
agsec4A2013 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2013/AGSEC4A.dta")
agsec4B2013 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2013/AGSEC4B.dta")
agsec4A2013$a4aq9[agsec4A2013$a4aq8 == "Pure Stand"] <- 100
agsec4B2013$a4bq9[agsec4B2013$a4bq8 == "Pure Stand"] <- 100

##how to handle mixed cropping
areaA2013 <- aggregate(cbind(agsec4A2013$a4aq7,agsec4A2013$a4aq9), list(agsec4A2013$HHID, agsec4A2013$plotID, agsec4A2013$cropID), sum, na.rm=T)
names(areaA2013) <- c("HHID","plotID","cropID","area", "prop")
areaB2013 <- aggregate(cbind(agsec4B2013$a4bq7,agsec4B2013$a4bq9), list(agsec4B2013$HHID, agsec4B2013$plotID, agsec4B2013$cropID), sum, na.rm=T)
names(areaB2013) <- c("HHID","plotID","cropID","area", "prop")
areaA2013$prop[areaA2013$prop>100] <- NA
areaB2013$prop[areaB2013$prop>100] <- NA


yieldA2013 <- merge(areaA2013, prodA2013, by = c("HHID","plotID","cropID"))
yieldB2013 <- merge(areaB2013, prodB2013, by = c("HHID","plotID","cropID"))

allA2013 <- merge(yieldA2013, agsec3A2013[c(1,3,6)])
allB2013 <- merge(yieldB2013, agsec3B2013[c(1,2,6)])
names(allA2013) <- c("HHID", "plotID", "cropID", "area", "prop", "prod", "pid")
names(allB2013) <- c("HHID", "plotID", "cropID", "area", "prop", "prod", "pid")
allA2013$season <- 1
allB2013$season <- 2

all2013 <- rbind(allA2013, allB2013)

all2013$yield <- all2013$prod / (all2013$area * all2013$prop/100)
all2013$yield[all2013$yield > 15000] <- NA 

#merge in gender
gsec2 <- read.dta("/home/bjvca/data/projects/MAFAP/data/UNPS2013/GSEC2.dta")
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

gsec2$HHID <-  substring(gsub("*-","",gsec2$HHID),2)
gsec2$pid <-  gsec2$h2q1
all2013$pid <- as.numeric(as.character(substrRight(all2013$pid,3)))


merged <- merge(gsec2[c("HHID","pid","h2q3")], all2013, by = c("HHID","pid"))

#merged <- subset(merged, cropID == 130)
merged <- subset(merged, yield > 100 & yield < 4000/2.5)
tapply(merged$yield, merged$h2q3 ,mean)
tapply(merged$yield, merged$h2q3 ,sd)
table(merged$h2q3)


