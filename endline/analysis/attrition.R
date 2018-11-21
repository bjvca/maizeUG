rm(list=ls())
library(foreign)
source("/home/bjvca/data/projects/digital green/endline/data/init.R")

### merge with baseline data
baseline <- read.csv("/home/bjvca/data/projects/digital green/endline/data/raw/baseline.csv")
 dta <- merge(baseline, dta, by="hhid", all.x=T)
dta$know_space <-  dta$know_space.y 
dta$messenger <-  dta$messenger.x
dta$recipient <-  dta$recipient.x
dta$femhead <-  dta$femhead.x
### attrition rate is low
sum(is.na(dta$know_space[dta$messenger!="ctrl"]))/length(dta$know_spacedta$messenger!="ctrl"])


### redo balance table for attriter
dta <- subset(dta, is.na(know_space))


dta$know_space <- dta$maizeoptimal_spacing == "a"
dta$know_small <- dta$maizeq22 == "c"
dta$know_weed <- dta$maizeq23 == "b"
dta$maizeage[dta$maizeage == 999] <- NA
siglev <-  1.96
set.seed(54321)

### here we do not drop the femheaded, in femheaded HH, the video was shown to the female
dta$recipient[dta$recipient == "n/a"] <- "female"
dta$messenger <- dta$maizevideo_shown
dta$video <- TRUE
dta$video[dta$messenger=="ctrl"] <- FALSE

dta$weight <- 1
dta$weight[dta$messenger == "female"] <- 1/(112*1/91)
dta$weight[dta$messenger == "male"] <- 1/(121*1/91)

outmat <- array(NA,c(22, 8))

outcome <- c("yield","","maizeage","","eduhead","","maizehh_no","","maizeprrooms","","maizeprinfo_receiv","", "fert.x","","seed","","maizedist_shop","","maizemobile","","maizemobile_access","")
for (i in seq(1,22,2)) {
print(i)
outmat[i,1] <-  mean(unlist(dta[ dta$video==FALSE,][outcome[i]]), na.rm=T)
outmat[i+1,1] <-  sd(unlist(dta[ dta$video==FALSE,][outcome[i]]), na.rm=T)
outmat[i,2] <- summary(lm(as.formula(paste(outcome[i],"video+ivr+sms+recipient+femhead",sep="~")) ,data=dta, weights=weight))$coef[2,1]
outmat[i+1,2] <- summary(lm(as.formula(paste(outcome[i],"video+ivr+sms+recipient+femhead",sep="~")) ,data=dta, weights=weight))$coef[2,2]
outmat[i,3]  <- summary(lm(as.formula(paste(outcome[i],"video+ivr+sms+recipient+femhead",sep="~")) ,data=dta, weights=weight))$coef[2,4]
outmat[i,4] <- summary(lm(as.formula(paste(outcome[i],"video+ivr+sms+recipient+femhead",sep="~")) ,data=dta, weights=weight))$coef[3,1]
outmat[i+1,4] <- summary(lm(as.formula(paste(outcome[i],"video+ivr+sms+recipient+femhead",sep="~")) ,data=dta, weights=weight))$coef[3,2]
outmat[i,5]  <- summary(lm(as.formula(paste(outcome[i],"video+ivr+sms+recipient+femhead",sep="~")) ,data=dta, weights=weight))$coef[3,4]
outmat[i,6] <- summary(lm(as.formula(paste(outcome[i],"video+ivr+sms+recipient+femhead",sep="~")) ,data=dta, weights=weight))$coef[4,1]
outmat[i+1,6] <- summary(lm(as.formula(paste(outcome[i],"video+ivr+sms+recipient+femhead",sep="~")) ,data=dta, weights=weight))$coef[4,2]
outmat[i,7] <-  summary(lm(as.formula(paste(outcome[i],"video+ivr+sms+recipient+femhead",sep="~")) ,data=dta, weights=weight))$coef[4,4]
outmat[i,8] <-  nobs(lm(as.formula(paste(outcome[i],"video+ivr+sms+recipient+femhead",sep="~")) ,data=dta, weights=weight))
}

## F-tests - these are partial F-test as we also control for design effects (messenger, recipient and femhead)

dta_cpy <- dta
write.csv(dta,"/home/bjvca/data/projects/digital green/endline/data/raw/baseline.csv")



### an extremely inelegant way to get rid of missings...
fm <- lm(video ~yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert.x+seed+maizedist_shop + maizemobile + maizemobile_access + as.factor(recipient) +femhead+ivr+sms, data=dta, weights=weight,na.action = na.exclude)
dta$resid <- resid(fm)
dta <- subset(dta, !is.na(resid))

reduced <- lm(video ~ recipient +femhead+ivr+sms, data=dta, weights=weight)
full <- lm(video ~yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert.x+seed+maizedist_shop + maizemobile + maizemobile_access  + recipient +femhead+ivr+sms, data=dta, weights=weight)
anova(reduced,full)

dta <- dta_cpy
dta$num_ivr[dta$ivr == "yes"] <- 1
dta$num_ivr[dta$ivr == "no"] <- 0
### an extremely inelegant way to get rid of missings...
fm <- lm(as.numeric(num_ivr) ~yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert.x+seed+maizedist_shop + maizemobile + maizemobile_access  + recipient +femhead+video+sms, data=dta, weights=weight,na.action = na.exclude)
dta$resid <- resid(fm)
dta <- subset(dta, !is.na(resid))

reduced <- lm(num_ivr ~ recipient +femhead+video+sms, data=dta, weights=weight)
full <- lm(num_ivr ~yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert.x+seed+maizedist_shop + maizemobile + maizemobile_access  + recipient +femhead+video+sms, data=dta, weights=weight)
anova(reduced,full)

dta <- dta_cpy
dta$num_sms[dta$sms == "yes"] <- 1
dta$num_sms[dta$sms == "no"] <- 0
### an extremely inelegant way to get rid of missings...
fm <- lm(as.numeric(num_sms) ~yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert.x+seed+maizedist_shop  + maizemobile + maizemobile_access + recipient +femhead+ video+ivr, data=dta, weights=weight,na.action = na.exclude)
dta$resid <- resid(fm)
dta <- subset(dta, !is.na(resid))

reduced <- lm(as.numeric(num_sms) ~ recipient +femhead+video+ivr, data=dta, weights=weight)
full <- lm(as.numeric(num_sms) ~yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert.x+seed+maizedist_shop  + maizemobile + maizemobile_access + recipient +femhead+video+ivr, data=dta, weights=weight)
anova(reduced,full)



