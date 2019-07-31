#balance and attrition
#changelog - this used to be a file that created a balance table for baseline characteristics for only the attritors
#but the reviewer rightly pointed out that sample size is too small to detect differences
#so we turn it around and created a balance table for baseline characteristics for only the non-attritors
#we also run a range of joint tests that were suggested by the reviewer where we create an indicator of attrition (T/F) and regress this on treatments and baseline characteristics and interactions

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
dta$fert <- dta$fert.x

###keep only non-attriters - all households that were interviewed at endline have non-missing on know_space 
dim(dta)
sum(is.na(dta$know_space))
sum(is.na(dta$know_space))/dim(dta)[1]
dta <- subset(dta, !is.na(know_space))
dim(dta)

dta$know_space <- dta$maizeoptimal_spacing == "a"
dta$know_small <- dta$maizeq22 == "c"
dta$know_weed <- dta$maizeq23 == "b"
dta$maizeage[dta$maizeage == 999] <- NA
siglev <-  1.96
set.seed(54321)

### here we do not drop the femheaded, in femheaded HH, the video was shown to the female
dta$recipient[is.na(dta$recipient)] <- "female"
dta$messenger <- dta$maizevideo_shown
dta$video <- TRUE
dta$video[dta$messenger=="ctrl"] <- FALSE

#couple   ctrl female   male 
#  1024    238   1177   1180 
#men need to get lowest weigths

dta$weight <- 1
#> 1/(1177*1/1024)
#[1] 0.8700085


#> 1/(1180*1/1024)
#[1] 0.8677966  ## this is lowest weight -> men


dta$weight[dta$messenger == "female"] <- 1/(1177*1/1024)
dta$weight[dta$messenger == "male"] <- 1/(1180*1/1024)


outmat <- array(NA,c(22, 8))



outcome <- outcome <- c("yield","","maizeage","","eduhead","","maizehh_no","","maizeprrooms","","maizeprinfo_receiv","", "fert","","seed","","maizedist_shop","","maizemobile","","maizemobile_access","")
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


### an extremely inelegant way to get rid of missings...
fm <- lm(video ~yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop + maizemobile + maizemobile_access + recipient +femhead+ivr+sms, data=dta, weights=weight,na.action = na.exclude)
dta$resid <- resid(fm)
dta <- subset(dta, !is.na(resid))

reduced <- lm(video ~ recipient +femhead+ivr+sms, data=dta, weights=weight)
full <- lm(video ~yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop + maizemobile + maizemobile_access  + recipient +femhead+ivr+sms, data=dta, weights=weight)
anova(reduced,full)

dta <- dta_cpy
dta$num_ivr[dta$ivr == "yes"] <- 1
dta$num_ivr[dta$ivr == "no"] <- 0
### an extremely inelegant way to get rid of missings...
fm <- lm(as.numeric(num_ivr) ~yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop + maizemobile + maizemobile_access  + recipient +femhead+video+sms, data=dta, weights=weight,na.action = na.exclude)
dta$resid <- resid(fm)
dta <- subset(dta, !is.na(resid))

reduced <- lm(num_ivr ~ recipient +femhead+video+sms, data=dta, weights=weight)
full <- lm(num_ivr ~yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop + maizemobile + maizemobile_access  + recipient +femhead+video+sms, data=dta, weights=weight)
anova(reduced,full)

dta <- dta_cpy
dta$num_sms[dta$sms == "yes"] <- 1
dta$num_sms[dta$sms == "no"] <- 0
### an extremely inelegant way to get rid of missings...
fm <- lm(as.numeric(num_sms) ~yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop  + maizemobile + maizemobile_access + recipient +femhead+ video+ivr, data=dta, weights=weight,na.action = na.exclude)
dta$resid <- resid(fm)
dta <- subset(dta, !is.na(resid))

reduced <- lm(as.numeric(num_sms) ~ recipient +femhead+video+ivr, data=dta, weights=weight)
full <- lm(as.numeric(num_sms) ~yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop  + maizemobile + maizemobile_access + recipient +femhead+video+ivr, data=dta, weights=weight)
anova(reduced,full)

### per refs suggestions
#balance and attrition

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
dta$fert <- dta$fert.x


### here we do not drop the femheaded, in femheaded HH, the video was shown to the female
dta$recipient[is.na(dta$recipient)] <- "female"
dta$messenger <- dta$maizevideo_shown
dta$video <- TRUE
dta$video[dta$messenger=="ctrl"] <- FALSE

#couple   ctrl female   male 
#  1024    238   1177   1180 
#men need to get lowest weigths

dta$weight <- 1
#> 1/(1177*1/1024)
#[1] 0.8700085


#> 1/(1180*1/1024)
#[1] 0.8677966  ## this is lowest weight -> men


dta$weight[dta$messenger == "female"] <- 1/(1177*1/1024)
dta$weight[dta$messenger == "male"] <- 1/(1180*1/1024)

 
dta$attriter <- is.na(dta$know_space)
dta_cpy <- dta


summary(lm(attriter~video+ivr+sms+recipient+femhead,weights=weight, data=dta))

dta <- dta_cpy
fm <- lm(attriter~video+ivr+sms,data=dta,weights=weight,na.action = na.exclude)

dta$resid <- resid(fm)
dta <- subset(dta, !is.na(resid))

reduced <- lm(attriter~1,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~video,data=dta,weights=weight)
anova(reduced,full)
full <-  lm(attriter~ivr,data=dta,weights=weight)
anova(reduced,full)
full <-  lm(attriter~sms,data=dta,weights=weight)
anova(reduced,full)
full <-  lm(attriter~video+ivr+sms,data=dta,weights=weight)
anova(reduced,full)



### do this in a loop
outmat <- array(NA,c(22,4))

outcome <- outcome <- c("yield","","maizeage","","eduhead","","maizehh_no","","maizeprrooms","","maizeprinfo_receiv","", "fert","","seed","","maizedist_shop","","maizemobile","","maizemobile_access","")
for (i in seq(1,22,2)) {
print(i)
dta <- dta_cpy
fm <- lm(as.formula(paste("attriter",paste(outcome[i],"(video+ivr+sms)",sep="*"),sep="~")),data=dta,weights=weight,na.action = na.exclude)
dta$resid <- resid(fm)
dta <- subset(dta, !is.na(resid))

reduced <- lm(as.formula(paste("attriter~video", outcome[i],sep="+")),data=dta,weights=weight,na.action = na.exclude)
full <-  lm(as.formula(paste("attriter",paste(outcome[i],"(video)",sep="*"),sep="~")),data=dta,weights=weight,na.action = na.exclude)

outmat[i,1] <-  anova(reduced,full)[[5]][2]
outmat[i+1,1] <- anova(reduced,full)[[6]][2]

reduced <- lm(as.formula(paste("attriter~ivr", outcome[i],sep="+")),data=dta,weights=weight,na.action = na.exclude)
full <-  lm(as.formula(paste("attriter",paste(outcome[i],"(ivr)",sep="*"),sep="~")),data=dta,weights=weight,na.action = na.exclude)

outmat[i,2] <-  anova(reduced,full)[[5]][2]
outmat[i+1,2] <- anova(reduced,full)[[6]][2]

reduced <- lm(as.formula(paste("attriter~sms", outcome[i],sep="+")),data=dta,weights=weight,na.action = na.exclude)
full <-  lm(as.formula(paste("attriter",paste(outcome[i],"(sms)",sep="*"),sep="~")),data=dta,weights=weight,na.action = na.exclude)

outmat[i,3] <-  anova(reduced,full)[[5]][2]
outmat[i+1,3] <- anova(reduced,full)[[6]][2]

reduced <- lm(as.formula(paste("attriter~video+ivr+sms", outcome[i],sep="+")),data=dta,weights=weight,na.action = na.exclude)
full <-   lm(as.formula(paste("attriter",paste(outcome[i],"(video+ivr+sms)",sep="*"),sep="~")),data=dta,weights=weight,na.action = na.exclude)

outmat[i,4] <-  anova(reduced,full)[[5]][2]
outmat[i+1,4] <- anova(reduced,full)[[6]][2]
}



dta <- dta_cpy

fm <- lm(attriter~(yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop+maizemobile+maizemobile_access)*video + (yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop+maizemobile+maizemobile_access)*ivr +(yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop+maizemobile+maizemobile_access)*sms,data=dta,weights=weight,na.action = na.exclude)

dta$resid <- resid(fm)
dta <- subset(dta, !is.na(resid))

reduced <- lm(attriter~video+yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop+maizemobile+maizemobile_access,data=dta,weights=weight)
full <- lm(attriter~(yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop+maizemobile+maizemobile_access)*video,data=dta,weights=weight)
anova(reduced,full)

reduced <- lm(attriter~ivr+yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop+maizemobile+maizemobile_access,data=dta,weights=weight)
full <- lm(attriter~(yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop+maizemobile+maizemobile_access)*ivr,data=dta,weights=weight)
anova(reduced,full)

reduced <- lm(attriter~sms+yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop+maizemobile+maizemobile_access,data=dta,weights=weight)
full <- lm(attriter~(yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop+maizemobile+maizemobile_access)*sms,data=dta,weights=weight)
anova(reduced,full)

reduced <- lm(attriter~video+ivr+sms+yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop+maizemobile+maizemobile_access,data=dta,weights=weight)
full <- lm(attriter~(yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop+maizemobile+maizemobile_access)*video + (yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop+maizemobile+maizemobile_access)*ivr +(yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop+maizemobile+maizemobile_access)*sms,data=dta,weights=weight)
anova(reduced,full)



