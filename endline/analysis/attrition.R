#balance and attrition

rm(list=ls())
library(foreign)
baseline <- read.dta("/home/bjvca/data/projects/digital green/baseline/DLEC.dta")
##dta <- read.csv("/home/bjvca/data/projects/digital green/baseline/baseline.csv")

source("/home/bjvca/data/projects/digital green/endline/data/init.R")


 dta <- merge(baseline, dta, by="hhid", all.x=T)
dta$attriter <- is.na(dta$know_space)

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

## merge in treatments
treats <- read.csv("/home/bjvca/data/projects/digital green/sampling/sampling_list_ID.csv")[c("HHID","IVR","sms")]
names(treats) <- c("hhid","ivr","sms")
treats$femhead <- FALSE
###merge in treatments for FHs - did they receive sms messages?
treats_FH <- read.csv("/home/bjvca/data/projects/digital green/sampling/femhead_list_ID.csv")[c("HHID","IVR")]
names(treats_FH) <- c("hhid","ivr")
treats_FH$sms <- "no"
treats_FH$femhead <- TRUE

treats <- rbind(treats_FH, treats)
treats$sms <- as.factor(treats$sms)
dta <- merge(treats, dta, by="hhid", all=F)


## overall attrition
summary(lm(attriter~1, data=dta))
## attrition among those that received video treatment
summary(lm(attriter~1, data=subset(dta, !video)))
summary(lm(attriter~1, data=subset(dta, video)))
summary(lm(attriter~1, data=subset(dta, ivr=="yes")))
summary(lm(attriter~1, data=subset(dta, sms=="yes")))
## F test of joint significance
summary(lm(attriter~video+ivr+sms, data=dta))

dta$yield <- dta$maizebags_harv*100/dta$maizearea_cultivation
dta$eduhead <- as.numeric(dta$maizeeduc>2)
dta$maizeprinfo_receiv <- as.numeric(dta$maizeprinfo_receiv=="Yes")
dta$fert <- as.numeric(dta$maizeprinfo_receiv_spouse=="Yes")
dta$seed <- as.numeric(dta$maizeprinput_use=="Yes")#### redo balance tests for DP_ICT paper
dta$maizemobile <- dta$maizemobile == "Yes"
dta$maizemobile_access <- dta$maizemobile_access == "Yes"
dta$maizemobile_access[dta$maizemobile == TRUE] <- TRUE 

outmat <- array(NA,c(22, 8))

dta$weight <- 1
dta$weight[dta$messenger == "female"] <- 1/(1287*1/1115)
dta$weight[dta$messenger == "male"] <- 1/(1301*1/1115)

outcome <- c("yield","","maizeage","","eduhead","","maizehh_no","","maizeprrooms","","maizeprinfo_receiv","", "fert","","seed","","maizedist_shop","","maizemobile","","maizemobile_access","")
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


#### an extremely inelegant way to get rid of missings...
#fm <- lm(video ~yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop + maizemobile + maizemobile_access + recipient +femhead+ivr+sms, data=dta, weights=weight,na.action = na.exclude)
#dta$resid <- resid(fm)
#dta <- subset(dta, !is.na(resid))

#reduced <- lm(video ~ recipient +femhead+ivr+sms, data=dta, weights=weight)
#full <- lm(video ~yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop + maizemobile + maizemobile_access  + recipient +femhead+ivr+sms, data=dta, weights=weight)
#anova(reduced,full)

#dta <- dta_cpy
#dta$num_ivr[dta$ivr == "yes"] <- 1
#dta$num_ivr[dta$ivr == "no"] <- 0
#### an extremely inelegant way to get rid of missings...
#fm <- lm(as.numeric(num_ivr) ~yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop + maizemobile + maizemobile_access  + recipient +femhead+video+sms, data=dta, weights=weight,na.action = na.exclude)
#dta$resid <- resid(fm)
#dta <- subset(dta, !is.na(resid))

#reduced <- lm(num_ivr ~ recipient +femhead+video+sms, data=dta, weights=weight)
#full <- lm(num_ivr ~yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop + maizemobile + maizemobile_access  + recipient +femhead+video+sms, data=dta, weights=weight)
#anova(reduced,full)

#dta <- dta_cpy
#dta$num_sms[dta$sms == "yes"] <- 1
#dta$num_sms[dta$sms == "no"] <- 0
#### an extremely inelegant way to get rid of missings...
#fm <- lm(as.numeric(num_sms) ~yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop  + maizemobile + maizemobile_access + recipient +femhead+ video+ivr, data=dta, weights=weight,na.action = na.exclude)
#dta$resid <- resid(fm)
#dta <- subset(dta, !is.na(resid))

#reduced <- lm(as.numeric(num_sms) ~ recipient +femhead+video+ivr, data=dta, weights=weight)
#full <- lm(as.numeric(num_sms) ~yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop  + maizemobile + maizemobile_access + recipient +femhead+video+ivr, data=dta, weights=weight)
#anova(reduced,full)

### per refs suggestions
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

dta <- dta_cpy
fm <- lm(attriter~yield*(video+ivr+sms),data=dta,weights=weight,na.action = na.exclude)

dta$resid <- resid(fm)
dta <- subset(dta, !is.na(resid))

reduced <- lm(attriter~video+yield,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~yield*(video),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~ivr+yield,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~yield*(ivr),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~sms+yield,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~yield*(sms),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~video+ivr+sms+yield,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~yield*(video+ivr+sms),data=dta,weights=weight)
anova(reduced,full)

dta <- dta_cpy

fm <- lm(attriter~maizeage*(video+ivr+sms),data=dta,weights=weight,na.action = na.exclude)

dta$resid <- resid(fm)
dta <- subset(dta, !is.na(resid))

reduced <- lm(attriter~video+maizeage,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizeage*(video),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~ivr+maizeage,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizeage*(ivr),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~sms+maizeage,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizeage*(sms),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~maizeage+ivr+sms+video,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizeage*(video+ivr+sms),data=dta,weights=weight)
anova(reduced,full)

dta <- dta_cpy

fm <- lm(attriter~maizeage*(video+ivr+sms),data=dta,weights=weight,na.action = na.exclude)

dta$resid <- resid(fm)
dta <- subset(dta, !is.na(resid))

reduced <- lm(attriter~video+eduhead,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~eduhead*(video),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~ivr+eduhead,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~eduhead*(ivr),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~sms+eduhead,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~eduhead*(sms),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~eduhead+ivr+sms+video,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~eduhead*(video+ivr+sms),data=dta,weights=weight)
anova(reduced,full)

dta <- dta_cpy

fm <- lm(attriter~maizehh_no*(video+ivr+sms),data=dta,weights=weight,na.action = na.exclude)

dta$resid <- resid(fm)
dta <- subset(dta, !is.na(resid))

reduced <- lm(attriter~video+maizehh_no,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizehh_no*(video),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~ivr+maizehh_no,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizehh_no*(ivr),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~sms+maizehh_no,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizehh_no*(sms),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~maizehh_no+ivr+sms+video,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizehh_no*(video+ivr+sms),data=dta,weights=weight)
anova(reduced,full)



dta <- dta_cpy

fm <- lm(attriter~maizeprrooms*(video+ivr+sms),data=dta,weights=weight,na.action = na.exclude)

dta$resid <- resid(fm)
dta <- subset(dta, !is.na(resid))

reduced <- lm(attriter~video+maizeprrooms,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizeprrooms*(video),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~ivr+maizeprrooms,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizeprrooms*(ivr),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~sms+maizeprrooms,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizeprrooms*(sms),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~maizeprrooms+ivr+sms+video,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizeprrooms*(video+ivr+sms),data=dta,weights=weight)
anova(reduced,full)

dta <- dta_cpy

fm <- lm(attriter~maizeprinfo_receiv*(video+ivr+sms),data=dta,weights=weight,na.action = na.exclude)

dta$resid <- resid(fm)
dta <- subset(dta, !is.na(resid))

reduced <- lm(attriter~video+maizeprinfo_receiv,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizeprinfo_receiv*(video),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~ivr+maizeprinfo_receiv,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizeprinfo_receiv*(ivr),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~sms+maizeprinfo_receiv,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizeprinfo_receiv*(sms),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~maizeprinfo_receiv+ivr+sms+video,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizeprinfo_receiv*(video+ivr+sms),data=dta,weights=weight)
anova(reduced,full)

dta <- dta_cpy

fm <- lm(attriter~fert*(video+ivr+sms),data=dta,weights=weight,na.action = na.exclude)

dta$resid <- resid(fm)
dta <- subset(dta, !is.na(resid))

reduced <- lm(attriter~video+fert,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~fert*(video),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~ivr+fert,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~fert*(ivr),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~sms+fert,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~fert*(sms),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~fert+ivr+sms+video,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~fert*(video+ivr+sms),data=dta,weights=weight)
anova(reduced,full)


dta <- dta_cpy

fm <- lm(attriter~seed*(video+ivr+sms),data=dta,weights=weight,na.action = na.exclude)

dta$resid <- resid(fm)
dta <- subset(dta, !is.na(resid))

reduced <- lm(attriter~video+seed,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~seed*(video),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~ivr+seed,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~seed*(ivr),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~sms+seed,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~seed*(sms),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~seed+ivr+sms+video,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~seed*(video+ivr+sms),data=dta,weights=weight)
anova(reduced,full)

dta <- dta_cpy

fm <- lm(attriter~maizedist_shop*(video+ivr+sms),data=dta,weights=weight,na.action = na.exclude)

dta$resid <- resid(fm)
dta <- subset(dta, !is.na(resid))

reduced <- lm(attriter~video+maizedist_shop,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizedist_shop*(video),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~ivr+maizedist_shop,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizedist_shop*(ivr),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~sms+maizedist_shop,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizedist_shop*(sms),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~maizedist_shop+ivr+sms+video,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizedist_shop*(video+ivr+sms),data=dta,weights=weight)
anova(reduced,full)


dta <- dta_cpy

fm <- lm(attriter~maizemobile*(video+ivr+sms),data=dta,weights=weight,na.action = na.exclude)

dta$resid <- resid(fm)
dta <- subset(dta, !is.na(resid))

reduced <- lm(attriter~video+maizemobile,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizemobile*(video),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~ivr+maizemobile,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizemobile*(ivr),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~sms+maizemobile,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizemobile*(sms),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~maizemobile+ivr+sms+video,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizemobile*(video+ivr+sms),data=dta,weights=weight)
anova(reduced,full)



dta <- dta_cpy

fm <- lm(attriter~maizemobile_access*(video+ivr+sms),data=dta,weights=weight,na.action = na.exclude)

dta$resid <- resid(fm)
dta <- subset(dta, !is.na(resid))

reduced <- lm(attriter~video+maizemobile_access,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizemobile_access*(video),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~ivr+maizemobile_access,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizemobile_access*(ivr),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~sms+maizemobile_access,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizemobile_access*(sms),data=dta,weights=weight)
anova(reduced,full)
reduced <- lm(attriter~maizemobile_access+ivr+sms+video,data=dta,weights=weight,na.action = na.exclude)
full <-  lm(attriter~maizemobile_access*(video+ivr+sms),data=dta,weights=weight)
anova(reduced,full)
