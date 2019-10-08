
### look at some balancing variables

library(foreign)
dta <- read.dta("/home/bjvca/data/projects/digital green/baseline/DLEC.dta")


dta$know_space <- dta$maizeoptimal_spacing == "a"
dta$know_small <- dta$maizeq22 == "c"
dta$know_weed <- dta$maizeq23 == "b"
dta$maizeage[dta$maizeage == 999] <- NA
siglev <-  1.96
set.seed(54321)
### drop the femheaded
dta <- subset(dta, recipient != "n/a")
dta$messenger <- dta$maizevideo_shown
dta$maizemobile <- dta$maizemobile == "Yes"
dta$maizemobile_access <- dta$maizemobile_access == "Yes"
dta$maizemobile_access[dta$maizemobile == TRUE] <- TRUE 

## merge in treatments
treats <- read.csv("/home/bjvca/data/projects/digital green/sampling/sampling_list_ID.csv")[c("HHID","IVR","sms")]
names(treats) <- c("hhid","ivr","sms")
dta <- merge(treats, dta, by="hhid", all=F)


dta <- subset(dta, messenger!='ctrl')
dta$weight <- 1
dta$weight[dta$recipient=="male" & dta$messenger=="female"] <- 340/369
dta$weight[dta$recipient=="male" & dta$messenger=="couple"] <- 340/377
dta$weight[dta$recipient=="male" & dta$messenger=="male"] <- 340/385
dta$weight[dta$recipient=="female" & dta$messenger=="female"] <- 340/373
dta$weight[dta$recipient=="female" & dta$messenger=="couple"] <- 340/385
dta$weight[dta$recipient=="female" & dta$messenger=="male"] <- 340/386
dta$weight[dta$recipient=="couple" & dta$messenger=="female"] <- 340/373

dta$weight[dta$recipient=="couple" & dta$messenger=="male"] <- 340/343
balance <- array(NA,c(20, 13))
jointF <-  array(NA,c(3, 6))
library(Hmisc)
balance[1,13] <-wtd.mean(dta$maizebags_harv*100/dta$maizearea_cultivation, dta$weight)
balance[2,13] <-sqrt(wtd.var(dta$maizebags_harv*100/dta$maizearea_cultivation, dta$weight))
balance[3,13] <-wtd.mean(dta$maizeage, dta$weight, na.rm=T)
balance[4,13] <-sqrt(wtd.var(dta$maizeage, dta$weight, na.rm=T))

balance[5,13] <-wtd.mean(as.numeric(dta$maizeeduc>2), na.rm=T)
balance[6,13] <-sqrt(wtd.var(as.numeric(dta$maizeeduc>2), na.rm=T))

balance[7,13] <-wtd.mean(dta$maizehh_no, na.rm=T)
balance[8,13] <-sqrt(wtd.var(dta$maizehh_no, na.rm=T))

balance[9,13] <-wtd.mean(dta$maizeprrooms, na.rm=T)
balance[10,13] <-sqrt(wtd.var(dta$maizeprrooms, na.rm=T))
 
balance[11,13] <-wtd.mean(as.numeric(dta$maizeprinfo_receiv=="Yes"), na.rm=T)
balance[12,13] <-sqrt(wtd.var(as.numeric(dta$maizeprinfo_receiv=="Yes"), na.rm=T))
#this is actually fertilizer use
balance[13,13] <-wtd.mean(as.numeric(dta$maizeprinfo_receiv_spouse=="Yes"),na.rm=T)
balance[14,13] <-sqrt(wtd.var(as.numeric(dta$maizeprinfo_receiv_spouse=="Yes"),na.rm=T))
#this is seed use

balance[15,13] <-wtd.mean(as.numeric(dta$maizeprinput_use=="Yes"), na.rm=T)
balance[16,13] <-sqrt(wtd.var(as.numeric(dta$maizeprinput_use=="Yes"), na.rm=T))
balance[17,13] <-wtd.mean(dta$maizedist_shop, na.rm=T)
balance[18,13] <-sqrt(wtd.var(dta$maizedist_shop, na.rm=T))

balance[19,13] <-wtd.mean(dta$maizemobile, na.rm=T)
balance[20,13] <-sqrt(wtd.var(dta$maizemobile, na.rm=T))

dta$yield <- dta$maizebags_harv*100/dta$maizearea_cultivation
#does imbalance persist if we restrict to households that were on the original sample list; that is, not on the backup lists?
#dta <- subset(dta,hh_yes=="Yes")
dta_copy <- dta




for (h in c(1,3,5,7,9,11)) {
if (h==1) {
############################################ H1: info asymmetry: rec=individual vs rec=couple #########################################################
dta <- dta_copy
dta$weight <- 1
dta$weight[dta$recipient == "female"] <-  1131/1144
dta$weight[dta$recipient == "male"] <- 1

treat <- "(recipient == 'couple') +as.factor(messenger)*ivr*sms + ivr*(recipient == 'couple')*sms" 
} else if (h==7) {
############################################ H2: promote collective approach ###################################################
dta <- dta_copy
dta$weight <- 1
dta$weight[dta$messenger == "female"] <-  1
dta$weight[dta$messenger == "male"] <- 1102/1114
treat <- "(messenger == 'couple') +as.factor(recipient)*ivr*sms + ivr*(messenger == 'couple')*sms"
} else if (h==3) {
############################################ H1a: info assym men vs couple ###################################################
dta <- subset(dta_copy, recipient != "female")
dta$weight <- 1
treat <- "(recipient == 'couple') +as.factor(messenger)*ivr*sms + ivr*(recipient == 'couple')*sms" 
} else if (h==5) {
############################################ H1b: info assym women vs couple ###################################################
dta <- subset(dta_copy, recipient != "male")
dta$weight <- 1
treat <- "(recipient == 'couple') +as.factor(messenger)*ivr*sms + ivr*(recipient == 'couple')*sms" 
} else if (h==9) {
############################################ H2a: info assym men vs couple ###################################################
dta <- subset(dta_copy, messenger != "female")
dta$weight <- 1
treat <- "(messenger == 'couple') +as.factor(recipient)*ivr*sms + ivr*(messenger == 'couple')*sms" 
} else if (h==11) {
############################################ H2b: info assym women vs couple ###################################################
dta <- subset(dta_copy, messenger != "male")
dta$weight <- 1
treat <- "(messenger == 'couple') +as.factor(recipient)*ivr*sms + ivr*(messenger == 'couple')*sms"
}




print(h)



balance[1,h] <-  summary(lm(as.formula(paste("yield",treat, sep="~")), data= dta, weights=weight))$coefficients[2,1]
balance[2,h] <-  summary(lm(as.formula(paste("yield",treat, sep="~")), data= dta, weights=weight))$coefficients[2,2]
balance[1,h+1] <-  summary(lm(as.formula(paste("yield",treat, sep="~")), data= dta, weights=weight))$coefficients[2,4]

balance[3,h] <- summary(lm(as.formula(paste("maizeage",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,1]
balance[4,h] <- summary(lm(as.formula(paste("maizeage",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,2]
balance[3,h+1] <- summary(lm(as.formula(paste("maizeage",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,4]

balance[5,h] <- summary(lm(as.formula(paste("(maizeeduc > 2)",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,1]
balance[6,h] <- summary(lm(as.formula(paste("(maizeeduc >2)",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,2]
balance[5,h+1] <- summary(lm(as.formula(paste("(maizeeduc > 2)",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,4]

balance[7,h] <- summary(lm(as.formula(paste("maizehh_no",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,1]
balance[8,h] <- summary(lm(as.formula(paste("maizehh_no",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,2]
balance[7,h+1] <- summary(lm(as.formula(paste("maizehh_no",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,4]

balance[9,h] <-summary(lm(as.formula(paste("maizeprrooms",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,1]
balance[10,h] <-summary(lm(as.formula(paste("maizeprrooms",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,2]
balance[9,h+1] <-summary(lm(as.formula(paste("maizeprrooms",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,4]

balance[11,h] <-summary(lm(as.formula(paste("(maizeprinfo_receiv=='Yes')",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,1]
balance[12,h] <-summary(lm(as.formula(paste("(maizeprinfo_receiv=='Yes')",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,2]
balance[11,h+1] <-summary(lm(as.formula(paste("(maizeprinfo_receiv=='Yes')",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,4]
### labeling mistake here: maizeprinfo_receiv_spouse should be fertilizer_use 

balance[13,h] <-summary(lm(as.formula(paste("maizeprinfo_receiv_spouse=='Yes'",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,1]
balance[14,h] <-summary(lm(as.formula(paste("maizeprinfo_receiv_spouse=='Yes'",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,2]
balance[13,h+1] <-summary(lm(as.formula(paste("maizeprinfo_receiv_spouse=='Yes'",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,4]
### labeling mistake here: maizeprinfo_receiv_spouse should be improvedseed_use
balance[15,h] <-summary(lm(as.formula(paste("maizeprinput_use=='Yes'",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,1]
balance[16,h] <-summary(lm(as.formula(paste("maizeprinput_use=='Yes'",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,2]
balance[15,h+1] <-summary(lm(as.formula(paste("maizeprinput_use=='Yes'",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,4]

balance[17,h] <-summary(lm(as.formula(paste("maizedist_shop",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,1]
balance[18,h] <-summary(lm(as.formula(paste("maizedist_shop",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,2]
balance[17,h+1] <-summary(lm(as.formula(paste("maizedist_shop",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,4]

balance[19,h] <-summary(lm(as.formula(paste("maizemobile",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,1]
balance[20,h] <-summary(lm(as.formula(paste("maizemobile",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,2]
balance[19,h+1] <-summary(lm(as.formula(paste("maizemobile",treat, sep="~")), data=  dta, weights=weight))$coefficients[2,4]
}

dta <- dta_copy
dta$weight <- 1
dta$weight[dta$recipient == "female"] <-  1131/1144
dta$weight[dta$recipient == "male"] <- 1

## joint tests and Nobs
covars <- c("yield + maizeage + (maizeeduc > 2) + maizehh_no+ maizeprrooms + (maizeprinfo_receiv=='Yes') + (maizeprinfo_receiv_spouse=='Yes') + (maizeprinput_use=='Yes') + maizedist_shop+maizemobile+ivr+sms+as.factor(messenger)")
x <- summary(lm(as.formula(paste("(recipient == 'couple')",covars, sep="~")), data= dta))
jointF[1,1] <- x$fstatistic[1]
jointF[2,1] <- pf(x$fstatistic[1],x$fstatistic[2],x$fstatistic[3],lower.tail=FALSE)
jointF[3,1] <- x$df[1] + x$df[2]

dta <- dta_copy
dta$weight <- 1
dta$weight[dta$messenger == "female"] <-  1
dta$weight[dta$messenger == "male"] <- 1102/1114

## joint tests and Nobs
covars <- c("yield + maizeage + (maizeeduc > 2) + maizehh_no+ maizeprrooms + (maizeprinfo_receiv=='Yes') + (maizeprinfo_receiv_spouse=='Yes') + (maizeprinput_use=='Yes') + maizedist_shop+maizemobile+ivr+sms+as.factor(recipient)")
x <- summary(lm(as.formula(paste("(messenger == 'couple')",covars, sep="~")), data= dta))
jointF[1,4] <- x$fstatistic[1]
jointF[2,4] <- pf(x$fstatistic[1],x$fstatistic[2],x$fstatistic[3],lower.tail=FALSE)
jointF[3,4] <- x$df[1] + x$df[2]



dta <- subset(dta_copy, recipient != "female")
dta$weight <- 1
covars <- c("yield + maizeage + (maizeeduc > 2) + maizehh_no+ maizeprrooms + (maizeprinfo_receiv=='Yes') + (maizeprinfo_receiv_spouse=='Yes') + (maizeprinput_use=='Yes') + maizedist_shop+maizemobile+ivr+sms")
x <- summary(lm(as.formula(paste("(recipient == 'couple')",covars, sep="~")), data= dta))
jointF[1,2] <- x$fstatistic[1]
jointF[2,2] <- pf(x$fstatistic[1],x$fstatistic[2],x$fstatistic[3],lower.tail=FALSE)
jointF[3,2] <- x$df[1] + x$df[2]

dta <- subset(dta_copy, recipient != "male")
dta$weight <- 1
covars <- c("yield + maizeage + (maizeeduc > 2) + maizehh_no+ maizeprrooms + (maizeprinfo_receiv=='Yes') + (maizeprinfo_receiv_spouse=='Yes') + (maizeprinput_use=='Yes') + maizedist_shop+maizemobile+ivr+sms")
x <- summary(lm(as.formula(paste("(recipient == 'couple')",covars, sep="~")), data= dta))
jointF[1,3] <- x$fstatistic[1]
jointF[2,3] <- pf(x$fstatistic[1],x$fstatistic[2],x$fstatistic[3],lower.tail=FALSE)
jointF[3,3] <- x$df[1] + x$df[2]


dta <- subset(dta_copy, messenger != "female")
dta$weight <- 1
covars <- c("yield + maizeage + (maizeeduc > 2) + maizehh_no+ maizeprrooms + (maizeprinfo_receiv=='Yes') + (maizeprinfo_receiv_spouse=='Yes') + (maizeprinput_use=='Yes') + maizedist_shop+maizemobile+ivr+sms")
x <- summary(lm(as.formula(paste("(messenger == 'couple')",covars, sep="~")), data= dta))
jointF[1,5] <- x$fstatistic[1]
jointF[2,5] <- pf(x$fstatistic[1],x$fstatistic[2],x$fstatistic[3],lower.tail=FALSE)
jointF[3,5] <- x$df[1] + x$df[2]


dta <- subset(dta_copy, messenger != "male")
dta$weight <- 1
covars <- c("yield + maizeage + (maizeeduc > 2) + maizehh_no+ maizeprrooms + (maizeprinfo_receiv=='Yes') + (maizeprinfo_receiv_spouse=='Yes') + (maizeprinput_use=='Yes') + maizedist_shop+maizemobile+ivr+sms")
x <- summary(lm(as.formula(paste("(messenger == 'couple')",covars, sep="~")), data= dta))
jointF[1,6] <- x$fstatistic[1]
jointF[2,6] <- pf(x$fstatistic[1],x$fstatistic[2],x$fstatistic[3],lower.tail=FALSE)
jointF[3,6] <- x$df[1] + x$df[2]


