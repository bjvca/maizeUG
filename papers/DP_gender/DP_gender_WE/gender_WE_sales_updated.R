rm(list=ls())
source("/home/bjvca/data/projects/digital green/endline/data/init_gender_WE.R")
baseline <-  read.csv("/home/bjvca/data/projects/digital green/baseline/base_merge.cvs")


#wget https://www.dropbox.com/s/sakp13112o1to6u/baseline.csv?dl=0
#wget https://www.dropbox.com/s/cr2i9tr3pr32fh3/AWS5.csv?dl=0

rm(list=ls())
dta <- read.csv("AWS.csv")
baseline <- read.csv("baseline.csv")
#set totrep to zero if you do not want simulation based inferecne

library(ggplot2)
library(doParallel)
library(data.table)
library(dplyr)
library(Hmisc)

set.seed(07032018)

### indexing results arrays
res_sales_w <- array(NA, c(13,4,7)) 
rownames(res_sales_w) <- c("sold","","amont_sold","","price_sold","", "to_trader","","to_middleman","","to_processor","","padj")
res_sales_b <- array(NA, c(13,4,7)) 
rownames(res_sales_b) <- c("sold","","amont_sold","","price_sold","", "to_trader","","to_middleman","","to_processor","","padj")
res_sales_m <- array(NA, c(13,4,7)) 
rownames(res_sales_m) <- c("sold","","amont_sold","","price_sold","", "to_trader","","to_middleman","","to_processor","","padj")


cl <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
registerDoParallel(cl)

### function definitions 
RI <- function(dep, indep, ctrls = NULL,  dta , nr_repl = 1000, w_int = NULL) {
# RI("(maizeeduc > 2)",treat ,ctrls = NULL, w_int="weights", dta= dta, nr_repl = totrep)
#RI("index" ,treat , contr_vars, w_int= w_int2,dta= data, nr_repl = 1000,h_int=1)
#indep <- "(recipient != 'male') +ivr+sms+ as.factor(messenger)" 
#ctrls <- NULL
#h_int <- 1
#dep <- "(maizeeduc > 2)"
##dta <- dta_bal
#nr_repl <- 100
#w_int <- "weights"

### determines treatmetn cell
	dta <- dta %>% 
    		mutate(treat = group_indices_(dta, .dots=c("recipient", "messenger", "ivr","sms"))) 
	### allocates unique ID based on treatment cell status and village
	dta <- dta %>% 
    		mutate(uniqID = group_indices_(dta, .dots=c("distID", "subID","vilID"))) 
	### the NULL
	crit <- ifelse(is.null(ctrls),summary(lm(as.formula(paste(dep,indep,sep="~")), weights=unlist(dta[w_int]), data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste(dep,indep,sep="~"), ctrls,sep="+")),  weights=unlist(dta[w_int]),data=dta))$coefficients[2,1])
if (is.null(ctrls)) {
	dta <-  data.table(cbind(dta[dep],dta[c("messenger","recipient","treat","uniqID","hhid","ivr","sms")]),cbind(dta[w_int]))
} else {
	dta <-  data.table(cbind(dta[dep],dta[c("messenger","recipient","treat","uniqID","hhid","ivr","sms")],cbind(dta[w_int]),cbind(dta[unlist(strsplit(ctrls,"[+]"))])))
}
	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
		dta_sim <- data.table(dta)
 		setDT(dta_sim)[,perm:=sample(treat, replace=FALSE)[1:.N],by = (uniqID)]

		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","female"))
		dta_sim$ivr <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$ivr,dta$treat))[table(dta$ivr, dta$treat)["yes",]>0]), "yes","no")
		dta_sim$sms <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$sms,dta$treat))[table(dta$sms, dta$treat)["yes",]>0]), "yes","no")
		return(abs(ifelse(is.null(ctrls),summary(lm(as.formula(paste(dep,indep,sep="~")), weights=weights, data=data.frame(dta_sim)))$coefficients[2,1],summary(lm(as.formula(paste(paste(dep,indep,sep="~"), ctrls,sep="+")), weights=weights, data=dta_sim))$coefficients[2,1])) > abs(crit) )
	}
	return(sum(oper)/nr_repl)
}

RI_FWER <- function(deps, indep,ctrls = NULL, dta ,p_vals , nr_repl = 1000,  w_int = NULL) {
### function to control for FWER using simulation (familywise sharp null)
### inspired on https://egap.org/methods-guides/10-things-you-need-know-about-multiple-comparisons
threshold_finder<- function(threshold){
  mean(apply(oper, 2, x <- function(x) sum(x <= threshold) > 0 ))
}
### determines treatmetn cell
#	dta <- dta %>% 
#    		mutate(treat = group_indices_(dta, .dots=c("recipient", "messenger"))) 
	dta <- dta %>% 
    		mutate(treat = group_indices_(dta, .dots=c("recipient", "messenger", "ivr","sms"))) 
	### allocates unique ID based on treatment cell status and village
	dta <- dta %>% 
    		mutate(uniqID = group_indices_(dta, .dots=c("distID", "subID","vilID"))) 
if (is.null(ctrls)) {
dta <-  data.table(cbind(dta[deps],dta[c("messenger","recipient","treat","uniqID","hhid","ivr","sms")]),cbind(dta[w_int]))

} else {
	dta <-  data.table(cbind(dta[deps],dta[c("messenger","recipient","treat","uniqID","hhid","ivr","sms")],cbind(dta[w_int]),cbind(dta[unlist(strsplit(ctrls,"[+]"))])))
}
	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
		dta_sim <- data.table(dta)
 		setDT(dta_sim)[,perm:=sample(treat, replace=FALSE)[1:.N],by = (uniqID)]


		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","female"))
		dta_sim$ivr <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$ivr,dta$treat))[table(dta$ivr, dta$treat)["yes",]>0]), "yes","no")
		dta_sim$sms <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$sms,dta$treat))[table(dta$sms, dta$treat)["yes",]>0]), "yes","no")

if (is.null(ctrls)) {
return(unlist(lapply(deps, function(dvar)  summary(lm(as.formula(paste(dvar,indep,sep="~")), weights=weights,data=dta_sim))$coefficients[2,4])))
} else {
return(unlist(lapply(deps, function(dvar)  summary(lm(as.formula(paste(paste(dvar,indep,sep="~"),ctrls, sep="+")), weights=weights,data=dta_sim))$coefficients[2,4])))
}		

		}

thresholds <- seq(0, 0.1, length.out = 10000)
type_I_rate <- sapply(thresholds, threshold_finder)
return( list=c(thresholds[max(which(type_I_rate <= 0.1))],thresholds[max(which(type_I_rate <= 0.05))], thresholds[max(which(type_I_rate <= 0.01))]))
}



wtd.sd <- function(x, w,...) {
return(sqrt(wtd.var(x,w)))
}

FW_index <- function(treat, indexer, contr_vars, w_int2,data, nr_repl=0) {
### function to make family wise index using covariance as weights (following http://cyrussamii.com/?p=2656)
### FW_index("messenger != 'ctrl' ", c("know_space", "know_combine", "know_weed"),dta)
data <- data[complete.cases(data[indexer]),]
x <- data[indexer]

				for(j in 1:ncol(x)){
					x[,j] <- (x[,j] - mean(x[,j]))/sd(x[,j])
				}

					i.vec <- as.matrix(rep(1,ncol(x)))
					Sx <- cov(x)
					
					data$index <- t(solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)%*%t(x))
mod <- lm(as.formula(paste("index",treat,sep="~")) ,weights=unlist(data[w_int2]), data=data)

					
if (nr_repl > 0) { 
	data$index <- as.vector(data$index)
	sig <- RI("index" ,treat , contr_vars, w_int= w_int2, data, nr_repl = nr_repl)
} else {
	sig <- summary(lm(as.formula(paste("index",treat,sep="~")) ,weights=unlist(data[w_int2]), data=data))$coefficients[2,4]
}
return(list(mod,sig, data))
}

## drop the control
dta <- subset(dta, messenger != "ctrl")
dta_copy <- dta

names(baseline)[names(baseline) == 'know_space'] <- 'b_know_space'
names(baseline)[names(baseline) == 'know_combine'] <- 'b_know_combine'
names(baseline)[names(baseline) == 'know_weed'] <- 'b_know_weed'
baseline$recipient <- NULL
baseline$messenger <- NULL
baseline$ivr <- NULL
baseline$sms <- NULL
baseline$maizeeduc <- (baseline$maizeeduc > 2)
baseline$maizeprinfo_receiv_spouse <- baseline$maizeprinfo_receiv_spouse=='Yes' 
baseline$maizeprinput_use <- (baseline$maizeprinput_use=='Yes')
baseline$maizemobile <- (baseline$maizemobile=='Yes')
### I use inverse hyperbolic sine transform instead of log to keep zeros (about 80)
baseline$yield <- asinh(baseline$yield)
ctrls <- NULL
### baseline has 37 duplicates - remove them
baseline <- subset(baseline,!(hhid %in% baseline$hhid[duplicated(baseline$hhid)]))
ctrls <- NULL


totrep <- 10000

####

for (h in 1:7) {
if (h==1) {
############################################ H1: empower: rec==couple or woman - rec==male #########################################################
dta <- dta_copy
treatment <- "(recipient != 'male') +ivr+sms+ as.factor(messenger)" 
dta <- merge(dta_copy,baseline, by="hhid")
dta$weights <- 1
dta$weights[dta$recipient == "female"] <-  1053/1135
ctrls <- "yield+maizeage+maizeeduc+maizehh_no+maizeprinfo_receiv_spouse+maizeprinput_use+maizemobile" 
} else if (h==2) {
############################################ H1a: empower : rec==female - rec==male ###################################################
dta <- subset(dta_copy, recipient == "male" | recipient == "female")
treatment <- "(recipient == 'female') +ivr+sms+as.factor(messenger)" 
dta <- merge(dta,baseline, by="hhid")
dta$weights <- 1
ctrls <- "maizeage+maizeeduc+maizeprrooms+maizeprinfo_receiv_spouse+maizeprinput_use+maizemobile" 
} else if (h==3) {
############################################ H1b: rec==couple - rec==male  ###################################################
dta <- subset(dta_copy, recipient == "male" | recipient == "couple")
treatment <- "(recipient == 'couple') +ivr+sms+ as.factor(messenger)"
dta <- merge(dta,baseline, by="hhid")
dta$weights <- 1
ctrls <- "yield+maizeage+maizehh_no+maizeprinfo_receiv_spouse+maizeprinput_use" 
} else if (h==4) {
############################################ H2: mes==couple or woman - mes==male ###################################################
dta <- dta_copy
ctrls<- NULL
dta$weights <- 1
dta$weights[dta$messenger == "female"] <-  1106/1108
treatment <- "(messenger != 'male') +ivr+sms+as.factor(recipient)"
} else if (h==5) {
######################## H3: comparision with the status quo  ################################
#####################   messenger== 'male' & recipient=='male' - messenger!= 'female or couple' & recipient=='female of couple' ##############################
dta <- dta_copy

dta$recipient <- factor(dta$recipient)
dta$messenger <- factor(dta$messenger)
treatment <- "(messenger!= 'male' & recipient!='male') +ivr+sms"
dta <- merge(dta,baseline, by="hhid")
dta <- subset(dta, !((messenger=='male' & (recipient %in% c("couple","female")) | recipient=='male' & (messenger %in% c("couple","female"))))   )
dta$weights <- 1
dta$weights[dta$messenger == "female" & dta$recipient == "female"] <-  318/349
dta$weights[dta$messenger == "female" & dta$recipient == "couple"] <-  318/318
dta$weights[dta$messenger == "couple" & dta$recipient == "female"] <-  318/347
dta$weights[dta$messenger == "couple" & dta$recipient == "couple"] <-  318/338

ctrls <- "yield+maizeage+maizeeduc+maizeprinfo_receiv_spouse+maizedist_shop+maizemobile" 
} else if (h==6) {
######################## H4: challenging role incongruity###########################
#############  messenger== 'female or couple' & recipient=='male' - messenger== 'male' & recipient=='male' ##############################
dta <- dta_copy
dta$recipient <- factor(dta$recipient)
dta$messenger <- factor(dta$messenger)
dta <- subset(dta, recipient =='male'  )
treatment <- "(messenger!= 'male') +ivr+sms"
dta <- merge(dta,baseline, by="hhid")

dta$weights <- 1
dta$weights[dta$messenger == "female" & dta$recipient == "male"] <-  339/348

ctrls <- "maizeprinfo_receiv+maizeprinput_use+maizedist_shop" 
} else if (h==7) {
######################## H5: this is the only correct test of gender homophilly ################################
###################### sex messenger== sex recipient - sex messenger!= sex recipient ##############################
#### we drop couples here ###################
dta <- dta_copy

dta$recipient <- factor(dta$recipient)
dta$messenger <- factor(dta$messenger)
dta <- subset(dta, messenger!='couple' &  recipient!='couple'   )
treatment <- "(messenger== recipient) +ivr+sms"
dta <- merge(dta,baseline, by="hhid")

dta$weights <- 1
## about 350 in each group, no need to use weights here

ctrls <- "maizeprrooms+maizeprinfo_receiv+maizeprinput_use" 
}


print(h)
################################################## knowledge  #####################################################

dta$sold_woman[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA

dta$outcome <- dta$sold_woman

res_sales_w[1,1,h] <-ifelse(h %in% c(5,6), wtd.mean(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.mean(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_sales_w[2,1,h] <- ifelse(h %in% c(5,6), wtd.sd(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.sd(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_sales_w[1,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_sales_w[2,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_sales_w[1,3,h] <- ifelse(totrep >0, RI("outcome",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("sold_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_sales_w[2,3,h] <- nobs(lm(as.formula(paste("outcome",treatment, sep="~")) ,data=dta))

dta$nr_bags_sold_woman[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$outcome <- dta$nr_bags_sold_woman

res_sales_w[3,1,h] <-ifelse(h %in% c(5,6), wtd.mean(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.mean(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_sales_w[4,1,h] <- ifelse(h %in% c(5,6), wtd.sd(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.sd(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_sales_w[3,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_sales_w[4,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_sales_w[3,3,h] <- ifelse(totrep >0, RI("outcome",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("sold_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_sales_w[4,3,h] <- nobs(lm(as.formula(paste("outcome",treatment, sep="~")) ,data=dta))

dta$price_sold_woman[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$outcome <- dta$price_sold_woman

res_sales_w[5,1,h] <-ifelse(h %in% c(5,6), wtd.mean(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.mean(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_sales_w[6,1,h] <- ifelse(h %in% c(5,6), wtd.sd(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.sd(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_sales_w[5,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_sales_w[6,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_sales_w[5,3,h] <- ifelse(totrep >0, RI("outcome",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("sold_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_sales_w[6,3,h] <- nobs(lm(as.formula(paste("outcome",treatment, sep="~")) ,data=dta))


dta$sold_to_trader_woman[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$outcome <- dta$sold_to_trader_woman
res_sales_w[7,1,h] <-ifelse(h %in% c(5,6), wtd.mean(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.mean(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_sales_w[8,1,h] <- ifelse(h %in% c(5,6), wtd.sd(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.sd(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_sales_w[7,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_sales_w[8,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_sales_w[7,3,h] <- ifelse(totrep >0, RI("outcome",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("sold_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_sales_w[8,3,h] <- nobs(lm(as.formula(paste("outcome",treatment, sep="~")) ,data=dta))

dta$sold_to_middleman_woman[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$outcome <- dta$sold_to_middleman_woman
res_sales_w[9,1,h] <-ifelse(h %in% c(5,6), wtd.mean(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.mean(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_sales_w[10,1,h] <- ifelse(h %in% c(5,6), wtd.sd(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.sd(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_sales_w[9,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_sales_w[10,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_sales_w[9,3,h] <- ifelse(totrep >0, RI("outcome",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("sold_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_sales_w[10,3,h] <- nobs(lm(as.formula(paste("outcome",treatment, sep="~")) ,data=dta))

dta$sold_to_processor_woman[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$outcome <- dta$sold_to_processor_woman
res_sales_w[11,1,h] <-ifelse(h %in% c(5,6), wtd.mean(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.mean(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_sales_w[12,1,h] <- ifelse(h %in% c(5,6), wtd.sd(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.sd(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_sales_w[11,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_sales_w[12,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_sales_w[11,3,h] <- ifelse(totrep >0, RI("outcome",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("sold_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_sales_w[12,3,h] <- nobs(lm(as.formula(paste("outcome",treatment, sep="~")) ,data=dta))



res_sales_w[13,1:3,h] <- RI_FWER(deps= c("sold_woman","nr_bags_sold_woman","price_sold_woman","sold_to_trader_woman","sold_to_middleman_woman","sold_to_processor_woman") ,indep = treatment , ctrls = ctrls,dta =dta, p_vals = res_sales_w[c(1,3,5,7,9,11),3,h], nr_repl = totrep, w_int="weights")


################### both as reported by woman

#dta$sold_both_woman[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA

#dta$outcome <- dta$sold_both_woman

#res_sales_b[1,1,h] <-ifelse(h %in% c(5,6), wtd.mean(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.mean(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
#res_sales_b[2,1,h] <- ifelse(h %in% c(5,6), wtd.sd(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.sd(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
#res_sales_b[1,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
#res_sales_b[2,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
#res_sales_b[1,3,h] <- ifelse(totrep >0, RI("outcome",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("sold_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
#res_sales_b[2,3,h] <- nobs(lm(as.formula(paste("outcome",treatment, sep="~")) ,data=dta))

#dta$nr_bags_sold_both_woman[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
#dta$outcome <- dta$nr_bags_sold_both_woman

#res_sales_b[3,1,h] <-ifelse(h %in% c(5,6), wtd.mean(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.mean(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
#res_sales_b[4,1,h] <- ifelse(h %in% c(5,6), wtd.sd(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.sd(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
#res_sales_b[3,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
#res_sales_b[4,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
#res_sales_b[3,3,h] <- ifelse(totrep >0, RI("outcome",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("sold_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
#res_sales_b[4,3,h] <- nobs(lm(as.formula(paste("outcome",treatment, sep="~")) ,data=dta))

#dta$price_sold_both_woman[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
#dta$outcome <- dta$price_sold_both_woman

#res_sales_b[5,1,h] <-ifelse(h %in% c(5,6), wtd.mean(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.mean(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
#res_sales_b[6,1,h] <- ifelse(h %in% c(5,6), wtd.sd(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.sd(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
#res_sales_b[5,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
#res_sales_b[6,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
#res_sales_b[5,3,h] <- ifelse(totrep >0, RI("outcome",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("sold_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
#res_sales_b[6,3,h] <- nobs(lm(as.formula(paste("outcome",treatment, sep="~")) ,data=dta))


#dta$sold_to_trader_both_woman[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
#dta$outcome <- dta$sold_to_trader_both_woman
#res_sales_b[7,1,h] <-ifelse(h %in% c(5,6), wtd.mean(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.mean(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
#res_sales_b[8,1,h] <- ifelse(h %in% c(5,6), wtd.sd(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.sd(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
#res_sales_b[7,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
#res_sales_b[8,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
#res_sales_b[7,3,h] <- ifelse(totrep >0, RI("outcome",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("sold_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
#res_sales_b[8,3,h] <- nobs(lm(as.formula(paste("outcome",treatment, sep="~")) ,data=dta))

#dta$sold_to_middleman_both_woman[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
#dta$outcome <- dta$sold_to_middleman_both_woman
#res_sales_b[9,1,h] <-ifelse(h %in% c(5,6), wtd.mean(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.mean(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
#res_sales_b[10,1,h] <- ifelse(h %in% c(5,6), wtd.sd(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.sd(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
#res_sales_b[9,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
#res_sales_b[10,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
#res_sales_b[9,3,h] <- ifelse(totrep >0, RI("outcome",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("sold_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
#res_sales_b[10,3,h] <- nobs(lm(as.formula(paste("outcome",treatment, sep="~")) ,data=dta))

#dta$sold_to_processor_both_woman[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
#dta$outcome <- dta$sold_to_processor_both_woman
#res_sales_b[11,1,h] <-ifelse(h %in% c(5,6), wtd.mean(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.mean(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
#res_sales_b[12,1,h] <- ifelse(h %in% c(5,6), wtd.sd(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.sd(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
#res_sales_b[11,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
#res_sales_b[12,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
#res_sales_b[11,3,h] <- ifelse(totrep >0, RI("outcome",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("sold_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
#res_sales_b[12,3,h] <- nobs(lm(as.formula(paste("outcome",treatment, sep="~")) ,data=dta))



#res_sales_b[13,1:3,h] <- RI_FWER(deps= c("sold_both_woman","nr_bags_sold_both_woman","price_sold_both_woman","sold_to_trader_both_woman","sold_to_middleman_both_woman","sold_to_processor_both_woman") ,indep = treatment , ctrls = ctrls,dta =dta, p_vals = res_sales_w[c(1,3,5,7,9,11),3,h], nr_repl = totrep, w_int="weights")

################## man as reported by woman
#dta$sold_man_woman[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA

#dta$outcome <- dta$sold_man_woman

#res_sales_m[1,1,h] <-ifelse(h %in% c(5,6), wtd.mean(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.mean(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
#res_sales_m[2,1,h] <- ifelse(h %in% c(5,6), wtd.sd(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.sd(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
#res_sales_m[1,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
#res_sales_m[2,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
#res_sales_m[1,3,h] <- ifelse(totrep >0, RI("outcome",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("sold_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
#res_sales_m[2,3,h] <- nobs(lm(as.formula(paste("outcome",treatment, sep="~")) ,data=dta))

#dta$nr_bags_sold_man_woman[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
#dta$outcome <- dta$nr_bags_sold_man_woman

#res_sales_m[3,1,h] <-ifelse(h %in% c(5,6), wtd.mean(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.mean(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
#res_sales_m[4,1,h] <- ifelse(h %in% c(5,6), wtd.sd(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.sd(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
#res_sales_m[3,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
#res_sales_m[4,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
#res_sales_m[3,3,h] <- ifelse(totrep >0, RI("outcome",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("sold_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
#res_sales_m[4,3,h] <- nobs(lm(as.formula(paste("outcome",treatment, sep="~")) ,data=dta))

#dta$price_sold_man_woman[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
#dta$outcome <- dta$price_sold_man_woman

#res_sales_m[5,1,h] <-ifelse(h %in% c(5,6), wtd.mean(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.mean(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
#res_sales_m[6,1,h] <- ifelse(h %in% c(5,6), wtd.sd(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.sd(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
#res_sales_m[5,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
#res_sales_m[6,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
#res_sales_m[5,3,h] <- ifelse(totrep >0, RI("outcome",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("sold_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
#res_sales_m[6,3,h] <- nobs(lm(as.formula(paste("outcome",treatment, sep="~")) ,data=dta))


#dta$sold_to_trader_man_woman[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
#dta$outcome <- dta$sold_to_trader_man_woman
#res_sales_m[7,1,h] <-ifelse(h %in% c(5,6), wtd.mean(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.mean(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
#res_sales_m[8,1,h] <- ifelse(h %in% c(5,6), wtd.sd(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.sd(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
#res_sales_m[7,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
#res_sales_m[8,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
#res_sales_m[7,3,h] <- ifelse(totrep >0, RI("outcome",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("sold_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
#res_sales_m[8,3,h] <- nobs(lm(as.formula(paste("outcome",treatment, sep="~")) ,data=dta))

#dta$sold_to_middleman_man_woman[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
#dta$outcome <- dta$sold_to_middleman_man_woman
#res_sales_m[9,1,h] <-ifelse(h %in% c(5,6), wtd.mean(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.mean(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
#res_sales_m[10,1,h] <- ifelse(h %in% c(5,6), wtd.sd(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.sd(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
#res_sales_m[9,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
#res_sales_m[10,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
#res_sales_m[9,3,h] <- ifelse(totrep >0, RI("outcome",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("sold_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
#res_sales_m[10,3,h] <- nobs(lm(as.formula(paste("outcome",treatment, sep="~")) ,data=dta))

#dta$sold_to_processor_man_woman[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
#dta$outcome <- dta$sold_to_processor_man_woman
#res_sales_m[11,1,h] <-ifelse(h %in% c(5,6), wtd.mean(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.mean(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
#res_sales_m[12,1,h] <- ifelse(h %in% c(5,6), wtd.sd(dta$outcome[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$outcome[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$outcome[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 7, wtd.sd(dta$outcome[dta$messenger != dta$recipient ],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
#res_sales_m[11,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
#res_sales_m[12,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("outcome",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
#res_sales_m[11,3,h] <- ifelse(totrep >0, RI("outcome",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("sold_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("outcome",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
#res_sales_m[12,3,h] <- nobs(lm(as.formula(paste("outcome",treatment, sep="~")) ,data=dta))



#res_sales_m[13,1:3,h] <- RI_FWER(deps= c("sold_man_woman","nr_bags_sold_man_woman","price_sold_man_woman","sold_to_trader_man_woman","sold_to_middleman_man_woman","sold_to_processor_man_woman") ,indep = treatment , ctrls = ctrls,dta =dta, p_vals = res_sales_m[c(1,3,5,7,9,11),3,h], nr_repl = totrep, w_int="weights")

}


save(res_sales_m, file = "res_sales_m.RData")

print(res_sales_m)


