rm(list=ls())
source("/home/bjvca/data/projects/digital green/endline/data/init_gender_WE.R")
baseline <-  read.csv("/home/bjvca/data/projects/digital green/baseline/base_merge.cvs")

#wget https://www.dropbox.com/s/sakp13112o1to6u/baseline.csv?dl=0
#wget https://www.dropbox.com/s/n7hn2x0y492ofgi/AWS.csv?dl=0

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
res_dec_w <- array(NA, c(13,4,5)) 
rownames(res_dec_w) <- c("dec_plant","","dec_time","","dec_space","", "dec_striga","","dec_weed","","index","","padj")
res_dec_b <- array(NA, c(13,4,5)) 
rownames(res_dec_b) <- c("dec_plant","","dec_time","","dec_space","", "dec_striga","","dec_weed","","index","","padj")

cl <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
registerDoParallel(cl)

### function definitions 
RI <- function(dep, indep, ctrls = NULL,  dta , nr_repl = 1000, h_int=h, w_int = NULL) {
# RI("(maizeeduc > 2)",treat ,ctrls = NULL, w_int="weights", dta= dta, nr_repl = totrep, h)
#indep <- "(recipient != 'male') +ivr+sms+ as.factor(messenger)" 
#ctrls <- NULL
#h_int <- 1
#dep <- "(maizeeduc > 2)"
##dta <- dta_bal
#nr_repl <- 100
#w_int <- "weights"

### determines treatmetn cell
	dta <- dta %>% 
    		mutate(treat = group_indices_(dta, .dots=c("recipient", "messenger"))) 
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
 		setDT(dta_sim)[,perm:=sample(treat),by = (uniqID)]
 		setDT(dta_sim)[,perm_ivr:=sample(ivr),by = (uniqID)]
		dta_sim$perm_ivr[is.na(dta_sim$perm_ivr)] <- "no"
		setDT(dta_sim)[perm_ivr =="yes",perm_sms:=sample(sms),by = (uniqID)]
dta_sim$perm_sms[is.na(dta_sim$perm_sms)] <- "no"
if ((h_int==1) | (h_int==4)  | (h_int==5) ) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","female"))
} else if (h_int==2) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female")
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["female",]>0]),"female","couple"))
} else if (h_int==3) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", "male")
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["female",]>0]),"female","couple"))

} 
		dta_sim$ivr <- dta_sim$perm_ivr
		dta_sim$sms <- dta_sim$perm_sms
		return(abs(ifelse(is.null(ctrls),summary(lm(as.formula(paste(dep,indep,sep="~")), weights=weights, data=data.frame(dta_sim)))$coefficients[2,1],summary(lm(as.formula(paste(paste(dep,indep,sep="~"), ctrls,sep="+")), weights=weights, data=dta_sim))$coefficients[2,1])) > abs(crit) )
	}
	return(sum(oper)/nr_repl)
}

RI_FWER <- function(deps, indep,ctrls = NULL, dta ,p_vals , nr_repl = 1000, h_int=h, w_int = NULL) {
### function to control for FWER using simulation (familywise sharp null)
### inspired on https://egap.org/methods-guides/10-things-you-need-know-about-multiple-comparisons
threshold_finder<- function(threshold){
  mean(apply(oper, 2, x <- function(x) sum(x <= threshold) > 0 ))
}
### determines treatmetn cell
	dta <- dta %>% 
    		mutate(treat = group_indices_(dta, .dots=c("recipient", "messenger"))) 
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
 		setDT(dta_sim)[,perm:=sample(treat),by = (uniqID)]
 		setDT(dta_sim)[,perm_ivr:=sample(ivr),by = (uniqID)]
		dta_sim$perm_ivr[is.na(dta_sim$perm_ivr)] <- "no"
		setDT(dta_sim)[perm_ivr =="yes",perm_sms:=sample(sms),by = (uniqID)]
dta_sim$perm_sms[is.na(dta_sim$perm_sms)] <- "no"
if ((h_int==1) | (h_int==4)  | (h_int==5) ) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","female"))
} else if (h_int==2) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female")
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["female",]>0]),"female","couple"))
} else if (h_int==3) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", "male")
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["female",]>0]),"female","couple"))

} 
		dta_sim$ivr <- dta_sim$perm_ivr
		dta_sim$sms <- dta_sim$perm_sms


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

FW_index <- function(treat, indexer, contr_vars, w_int2,data, nr_repl=0,h_int=h) {
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
	sig <- RI("index" ,treat , contr_vars, w_int= w_int2, data, nr_repl = nr_repl,h_int)
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

for (h in 1:5) {
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
############################################ H3: mes==couple or woman - mes==male ###################################################
dta <- dta_copy
ctrls<- NULL
dta$weights <- 1
dta$weights[dta$messenger == "female"] <-  1106/1108
treatment <- "(messenger != 'male') +ivr+sms+as.factor(recipient)"
} else if (h==5) {
######################## H4: messenger== 'male' & recipient=='male' - messenger!= 'male' & recipient!='male' ##############################
dta <- dta_copy
dta$recipient <- factor(dta$recipient)
dta$messenger <- factor(dta$messenger)
treatment <- "(messenger!= 'male' & recipient!='male') +ivr+sms"
dta <- merge(dta,baseline, by="hhid")
dta$weights <- 1
dta$weights[dta$messenger == "female" & dta$recipient == "female"] <-  342/384
dta$weights[dta$messenger == "male" & dta$recipient == "female"] <-  342/383
dta$weights[dta$messenger == "couple" & dta$recipient == "female"] <-  342/368
dta$weights[dta$messenger == "couple" & dta$recipient == "couple"] <-  342/369
dta$weights[dta$messenger == "female" & dta$recipient == "male"] <-  342/382
dta$weights[dta$messenger == "couple" & dta$recipient == "male"] <-  342/369
ctrls <- "maizeage+maizeeduc+maizeprinfo_receiv_spouse+maizeprinput_use+maizemobile" 
}
print(h)
################################################## decisions  #####################################################
dta_glob <- dta
#10. Who decided maize should be planted on this ${garden1} plot?
man <- "mgt_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")


dta_ind <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
share_woman_decide <- aggregate(dta_ind$decide,list(dta_ind$hhid),mean,na.rm=T)
names(share_woman_decide) <- c("hhid","share_woman_dec_maize")
dta <- merge(dta_glob,share_woman_decide,by="hhid")
dta$share_woman_dec_maize[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA


res_dec_w[1,1,h] <- ifelse(h ==5, wtd.mean(dta$share_woman_dec_maize[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$share_woman_dec_maize[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$share_woman_dec_maize[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T))))
res_dec_w[2,1,h] <-  ifelse(h ==5, wtd.sd(dta$share_woman_dec_maize[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$share_woman_dec_maize[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$share_woman_dec_maize[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T))))
res_dec_w[1,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_dec_w[2,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_dec_w[1,3,h] <- ifelse(totrep >0, RI("share_woman_dec_maize",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_dec_w[2,3,h] <- nobs(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,data=dta))

#Who decided to start planting maize on ${garden1} plot at that particular time?
man <- "dectime_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")


dta_ind <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
share_woman_decide <- aggregate(dta_ind$decide,list(dta_ind$hhid),mean,na.rm=T)
names(share_woman_decide) <- c("hhid","share_woman_dec_maize")
dta <- merge(dta_glob,share_woman_decide,by="hhid")
dta$share_woman_dec_maize[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA

res_dec_w[3,1,h] <- ifelse(h ==5, wtd.mean(dta$share_woman_dec_maize[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$share_woman_dec_maize[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$share_woman_dec_maize[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T))))
res_dec_w[4,1,h] <-  ifelse(h ==5, wtd.sd(dta$share_woman_dec_maize[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$share_woman_dec_maize[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$share_woman_dec_maize[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T))))
res_dec_w[3,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_dec_w[4,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_dec_w[3,3,h] <- ifelse(totrep >0, RI("share_woman_dec_maize",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_dec_w[4,3,h] <- nobs(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,data=dta))

#23. Who decided to use this spacing and/or seed density on ${garden1} plot?  

man <- "decspace_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

dta_ind <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
share_woman_decide <- aggregate(dta_ind$decide,list(dta_ind$hhid),mean,na.rm=T)
names(share_woman_decide) <- c("hhid","share_woman_dec_maize")
dta <- merge(dta_glob,share_woman_decide,by="hhid")
dta$share_woman_dec_maize[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA

res_dec_w[5,1,h] <- ifelse(h ==5, wtd.mean(dta$share_woman_dec_maize[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$share_woman_dec_maize[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$share_woman_dec_maize[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T))))
res_dec_w[6,1,h] <-  ifelse(h ==5, wtd.sd(dta$share_woman_dec_maize[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$share_woman_dec_maize[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$share_woman_dec_maize[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T))))
res_dec_w[5,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_dec_w[6,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_dec_w[5,3,h] <- ifelse(totrep >0, RI("share_woman_dec_maize",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_dec_w[6,3,h] <- nobs(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,data=dta))

#23. 25.Who decided  on this particular way to fight striga (kayongo)? 

man <- "decstriga_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

dta_ind <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
share_woman_decide <- aggregate(dta_ind$decide,list(dta_ind$hhid),mean,na.rm=T)
names(share_woman_decide) <- c("hhid","share_woman_dec_maize")
dta <- merge(dta_glob,share_woman_decide,by="hhid")
dta$share_woman_dec_maize[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA

res_dec_w[7,1,h] <- ifelse(h ==5, wtd.mean(dta$share_woman_dec_maize[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$share_woman_dec_maize[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$share_woman_dec_maize[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T))))
res_dec_w[8,1,h] <-  ifelse(h ==5, wtd.sd(dta$share_woman_dec_maize[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$share_woman_dec_maize[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$share_woman_dec_maize[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T))))
res_dec_w[7,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_dec_w[8,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_dec_w[7,3,h] <- ifelse(totrep >0, RI("share_woman_dec_maize",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_dec_w[8,3,h] <- nobs(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,data=dta))

#27. Who decided on when to do the first weeding for ${garden1} plot?

man <- "decweed_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

dta_ind <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
share_woman_decide <- aggregate(dta_ind$decide,list(dta_ind$hhid),mean,na.rm=T)
names(share_woman_decide) <- c("hhid","share_woman_dec_maize")
dta <- merge(dta_glob,share_woman_decide,by="hhid")
dta$share_woman_dec_maize[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA

res_dec_w[9,1,h] <- ifelse(h ==5, wtd.mean(dta$share_woman_dec_maize[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$share_woman_dec_maize[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$share_woman_dec_maize[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T))))
res_dec_w[10,1,h] <-  ifelse(h ==5, wtd.sd(dta$share_woman_dec_maize[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$share_woman_dec_maize[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$share_woman_dec_maize[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T))))
res_dec_w[9,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_dec_w[10,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_dec_w[9,3,h] <- ifelse(totrep >0, RI("share_woman_dec_maize",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_dec_w[10,3,h] <- nobs(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,data=dta))

man <- "mgt_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_mgt_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_mgt_woman) <- c("hhid","time","mgt_woman")
man <- "dectime_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_dectime_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_dectime_woman) <- c("hhid","time","dectime_woman")
man <- "decspace_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decspace_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decspace_woman) <-  c("hhid","time","decspace_woman")
man <- "decstriga_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decstriga_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decstriga_woman) <-  c("hhid","time","decstriga_woman")
man <- "decweed_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decweed_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decweed_woman) <-  c("hhid","time","decweed_woman")



all_ind <- merge(merge(merge(merge(dta_ind_mgt_woman,dta_ind_dectime_woman),dta_ind_decspace_woman),dta_ind_decstriga_woman), dta_ind_decweed_woman )
all_hh <- aggregate(all_ind[c( "mgt_woman"  ,  "dectime_woman", "decspace_woman", "decstriga_woman","decweed_woman")],list(all_ind$hhid),mean,na.rm=T)
names(all_hh)[names(all_hh) == 'Group.1'] <- 'hhid'

dta <- merge(dta_glob,all_hh,by="hhid")
dta$mgt_woman[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
indexer <- FW_index(treatment,c( "mgt_woman"  ,  "dectime_woman", "decspace_woman", "decstriga_woman","decweed_woman"),ctrls,w_int="weights",dta, nr_repl=totrep,h_int = h)



res_dec_w[11,1,h] <- ifelse(h ==5, wtd.mean(indexer[[3]]$index[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(indexer[[3]]$index[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(indexer[[3]]$index[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T))))
res_dec_w[12,1,h] <-  ifelse(h ==5, wtd.sd(indexer[[3]]$index[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(indexer[[3]]$index[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(indexer[[3]]$index[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T))))
res_dec_w[11,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_dec_w[12,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_dec_w[11,3,h] <-  indexer[[2]]
res_dec_w[12,3,h] <-  nobs(indexer[[1]])


res_dec_w[13,1:3,h] <- RI_FWER(deps= c( "mgt_woman"  ,  "dectime_woman", "decspace_woman", "decstriga_woman","decweed_woman") ,indep = treatment , ctrls = ctrls,dta =dta, p_vals = res_dec_w[c(1,3,5,7,9),3,h], nr_repl = totrep,h_int=h, w_int="weights")

################################################## decisions - jointly made #####################################################

#10. Who decided maize should be planted on this ${garden1} plot?
man <- "mgt_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")


dta_ind <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
share_woman_decide <- aggregate(dta_ind$decide,list(dta_ind$hhid),mean,na.rm=T)
names(share_woman_decide) <- c("hhid","share_woman_dec_maize")
dta <- merge(dta_glob,share_woman_decide,by="hhid")
dta$share_woman_dec_maize[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA

res_dec_b[1,1,h] <- ifelse(h ==5, wtd.mean(dta$share_woman_dec_maize[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$share_woman_dec_maize[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$share_woman_dec_maize[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T))))
res_dec_b[2,1,h] <-  ifelse(h ==5, wtd.sd(dta$share_woman_dec_maize[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$share_woman_dec_maize[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$share_woman_dec_maize[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T))))
res_dec_b[1,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_dec_b[2,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_dec_b[1,3,h] <- ifelse(totrep >0, RI("share_woman_dec_maize",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_dec_b[2,3,h] <- nobs(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,data=dta))

#Who decided to start planting maize on ${garden1} plot at that particular time?
man <- "dectime_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")


dta_ind <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
share_woman_decide <- aggregate(dta_ind$decide,list(dta_ind$hhid),mean,na.rm=T)
names(share_woman_decide) <- c("hhid","share_woman_dec_maize")
dta <- merge(dta_glob,share_woman_decide,by="hhid")
dta$share_woman_dec_maize[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA

res_dec_b[3,1,h] <- ifelse(h ==5, wtd.mean(dta$share_woman_dec_maize[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$share_woman_dec_maize[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$share_woman_dec_maize[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T))))
res_dec_b[4,1,h] <-  ifelse(h ==5, wtd.sd(dta$share_woman_dec_maize[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$share_woman_dec_maize[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$share_woman_dec_maize[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T))))
res_dec_b[3,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_dec_b[4,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_dec_b[3,3,h] <- ifelse(totrep >0, RI("share_woman_dec_maize",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_dec_b[4,3,h] <- nobs(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,data=dta))

#23. Who decided to use this spacing and/or seed density on ${garden1} plot?  

man <- "decspace_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

dta_ind <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
share_woman_decide <- aggregate(dta_ind$decide,list(dta_ind$hhid),mean,na.rm=T)
names(share_woman_decide) <- c("hhid","share_woman_dec_maize")
dta <- merge(dta_glob,share_woman_decide,by="hhid")
dta$share_woman_dec_maize[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA

res_dec_b[5,1,h] <- ifelse(h ==5, wtd.mean(dta$share_woman_dec_maize[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$share_woman_dec_maize[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$share_woman_dec_maize[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T))))
res_dec_b[6,1,h] <-  ifelse(h ==5, wtd.sd(dta$share_woman_dec_maize[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$share_woman_dec_maize[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$share_woman_dec_maize[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T))))
res_dec_b[5,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_dec_b[6,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_dec_b[5,3,h] <- ifelse(totrep >0, RI("share_woman_dec_maize",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_dec_b[6,3,h] <- nobs(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,data=dta))

#23. 25.Who decided  on this particular way to fight striga (kayongo)? 

man <- "decstriga_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

dta_ind <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
share_woman_decide <- aggregate(dta_ind$decide,list(dta_ind$hhid),mean,na.rm=T)
names(share_woman_decide) <- c("hhid","share_woman_dec_maize")
dta <- merge(dta_glob,share_woman_decide,by="hhid")
dta$share_woman_dec_maize[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA

res_dec_b[7,1,h] <- ifelse(h ==5, wtd.mean(dta$share_woman_dec_maize[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$share_woman_dec_maize[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$share_woman_dec_maize[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T))))
res_dec_b[8,1,h] <-  ifelse(h ==5, wtd.sd(dta$share_woman_dec_maize[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$share_woman_dec_maize[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$share_woman_dec_maize[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T))))
res_dec_b[7,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_dec_b[8,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_dec_b[7,3,h] <- ifelse(totrep >0, RI("share_woman_dec_maize",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_dec_b[8,3,h] <- nobs(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,data=dta))

#27. Who decided on when to do the first weeding for ${garden1} plot?

man <- "decweed_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

dta_ind <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
share_woman_decide <- aggregate(dta_ind$decide,list(dta_ind$hhid),mean,na.rm=T)
names(share_woman_decide) <- c("hhid","share_woman_dec_maize")
dta <- merge(dta_glob,share_woman_decide,by="hhid")
dta$share_woman_dec_maize[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA

res_dec_b[9,1,h] <- ifelse(h ==5, wtd.mean(dta$share_woman_dec_maize[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$share_woman_dec_maize[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$share_woman_dec_maize[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T))))
res_dec_b[10,1,h] <-  ifelse(h ==5, wtd.sd(dta$share_woman_dec_maize[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$share_woman_dec_maize[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$share_woman_dec_maize[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T))))
res_dec_b[9,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_dec_b[10,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_dec_b[9,3,h] <- ifelse(totrep >0, RI("share_woman_dec_maize",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("share_woman_dec_maize",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_dec_b[10,3,h] <- nobs(lm(as.formula(paste("share_woman_dec_maize",treatment, sep="~")) ,data=dta))


man <- "mgt_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_mgt_both_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_mgt_both_woman) <- c("hhid","time","mgt_both_woman")
man <- "dectime_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_dectime_both_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_dectime_both_woman) <- c("hhid","time","dectime_both_woman")
man <- "decspace_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decspace_both_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decspace_both_woman) <-  c("hhid","time","decspace_both_woman")
man <- "decstriga_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decstriga_both_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decstriga_both_woman) <-  c("hhid","time","decstriga_both_woman")
man <- "decweed_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decweed_both_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decweed_both_woman) <-  c("hhid","time","decweed_both_woman")


all_ind <- merge(merge(merge(merge(dta_ind_mgt_both_woman,dta_ind_dectime_both_woman),dta_ind_decspace_both_woman),dta_ind_decstriga_both_woman), dta_ind_decweed_both_woman )
all_hh <- aggregate(all_ind[c( "mgt_both_woman"  ,  "dectime_both_woman", "decspace_both_woman", "decstriga_both_woman","decweed_both_woman")],list(all_ind$hhid),mean,na.rm=T)
names(all_hh)[names(all_hh) == 'Group.1'] <- 'hhid'


dta <- merge(dta_glob,all_hh,by="hhid")
dta$mgt_both_woman[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
indexer <- FW_index(treatment,c( "mgt_both_woman"  ,  "dectime_both_woman", "decspace_both_woman", "decstriga_both_woman","decweed_both_woman"),ctrls,w_int="weights",dta, nr_repl=totrep,h_int = h)



res_dec_b[11,1,h] <- ifelse(h ==5, wtd.mean(indexer[[3]]$index[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(indexer[[3]]$index[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(indexer[[3]]$index[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T))))
res_dec_b[12,1,h] <-  ifelse(h ==5, wtd.sd(indexer[[3]]$index[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(indexer[[3]]$index[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(indexer[[3]]$index[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T))))
res_dec_b[11,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_dec_b[12,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_dec_b[11,3,h] <-  indexer[[2]]
res_dec_b[12,3,h] <-  nobs(indexer[[1]])


res_dec_b[13,1:3,h] <- RI_FWER(deps= c( "mgt_both_woman"  ,  "dectime_both_woman", "decspace_both_woman", "decstriga_both_woman","decweed_both_woman") ,indep = treatment , ctrls = ctrls,dta =dta, p_vals = res_dec_w[c(1,3,5,7,9),3,h], nr_repl = totrep,h_int=h, w_int="weights")
}





