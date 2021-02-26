rm(list=ls())
stopCluster(cl)
source("/home/bjvca/data/projects/digital green/papers/DP_cooperation/analysis/init_gender.R")
baseline <-  read.csv("/home/bjvca/data/projects/digital green/baseline/base_merge.cvs")

## uncomment lines below if run on AWS
#wget https://www.dropbox.com/s/41h1i1icz5gmatx/AWS_coop.csv?dl=0
#wget https://www.dropbox.com/s/s4xne419eyhgsf0/base_coop.cvs?dl=0
#install.packages(c("ggplot2","doParallel","data.table","dplyr","Hmisc"))

#rm(list=ls())
#dta <- read.csv("AWS_coop.csv")
#baseline <- read.csv("base_coop.csv")


library(ggplot2)
library(doParallel)
library(data.table)
library(dplyr)
library(Hmisc)

set.seed(07032018)

### indexing results arrays

### indexing results arrays
res_itt_know <- array(NA, c(11,4,6)) 
rownames(res_itt_know) <- c("know_space","","know_combine","","know_weed","", "know_armyworm","","know_ind","","p-vals")
res_itt_pract <- array(NA, c(23,4,6))
rownames(res_itt_pract) <- c("first_day","","space","","striga","","weed","", "use_fert","","seed","","combiner","","bought_seed","","chem","","labour","","pract_index","","p-vals")
res_itt_fert <- array(NA, c(9,4,6))
rownames(res_itt_fert) <- c("use_DAP","","use_urea","","use_organic","","fert_index","","p-vals")
res_itt_seed <- array(NA, c(7,4,6))
rownames(res_itt_seed) <- c("hybrid","","opv","","seed_inded","","p-vals")
res_itt_prod <- array(NA, c(15,4,6))
rownames(res_itt_prod) <- c("prod","","area","","yield","","yield_better","","labour","","labour_prod","","prod_index","","p-vals")

fert_plot <- data.frame(matrix(NA, 4,4))
names(fert_plot) <- c("x","y","ylo","yhi")
seed_plot <- data.frame(matrix(NA, 4,4))
names(seed_plot) <- c("x","y","ylo","yhi")

res_itt_wel <-  array(NA, c(13,4,6))
rownames(res_itt_wel) <- c("better_av","","better_6m","","eatpref","","eatenough","","log_cons","","welfare_index","","p-vals")

prod_plot <- array(NA, c(6,4,6))
colnames(prod_plot) <-  c("x","y","ylo","yhi")

cl <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
registerDoParallel(cl)
### function definitions 

trim <- function(var, dataset, trim_perc=.1) {
### function for triming a dataset *dataset* on a variable *var*
return( subset(dataset,dataset[var] > quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)), na.rm=T)[1] & dataset[var] < quantile(dataset[var], c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[2]) )
}



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
		return(abs(ifelse(is.null(ctrls),summary(lm(as.formula(paste(dep,indep,sep="~")), weights=unlist(dta_sim$w_int), data=data.frame(dta_sim)))$coefficients[2,1],summary(lm(as.formula(paste(paste(dep,indep,sep="~"), ctrls,sep="+")), weights=unlist(dta_sim$w_int), data=dta_sim))$coefficients[2,1])) > abs(crit) )
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
return(unlist(lapply(deps, function(dvar)  summary(lm(as.formula(paste(dvar,indep,sep="~")), weights==unlist(dta_sim$w_int),data=dta_sim))$coefficients[2,4])))
} else {
return(unlist(lapply(deps, function(dvar)  summary(lm(as.formula(paste(paste(dvar,indep,sep="~"),ctrls, sep="+")), weights=unlist(dta_sim$w_int),data=dta_sim))$coefficients[2,4])))
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
mod <- lm(as.formula(paste(paste("index",treat,sep="~"),contr_vars, sep="+")) ,weights=unlist(data[w_int2]), data=data)

					
if (nr_repl > 0) { 
	data$index <- as.vector(data$index)
	sig <- RI("index" ,treat , contr_vars, w_int= w_int2, data, nr_repl = nr_repl)
} else {
	sig <- summary(lm(as.formula(paste(paste("index",treat,sep="~"), contr_vars, sep="+")) ,weights=unlist(data[w_int2]), data=data))$coefficients[2,4]
}
return(list(mod,sig, data))
}

### wrapper function to make graphs to be used for presentations
library(dplyr)
library(plyr)
credplot.gg <- function(d,units, hypo, axlabs, lim){
 # d is a data frame with 4 columns
 # d$x gives variable names
 # d$y gives center point
 # d$ylo gives lower limits
 # d$yhi gives upper limits
 require(ggplot2)
 p <- ggplot(d, aes(x=x, y=y, ymin=ylo, ymax=yhi, colour=as.factor(grp)))+
 geom_pointrange(position=position_dodge(-.4), size=1)+
 geom_hline(yintercept = 0, linetype=2)+
 coord_flip(ylim = c(-lim,lim))+
 xlab('') + ylab(units)+ labs(title=hypo)  + theme_minimal()+ theme(axis.text=element_text(size=18),
        axis.title=element_text(size=14,face="bold"),legend.text=element_text(size=18), plot.title = element_text(size=22,hjust = 0.5), legend.title=element_blank())+
    geom_errorbar(aes(ymin=ylo, ymax=yhi),position=position_dodge(-.4),width=0,cex=2.5) + scale_colour_manual(values = c('#25276d','#00d8ff','#cb00ff')) + scale_x_discrete(labels=axlabs)
 return(p)
}


#########################################################################################

## drop the control
dta <- subset(dta, messenger != "ctrl")
dta <- subset(dta, interview_status == "couple interviewed")

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



dta$weight_av <- 1
dta$weight_av[dta$recipient=="male" & dta$messenger=="female"] <- 235/273
dta$weight_av[dta$recipient=="male" & dta$messenger=="couple"] <- 235/268
dta$weight_av[dta$recipient=="male" & dta$messenger=="male"] <- 235/273
dta$weight_av[dta$recipient=="female" & dta$messenger=="female"] <- 235/247
dta$weight_av[dta$recipient=="female" & dta$messenger=="couple"] <- 235/272
dta$weight_av[dta$recipient=="female" & dta$messenger=="male"] <- 235/265
dta$weight_av[dta$recipient=="couple" & dta$messenger=="female"] <- 235/235
dta$weight_av[dta$recipient=="couple" & dta$messenger=="couple"] <- 309/261
dta$weight_av[dta$recipient=="couple" & dta$messenger=="male"] <- 235/240

dta_copy <- dta


#set totrep to zero if you do not want simulation based inferecne
totrep <- 0
####

for (h in c(1,5)) {
if (h==1) {
############################################ H1: info asymmetry: rec=individual vs rec=couple #########################################################
dta <- dta_copy
dta$weight <- 1
dta$weight[dta$recipient == "female"] <-  1
dta$weight[dta$recipient == "male"] <- 811/814
dta <- merge(dta,baseline, by="hhid")

ctrls <- "maizeage+maizeeduc+maizehh_no+maizeprinfo_receiv+maizeprinfo_receiv_spouse+maizemobile" 

treatment <- "(recipient == 'couple') +as.factor(messenger)*ivr*sms + ivr*(recipient == 'couple')*sms" 
} else if (h==4) {
############################################ H2: promote collective approach ###################################################
dta <- dta_copy
dta$weight <- 1
dta$weight[dta$messenger == "female"] <-  1
dta$weight[dta$messenger == "male"] <- 1102/1114
treatment <- "(messenger == 'couple') +as.factor(recipient)*ivr*sms + ivr*(messenger == 'couple')*sms"
dta <- merge(dta,baseline, by="hhid")
ctrls <- "maizeprinput_use" 
} else if (h==2) {
############################################ H1a: info assym men vs couple ###################################################
dta <- subset(dta_copy, recipient != "female")
dta$weight <- 1
treatment <- "(recipient == 'couple') +as.factor(messenger)*ivr*sms + ivr*(recipient == 'couple')*sms" 
dta <- merge(dta,baseline, by="hhid")
ctrls <- "maizeprinfo_receiv_spouse+maizeprinput_use" 
} else if (h==3) {
############################################ H1b: info assym women vs couple ###################################################
dta <- subset(dta_copy, recipient != "male")
dta$weight <- 1
treatment <- "(recipient == 'couple') +as.factor(messenger)*ivr*sms + ivr*(recipient == 'couple')*sms" 
dta <- merge(dta,baseline, by="hhid")
ctrls <- "maizeage+maizeeduc+maizehh_no+maizeprinfo_receiv+maizemobile" 
} else if (h==5) {
############################################ H2a: info assym men vs couple ###################################################
dta <- subset(dta_copy, messenger != "female")
dta$weight <- 1
treatment <- "(messenger == 'couple') +as.factor(recipient)*ivr*sms + ivr*(messenger == 'couple')*sms" 
dta <- merge(dta,baseline, by="hhid")
ctrls <- "maizeprinput_use" 

} else if (h==6) {
############################################ H2b: info assym women vs couple ###################################################
dta <- subset(dta_copy, messenger != "male")
dta$weight <- 1
treatment <- "(messenger == 'couple') +as.factor(recipient)*ivr*sms + ivr*(messenger == 'couple')*sms"
dta <- merge(dta,baseline, by="hhid")
ctrls <- "maizeprinput_use" 

}

print(h)
dta_bal <- dta


################################ knowledge  ############################

res_itt_know[1,1,h] <- mean(dta_bal$know_space,na.rm=T)
res_itt_know[2,1,h] <- sd(dta_bal$know_space,na.rm=T)
res_itt_know[1,2,h] <- summary(lm(as.formula(paste(paste("know_space",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_know[2,2,h] <- summary(lm(as.formula(paste(paste("know_space",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_know[1,3,h] <- ifelse(totrep >0, RI("know_space", treatment ,ctrls, dta_bal,  totrep, "weight"),summary(lm(as.formula(paste(paste("know_space",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,4])
res_itt_know[1,4,h] <- nobs(lm(as.formula(paste(paste("know_space",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))

res_itt_know[3,1,h] <-  mean(dta_bal$know_combine,na.rm=T)
res_itt_know[4,1,h] <-  sd(dta_bal$know_combine,na.rm=T)
res_itt_know[3,2,h] <- summary(lm(as.formula(paste(paste("know_combine",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_know[4,2,h] <- summary(lm(as.formula(paste(paste("know_combine",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_know[3,3,h] <-  ifelse(totrep >0, RI("know_combine",treatment ,ctrls, dta_bal, nr_repl = totrep,  "weight"),summary(lm(as.formula(paste(paste("know_combine",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,4])
res_itt_know[3,4,h] <- nobs(lm(as.formula(paste(paste("know_combine",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))

res_itt_know[5,1,h] <-  mean(dta_bal$know_weed,na.rm=T)
res_itt_know[6,1,h] <-  sd(dta_bal$know_weed,na.rm=T)
res_itt_know[5,2,h] <- summary(lm(as.formula(paste(paste("know_weed",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_know[6,2,h] <- summary(lm(as.formula(paste(paste("know_weed",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_know[5,3,h] <-  ifelse(totrep >0, RI("know_weed",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"),summary(lm(as.formula(paste(paste("know_weed",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,4])
res_itt_know[5,4,h] <- nobs(lm(as.formula(paste(paste("know_weed",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))

res_itt_know[7,1,h] <- mean(dta_bal$know_armyworm,na.rm=T)
res_itt_know[8,1,h] <- sd(dta_bal$know_armyworm,na.rm=T)
res_itt_know[7,2,h] <- summary(lm(as.formula(paste(paste("know_armyworm",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_know[8,2,h] <- summary(lm(as.formula(paste(paste("know_armyworm",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_know[7,3,h] <-  ifelse(totrep >0, RI("know_armyworm",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"),summary(lm(as.formula(paste(paste("know_armyworm",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,4])
res_itt_know[7,4,h] <- nobs(lm(as.formula(paste(paste("know_armyworm",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))

## no need to include armyworm in the index because we do not really expect an effect
indexer <- FW_index(treatment, c("know_space", "know_combine", "know_weed"),ctrls,w_int2 = "weight", data =dta_bal, nr_repl=totrep )
res_itt_know[9,1,h] <-  mean(indexer[[3]]$index,na.rm=T)
res_itt_know[10,1,h] <-  sd(indexer[[3]]$index,na.rm=T)
res_itt_know[9,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_know[10,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_know[9,3,h] <-  indexer[[2]]
res_itt_know[9,4,h]  <- nobs(indexer[[1]])

prod_plot[1,1,h] <- "knowledge"
prod_plot[1,3:4,h] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index)
prod_plot[1,2,h] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index)

##calculated corrected critical values
print(h)

res_itt_know[11,1:3 ,h] <- RI_FWER(deps= c("know_space","know_combine","know_weed") ,indep = treatment, ctrls=ctrls ,dta =dta_bal, p_vals = res_itt_know[c(1,3,5),3,h], nr_repl = totrep, w_int="weight")

################################### practices #############################

###timely planting
res_itt_pract[1,1,h]  <- mean(dta_bal$days_after_rain, na.rm=T)
res_itt_pract[2,1,h]  <- sd(dta_bal$days_after_rain, na.rm=T)
res_itt_pract[1,2,h]  <- summary(lm(as.formula(paste(paste("days_after_rain", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_pract[2,2,h]  <- summary(lm(as.formula(paste(paste("days_after_rain", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_pract[1,3,h]  <- ifelse(totrep >0, RI("days_after_rain",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("days_after_rain", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4]) 
res_itt_pract[1,4,h]  <- nobs(lm(as.formula(paste(paste("days_after_rain", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

### used recommended spacing use on at lease one plot as reported by at least one spouse
res_itt_pract[3,1,h]  <- mean(dta_bal$space, na.rm=T)
res_itt_pract[4,1,h]  <- sd(dta_bal$space, na.rm=T)
res_itt_pract[3,2,h]  <- summary(lm(as.formula(paste(paste("space", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_pract[4,2,h]  <- summary(lm(as.formula(paste(paste("space", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_pract[3,3,h]  <- ifelse(totrep >0, RI("space",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"),  summary(lm(as.formula(paste(paste("space", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4])
res_itt_pract[3,4,h]  <- nobs(lm(as.formula(paste(paste("space", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

# used recommended way to fight striga - this should be changed to include info of all plots 
res_itt_pract[5,1,h]  <- mean(dta_bal$striga, na.rm=T)
res_itt_pract[6,1,h]  <- sd(dta_bal$striga, na.rm=T)
res_itt_pract[5,2,h]  <- summary(lm(as.formula(paste(paste("striga", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_pract[6,2,h]  <- summary(lm(as.formula(paste(paste("striga", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_pract[5,3,h]  <- ifelse(totrep >0, RI("striga",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("striga", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4])
res_itt_pract[5,4,h]  <- nobs(lm(as.formula(paste(paste("striga", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

# weeded on recommended timing? - this should be changed to include info of all plots 
res_itt_pract[7,1,h]  <- mean(dta_bal$weed, na.rm=T)
res_itt_pract[8,1,h]  <- sd(dta_bal$weed, na.rm=T)
res_itt_pract[7,2,h]  <- summary(lm(as.formula(paste(paste("weed", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_pract[8,2,h]  <- summary(lm(as.formula(paste(paste("weed", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_pract[7,3,h]  <- ifelse(totrep >0, RI("weed",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"),summary(lm(as.formula(paste(paste("weed_both", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4])
res_itt_pract[7,4,h]  <- nobs(lm(as.formula(paste(paste("weed", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

# fertilizer use
#res_itt_pract[9,1,h]  <-  mean(dta_bal$fert[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_pract[10,1,h]  <-  sd(dta_bal$fert[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_pract[9,2,h]  <- summary(lm(as.formula(paste(paste("fert", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
#res_itt_pract[10,2,h]  <- summary(lm(as.formula(paste(paste("fert", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
#res_itt_pract[9,3,h]  <- ifelse(totrep >0, RI("fert",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("fert", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4]) 
#res_itt_pract[9,4,h]  <- nobs(lm(as.formula(paste(paste("fert", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

##fert_plot[1,1] <- "all fertilizer"
##fert_plot[1,3:4] <- confint(lm(as.formula(paste(paste("fert", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight), level=.9)[2,] /  mean(dta_bal$fert[dta_bal$messenger == "ctrl"], na.rm=T)
##fert_plot[1,2] <-  summary(lm(as.formula(paste(paste("fert", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1] /  mean(dta_bal$fert[dta_bal$messenger == "ctrl"], na.rm=T)

#### fert = DAP/NPK
res_itt_fert[1,1,h]  <-  mean(dta_bal$fert_dap, na.rm=T)
res_itt_fert[2,1,h]  <-  sd(dta_bal$fert_dap, na.rm=T)
res_itt_fert[1,2,h]  <- summary(lm(as.formula(paste(paste("fert_dap", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_fert[2,2,h]  <- summary(lm(as.formula(paste(paste("fert_dap", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_fert[1,3,h]  <- ifelse(totrep >0, RI("fert_dap",treatment , ctrls, dta_bal, nr_repl = totrep, "weight") , summary(lm(as.formula(paste(paste("fert_dap", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4])
res_itt_fert[1,4,h]  <- nobs(lm(as.formula(paste(paste("fert_dap", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

##fert_plot[2,1] <- "DAP/NPK"
##fert_plot[2,3:4] <- confint(lm(as.formula(paste(paste("fert_dap", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight), level=.9)[2,] /  mean(dta_bal$fert_dap[dta_bal$messenger == "ctrl"], na.rm=T)
##fert_plot[2,2] <-  summary(lm(as.formula(paste(paste("fert_dap", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1] /  mean(dta_bal$fert_dap[dta_bal$messenger == "ctrl"], na.rm=T)

#### fert = urea
res_itt_fert[3,1,h]  <-  mean(dta_bal$fert_urea, na.rm=T)
res_itt_fert[4,1,h]  <-  sd(dta_bal$fert_urea, na.rm=T)
res_itt_fert[3,2,h]  <- summary(lm(as.formula(paste(paste("fert_urea", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_fert[4,2,h]  <- summary(lm(as.formula(paste(paste("fert_urea", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_fert[3,3,h]  <- ifelse(totrep >0, RI("fert_urea",treatment , ctrls, dta_bal, nr_repl = totrep, "weight") , summary(lm(as.formula(paste(paste("fert_urea_both", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4])
res_itt_fert[3,4,h]  <- nobs(lm(as.formula(paste(paste("fert_urea", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

##fert_plot[3,1] <- "urea"
##fert_plot[3,3:4] <- confint(lm(as.formula(paste(paste("fert_urea", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight), level=.9)[2,] /  mean(dta_bal$fert_urea[dta_bal$messenger == "ctrl"], na.rm=T)
##fert_plot[3,2] <-  summary(lm(as.formula(paste(paste("fert_urea", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1] /  mean(dta_bal$fert_urea[dta_bal$messenger == "ctrl"], na.rm=T)

#### fert = organic
res_itt_fert[5,1,h]  <-  mean(dta_bal$fert_org, na.rm=T)
res_itt_fert[6,1,h]  <-  sd(dta_bal$fert_org, na.rm=T)
res_itt_fert[5,2,h]  <-  summary(lm(as.formula(paste(paste("fert_org", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_fert[6,2,h]  <- summary(lm(as.formula(paste(paste("fert_org", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_fert[5,3,h]  <- ifelse(totrep >0, RI("fert_org",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("fert_org", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4]) 
res_itt_fert[5,4,h]  <-  nobs(lm(as.formula(paste(paste("fert_org", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

##fert_plot[4,1] <- "organic"
##fert_plot[4,3:4] <- confint(lm(as.formula(paste(paste("fert_org", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight), level=.9)[2,] /  mean(dta_bal$fert_org[dta_bal$messenger == "ctrl"], na.rm=T)
##fert_plot[4,2] <-  summary(lm(as.formula(paste(paste("fert_org", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1] /  mean(dta_bal$fert_org[dta_bal$messenger == "ctrl"], na.rm=T)

#dta_bal$fert_dap <- !(dta_bal$fert_dap) 
#dta_bal$fert_org <- !(dta_bal$fert_org) 
indexer <-  FW_index(treatment,c("fert_dap","fert_urea","fert_org"),ctrls,w_int2 = "weight", data =dta_bal, nr_repl=totrep )
res_itt_fert[7,1,h] <-  mean(indexer[[3]]$index)
res_itt_fert[8,1,h] <-  sd(indexer[[3]]$index)
res_itt_fert[7,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_fert[8,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_fert[7,3,h] <-  indexer[[2]]
res_itt_fert[7,4,h] <-  nobs(indexer[[1]])

prod_plot[3,1,h] <- "fertilizer"
prod_plot[3,3:4,h] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index)
prod_plot[3,2,h] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index)


###improved seed  
#res_itt_pract[11,1,h]  <-  mean(dta_bal$impseed[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_pract[12,1,h]  <-  sd(dta_bal$impseed[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_pract[11,2,h]  <- summary(lm(as.formula(paste(paste("impseed", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
#res_itt_pract[12,2,h]  <- summary(lm(as.formula(paste(paste("impseed", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
#res_itt_pract[11,3,h]  <- ifelse(totrep >0, RI("impseed",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"),  summary(lm(as.formula(paste(paste("impseed", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4]) 
#res_itt_pract[11,4,h]  <- nobs(lm(as.formula(paste(paste("impseed", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

##seed_plot[1,1] <- "all seed"
##seed_plot[1,3:4] <- confint(lm(as.formula(paste(paste("impseed", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight), level=.9)[2,] /  mean(dta_bal$impseed[dta_bal$messenger == "ctrl"], na.rm=T)
##seed_plot[1,2] <-  summary(lm(as.formula(paste(paste("impseed", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1] /  mean(dta_bal$impseed[dta_bal$messenger == "ctrl"], na.rm=T)



### hybrid
res_itt_seed[1,1,h]  <-  mean(dta_bal$hybrid, na.rm=T)
res_itt_seed[2,1,h]  <-  sd(dta_bal$hybrid, na.rm=T)
res_itt_seed[1,2,h] <- summary(lm(as.formula(paste(paste("hybrid", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_seed[2,2,h] <- summary(lm(as.formula(paste(paste("hybrid", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_seed[1,3,h] <- ifelse(totrep >0, RI("hybrid",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("hybrid", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4]) 
res_itt_seed[1,4,h] <- nobs(lm(as.formula(paste(paste("hybrid", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

##seed_plot[2,1] <- "hybrid"
##seed_plot[2,3:4] <- confint(lm(as.formula(paste(paste("hybrid", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight), level=.9)[2,] /  mean(dta_bal$hybrid[dta_bal$messenger == "ctrl"], na.rm=T)
##seed_plot[2,2] <-  summary(lm(as.formula(paste(paste("hybrid", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1] /  mean(dta_bal$hybrid[dta_bal$messenger == "ctrl"], na.rm=T)

### opv
res_itt_seed[3,1,h]  <-  mean(dta_bal$opv, na.rm=T)
res_itt_seed[4,1,h]  <-  sd(dta_bal$opv, na.rm=T)
res_itt_seed[3,2,h] <- summary(lm(as.formula(paste(paste("opv", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_seed[4,2,h] <- summary(lm(as.formula(paste(paste("opv", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_seed[3,3,h] <- ifelse(totrep >0, RI("opv",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("opv", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4]) 
res_itt_seed[3,4,h] <- nobs(lm(as.formula(paste(paste("opv", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

indexer <-  FW_index(treatment,c("hybrid","opv"),ctrls,w_int2 = "weight", data =dta_bal, nr_repl=totrep )
res_itt_seed[5,1,h] <-  mean(indexer[[3]]$index)
res_itt_seed[6,1,h] <-  sd(indexer[[3]]$index)
res_itt_seed[5,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_seed[6,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_seed[5,3,h] <-  indexer[[2]]
res_itt_seed[6,4,h] <-  nobs(indexer[[1]])

prod_plot[4,1,h] <- "seed"
prod_plot[4,3:4,h] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index)
prod_plot[4,2,h] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index)

## shorter after rain the better
dta_bal$days_after_rain <- -dta_bal$days_after_rain 

indexer <-  FW_index(treatment,c("days_after_rain","space","striga","weed"),ctrls,w_int2 = "weight", data =dta_bal, nr_repl=totrep )
res_itt_pract[21,1,h] <-  mean(indexer[[3]]$index)
res_itt_pract[22,1,h] <-  sd(indexer[[3]]$index)
res_itt_pract[21,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_pract[22,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_pract[21,3,h] <-  indexer[[2]]
res_itt_pract[21,4,h] <-  nobs(indexer[[1]])

prod_plot[2,1,h] <- "adoption"
prod_plot[2,3:4,h] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index)
prod_plot[2,2,h] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index)

##res_itt_pract <- Bcorr( c("day_one","space","striga","weed", "fert","impseed", "combiner","bought_seed","chem","labour"),dta_bal, res_itt_pract ,h)
##res_itt_fert <- Bcorr(c("fert_dap","fert_urea","fert_org"),dta_bal, res_itt_fert ,h)
##res_itt_seed <- Bcorr(c("hybrid","opv"),dta_bal, res_itt_seed ,h)

res_itt_pract[23,1:3 ,h] <- RI_FWER(c("day_one","space","striga","weed"),indep = treatment, ctrls=ctrls ,dta =dta_bal, p_vals = res_itt_pract[c(1,3,5,7),3,h], nr_repl = totrep, w_int="weight")

res_itt_fert[9,1:3 ,h] <- RI_FWER(c("fert_dap","fert_urea","fert_org"),indep = treatment, ctrls=ctrls ,dta =dta_bal, p_vals = res_itt_fert[c(1,3,5),3,h], nr_repl = totrep, w_int="weight")

res_itt_seed[7,1:3 ,h] <-  RI_FWER(c("hybrid","opv"),indep = treatment, ctrls=ctrls ,dta =dta_bal, p_vals = res_itt_seed[c(1,3),3,h], nr_repl = totrep, w_int="weight")

################################# production ###########################
####### does the video increases production related outcomes?

#trimming is done on end result
dta_bal2 <- subset(dta_bal, prod_tot>0)
dta_bal2$log_prod_tot <- log(dta_bal2$prod_tot)
dta_trim <- trim("log_prod_tot", dta_bal2, .05)

### production
res_itt_prod[1,1,h] <- mean(dta_trim$log_prod_tot, na.rm=T)
res_itt_prod[2,1,h] <- sd(dta_trim$log_prod_tot, na.rm=T)
res_itt_prod[1,2,h] <- summary(lm(as.formula(paste(paste("log_prod_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,1]
res_itt_prod[2,2,h] <- summary(lm(as.formula(paste(paste("log_prod_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,2]
res_itt_prod[1,3,h] <- ifelse(totrep >0, RI("log_prod_tot",treatment , ctrls, dta_trim, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("log_prod_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,4])
res_itt_prod[1,4,h] <- nobs(lm(as.formula(paste(paste("log_prod_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))

### area
dta_bal2 <- subset(dta_bal, area_tot>0)
dta_bal2$log_area_tot <- log(dta_bal2$area_tot)
dta_trim <- trim("log_area_tot", dta_bal2, .05)

res_itt_prod[3,1,h] <- mean(dta_trim$log_area_tot, na.rm=T)
res_itt_prod[4,1,h] <- sd(dta_trim$log_area_tot, na.rm=T)
res_itt_prod[3,2,h] <- summary(lm(as.formula(paste(paste("log_area_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,1]
res_itt_prod[4,2,h] <- summary(lm(as.formula(paste(paste("log_area_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,2]
res_itt_prod[3,3,h] <- ifelse(totrep >0, RI("log_area_tot",treatment , ctrls, dta_trim, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("log_area_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,4])
res_itt_prod[3,4,h] <- nobs(lm(as.formula(paste(paste("log_area_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))

###yield

dta_bal2 <- subset(dta_bal, yield_av >0)
dta_bal2$log_yield_av <- log(dta_bal2$yield_av)
dta_trim <- trim("log_yield_av", dta_bal2, .05)

res_itt_prod[5,1,h] <- mean(dta_trim$log_yield_av, na.rm=T)
res_itt_prod[6,1,h] <- sd(dta_trim$log_yield_av, na.rm=T)
res_itt_prod[5,2,h] <- summary(lm(as.formula(paste(paste("log_yield_av",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,1]
res_itt_prod[6,2,h] <- summary(lm(as.formula(paste(paste("log_yield_av",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,2]
res_itt_prod[5,3,h] <- ifelse(totrep >0, RI("log_yield_av",treatment , ctrls, dta_trim, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("log_yield_av",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,4])
res_itt_prod[5,4,h] <- nobs(lm(as.formula(paste(paste("log_yield_av",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))

### was yield better compared to normal year?
res_itt_prod[7,1,h] <- mean(dta_bal$yield_better, na.rm=T)
res_itt_prod[8,1,h] <- sd(dta_bal$yield_better, na.rm=T)
res_itt_prod[7,2,h] <- summary(lm(as.formula(paste(paste("yield_better",treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_prod[8,2,h] <- summary(lm(as.formula(paste(paste("yield_better",treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_prod[7,3,h] <- ifelse(totrep >0, RI("yield_better",treatment , ctrls, dta_trim, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("yield_better",treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4])
res_itt_prod[7,4,h] <- nobs(lm(as.formula(paste(paste("yield_better",treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

## labour

#dta_bal2 <- subset(dta_bal, tot_time_hh>0)
#dta_bal2$log_tot_time_hh <- log(dta_bal2$tot_time_hh)
#dta_trim <- trim("log_tot_time_hh", dta_bal2, .05)

#res_itt_prod[9,1,h] <- mean(dta_trim$log_tot_time_hh, na.rm=T)
#res_itt_prod[10,1,h] <- sd(dta_trim$log_tot_time_hh, na.rm=T)
#res_itt_prod[9,2,h] <- summary(lm(as.formula(paste(paste("log_tot_time_hh",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,1]
#res_itt_prod[10,2,h] <- summary(lm(as.formula(paste(paste("log_tot_time_hh",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,2]
#res_itt_prod[9,3,h] <- ifelse(totrep >0, RI("log_tot_time_hh",treatment , ctrls, dta_trim, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("log_tot_time_hh",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,4])
#res_itt_prod[9,4,h] <- nobs(lm(as.formula(paste(paste("log_tot_time_hh",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))

### labour productivity
#dta_bal2 <- subset(dta_bal, prod_tot>0)
#dta_bal2 <- subset(dta_bal2, tot_time_hh>0)
#dta_bal2$log_labour_prod <- log(dta_bal2$prod_tot/dta_bal2$tot_time_hh)
#dta_bal2$labour_prod <- dta_bal2$prod_tot/dta_bal2$tot_time_hh
#dta_trim <- trim("log_labour_prod", dta_bal2, .05)

#res_itt_prod[11,1,h] <- mean(dta_trim$log_labour_prod, na.rm=T)
#res_itt_prod[12,1,h] <- sd(dta_trim$log_labour_prod, na.rm=T)
#res_itt_prod[11,2,h] <- summary(lm(as.formula(paste(paste("log_labour_prod",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,1]
#res_itt_prod[12,2,h] <- summary(lm(as.formula(paste(paste("log_labour_prod",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,2]
#res_itt_prod[11,3,h] <- ifelse(totrep >0, RI("log_labour_prod",treatment , ctrls, dta_trim, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("tot_time_hh",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,4]) 
#res_itt_prod[11,4,h] <- nobs(lm(as.formula(paste(paste("log_labour_prod",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))

###index
dta_bal2 <- subset(dta_bal, area_tot >0 & prod_tot>0 & yield_av >0)
dta_bal2$log_prod_tot <- log(dta_bal2$prod_tot)
dta_bal2$log_area_tot <- log(dta_bal2$area_tot)
dta_bal2$log_yield_av <- log(dta_bal2$yield_av)
#dta_bal2$log_tot_time_hh <- log(dta_bal2$tot_time_hh)
#dta_bal2$log_tot_time_hh[is.infinite(dta_bal2$log_tot_time_hh)] <- NA 


dta_bal2 <- trim("log_yield_av", dta_bal2, .05)




dta_bal2$log_area_tot <- -dta_bal2$log_area_tot
#dta_bal2$log_tot_time_hh <- -dta_bal2$log_tot_time_hh

indexer <- FW_index(treatment, c("log_prod_tot", "log_area_tot", "yield_better"),ctrls,w_int2 = "weight", data =dta_bal2, nr_repl=totrep )
res_itt_prod[13,1,h] <-  mean(indexer[[3]]$index)
res_itt_prod[14,1,h] <-  sd(indexer[[3]]$index)
res_itt_prod[13,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_prod[14,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_prod[13,3,h] <-  indexer[[2]]
res_itt_prod[13,4,h] <-  nobs(indexer[[1]])

prod_plot[5,1,h] <- "production"
prod_plot[5,3:4,h] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index)
prod_plot[5,2,h] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index)

res_itt_prod[15,1:3 ,h] <- RI_FWER(c("log_prod_tot", "log_area_tot", "yield_better", "tot_time_hh"),indep = treatment, ctrls=ctrls ,dta =dta_bal2, p_vals = res_itt_prod[c(1,3,7,9),3,h], nr_repl = totrep, w_int="weight")


################################ welfare #############################

## better off than average
res_itt_wel[1,1,h] <- mean(dta_bal$better_av, na.rm=T)
res_itt_wel[2,1,h] <- sd(dta_bal$better_av, na.rm=T)
res_itt_wel[1,2,h]  <- summary(lm(as.formula(paste(paste("better_av",treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_wel[2,2,h]  <- summary(lm(as.formula(paste(paste("better_av",treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_wel[1,3,h]  <- ifelse(totrep >0, RI("better_av",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("better_av",treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4])
res_itt_wel[1,4,h]  <- nobs(lm(as.formula(paste(paste("better_av",treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

## better off than 6mo
res_itt_wel[3,1,h] <- mean(dta_bal$better_6m, na.rm=T)
res_itt_wel[4,1,h] <- sd(dta_bal$better_6m, na.rm=T)
res_itt_wel[3,2,h]  <- summary(lm(as.formula(paste(paste("better_6m",treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_wel[4,2,h]  <- summary(lm(as.formula(paste(paste("better_6m",treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_wel[3,3,h]  <- ifelse(totrep >0, RI("better_6m",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"),  summary(lm(as.formula(paste(paste("better_6m",treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4])
res_itt_wel[3,4,h]  <- nobs(lm(as.formula(paste(paste("better_6m",treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

#can eat preferred foods
res_itt_wel[5,1,h] <- mean(dta_bal$eatpref, na.rm=T)
res_itt_wel[6,1,h] <- sd(dta_bal$eatpref, na.rm=T)
res_itt_wel[5,2,h]  <-  summary(lm(as.formula(paste(paste("eatpref",treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_wel[6,2,h]  <-  summary(lm(as.formula(paste(paste("eatpref",treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_wel[5,3,h]  <- ifelse(totrep >0, RI("eatpref",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"),summary(lm(as.formula(paste(paste("eatpref",treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4])
res_itt_wel[5,4,h]  <-  nobs(lm(as.formula(paste(paste("eatpref",treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

#can eat enough foods
res_itt_wel[7,1,h] <- mean(dta_bal$eatenough, na.rm=T)
res_itt_wel[8,1,h] <- sd(dta_bal$eatenough, na.rm=T)
res_itt_wel[7,2,h]  <- summary(lm(as.formula(paste(paste("eatenough",treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_wel[8,2,h]  <- summary(lm(as.formula(paste(paste("eatenough",treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_wel[7,3,h]  <- ifelse(totrep >0, RI("eatenough",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("eatenough",treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4])
res_itt_wel[7,4,h]  <- nobs(lm(as.formula(paste(paste("eatenough",treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

#consumption - logged and trimmed
dta_bal2 <- subset(dta_bal, cons>0)
dta_bal2$log_cons <- log(dta_bal2$cons)
dta_trim <- trim("log_cons", dta_bal2, .05)

res_itt_wel[9,1,h] <- mean(dta_trim$log_cons, na.rm=T)
res_itt_wel[10,1,h] <- sd(dta_trim$log_cons, na.rm=T)
res_itt_wel[9,2,h]  <- summary(lm(as.formula(paste(paste("log_cons",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,1]
res_itt_wel[10,2,h]  <- summary(lm(as.formula(paste(paste("log_cons",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,2]
res_itt_wel[9,3,h]  <- ifelse(totrep >0, RI("log_cons",treatment , ctrls, dta_trim, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("log_cons",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,4])
res_itt_wel[9,4,h]  <- nobs(lm(as.formula(paste(paste("log_cons",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))



indexer <- FW_index(treatment, c("better_av", "better_6m", "eatpref","eatenough","log_cons"),ctrls,w_int2 = "weight", data =dta_trim, nr_repl=totrep )
res_itt_wel[11,1,h] <-  mean(indexer[[3]]$index)
res_itt_wel[12,1,h] <-  sd(indexer[[3]]$index)
res_itt_wel[11,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_wel[12,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_wel[11,3,h] <-  indexer[[2]]
res_itt_wel[11,4,h] <-  nobs(indexer[[1]])

prod_plot[6,1,h] <- "welfare"
prod_plot[6,3:4,h] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index)
prod_plot[6,2,h] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index)



res_itt_wel[13,1:3,h] <- RI_FWER(c("better_av", "better_6m", "eatpref","eatenough","log_cons"),indep = treatment, ctrls=ctrls ,dta =dta_trim, p_vals = res_itt_wel[c(1,3,5,7,9),3,h], nr_repl = totrep, w_int="weight")



}


save(res_itt_know, file = "res_itt_know.RData")
save(res_itt_pract, file = "res_itt_pract.RData")
save(res_itt_fert, file = "res_itt_fert.RData")
save(res_itt_seed, file = "res_itt_seed.RData")
save(res_itt_prod, file = "res_itt_prod.RData")
save(res_itt_wel, file = "res_itt_wel.RData")

print(res_itt_know)
print(res_itt_pract)
print(res_itt_fert)
print(res_itt_seed)
print(res_itt_prod)
print(res_itt_wel)

###plotting
plotter <- data.frame(prod_plot[,,1])
plotter$y <- as.numeric(as.character(plotter$y))
plotter$ylo <- as.numeric(as.character(plotter$ylo))
plotter$yhi <- as.numeric(as.character(plotter$yhi))
plotter$grp <- "T1: reducing info asymmetry"


plotter2 <- data.frame(prod_plot[,,5])
plotter2$y <- as.numeric(as.character(plotter2$y))
plotter2$ylo <- as.numeric(as.character(plotter2$ylo))
plotter2$yhi <- as.numeric(as.character(plotter2$yhi))
plotter2$grp <- "T2: HH cooperative approach"

plotter <- rbind(plotter,plotter2)

plotter$x <-  factor(plotter$x, levels=(c('welfare','production','seed','fertilizer','adoption','knowledge')))
png("/home/bjvca/data/projects/digital green/papers/DP_cooperation/results/ourcomes.png", units="px", height=3200, width= 6400, res=600)
credplot.gg(plotter,'SDs','',levels(plotter$x),.25)
dev.off()





