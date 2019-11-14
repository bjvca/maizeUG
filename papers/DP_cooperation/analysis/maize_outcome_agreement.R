rm(list=ls())
stopCluster(cl)
source("/home/bjvca/data/projects/digital green/papers/DP_cooperation/analysis/init_gender.R")
baseline <-  read.csv("/home/bjvca/data/projects/digital green/baseline/base_merge.cvs")

## uncomment lines below if run on AWS
#wget https://www.dropbox.com/s/z9p0qd4t29xiy14/AWS_coop.csv?dl=0
#wget https://www.dropbox.com/s/s4xne419eyhgsf0/base_coop.cvs?dl=0
#install.packages(c("ggplot2","doParallel","data.table","dplyr","Hmisc"))

rm(list=ls())
dta <- read.csv("AWS_coop.csv")
baseline <- read.csv("base_coop.csv")


library(ggplot2)
library(doParallel)
library(data.table)
library(dplyr)
library(Hmisc)

set.seed(07032018)

### indexing results arrays

### indexing results arrays
res_itt_pract <- array(NA, c(40,4,6))
rownames(res_itt_pract) <- c("first_day","","space","","striga","","weed","", "use_fert","","seed","","pract_index","","area","","prod","","labour_man","","labour_woman","","agree_sold","","sold_man","","sold_woman","","sold_both","","prod_index","","time_index","","sales_index","","pval_adopt","pvals_prod","pvals_time","pvals_sales")
disagree_plot <- array(NA, c(4,4,6))
colnames(disagree_plot) <-  c("x","y","ylo","yhi")

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
### baseline has 37 duplicates - remove them
baseline <- subset(baseline,!(hhid %in% baseline$hhid[duplicated(baseline$hhid)]))
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
totrep <- 10000
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


###agree_plant_share
res_itt_pract[1,1,h]  <- mean(dta_bal$disagree_plant_share, na.rm=T)
res_itt_pract[2,1,h]  <- sd(dta_bal$disagree_plant_share, na.rm=T)
res_itt_pract[1,2,h]  <- summary(lm(as.formula(paste(paste("disagree_plant_share", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_pract[2,2,h]  <- summary(lm(as.formula(paste(paste("disagree_plant_share", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_pract[1,3,h]  <- ifelse(totrep >0, RI("disagree_plant_share",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("disagree_plant_share", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4]) 
res_itt_pract[1,4,h]  <- nobs(lm(as.formula(paste(paste("disagree_plant_share", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

###disagree_spacing_share
res_itt_pract[3,1,h]  <- mean(dta_bal$disagree_spacing_share, na.rm=T)
res_itt_pract[4,1,h]  <- sd(dta_bal$disagree_spacing_share, na.rm=T)
res_itt_pract[3,2,h]  <- summary(lm(as.formula(paste(paste("disagree_spacing_share", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_pract[4,2,h]  <- summary(lm(as.formula(paste(paste("disagree_spacing_share", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_pract[3,3,h]  <- ifelse(totrep >0, RI("disagree_spacing_share",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"),  summary(lm(as.formula(paste(paste("disagree_spacing_share", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4])
res_itt_pract[3,4,h]  <- nobs(lm(as.formula(paste(paste("disagree_spacing_share", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

#disagree_striga_share
res_itt_pract[5,1,h]  <- mean(dta_bal$disagree_striga_share, na.rm=T)
res_itt_pract[6,1,h]  <- sd(dta_bal$disagree_striga_share, na.rm=T)
res_itt_pract[5,2,h]  <- summary(lm(as.formula(paste(paste("disagree_striga_share", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_pract[6,2,h]  <- summary(lm(as.formula(paste(paste("disagree_striga_share", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_pract[5,3,h]  <- ifelse(totrep >0, RI("disagree_striga_share",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("disagree_striga_share", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4])
res_itt_pract[5,4,h]  <- nobs(lm(as.formula(paste(paste("disagree_striga_share", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

# disagree_weeding_share 
res_itt_pract[7,1,h]  <- mean(dta_bal$disagree_weeding_share, na.rm=T)
res_itt_pract[8,1,h]  <- sd(dta_bal$disagree_weeding_share, na.rm=T)
res_itt_pract[7,2,h]  <- summary(lm(as.formula(paste(paste("disagree_weeding_share", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_pract[8,2,h]  <- summary(lm(as.formula(paste(paste("disagree_weeding_share", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_pract[7,3,h]  <- ifelse(totrep >0, RI("disagree_weeding_share",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"),summary(lm(as.formula(paste(paste("disagree_weeding_share", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4])
res_itt_pract[7,4,h]  <- nobs(lm(as.formula(paste(paste("disagree_weeding_share", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

# fertilizer use
res_itt_pract[9,1,h]  <-  mean(dta_bal$disagree_fert_share, na.rm=T)
res_itt_pract[10,1,h]  <-  sd(dta_bal$disagree_fert_share, na.rm=T)
res_itt_pract[9,2,h]  <- summary(lm(as.formula(paste(paste("disagree_fert_share", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_pract[10,2,h]  <- summary(lm(as.formula(paste(paste("disagree_fert_share", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_pract[9,3,h]  <- ifelse(totrep >0, RI("disagree_fert_share",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("disagree_fert_share", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4]) 
res_itt_pract[9,4,h]  <- nobs(lm(as.formula(paste(paste("disagree_fert_share", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))



###improved seed  
res_itt_pract[11,1,h]  <-  mean(dta_bal$disagree_seed_share, na.rm=T)
res_itt_pract[12,1,h]  <-  sd(dta_bal$disagree_seed_share, na.rm=T)
res_itt_pract[11,2,h]  <- summary(lm(as.formula(paste(paste("disagree_seed_share", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_pract[12,2,h]  <- summary(lm(as.formula(paste(paste("disagree_seed_share", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_pract[11,3,h]  <- ifelse(totrep >0, RI("disagree_seed_share",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"),  summary(lm(as.formula(paste(paste("disagree_seed_share", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4]) 
res_itt_pract[11,4,h]  <- nobs(lm(as.formula(paste(paste("disagree_seed_share", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))



indexer <-  FW_index(treatment,c("disagree_plant_share","disagree_spacing_share","disagree_striga_share","disagree_weeding_share","disagree_fert_share","disagree_seed_share"),ctrls,w_int2 = "weight", data =dta_bal, nr_repl=totrep )
res_itt_pract[13,1,h] <-  mean(indexer[[3]]$index)
res_itt_pract[14,1,h] <-  sd(indexer[[3]]$index)
res_itt_pract[13,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_pract[14,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_pract[13,3,h] <-  indexer[[2]]
res_itt_pract[14,4,h] <-  nobs(indexer[[1]])

disagree_plot[1,1,h] <- "disagreement on adoption"
disagree_plot[1,3:4,h] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index)
disagree_plot[1,2,h] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index)

##res_itt_pract <- Bcorr( c("day_one","space","striga","weed", "fert","impseed", "combiner","bought_seed","chem","labour"),dta_bal, res_itt_pract ,h)
##res_itt_fert <- Bcorr(c("fert_dap","fert_urea","fert_org"),dta_bal, res_itt_fert ,h)
##res_itt_seed <- Bcorr(c("hybrid","opv"),dta_bal, res_itt_seed ,h)

res_itt_pract[37,1:3 ,h] <- RI_FWER(c("disagree_plant_share","disagree_spacing_share","disagree_striga_share","disagree_weeding_share","disagree_fert_share","disagree_seed_share"),indep = treatment, ctrls=ctrls ,dta =dta_bal, p_vals = res_itt_pract[c(1,3,5,7,9,11),3,h], nr_repl = totrep, w_int="weight")

###area
dta_bal2 <- subset(dta_bal, area_tot>0)
dta_bal2$log_area_tot <- log(dta_bal2$area_tot)
dta_trim <- trim("log_area_tot", dta_bal2, .05)

res_itt_pract[15,1,h] <- mean(dta_trim$disagree_area, na.rm=T)
res_itt_pract[16,1,h] <- sd(dta_trim$disagree_area, na.rm=T)
res_itt_pract[15,2,h] <- summary(lm(as.formula(paste(paste("disagree_area",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,1]
res_itt_pract[16,2,h] <- summary(lm(as.formula(paste(paste("disagree_area",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,2]
res_itt_pract[15,3,h] <- ifelse(totrep >0, RI("disagree_area",treatment , ctrls, dta_trim, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("disagree_area",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,4])
res_itt_pract[15,4,h] <- nobs(lm(as.formula(paste(paste("disagree_area",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))


###production

dta_bal2 <- subset(dta_bal, prod_tot>0)
dta_bal2$log_prod_tot <- log(dta_bal2$prod_tot)
dta_trim <- trim("log_prod_tot", dta_bal2, .05)

res_itt_pract[17,1,h] <- mean(dta_trim$disagree_yield, na.rm=T)
res_itt_pract[18,1,h] <- sd(dta_trim$disagree_yield, na.rm=T)
res_itt_pract[17,2,h] <- summary(lm(as.formula(paste(paste("disagree_yield",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,1]
res_itt_pract[18,2,h] <- summary(lm(as.formula(paste(paste("disagree_yield",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,2]
res_itt_pract[17,3,h] <- ifelse(totrep >0, RI("disagree_yield",treatment , ctrls, dta_trim, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("disagree_yield",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,4])
res_itt_pract[18,4,h] <- nobs(lm(as.formula(paste(paste("disagree_yield",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))


indexer <-  FW_index(treatment,c("disagree_area","disagree_prod"),ctrls,w_int2 = "weight", data =dta_bal2, nr_repl=totrep )
res_itt_pract[31,1,h] <-  mean(indexer[[3]]$index)
res_itt_pract[32,1,h] <-  sd(indexer[[3]]$index)
res_itt_pract[31,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_pract[32,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_pract[31,3,h] <-  indexer[[2]]
res_itt_pract[32,4,h] <-  nobs(indexer[[1]])

disagree_plot[2,1,h] <- "disagreement on maize production"
disagree_plot[2,3:4,h] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index)
disagree_plot[2,2,h] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index)

res_itt_pract[38,1:3 ,h] <- RI_FWER(c("disagree_area","disagree_prod"),indep = treatment, ctrls=ctrls ,dta =dta_bal, p_vals = res_itt_pract[c(15,17),3,h], nr_repl = totrep, w_int="weight")


### man time
res_itt_pract[19,1,h]  <-  mean(dta_bal$disagree_man_time, na.rm=T)
res_itt_pract[20,1,h]  <-  sd(dta_bal$disagree_man_time, na.rm=T)
res_itt_pract[19,2,h]  <- summary(lm(as.formula(paste(paste("disagree_man_time", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_pract[20,2,h]  <- summary(lm(as.formula(paste(paste("disagree_man_time", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_pract[19,3,h]  <- ifelse(totrep >0, RI("disagree_man_time",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("disagree_man_time", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4]) 
res_itt_pract[19,4,h]  <- nobs(lm(as.formula(paste(paste("disagree_man_time", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))
### man woman
res_itt_pract[21,1,h]  <-  mean(dta_bal$disagree_woman_time, na.rm=T)
res_itt_pract[22,1,h]  <-  sd(dta_bal$disagree_woman_time, na.rm=T)
res_itt_pract[21,2,h]  <- summary(lm(as.formula(paste(paste("disagree_woman_time", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_pract[22,2,h]  <- summary(lm(as.formula(paste(paste("disagree_woman_time", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_pract[21,3,h]  <- ifelse(totrep >0, RI("disagree_woman_time",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("disagree_woman_time", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4]) 
res_itt_pract[21,4,h]  <- nobs(lm(as.formula(paste(paste("disagree_woman_time", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

indexer <-  FW_index(treatment,c("disagree_man_time","disagree_woman_time"),ctrls,w_int2 = "weight", data =dta_bal, nr_repl=totrep )
res_itt_pract[33,1,h] <-  mean(indexer[[3]]$index)
res_itt_pract[34,1,h] <-  sd(indexer[[3]]$index)
res_itt_pract[33,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_pract[34,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_pract[33,3,h] <-  indexer[[2]]
res_itt_pract[34,4,h] <-  nobs(indexer[[1]])

disagree_plot[3,1,h] <- "disagreement on labor time"
disagree_plot[3,3:4,h] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index)
disagree_plot[3,2,h] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index)

res_itt_pract[39,1:3 ,h] <- RI_FWER(c("disagree_man_time","disagree_woman_time"),indep = treatment, ctrls=ctrls ,dta =dta_bal, p_vals = res_itt_pract[c(19,21),3,h], nr_repl = totrep, w_int="weight")



##disagree sold

res_itt_pract[23,1,h]  <-  mean(dta_bal$disagree_sold, na.rm=T)
res_itt_pract[24,1,h]  <-  sd(dta_bal$disagree_sold, na.rm=T)
res_itt_pract[23,2,h]  <- summary(lm(as.formula(paste(paste("disagree_sold", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_pract[24,2,h]  <- summary(lm(as.formula(paste(paste("disagree_sold", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_pract[23,3,h]  <- ifelse(totrep >0, RI("disagree_sold",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("disagree_sold", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4]) 
res_itt_pract[23,4,h]  <- nobs(lm(as.formula(paste(paste("disagree_sold", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))
### sold_man
res_itt_pract[25,1,h]  <-  mean(dta_bal$disagree_sold_man, na.rm=T)
res_itt_pract[26,1,h]  <-  sd(dta_bal$disagree_sold_man, na.rm=T)
res_itt_pract[25,2,h]  <- summary(lm(as.formula(paste(paste("disagree_sold_man", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_pract[26,2,h]  <- summary(lm(as.formula(paste(paste("disagree_sold_man", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_pract[25,3,h]  <- ifelse(totrep >0, RI("disagree_sold_man",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("disagree_sold_man", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4]) 
res_itt_pract[25,4,h]  <- nobs(lm(as.formula(paste(paste("disagree_sold_man", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

### sold_woman
res_itt_pract[27,1,h]  <-  mean(dta_bal$disagree_sold_woman, na.rm=T)
res_itt_pract[28,1,h]  <-  sd(dta_bal$disagree_sold_woman, na.rm=T)
res_itt_pract[27,2,h]  <- summary(lm(as.formula(paste(paste("disagree_sold_woman", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_pract[28,2,h]  <- summary(lm(as.formula(paste(paste("disagree_sold_woman", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_pract[27,3,h]  <- ifelse(totrep >0, RI("disagree_sold_woman",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("disagree_sold_woman", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4]) 
res_itt_pract[27,4,h]  <- nobs(lm(as.formula(paste(paste("disagree_sold_woman", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

### sold_both
res_itt_pract[29,1,h]  <-  mean(dta_bal$disagree_sold_both, na.rm=T)
res_itt_pract[30,1,h]  <-  sd(dta_bal$disagree_sold_both, na.rm=T)
res_itt_pract[29,2,h]  <- summary(lm(as.formula(paste(paste("disagree_sold_both", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_pract[30,2,h]  <- summary(lm(as.formula(paste(paste("disagree_sold_both", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_pract[29,3,h]  <- ifelse(totrep >0, RI("disagree_sold_both",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("disagree_sold_both", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4]) 
res_itt_pract[29,4,h]  <- nobs(lm(as.formula(paste(paste("disagree_sold_both", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

indexer <-  FW_index(treatment,c("disagree_sold","disagree_sold_man","disagree_sold_woman","disagree_sold_both"),ctrls,w_int2 = "weight", data =dta_bal, nr_repl=totrep )
res_itt_pract[35,1,h] <-  mean(indexer[[3]]$index)
res_itt_pract[36,1,h] <-  sd(indexer[[3]]$index)
res_itt_pract[35,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_pract[36,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_pract[35,3,h] <-  indexer[[2]]
res_itt_pract[36,4,h] <-  nobs(indexer[[1]])

disagree_plot[4,1,h] <- "disagreement on maize sales"
disagree_plot[4,3:4,h] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index)
disagree_plot[4,2,h] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index)

res_itt_pract[40,1:3 ,h] <- RI_FWER(c("disagree_sold","disagree_sold_man","disagree_sold_woman","disagree_sold_both"),indep = treatment, ctrls=ctrls ,dta =dta_bal, p_vals = res_itt_pract[c(23,25,27,29),3,h], nr_repl = totrep, w_int="weight")

}


save(res_itt_pract, file = "res_agree.RData")


print(res_itt_pract)


###plotting
plotter <- data.frame(disagree_plot[,,1])
plotter$y <- as.numeric(as.character(plotter$y))
plotter$ylo <- as.numeric(as.character(plotter$ylo))
plotter$yhi <- as.numeric(as.character(plotter$yhi))
plotter$grp <- "T1: reducing info asymmetry"


plotter2 <- data.frame(disagree_plot[,,5])
plotter2$y <- as.numeric(as.character(plotter2$y))
plotter2$ylo <- as.numeric(as.character(plotter2$ylo))
plotter2$yhi <- as.numeric(as.character(plotter2$yhi))
plotter2$grp <- "T2: HH cooperative approach"

plotter <- rbind(plotter,plotter2)

plotter$x <-  factor(plotter$x, levels=(rev(c("disagreement on adoption","disagreement on maize production","disagreement on labor time","disagreement on maize sales"))))
png("/home/bjvca/data/projects/digital green/papers/DP_cooperation/results/disagreement.png", units="px", height=3200, width= 6400, res=600)
credplot.gg(plotter,'SDs','',levels(plotter$x),0.3)
dev.off()





