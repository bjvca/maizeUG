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

prod_plot <- array(NA, c(3,4,6))
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
dta$weight_av[dta$recipient=="male" & dta$messenger=="female"] <- 309/354
dta$weight_av[dta$recipient=="male" & dta$messenger=="couple"] <- 309/339
dta$weight_av[dta$recipient=="male" & dta$messenger=="male"] <- 309/347
dta$weight_av[dta$recipient=="female" & dta$messenger=="female"] <- 309/348
dta$weight_av[dta$recipient=="female" & dta$messenger=="couple"] <- 309/343
dta$weight_av[dta$recipient=="female" & dta$messenger=="male"] <- 309/347
dta$weight_av[dta$recipient=="couple" & dta$messenger=="female"] <- 309/319
dta$weight_av[dta$recipient=="couple" & dta$messenger=="couple"] <- 309/336
dta$weight_av[dta$recipient=="couple" & dta$messenger=="male"] <- 309/309

dta_copy <- dta


#set totrep to zero if you do not want simulation based inferecne
totrep <- 0
####

for (h in 1:6) {
if (h==1) {
############################################ H1: info asymmetry: rec=individual vs rec=couple #########################################################
dta <- dta_copy
dta$weight <- 1
dta$weight[dta$recipient == "female"] <-  1131/1144
dta$weight[dta$recipient == "male"] <- 1
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


dta_glob <- dta

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

man <- "mgt_man"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_mgt_man <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_mgt_man) <- c("hhid","time","mgt_man")
man <- "dectime_man"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_dectime_man <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_dectime_man) <- c("hhid","time","dectime_man")
man <- "decspace_man"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decspace_man <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decspace_man) <-  c("hhid","time","decspace_man")
man <- "decstriga_man"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decstriga_man <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decstriga_man) <-  c("hhid","time","decstriga_man")
man <- "decweed_man"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decweed_man <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decweed_man) <-  c("hhid","time","decweed_man")


all_ind <- merge(merge(merge(merge(dta_ind_mgt_woman,dta_ind_dectime_woman),dta_ind_decspace_woman),dta_ind_decstriga_woman), dta_ind_decweed_woman )
all_ind <- merge(merge(merge(merge(merge(all_ind,dta_ind_mgt_man),dta_ind_dectime_man),dta_ind_decspace_man),dta_ind_decstriga_man), dta_ind_decweed_man )
### arbitrary threshold allert!!! women managed plots are defined as plots where 3 or more out of 5 decision are made by the woman alone as reported by the woman
all_ind$women_decisions <- rowSums(all_ind[,3:7]) >= 1
#all_ind$women_decisions <- all_ind$mgt_woman 
all_ind$men_decisions <- rowSums(all_ind[,8:12]) >= 1
#all_ind$men_decisions <- all_ind$mgt_man 

################################# production ###########################
####### does the video increases production related outcomes?


### production
dec_vars <- paste(paste("prod",paste("_pl",1:5, sep=""), sep=""), "sp1",sep="_")
dta_ind_prod_sp1 <- reshape(cbind(dta_glob[c("hhid","person_interviewed", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_prod_sp1) <- c("hhid","person_interviewed","time","prod_sp1")
dec_vars <- paste(paste("prod",paste("_pl",1:5, sep=""), sep=""), "sp2",sep="_")
dta_ind_prod_sp2 <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_prod_sp2) <- c("hhid","time","prod_sp2")
prod_ind <- merge(dta_ind_prod_sp1,dta_ind_prod_sp2, by=c("hhid","time"))
prod_ind$prod <- ifelse(prod_ind$person_interviewed=="woman",prod_ind$prod_sp1,prod_ind$prod_sp2)

### area
dec_vars <- paste(paste("area",paste("_pl",1:5, sep=""), sep=""), "sp1",sep="_")
dta_ind_area_sp1 <- reshape(cbind(dta_glob[c("hhid","person_interviewed", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_area_sp1) <- c("hhid","person_interviewed","time","area_sp1")
dec_vars <- paste(paste("area",paste("_pl",1:5, sep=""), sep=""), "sp2",sep="_")
dta_ind_area_sp2 <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_area_sp2) <- c("hhid","time","area_sp2")
area_ind <- merge(dta_ind_area_sp1,dta_ind_area_sp2, by=c("hhid","time"))
area_ind$area <- ifelse(area_ind$person_interviewed=="woman",area_ind$area_sp1,area_ind$area_sp2)

### yield_better
dec_vars <- paste("yield_better_sp1",paste("_pl",1:5, sep=""), sep="")
dta_ind_yield_better_sp1 <- reshape(cbind(dta_glob[c("hhid","person_interviewed", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_yield_better_sp1) <- c("hhid","person_interviewed","time","yield_better_sp1")
dec_vars <- paste("yield_better_sp2",paste("_pl",1:5, sep=""), sep="")
dta_ind_yield_better_sp2 <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_yield_better_sp2) <- c("hhid","time","yield_better_sp2")
yield_better_ind <- merge(dta_ind_yield_better_sp1,dta_ind_yield_better_sp2, by=c("hhid","time"))
yield_better_ind$yield_better <- ifelse(yield_better_ind$person_interviewed=="woman",yield_better_ind$yield_better_sp1,yield_better_ind$yield_better_sp2)

prod_ind <- merge(all_ind,merge(prod_ind,merge(area_ind,yield_better_ind, by=c("hhid","time")), by=c("hhid","time")), by=c("hhid","time"))[c("hhid","time","women_decisions","men_decisions","prod","area","yield_better")]
names(prod_ind) <- c("hhid","time","women_decisions","men_decisions","prod_w","area_w","yield_better_w")
###now for men
### production
dec_vars <- paste(paste("prod",paste("_pl",1:5, sep=""), sep=""), "sp1",sep="_")
dta_ind_prod_sp1 <- reshape(cbind(dta_glob[c("hhid","person_interviewed", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_prod_sp1) <- c("hhid","person_interviewed","time","prod_sp1")
dec_vars <- paste(paste("prod",paste("_pl",1:5, sep=""), sep=""), "sp2",sep="_")
dta_ind_prod_sp2 <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_prod_sp2) <- c("hhid","time","prod_sp2")
prod_ind_m <- merge(dta_ind_prod_sp1,dta_ind_prod_sp2, by=c("hhid","time"))
prod_ind_m$prod <- ifelse(prod_ind_m$person_interviewed=="man",prod_ind_m$prod_sp1,prod_ind_m$prod_sp2)

### area
dec_vars <- paste(paste("area",paste("_pl",1:5, sep=""), sep=""), "sp1",sep="_")
dta_ind_area_sp1 <- reshape(cbind(dta_glob[c("hhid","person_interviewed", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_area_sp1) <- c("hhid","person_interviewed","time","area_sp1")
dec_vars <- paste(paste("area",paste("_pl",1:5, sep=""), sep=""), "sp2",sep="_")
dta_ind_area_sp2 <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_area_sp2) <- c("hhid","time","area_sp2")
area_ind <- merge(dta_ind_area_sp1,dta_ind_area_sp2, by=c("hhid","time"))
area_ind$area <- ifelse(area_ind$person_interviewed=="man",area_ind$area_sp1,area_ind$area_sp2)

### yield_better
dec_vars <- paste("yield_better_sp1",paste("_pl",1:5, sep=""), sep="")
dta_ind_yield_better_sp1 <- reshape(cbind(dta_glob[c("hhid","person_interviewed", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_yield_better_sp1) <- c("hhid","person_interviewed","time","yield_better_sp1")
dec_vars <- paste("yield_better_sp2",paste("_pl",1:5, sep=""), sep="")
dta_ind_yield_better_sp2 <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_yield_better_sp2) <- c("hhid","time","yield_better_sp2")
yield_better_ind <- merge(dta_ind_yield_better_sp1,dta_ind_yield_better_sp2, by=c("hhid","time"))
yield_better_ind$yield_better <- ifelse(yield_better_ind$person_interviewed=="man",yield_better_ind$yield_better_sp1,yield_better_ind$yield_better_sp2)

prod_ind <- merge(prod_ind,merge(prod_ind_m,merge(area_ind,yield_better_ind, by=c("hhid","time")), by=c("hhid","time")), by=c("hhid","time"))[c("hhid","time","women_decisions","men_decisions","prod_w","area_w","yield_better_w","prod","area","yield_better")]
names(prod_ind) <- c("hhid","time","women_decisions","men_decisions","prod_w","area_w","yield_better_w","prod_m","area_m","yield_better_m")

###########################################################################
prod_hh_w <- aggregate(prod_ind$women_decisions*prod_ind$prod_w, list(prod_ind$hhid), sum, na.rm=T)
names(prod_hh_w) <- c("hhid","prod_w")
area_hh_w <- aggregate(prod_ind$women_decisions*prod_ind$area_w, list(prod_ind$hhid), sum, na.rm=T)
names(area_hh_w) <- c("hhid","area_w")
yield_hh_w <- merge(prod_hh_w, area_hh_w)
names(yield_hh_w) <-  c("hhid","prod_w","area_w")

prod_hh_m <- aggregate(prod_ind$men_decisions*prod_ind$prod_m, list(prod_ind$hhid), sum, na.rm=T)
names(prod_hh_m) <- c("hhid","prod_m")
area_hh_m <- aggregate(prod_ind$men_decisions*prod_ind$area_m, list(prod_ind$hhid), sum, na.rm=T)
names(area_hh_m) <- c("hhid","area_m")
yield_hh_m <- merge(prod_hh_m, area_hh_m)
names(yield_hh_m) <-  c("hhid","prod_m","area_m")

dta <- merge(dta_bal,merge(yield_hh_m, yield_hh_w,by="hhid"),by="hhid")
dta$yield_m <- dta$prod_m/dta$area_m

dta$yield_w <- dta$prod_w/dta$area_w
dta[sapply(dta, is.infinite)] <- NA
dta <- subset(dta, yield_w>0 & yield_m >0)



#trimming is done on end result
dta_bal2 <- dta
dta_bal2$log_prod_tot <- dta_bal2$prod_m - dta_bal2$prod_w
dta_trim <- trim("log_prod_tot", dta_bal2, .05)

### production
res_itt_prod[1,1,h] <- mean(dta_trim$log_prod_tot, na.rm=T)
res_itt_prod[2,1,h] <- sd(dta_trim$log_prod_tot, na.rm=T)
res_itt_prod[1,2,h] <- summary(lm(as.formula(paste(paste("log_prod_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,1]
res_itt_prod[2,2,h] <- summary(lm(as.formula(paste(paste("log_prod_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,2]
res_itt_prod[1,3,h] <- ifelse(totrep >0, RI("log_prod_tot",treatment , ctrls, dta_trim, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("log_prod_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,4])
res_itt_prod[1,4,h] <- nobs(lm(as.formula(paste(paste("log_prod_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))

prod_plot[,,1]

prod_plot[1,1,h] <- "production"
prod_plot[1,3:4,h] <- confint(lm(as.formula(paste(paste("log_prod_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim), level=.9)[2,]/ res_itt_prod[2,2,h] 
prod_plot[1,2,h] <- res_itt_prod[1,2,h] /res_itt_prod[2,2,h] 

### area
dta_bal2 <- dta
dta_bal2$log_area_tot <- dta_bal2$area_m - dta_bal2$area_w
dta_trim <- trim("log_area_tot", dta_bal2, .05)

res_itt_prod[3,1,h] <- mean(dta_trim$log_area_tot, na.rm=T)
res_itt_prod[4,1,h] <- sd(dta_trim$log_area_tot, na.rm=T)
res_itt_prod[3,2,h] <- summary(lm(as.formula(paste(paste("log_area_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,1]
res_itt_prod[4,2,h] <- summary(lm(as.formula(paste(paste("log_area_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,2]
res_itt_prod[3,3,h] <- ifelse(totrep >0, RI("log_area_tot",treatment , ctrls, dta_trim, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("log_area_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,4])
res_itt_prod[3,4,h] <- nobs(lm(as.formula(paste(paste("log_area_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))

prod_plot[2,1,h] <- "area"
prod_plot[2,3:4,h] <- confint(lm(as.formula(paste(paste("log_area_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim), level=.9)[2,]/ res_itt_prod[4,2,h] 
prod_plot[2,2,h] <- res_itt_prod[3,2,h] /res_itt_prod[4,2,h] 

###yield gap:
dta_bal2 <- dta
dta_bal2$log_yield_av <- dta_bal2$yield_m - dta_bal2$yield_w
dta_trim <- trim("log_yield_av", dta_bal2, .05)


res_itt_prod[5,1,h] <- mean(dta_trim$log_yield_av, na.rm=T)
res_itt_prod[6,1,h] <- sd(dta_trim$log_yield_av, na.rm=T)
res_itt_prod[5,2,h] <- summary(lm(as.formula(paste(paste("log_yield_av",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,1]
res_itt_prod[6,2,h] <- summary(lm(as.formula(paste(paste("log_yield_av",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,2]
res_itt_prod[5,3,h] <- ifelse(totrep >0, RI("log_yield_av",treatment , ctrls, dta_trim, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("log_yield_av",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,4])
res_itt_prod[5,4,h] <- nobs(lm(as.formula(paste(paste("log_yield_av",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))

prod_plot[3,1,h] <- "yield"
prod_plot[3,3:4,h] <- confint(lm(as.formula(paste(paste("log_yield_av",treatment, sep="~"),ctrls,sep="+")), data=dta_trim), level=.9)[2,]/ res_itt_prod[6,2,h] 
prod_plot[3,2,h] <- res_itt_prod[5,2,h] /res_itt_prod[6,2,h] 

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
plotter$grp <- "reducing info asymmetry"


plotter2 <- data.frame(prod_plot[,,4])
plotter2$y <- as.numeric(as.character(plotter2$y))
plotter2$ylo <- as.numeric(as.character(plotter2$ylo))
plotter2$yhi <- as.numeric(as.character(plotter2$yhi))
plotter2$grp <- "HH cooperative approach"

plotter <- rbind(plotter,plotter2)

plotter$x <-  factor(plotter$x, levels=(c('production','area','yield')))
png("/home/bjvca/data/projects/digital green/papers/DP_cooperation/results/ourcomes_gaps.png", units="px", height=3200, width= 6400, res=600)
credplot.gg(plotter,'SDs','',levels(plotter$x),2.5)
dev.off()





