rm(list=ls())
source("/home/bjvca/data/projects/digital green/endline/data/init_gender.R")
#source("functions.R")
#dta <- read.csv("AWS.csv")
#set totrep to zero if you do not want simulation based inferecne
totrep <- 0
library(ggplot2)
library(doParallel)
library(data.table)
library(dplyr)
library(Hmisc)

set.seed(07032018)

### indexing results arrays
res_time <- array(NA, c(12,4,7)) 
rownames(res_time) <- c("preparing","","plantig","","weeding","", "spraying","","harvest","","total","")
res_time_man <- array(NA, c(12,4,7)) 
rownames(res_time_man)  <- c("preparing","","plantig","","weeding","", "spraying","","harvest","","total","")
res_time_woman <- array(NA, c(12,4,7)) 
rownames(res_time_woman)  <- c("preparing","","plantig","","weeding","", "spraying","","harvest","","total","")

res_itt_pract <- array(NA, c(22,4,7))
rownames(res_itt_pract) <- c("first_day","","space","","striga","","weed","", "use_fert","","seed","","combiner","","bought_seed","","chem","","labour","","pract_index","")
res_itt_fert <- array(NA, c(6,4,7))
rownames(res_itt_fert) <- c("use_DAP","","use_urea","","use_organic","")
res_itt_seed <- array(NA, c(4,4,7))
rownames(res_itt_seed) <- c("hybrid","","opv","")
res_itt_prod <- array(NA, c(10,4,3))
rownames(res_itt_prod) <- c("prod","","area","","yield","","yield_better","","prod_index","")
prod_plot <- data.frame(matrix(NA, 4,4))
names(prod_plot) <- c("x","y","ylo","yhi")
fert_plot <- data.frame(matrix(NA, 4,4))
names(fert_plot) <- c("x","y","ylo","yhi")
seed_plot <- data.frame(matrix(NA, 4,4))
names(seed_plot) <- c("x","y","ylo","yhi")

res_itt_wel <-  array(NA, c(12,4,3))
rownames(res_itt_wel) <- c("better_av","","better_6m","","eatpref","","eatenough","","log_cons","","welfare_index","")
res_itt_disp <-  array(NA, c(8,4,3))
rownames(res_itt_disp) <- c("cons_maize","","sold_maize","","saved_seed","","disp_index","")

cl <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
registerDoParallel(cl)
plot_res <- array(NA, c(12,5,7))
colnames(plot_res) <-  c("x","y","ylo","yhi","grp")
###@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### For the comparison between different ways of delivering information to a household, we only keep households where the video was also shown to the man (as generally he will be the main decision maker)
###dta <- subset(dta, (recipient !="female" | messenger == "ctrl"))
### and so delivery mode needs its own RI function


RI <- function(dep, indep, ctrls = NULL,  dta , nr_repl = 1000, h_int=h, w_int = NULL) {
#indep <-  "(recipient == 'couple') +ivr+sms+as.factor(messenger)" 
#ctrls <- NULL
#h_int <- 1
#dep <- "know_space"
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
if ((h_int==1) | (h_int==2)  | (h_int==5) | (h_int==6)) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","female"))
} else if (h_int==3) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female")
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["female",]>0]),"female","couple"))
} else if (h_int==4) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", "male")
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["female",]>0]),"female","couple"))

} else if (h_int==7) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]), "male", "female")
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", "female")

} 
		dta_sim$ivr <- dta_sim$perm_ivr
		dta_sim$sms <- dta_sim$perm_sms
		return(abs(ifelse(is.null(ctrls),summary(lm(as.formula(paste(dep,indep,sep="~")), weights=weights, data=data.frame(dta_sim)))$coefficients[2,1],summary(lm(as.formula(paste(paste(dep,indep,sep="~"), ctrls,sep="+")), weights=weights, data=dta_sim))$coefficients[2,1])) > abs(crit) )
	}
	return(sum(oper)/nr_repl)
}


RI_FWER <- function(deps, indep,ctrls = NULL, dta ,p_vals , nr_repl = 1000, h_int=h) {
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
dta <-  data.table(cbind(dta[deps],dta[c("messenger","recipient","treat","uniqID","hhid","ivr","sms","called","totsms")]))

} else {
	dta <-  data.table(cbind(dta[deps],dta[c("messenger","recipient","treat","uniqID","hhid","ivr","sms","called","totsms")],cbind(dta[unlist(strsplit(ctrls,"[+]"))])))
}
	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
		dta_sim <- data.table(dta)
 		setDT(dta_sim)[,perm:=sample(treat),by = (uniqID)]
if ((h_int==1) | (h_int==2)  | (h_int==5) | (h_int==6)) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","female"))
} else if (h_int==3) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female")
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["female",]>0]),"female","couple"))
} else if (h_int==4) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", "male")
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["female",]>0]),"female","couple"))

} else if (h_int==7) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]), "male", "female")
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", "female")

} 
if (is.null(ctrls)) {
return(unlist(lapply(deps, function(dvar)  summary(lm(as.formula(paste(dvar,indep,sep="~")), data=dta_sim))$coefficients[2,4])))
} else {
return(unlist(lapply(deps, function(dvar)  summary(lm(as.formula(paste(paste(dvar,indep,sep="~"),ctrls, sep="+")), data=dta_sim))$coefficients[2,4])))
}		

		}

thresholds <- seq(0, 0.1, length.out = 10000)
type_I_rate <- sapply(thresholds, threshold_finder)
return( list=c(thresholds[max(which(type_I_rate <= 0.1))],thresholds[max(which(type_I_rate <= 0.05))], thresholds[max(which(type_I_rate <= 0.01))]))
}

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


### wrapper function to make a graph to be used for presentations
credplot.gg <- function(d,units){
 # d is a data frame with 4 columns
 # d$x gives variable names
 # d$y gives center point
 # d$ylo gives lower limits
 # d$yhi gives upper limits
 require(ggplot2)
 p <- ggplot(d, aes(x=x, y=y, ymin=ylo, ymax=yhi, colour=as.factor(grp)))+
 geom_pointrange(position=position_dodge(-.4))+ 
geom_text(aes(label=format(round(y, 2), nsmall = 2)), nudge_x = .1, hjust=-0.1)+
 geom_hline(yintercept = 0, linetype=2)+
 coord_flip()+
 xlab('') + ylab(units)+ theme(axis.text=element_text(size=18),
        axis.title=element_text(size=14,face="bold"),legend.text=element_text(size=18), legend.title=element_blank())+
    geom_errorbar(aes(ymin=ylo, ymax=yhi),position=position_dodge(-.4),width=0,cex=1.5) 
 return(p)
}

###@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

## alternative way to correct for multiple hypotheses, inspried on: http://blogs.worldbank.org/impactevaluations/tools-of-the-trade-a-quick-adjustment-for-multiple-hypothesis-testing


Bcorr <- function(vars, data, res_array, h=h) {
i <- 1
#loop over vars
for (var in vars) {
 test <- cor(data[setdiff(vars,var)], use="pairwise.complete.obs")
diag(test) <- NA
res_array[i,4,h] <- 1-(1 - res_array[i,3,h])^length(vars)^(1-mean(test, na.rm=T))
i <- i+2
}
return(res_array)
}

trim <- function(var, dataset, trim_perc=.1) {
### function for triming a dataset *dataset* on a variable *var*
return( subset(dataset,dataset[var] > quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)), na.rm=T)[1] & dataset[var] < quantile(dataset[var], c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[2]) )
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

baseline <-  read.csv("/home/bjvca/data/projects/digital green/baseline/base_merge.cvs")
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
ctrls <- NULL

##############################################   here the analysis loop starts ###########################################################

for (h in 1:7) {
if (h==1) {
############################################ H1: info asymmetry: rec=individual vs rec=couple #########################################################
dta <- dta_copy
treatment <- "(recipient == 'couple') +ivr+sms+as.factor(messenger)" 
##uncomment to include controls for imbalance - use no spaces in ctrls
dta <- merge(dta_copy,baseline, by="hhid")
dta$weights <- 1
dta$weights[dta$recipient == "male"] <- 1038/1040
ctrls <- "yield+maizeage+maizeeduc+maizehh_no+maizeprinfo_receiv_spouse" 

} else if (h==2) {
############################################ H2: empower: rec=male vs rec=couple or woman #########################################################
dta <- dta_copy
treatment <- "(recipient != 'male') +ivr+sms+ as.factor(messenger)" 
dta <- merge(dta_copy,baseline, by="hhid")
dta$weights <- 1
dta$weights[dta$recipient == "female"] <-  964/1038
ctrls <- "yield+maizeage+maizeeduc+maizeprinput_use" 
} else if (h==3) {
############################################ H2a: empower : rec=male vs rec=female ###################################################
dta <- subset(dta_copy, recipient == "male" | recipient == "female")
dta$weights <- 1
ctrls<- NULL
treatment <- "(recipient == 'female') +ivr+sms+as.factor(messenger)" 
} else if (h==4) {
############################################ H2b: empower a2: rec=male vs rec=couple ###################################################
dta <- subset(dta_copy, recipient == "male" | recipient == "couple")
dta$weights <- 1
ctrls<- NULL
treatment <- "(recipient == 'couple') +ivr+sms+ as.factor(messenger)"
##is this equal to just comparing female to couple???

} else if (h==5) {
############################################ H3: promote collective approach ###################################################
dta <- dta_copy
dta$weights <- 1
dta$weights[dta$messenger == "female"] <-  1003/1021
ctrls<- NULL
treatment <- "(messenger == 'couple') +ivr+sms+as.factor(recipient)"
} else if (h==6) {
############################################ H4: challenge gender stereotype ###################################################
dta <- dta_copy
dta$weights <- 1
dta$weights[dta$messenger == "female"] <-  1018/1021
ctrls<- NULL
treatment <- "(messenger != 'male') +ivr+sms+as.factor(recipient)"
} else if (h==7) {
############################################ H5: homophily ###################################################
dta <- subset(dta_copy, recipient != "couple" & messenger != "couple")
dta <- merge(dta,baseline, by="hhid")
dta$recipient <- factor(dta$recipient)
dta$messenger <- factor(dta$messenger)
dta$weights <- 1
dta$weights[dta$messenger == "female" & dta$recipient == "female"] <-  348/354
dta$weights[dta$messenger == "male" & dta$recipient == "female"] <-  347/354
dta$weights[dta$messenger == "male" & dta$recipient == "male"] <-  347/354
ctrls<- "maizehh_no"
treatment <- "(messenger == recipient) +ivr+sms"
}
print(h)
################################################## labour time  #####################################################
########################*************************** HH level *****************************############################

#preparing
res_time[1,1,h] <- ifelse(h ==1, wtd.mean(dta$time_prep_hh_mean[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.mean(dta$time_prep_hh_mean[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$time_prep_hh_mean[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.mean(dta$time_prep_hh_mean[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.mean(dta$time_prep_hh_mean[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time[2,1,h] <- ifelse(h ==1, wtd.sd(dta$time_prep_hh_mean[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.sd(dta$time_prep_hh_mean[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$time_prep_hh_mean[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.sd(dta$time_prep_hh_mean[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.sd(dta$time_prep_hh_mean[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time[1,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_prep_hh_mean",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("time_prep_hh_mean",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_time[2,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_prep_hh_mean",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("time_prep_hh_mean",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_time[1,3,h] <- ifelse(totrep >0, RI("time_prep_hh_mean",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_prep_hh_mean",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("time_prep_hh_mean",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_time[2,3,h] <- nobs(lm(as.formula(paste("time_prep_hh_mean",treatment, sep="~")) ,data=dta))
#planting
res_time[3,1,h] <- ifelse(h ==1, wtd.mean(dta$time_plant_hh_mean[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.mean(dta$time_plant_hh_mean[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$time_plant_hh_mean[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.mean(dta$time_plant_hh_mean[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.mean(dta$time_plant_hh_mean[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time[4,1,h] <- ifelse(h ==1, wtd.sd(dta$time_plant_hh_mean[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.sd(dta$time_plant_hh_mean[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$time_plant_hh_mean[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.sd(dta$time_plant_hh_mean[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.sd(dta$time_plant_hh_mean[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time[3,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_plant_hh_mean",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("time_plant_hh_mean",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_time[4,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_plant_hh_mean",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("time_plant_hh_mean",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_time[3,3,h] <- ifelse(totrep >0, RI("time_plant_hh_mean",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_plant_hh_mean",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("time_plant_hh_mean",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_time[4,3,h] <- nobs(lm(as.formula(paste("time_plant_hh_mean",treatment, sep="~")) ,data=dta))
#weeding
res_time[5,1,h] <- ifelse(h ==1, wtd.mean(dta$time_weed_hh_mean[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.mean(dta$time_weed_hh_mean[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$time_weed_hh_mean[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.mean(dta$time_weed_hh_mean[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.mean(dta$time_weed_hh_mean[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time[6,1,h] <- ifelse(h ==1, wtd.sd(dta$time_weed_hh_mean[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.sd(dta$time_weed_hh_mean[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$time_weed_hh_mean[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.sd(dta$time_weed_hh_mean[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.sd(dta$time_weed_hh_mean[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time[5,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_weed_hh_mean",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("time_weed_hh_mean",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_time[6,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_weed_hh_mean",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("time_weed_hh_mean",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_time[5,3,h] <- ifelse(totrep >0, RI("time_weed_hh_mean",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_weed_hh_mean",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("time_weed_hh_mean",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_time[6,3,h] <- nobs(lm(as.formula(paste("time_weed_hh_mean",treatment, sep="~")) ,data=dta))
##spraying
res_time[7,1,h] <- ifelse(h ==1, wtd.mean(dta$time_spray_hh_mean[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.mean(dta$time_spray_hh_mean[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$time_spray_hh_mean[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.mean(dta$time_spray_hh_mean[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.mean(dta$time_spray_hh_mean[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time[8,1,h] <- ifelse(h ==1, wtd.sd(dta$time_spray_hh_mean[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.sd(dta$time_spray_hh_mean[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$time_spray_hh_mean[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.sd(dta$time_spray_hh_mean[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.sd(dta$time_spray_hh_mean[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time[7,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_spray_hh_mean",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("time_spray_hh_mean",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_time[8,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_spray_hh_mean",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("time_spray_hh_mean",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_time[7,3,h] <- ifelse(totrep >0, RI("time_spray_hh_mean",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_spray_hh_mean",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("time_spray_hh_mean",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_time[8,3,h] <- nobs(lm(as.formula(paste("time_spray_hh_mean",treatment, sep="~")) ,data=dta))

##harv
res_time[9,1,h] <- ifelse(h ==1, wtd.mean(dta$time_harv_hh_mean[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.mean(dta$time_harv_hh_mean[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$time_harv_hh_mean[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.mean(dta$time_harv_hh_mean[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.mean(dta$time_harv_hh_mean[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time[10,1,h] <- ifelse(h ==1, wtd.sd(dta$time_harv_hh_mean[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.sd(dta$time_harv_hh_mean[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$time_harv_hh_mean[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.sd(dta$time_harv_hh_mean[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.sd(dta$time_harv_hh_mean[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time[9,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_harv_hh_mean",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("time_harv_hh_mean",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_time[10,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_harv_hh_mean",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("time_harv_hh_mean",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_time[9,3,h] <- ifelse(totrep >0, RI("time_harv_hh_mean",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_harv_hh_mean",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("time_harv_hh_mean",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_time[10,3,h] <- nobs(lm(as.formula(paste("time_harv_hh_mean",treatment, sep="~")) ,data=dta))

##total
res_time[11,1,h] <- ifelse(h ==1, wtd.mean(dta$tot_time_hh_mean[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.mean(dta$tot_time_hh_mean[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$tot_time_hh_mean[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.mean(dta$tot_time_hh_mean[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.mean(dta$tot_time_hh_mean[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time[12,1,h] <- ifelse(h ==1, wtd.sd(dta$tot_time_hh_mean[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.sd(dta$tot_time_hh_mean[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$tot_time_hh_mean[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.sd(dta$tot_time_hh_mean[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.sd(dta$tot_time_hh_mean[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time[11,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("tot_time_hh_mean",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("tot_time_hh_mean",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_time[12,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("tot_time_hh_mean",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("tot_time_hh_mean",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_time[11,3,h] <- ifelse(totrep >0, RI("tot_time_hh_mean",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("tot_time_hh_mean",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("tot_time_hh_mean",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_time[12,3,h] <- nobs(lm(as.formula(paste("tot_time_hh_mean",treatment, sep="~")) ,data=dta))


#res_time <- Bcorr(c("time_prep_hh_mean","time_plant_hh_mean","time_weed_hh_mean"),dta, res_time ,h)

########################*************************** man *****************************############################
#preparing
res_time_man[1,1,h] <- ifelse(h ==1, wtd.mean(dta$time_prep_man[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.mean(dta$time_prep_man[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$time_prep_man[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.mean(dta$time_prep_man[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.mean(dta$time_prep_man[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time_man[2,1,h] <- ifelse(h ==1, wtd.sd(dta$time_prep_man[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.sd(dta$time_prep_man[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$time_prep_man[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.sd(dta$time_prep_man[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.sd(dta$time_prep_man[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time_man[1,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_prep_man",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("time_prep_man",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_time_man[2,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_prep_man",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("time_prep_man",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_time_man[1,3,h] <- ifelse(totrep >0, RI("time_prep_man",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_prep_man",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("time_prep_man",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_time_man[2,3,h] <- nobs(lm(as.formula(paste("time_prep_man",treatment, sep="~")) ,data=dta))
#planting
res_time_man[3,1,h] <- ifelse(h ==1, wtd.mean(dta$time_plant_man[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.mean(dta$time_plant_man[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$time_plant_man[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.mean(dta$time_plant_man[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.mean(dta$time_plant_man[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time_man[4,1,h] <- ifelse(h ==1, wtd.sd(dta$time_plant_man[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.sd(dta$time_plant_man[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$time_plant_man[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.sd(dta$time_plant_man[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.sd(dta$time_plant_man[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time_man[3,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_plant_man",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("time_plant_man",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_time_man[4,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_plant_man",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("time_plant_man",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_time_man[3,3,h] <- ifelse(totrep >0, RI("time_plant_man",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_plant_man",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("time_plant_man",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_time_man[4,3,h] <- nobs(lm(as.formula(paste("time_plant_man",treatment, sep="~")) ,data=dta))
#weeding
res_time_man[5,1,h] <- ifelse(h ==1, wtd.mean(dta$time_weed_man[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.mean(dta$time_weed_man[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$time_weed_man[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.mean(dta$time_weed_man[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.mean(dta$time_weed_man[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time_man[6,1,h] <- ifelse(h ==1, wtd.sd(dta$time_weed_man[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.sd(dta$time_weed_man[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$time_weed_man[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.sd(dta$time_weed_man[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.sd(dta$time_weed_man[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time_man[5,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_weed_man",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("time_weed_man",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_time_man[6,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_weed_man",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("time_weed_man",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_time_man[5,3,h] <- ifelse(totrep >0, RI("time_weed_man",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_weed_man",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("time_weed_man",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_time_man[6,3,h] <- nobs(lm(as.formula(paste("time_weed_man",treatment, sep="~")) ,data=dta))
##spraying
res_time_man[7,1,h] <- ifelse(h ==1, wtd.mean(dta$time_spray_man[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.mean(dta$time_spray_man[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$time_spray_man[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.mean(dta$time_spray_man[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.mean(dta$time_spray_man[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time_man[8,1,h] <- ifelse(h ==1, wtd.sd(dta$time_spray_man[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.sd(dta$time_spray_man[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$time_spray_man[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.sd(dta$time_spray_man[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.sd(dta$time_spray_man[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time_man[7,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_spray_man",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("time_spray_man",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_time_man[8,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_spray_man",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("time_spray_man",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_time_man[7,3,h] <- ifelse(totrep >0, RI("time_spray_man",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_spray_man",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("time_spray_man",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_time_man[8,3,h] <- nobs(lm(as.formula(paste("time_spray_man",treatment, sep="~")) ,data=dta))

##harv
res_time_man[9,1,h] <- ifelse(h ==1, wtd.mean(dta$time_harv_man[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.mean(dta$time_harv_man[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$time_harv_man[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.mean(dta$time_harv_man[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.mean(dta$time_harv_man[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time_man[10,1,h] <- ifelse(h ==1, wtd.sd(dta$time_harv_man[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.sd(dta$time_harv_man[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$time_harv_man[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.sd(dta$time_harv_man[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.sd(dta$time_harv_man[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time_man[9,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_harv_man",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("time_harv_man",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_time_man[10,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_harv_man",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("time_harv_man",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_time_man[9,3,h] <- ifelse(totrep >0, RI("time_harv_man",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_harv_man",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("time_harv_man",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_time_man[10,3,h] <- nobs(lm(as.formula(paste("time_harv_man",treatment, sep="~")) ,data=dta))

##total
res_time_man[11,1,h] <- ifelse(h ==1, wtd.mean(dta$tot_time_man[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.mean(dta$tot_time_man[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$tot_time_man[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.mean(dta$tot_time_man[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.mean(dta$tot_time_man[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time_man[12,1,h] <- ifelse(h ==1, wtd.sd(dta$tot_time_man[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.sd(dta$tot_time_man[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$tot_time_man[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.sd(dta$tot_time_man[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.sd(dta$tot_time_man[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time_man[11,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("tot_time_man",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("tot_time_man",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_time_man[12,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("tot_time_man",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("tot_time_man",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_time_man[11,3,h] <- ifelse(totrep >0, RI("tot_time_man",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("tot_time_man",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("tot_time_man",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_time_man[12,3,h] <- nobs(lm(as.formula(paste("tot_time_man",treatment, sep="~")) ,data=dta))

########################*************************** woman *****************************############################

#preparing
res_time_woman[1,1,h] <- ifelse(h ==1, wtd.mean(dta$time_prep_woman[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.mean(dta$time_prep_woman[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$time_prep_woman[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.mean(dta$time_prep_woman[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.mean(dta$time_prep_woman[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time_woman[2,1,h] <- ifelse(h ==1, wtd.sd(dta$time_prep_woman[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.sd(dta$time_prep_woman[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$time_prep_woman[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.sd(dta$time_prep_woman[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.sd(dta$time_prep_woman[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time_woman[1,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_prep_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("time_prep_woman",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_time_woman[2,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_prep_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("time_prep_woman",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_time_woman[1,3,h] <- ifelse(totrep >0, RI("time_prep_woman",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_prep_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("time_prep_woman",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_time_woman[2,3,h] <- nobs(lm(as.formula(paste("time_prep_woman",treatment, sep="~")) ,data=dta))
#planting
res_time_woman[3,1,h] <- ifelse(h ==1, wtd.mean(dta$time_plant_woman[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.mean(dta$time_plant_woman[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$time_plant_woman[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.mean(dta$time_plant_woman[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.mean(dta$time_plant_woman[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time_woman[4,1,h] <- ifelse(h ==1, wtd.sd(dta$time_plant_woman[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.sd(dta$time_plant_woman[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$time_plant_woman[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.sd(dta$time_plant_woman[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.sd(dta$time_plant_woman[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time_woman[3,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_plant_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("time_plant_woman",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_time_woman[4,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_plant_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("time_plant_woman",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_time_woman[3,3,h] <- ifelse(totrep >0, RI("time_plant_woman",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_plant_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("time_plant_woman",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_time_woman[4,3,h] <- nobs(lm(as.formula(paste("time_plant_woman",treatment, sep="~")) ,data=dta))
#weeding
res_time_woman[5,1,h] <- ifelse(h ==1, wtd.mean(dta$time_weed_woman[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.mean(dta$time_weed_woman[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$time_weed_woman[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.mean(dta$time_weed_woman[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.mean(dta$time_weed_woman[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time_woman[6,1,h] <- ifelse(h ==1, wtd.sd(dta$time_weed_woman[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.sd(dta$time_weed_woman[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$time_weed_woman[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.sd(dta$time_weed_woman[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.sd(dta$time_weed_woman[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time_woman[5,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_weed_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("time_weed_woman",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_time_woman[6,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_weed_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("time_weed_woman",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_time_woman[5,3,h] <- ifelse(totrep >0, RI("time_weed_woman",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_weed_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("time_weed_woman",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_time_woman[6,3,h] <- nobs(lm(as.formula(paste("time_weed_woman",treatment, sep="~")) ,data=dta))
##spraying
res_time_woman[7,1,h] <- ifelse(h ==1, wtd.mean(dta$time_spray_woman[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.mean(dta$time_spray_woman[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$time_spray_woman[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.mean(dta$time_spray_woman[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.mean(dta$time_spray_woman[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time_woman[8,1,h] <- ifelse(h ==1, wtd.sd(dta$time_spray_woman[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.sd(dta$time_spray_woman[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$time_spray_woman[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.sd(dta$time_spray_woman[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.sd(dta$time_spray_woman[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time_woman[7,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_spray_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("time_spray_woman",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_time_woman[8,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_spray_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("time_spray_woman",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_time_woman[7,3,h] <- ifelse(totrep >0, RI("time_spray_woman",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_spray_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("time_spray_woman",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_time_woman[8,3,h] <- nobs(lm(as.formula(paste("time_spray_woman",treatment, sep="~")) ,data=dta))

##harv
res_time_woman[9,1,h] <- ifelse(h ==1, wtd.mean(dta$time_harv_woman[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.mean(dta$time_harv_woman[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$time_harv_woman[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.mean(dta$time_harv_woman[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.mean(dta$time_harv_woman[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time_woman[10,1,h] <- ifelse(h ==1, wtd.sd(dta$time_harv_woman[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.sd(dta$time_harv_woman[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$time_harv_woman[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.sd(dta$time_harv_woman[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.sd(dta$time_harv_woman[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time_woman[9,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_harv_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("time_harv_woman",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_time_woman[10,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_harv_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("time_harv_woman",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_time_woman[9,3,h] <- ifelse(totrep >0, RI("time_harv_woman",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("time_harv_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("time_harv_woman",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_time_woman[10,3,h] <- nobs(lm(as.formula(paste("time_harv_woman",treatment, sep="~")) ,data=dta))

##total
res_time_woman[11,1,h] <- ifelse(h ==1, wtd.mean(dta$tot_time_woman[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.mean(dta$tot_time_woman[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$tot_time_woman[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.mean(dta$tot_time_woman[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.mean(dta$tot_time_woman[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time_woman[12,1,h] <- ifelse(h ==1, wtd.sd(dta$tot_time_woman[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.sd(dta$tot_time_woman[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$tot_time_woman[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.sd(dta$tot_time_woman[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.sd(dta$tot_time_woman[dta$messenger != dta$recipient],dta$weights[dta$messenger != dta$recipient], na.rm=T)))))
res_time_woman[11,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("tot_time_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("tot_time_woman",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_time_woman[12,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("tot_time_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("tot_time_woman",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_time_woman[11,3,h] <- ifelse(totrep >0, RI("tot_time_woman",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("tot_time_woman",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("tot_time_woman",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_time_woman[12,3,h] <- nobs(lm(as.formula(paste("tot_time_woman",treatment, sep="~")) ,data=dta))


#res_time_w <- Bcorr(c("time_prep_w","time_plant_w","time_weed_w"),dta, res_time ,h)
}

## plots for presentations

### wrapper function to make a graph to be used for presentations
credplot.gg <- function(d,units){
 # d is a data frame with 4 columns
 # d$x gives variable names
 # d$y gives center point
 # d$ylo gives lower limits
 # d$yhi gives upper limits
 require(ggplot2)
 p <- ggplot(d, aes(x=x, y=y, ymin=ylo, ymax=yhi))+
 geom_pointrange(position=position_dodge(-.4))+ 
geom_text(aes(label=format(round(y, 2), nsmall = 2)), nudge_x = .1, hjust=-0.1)+
 geom_hline(yintercept = 0, linetype=2)+
 coord_flip()+
 xlab('') + ylab(units)+ theme(axis.text=element_text(size=18),
        axis.title=element_text(size=14,face="bold"),legend.text=element_text(size=18), legend.title=element_blank())+
    geom_errorbar(aes(ymin=ylo, ymax=yhi),position=position_dodge(-.4),width=0,cex=1.5) 
 return(p)
}

for (h in 1:7) {

sig <- 1.64  ## 90 percent
plot_ag <- data.frame(matrix(NA, 6,5))
names(plot_ag) <- c("x","y","ylo","yhi","grp")

plot_ag[1,1] <- "land preparation"
plot_ag[2,1] <- "planting"
plot_ag[3,1] <- "weeding"
plot_ag[4,1] <- "spraying"
plot_ag[5,1] <- "harvesting"
plot_ag[6,1] <- "total"


plot_ag[1,2] <- res_time[1,2,h]/res_time[1,1,h]
plot_ag[2,2] <- res_time[3,2,h]/res_time[3,1,h]
plot_ag[3,2] <- res_time[5,2,h]/res_time[5,1,h]
plot_ag[4,2] <- res_time[7,2,h]/res_time[7,1,h] 
plot_ag[5,2] <- res_time[9,2,h]/res_time[9,1,h]
plot_ag[6,2] <- res_time[11,2,h]/res_time[11,1,h]


plot_ag[1,3] <- (res_time[1,2,h] - sig*res_time[2,2,h])/res_time[1,1,h]
plot_ag[2,3] <- (res_time[3,2,h] - sig*res_time[4,2,h])/res_time[3,1,h]
plot_ag[3,3] <- (res_time[5,2,h] - sig*res_time[6,2,h])/res_time[5,1,h]
plot_ag[4,3] <- (res_time[7,2,h] - sig*res_time[8,2,h])/res_time[7,1,h]
plot_ag[5,3] <- (res_time[9,2,h] - sig*res_time[10,2,h])/res_time[9,1,h]
plot_ag[6,3] <- (res_time[11,2,h] - sig*res_time[12,2,h])/res_time[11,1,h]


plot_ag[1,4] <- (res_time[1,2,h] + sig*res_time[2,2,h])/res_time[1,1,h]
plot_ag[2,4] <- (res_time[3,2,h] + sig*res_time[4,2,h])/res_time[3,1,h]
plot_ag[3,4] <- (res_time[5,2,h] + sig*res_time[6,2,h])/res_time[5,1,h]
plot_ag[4,4] <- (res_time[7,2,h] + sig*res_time[8,2,h])/res_time[7,1,h]
plot_ag[5,4] <- (res_time[9,2,h] + sig*res_time[10,2,h])/res_time[9,1,h]
plot_ag[6,4] <- (res_time[11,2,h] + sig*res_time[12,2,h])/res_time[11,1,h]


plot_ag$x <- factor(plot_ag$x, plot_ag$x)
plot_ag$x <- factor(plot_ag$x, levels=rev(levels(plot_ag$x)))

pdf(paste(paste("/home/bjvca/data/projects/digital green/endline/results/timeplot",h,sep="_"),".pdf", sep=""))
print(credplot.gg(plot_ag,'%'))
dev.off()
}

