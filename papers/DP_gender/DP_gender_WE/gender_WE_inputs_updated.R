#rm(list=ls())
#source("/home/bjvca/data/projects/digital green/endline/data/init_gender_WE.R")
#baseline <-  read.csv("/home/bjvca/data/projects/digital green/baseline/base_merge.cvs")

#wget https://www.dropbox.com/s/sakp13112o1to6u/baseline.csv?dl=0
##wget https://www.dropbox.com/s/n7hn2x0y492ofgi/AWS.csv?dl=0
##wget https://www.dropbox.com/s/t6vkm91bawxbz8g/AWS2.csv?dl=0
#wget https://www.dropbox.com/s/iv11klkgogkb85p/AWS4.csv?dl=0

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
res_inputs_w <- array(NA, c(13,4,6)) 
rownames(res_inputs_w) <- c(  "DAP", "","urea","", "organic","","hybrid","","OPV","","index_inputs","","padj")
res_inputs_b <- array(NA, c(13,4,6)) 
rownames(res_inputs_b) <- c(  "DAP", "","urea","", "organic","","hybrid","","OPV","","index_inputs","","padj")
res_inputs_m <- array(NA, c(13,4,6)) 
rownames(res_inputs_m) <- c(  "DAP", "","urea","", "organic","","hybrid","","OPV","","index_inputs","","padj")

cl <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
registerDoParallel(cl)

### function definitions 
RI <- function(dep, indep, ctrls = NULL,  dta , nr_repl = 1000, w_int = NULL, h_int = h) {
# RI("(maizeeduc > 2)",treat ,ctrls = NULL, w_int="weights", dta= dta, nr_repl = totrep)
#RI("index" ,treat , contr_vars, w_int= w_int2,dta= data, nr_repl = 1000,h_int=1)
#indep <- treat 
#ctrls <- NULL
#h_int <- 2
#dep <- "log_maizearea_cultivation"
#dta <- dta
#nr_repl <- 100
#w_int <- dta2$weightes

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
if (h_int == 2) {
	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
		dta_sim <- data.table(dta)
 		setDT(dta_sim)[,perm:=sample(treat, replace=FALSE)[1:.N],by = (uniqID)]
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female")
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","female"))
		dta_sim$ivr <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$ivr,dta$treat))[table(dta$ivr, dta$treat)["yes",]>0]), "yes","no")
		dta_sim$sms <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$sms,dta$treat))[table(dta$sms, dta$treat)["yes",]>0]), "yes","no")
		return(abs(ifelse(is.null(ctrls),summary(lm(as.formula(paste(dep,indep,sep="~")), weights=weights, data=data.frame(dta_sim)))$coefficients[2,1],summary(lm(as.formula(paste(paste(dep,indep,sep="~"), ctrls,sep="+")), weights=weights, data=dta_sim))$coefficients[2,1])) > abs(crit) )
	}
} else if (h_int == 3) {
	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
		dta_sim <- data.table(dta)
 		setDT(dta_sim)[,perm:=sample(treat, replace=FALSE)[1:.N],by = (uniqID)]
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "couple")
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","female"))
		dta_sim$ivr <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$ivr,dta$treat))[table(dta$ivr, dta$treat)["yes",]>0]), "yes","no")
		dta_sim$sms <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$sms,dta$treat))[table(dta$sms, dta$treat)["yes",]>0]), "yes","no")
		return(abs(ifelse(is.null(ctrls),summary(lm(as.formula(paste(dep,indep,sep="~")), weights=weights, data=data.frame(dta_sim)))$coefficients[2,1],summary(lm(as.formula(paste(paste(dep,indep,sep="~"), ctrls,sep="+")), weights=weights, data=dta_sim))$coefficients[2,1])) > abs(crit) )
	}
}  else if (h_int == 4) {
	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
		dta_sim <- data.table(dta)
 		setDT(dta_sim)[,perm:=sample(treat, replace=FALSE)[1:.N],by = (uniqID)]
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","female"))
		dta_sim$ivr <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$ivr,dta$treat))[table(dta$ivr, dta$treat)["yes",]>0]), "yes","no")
		dta_sim$sms <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$sms,dta$treat))[table(dta$sms, dta$treat)["yes",]>0]), "yes","no")
		return(abs(ifelse(is.null(ctrls),summary(lm(as.formula(paste(dep,indep,sep="~")), weights=weights, data=data.frame(dta_sim)))$coefficients[2,1],summary(lm(as.formula(paste(paste(dep,indep,sep="~"), ctrls,sep="+")), weights=weights, data=dta_sim))$coefficients[2,1])) > abs(crit) )
	}
} else if (h_int == 5) {
	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
		dta_sim <- data.table(dta)
 		setDT(dta_sim)[,perm:=sample(treat, replace=FALSE)[1:.N],by = (uniqID)]
		#dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","female"))
		dta_sim$ivr <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$ivr,dta$treat))[table(dta$ivr, dta$treat)["yes",]>0]), "yes","no")
		dta_sim$sms <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$sms,dta$treat))[table(dta$sms, dta$treat)["yes",]>0]), "yes","no")
		return(abs(ifelse(is.null(ctrls),summary(lm(as.formula(paste(dep,indep,sep="~")), weights=weights, data=data.frame(dta_sim)))$coefficients[2,1],summary(lm(as.formula(paste(paste(dep,indep,sep="~"), ctrls,sep="+")), weights=weights, data=dta_sim))$coefficients[2,1])) > abs(crit) )
	}
} else if (h_int == 6) {
	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
		dta_sim <- data.table(dta)
 		setDT(dta_sim)[,perm:=sample(treat, replace=FALSE)[1:.N],by = (uniqID)]
		#dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","female"))
		dta_sim$ivr <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$ivr,dta$treat))[table(dta$ivr, dta$treat)["yes",]>0]), "yes","no")
		dta_sim$sms <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$sms,dta$treat))[table(dta$sms, dta$treat)["yes",]>0]), "yes","no")
		return(abs(ifelse(is.null(ctrls),summary(lm(as.formula(paste(dep,indep,sep="~")), weights=weights, data=data.frame(dta_sim)))$coefficients[2,1],summary(lm(as.formula(paste(paste(dep,indep,sep="~"), ctrls,sep="+")), weights=weights, data=dta_sim))$coefficients[2,1])) > abs(crit) )
	}
} else {
	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
		dta_sim <- data.table(dta)
 		setDT(dta_sim)[,perm:=sample(treat, replace=FALSE)[1:.N],by = (uniqID)]
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		
		
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","female"))
		dta_sim$ivr <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$ivr,dta$treat))[table(dta$ivr, dta$treat)["yes",]>0]), "yes","no")
		dta_sim$sms <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$sms,dta$treat))[table(dta$sms, dta$treat)["yes",]>0]), "yes","no")
		return(abs(ifelse(is.null(ctrls),summary(lm(as.formula(paste(dep,indep,sep="~")), weights=weights, data=data.frame(dta_sim)))$coefficients[2,1],summary(lm(as.formula(paste(paste(dep,indep,sep="~"), ctrls,sep="+")), weights=weights, data=dta_sim))$coefficients[2,1])) > abs(crit) )
	}
}

	return(sum(oper)/nr_repl)
}


RI_FWER <- function(deps, indep,ctrls = NULL, dta ,p_vals , nr_repl = 1000,  w_int = NULL, h_int = h) {
### function to control for FWER using simulation (familywise sharp null)
### inspired on https://egap.org/methods-guides/10-things-you-need-know-about-multiple-comparisons

#indep <- treat 
#ctrls <- NULL
#h_int <- 1
##dep <- "log_maizearea_cultivation"
#dta <- dta
#nr_repl <- 100
#w_int <- dta2$weightes


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
if (h_int == 2) {
	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
		dta_sim <- data.table(dta)
 		setDT(dta_sim)[,perm:=sample(treat, replace=FALSE)[1:.N],by = (uniqID)]
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female")
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","female"))
		dta_sim$ivr <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$ivr,dta$treat))[table(dta$ivr, dta$treat)["yes",]>0]), "yes","no")
		dta_sim$sms <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$sms,dta$treat))[table(dta$sms, dta$treat)["yes",]>0]), "yes","no")

if (is.null(ctrls)) {
return(unlist(lapply(deps, function(dvar)  summary(lm(as.formula(paste(dvar,indep,sep="~")), weights=weights,data=dta_sim))$coefficients[2,4])))
} else {
return(unlist(lapply(deps, function(dvar)  summary(lm(as.formula(paste(paste(dvar,indep,sep="~"),ctrls, sep="+")), weights=weights,data=dta_sim))$coefficients[2,4])))
}		
}
} else if (h_int == 3) {
	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
		dta_sim <- data.table(dta)
 		setDT(dta_sim)[,perm:=sample(treat, replace=FALSE)[1:.N],by = (uniqID)]
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "couple")
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","female"))
		dta_sim$ivr <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$ivr,dta$treat))[table(dta$ivr, dta$treat)["yes",]>0]), "yes","no")
		dta_sim$sms <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$sms,dta$treat))[table(dta$sms, dta$treat)["yes",]>0]), "yes","no")
		
if (is.null(ctrls)) {
return(unlist(lapply(deps, function(dvar)  summary(lm(as.formula(paste(dvar,indep,sep="~")), weights=weights,data=dta_sim))$coefficients[2,4])))
} else {
return(unlist(lapply(deps, function(dvar)  summary(lm(as.formula(paste(paste(dvar,indep,sep="~"),ctrls, sep="+")), weights=weights,data=dta_sim))$coefficients[2,4])))
}		
}
}  else if (h_int == 4) {
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
} else if (h_int == 5) {
	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
		dta_sim <- data.table(dta)
 		setDT(dta_sim)[,perm:=sample(treat, replace=FALSE)[1:.N],by = (uniqID)]
		#dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","female"))
		dta_sim$ivr <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$ivr,dta$treat))[table(dta$ivr, dta$treat)["yes",]>0]), "yes","no")
		dta_sim$sms <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$sms,dta$treat))[table(dta$sms, dta$treat)["yes",]>0]), "yes","no")
if (is.null(ctrls)) {
return(unlist(lapply(deps, function(dvar)  summary(lm(as.formula(paste(dvar,indep,sep="~")), weights=weights,data=dta_sim))$coefficients[2,4])))
} else {
return(unlist(lapply(deps, function(dvar)  summary(lm(as.formula(paste(paste(dvar,indep,sep="~"),ctrls, sep="+")), weights=weights,data=dta_sim))$coefficients[2,4])))
}		
}	
} else if (h_int == 6) {
	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
		dta_sim <- data.table(dta)
 		setDT(dta_sim)[,perm:=sample(treat, replace=FALSE)[1:.N],by = (uniqID)]
		#dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","female"))
		dta_sim$ivr <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$ivr,dta$treat))[table(dta$ivr, dta$treat)["yes",]>0]), "yes","no")
		dta_sim$sms <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$sms,dta$treat))[table(dta$sms, dta$treat)["yes",]>0]), "yes","no")
		
if (is.null(ctrls)) {
return(unlist(lapply(deps, function(dvar)  summary(lm(as.formula(paste(dvar,indep,sep="~")), weights=weights,data=dta_sim))$coefficients[2,4])))
} else {
return(unlist(lapply(deps, function(dvar)  summary(lm(as.formula(paste(paste(dvar,indep,sep="~"),ctrls, sep="+")), weights=weights,data=dta_sim))$coefficients[2,4])))
}		
}
} else {
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

ctrls <- NULL


totrep <- 10000

####

for (h in 1:3) {
if (h==1) {
############################################ H1: empower: rec==couple or woman - rec==male #########################################################
dta <- dta_copy
treatment <- "(recipient != 'male') +ivr+sms+ as.factor(messenger)" 

dta$weights <- 1
dta$weights[dta$recipient == "female"] <-  1053/1135

} else if (h==2) {
############################################ H1a: empower : rec==female - rec==male ###################################################
dta <- subset(dta_copy, recipient == "male" | recipient == "female")
treatment <- "(recipient == 'female') +ivr+sms+as.factor(messenger)" 

dta$weights <- 1

} else if (h==3) {
############################################ H1b: rec==couple - rec==male  ###################################################
dta <- subset(dta_copy, recipient == "male" | recipient == "couple")
treatment <- "(recipient == 'couple') +ivr+sms+ as.factor(messenger)"

dta$weights <- 1

} else if (h==4) {
############################################ H2: mes==couple or woman - mes==male ###################################################
dta <- dta_copy
ctrls<- NULL
dta$weights <- 1
dta$weights[dta$messenger == "female"] <-  1106/1108
treatment <- "(messenger != 'male') +ivr+sms+as.factor(recipient)"
} else if (h==5) {
############################# H2a: challenging role incongruity###########################
#############  messenger== 'female or couple' & recipient=='male' - messenger== 'male' & recipient=='male' ##############################
dta <- dta_copy
dta$recipient <- factor(dta$recipient)
dta$messenger <- factor(dta$messenger)
dta <- subset(dta, recipient =='female'  )
set.seed(54321)

treatment <- "(messenger!= 'male') +ivr+sms"


dta$weights <- 1
dta$weights[dta$messenger == "female" & dta$recipient == "female"] <-  343/348

} else if (h==6) {
######################## H4: challenging role incongruity###########################
#############  messenger== 'female or couple' & recipient=='male' - messenger== 'male' & recipient=='male' ##############################
dta <- dta_copy
dta$recipient <- factor(dta$recipient)
dta$messenger <- factor(dta$messenger)
dta <- subset(dta, recipient =='male'  )
set.seed(54321)

treatment <- "(messenger!= 'male') +ivr+sms"


dta$weights <- 1
dta$weights[dta$messenger == "female" & dta$recipient == "male"] <-  339/354

}

print(h)
################################################## inputs woman  #####################################################
dta_glob <- dta

#use of DAP
man <- "decDAP_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")


dta_ind <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
share_woman_decide <- aggregate(dta_ind$decide,list(dta_ind$hhid),mean,na.rm=T)
names(share_woman_decide) <- c("hhid","share_woman_dec_maize")
dta <- merge(dta_glob,share_woman_decide,by="hhid")
dta$share_woman_dec_maize[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$adopt <- dta$share_woman_dec_maize


res_inputs_w[1,1,h] <- ifelse(h %in% c(6), wtd.mean(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_w[2,1,h] <-   ifelse(h %in% c(6), wtd.sd(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_w[1,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_inputs_w[2,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_inputs_w[1,3,h] <- ifelse(totrep >0, RI("adopt",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_inputs_w[2,3,h] <- nobs(lm(as.formula(paste("adopt",treatment, sep="~")) ,data=dta))

#23. Who decided to use this spacing and/or seed density on ${garden1} plot?  
dta <- dta_glob
man <- "decUrea_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

dta_ind <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
share_woman_decide <- aggregate(dta_ind$decide,list(dta_ind$hhid),mean,na.rm=T)
names(share_woman_decide) <- c("hhid","share_woman_dec_maize")
dta <- merge(dta_glob,share_woman_decide,by="hhid")
dta$share_woman_dec_maize[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$adopt <- dta$share_woman_dec_maize

res_inputs_w[3,1,h] <- ifelse(h %in% c(6), wtd.mean(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_w[4,1,h] <-   ifelse(h %in% c(6), wtd.sd(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))

res_inputs_w[3,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_inputs_w[4,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_inputs_w[3,3,h] <- ifelse(totrep >0, RI("adopt",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_inputs_w[4,3,h] <- nobs(lm(as.formula(paste("adopt",treatment, sep="~")) ,data=dta))

#23. 25.Who decided  on this particular way to fight striga (kayongo)? 
dta <- dta_glob
man <- "decOrg_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

dta_ind <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
share_woman_decide <- aggregate(dta_ind$decide,list(dta_ind$hhid),mean,na.rm=T)
names(share_woman_decide) <- c("hhid","share_woman_dec_maize")
dta <- merge(dta_glob,share_woman_decide,by="hhid")
dta$share_woman_dec_maize[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$adopt <- dta$share_woman_dec_maize

res_inputs_w[5,1,h] <- ifelse(h %in% c(6), wtd.mean(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_w[6,1,h] <-   ifelse(h %in% c(6), wtd.sd(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))

res_inputs_w[5,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_inputs_w[6,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_inputs_w[5,3,h] <- ifelse(totrep >0, RI("adopt",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_inputs_w[6,3,h] <- nobs(lm(as.formula(paste("adopt",treatment, sep="~")) ,data=dta))

#27. Who decided on when to do the first weeding for ${garden1} plot?
dta <- dta_glob
man <- "decHybrid_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

dta_ind <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
share_woman_decide <- aggregate(dta_ind$decide,list(dta_ind$hhid),mean,na.rm=T)
names(share_woman_decide) <- c("hhid","share_woman_dec_maize")
dta <- merge(dta_glob,share_woman_decide,by="hhid")
dta$share_woman_dec_maize[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$adopt <- dta$share_woman_dec_maize

res_inputs_w[7,1,h] <- ifelse(h %in% c(6), wtd.mean(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_w[8,1,h] <-   ifelse(h %in% c(6), wtd.sd(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))

res_inputs_w[7,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_inputs_w[8,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_inputs_w[7,3,h] <- ifelse(totrep >0, RI("adopt",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_inputs_w[8,3,h] <- nobs(lm(as.formula(paste("adopt",treatment, sep="~")) ,data=dta))

dta <- dta_glob
man <- "decOPV_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

dta_ind <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
share_woman_decide <- aggregate(dta_ind$decide,list(dta_ind$hhid),mean,na.rm=T)
names(share_woman_decide) <- c("hhid","share_woman_dec_maize")
dta <- merge(dta_glob,share_woman_decide,by="hhid")
dta$share_woman_dec_maize[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$adopt <- dta$share_woman_dec_maize

res_inputs_w[9,1,h] <- ifelse(h %in% c(6), wtd.mean(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_w[10,1,h] <-   ifelse(h %in% c(6), wtd.sd(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))

res_inputs_w[9,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_inputs_w[10,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_inputs_w[9,3,h] <- ifelse(totrep >0, RI("adopt",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_inputs_w[10,3,h] <- nobs(lm(as.formula(paste("adopt",treatment, sep="~")) ,data=dta))
### index /// disregard names, this is copy paste from the gender_WE_decisions.R

man <- "decDAP_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_mgt_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_mgt_woman) <- c("hhid","time","mgt_woman")
man <- "decUrea_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_dectime_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_dectime_woman) <- c("hhid","time","dectime_woman")
man <- "decOrg_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decspace_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decspace_woman) <-  c("hhid","time","decspace_woman")
man <- "decHybrid_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decstriga_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decstriga_woman) <-  c("hhid","time","decstriga_woman")
man <- "decOPV_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decweed_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decweed_woman) <-  c("hhid","time","decweed_woman")



all_ind <- merge(merge(merge(merge(dta_ind_mgt_woman,dta_ind_dectime_woman),dta_ind_decspace_woman),dta_ind_decstriga_woman), dta_ind_decweed_woman )
all_hh <- aggregate(all_ind[c( "mgt_woman"  ,  "dectime_woman", "decspace_woman", "decstriga_woman","decweed_woman")],list(all_ind$hhid),mean,na.rm=T)
names(all_hh)[names(all_hh) == 'Group.1'] <- 'hhid'

dta <- merge(dta_glob,all_hh,by="hhid")
dta$mgt_woman[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA

indexer <- FW_index(treatment,c( "mgt_woman"  ,  "dectime_woman", "decspace_woman", "decstriga_woman","decweed_woman"),ctrls,w_int="weights",dta, nr_repl=totrep)


res_inputs_w[11,1,h] <- ifelse(h %in% c(6), wtd.mean(indexer[[3]]$index[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(indexer[[3]]$index[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(indexer[[3]]$index[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(indexer[[3]]$index[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_w[12,1,h] <- ifelse(h %in% c(6), wtd.sd(indexer[[3]]$index[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(indexer[[3]]$index[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(indexer[[3]]$index[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(indexer[[3]]$index[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))


res_inputs_w[11,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_inputs_w[12,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_inputs_w[11,3,h] <-  indexer[[2]]
res_inputs_w[12,3,h] <-  nobs(indexer[[1]])


res_inputs_w[13,1:3,h] <- RI_FWER(deps= c( "mgt_woman"  ,  "dectime_woman", "decspace_woman", "decstriga_woman","decweed_woman") ,indep = treatment , ctrls = ctrls,dta =dta, p_vals = res_inputs_w[c(1,3,5,7,9),3,h], nr_repl = totrep, w_int="weights")

################################################## decisions - jointly made #####################################################

#use of DAP
man <- "decDAP_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")


dta_ind <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
share_woman_decide <- aggregate(dta_ind$decide,list(dta_ind$hhid),mean,na.rm=T)
names(share_woman_decide) <- c("hhid","share_woman_dec_maize")
dta <- merge(dta_glob,share_woman_decide,by="hhid")
dta$share_woman_dec_maize[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$adopt <- dta$share_woman_dec_maize

res_inputs_b[1,1,h] <- ifelse(h %in% c(6), wtd.mean(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_b[2,1,h] <-   ifelse(h %in% c(6), wtd.sd(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_b[1,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_inputs_b[2,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_inputs_b[1,3,h] <- ifelse(totrep >0, RI("adopt",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_inputs_b[2,3,h] <- nobs(lm(as.formula(paste("adopt",treatment, sep="~")) ,data=dta))

#23. Who decided to use this spacing and/or seed density on ${garden1} plot?  
dta <- dta_glob
man <- "decUrea_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

dta_ind <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
share_woman_decide <- aggregate(dta_ind$decide,list(dta_ind$hhid),mean,na.rm=T)
names(share_woman_decide) <- c("hhid","share_woman_dec_maize")
dta <- merge(dta_glob,share_woman_decide,by="hhid")
dta$share_woman_dec_maize[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$adopt <- dta$share_woman_dec_maize

res_inputs_b[3,1,h] <- ifelse(h %in% c(6), wtd.mean(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_b[4,1,h] <-   ifelse(h %in% c(6), wtd.sd(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_b[3,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_inputs_b[4,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_inputs_b[3,3,h] <- ifelse(totrep >0, RI("adopt",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_inputs_b[4,3,h] <- nobs(lm(as.formula(paste("adopt",treatment, sep="~")) ,data=dta))

#23. 25.Who decided  on this particular way to fight striga (kayongo)? 
dta <- dta_glob
man <- "decOrg_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

dta_ind <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
share_woman_decide <- aggregate(dta_ind$decide,list(dta_ind$hhid),mean,na.rm=T)
names(share_woman_decide) <- c("hhid","share_woman_dec_maize")
dta <- merge(dta_glob,share_woman_decide,by="hhid")
dta$share_woman_dec_maize[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$adopt <- dta$share_woman_dec_maize

res_inputs_b[5,1,h] <- ifelse(h %in% c(6), wtd.mean(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_b[6,1,h] <-   ifelse(h %in% c(6), wtd.sd(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_b[5,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_inputs_b[6,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_inputs_b[5,3,h] <- ifelse(totrep >0, RI("adopt",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_inputs_b[6,3,h] <- nobs(lm(as.formula(paste("adopt",treatment, sep="~")) ,data=dta))

#27. Who decided on when to do the first weeding for ${garden1} plot?
dta <- dta_glob
man <- "decHybrid_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

dta_ind <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
share_woman_decide <- aggregate(dta_ind$decide,list(dta_ind$hhid),mean,na.rm=T)
names(share_woman_decide) <- c("hhid","share_woman_dec_maize")
dta <- merge(dta_glob,share_woman_decide,by="hhid")
dta$share_woman_dec_maize[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$adopt <- dta$share_woman_dec_maize

res_inputs_b[7,1,h] <- ifelse(h %in% c(6), wtd.mean(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_b[8,1,h] <-   ifelse(h %in% c(6), wtd.sd(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_b[7,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_inputs_b[8,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_inputs_b[7,3,h] <- ifelse(totrep >0, RI("adopt",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_inputs_b[8,3,h] <- nobs(lm(as.formula(paste("adopt",treatment, sep="~")) ,data=dta))

dta <- dta_glob
man <- "decOPV_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

dta_ind <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
share_woman_decide <- aggregate(dta_ind$decide,list(dta_ind$hhid),mean,na.rm=T)
names(share_woman_decide) <- c("hhid","share_woman_dec_maize")
dta <- merge(dta_glob,share_woman_decide,by="hhid")
dta$share_woman_dec_maize[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$adopt <- dta$share_woman_dec_maize

res_inputs_b[9,1,h] <- ifelse(h %in% c(6), wtd.mean(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_b[10,1,h] <-   ifelse(h %in% c(6), wtd.sd(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_b[9,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_inputs_b[10,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_inputs_b[9,3,h] <- ifelse(totrep >0, RI("adopt",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_inputs_b[10,3,h] <- nobs(lm(as.formula(paste("adopt",treatment, sep="~")) ,data=dta))
### index /// disregard names, this is copy paste from the gender_WE_decisions.R

man <- "decDAP_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_mgt_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_mgt_woman) <- c("hhid","time","mgt_woman")
man <- "decUrea_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_dectime_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_dectime_woman) <- c("hhid","time","dectime_woman")
man <- "decOrg_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decspace_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decspace_woman) <-  c("hhid","time","decspace_woman")
man <- "decHybrid_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decstriga_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decstriga_woman) <-  c("hhid","time","decstriga_woman")
man <- "decOPV_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decweed_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decweed_woman) <-  c("hhid","time","decweed_woman")



all_ind <- merge(merge(merge(merge(dta_ind_mgt_woman,dta_ind_dectime_woman),dta_ind_decspace_woman),dta_ind_decstriga_woman), dta_ind_decweed_woman )
all_hh <- aggregate(all_ind[c( "mgt_woman"  ,  "dectime_woman", "decspace_woman", "decstriga_woman","decweed_woman")],list(all_ind$hhid),mean,na.rm=T)
names(all_hh)[names(all_hh) == 'Group.1'] <- 'hhid'

dta <- merge(dta_glob,all_hh,by="hhid")
dta$mgt_woman[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA

indexer <- FW_index(treatment,c( "mgt_woman"  ,  "dectime_woman", "decspace_woman", "decstriga_woman","decweed_woman"),ctrls,w_int="weights",dta, nr_repl=totrep)


res_inputs_b[11,1,h] <- ifelse(h %in% c(6), wtd.mean(indexer[[3]]$index[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(indexer[[3]]$index[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(indexer[[3]]$index[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(indexer[[3]]$index[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_b[12,1,h] <- ifelse(h %in% c(6), wtd.sd(indexer[[3]]$index[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(indexer[[3]]$index[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(indexer[[3]]$index[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(indexer[[3]]$index[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))


res_inputs_b[11,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_inputs_b[12,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_inputs_b[11,3,h] <-  indexer[[2]]
res_inputs_b[12,3,h] <-  nobs(indexer[[1]])


res_inputs_b[13,1:3,h] <- RI_FWER(deps= c( "mgt_woman"  ,  "dectime_woman", "decspace_woman", "decstriga_woman","decweed_woman") ,indep = treatment , ctrls = ctrls,dta =dta, p_vals = res_inputs_b[c(1,3,5,7,9),3,h], nr_repl = totrep, w_int="weights")

################################ men

#use of DAP
man <- "decDAP_man_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")


dta_ind <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
share_woman_decide <- aggregate(dta_ind$decide,list(dta_ind$hhid),mean,na.rm=T)
names(share_woman_decide) <- c("hhid","share_woman_dec_maize")
dta <- merge(dta_glob,share_woman_decide,by="hhid")
dta$share_woman_dec_maize[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$adopt <- dta$share_woman_dec_maize

res_inputs_m[1,1,h] <- ifelse(h %in% c(6), wtd.mean(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_m[2,1,h] <-   ifelse(h %in% c(6), wtd.sd(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_m[1,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_inputs_m[2,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_inputs_m[1,3,h] <- ifelse(totrep >0, RI("adopt",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_inputs_m[2,3,h] <- nobs(lm(as.formula(paste("adopt",treatment, sep="~")) ,data=dta))

#23. Who decided to use this spacing and/or seed density on ${garden1} plot?  
dta <- dta_glob
man <- "decUrea_man_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

dta_ind <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
share_woman_decide <- aggregate(dta_ind$decide,list(dta_ind$hhid),mean,na.rm=T)
names(share_woman_decide) <- c("hhid","share_woman_dec_maize")
dta <- merge(dta_glob,share_woman_decide,by="hhid")
dta$share_woman_dec_maize[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$adopt <- dta$share_woman_dec_maize

res_inputs_m[3,1,h] <- ifelse(h %in% c(6), wtd.mean(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_m[4,1,h] <-   ifelse(h %in% c(6), wtd.sd(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_m[3,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_inputs_m[4,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_inputs_m[3,3,h] <- ifelse(totrep >0, RI("adopt",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_inputs_m[4,3,h] <- nobs(lm(as.formula(paste("adopt",treatment, sep="~")) ,data=dta))

#23. 25.Who decided  on this particular way to fight striga (kayongo)? 
dta <- dta_glob
man <- "decOrg_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

dta_ind <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
share_woman_decide <- aggregate(dta_ind$decide,list(dta_ind$hhid),mean,na.rm=T)
names(share_woman_decide) <- c("hhid","share_woman_dec_maize")
dta <- merge(dta_glob,share_woman_decide,by="hhid")
dta$share_woman_dec_maize[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$adopt <- dta$share_woman_dec_maize

res_inputs_m[5,1,h] <- ifelse(h %in% c(6), wtd.mean(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_m[6,1,h] <-   ifelse(h %in% c(6), wtd.sd(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_m[5,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_inputs_m[6,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_inputs_m[5,3,h] <- ifelse(totrep >0, RI("adopt",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_inputs_m[6,3,h] <- nobs(lm(as.formula(paste("adopt",treatment, sep="~")) ,data=dta))

#27. Who decided on when to do the first weeding for ${garden1} plot?
dta <- dta_glob
man <- "decHybrid_man_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

dta_ind <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
share_woman_decide <- aggregate(dta_ind$decide,list(dta_ind$hhid),mean,na.rm=T)
names(share_woman_decide) <- c("hhid","share_woman_dec_maize")
dta <- merge(dta_glob,share_woman_decide,by="hhid")
dta$share_woman_dec_maize[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$adopt <- dta$share_woman_dec_maize

res_inputs_m[7,1,h] <- ifelse(h %in% c(6), wtd.mean(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_m[8,1,h] <-   ifelse(h %in% c(6), wtd.sd(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_m[7,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_inputs_m[8,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_inputs_m[7,3,h] <- ifelse(totrep >0, RI("adopt",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_inputs_m[8,3,h] <- nobs(lm(as.formula(paste("adopt",treatment, sep="~")) ,data=dta))

dta <- dta_glob
man <- "decOPV_man_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

dta_ind <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
share_woman_decide <- aggregate(dta_ind$decide,list(dta_ind$hhid),mean,na.rm=T)
names(share_woman_decide) <- c("hhid","share_woman_dec_maize")
dta <- merge(dta_glob,share_woman_decide,by="hhid")
dta$share_woman_dec_maize[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$adopt <- dta$share_woman_dec_maize

res_inputs_m[9,1,h] <- ifelse(h %in% c(6), wtd.mean(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_m[10,1,h] <-   ifelse(h %in% c(6), wtd.sd(dta$adopt[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$adopt[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$adopt[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$adopt[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_m[9,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_inputs_m[10,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_inputs_m[9,3,h] <- ifelse(totrep >0, RI("adopt",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("adopt",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("adopt",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_inputs_m[10,3,h] <- nobs(lm(as.formula(paste("adopt",treatment, sep="~")) ,data=dta))
### index /// disregard names, this is copy paste from the gender_WE_decisions.R

man <- "decDAP_man_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_mgt_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_mgt_woman) <- c("hhid","time","mgt_woman")
man <- "decUrea_man_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_dectime_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_dectime_woman) <- c("hhid","time","dectime_woman")
man <- "decOrg_man_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decspace_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decspace_woman) <-  c("hhid","time","decspace_woman")
man <- "decHybrid_man_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decstriga_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decstriga_woman) <-  c("hhid","time","decstriga_woman")
man <- "decOPV_man_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decweed_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decweed_woman) <-  c("hhid","time","decweed_woman")



all_ind <- merge(merge(merge(merge(dta_ind_mgt_woman,dta_ind_dectime_woman),dta_ind_decspace_woman),dta_ind_decstriga_woman), dta_ind_decweed_woman )
all_hh <- aggregate(all_ind[c( "mgt_woman"  ,  "dectime_woman", "decspace_woman", "decstriga_woman","decweed_woman")],list(all_ind$hhid),mean,na.rm=T)
names(all_hh)[names(all_hh) == 'Group.1'] <- 'hhid'

dta <- merge(dta_glob,all_hh,by="hhid")
dta$mgt_woman[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA

indexer <- FW_index(treatment,c( "mgt_woman"  ,  "dectime_woman", "decspace_woman", "decstriga_woman","decweed_woman"),ctrls,w_int="weights",dta, nr_repl=totrep)


res_inputs_m[11,1,h] <- ifelse(h %in% c(6), wtd.mean(indexer[[3]]$index[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(indexer[[3]]$index[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(indexer[[3]]$index[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(indexer[[3]]$index[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_inputs_m[12,1,h] <- ifelse(h %in% c(6), wtd.sd(indexer[[3]]$index[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(indexer[[3]]$index[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(indexer[[3]]$index[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(indexer[[3]]$index[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))

res_inputs_m[11,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_inputs_m[12,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_inputs_m[11,3,h] <-  indexer[[2]]
res_inputs_m[12,3,h] <-  nobs(indexer[[1]])


res_inputs_m[13,1:3,h] <- RI_FWER(deps= c( "mgt_woman"  ,  "dectime_woman", "decspace_woman", "decstriga_woman","decweed_woman") ,indep = treatment , ctrls = ctrls,dta =dta, p_vals = res_inputs_m[c(1,3,5,7,9),3,h], nr_repl = totrep, w_int="weights")
}

save(res_inputs_m, file = "res_inputs_m.RData")
save(res_inputs_b, file = "res_inputs_b.RData")
save(res_inputs_w, file = "res_inputs_w.RData")
print(res_inputs_m)
print(res_inputs_b)
print(res_inputs_w)
print("exit")




