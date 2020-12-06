rm(list=ls())

source("/home/bjvca/data/projects/digital green/endline/data/init_gender.R")
baseline <-  read.csv("/home/bjvca/data/projects/digital green/baseline/base_merge.cvs")

## uncomment lines below if run on AWS
#wget https://www.dropbox.com/s/sakp13112o1to6u/baseline.csv?dl=0
#wget https://www.dropbox.com/s/n7hn2x0y492ofgi/AWS.csv?dl=0
#wget https://www.dropbox.com/s/t6vkm91bawxbz8g/AWS2.csv?dl=0
#install.packages(c("ggplot2","doParallel","data.table","dplyr","Hmisc"))

#rm(list=ls())
#dta <- read.csv("AWS.csv")
#baseline <- read.csv("baseline.csv")


library(ggplot2)
library(doParallel)
library(data.table)
library(dplyr)
library(Hmisc)
library(car)

set.seed(07032018)

### indexing results arrays
res_know_m <- array(NA, c(11,4,6)) 
rownames(res_know_m) <- c("know_space","","know_combine","","know_weed","", "know_armyworm","","know_ind","","padj")
res_know_w <- array(NA, c(11,4,6)) 
rownames(res_know_w) <- c("know_space","","know_combine","","know_weed","", "know_armyworm","","know_ind","","padj")
res_know_b <- array(NA, c(11,4,6)) 
rownames(res_know_b) <- c("know_space","","know_combine","","know_weed","", "know_armyworm","","know_ind","","padj")

plot_res <- array(NA, c(12,5,6))
colnames(plot_res) <-  c("x","y","ylo","yhi","grp")

cl <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
registerDoParallel(cl)

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

FW_index <- function(treat, indexer, data, nr_repl=0) {
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
mod <- lm(as.formula(paste("index",treat,sep="~")) , data=data)

					
if (nr_repl > 0) { 
	data$index <- as.vector(data$index)
	sig <- RI("index" ,treat , data, nr_repl = nr_repl)
} else {
	sig <- summary(lm(as.formula(paste("index",treat,sep="~")) , data=data))$coefficients[2,4]
}
return(list(mod,sig, data))
}

### wrapper function to make graphs to be used for presentations
credplot.gg <- function(d,units, hypo){
 # d is a data frame with 4 columns
 # d$x gives variable names
 # d$y gives center point
 # d$ylo gives lower limits
 # d$yhi gives upper limits
 require(ggplot2)
 p <- ggplot(d, aes(x=x, y=y, ymin=ylo, ymax=yhi, colour=as.factor(grp)))+
 geom_pointrange(position=position_dodge(-.4))+
 geom_hline(yintercept = 0, linetype=2)+
 coord_flip(ylim = c(-.25,.25))+
 xlab('') + ylab(units)+ labs(title=hypo)  + theme(axis.text=element_text(size=18),
        axis.title=element_text(size=14,face="bold"),legend.text=element_text(size=18), plot.title = element_text(size=22,hjust = 0.5), legend.title=element_blank())+
    geom_errorbar(aes(ymin=ylo, ymax=yhi),position=position_dodge(-.4),width=0,cex=1.5) + scale_colour_manual(values = c('#25276d','#00d8ff','#cb00ff'))
 return(p)
}


#########################################################################################

## drop the control
dta <- subset(dta, messenger != "ctrl")

dta_copy <- dta


#names(baseline)[names(baseline) == 'know_space'] <- 'b_know_space'
#names(baseline)[names(baseline) == 'know_combine'] <- 'b_know_combine'
#names(baseline)[names(baseline) == 'know_weed'] <- 'b_know_weed'
#baseline$recipient <- NULL
#baseline$messenger <- NULL
#baseline$ivr <- NULL
#baseline$sms <- NULL
#baseline$maizeeduc <- (baseline$maizeeduc > 2)
#baseline$maizeprinfo_receiv_spouse <- baseline$maizeprinfo_receiv_spouse=='Yes' 
#baseline$maizeprinput_use <- (baseline$maizeprinput_use=='Yes')
#baseline$maizemobile <- (baseline$maizemobile=='Yes')
#### I use inverse hyperbolic sine transform instead of log to keep zeros (about 80)
#baseline$yield <- asinh(baseline$yield)
ctrls <- NULL

#set totrep to zero if you do not want simulation based inferecne
totrep <- 0


####
outcomes_ind <- c("know_space","know_comine","know_weed","know_armyworm")
out_w <- array(NA,c(length(outcomes_ind),12,2))
out_m <- array(NA,c(length(outcomes_ind),12,2))
out_j <- array(NA,c(length(outcomes_ind),12,2))
#relevel treatments to make sure male is control group
dta$recipient <- as.factor(dta$recipient)
dta$messenger <- as.factor(dta$messenger)
treatment <- "recipient*messenger"
dta <- within(dta, recipient <- relevel(recipient, ref = "male"))
dta <- within(dta, messenger <- relevel(messenger, ref = "male"))
i <- 1
for (outcome in c("know_space","know_combine","know_weed","know_armyworm")) {

out_w[i,1,1] <- mean(unlist(dta[dta$recipient=="male",][paste(outcome,"w",sep="_")]), na.rm=T)
out_m[i,1,1] <- mean(unlist(dta[dta$recipient=="male",][paste(outcome,"m",sep="_")]), na.rm=T)
out_j[i,1,1] <- mean(unlist(dta[dta$recipient=="male",][paste(outcome,"j",sep="_")]), na.rm=T)

out_w[i,2,1] <- sd(unlist(dta[dta$recipient=="male",][paste(outcome,"w",sep="_")]), na.rm=T)
out_m[i,2,1] <- sd(unlist(dta[dta$recipient=="male",][paste(outcome,"m",sep="_")]), na.rm=T)
out_j[i,2,1] <- sd(unlist(dta[dta$recipient=="male",][paste(outcome,"j",sep="_")]), na.rm=T)

out_w[i,1,2] <- mean(unlist(dta[dta$messenger=="male",][paste(outcome,"w",sep="_")]), na.rm=T)
out_m[i,1,2] <- mean(unlist(dta[dta$messenger=="male",][paste(outcome,"m",sep="_")]), na.rm=T)
out_j[i,1,2] <- mean(unlist(dta[dta$messenger=="male",][paste(outcome,"j",sep="_")]), na.rm=T)

out_w[i,2,2] <- sd(unlist(dta[dta$messenger=="male",][paste(outcome,"w",sep="_")]), na.rm=T)
out_m[i,2,2] <- sd(unlist(dta[dta$messenger=="male",][paste(outcome,"m",sep="_")]), na.rm=T)
out_j[i,2,2] <- sd(unlist(dta[dta$messenger=="male",][paste(outcome,"j",sep="_")]), na.rm=T)

mod_w <- lm(as.formula(paste(paste(outcome,"w",sep="_"),treatment, sep="~")),data=dta)
mod_m <- lm(as.formula(paste(paste(outcome,"m",sep="_"),treatment, sep="~")),data=dta)
mod_j <- lm(as.formula(paste(paste(outcome,"j",sep="_"),treatment, sep="~")),data=dta)
#out[,,1] collects results for T1
#couple treatment
#coef estimate, se and p-value
out_w[i,3:5,1] <- summary(mod_w)$coefficients[2,c(1:2,4)]
out_m[i,3:5,1] <- summary(mod_m)$coefficients[2,c(1:2,4)]
out_j[i,3:5,1] <- summary(mod_j)$coefficients[2,c(1:2,4)]

#lower and upper conf limits
out_w[i,6:7,1] <- confint(mod_w)[2,]
out_m[i,6:7,1] <- confint(mod_m)[2,]
out_j[i,6:7,1] <- confint(mod_j)[2,]

#woman treatment
#coef estimate, se and p-value
out_w[i,8:10,1] <- summary(mod_w)$coefficients[3,c(1:2,4)]
out_m[i,8:10,1] <- summary(mod_m)$coefficients[3,c(1:2,4)]
out_j[i,8:10,1] <- summary(mod_j)$coefficients[3,c(1:2,4)]
#lower and upper conf limits
out_w[i,11:12,1] <- confint(mod_w)[3,]
out_m[i,11:12,1] <- confint(mod_m)[3,]
out_j[i,11:12,1] <- confint(mod_j)[3,]

#out[,,2] collects results for T2
#couple treatment
#coef estimate, se and p-value
out_w[i,3:5,2] <- summary(mod_w)$coefficients[4,c(1:2,4)]
out_m[i,3:5,2] <- summary(mod_m)$coefficients[4,c(1:2,4)]
out_j[i,3:5,2] <- summary(mod_j)$coefficients[4,c(1:2,4)]

#lower and upper conf limits
out_w[i,6:7,2] <- confint(mod_w)[4,]
out_m[i,6:7,2] <- confint(mod_m)[4,]
out_j[i,6:7,2] <- confint(mod_j)[4,]

#woman treatment
#coef estimate, se and p-value
out_w[i,8:10,2] <- summary(mod_w)$coefficients[5,c(1:2,4)]
out_m[i,8:10,2] <- summary(mod_m)$coefficients[5,c(1:2,4)]
out_j[i,8:10,2] <- summary(mod_j)$coefficients[5,c(1:2,4)]
#lower and upper conf limits
out_w[i,11:12,2] <- confint(mod_w)[5,]
out_m[i,11:12,2] <- confint(mod_m)[5,]
out_j[i,11:12,2] <- confint(mod_j)[5,]
i <- i + 1
}


summary(lm(know_space_w~dta$messenger,dta[dta$recipient == "male"]))

treatment <- "recipient*messenger"
mod < lm(as.formula(paste("know_space_w",treatment, sep="~")),data=dta)
summary(mod)

linearHypothesis(mod,"recipientcouple = recipientfemale")

#using bootstrap

mod <- lm(as.formula(paste("know_space_w",treatment, sep="~")),data=na.omit(dta[c("know_space_w","messenger","recipient")]))
Boot(mod)


test.boot <- Boot(mod)

linearHypothesis(mod,"recipientcouple = recipientfemale",vcov=vcov(test.boot) )

print(h)
################################################## knowledge  #####################################################
res_know_w[1,1,h] <- ifelse(h %in% c(6), wtd.mean(dta$know_space_w[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$know_space_w[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$know_space_w[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$know_space_w[dta$recipient == "female" & dta$messenger == "male"], dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_know_w[2,1,h] <-  ifelse(h %in% c(6), wtd.sd(dta$know_space_w[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$know_space_w[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$know_space_w[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$know_space_w[dta$recipient == "female" & dta$messenger == "male"], dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_know_w[1,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_space_w",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("know_space_w",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_know_w[2,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_space_w",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("know_space_w",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_know_w[1,3,h] <- ifelse(totrep >0, RI("know_space_w",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_space_w",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("know_space_w",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_know_w[2,3,h] <- nobs(lm(as.formula(paste("know_space_w",treatment, sep="~")) ,data=dta))


res_know_w[3,1,h] <-ifelse(h %in% c(6), wtd.mean(dta$know_combine_w[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$know_combine_w[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$know_combine_w[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$know_combine_w[dta$recipient == "female" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "female"], na.rm=T)))))
res_know_w[4,1,h] <-  ifelse(h %in% c(6), wtd.sd(dta$know_combine_w[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$know_combine_w[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$know_combine_w[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5,wtd.sd(dta$know_combine_w[dta$recipient == "female" & dta$messenger == "male"], dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_know_w[3,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_combine_w",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("know_combine_w",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_know_w[4,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_combine_w",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("know_combine_w",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_know_w[3,3,h] <- ifelse(totrep >0, RI("know_combine_w",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_combine_w",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("know_combine_w",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_know_w[4,3,h] <- nobs(lm(as.formula(paste("know_combine_w",treatment, sep="~")) ,data=dta))

res_know_w[5,1,h] <- ifelse(h %in% c(6), wtd.mean(dta$know_weed_w[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$know_weed_w[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$know_weed_w[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$know_weed_w[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_know_w[6,1,h] <-  ifelse(h %in% c(6), wtd.sd(dta$know_weed_w[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$know_weed_w[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$know_weed_w[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$know_weed_w[dta$recipient == "female" & dta$messenger == "male"], dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_know_w[5,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_weed_w",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("know_weed_w",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_know_w[6,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_weed_w",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("know_weed_w",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_know_w[5,3,h] <- ifelse(totrep >0, RI("know_weed_w",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_weed_w",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("know_weed_w",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_know_w[6,3,h] <- nobs(lm(as.formula(paste("know_weed_w",treatment, sep="~")) ,data=dta))


res_know_w[7,1,h] <- ifelse(h %in% c(6), wtd.mean(dta$know_armyworm_w[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$know_armyworm_w[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$know_armyworm_w[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$know_armyworm_w[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_know_w[8,1,h] <-  ifelse(h %in% c(6), wtd.sd(dta$know_armyworm_w[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$know_armyworm_w[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$know_armyworm_w[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$know_armyworm_w[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_know_w[7,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_armyworm_w",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("know_armyworm_w",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_know_w[8,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_armyworm_w",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("know_armyworm_w",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_know_w[7,3,h] <- ifelse(totrep >0, RI("know_armyworm_w",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_armyworm_w",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("know_armyworm_w",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_know_w[8,3,h] <- nobs(lm(as.formula(paste("know_armyworm_w",treatment, sep="~")) ,data=dta))
end_time <- Sys.time()


indexer <- FW_index(treatment, c("know_space_w", "know_combine_w", "know_weed_w"),dta, nr_repl=0)

res_know_w[9,1,h] <- ifelse(h %in% c(6), wtd.mean(indexer[[3]]$index[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(indexer[[3]]$index[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(indexer[[3]]$index[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(indexer[[3]]$index[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))

res_know_w[10,1,h] <-  ifelse(h %in% c(6), wtd.sd(indexer[[3]]$index[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(indexer[[3]]$index[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(indexer[[3]]$index[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(indexer[[3]]$index[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))


res_know_w[9,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_know_w[10,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_know_w[9,3,h] <-  indexer[[2]]
res_know_w[10,3,h] <-  nobs(indexer[[1]])

plot_res[1,1,h] <- "knowledge"
plot_res[1,2,h] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index, na.rm=T)
plot_res[1,3:4,h] <- confint(indexer[[1]], level=.9)[2,]/sd(indexer[[3]]$index, na.rm=T)
plot_res[1,5,h] <- "woman"


res_know_w[11,1:3,h] <- RI_FWER(deps= c("know_space_w","know_combine_w","know_weed_w") ,indep = treatment , ctrls = ctrls,dta =dta, p_vals = res_know_w[c(1,3,5),3,h], nr_repl = totrep, w_int="weights")

################################################### knowledge - both  #####################################################
dta$know_space_j <- dta$know_space_w * dta$know_space_m


res_know_b[1,1,h] <- ifelse(h %in% c(6), wtd.mean(dta$know_space_j[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$know_space_j[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$know_space_j[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$know_space_j[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_know_b[2,1,h] <-  ifelse(h %in% c(6), wtd.sd(dta$know_space_j[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$know_space_j[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$know_space_j[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$know_space_j[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_know_b[1,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_space_j",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("know_space_j",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_know_b[2,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_space_j",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("know_space_j",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_know_b[1,3,h] <- ifelse(totrep >0, RI("know_space_j",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_space_j",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("know_space_j",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_know_b[2,3,h] <- nobs(lm(as.formula(paste("know_space_j",treatment, sep="~")) ,data=dta))



dta$know_combine_j <- dta$know_combine_w * dta$know_combine_m

res_know_b[3,1,h] <-ifelse(h %in% c(6), wtd.mean(dta$know_combine_j[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$know_combine_j[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$know_combine_j[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$know_combine_j[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_know_b[4,1,h] <-  ifelse(h %in% c(6), wtd.sd(dta$know_combine_j[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$know_combine_j[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$know_combine_j[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$know_combine_j[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_know_b[3,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_combine_j",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("know_combine_j",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_know_b[4,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_combine_j",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("know_combine_j",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_know_b[3,3,h] <- ifelse(totrep >0, RI("know_combine_j",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_combine_j",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("know_combine_j",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_know_b[4,3,h] <- nobs(lm(as.formula(paste("know_combine_j",treatment, sep="~")) ,data=dta))

dta$know_weed_j <- dta$know_weed_w * dta$know_weed_m

res_know_b[5,1,h] <- ifelse(h %in% c(6), wtd.mean(dta$know_weed_j[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$know_weed_j[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$know_weed_j[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$know_weed_j[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_know_b[6,1,h] <-  ifelse(h %in% c(6), wtd.sd(dta$know_weed_j[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$know_weed_j[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$know_weed_j[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$know_weed_j[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_know_b[5,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_weed_j",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("know_weed_j",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_know_b[6,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_weed_j",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("know_weed_j",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_know_b[5,3,h] <- ifelse(totrep >0, RI("know_weed_j",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_weed_j",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("know_weed_j",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_know_b[6,3,h] <- nobs(lm(as.formula(paste("know_weed_j",treatment, sep="~")) ,data=dta))

dta$know_armyworm_j <- dta$know_armyworm_w * dta$know_armyworm_m

res_know_b[7,1,h] <- ifelse(h %in% c(6), wtd.mean(dta$know_armyworm_j[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$know_armyworm_j[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$know_armyworm_j[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$know_armyworm_j[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_know_b[8,1,h] <-  ifelse(h %in% c(6), wtd.sd(dta$know_armyworm_j[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$know_armyworm_j[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$know_armyworm_j[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$know_armyworm_j[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_know_b[7,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_armyworm_j",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("know_armyworm_j",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_know_b[8,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_armyworm_j",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("know_armyworm_j",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_know_b[7,3,h] <- ifelse(totrep >0, RI("know_armyworm_j",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_armyworm_j",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("know_armyworm_j",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_know_b[8,3,h] <- nobs(lm(as.formula(paste("know_armyworm_j",treatment, sep="~")) ,data=dta))

indexer <- FW_index(treatment, c("know_space_j", "know_combine_j", "know_weed_j"),ctrls,w_int="weights",dta, nr_repl=totrep)

res_know_b[9,1,h] <- ifelse(h %in% c(6), wtd.mean(indexer[[3]]$index[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(indexer[[3]]$index[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(indexer[[3]]$index[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(indexer[[3]]$index[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))

res_know_b[10,1,h] <-  ifelse(h %in% c(6), wtd.sd(indexer[[3]]$index[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(indexer[[3]]$index[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(indexer[[3]]$index[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(indexer[[3]]$index[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))


res_know_b[9,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_know_b[10,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_know_b[9,3,h] <-  indexer[[2]]
res_know_b[10,3,h] <-  nobs(indexer[[1]])

plot_res[2,1,h] <- "knowledge"
plot_res[2,2,h] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index, na.rm=T)
plot_res[2,3:4,h] <- confint(indexer[[1]], level=.9)[2,]/sd(indexer[[3]]$index, na.rm=T)
plot_res[2,5,h] <- "joint"



res_know_b[11,1:3,h] <- RI_FWER(deps= c("know_space_j","know_combine_j","know_weed_j") ,indep = treatment , ctrls = ctrls,dta =dta, p_vals = res_know_b[c(1,3,5),3,h], nr_repl = totrep, w_int="weights")

################################################# knowledge men #####################################################


res_know_m[1,1,h] <- ifelse(h %in% c(6), wtd.mean(dta$know_space_m[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$know_space_m[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$know_space_m[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$know_space_m[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_know_m[2,1,h] <-  ifelse(h %in% c(6), wtd.sd(dta$know_space_m[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$know_space_m[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$know_space_m[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$know_space_m[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_know_m[1,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_space_m",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("know_space_m",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_know_m[2,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_space_m",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("know_space_m",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_know_m[1,3,h] <- ifelse(totrep >0, RI("know_space_m",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_space_m",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("know_space_m",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_know_m[2,3,h] <- nobs(lm(as.formula(paste("know_space_m",treatment, sep="~")) ,data=dta))

res_know_m[3,1,h] <-ifelse(h %in% c(6), wtd.mean(dta$know_combine_m[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$know_combine_m[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$know_combine_m[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$know_combine_m[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_know_m[4,1,h] <-  ifelse(h %in% c(6), wtd.sd(dta$know_combine_m[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$know_combine_m[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$know_combine_m[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$know_combine_m[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_know_m[3,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_combine_m",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("know_combine_m",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_know_m[4,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_combine_m",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("know_combine_m",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_know_m[3,3,h] <- ifelse(totrep >0, RI("know_combine_m",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_combine_m",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("know_combine_m",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_know_m[4,3,h] <- nobs(lm(as.formula(paste("know_combine_m",treatment, sep="~")) ,data=dta))

res_know_m[5,1,h] <- ifelse(h %in% c(6), wtd.mean(dta$know_weed_m[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$know_weed_m[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$know_weed_m[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$know_weed_m[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_know_m[6,1,h] <-  ifelse(h %in% c(6), wtd.sd(dta$know_weed_m[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$know_weed_m[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$know_weed_m[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$know_weed_m[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_know_m[5,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_weed_m",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("know_weed_m",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_know_m[6,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_weed_m",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("know_weed_m",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_know_m[5,3,h] <- ifelse(totrep >0, RI("know_weed_m",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_weed_m",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("know_weed_m",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_know_m[6,3,h] <- nobs(lm(as.formula(paste("know_weed_m",treatment, sep="~")) ,data=dta))

res_know_m[7,1,h] <- ifelse(h %in% c(6), wtd.mean(dta$know_armyworm_m[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(dta$know_armyworm_m[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(dta$know_armyworm_m[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$know_armyworm_m[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_know_m[8,1,h] <-  ifelse(h %in% c(6), wtd.sd(dta$know_armyworm_m[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(dta$know_armyworm_m[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(dta$know_armyworm_m[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$know_armyworm_m[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))
res_know_m[7,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_armyworm_m",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("know_armyworm_m",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_know_m[8,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_armyworm_m",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("know_armyworm_m",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_know_m[7,3,h] <- ifelse(totrep >0, RI("know_armyworm_m",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_armyworm_m",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("know_armyworm_m",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_know_m[8,3,h] <- nobs(lm(as.formula(paste("know_armyworm_m",treatment, sep="~")) ,data=dta))

indexer <- FW_index(treatment, c("know_space_m", "know_combine_m", "know_weed_m"),ctrls,w_int="weights",dta, nr_repl=totrep)


res_know_m[9,1,h] <- ifelse(h %in% c(6), wtd.mean(indexer[[3]]$index[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.mean(indexer[[3]]$index[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.mean(indexer[[3]]$index[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.mean(indexer[[3]]$index[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))

res_know_m[10,1,h] <-  ifelse(h %in% c(6), wtd.sd(indexer[[3]]$index[dta$recipient == "male" & dta$messenger == "male"], dta$weights[dta$recipient == "male" & dta$messenger == "male"], na.rm=T),ifelse(h %in% c(1,2,3), wtd.sd(indexer[[3]]$index[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 4, wtd.sd(indexer[[3]]$index[dta$messenger == "male"],dta$weights[dta$messenger == "male"], na.rm=T), ifelse( h == 5, wtd.sd(indexer[[3]]$index[dta$recipient == "female" & dta$messenger == "male" ],dta$weights[dta$recipient == "female" & dta$messenger == "male"], na.rm=T)))))


res_know_m[9,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_know_m[10,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_know_m[9,3,h] <-  indexer[[2]]
res_know_m[10,3,h] <-  nobs(indexer[[1]])

plot_res[3,1,h] <- "knowledge"
plot_res[3,2,h] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index, na.rm=T)
plot_res[3,3:4,h] <- confint(indexer[[1]], level=.9)[2,]/sd(indexer[[3]]$index, na.rm=T)
plot_res[3,5,h] <- "man"


res_know_m[11,1:3,h] <- RI_FWER(deps= c("know_space_m","know_combine_m","know_weed_m") ,indep = treatment , ctrls = ctrls,dta =dta, p_vals = res_know_m[c(1,3,5),3,h], nr_repl = totrep, w_int="weights")

}

save(res_know_m, file = "res_know_m.RData")
save(res_know_b, file = "res_know_b.RData")
save(res_know_w, file = "res_know_w.RData")

print(res_know_w)
print(res_know_b)
print(res_know_m)
print("end")


