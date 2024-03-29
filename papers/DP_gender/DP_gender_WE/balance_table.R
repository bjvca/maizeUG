### look at some balancing variables



#wget https://www.dropbox.com/s/0lzgj61wmv9wmum/DLECv2.dta?dl=0
#rm(list=ls())

library(foreign)
library(ggplot2)
library(doParallel)
library(data.table)
library(dplyr)
library(Hmisc)

dta <- read.dta("DLECv2.dta")
dta <- read.dta("/home/bjvca/data/projects/digital green/baseline/DLECv2.dta")
dta$know_space <- dta$maizeoptimal_spacing == "a"
dta$know_small <- dta$maizeq22 == "c"
dta$know_weed <- dta$maizeq23 == "b"
dta$maizeage[dta$maizeage == 999] <- NA
dta$maizeage_spouse[dta$maizeage_spouse == 999] <- NA

dta$maizeage_man <- dta$maizeage
dta$maizeage_man[dta$recipient == "female"] <- dta$maizeage_spouse[dta$recipient == "female"] 
dta$maizeage_woman <- dta$maizeage_spouse
dta$maizeage_woman[dta$recipient == "female"] <- dta$maizeage[dta$recipient == "female"] 


dta$maizeeduc_man <- dta$maizeeduc
dta$maizeeduc_man[dta$recipient == "female"] <- dta$maizeeduc_spouse[dta$recipient == "female"] 

dta$maizeeduc_woman <- dta$maizeeduc_spouse
dta$maizeeduc_woman[dta$recipient == "female"] <- dta$maizeeduc[dta$recipient == "female"]


siglev <-  1.96
set.seed(54321)
### drop the femheaded
dta <- subset(dta, recipient != "")

dta$yield <- dta$maizebags_harv*100/dta$maizearea_cultivation
dta <- subset(dta, messenger!='ctrl')
###


i_dist <- 1
dta$distID <- NULL
dta$subID <- NULL
dta$vilID <- NULL

dta[dta == 8888] <- NA
dta[dta == "n/a"] <- NA
dta[dta == ""] <- NA

for (dist in names(table(dta$dist))) {
	print(dist)
	i_sub <- 1
	for (sub in names(table(dta$sub[dta$district==dist]))) {
		print(sub)
			i_village <- 1
			for (village in names(table(dta$village[dta$district == dist & dta$sub == sub]))) {
				print(village)
				dta$vilID[dta$district == dist & dta$sub == sub & dta$village == village] <- i_village
				i_village <- i_village + 1
			}
		dta$subID[dta$district == dist & dta$sub == sub] <- i_sub
		i_sub <- i_sub + 1
	}
dta$distID[dta$district==dist] <- i_dist
i_dist <- i_dist + 1
}

dta$distID <- as.numeric(dta$distID)
dta$subID <- as.numeric(dta$subID)
dta$vilID <- as.numeric(dta$vilID)





dta_copy <- dta

balance <- array(NA,c(14,3, 6))
jointF <-  array(NA,c(3, 6))

###################################### some functions #############################

trim <- function(var, dataset, trim_perc=.1) {
### function for triming a dataset *dataset* on a variable *var*
return( subset(dataset,dataset[var] > quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)), na.rm=T)[1] & dataset[var] < quantile(dataset[var], c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[2]) )
}

wtd.sd <- function(x, w,...) {
return(sqrt(wtd.var(x,w)))
}

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
#################################

cl <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
registerDoParallel(cl)

totrep <- 100

####

for (h in 1:6) {
if (h==1) {
############################################ H1: empower: rec==couple or woman - rec==male #########################################################
dta <- dta_copy
set.seed(54321)
treat <- "(recipient != 'male') +ivr+sms+ as.factor(messenger)" 
dta$weights <- 1
dta$weights[dta$recipient == "female"] <-  1053/1135
} else if (h==2) {
############################################ H1a: empower : rec==female - rec==male ###################################################
dta <- subset(dta_copy, recipient == "male" | recipient == "female")
set.seed(54321)
dta$weights <- 1
treat <- "(recipient == 'female') +ivr+sms+as.factor(messenger)" 
} else if (h==3) {
############################################ H1b: rec==couple - rec==male  ###################################################
dta <- subset(dta_copy, recipient == "male" | recipient == "couple")
set.seed(54321)
dta$weights <- 1
treat <- "(recipient == 'couple') +ivr+sms+ as.factor(messenger)"
} else if (h==4) {
############################################ H2: involving women in conveying info
#############################mes==couple or woman - mes==male ###################################################
dta <- dta_copy
set.seed(54321)
ctrls<- NULL
dta$weights <- 1
dta$weights[dta$messenger == "female"] <-  1106/1108
treat <- "(messenger != 'male') +ivr+sms+as.factor(recipient)"
} else if (h==5) {
######################## H3:role model effect  ################################

dta <- dta_copy
dta$recipient <- factor(dta$recipient)
dta$messenger <- factor(dta$messenger)
dta <- subset(dta, recipient =='female'  )
set.seed(54321)

treat <- "(messenger!= 'male') +ivr+sms"


dta$weights <- 1
dta$weights[dta$messenger == "female" & dta$recipient == "female"] <-  368/384

} else if (h==6) {
######################## H4: challenging role incongruity###########################
#############  messenger== 'female or couple' & recipient=='male' - messenger== 'male' & recipient=='male' ##############################
dta <- dta_copy
dta$recipient <- factor(dta$recipient)
dta$messenger <- factor(dta$messenger)
dta <- subset(dta, recipient =='male'  )
set.seed(54321)

treat <- "(messenger!= 'male') +ivr+sms"


dta$weights <- 1
dta$weights[dta$messenger == "female" & dta$recipient == "male"] <-  369/382


}

print(h)


dta2 <- trim("maizearea_cultivation",dta,.05)
dta2$log_maizearea_cultivation <- log(dta2$maizearea_cultivation)
### yield - trim and log
balance[1,1,h] <-   wtd.mean(dta2$log_maizearea_cultivation, dta2$weights, na.rm=T)
balance[2,1,h] <-   wtd.sd(dta2$log_maizearea_cultivation, dta2$weights, na.rm=T)
balance[1,2,h] <-  summary(lm(as.formula(paste("log_maizearea_cultivation",treat, sep="~")) ,weights=weights,data=dta2))$coefficients[2,1]
balance[2,2,h] <-  summary(lm(as.formula(paste("log_maizearea_cultivation",treat, sep="~")) ,weights=weights,data=dta2))$coefficients[2,2]
balance[1,3,h] <-   ifelse(totrep >0, RI("log_maizearea_cultivation",treat ,ctrls = NULL, w_int="weights", dta= dta2, nr_repl = totrep),summary(lm(as.formula(paste("log_maizearea_cultivation",treat, sep="~")) ,weights=weights,data=dta2))$coefficients[2,4])
balance[2,3,h] <- nobs(lm(as.formula(paste("log_maizearea_cultivation",treat, sep="~")) ,data=dta2))

balance[3,1,h] <-   wtd.mean(dta$maizeage_man, dta$weights, na.rm=T)
balance[4,1,h] <-   wtd.sd(dta$maizeage_man, dta$weights, na.rm=T)
balance[3,2,h] <-  summary(lm(as.formula(paste("maizeage_man",treat, sep="~")) ,weights=weights,data=dta))$coefficients[2,1]
balance[4,2,h] <-  summary(lm(as.formula(paste("maizeage_man",treat, sep="~")) ,weights=weights,data=dta))$coefficients[2,2]
balance[3,3,h] <-   ifelse(totrep >0, RI("maizeage_man",treat ,ctrls = NULL, w_int="weights", dta= dta, nr_repl = totrep),summary(lm(as.formula(paste("maizeage_man",treat, sep="~")) ,weights=weights,data=dta))$coefficients[2,4])
balance[4,3,h] <- nobs(lm(as.formula(paste("maizeage_man",treat, sep="~")) ,data=dta))


dta$maizeeduc_man <-  dta$maizeeduc_man > 2
balance[5,1,h] <-   wtd.mean(dta$maizeeduc_man, dta$weights, na.rm=T)
balance[6,1,h] <-   wtd.sd(dta$maizeeduc_man, dta$weights, na.rm=T)
balance[5,2,h] <-  summary(lm(as.formula(paste("maizeeduc_man",treat, sep="~")) ,weights=weights,data=dta))$coefficients[2,1]
balance[6,2,h] <-  summary(lm(as.formula(paste("maizeeduc_man",treat, sep="~")) ,weights=weights,data=dta))$coefficients[2,2]
balance[5,3,h] <-   ifelse(totrep >0, RI("maizeeduc_man",treat ,ctrls = NULL, w_int="weights", dta= dta, nr_repl = totrep),summary(lm(as.formula(paste("maizeeduc_man",treat, sep="~")) ,weights=weights,data=dta))$coefficients[2,4])
balance[6,3,h] <- nobs(lm(as.formula(paste("maizeeduc_man",treat, sep="~")) ,data=dta))

### hh size

balance[7,1,h] <-   wtd.mean(dta$maizehh_no, dta$weights, na.rm=T)
balance[8,1,h] <-   wtd.sd(dta$maizehh_no, dta$weights, na.rm=T)
balance[7,2,h] <-  summary(lm(as.formula(paste("maizehh_no",treat, sep="~")) ,weights=weights,data=dta))$coefficients[2,1]
balance[8,2,h] <-  summary(lm(as.formula(paste("maizehh_no",treat, sep="~")) ,weights=weights,data=dta))$coefficients[2,2]
balance[7,3,h] <-   ifelse(totrep >0, RI("maizehh_no",treat ,ctrls = NULL, w_int="weights", dta= dta, nr_repl = totrep),summary(lm(as.formula(paste("maizehh_no",treat, sep="~")) ,weights=weights,data=dta))$coefficients[2,4])
balance[8,3,h] <- nobs(lm(as.formula(paste("maizehh_no",treat, sep="~")) ,data=dta))

#maizedist_shop trimmed and logged

dta2 <- trim("maizedist_shop",dta,.05)
dta2$log_maizedist_shop <- log(dta2$maizedist_shop)

balance[9,1,h] <-   wtd.mean(dta$maizedist_shop, dta$weights, na.rm=T)
balance[10,1,h] <-   wtd.sd(dta$maizedist_shop, dta$weights, na.rm=T)
balance[9,2,h] <-  summary(lm(as.formula(paste("maizedist_shop",treat, sep="~")) ,weights=weights,data=dta))$coefficients[2,1]
balance[10,2,h] <-  summary(lm(as.formula(paste("maizedist_shop",treat, sep="~")) ,weights=weights,data=dta))$coefficients[2,2]
balance[9,3,h] <-   ifelse(totrep >0, RI("maizedist_shop",treat ,ctrls = NULL, w_int="weights", dta= dta, nr_repl = totrep),summary(lm(as.formula(paste("maizedist_shop",treat, sep="~")) ,weights=weights,data=dta))$coefficients[2,4])
balance[10,3,h] <- nobs(lm(as.formula(paste("maizedist_shop",treat, sep="~")) ,data=dta))

###wall material is mud of clay (not burned bricks)
dta$maizeprwall <- dta$maizeprwall != 1
balance[11,1,h] <-   wtd.mean(dta$maizeprwall, dta$weights, na.rm=T)
balance[12,1,h] <-   wtd.sd(dta$maizeprwall, dta$weights, na.rm=T)
balance[11,2,h] <-  summary(lm(as.formula(paste("maizeprwall",treat, sep="~")) ,weights=weights,data=dta))$coefficients[2,1]
balance[12,2,h] <-  summary(lm(as.formula(paste("maizeprwall",treat, sep="~")) ,weights=weights,data=dta))$coefficients[2,2]
balance[11,3,h] <-   ifelse(totrep >0, RI("maizeprwall",treat ,ctrls = NULL, w_int="weights", dta= dta, nr_repl = totrep),summary(lm(as.formula(paste("maizeprwall",treat, sep="~")) ,weights=weights,data=dta))$coefficients[2,4])
balance[12,3,h] <- nobs(lm(as.formula(paste("maizeprwall",treat, sep="~")) ,data=dta))

#number of rooms

balance[13,1,h] <-   wtd.mean(dta$maizeprrooms, dta$weights, na.rm=T)
balance[14,1,h] <-   wtd.sd(dta$maizeprrooms, dta$weights, na.rm=T)
balance[13,2,h] <-  summary(lm(as.formula(paste("maizeprrooms",treat, sep="~")) ,weights=weights,data=dta))$coefficients[2,1]
balance[14,2,h] <-  summary(lm(as.formula(paste("maizeprrooms",treat, sep="~")) ,weights=weights,data=dta))$coefficients[2,2]
balance[13,3,h] <-   ifelse(totrep >0, RI("maizeprrooms",treat ,ctrls = NULL, w_int="weights", dta= dta, nr_repl = totrep),summary(lm(as.formula(paste("maizeprrooms",treat, sep="~")) ,weights=weights,data=dta))$coefficients[2,4])
balance[14,3,h] <- nobs(lm(as.formula(paste("maizeprrooms",treat, sep="~")) ,data=dta))

}

print(balance)
#H1
dta <- dta_copy
## joint tests and Nobs
covars <- c("log_maizearea_cultivation + maizeage_man + (maizeeduc_man > 2) + maizehh_no+ maizedist_shop + (maizeprwall != 1) + maizeprrooms")


dta$weights <- 1
dta$weights[dta$recipient == "female"] <-  1053/1135

dta2 <- trim("maizearea_cultivation",dta,.05)
dta2$log_maizearea_cultivation <- log(dta2$maizearea_cultivation)



x <- summary(lm(as.formula(paste("(recipient != 'male')",covars, sep="~")), data= dta2))
jointF[1,1] <- x$fstatistic[1]
jointF[2,1] <- pf(x$fstatistic[1],x$fstatistic[2],x$fstatistic[3],lower.tail=FALSE)
jointF[3,1] <- x$df[1] + x$df[2]

############################################ H1: empower: rec==couple or woman - rec==male #########################################################

## joint tests and Nobs
covars <- c("log_maizearea_cultivation + maizeage_man + (maizeeduc_man > 2) + maizehh_no+ maizedist_shop + (maizeprwall != 1) + maizeprrooms")
##
dta <- subset(dta_copy, recipient == "male" | recipient == "female")
dta$weights <- 1

dta2 <- trim("maizearea_cultivation",dta,.05)
dta2$log_maizearea_cultivation <- log(dta2$maizearea_cultivation)


x <- summary(lm(as.formula(paste("(recipient == 'female')",covars, sep="~")), data= dta2))
jointF[1,2] <- x$fstatistic[1]
jointF[2,2] <- pf(x$fstatistic[1],x$fstatistic[2],x$fstatistic[3],lower.tail=FALSE)
jointF[3,2] <- x$df[1] + x$df[2]

#H1b:
dta <- subset(dta_copy, recipient == "male" | recipient == "couple")
dta$weights <- 1
treat <- "(recipient == 'couple') +ivr+sms+ as.factor(messenger)"


dta2 <- trim("maizearea_cultivation",dta,.05)
dta2$log_maizearea_cultivation <- log(dta2$maizearea_cultivation)


x <- summary(lm(as.formula(paste("(recipient == 'couple')",covars, sep="~")), data= dta2))
jointF[1,3] <- x$fstatistic[1]
jointF[2,3] <- pf(x$fstatistic[1],x$fstatistic[2],x$fstatistic[3],lower.tail=FALSE)
jointF[3,3] <- x$df[1] + x$df[2]

############################################ H3: mes==couple or woman - mes==male ###################################################
dta <- dta_copy
dta$weights <- 1
dta$weights[dta$messenger == "female"] <-  1106/1108

dta2 <- trim("maizearea_cultivation",dta,.05)
dta2$log_maizearea_cultivation <- log(dta2$maizearea_cultivation)

x <- summary(lm(as.formula(paste("(messenger != 'male')",covars, sep="~")), data= dta2))
jointF[1,4] <- x$fstatistic[1]
jointF[2,4] <- pf(x$fstatistic[1],x$fstatistic[2],x$fstatistic[3],lower.tail=FALSE)
jointF[3,4] <- x$df[1] + x$df[2]


######################## H5: messenger== 'male' & recipient=='female' - messenger!= 'male' & recipient!=='female' ##############################
dta <- dta_copy

dta$recipient <- factor(dta$recipient)
dta$messenger <- factor(dta$messenger)
dta <- subset(dta, recipient =='female'  )
set.seed(54321)

treat <- "(messenger!= 'male') +ivr+sms"


dta$weights <- 1
dta$weights[dta$messenger == "female" & dta$recipient == "female"] <-  368/384


dta2 <- trim("maizearea_cultivation",dta,.05)
dta2$log_maizearea_cultivation <- log(dta2$maizearea_cultivation)

x <- summary(lm(as.formula(paste("(messenger!= 'male')",covars, sep="~")), data= dta2))
jointF[1,5] <- x$fstatistic[1]
jointF[2,5] <- pf(x$fstatistic[1],x$fstatistic[2],x$fstatistic[3],lower.tail=FALSE)
jointF[3,5] <- x$df[1] + x$df[2]


######################## H5: messenger== 'male' & recipient=='male' - messenger!= 'male' & recipient=='male' ##############################
dta <- dta_copy

dta$recipient <- factor(dta$recipient)
dta$messenger <- factor(dta$messenger)
dta <- subset(dta, recipient =='male'  )
set.seed(54321)

treat <- "(messenger!= 'male') +ivr+sms"


dta$weights <- 1
dta$weights[dta$messenger == "female" & dta$recipient == "male"] <-  339/348


dta2 <- trim("maizearea_cultivation",dta,.05)
dta2$log_maizearea_cultivation <- log(dta2$maizearea_cultivation)


x <- summary(lm(as.formula(paste("(messenger!= 'male')",covars, sep="~")), data= dta2))
jointF[1,6] <- x$fstatistic[1]
jointF[2,6] <- pf(x$fstatistic[1],x$fstatistic[2],x$fstatistic[3],lower.tail=FALSE)
jointF[3,6] <- x$df[1] + x$df[2]



