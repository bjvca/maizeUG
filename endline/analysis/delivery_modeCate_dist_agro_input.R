rm(list=ls())
library(foreign)
### running the data preparation file
source("/home/bjvca/data/projects/digital green/endline/data/init.R")

#### The analysis was done on the Amazon's cloud computing platform, on which data was directly imported from my dropbox account

#wget https://www.dropbox.com/s/h783b3iml5fiyoo/baseline.csv?dl=0
#wget https://www.dropbox.com/s/gr5lenciwlrtujm/AWS_delivery_mode.csv?dl=0
#install.packages(c("ggplot2","doParallel","data.table","dplyr"))

dta <- read.csv("AWS_delivery_mode.csv")
baseline <- read.csv("baseline_delivery_mode.csv")[c("hhid","maizeeduc","maizehh_no","maizeprrooms","maizedist_shop","maizemobile_access","maizeage","maizeprinfo_receiv","maizeprwall","maizeprearn","maizeprinfo_receiv_spouse","maizearea_cultivation","maizebags_harv")]

#source("/home/bjvca/data/projects/digital green/endline/data/init.R")
#baseline <- read.csv("/home/bjvca/data/projects/digital green/endline/data/raw/baseline.csv")[c("hhid","maizeeduc","maizehh_no","maizeprrooms","maizedist_shop","maizemobile_access","maizeage","maizeprinfo_receiv","maizeprwall","maizeprearn","maizeprinfo_receiv_spouse","maizearea_cultivation","maizebags_harv")]
### this has someduplicate



#set totrep to zero if you do not want simulation based inferecne
library(ggplot2)
library(doParallel)
library(data.table)
library(dplyr)

set.seed(07032018)
cl <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
registerDoParallel(cl)
dta$messenger <- as.character(dta$messenger)
dta$sms <- dta$sms.x
dta$sms.x <- NULL
dta$sms.y <- NULL
dta$ivr <- dta$ivr.x
dta$ivr.x <- NULL
dta$ivr.y <- NULL

### indexing results arrays
res_itt_know <- array(NA, c(11,4,3)) 
rownames(res_itt_know) <- c("know_space","","know_combine","","know_weed","", "know_armyworm","","know_ind","","p-vals")
res_itt_pract <- array(NA, c(23,4,3))
rownames(res_itt_pract) <- c("first_day","","space","","striga","","weed","", "use_fert","","seed","","combiner","","bought_seed","","chem","","labour","","pract_index","","p-vals")
res_itt_fert <- array(NA, c(9,4,3))
rownames(res_itt_fert) <- c("use_DAP","","use_urea","","use_organic","","fert_index","","p-vals")
res_itt_seed <- array(NA, c(7,4,3))
rownames(res_itt_seed) <- c("hybrid","","opv","","seed_inded","","p-vals")
res_itt_prod <- array(NA, c(15,4,3))
rownames(res_itt_prod) <- c("prod","","area","","yield","","yield_better","","labour","","labour_prod","","prod_index","","p-vals")
prod_plot <- data.frame(matrix(NA, 4,4))
names(prod_plot) <- c("x","y","ylo","yhi")
fert_plot <- data.frame(matrix(NA, 4,4))
names(fert_plot) <- c("x","y","ylo","yhi")
seed_plot <- data.frame(matrix(NA, 4,4))
names(seed_plot) <- c("x","y","ylo","yhi")

res_itt_wel <-  array(NA, c(13,4,3))
rownames(res_itt_wel) <- c("better_av","","better_6m","","eatpref","","eatenough","","log_cons","","welfare_index","","p-vals")



RI <- function(dep, indep, ctrls = NULL,  dta , nr_repl = 1000, w_int = NULL) {
# RI("(maizeeduc > 2)",treat ,ctrls = NULL, w_int="weights", dta= dta, nr_repl = totrep)
#RI("index" ,treat , contr_vars, w_int= w_int2,dta= data, nr_repl = 1000,h_int=1)
#indep <- "(recipient != 'male') +ivr+sms+ as.factor(messenger)" 
#ctrls <- NULL
#h_int <- 1
#dep <- "weed"
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
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["ctrl",]>0]), "ctrl","female")))
		dta_sim$ivr <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$ivr,dta$treat))[table(dta$ivr, dta$treat)["yes",]>0]), "yes","no")
		dta_sim$sms <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$sms,dta$treat))[table(dta$sms, dta$treat)["yes",]>0]), "yes","no")
		return(abs(ifelse(is.null(ctrls),summary(lm(as.formula(paste(dep,indep,sep="~")), weights=unlist(dta_sim[,w_int, with=FALSE]), data=dta_sim))$coefficients[2,1],summary(lm(as.formula(paste(paste(dep,indep,sep="~"), ctrls,sep="+")), weights=unlist(dta_sim[,w_int, with=FALSE]), data=dta_sim))$coefficients[2,1])) > abs(crit) )
	}
	return(sum(oper)/nr_repl)
}


RI_FWER <- function(deps, indep,ctrls = NULL, dta ,p_vals , nr_repl = 1000,  w_int = NULL) {
### function to control for FWER using simulation (familywise sharp null)
### inspired on https://egap.org/methods-guides/10-things-you-need-know-about-multiple-comparisons

#RI_FWER(deps= c("know_space","know_combine","know_weed") ,indep = treatment, ctrls=ctrls ,dta =dta_bal, p_vals = res_itt_know[c(1,3,5),3,h], nr_repl = totrep, w_int="weight")

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
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["ctrl",]>0]), "ctrl","female")))
		dta_sim$ivr <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$ivr,dta$treat))[table(dta$ivr, dta$treat)["yes",]>0]), "yes","no")
		dta_sim$sms <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$sms,dta$treat))[table(dta$sms, dta$treat)["yes",]>0]), "yes","no")

if (is.null(ctrls)) {
return(unlist(lapply(deps, function(dvar)  summary(lm(as.formula(paste(dvar,indep,sep="~")), weights=unlist(dta_sim[,w_int, with=FALSE]),data=dta_sim))$coefficients[2,4])))
} else {
return(unlist(lapply(deps, function(dvar)  summary(lm(as.formula(paste(paste(dvar,indep,sep="~"),ctrls, sep="+")), weights=unlist(dta_sim[,w_int, with=FALSE]),data=dta_sim))$coefficients[2,4])))
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

dta$weight <- 1
dta$weight[dta$messenger == "female"] <- 1/(1191*1/1018)
dta$weight[dta$messenger == "male"] <- 1/(1182*1/1018)


baseline <- subset(baseline, !(hhid %in% baseline$hhid[duplicated(baseline$hhid)]))
dta <- merge(baseline, dta, by="hhid")

### only for the 66 percent closest to agro-input dealer
dta <- subset(dta, maizedist_shop<=5)

###number of RI replications: if set to 0, conventional p-values are reported, in the analysis, 10000 replications were used
totrep <- 10000
### better to loop over h: 
for (h in 1:3) {
if (h == 1) {
dta_bal <- dta
treatment <- "(messenger != 'ctrl')+ivr+sms+as.factor(recipient)"
ctrls <- "femhead" 
} else if ( h==2 ) {
dta_bal <- dta
###uncomment here to look at heterogeneity wrt to mobile access
#dta_bal <- subset(dta,maizemobile_access == TRUE)
treatment <- "ivr+(messenger != 'ctrl')+sms+as.factor(recipient)"
ctrls <- "femhead"  
} else if (h==3) {
dta_bal <- dta
###uncomment here to look at heterogeneity wrt to mobile ownership
#dta_bal <- subset(dta,maizemobile == TRUE)
treatment <- "sms+(messenger != 'ctrl')+ivr+as.factor(recipient)" 
ctrls <- "femhead" 
}
################################ knowledge  ############################

#res_itt_know[1,1,h] <- mean(dta_bal$know_space[dta_bal$messenger == "ctrl"],na.rm=T)
#res_itt_know[2,1,h] <- sd(dta_bal$know_space[dta_bal$messenger == "ctrl"],na.rm=T)
#res_itt_know[1,2,h] <- summary(lm(as.formula(paste(paste("know_space",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,1]
#res_itt_know[2,2,h] <- summary(lm(as.formula(paste(paste("know_space",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,2]
#res_itt_know[1,3,h] <- ifelse(totrep >0, RI("know_space", treatment ,ctrls, dta_bal,  totrep, "weight"),summary(lm(as.formula(paste(paste("know_space",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,4])
#res_itt_know[1,4,h] <- nobs(lm(as.formula(paste(paste("know_space",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))

#res_itt_know[3,1,h] <-  mean(dta_bal$know_combine[dta_bal$messenger == "ctrl"],na.rm=T)
#res_itt_know[4,1,h] <-  sd(dta_bal$know_combine[dta_bal$messenger == "ctrl"],na.rm=T)
#res_itt_know[3,2,h] <- summary(lm(as.formula(paste(paste("know_combine",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,1]
#res_itt_know[4,2,h] <- summary(lm(as.formula(paste(paste("know_combine",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,2]
#res_itt_know[3,3,h] <-  ifelse(totrep >0, RI("know_combine",treatment ,ctrls, dta_bal, nr_repl = totrep,  "weight"),summary(lm(as.formula(paste(paste("know_combine",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,4])
#res_itt_know[3,4,h] <- nobs(lm(as.formula(paste(paste("know_combine",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))

#res_itt_know[5,1,h] <-  mean(dta_bal$know_weed[dta_bal$messenger == "ctrl"],na.rm=T)
#res_itt_know[6,1,h] <-  sd(dta_bal$know_weed[dta_bal$messenger == "ctrl"],na.rm=T)
#res_itt_know[5,2,h] <- summary(lm(as.formula(paste(paste("know_weed",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,1]
#res_itt_know[6,2,h] <- summary(lm(as.formula(paste(paste("know_weed",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,2]
#res_itt_know[5,3,h] <-  ifelse(totrep >0, RI("know_weed",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"),summary(lm(as.formula(paste(paste("know_weed",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,4])
#res_itt_know[5,4,h] <- nobs(lm(as.formula(paste(paste("know_weed",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))

#res_itt_know[7,1,h] <- mean(dta_bal$know_armyworm[dta_bal$messenger == "ctrl"],na.rm=T)
#res_itt_know[8,1,h] <- sd(dta_bal$know_armyworm[dta_bal$messenger == "ctrl"],na.rm=T)
#res_itt_know[7,2,h] <- summary(lm(as.formula(paste(paste("know_armyworm",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,1]
#res_itt_know[8,2,h] <- summary(lm(as.formula(paste(paste("know_armyworm",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,2]
#res_itt_know[7,3,h] <-  ifelse(totrep >0, RI("know_armyworm",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"),summary(lm(as.formula(paste(paste("know_armyworm",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,4])
#res_itt_know[7,4,h] <- nobs(lm(as.formula(paste(paste("know_armyworm",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))

### no need to include armyworm in the index because we do not really expect an effect
#indexer <- FW_index(treatment, c("know_space", "know_combine", "know_weed"),ctrls,w_int2 = "weight", data =dta_bal, nr_repl=totrep )
#res_itt_know[9,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"],na.rm=T)
#res_itt_know[10,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"],na.rm=T)
#res_itt_know[9,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
#res_itt_know[10,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
#res_itt_know[9,3,h] <-  indexer[[2]]
#res_itt_know[9,4,h]  <- nobs(indexer[[1]])

#prod_plot[1,1] <- "knowledge"
#prod_plot[1,3:4] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
#prod_plot[1,2] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
###calculated corrected critical values
#print(h)

#res_itt_know[11,1:3 ,h] <- RI_FWER(deps= c("know_space","know_combine","know_weed") ,indep = treatment, ctrls=ctrls ,dta =dta_bal, p_vals = res_itt_know[c(1,3,5),3,h], nr_repl = totrep, w_int="weight")

#################################### practices #############################

####timely planting
#res_itt_pract[1,1,h]  <- mean(dta_bal$day_one[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_pract[2,1,h]  <- sd(dta_bal$day_one[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_pract[1,2,h]  <- summary(lm(as.formula(paste(paste("day_one", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
#res_itt_pract[2,2,h]  <- summary(lm(as.formula(paste(paste("day_one", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
#res_itt_pract[1,3,h]  <- ifelse(totrep >0, RI("day_one",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("day_one", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4]) 
#res_itt_pract[1,4,h]  <- nobs(lm(as.formula(paste(paste("day_one", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

#### used recommended spacing use on at lease one plot as reported by at least one spouse
#res_itt_pract[3,1,h]  <- mean(dta_bal$space[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_pract[4,1,h]  <- sd(dta_bal$space[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_pract[3,2,h]  <- summary(lm(as.formula(paste(paste("space", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
#res_itt_pract[4,2,h]  <- summary(lm(as.formula(paste(paste("space", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
#res_itt_pract[3,3,h]  <- ifelse(totrep >0, RI("space",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"),  summary(lm(as.formula(paste(paste("space", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4])
#res_itt_pract[3,4,h]  <- nobs(lm(as.formula(paste(paste("space", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

## used recommended way to fight striga - this should be changed to include info of all plots 
#res_itt_pract[5,1,h]  <- mean(dta_bal$striga[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_pract[6,1,h]  <- sd(dta_bal$striga[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_pract[5,2,h]  <- summary(lm(as.formula(paste(paste("striga", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
#res_itt_pract[6,2,h]  <- summary(lm(as.formula(paste(paste("striga", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
#res_itt_pract[5,3,h]  <- ifelse(totrep >0, RI("striga",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("striga", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4])
#res_itt_pract[5,4,h]  <- nobs(lm(as.formula(paste(paste("striga", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

## weeded on recommended timing? - this should be changed to include info of all plots 
#res_itt_pract[7,1,h]  <- mean(dta_bal$weed[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_pract[8,1,h]  <- sd(dta_bal$weed[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_pract[7,2,h]  <- summary(lm(as.formula(paste(paste("weed", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
#res_itt_pract[8,2,h]  <- summary(lm(as.formula(paste(paste("weed", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
#res_itt_pract[7,3,h]  <- ifelse(totrep >0, RI("weed",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"),summary(lm(as.formula(paste(paste("weed", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4])
#res_itt_pract[7,4,h]  <- nobs(lm(as.formula(paste(paste("weed", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

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
res_itt_fert[1,1,h]  <-  mean(dta_bal$fert_dap[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_fert[2,1,h]  <-  sd(dta_bal$fert_dap[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_fert[1,2,h]  <- summary(lm(as.formula(paste(paste("fert_dap", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_fert[2,2,h]  <- summary(lm(as.formula(paste(paste("fert_dap", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_fert[1,3,h]  <- ifelse(totrep >0, RI("fert_dap",treatment , ctrls, dta_bal, nr_repl = totrep, "weight") , summary(lm(as.formula(paste(paste("fert_dap", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4])
res_itt_fert[1,4,h]  <- nobs(lm(as.formula(paste(paste("fert_dap", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

##fert_plot[2,1] <- "DAP/NPK"
##fert_plot[2,3:4] <- confint(lm(as.formula(paste(paste("fert_dap", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight), level=.9)[2,] /  mean(dta_bal$fert_dap[dta_bal$messenger == "ctrl"], na.rm=T)
##fert_plot[2,2] <-  summary(lm(as.formula(paste(paste("fert_dap", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1] /  mean(dta_bal$fert_dap[dta_bal$messenger == "ctrl"], na.rm=T)

#### fert = urea
res_itt_fert[3,1,h]  <-  mean(dta_bal$fert_urea[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_fert[4,1,h]  <-  sd(dta_bal$fert_urea[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_fert[3,2,h]  <- summary(lm(as.formula(paste(paste("fert_urea", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_fert[4,2,h]  <- summary(lm(as.formula(paste(paste("fert_urea", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_fert[3,3,h]  <- ifelse(totrep >0, RI("fert_urea",treatment , ctrls, dta_bal, nr_repl = totrep, "weight") , summary(lm(as.formula(paste(paste("fert_urea", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4])
res_itt_fert[3,4,h]  <- nobs(lm(as.formula(paste(paste("fert_urea", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

##fert_plot[3,1] <- "urea"
##fert_plot[3,3:4] <- confint(lm(as.formula(paste(paste("fert_urea", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight), level=.9)[2,] /  mean(dta_bal$fert_urea[dta_bal$messenger == "ctrl"], na.rm=T)
##fert_plot[3,2] <-  summary(lm(as.formula(paste(paste("fert_urea", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1] /  mean(dta_bal$fert_urea[dta_bal$messenger == "ctrl"], na.rm=T)

#### fert = organic
res_itt_fert[5,1,h]  <-  mean(dta_bal$fert_org[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_fert[6,1,h]  <-  sd(dta_bal$fert_org[dta_bal$messenger == "ctrl"], na.rm=T)
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
res_itt_fert[7,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
res_itt_fert[8,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
res_itt_fert[7,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_fert[8,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_fert[7,3,h] <-  indexer[[2]]
res_itt_fert[7,4,h] <-  nobs(indexer[[1]])


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
res_itt_seed[1,1,h]  <-  mean(dta_bal$hybrid[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_seed[2,1,h]  <-  sd(dta_bal$hybrid[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_seed[1,2,h] <- summary(lm(as.formula(paste(paste("hybrid", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_seed[2,2,h] <- summary(lm(as.formula(paste(paste("hybrid", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_seed[1,3,h] <- ifelse(totrep >0, RI("hybrid",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("hybrid", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4]) 
res_itt_seed[1,4,h] <- nobs(lm(as.formula(paste(paste("hybrid", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

##seed_plot[2,1] <- "hybrid"
##seed_plot[2,3:4] <- confint(lm(as.formula(paste(paste("hybrid", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight), level=.9)[2,] /  mean(dta_bal$hybrid[dta_bal$messenger == "ctrl"], na.rm=T)
##seed_plot[2,2] <-  summary(lm(as.formula(paste(paste("hybrid", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1] /  mean(dta_bal$hybrid[dta_bal$messenger == "ctrl"], na.rm=T)

### opv
res_itt_seed[3,1,h]  <-  mean(dta_bal$opv[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_seed[4,1,h]  <-  sd(dta_bal$opv[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_seed[3,2,h] <- summary(lm(as.formula(paste(paste("opv", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_seed[4,2,h] <- summary(lm(as.formula(paste(paste("opv", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_seed[3,3,h] <- ifelse(totrep >0, RI("opv",treatment , ctrls, dta_bal, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("opv", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4]) 
res_itt_seed[3,4,h] <- nobs(lm(as.formula(paste(paste("opv", treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

indexer <-  FW_index(treatment,c("hybrid","opv"),ctrls,w_int2 = "weight", data =dta_bal, nr_repl=totrep )
res_itt_seed[5,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
res_itt_seed[6,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
res_itt_seed[5,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_seed[6,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_seed[5,3,h] <-  indexer[[2]]
res_itt_seed[6,4,h] <-  nobs(indexer[[1]])


#indexer <-  FW_index(treatment,c("day_one","space","striga","weed"),ctrls,w_int2 = "weight", data =dta_bal, nr_repl=totrep )
#res_itt_pract[21,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
#res_itt_pract[22,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
#res_itt_pract[21,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
#res_itt_pract[22,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
#res_itt_pract[21,3,h] <-  indexer[[2]]
#res_itt_pract[21,4,h] <-  nobs(indexer[[1]])

###prod_plot[2,1] <- "adoption"
###prod_plot[2,3:4] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
###prod_plot[2,2] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])


###res_itt_pract <- Bcorr( c("day_one","space","striga","weed", "fert","impseed", "combiner","bought_seed","chem","labour"),dta_bal, res_itt_pract ,h)
###res_itt_fert <- Bcorr(c("fert_dap","fert_urea","fert_org"),dta_bal, res_itt_fert ,h)
###res_itt_seed <- Bcorr(c("hybrid","opv"),dta_bal, res_itt_seed ,h)

#res_itt_pract[23,1:3 ,h] <- RI_FWER(c("day_one","space","striga","weed"),indep = treatment, ctrls=ctrls ,dta =dta_bal, p_vals = res_itt_pract[c(1,3,5,7),3,h], nr_repl = totrep, w_int="weight")

res_itt_fert[9,1:3 ,h] <- RI_FWER(c("fert_dap","fert_urea","fert_org"),indep = treatment, ctrls=ctrls ,dta =dta_bal, p_vals = res_itt_fert[c(1,3,5),3,h], nr_repl = totrep, w_int="weight")

res_itt_seed[7,1:3 ,h] <-  RI_FWER(c("hybrid","opv"),indep = treatment, ctrls=ctrls ,dta =dta_bal, p_vals = res_itt_seed[c(1,3),3,h], nr_repl = totrep, w_int="weight")

################################# production ###########################
####### does the video increases production related outcomes?

##trimming is done on end result
#dta_bal2 <- subset(dta_bal, prod_tot>0)
#dta_bal2$log_prod_tot <- log(dta_bal2$prod_tot)
#dta_trim <- trim("log_prod_tot", dta_bal2, .05)

#### production
#res_itt_prod[1,1,h] <- mean(dta_trim$log_prod_tot[dta_trim$messenger == "ctrl"], na.rm=T)
#res_itt_prod[2,1,h] <- sd(dta_trim$log_prod_tot[dta_trim$messenger == "ctrl"], na.rm=T)
#res_itt_prod[1,2,h] <- summary(lm(as.formula(paste(paste("log_prod_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,1]
#res_itt_prod[2,2,h] <- summary(lm(as.formula(paste(paste("log_prod_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,2]
#res_itt_prod[1,3,h] <- ifelse(totrep >0, RI("log_prod_tot",treatment , ctrls, dta_trim, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("log_prod_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,4])
#res_itt_prod[1,4,h] <- nobs(lm(as.formula(paste(paste("log_prod_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))

#### area
#dta_bal2 <- subset(dta_bal, area_tot>0)
#dta_bal2$log_area_tot <- log(dta_bal2$area_tot)
#dta_trim <- trim("log_area_tot", dta_bal2, .05)

#res_itt_prod[3,1,h] <- mean(dta_trim$log_area_tot[dta_trim$messenger == "ctrl"], na.rm=T)
#res_itt_prod[4,1,h] <- sd(dta_trim$log_area_tot[dta_trim$messenger == "ctrl"], na.rm=T)
#res_itt_prod[3,2,h] <- summary(lm(as.formula(paste(paste("log_area_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,1]
#res_itt_prod[4,2,h] <- summary(lm(as.formula(paste(paste("log_area_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,2]
#res_itt_prod[3,3,h] <- ifelse(totrep >0, RI("log_area_tot",treatment , ctrls, dta_trim, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("log_area_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,4])
#res_itt_prod[3,4,h] <- nobs(lm(as.formula(paste(paste("log_area_tot",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))

####yield

#dta_bal2 <- subset(dta_bal, yield_av >0)
#dta_bal2$log_yield_av <- log(dta_bal2$yield_av)
#dta_trim <- trim("log_yield_av", dta_bal2, .05)

#res_itt_prod[5,1,h] <- mean(dta_trim$log_yield_av[dta_trim$messenger == "ctrl"], na.rm=T)
#res_itt_prod[6,1,h] <- sd(dta_trim$log_yield_av[dta_trim$messenger == "ctrl"], na.rm=T)
#res_itt_prod[5,2,h] <- summary(lm(as.formula(paste(paste("log_yield_av",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,1]
#res_itt_prod[6,2,h] <- summary(lm(as.formula(paste(paste("log_yield_av",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,2]
#res_itt_prod[5,3,h] <- ifelse(totrep >0, RI("log_yield_av",treatment , ctrls, dta_trim, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("log_yield_av",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,4])
#res_itt_prod[5,4,h] <- nobs(lm(as.formula(paste(paste("log_yield_av",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))

#### was yield better compared to normal year?
#res_itt_prod[7,1,h] <- mean(dta_bal$yield_better[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_prod[8,1,h] <- sd(dta_bal$yield_better[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_prod[7,2,h] <- summary(lm(as.formula(paste(paste("yield_better",treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,1]
#res_itt_prod[8,2,h] <- summary(lm(as.formula(paste(paste("yield_better",treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,2]
#res_itt_prod[7,3,h] <- ifelse(totrep >0, RI("yield_better",treatment , ctrls, dta_trim, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("yield_better",treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))$coefficients[2,4])
#res_itt_prod[7,4,h] <- nobs(lm(as.formula(paste(paste("yield_better",treatment, sep="~"),ctrls,sep="+")), data=dta_bal, weights = weight))

### labour

#dta_bal2 <- subset(dta_bal, tot_time_hh>0)
#dta_bal2$log_tot_time_hh <- log(dta_bal2$tot_time_hh)
#dta_trim <- trim("log_tot_time_hh", dta_bal2, .05)

#res_itt_prod[9,1,h] <- mean(dta_trim$log_tot_time_hh[dta_trim$messenger == "ctrl"], na.rm=T)
#res_itt_prod[10,1,h] <- sd(dta_trim$log_tot_time_hh[dta_trim$messenger == "ctrl"], na.rm=T)
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

#res_itt_prod[11,1,h] <- mean(dta_trim$log_labour_prod[dta_trim$messenger == "ctrl"], na.rm=T)
#res_itt_prod[12,1,h] <- sd(dta_trim$log_labour_prod[dta_trim$messenger == "ctrl"], na.rm=T)
#res_itt_prod[11,2,h] <- summary(lm(as.formula(paste(paste("log_labour_prod",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,1]
#res_itt_prod[12,2,h] <- summary(lm(as.formula(paste(paste("log_labour_prod",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,2]
#res_itt_prod[11,3,h] <- ifelse(totrep >0, RI("log_labour_prod",treatment , ctrls, dta_trim, nr_repl = totrep, "weight"), summary(lm(as.formula(paste(paste("tot_time_hh",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))$coefficients[2,4]) 
#res_itt_prod[11,4,h] <- nobs(lm(as.formula(paste(paste("log_labour_prod",treatment, sep="~"),ctrls,sep="+")), data=dta_trim))

####index
#dta_bal2 <- subset(dta_bal, area_tot >0 & prod_tot>0 & yield_av >0)
#dta_bal2$log_prod_tot <- log(dta_bal2$prod_tot)
#dta_bal2$log_area_tot <- log(dta_bal2$area_tot)
#dta_bal2$log_yield_av <- log(dta_bal2$yield_av)
#dta_bal2$log_tot_time_hh <- log(dta_bal2$tot_time_hh)
#dta_bal2$log_tot_time_hh[is.infinite(dta_bal2$log_tot_time_hh)] <- NA 


#dta_bal2 <- trim("log_yield_av", dta_bal2, .05)




#dta_bal2$log_area_tot <- -dta_bal2$log_area_tot

#indexer <- FW_index(treatment, c("log_prod_tot", "log_area_tot", "yield_better", "log_tot_time_hh"),ctrls,w_int2 = "weight", data =dta_bal2, nr_repl=totrep )
#res_itt_prod[13,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
#res_itt_prod[14,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
#res_itt_prod[13,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
#res_itt_prod[14,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
#res_itt_prod[13,3,h] <-  indexer[[2]]
#res_itt_prod[13,4,h] <-  nobs(indexer[[1]])

##prod_plot[3,1] <- "production"
##prod_plot[3,3:4] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
##prod_plot[3,2] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])

#res_itt_prod[15,1:3 ,h] <- RI_FWER(c("log_prod_tot", "log_area_tot", "yield_better", "tot_time_hh"),indep = treatment, ctrls=ctrls ,dta =dta_bal2, p_vals = res_itt_prod[c(1,3,7,9),3,h], nr_repl = totrep, w_int="weight")



}

save(res_itt_know, file = "res_itt_know.RData")
save(res_itt_pract, file = "res_itt_pract.RData")
save(res_itt_fert, file = "res_itt_fert.RData")
save(res_itt_seed, file = "res_itt_seed.RData")
save(res_itt_prod, file = "res_itt_prod.RData")









