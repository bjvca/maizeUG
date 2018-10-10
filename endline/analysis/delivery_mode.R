rm(list=ls())
library(foreign)
#source("/home/bjvca/data/projects/digital green/endline/data/init.R")
#mobile <- read.dta("/home/bjvca/data/projects/digital green/baseline/DLEC.dta")[c("hhid","maizemobile","maizemobile_access")]
#source("functions.R")

#wget https://www.dropbox.com/s/p6kuazj263x9ilr/DLEC.dta?dl=0
#wget https://www.dropbox.com/s/n7hn2x0y492ofgi/AWS.csv?dl=0
#install.packages(c("ggplot2","doParallel","data.table","dplyr"))

dta <- read.csv("AWS.csv")
mobile <- read.dta("DLEC.dta")[c("hhid","maizemobile","maizemobile_access")]


#set totrep to zero if you do not want simulation based inferecne
library(ggplot2)
library(doParallel)
library(data.table)
library(dplyr)

set.seed(07032018)
cl <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
registerDoParallel(cl)
#dta <- subset(dta, !is.na(interview_status))
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
res_itt_disp <-  array(NA, c(9,4,3))
rownames(res_itt_disp) <- c("cons_maize","","sold_maize","","saved_seed","","disp_index","","p-vals")


###@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### For the comparison between different ways of delivering information to a household, we only keep households where the video was also shown to the man (as generally he will be the main decision maker)
###dta <- subset(dta, (recipient !="female" | messenger == "ctrl"))
### and so delivery mode needs its own RI function


RI <- function(dep, indep, dta , nr_repl = 1000, h_int=h) {
#indep <- "(messenger != 'ctrl')+ivr+sms+as.factor(recipient) + as.factor(messenger)" 
#h_int <- 1
#dep <- "know_combine"
#dta <- dta_bal
#nr_repl <- 1000


	### the NULL
	crit <- summary(lm(as.formula(paste(dep,indep,sep="~")), data=dta))$coefficients[2,1]

### determines treatmetn cell

	dta <- dta %>% 
    		mutate(treat = group_indices_(dta, .dots=c("recipient", "messenger"))) 
	### allocates unique ID based on treatment cell status and village
	dta <- dta %>% 
    		mutate(uniqID = group_indices_(dta, .dots=c("distID", "subID","vilID"))) 

	dta <-  data.table(cbind(dta[dep],dta[c("messenger","recipient","treat","uniqID","hhid","ivr","sms","called","totsms", "femhead")]))

	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
		dta_sim <- data.table(dta)
 		setDT(dta_sim)[,perm:=sample(treat),by = (uniqID)]
 		setDT(dta_sim)[!(perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["ctrl",]>0])),perm_ivr:=sample(ivr),by = (uniqID)]
		dta_sim$perm_ivr[is.na(dta_sim$perm_ivr)] <- "no"
		setDT(dta_sim)[perm_ivr =="yes",perm_sms:=sample(sms),by = (uniqID)]
dta_sim$perm_sms[is.na(dta_sim$perm_sms)] <- "no"
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["ctrl",]>0]), "ctrl", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","female")))
		dta_sim$ivr <- dta_sim$perm_ivr
		dta_sim$sms <- dta_sim$perm_sms
		return(abs(coef(lm(as.formula(paste(dep,indep,sep="~")), data=dta_sim))[2]) > abs(crit) )
	}
	return(sum(oper)/nr_repl)
}



RI_FWER <- function(deps, indep, dta ,p_vals , nr_repl = 1000, h_int=h) {
### function to control for FWER using simulation (familywise sharp null)
### inspired on https://egap.org/methods-guides/10-things-you-need-know-about-multiple-comparisons
threshold_finder<- function(threshold){
  mean(apply(oper, 2, x <- function(x) sum(x <= threshold) > 0 ))
}

	dta <- dta %>% 
    		mutate(treat = group_indices_(dta, .dots=c("recipient", "messenger"))) 
	### allocates unique ID based on treatment cell status and village
	dta <- dta %>% 
    		mutate(uniqID = group_indices_(dta, .dots=c("distID", "subID","vilID"))) 
 data.table(cbind(dta[deps],dta[c("messenger","recipient","treat","uniqID","hhid","ivr","sms","called","totsms", "femhead")]))
	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
		dta_sim <- data.table(dta)
 		setDT(dta_sim)[,perm:=sample(treat),by = (uniqID)]
 		setDT(dta_sim)[!(perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["ctrl",]>0])),perm_ivr:=sample(ivr),by = (uniqID)]
		dta_sim$perm_ivr[is.na(dta_sim$perm_ivr)] <- "no"
		setDT(dta_sim)[perm_ivr =="yes",perm_sms:=sample(sms),by = (uniqID)]
dta_sim$perm_sms[is.na(dta_sim$perm_sms)] <- "no"
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["ctrl",]>0]), "ctrl", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","female")))
		dta_sim$ivr <- dta_sim$perm_ivr
		dta_sim$sms <- dta_sim$perm_sms

return(unlist(lapply(deps, function(dvar) summary(lm(as.formula(paste(dvar,indep,sep="~")), data=dta_sim))$coefficients[2,4])))
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

FW_index <- function(treat, indexer, data, nr_repl=0,h_ind=h) {
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
	sig <- RI("index" ,treat , data, nr_repl = nr_repl,h_ind)
} else {
	sig <- summary(lm(as.formula(paste("index",treat,sep="~")) , data=data))$coefficients[2,4]
}
return(list(mod,sig, data))
}

### merge if they have phone and if they have access to phone
library(foreign)

mobile$maizemobile <- mobile$maizemobile == "Yes"
mobile$maizemobile_access <- mobile$maizemobile_access == "Yes"
mobile$maizemobile_access[mobile$maizemobile == TRUE] <- TRUE 

dta <- merge(dta,mobile)

##redo entire analysis, but with only those who have access to phone for ivr and those who own a mobile phone for sms

totrep <- 10000
### better to loop over h: 
for (h in seq(2,3,1)) {
if (h == 1) {
dta_bal <- dta
treatment <- "(messenger != 'ctrl')+ivr+sms+as.factor(recipient) + as.factor(messenger) + femhead" 
} else if ( h==2 ) {
dta_bal <- subset(dta,maizemobile_access == TRUE)
#dta_bal <- dta
treatment <- "ivr+(messenger != 'ctrl')+sms+as.factor(recipient) + as.factor(messenger) + femhead" 
} else if (h==3) {
dta_bal <- subset(dta,maizemobile == TRUE)
#dta_bal <- dta
treatment <- "sms+(messenger != 'ctrl')+ivr+as.factor(recipient) + as.factor(messenger) + femhead" 
}
################################ knowledge  ############################


####no need to balance data here because we control for both messenger and recipient factor levels


#res_itt_know[1,1,h] <- mean(dta_bal$know_space[dta_bal$messenger == "ctrl"],na.rm=T)
#res_itt_know[2,1,h] <- sd(dta_bal$know_space[dta_bal$messenger == "ctrl"],na.rm=T)
#res_itt_know[1,2,h] <- summary(lm(as.formula(paste("know_space",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
#res_itt_know[2,2,h] <- summary(lm(as.formula(paste("know_space",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
#res_itt_know[1,3,h] <- ifelse(totrep >0, RI("know_space", treatment , dta_bal,  totrep,h),summary(lm(as.formula(paste("know_space",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])
#res_itt_know[1,4,h] <- nobs(lm(as.formula(paste("know_space",treatment, sep="~")) ,data=dta_bal))

#res_itt_know[3,1,h] <-  mean(dta_bal$know_combine[dta_bal$messenger == "ctrl"],na.rm=T)
#res_itt_know[4,1,h] <-  sd(dta_bal$know_combine[dta_bal$messenger == "ctrl"],na.rm=T)
#res_itt_know[3,2,h] <- summary(lm(as.formula(paste("know_combine",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
#res_itt_know[4,2,h] <- summary(lm(as.formula(paste("know_combine",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
#res_itt_know[3,3,h] <-  ifelse(totrep >0, RI("know_combine",treatment , dta_bal, nr_repl = totrep,h),summary(lm(as.formula(paste("know_combine",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])
#res_itt_know[3,4,h] <- nobs(lm(as.formula(paste("know_combine",treatment, sep="~")) ,data=dta_bal))

#res_itt_know[5,1,h] <-  mean(dta_bal$know_weed[dta_bal$messenger == "ctrl"],na.rm=T)
#res_itt_know[6,1,h] <-  sd(dta_bal$know_weed[dta_bal$messenger == "ctrl"],na.rm=T)
#res_itt_know[5,2,h] <- summary(lm(as.formula(paste("know_weed",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
#res_itt_know[6,2,h] <- summary(lm(as.formula(paste("know_weed",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
#res_itt_know[5,3,h] <-  ifelse(totrep >0, RI("know_weed",treatment , dta_bal, nr_repl = totrep,h),summary(lm(as.formula(paste("know_weed",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])
#res_itt_know[5,4,h] <- nobs(lm(as.formula(paste("know_weed",treatment, sep="~")) ,data=dta_bal))

#res_itt_know[7,1,h] <- mean(dta_bal$know_armyworm[dta_bal$messenger == "ctrl"],na.rm=T)
#res_itt_know[8,1,h] <- sd(dta_bal$know_armyworm[dta_bal$messenger == "ctrl"],na.rm=T)
#res_itt_know[7,2,h] <- summary(lm(as.formula(paste("know_armyworm",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
#res_itt_know[8,2,h] <- summary(lm(as.formula(paste("know_armyworm",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
#res_itt_know[7,3,h] <-  ifelse(totrep >0, RI("know_armyworm",treatment , dta_bal, nr_repl = totrep,h),summary(lm(as.formula(paste("know_armyworm",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])
#res_itt_know[7,4,h] <- nobs(lm(as.formula(paste("know_armyworm",treatment, sep="~")) ,data=dta_bal))

### no need to include armyworm in the index because we do not really expect an effect
#indexer <- FW_index(treatment, c("know_space", "know_combine", "know_weed"),dta_bal, nr_repl=totrep,h)
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
#res_itt_know[11,1:3 ,h] <- RI_FWER(deps= c("know_space","know_combine","know_weed") ,indep = treatment ,dta =dta_bal, p_vals = res_itt_know[c(1,3,5),3,h], nr_repl = 10000,h_int=h)

#res_itt_know <- Bcorr(c("know_space", "know_combine", "know_weed"),dta_bal, res_itt_know ,h)


################################## practices #############################

#####timely planting
#res_itt_pract[1,1,h]  <- mean(dta_bal$day_one[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_pract[2,1,h]  <- sd(dta_bal$day_one[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_pract[1,2,h]  <- summary(lm(as.formula(paste("day_one", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
#res_itt_pract[2,2,h]  <- summary(lm(as.formula(paste("day_one", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
#res_itt_pract[1,3,h]  <- ifelse(totrep >0, RI("day_one",treatment , dta_bal, nr_repl = totrep,h), summary(lm(as.formula(paste("day_one", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
#res_itt_pract[1,4,h]  <- nobs(lm(as.formula(paste("day_one", treatment, sep ="~")), data=dta_bal))

##### used recommended spacing use on at lease one plot as reported by at least one spouse
#res_itt_pract[3,1,h]  <- mean(dta_bal$space[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_pract[4,1,h]  <- sd(dta_bal$space[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_pract[3,2,h]  <- summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
#res_itt_pract[4,2,h]  <- summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
#res_itt_pract[3,3,h]  <- ifelse(totrep >0, RI("space",treatment, dta_bal, nr_repl = totrep,h),  summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])
#res_itt_pract[3,4,h]  <- nobs(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))

### used recommended way to fight striga - this should be changed to include info of all plots 
#res_itt_pract[5,1,h]  <- mean(dta_bal$striga[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_pract[6,1,h]  <- sd(dta_bal$striga[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_pract[5,2,h]  <- summary(lm(as.formula(paste("striga", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
#res_itt_pract[6,2,h]  <- summary(lm(as.formula(paste("striga", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
#res_itt_pract[5,3,h]  <- ifelse(totrep >0, RI("striga",treatment , dta_bal, nr_repl = totrep,h), summary(lm(as.formula(paste("striga", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])
#res_itt_pract[5,4,h]  <- nobs(lm(as.formula(paste("striga", treatment, sep ="~")), data=dta_bal))

### weeded on recommended timing? - this should be changed to include info of all plots 
#res_itt_pract[7,1,h]  <- mean(dta_bal$weed[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_pract[8,1,h]  <- sd(dta_bal$weed[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_pract[7,2,h]  <- summary(lm(as.formula(paste("weed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
#res_itt_pract[8,2,h]  <- summary(lm(as.formula(paste("weed", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
#res_itt_pract[7,3,h]  <- ifelse(totrep >0, RI("weed",treatment, dta_bal, nr_repl = totrep,h),summary(lm(as.formula(paste("weed", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])
#res_itt_pract[7,4,h]  <- nobs(lm(as.formula(paste("weed", treatment, sep ="~")), data=dta_bal))

### fertilizer use
##res_itt_pract[9,1,h]  <-  mean(dta_bal$fert[dta_bal$messenger == "ctrl"], na.rm=T)
##res_itt_pract[10,1,h]  <-  sd(dta_bal$fert[dta_bal$messenger == "ctrl"], na.rm=T)
##res_itt_pract[9,2,h]  <- summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
##res_itt_pract[10,2,h]  <- summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
##res_itt_pract[9,3,h]  <- ifelse(totrep >0, RI("fert",treatment , dta_bal, nr_repl = totrep,h), summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
##res_itt_pract[9,4,h]  <- nobs(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))

##fert_plot[1,1] <- "all fertilizer"
##fert_plot[1,3:4] <- confint(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$fert[dta_bal$messenger == "ctrl"], na.rm=T)
##fert_plot[1,2] <-  summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$fert[dta_bal$messenger == "ctrl"], na.rm=T)

##### fert = DAP/NPK
#res_itt_fert[1,1,h]  <-  mean(dta_bal$fert_dap[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_fert[2,1,h]  <-  sd(dta_bal$fert_dap[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_fert[1,2,h]  <- summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
#res_itt_fert[2,2,h]  <- summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
#res_itt_fert[1,3,h]  <- ifelse(totrep >0, RI("fert_dap",treatment , dta_bal, nr_repl = totrep,h) , summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])
#res_itt_fert[1,4,h]  <- nobs(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))

##fert_plot[2,1] <- "DAP/NPK"
##fert_plot[2,3:4] <- confint(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$fert_dap[dta_bal$messenger == "ctrl"], na.rm=T)
##fert_plot[2,2] <-  summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$fert_dap[dta_bal$messenger == "ctrl"], na.rm=T)

##### fert = urea
#res_itt_fert[3,1,h]  <-  mean(dta_bal$fert_urea[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_fert[4,1,h]  <-  sd(dta_bal$fert_urea[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_fert[3,2,h]  <- summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
#res_itt_fert[4,2,h]  <- summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
#res_itt_fert[3,3,h]  <- ifelse(totrep >0, RI("fert_urea",treatment , dta_bal, nr_repl = totrep,h) , summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])
#res_itt_fert[3,4,h]  <- nobs(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))

##fert_plot[3,1] <- "urea"
##fert_plot[3,3:4] <- confint(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$fert_urea[dta_bal$messenger == "ctrl"], na.rm=T)
##fert_plot[3,2] <-  summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$fert_urea[dta_bal$messenger == "ctrl"], na.rm=T)

##### fert = organic
#res_itt_fert[5,1,h]  <-  mean(dta_bal$fert_org[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_fert[6,1,h]  <-  sd(dta_bal$fert_org[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_fert[5,2,h]  <-  summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
#res_itt_fert[6,2,h]  <- summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
#res_itt_fert[5,3,h]  <- ifelse(totrep >0, RI("fert_org",treatment , dta_bal, nr_repl = totrep,h), summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
#res_itt_fert[5,4,h]  <-  nobs(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))

##fert_plot[4,1] <- "organic"
##fert_plot[4,3:4] <- confint(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$fert_org[dta_bal$messenger == "ctrl"], na.rm=T)
##fert_plot[4,2] <-  summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$fert_org[dta_bal$messenger == "ctrl"], na.rm=T)

##dta_bal$fert_dap <- !(dta_bal$fert_dap) 
#dta_bal$fert_org <- !(dta_bal$fert_org) 
#indexer <-  FW_index(treatment,c("fert_dap","fert_urea","fert_org"),dta_bal, nr_repl=totrep)
#res_itt_fert[7,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
#res_itt_fert[8,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
#res_itt_fert[7,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
#res_itt_fert[8,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
#res_itt_fert[7,3,h] <-  indexer[[2]]
#res_itt_fert[8,4,h] <-  nobs(indexer[[1]])


###improved seed  
##res_itt_pract[11,1,h]  <-  mean(dta_bal$impseed[dta_bal$messenger == "ctrl"], na.rm=T)
##res_itt_pract[12,1,h]  <-  sd(dta_bal$impseed[dta_bal$messenger == "ctrl"], na.rm=T)
##res_itt_pract[11,2,h]  <- summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
##res_itt_pract[12,2,h]  <- summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
##res_itt_pract[11,3,h]  <- ifelse(totrep >0, RI("impseed",treatment , dta_bal, nr_repl = totrep,h),  summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
##res_itt_pract[11,4,h]  <- nobs(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))

##seed_plot[1,1] <- "all seed"
##seed_plot[1,3:4] <- confint(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$impseed[dta_bal$messenger == "ctrl"], na.rm=T)
##seed_plot[1,2] <-  summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$impseed[dta_bal$messenger == "ctrl"], na.rm=T)



### hybrid
#res_itt_seed[1,1,h]  <-  mean(dta_bal$hybrid[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_seed[2,1,h]  <-  sd(dta_bal$hybrid[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_seed[1,2,h] <- summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
#res_itt_seed[2,2,h] <- summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
#res_itt_seed[1,3,h] <- ifelse(totrep >0, RI("hybrid",treatment , dta_bal, nr_repl = totrep,h), summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
#res_itt_seed[1,4,h] <- nobs(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))

##seed_plot[2,1] <- "hybrid"
##seed_plot[2,3:4] <- confint(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$hybrid[dta_bal$messenger == "ctrl"], na.rm=T)
##seed_plot[2,2] <-  summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$hybrid[dta_bal$messenger == "ctrl"], na.rm=T)

### opv
#res_itt_seed[3,1,h]  <-  mean(dta_bal$opv[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_seed[4,1,h]  <-  sd(dta_bal$opv[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_seed[3,2,h] <- summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
#res_itt_seed[4,2,h] <- summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
#res_itt_seed[3,3,h] <- ifelse(totrep >0, RI("opv",treatment , dta_bal, nr_repl = totrep,h), summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
#res_itt_seed[3,4,h] <- nobs(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))

#indexer <-  FW_index(treatment,c("hybrid","opv"),dta_bal, nr_repl=totrep)
#res_itt_seed[5,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
#res_itt_seed[6,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
#res_itt_seed[5,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
#res_itt_seed[6,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
#res_itt_seed[5,3,h] <-  indexer[[2]]
#res_itt_seed[6,4,h] <-  nobs(indexer[[1]])

##seed_plot[3,1] <- "open pollinated"
##seed_plot[3,3:4] <- confint(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$opv[dta_bal$messenger == "ctrl"], na.rm=T)
##seed_plot[3,2] <-  summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$opv[dta_bal$messenger == "ctrl"], na.rm=T)

###combiner
##res_itt_pract[13,1,h]  <-  mean(dta_bal$combiner[dta_bal$messenger == "ctrl"], na.rm=T)
##res_itt_pract[14,1,h]  <-  sd(dta_bal$combiner[dta_bal$messenger == "ctrl"], na.rm=T)
##res_itt_pract[13,2,h]  <- summary(lm(as.formula(paste("combiner", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
##res_itt_pract[14,2,h]  <- summary(lm(as.formula(paste("combiner", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
##res_itt_pract[13,3,h]  <- ifelse(totrep >0, RI("combiner",treatment , dta_bal, nr_repl = totrep,h), summary(lm(as.formula(paste("combiner", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
##res_itt_pract[13,4,h]  <- nobs(lm(as.formula(paste("combiner", treatment, sep ="~")), data=dta_bal))

##### bought seed
##res_itt_pract[15,1,h]  <-  mean(dta_bal$bought_seed[dta_bal$messenger == "ctrl"], na.rm=T)
##res_itt_pract[16,1,h]  <-  sd(dta_bal$bought_seed[dta_bal$messenger == "ctrl"], na.rm=T)
##res_itt_pract[15,2,h]  <- summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
##res_itt_pract[16,2,h]  <- summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
##res_itt_pract[15,3,h]  <- ifelse(totrep >0, RI("bought_seed",treatment , dta_bal, nr_repl = totrep,h), summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
##res_itt_pract[15,4,h]  <- nobs(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))

###seed_plot[4,1] <- "bought seed"
###seed_plot[4,3:4] <- confint(lm(as.formula(paste("bought_seed", treatment, sep ="~"), level=.9), data=dta_bal))[2,] /  mean(dta_bal$bought_seed[dta_bal$messenger == "ctrl"], na.rm=T)
###seed_plot[4,2] <-  summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$bought_seed[dta_bal$messenger == "ctrl"], na.rm=T)


###### used chemicals
##res_itt_pract[17,1,h]  <-  mean(dta_bal$chem[dta_bal$messenger == "ctrl"], na.rm=T)
##res_itt_pract[18,1,h]  <-  sd(dta_bal$chem[dta_bal$messenger == "ctrl"], na.rm=T)
##res_itt_pract[17,2,h]  <- summary(lm(as.formula(paste("chem", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
##res_itt_pract[18,2,h]  <- summary(lm(as.formula(paste("chem", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
##res_itt_pract[17,3,h]  <- ifelse(totrep >0, RI("chem",treatment , dta_bal, nr_repl = totrep,h), summary(lm(as.formula(paste("chem", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
##res_itt_pract[17,4,h]  <- nobs(lm(as.formula(paste("chem", treatment, sep ="~")), data=dta_bal))

#####hired labour
##res_itt_pract[19,1,h]  <-  mean(dta_bal$labour[dta_bal$messenger == "ctrl"], na.rm=T)
##res_itt_pract[20,1,h]  <-  sd(dta_bal$labour[dta_bal$messenger == "ctrl"], na.rm=T)
##res_itt_pract[19,2,h]  <- summary(lm(as.formula(paste("labour", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
##res_itt_pract[20,2,h]  <- summary(lm(as.formula(paste("labour", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
##res_itt_pract[19,3,h]  <- ifelse(totrep >0, RI("labour",treatment , dta_bal, nr_repl = totrep,h), summary(lm(as.formula(paste("labour", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
##res_itt_pract[19,4,h]  <- nobs(lm(as.formula(paste("labour", treatment, sep ="~")), data=dta_bal))

##if (totrep >0) {
##res_h0_pract[1:7,4,h] <- FSR_RI( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment ,dta_bal, pvals =  res_h0_pract[,3,h] , nr_repl_pi = 100)

##res_h0_pract[1:7,4,h] <- FSR_OLS( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment,dta_bal, nr_repl = totrep)[[4]]

#indexer <-  FW_index(treatment,c("day_one","space","striga","weed"),dta_bal, nr_repl=totrep)
#res_itt_pract[21,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
#res_itt_pract[22,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
#res_itt_pract[21,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
#res_itt_pract[22,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
#res_itt_pract[21,3,h] <-  indexer[[2]]
#res_itt_pract[21,4,h] <-  nobs(indexer[[1]])

##prod_plot[2,1] <- "adoption"
##prod_plot[2,3:4] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
##prod_plot[2,2] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])


##res_itt_pract <- Bcorr( c("day_one","space","striga","weed", "fert","impseed", "combiner","bought_seed","chem","labour"),dta_bal, res_itt_pract ,h)
##res_itt_fert <- Bcorr(c("fert_dap","fert_urea","fert_org"),dta_bal, res_itt_fert ,h)
##res_itt_seed <- Bcorr(c("hybrid","opv"),dta_bal, res_itt_seed ,h)

##res_itt_pract[23,1:3 ,h] <- RI_FWER(c("day_one","space","striga","weed"),indep = treatment,dta_bal, res_itt_pract[c(1,3,5,7),3,h], 10000,h_int=h)

#res_itt_fert[9,1:3 ,h] <- RI_FWER(c("fert_dap","fert_urea","fert_org"),indep = treatment,dta_bal, res_itt_fert[c(1,3,5),3,h],10000,h_int=h)

#res_itt_seed[7,1:3 ,h] <-  RI_FWER(c("hybrid","opv"),indep = treatment,dta_bal, res_itt_seed[c(1,3),3,h],10000,h_int=h)
#}

################################# production ###########################
####### does the video increases production related outcomes?

##trimming is done on end result
#dta_bal2 <- subset(dta_bal, prod_tot>0)
#dta_bal2$log_prod_tot <- log(dta_bal2$prod_tot)
#dta_trim <- trim("log_prod_tot", dta_bal2, .05)

#### production
#res_itt_prod[1,1,h] <- mean(dta_trim$log_prod_tot[dta_trim$messenger == "ctrl"], na.rm=T)
#res_itt_prod[2,1,h] <- sd(dta_trim$log_prod_tot[dta_trim$messenger == "ctrl"], na.rm=T)
#res_itt_prod[1,2,h] <- summary(lm(as.formula(paste("log_prod_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
#res_itt_prod[2,2,h] <- summary(lm(as.formula(paste("log_prod_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,2]
#res_itt_prod[1,3,h] <- ifelse(totrep >0, RI("log_prod_tot",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_prod_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])
#res_itt_prod[1,4,h] <- nobs(lm(as.formula(paste("log_prod_tot",treatment,sep = "~")), data=dta_trim))

#### area
#dta_bal2 <- subset(dta_bal, area_tot>0)
#dta_bal2$log_area_tot <- log(dta_bal2$area_tot)
#dta_trim <- trim("log_area_tot", dta_bal2, .05)

#res_itt_prod[3,1,h] <- mean(dta_trim$log_area_tot[dta_trim$messenger == "ctrl"], na.rm=T)
#res_itt_prod[4,1,h] <- sd(dta_trim$log_area_tot[dta_trim$messenger == "ctrl"], na.rm=T)
#res_itt_prod[3,2,h] <- summary(lm(as.formula(paste("log_area_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
#res_itt_prod[4,2,h] <- summary(lm(as.formula(paste("log_area_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,2]
#res_itt_prod[3,3,h] <- ifelse(totrep >0, RI("log_area_tot",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_area_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])
#res_itt_prod[3,4,h] <- nobs(lm(as.formula(paste("log_area_tot",treatment,sep = "~")), data=dta_trim))

####yield

#dta_bal2 <- subset(dta_bal, yield_av >0)
#dta_bal2$log_yield_av <- log(dta_bal2$yield_av)
#dta_trim <- trim("log_yield_av", dta_bal2, .05)

#res_itt_prod[5,1,h] <- mean(dta_trim$log_yield_av[dta_trim$messenger == "ctrl"], na.rm=T)
#res_itt_prod[6,1,h] <- sd(dta_trim$log_yield_av[dta_trim$messenger == "ctrl"], na.rm=T)
#res_itt_prod[5,2,h] <- summary(lm(as.formula(paste("log_yield_av",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
#res_itt_prod[6,2,h] <- summary(lm(as.formula(paste("log_yield_av",treatment,sep = "~")), data=dta_trim))$coefficients[2,2]
#res_itt_prod[5,3,h] <- ifelse(totrep >0, RI("log_yield_av",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_yield_av",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])
#res_itt_prod[5,4,h] <- nobs(lm(as.formula(paste("log_yield_av",treatment,sep = "~")), data=dta_trim))

#### was yield better compared to normal year?
#res_itt_prod[7,1,h] <- mean(dta_bal$yield_better[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_prod[8,1,h] <- sd(dta_bal$yield_better[dta_bal$messenger == "ctrl"], na.rm=T)
#res_itt_prod[7,2,h] <- summary(lm(as.formula(paste("yield_better",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
#res_itt_prod[8,2,h] <- summary(lm(as.formula(paste("yield_better",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
#res_itt_prod[7,3,h] <- ifelse(totrep >0, RI("yield_better",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("yield_better",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])
#res_itt_prod[7,4,h] <- nobs(lm(as.formula(paste("yield_better",treatment,sep = "~")), data=dta_bal))

### labour

#dta_bal2 <- subset(dta_bal, tot_time_hh>0)
#dta_bal2$log_tot_time_hh <- log(dta_bal2$tot_time_hh)
#dta_trim <- trim("log_tot_time_hh", dta_bal2, .05)

#res_itt_prod[9,1,h] <- mean(dta_trim$log_tot_time_hh[dta_trim$messenger == "ctrl"], na.rm=T)
#res_itt_prod[10,1,h] <- sd(dta_trim$log_tot_time_hh[dta_trim$messenger == "ctrl"], na.rm=T)
#res_itt_prod[9,2,h] <- summary(lm(as.formula(paste("log_tot_time_hh",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
#res_itt_prod[10,2,h] <- summary(lm(as.formula(paste("log_tot_time_hh",treatment,sep = "~")), data=dta_trim))$coefficients[2,2]
#res_itt_prod[9,3,h] <- ifelse(totrep >0, RI("log_tot_time_hh",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_tot_time_hh",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])
#res_itt_prod[9,4,h] <- nobs(lm(as.formula(paste("log_tot_time_hh",treatment,sep = "~")), data=dta_trim))

### labour productivity
#dta_bal2 <- subset(dta_bal, prod_tot>0)
#dta_bal2 <- subset(dta_bal2, tot_time_hh>0)
#dta_bal2$log_labour_prod <- log(dta_bal2$prod_tot/dta_bal2$tot_time_hh)
#dta_bal2$labour_prod <- dta_bal2$prod_tot/dta_bal2$tot_time_hh
#dta_trim <- trim("log_labour_prod", dta_bal2, .05)

#res_itt_prod[11,1,h] <- mean(dta_trim$log_labour_prod[dta_trim$messenger == "ctrl"], na.rm=T)
#res_itt_prod[12,1,h] <- sd(dta_trim$log_labour_prod[dta_trim$messenger == "ctrl"], na.rm=T)
#res_itt_prod[11,2,h] <- summary(lm(as.formula(paste("log_labour_prod",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
#res_itt_prod[12,2,h] <- summary(lm(as.formula(paste("log_labour_prod",treatment,sep = "~")), data=dta_trim))$coefficients[2,2]
#res_itt_prod[11,3,h] <- ifelse(totrep >0, RI("log_labour_prod",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("tot_time_hh",treatment,sep = "~")), data=dta_trim))$coefficients[2,4]) 
#res_itt_prod[11,4,h] <- nobs(lm(as.formula(paste("log_labour_prod",treatment,sep = "~")), data=dta_trim))

####index
#dta_bal2 <- subset(dta_bal, area_tot >0 & prod_tot>0 & yield_av >0)
#dta_bal2$log_prod_tot <- log(dta_bal2$prod_tot)
#dta_bal2$log_area_tot <- log(dta_bal2$area_tot)
#dta_bal2$log_yield_av <- log(dta_bal2$yield_av)

#dta_bal2 <- trim("log_yield_av", dta_bal2, .05)


##res_itt_prod <- Bcorr(c("log_prod_tot","log_area_tot","log_yield_av","yield_better"),dta_bal2, res_itt_prod ,h)
##RI_FWER(c("log_prod_tot","log_area_tot","log_yield_av","yield_better"),"(messenger != 'ctrl') +ivr+sms+as.factor(recipient)",dta_bal2, c(0.7791	,0.0264	,0.0154	,0.5528))

#dta_bal2$log_area_tot <- -dta_bal2$log_area_tot

#	indexer <- FW_index(treatment, c("log_prod_tot", "log_area_tot", "yield_better", "tot_time_hh"),dta_bal2, nr_repl=totrep)
#res_itt_prod[13,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
#res_itt_prod[14,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
#res_itt_prod[13,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
#res_itt_prod[14,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
#res_itt_prod[13,3,h] <-  indexer[[2]]
#res_itt_prod[13,4,h] <-  nobs(indexer[[1]])

#prod_plot[3,1] <- "production"
#prod_plot[3,3:4] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
#prod_plot[3,2] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])

#res_itt_prod[15,1:3 ,h] <- RI_FWER(c("log_prod_tot", "log_area_tot", "yield_better", "tot_time_hh"),indep = treatment,dta_bal2, res_itt_prod[c(1,3,7,9),3,h], 10000,h_int=h)

################################## disposal ##########################
### maize consumed


res_itt_disp[1,1,h] <- mean(dta_bal$cons_maize_yes[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_disp[2,1,h] <- sd(dta_bal$cons_maize_yes[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_disp[1,2,h] <- summary(lm(as.formula(paste("cons_maize_yes",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_disp[2,2,h] <- summary(lm(as.formula(paste("cons_maize_yes",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_disp[1,3,h] <- ifelse(totrep >0, RI("cons_maize_yes",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("cons_maize_yes",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])
res_itt_disp[1,4,h] <- nobs(lm(as.formula(paste("cons_maize_yes",treatment,sep = "~")), data=dta_bal))

### sold maize?

res_itt_disp[3,1,h] <- mean(dta_bal$sold_maize[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_disp[4,1,h] <- sd(dta_bal$sold_maize[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_disp[3,2,h] <- summary(lm(as.formula(paste("sold_maize",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_disp[4,2,h] <- summary(lm(as.formula(paste("sold_maize",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_disp[3,3,h] <- ifelse(totrep >0, RI("sold_maize",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("sold_maize",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])
res_itt_disp[3,4,h] <- nobs(lm(as.formula(paste("sold_maize",treatment,sep = "~")), data=dta_bal))

### kept maize for seed?
res_itt_disp[5,1,h] <- mean(dta_bal$save_seed[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_disp[6,1,h] <- sd(dta_bal$save_seed[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_disp[5,2,h] <- summary(lm(as.formula(paste("save_seed",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_disp[6,2,h] <- summary(lm(as.formula(paste("save_seed",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_disp[5,3,h] <- ifelse(totrep >0, RI("save_seed",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("save_seed",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])
res_itt_disp[5,4,h] <- nobs(lm(as.formula(paste("save_seed",treatment,sep = "~")), data=dta_bal))

#res_itt_disp <- Bcorr(c("cons_maize_yes","sold_maize","save_seed"),dta_bal, res_itt_disp ,h)


#res_h0_disp[1:3,4,h] <- FSR_OLS(c("cons_maize_yes", "sold_maize", "save_seed"),treatment,dta_bal, nr_repl = totrep)[[4]]
dta_bal2 <- dta_bal
dta_bal2$save_seed <- !dta_bal2$save_seed

indexer <- FW_index(treatment, c("cons_maize_yes", "sold_maize", "save_seed"),dta_bal2, nr_repl=totrep)
res_itt_disp[7,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
res_itt_disp[8,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
res_itt_disp[7,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_disp[8,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_disp[7,3,h] <-  indexer[[2]]
res_itt_disp[7,4,h] <-  nobs(indexer[[1]])

res_itt_disp[9,1:3,h] <- RI_FWER(c("cons_maize_yes", "sold_maize", "save_seed"),indep = treatment,dta_bal, res_itt_disp[c(1,3,5),3,h], 10000,h_int=h)
################################ welfare #############################

## better off than average
res_itt_wel[1,1,h] <- mean(dta_bal$better_av[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_wel[2,1,h] <- sd(dta_bal$better_av[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_wel[1,2,h]  <- summary(lm(as.formula(paste("better_av",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_wel[2,2,h]  <- summary(lm(as.formula(paste("better_av",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_wel[1,3,h]  <- ifelse(totrep >0, RI("better_av",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("better_av",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])
res_itt_wel[1,4,h]  <- nobs(lm(as.formula(paste("better_av",treatment,sep = "~")), data=dta_bal))

## better off than 6mo
res_itt_wel[3,1,h] <- mean(dta_bal$better_6m[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_wel[4,1,h] <- sd(dta_bal$better_6m[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_wel[3,2,h]  <- summary(lm(as.formula(paste("better_6m",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_wel[4,2,h]  <- summary(lm(as.formula(paste("better_6m",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_wel[3,3,h]  <- ifelse(totrep >0, RI("better_6m",treatment , dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("better_6m",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])
res_itt_wel[3,4,h]  <- nobs(lm(as.formula(paste("better_6m",treatment,sep = "~")), data=dta_bal))

#can eat preferred foods
res_itt_wel[5,1,h] <- mean(dta_bal$eatpref[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_wel[6,1,h] <- sd(dta_bal$eatpref[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_wel[5,2,h]  <-  summary(lm(as.formula(paste("eatpref",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_wel[6,2,h]  <-  summary(lm(as.formula(paste("eatpref",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_wel[5,3,h]  <- ifelse(totrep >0, RI("eatpref",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("eatpref",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])
res_itt_wel[5,4,h]  <-  nobs(lm(as.formula(paste("eatpref",treatment,sep = "~")), data=dta_bal))

#can eat enough foods
res_itt_wel[7,1,h] <- mean(dta_bal$eatenough[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_wel[8,1,h] <- sd(dta_bal$eatenough[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_wel[7,2,h]  <- summary(lm(as.formula(paste("eatenough",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_wel[8,2,h]  <- summary(lm(as.formula(paste("eatenough",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_wel[7,3,h]  <- ifelse(totrep >0, RI("eatenough",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("eatenough",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])
res_itt_wel[7,4,h]  <- nobs(lm(as.formula(paste("eatenough",treatment,sep = "~")), data=dta_bal))

#consumption - logged and trimmed
dta_bal2 <- subset(dta_bal, cons>0)
dta_bal2$log_cons <- log(dta_bal2$cons)
dta_trim <- trim("log_cons", dta_bal2, .05)

res_itt_wel[9,1,h] <- mean(dta_trim$log_cons[dta_trim$messenger == "ctrl"], na.rm=T)
res_itt_wel[10,1,h] <- sd(dta_trim$log_cons[dta_trim$messenger == "ctrl"], na.rm=T)
res_itt_wel[9,2,h]  <- summary(lm(as.formula(paste("log_cons",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_itt_wel[10,2,h]  <- summary(lm(as.formula(paste("log_cons",treatment,sep = "~")), data=dta_trim))$coefficients[2,2]
res_itt_wel[9,3,h]  <- ifelse(totrep >0, RI("log_cons",treatment , dta_bal2, nr_repl = totrep), summary(lm(as.formula(paste("log_cons",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])
res_itt_wel[9,4,h]  <- nobs(lm(as.formula(paste("log_cons",treatment,sep = "~")), data=dta_trim))



	indexer <- FW_index(treatment, c("better_av", "better_6m", "eatpref","eatenough","log_cons"),dta_trim, nr_repl=totrep)
res_itt_wel[11,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
res_itt_wel[12,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
res_itt_wel[11,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_wel[12,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_wel[11,3,h] <-  indexer[[2]]
res_itt_wel[11,4,h] <-  nobs(indexer[[1]])

prod_plot[4,1] <- "welfare"
prod_plot[4,3:4] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
prod_plot[4,2] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])

prod_plot$x <- factor(prod_plot$x, levels=rev(prod_plot$x))
prod_plot$grp <- "video"
res_itt_wel[13,1:3,h] <- RI_FWER(c("better_av", "better_6m", "eatpref","eatenough","log_cons"),indep = treatment,dta_trim, res_itt_wel[c(1,3,5,7,9),3,h], 10000,h_int=h)
}

pdf("/home/bjvca/data/projects/digital green/endline/results/summaryplot_video.pdf")
credplot.gg(prod_plot,'SDs')
dev.off()
fert_plot$grp <- NA
fert_plot$grp[9:12] <- "sms"
fert_plot$grp[1:4] <- "video"
fert_plot$grp[5:8] <- "ivr"
seed_plot$grp <- NA
seed_plot$grp[9:12] <- "sms"
seed_plot$grp[1:4] <- "video"
seed_plot$grp[5:8] <- "ivr"

fert_plot$x <- factor(fert_plot$x, levels=rev(fert_plot$x))
fert_plot$grp <- factor(fert_plot$grp, levels = c("video","ivr", "sms"))

seed_plot$x <- factor(seed_plot$x, levels=rev(seed_plot$x))
seed_plot$grp <- factor(seed_plot$grp, levels = c("video","ivr", "sms"))

pdf("/home/bjvca/data/projects/digital green/endline/results/summaryplot_mode.pdf")
credplot.gg(prod_plot,'SDs')
dev.off()

pdf("/home/bjvca/data/projects/digital green/endline/results/fertplot_mode.pdf")
credplot.gg(fert_plot,'%-change')
dev.off()

pdf("/home/bjvca/data/projects/digital green/endline/results/seedplot_mode.pdf")
credplot.gg(seed_plot,'%-change')
dev.off()




############################################################### TOT #######################################################################################


source("/home/bjvca/data/projects/digital green/endline/data/init.R")
#source("functions.R")
#dta <- read.csv("AWS.csv")
#set totrep to zero if you do not want simulation based inferecne


set.seed(07032018)
dta <- subset(dta, !is.na(interview_status))
dta$messenger <- as.character(dta$messenger)

### indexing results arrays
res_tot_know <- array(NA, c(5,4,4)) 
rownames(res_tot_know) <- c("know_space","know_combine","know_weed", "know_armyworm","know_ind")
res_tot_pract <- array(NA, c(16,4,4))
rownames(res_tot_pract) <- c("space","striga","weed", "use_fert","use_DAP","use_urea","use_organic","seed","hybrid","opv","combiner","bought_seed","chem","labour","first_day","pract_index")
res_tot_prod <- array(NA, c(5,4,4))
rownames(res_tot_prod) <- c("prod","area","yield","yield_better","prod_index")
res_tot_wel <-  array(NA, c(6,4,4))
rownames(res_tot_wel) <- c("better_av","better_6m","eatpref","eatenough","log_cons","welfare_index")
res_tot_disp <-  array(NA, c(4,4,4))
rownames(res_tot_disp) <- c("cons_maize","sold_maize","saved_seed","disp_index")

cl <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
registerDoParallel(cl)



h <- 2
############################################# is there an additional ivr effect ##############################################
## drop the control
dta <- subset(dta, (recipient !="female" | messenger == "ctrl"))
dta <- subset(dta, messenger != "ctrl")

ctrl_vars <- "as.factor(recipient) + as.factor(messenger) + sms" 
treatment  <- "called"
instrument <- "(ivr == 'yes')" 
dta_bal <- dta

res_tot_know[1,2,h] <- coefficients(ivreg(as.formula(paste(paste(paste(paste("know_space",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_know[1,3,h] <- ifelse(totrep >0, RI_ivreg_ivr("know_space",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("know_space",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

res_tot_know[2,2,h] <-coefficients(ivreg(as.formula(paste(paste(paste(paste("know_combine",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_know[2,3,h] <- ifelse(totrep >0, RI_ivreg_ivr("know_combine",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("know_combine",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])


res_tot_know[3,2,h] <- coefficients(ivreg(as.formula(paste(paste(paste(paste("know_weed",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_know[3,3,h] <-  ifelse(totrep >0, RI_ivreg_ivr("know_weed",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("know_weed",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

res_tot_know[4,2,h] <- coefficients(ivreg(as.formula(paste(paste(paste(paste("know_armyworm",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_know[4,3,h] <-  ifelse(totrep >0, RI_ivreg_ivr("know_armyworm",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("know_armyworm",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

## no need to include armyworm in the index because we do not really expect an effect - but even when we include the effect is sig, so keep it in
#indexer <- FW_index(treatment, c("know_space", "know_combine", "know_weed","know_armyworm"),dta_bal, nr_repl=totrep)
#res_h0_know[5,1,h] <-  indexer[[1]]$coefficients[1,1]
#res_h0_know[5,2,h] <-  indexer[[1]]$coefficients[2,1]
#res_h0_know[5,3,h] <-  indexer[[2]]

###if (wyfs_stat) {
###	res_h0_know[1:4,4,h] <- FSR_RI_ivr( c("know_space","know_combine","know_weed", "know_armyworm") ,treatment ,dta_bal, pvals = res_h0_know[1:4,3,h], nr_repl_RI_ivr = 100)[[4]]
###	} else { 
###if (totrep >0) {
###	res_h0_know[1:4,4,h] <- FSR_OLS( c("know_space","know_combine","know_weed","know_armyworm") ,treatment,dta_bal, nr_repl = totrep)[[4]]
###}
###}

################################# practices #############################
#### used recommended spacing use on at lease one plot as reported by at least one spouse


res_tot_pract[1,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("space",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_pract[1,3,h]  <- ifelse(totrep >0, RI_ivreg_ivr("space",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("space",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

## used recommended way to fight striga - this should be changed to include info of all plots 
res_tot_pract[2,2,h]  <-  coefficients(ivreg(as.formula(paste(paste(paste(paste("striga",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_pract[2,3,h]  <- ifelse(totrep >0, RI_ivreg_ivr("striga",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("striga",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

## weeded on recommended timing? - this should be changed to include info of all plots 
res_tot_pract[3,2,h]  <-coefficients(ivreg(as.formula(paste(paste(paste(paste("weed",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]

res_tot_pract[3,3,h]  <-ifelse(totrep >0, RI_ivreg_ivr("weed",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("weed",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4]) 

## fertilizer use
res_tot_pract[4,2,h]  <-coefficients(ivreg(as.formula(paste(paste(paste(paste("fert",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]

res_tot_pract[4,3,h]  <- ifelse(totrep >0, RI_ivreg_ivr("fert",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("fert",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])
#### fert = DAP/NPK
res_tot_pract[5,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("fert_dap",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]

res_tot_pract[5,3,h]  <- ifelse(totrep >0, RI_ivreg_ivr("fert_dap",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("fert_dap",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

#### fert = urea

res_tot_pract[6,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("fert_urea",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_pract[6,3,h]  <- ifelse(totrep >0, RI_ivreg_ivr("fert_urea",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("fert_urea",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

#### fert = organic

res_tot_pract[7,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("fert_org",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_pract[7,3,h]  <- ifelse(totrep >0, RI_ivreg_ivr("fert_org",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("fert_org",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

##improved seed  

res_tot_pract[8,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("impseed",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_pract[8,3,h]  <- ifelse(totrep >0, RI_ivreg_ivr("impseed",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("impseed",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])
## hybrid

res_tot_pract[9,2,h] <- coefficients(ivreg(as.formula(paste(paste(paste(paste("hybrid",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_pract[9,3,h] <- ifelse(totrep >0, RI_ivreg_ivr("hybrid",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("hybrid",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

## opv

res_tot_pract[10,2,h] <- coefficients(ivreg(as.formula(paste(paste(paste(paste("opv",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_pract[10,3,h] <- ifelse(totrep >0, RI_ivreg_ivr("opv",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("opv",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

##combiner

res_tot_pract[11,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("combiner",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_pract[11,3,h]  <- ifelse(totrep >0, RI_ivreg_ivr("combiner",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("combiner",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

### bought seed

res_tot_pract[12,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("bought_seed",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_pract[12,3,h]  <- ifelse(totrep >0, RI_ivreg_ivr("bought_seed",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("bought_seed",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])
#### used chemicals

res_tot_pract[13,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("chem",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_pract[13,3,h]  <- ifelse(totrep >0, RI_ivreg_ivr("chem",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("chem",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

###hired labour

res_tot_pract[14,2,h]  <- scoefficients(ivreg(as.formula(paste(paste(paste(paste("labour",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_pract[14,3,h]  <- ifelse(totrep >0, RI_ivreg_ivr("labour",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("labour",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

###timely planting

res_tot_pract[15,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("day_one",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_pract[15,3,h]  <- ifelse(totrep >0, RI_ivreg_ivr("day_one",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("day_one",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

#if (totrep >0) {
#res_tot_pract[1:7,4,h] <- FSR_RI_ivr( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment ,dta_bal, pvals =  res_tot_pract[,3,h] , nr_repl_pi = 100)

#res_tot_pract[1:7,4,h] <- FSR_OLS( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment,dta_bal, nr_repl = totrep)[[4]]

#indexer <-  FW_index(treatment,c("space","striga","weed", "fert","impseed", "combiner","bought_seed","chem","labour","day_one"),dta_bal, nr_repl=totrep)
#res_tot_pract[16,1,h] <-  indexer[[1]]$coefficients[1,1]
#res_tot_pract[16,2,h] <-  indexer[[1]]$coefficients[2,1]
#res_tot_pract[16,3,h] <-  indexer[[2]]


################################# production ###########################
##### does the video increases production related outcomes?

#trimming is done on end result
dta_bal2 <- subset(dta_bal, prod_tot>0)
dta_bal2$log_prod_tot <- log(dta_bal2$prod_tot)
dta_trim <- trim("log_prod_tot", dta_bal2, .05)

### production

res_tot_prod[1,2,h] <- coefficients(ivreg(as.formula(paste(paste(paste(paste("log_prod_tot",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_prod[1,3,h] <- ifelse(totrep >0, RI_ivreg_ivr("log_prod_tot",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("log_prod_tot",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

### area

dta_bal2 <- subset(dta_bal, area_tot>0)
dta_bal2$log_area_tot <- log(dta_bal2$area_tot)

dta_trim <- trim("log_area_tot", dta_bal2, .05)


res_tot_prod[2,2,h] <-coefficients(ivreg(as.formula(paste(paste(paste(paste("log_area_tot",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_prod[2,3,h] <- ifelse(totrep >0, RI_ivreg_ivr("log_area_tot",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("log_area_tot",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

###yield

dta_bal2 <- subset(dta_bal, yield_av >0)
dta_bal2$log_yield_av <- log(dta_bal2$yield_av)
dta_trim <- trim("log_yield_av", dta_bal2, .05)


res_tot_prod[3,2,h] <- coefficients(ivreg(as.formula(paste(paste(paste(paste("log_yield_av",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_trim))[2]
res_tot_prod[3,3,h] <- ifelse(totrep >0, RI_ivreg_ivr("log_yield_av",ctrl_vars , dta_treim, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("log_yield_av",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_trim)))[2,4])

### was yield better compared to normal year?

res_tot_prod[4,2,h] <- coefficients(ivreg(as.formula(paste(paste(paste(paste("yield_better",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_prod[4,3,h] <- ifelse(totrep >0, RI_ivreg_ivr("yield_better",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("yield_better",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

dta_bal2 <- subset(dta_bal, area_tot >0 & prod_tot>0 & yield_av >0)
dta_bal2$log_prod_tot <- log(dta_bal2$prod_tot)
dta_bal2$log_area_tot <- log(dta_bal2$area_tot)
dta_bal2$log_yield_av <- log(dta_bal2$yield_av)

#dta_bal2 <- trim("log_prod_tot", dta_bal2, .1)
#dta_bal2 <- trim("log_area_tot", dta_bal2, .1)
dta_bal2 <- trim("log_yield_av", dta_bal2, .05)


#res_tot_prod[1:4,4,h] <- FSR_OLS( c("log_prod_tot", "log_area_tot", "log_yield_av","yield_better") ,treatment,dta_bal, nr_repl = totrep)[[4]]

#if (totrep >0) {
#dta_bal2$log_area_tot <- -dta_bal2$log_area_tot

#	indexer <- FW_index(treatment, c("log_prod_tot", "log_area_tot", "yield_better"),dta_bal2, nr_repl=totrep)
#	res_tot_prod[5,1,h] <-  indexer[[1]]$coefficients[1,1]
#	res_tot_prod[5,2,h] <-  indexer[[1]]$coefficients[2,1]
#	res_tot_prod[5,3,h] <-  indexer[[2]]

################################## disposal ##########################
#### maize consumed


res_tot_disp[1,2,h] <- coefficients(ivreg(as.formula(paste(paste(paste(paste("cons_maize_yes",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_disp[1,3,h] <- ifelse(totrep >0, RI_ivreg_ivr("cons_maize_yes",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("cons_maize_yes",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

### sold maize?


res_tot_disp[2,2,h] <- coefficients(ivreg(as.formula(paste(paste(paste(paste("sold_maize",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_disp[2,3,h] <- ifelse(totrep >0, RI_ivreg_ivr("sold_maize",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("sold_maize",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])
### kept maize for seed?


res_tot_disp[3,2,h] <- coefficients(ivreg(as.formula(paste(paste(paste(paste("save_seed",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_disp[3,3,h] <- ifelse(totrep >0, RI_ivreg_ivr("save_seed",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("save_seed",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

##res_tot_disp[1:3,4,h] <- FSR_OLS(c("cons_maize_yes", "sold_maize", "save_seed"),treatment,dta_bal, nr_repl = totrep)[[4]]
#dta_bal2 <- dta_bal
#dta_bal2$save_seed <- !dta_bal2$save_seed

#	indexer <- FW_index(treatment, c("cons_maize_yes", "sold_maize", "save_seed"),dta_bal2, nr_repl=totrep)
#	res_tot_disp[4,1,h] <-  indexer[[1]]$coefficients[1,1]
#	res_tot_disp[4,2,h] <-  indexer[[1]]$coefficients[2,1]
#	res_tot_disp[4,3,h] <-  indexer[[2]]


################################ welfare #############################

## better off than average

res_tot_wel[1,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("better_av",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_wel[1,3,h]  <- ifelse(totrep >0, RI_ivreg_ivr("better_av",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("better_av",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])
## better off than 6mo

res_tot_wel[2,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("better_6m",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_wel[2,3,h]  <- ifelse(totrep >0, RI_ivreg_ivr("better_6m",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("better_6m",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])


res_tot_wel[3,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("eatpref",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_wel[3,3,h]  <- ifelse(totrep >0, RI_ivreg_ivr("eatpref",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("eatpref",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

res_tot_wel[4,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("eatenough",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_wel[4,3,h]  <- ifelse(totrep >0, RI_ivreg_ivr("eatenough",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("eatenough",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

#consumption - logged and trimmed
dta_bal2 <- subset(dta_bal, cons>0)
dta_bal2$log_cons <- log(dta_bal2$cons)
dta_trim <- trim("log_cons", dta_bal2, .05)


res_tot_wel[5,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("log_cons",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_wel[5,3,h]  <- ifelse(totrep >0, RI_ivreg_ivr("log_cons",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("log_cons",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])


#	#res_tot_wel[1:5,4,h]   <- FSR_OLS(c("better_av", "better_6m", "eatpref","eatenough","log_cons"),treatment,dta_bal, nr_repl = totrep)[[4]]
#	indexer <- FW_index(treatment, c("better_av", "better_6m", "eatpref","eatenough","log_cons"),dta_trim, nr_repl=totrep)
#	res_tot_wel[6,1,h] <-  indexer[[1]]$coefficients[1,1]
#	res_tot_wel[6,2,h] <-  indexer[[1]]$coefficients[2,1]
#	res_tot_wel[6,3,h] <-  indexer[[2]]

################################################## is there and additional sms effect ###################################################
h <- 3
dta <- subset(dta, ivr == "yes")
treatment <- "(totsms > 0)"
ctrl_vars <- "as.factor(recipient) + as.factor(messenger)" 
instrument <- "(sms == 'yes')"
############################### knowledge  ############################
dta_bal <- dta

res_tot_know[1,2,h] <- coefficients(ivreg(as.formula(paste(paste(paste(paste("know_space",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_know[1,3,h] <- ifelse(totrep >0, RI_ivreg_sms("know_space",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("know_space",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

res_tot_know[2,2,h] <-coefficients(ivreg(as.formula(paste(paste(paste(paste("know_combine",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_know[2,3,h] <- ifelse(totrep >0, RI_ivreg_sms("know_combine",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("know_combine",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])


res_tot_know[3,2,h] <- coefficients(ivreg(as.formula(paste(paste(paste(paste("know_weed",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_know[3,3,h] <-  ifelse(totrep >0, RI_ivreg_sms("know_weed",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("know_weed",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

res_tot_know[4,2,h] <- coefficients(ivreg(as.formula(paste(paste(paste(paste("know_armyworm",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_know[4,3,h] <-  ifelse(totrep >0, RI_ivreg_sms("know_armyworm",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("know_armyworm",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

## no need to include armyworm in the index because we do not really expect an effect - but even when we include the effect is sig, so keep it in
#indexer <- FW_index(treatment, c("know_space", "know_combine", "know_weed","know_armyworm"),dta_bal, nr_repl=totrep)
#res_h0_know[5,1,h] <-  indexer[[1]]$coefficients[1,1]
#res_h0_know[5,2,h] <-  indexer[[1]]$coefficients[2,1]
#res_h0_know[5,3,h] <-  indexer[[2]]

###if (wyfs_stat) {
###	res_h0_know[1:4,4,h] <- FSR_RI_ivr( c("know_space","know_combine","know_weed", "know_armyworm") ,treatment ,dta_bal, pvals = res_h0_know[1:4,3,h], nr_repl_RI_ivr = 100)[[4]]
###	} else { 
###if (totrep >0) {
###	res_h0_know[1:4,4,h] <- FSR_OLS( c("know_space","know_combine","know_weed","know_armyworm") ,treatment,dta_bal, nr_repl = totrep)[[4]]
###}
###}

################################# practices #############################
#### used recommended spacing use on at lease one plot as reported by at least one spouse


res_tot_pract[1,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("space",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_pract[1,3,h]  <- ifelse(totrep >0, RI_ivreg_sms("space",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("space",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

## used recommended way to fight striga - this should be changed to include info of all plots 
res_tot_pract[2,2,h]  <-  coefficients(ivreg(as.formula(paste(paste(paste(paste("striga",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_pract[2,3,h]  <- ifelse(totrep >0, RI_ivreg_sms("striga",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("striga",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

## weeded on recommended timing? - this should be changed to include info of all plots 
res_tot_pract[3,2,h]  <-coefficients(ivreg(as.formula(paste(paste(paste(paste("weed",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]

res_tot_pract[3,3,h]  <-ifelse(totrep >0, RI_ivreg_sms("weed",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("weed",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4]) 

## fertilizer use
res_tot_pract[4,2,h]  <-coefficients(ivreg(as.formula(paste(paste(paste(paste("fert",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]

res_tot_pract[4,3,h]  <- ifelse(totrep >0, RI_ivreg_sms("fert",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("fert",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])
#### fert = DAP/NPK
res_tot_pract[5,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("fert_dap",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]

res_tot_pract[5,3,h]  <- ifelse(totrep >0, RI_ivreg_sms("fert_dap",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("fert_dap",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

#### fert = urea

res_tot_pract[6,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("fert_urea",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_pract[6,3,h]  <- ifelse(totrep >0, RI_ivreg_sms("fert_urea",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("fert_urea",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

#### fert = organic

res_tot_pract[7,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("fert_org",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_pract[7,3,h]  <- ifelse(totrep >0, RI_ivreg_sms("fert_org",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("fert_org",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

##improved seed  

res_tot_pract[8,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("impseed",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_pract[8,3,h]  <- ifelse(totrep >0, RI_ivreg_sms("impseed",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("impseed",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])
## hybrid

res_tot_pract[9,2,h] <- coefficients(ivreg(as.formula(paste(paste(paste(paste("hybrid",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_pract[9,3,h] <- ifelse(totrep >0, RI_ivreg_sms("hybrid",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("hybrid",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

## opv

res_tot_pract[10,2,h] <- coefficients(ivreg(as.formula(paste(paste(paste(paste("opv",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_pract[10,3,h] <- ifelse(totrep >0, RI_ivreg_sms("opv",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("opv",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

##combiner

res_tot_pract[11,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("combiner",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_pract[11,3,h]  <- ifelse(totrep >0, RI_ivreg_sms("combiner",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("combiner",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

### bought seed

res_tot_pract[12,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("bought_seed",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_pract[12,3,h]  <- ifelse(totrep >0, RI_ivreg_sms("bought_seed",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("bought_seed",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])
#### used chemicals

res_tot_pract[13,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("chem",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_pract[13,3,h]  <- ifelse(totrep >0, RI_ivreg_sms("chem",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("chem",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

###hired labour

res_tot_pract[14,2,h]  <- scoefficients(ivreg(as.formula(paste(paste(paste(paste("labour",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_pract[14,3,h]  <- ifelse(totrep >0, RI_ivreg_sms("labour",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("labour",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

###timely planting

res_tot_pract[15,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("day_one",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_pract[15,3,h]  <- ifelse(totrep >0, RI_ivreg_sms("day_one",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("day_one",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

#if (totrep >0) {
#res_tot_pract[1:7,4,h] <- FSR_RI_ivr( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment ,dta_bal, pvals =  res_tot_pract[,3,h] , nr_repl_pi = 100)

#res_tot_pract[1:7,4,h] <- FSR_OLS( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment,dta_bal, nr_repl = totrep)[[4]]

#indexer <-  FW_index(treatment,c("space","striga","weed", "fert","impseed", "combiner","bought_seed","chem","labour","day_one"),dta_bal, nr_repl=totrep)
#res_tot_pract[16,1,h] <-  indexer[[1]]$coefficients[1,1]
#res_tot_pract[16,2,h] <-  indexer[[1]]$coefficients[2,1]
#res_tot_pract[16,3,h] <-  indexer[[2]]


################################# production ###########################
##### does the video increases production related outcomes?

#trimming is done on end result
dta_bal2 <- subset(dta_bal, prod_tot>0)
dta_bal2$log_prod_tot <- log(dta_bal2$prod_tot)
dta_trim <- trim("log_prod_tot", dta_bal2, .05)

### production

res_tot_prod[1,2,h] <- coefficients(ivreg(as.formula(paste(paste(paste(paste("log_prod_tot",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_prod[1,3,h] <- ifelse(totrep >0, RI_ivreg_sms("log_prod_tot",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("log_prod_tot",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

### area

dta_bal2 <- subset(dta_bal, area_tot>0)
dta_bal2$log_area_tot <- log(dta_bal2$area_tot)

dta_trim <- trim("log_area_tot", dta_bal2, .05)


res_tot_prod[2,2,h] <-coefficients(ivreg(as.formula(paste(paste(paste(paste("log_area_tot",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_prod[2,3,h] <- ifelse(totrep >0, RI_ivreg_sms("log_area_tot",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("log_area_tot",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

###yield

dta_bal2 <- subset(dta_bal, yield_av >0)
dta_bal2$log_yield_av <- log(dta_bal2$yield_av)
dta_trim <- trim("log_yield_av", dta_bal2, .05)


res_tot_prod[3,2,h] <- coefficients(ivreg(as.formula(paste(paste(paste(paste("log_yield_av",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_prod[3,3,h] <- ifelse(totrep >0, RI_ivreg_sms("log_yield_av",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("log_yield_av",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

### was yield better compared to normal year?

res_tot_prod[4,2,h] <- coefficients(ivreg(as.formula(paste(paste(paste(paste("yield_better",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_prod[4,3,h] <- ifelse(totrep >0, RI_ivreg_sms("yield_better",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("yield_better",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

dta_bal2 <- subset(dta_bal, area_tot >0 & prod_tot>0 & yield_av >0)
dta_bal2$log_prod_tot <- log(dta_bal2$prod_tot)
dta_bal2$log_area_tot <- log(dta_bal2$area_tot)
dta_bal2$log_yield_av <- log(dta_bal2$yield_av)

#dta_bal2 <- trim("log_prod_tot", dta_bal2, .1)
#dta_bal2 <- trim("log_area_tot", dta_bal2, .1)
dta_bal2 <- trim("log_yield_av", dta_bal2, .05)


#res_tot_prod[1:4,4,h] <- FSR_OLS( c("log_prod_tot", "log_area_tot", "log_yield_av","yield_better") ,treatment,dta_bal, nr_repl = totrep)[[4]]

#if (totrep >0) {
#dta_bal2$log_area_tot <- -dta_bal2$log_area_tot

#	indexer <- FW_index(treatment, c("log_prod_tot", "log_area_tot", "yield_better"),dta_bal2, nr_repl=totrep)
#	res_tot_prod[5,1,h] <-  indexer[[1]]$coefficients[1,1]
#	res_tot_prod[5,2,h] <-  indexer[[1]]$coefficients[2,1]
#	res_tot_prod[5,3,h] <-  indexer[[2]]

################################## disposal ##########################
#### maize consumed


res_tot_disp[1,2,h] <- coefficients(ivreg(as.formula(paste(paste(paste(paste("cons_maize_yes",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_disp[1,3,h] <- ifelse(totrep >0, RI_ivreg_sms("cons_maize_yes",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("cons_maize_yes",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

### sold maize?


res_tot_disp[2,2,h] <- coefficients(ivreg(as.formula(paste(paste(paste(paste("sold_maize",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_disp[2,3,h] <- ifelse(totrep >0, RI_ivreg_sms("sold_maize",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("sold_maize",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])
### kept maize for seed?


res_tot_disp[3,2,h] <- coefficients(ivreg(as.formula(paste(paste(paste(paste("save_seed",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_disp[3,3,h] <- ifelse(totrep >0, RI_ivreg_sms("save_seed",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("save_seed",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

##res_tot_disp[1:3,4,h] <- FSR_OLS(c("cons_maize_yes", "sold_maize", "save_seed"),treatment,dta_bal, nr_repl = totrep)[[4]]
#dta_bal2 <- dta_bal
#dta_bal2$save_seed <- !dta_bal2$save_seed

#	indexer <- FW_index(treatment, c("cons_maize_yes", "sold_maize", "save_seed"),dta_bal2, nr_repl=totrep)
#	res_tot_disp[4,1,h] <-  indexer[[1]]$coefficients[1,1]
#	res_tot_disp[4,2,h] <-  indexer[[1]]$coefficients[2,1]
#	res_tot_disp[4,3,h] <-  indexer[[2]]


################################ welfare #############################

## better off than average

res_tot_wel[1,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("better_av",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_wel[1,3,h]  <- ifelse(totrep >0, RI_ivreg_sms("better_av",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("better_av",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])
## better off than 6mo

res_tot_wel[2,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("better_6m",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_wel[2,3,h]  <- ifelse(totrep >0, RI_ivreg_sms("better_6m",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("better_6m",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])


res_tot_wel[3,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("eatpref",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_wel[3,3,h]  <- ifelse(totrep >0, RI_ivreg_sms("eatpref",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("eatpref",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

res_tot_wel[4,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("eatenough",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_wel[4,3,h]  <- ifelse(totrep >0, RI_ivreg_sms("eatenough",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("eatenough",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])

#consumption - logged and trimmed
dta_bal2 <- subset(dta_bal, cons>0)
dta_bal2$log_cons <- log(dta_bal2$cons)
dta_trim <- trim("log_cons", dta_bal2, .05)


res_tot_wel[5,2,h]  <- coefficients(ivreg(as.formula(paste(paste(paste(paste("log_cons",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal))[2]
res_tot_wel[5,3,h]  <- ifelse(totrep >0, RI_ivreg_sms("log_cons",ctrl_vars , dta_bal, nr_repl = totrep),coef(summary(ivreg(as.formula(paste(paste(paste(paste("log_cons",treatment, sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta_bal)))[2,4])


#	#res_tot_wel[1:5,4,h]   <- FSR_OLS(c("better_av", "better_6m", "eatpref","eatenough","log_cons"),treatment,dta_bal, nr_repl = totrep)[[4]]
#	indexer <- FW_index(treatment, c("better_av", "better_6m", "eatpref","eatenough","log_cons"),dta_trim, nr_repl=totrep)
#	res_tot_wel[6,1,h] <-  indexer[[1]]$coefficients[1,1]
#	res_tot_wel[6,2,h] <-  indexer[[1]]$coefficients[2,1]
#	res_tot_wel[6,3,h] <-  indexer[[2]]



####################################### TOT
### check in baselin data if hh owns a phone or if it has access to a phone
library(foreign)
mobile <- read.dta("/home/bjvca/data/projects/digital green/baseline/DLEC.dta")[c("hhid","maizemobile","maizemobile_access")]
mobile$maizemobile <- mobile$maizemobile == "Yes"
mobile$maizemobile_access <- mobile$maizemobile_access == "Yes"
mobile$maizemobile_access[mobile$maizemobile == TRUE] <- TRUE 

dta <- merge(dta,mobile)

##redo entire analysis, but with only those who have access to phone for ivr and those who own a mobile phone for sms

totrep <- 0
### better to loop over h: 
for (h in seq(1,3,1)) {
if (h == 1) {
dta_bal <- dta
treatment <- "(messenger != 'ctrl')+ivr+sms+as.factor(recipient) + as.factor(messenger) + femhead" 
} else if ( h==2 ) {
dta_bal <- subset(dta,maizemobile_access == TRUE)
treatment <- "ivr+(messenger != 'ctrl')+sms+as.factor(recipient) + as.factor(messenger) + femhead" 
} else if (h==3) {
dta_bal <- subset(dta,maizemobile == TRUE)
treatment <- "sms+(messenger != 'ctrl')+ivr+as.factor(recipient) + as.factor(messenger) + femhead" 
}
############################### knowledge  ############################
##no need to balance data here because we control for both messenger and recipient factor levels



res_itt_know[1,1,h] <- mean(dta_bal$know_space[dta_bal$messenger == "ctrl"],na.rm=T)
res_itt_know[2,1,h] <- sd(dta_bal$know_space[dta_bal$messenger == "ctrl"],na.rm=T)
res_itt_know[1,2,h] <- summary(lm(as.formula(paste("know_space",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_itt_know[2,2,h] <- summary(lm(as.formula(paste("know_space",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
res_itt_know[1,3,h] <- ifelse(totrep >0, RI("know_space", treatment , dta_bal,  totrep,h),summary(lm(as.formula(paste("know_space",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

res_itt_know[3,1,h] <-  mean(dta_bal$know_combine[dta_bal$messenger == "ctrl"],na.rm=T)
res_itt_know[4,1,h] <-  sd(dta_bal$know_combine[dta_bal$messenger == "ctrl"],na.rm=T)
res_itt_know[3,2,h] <- summary(lm(as.formula(paste("know_combine",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_itt_know[4,2,h] <- summary(lm(as.formula(paste("know_combine",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
res_itt_know[3,3,h] <-  ifelse(totrep >0, RI("know_combine",treatment , dta_bal, nr_repl = totrep,h),summary(lm(as.formula(paste("know_combine",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

res_itt_know[5,1,h] <-  mean(dta_bal$know_weed[dta_bal$messenger == "ctrl"],na.rm=T)
res_itt_know[6,1,h] <-  sd(dta_bal$know_weed[dta_bal$messenger == "ctrl"],na.rm=T)
res_itt_know[5,2,h] <- summary(lm(as.formula(paste("know_weed",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_itt_know[6,2,h] <- summary(lm(as.formula(paste("know_weed",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
res_itt_know[5,3,h] <-  ifelse(totrep >0, RI("know_weed",treatment , dta_bal, nr_repl = totrep,h),summary(lm(as.formula(paste("know_weed",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

res_itt_know[7,1,h] <- mean(dta_bal$know_armyworm[dta_bal$messenger == "ctrl"],na.rm=T)
res_itt_know[8,1,h] <- sd(dta_bal$know_armyworm[dta_bal$messenger == "ctrl"],na.rm=T)
res_itt_know[7,2,h] <- summary(lm(as.formula(paste("know_armyworm",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_itt_know[8,2,h] <- summary(lm(as.formula(paste("know_armyworm",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
res_itt_know[7,3,h] <-  ifelse(totrep >0, RI("know_armyworm",treatment , dta_bal, nr_repl = totrep,h),summary(lm(as.formula(paste("know_armyworm",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

## no need to include armyworm in the index because we do not really expect an effect
indexer <- FW_index(treatment, c("know_space", "know_combine", "know_weed"),dta_bal, nr_repl=totrep,h)
res_itt_know[9,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"],na.rm=T)
res_itt_know[10,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"],na.rm=T)
res_itt_know[9,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_know[10,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_know[9,3,h] <-  indexer[[2]]

prod_plot[1,1] <- "knowledge"
prod_plot[1,3:4] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
prod_plot[1,2] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
##calculated corrected critical values
print(h)
RI_FWER(deps= c("know_space","know_combine","know_weed") ,indep = treatment ,dta =dta_bal, p_vals = res_itt_know[c(1,3,5),3,h], nr_repl = totrep,h_int=h)

################################# practices #############################
###timely planting
res_itt_pract[1,1,h]  <- mean(dta_bal$day_one[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[2,1,h]  <- sd(dta_bal$day_one[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[1,2,h]  <- summary(lm(as.formula(paste("day_one", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[2,2,h]  <- summary(lm(as.formula(paste("day_one", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[1,3,h]  <- ifelse(totrep >0, RI("day_one",treatment , dta_bal, nr_repl = totrep,h), summary(lm(as.formula(paste("day_one", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

#### used recommended spacing use on at lease one plot as reported by at least one spouse
res_itt_pract[3,1,h]  <- mean(dta_bal$space[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[4,1,h]  <- sd(dta_bal$space[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[3,2,h]  <- summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[4,2,h]  <- summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[3,3,h]  <- ifelse(totrep >0, RI("space",treatment, dta_bal, nr_repl = totrep,h),  summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## used recommended way to fight striga - this should be changed to include info of all plots 
res_itt_pract[5,1,h]  <- mean(dta_bal$striga[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[6,1,h]  <- sd(dta_bal$striga[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[5,2,h]  <- summary(lm(as.formula(paste("striga", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[6,2,h]  <- summary(lm(as.formula(paste("striga", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[5,3,h]  <- ifelse(totrep >0, RI("striga",treatment , dta_bal, nr_repl = totrep,h), summary(lm(as.formula(paste("striga", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## weeded on recommended timing? - this should be changed to include info of all plots 
res_itt_pract[7,1,h]  <- mean(dta_bal$weed[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[8,1,h]  <- sd(dta_bal$weed[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[7,2,h]  <- summary(lm(as.formula(paste("weed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[8,2,h]  <- summary(lm(as.formula(paste("weed", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[7,3,h]  <- ifelse(totrep >0, RI("weed",treatment, dta_bal, nr_repl = totrep,h),summary(lm(as.formula(paste("weed", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## fertilizer use
res_itt_pract[9,1,h]  <-  mean(dta_bal$fert[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[10,1,h]  <-  sd(dta_bal$fert[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[9,2,h]  <- summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[10,2,h]  <- summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[9,3,h]  <- ifelse(totrep >0, RI("fert",treatment , dta_bal, nr_repl = totrep,h), summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

#fert_plot[1,1] <- "all fertilizer"
#fert_plot[1,3:4] <- confint(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$fert[dta_bal$messenger == "ctrl"], na.rm=T)
#fert_plot[1,2] <-  summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$fert[dta_bal$messenger == "ctrl"], na.rm=T)

#### fert = DAP/NPK
res_itt_fert[1,1,h]  <-  mean(dta_bal$fert_dap[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_fert[2,1,h]  <-  sd(dta_bal$fert_dap[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_fert[1,2,h]  <- summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_fert[2,2,h]  <- summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_fert[1,3,h]  <- ifelse(totrep >0, RI("fert_dap",treatment , dta_bal, nr_repl = totrep,h) , summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

#fert_plot[2,1] <- "DAP/NPK"
#fert_plot[2,3:4] <- confint(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$fert_dap[dta_bal$messenger == "ctrl"], na.rm=T)
#fert_plot[2,2] <-  summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$fert_dap[dta_bal$messenger == "ctrl"], na.rm=T)

#### fert = urea
res_itt_fert[3,1,h]  <-  mean(dta_bal$fert_urea[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_fert[4,1,h]  <-  sd(dta_bal$fert_urea[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_fert[3,2,h]  <- summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_fert[4,2,h]  <- summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_fert[3,3,h]  <- ifelse(totrep >0, RI("fert_urea",treatment , dta_bal, nr_repl = totrep,h) , summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

#fert_plot[3,1] <- "urea"
#fert_plot[3,3:4] <- confint(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$fert_urea[dta_bal$messenger == "ctrl"], na.rm=T)
#fert_plot[3,2] <-  summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$fert_urea[dta_bal$messenger == "ctrl"], na.rm=T)

#### fert = organic
res_itt_fert[5,1,h]  <-  mean(dta_bal$fert_org[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_fert[6,1,h]  <-  sd(dta_bal$fert_org[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_fert[5,2,h]  <-  summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_fert[6,2,h]  <- summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_fert[5,3,h]  <- ifelse(totrep >0, RI("fert_org",treatment , dta_bal, nr_repl = totrep,h), summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

#fert_plot[4,1] <- "organic"
#fert_plot[4,3:4] <- confint(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$fert_org[dta_bal$messenger == "ctrl"], na.rm=T)
#fert_plot[4,2] <-  summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$fert_org[dta_bal$messenger == "ctrl"], na.rm=T)

##improved seed  
res_itt_pract[11,1,h]  <-  mean(dta_bal$impseed[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[12,1,h]  <-  sd(dta_bal$impseed[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[11,2,h]  <- summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[12,2,h]  <- summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[11,3,h]  <- ifelse(totrep >0, RI("impseed",treatment , dta_bal, nr_repl = totrep,h),  summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

#seed_plot[1,1] <- "all seed"
#seed_plot[1,3:4] <- confint(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$impseed[dta_bal$messenger == "ctrl"], na.rm=T)
#seed_plot[1,2] <-  summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$impseed[dta_bal$messenger == "ctrl"], na.rm=T)



## hybrid
res_itt_seed[1,1,h]  <-  mean(dta_bal$hybrid[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_seed[2,1,h]  <-  sd(dta_bal$hybrid[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_seed[1,2,h] <- summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_seed[2,2,h] <- summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_seed[1,3,h] <- ifelse(totrep >0, RI("hybrid",treatment , dta_bal, nr_repl = totrep,h), summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
#seed_plot[2,1] <- "hybrid"
#seed_plot[2,3:4] <- confint(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$hybrid[dta_bal$messenger == "ctrl"], na.rm=T)
#seed_plot[2,2] <-  summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$hybrid[dta_bal$messenger == "ctrl"], na.rm=T)

## opv
res_itt_seed[3,1,h]  <-  mean(dta_bal$opv[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_seed[4,1,h]  <-  sd(dta_bal$opv[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_seed[3,2,h] <- summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_seed[4,2,h] <- summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_seed[3,3,h] <- ifelse(totrep >0, RI("opv",treatment , dta_bal, nr_repl = totrep,h), summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

#seed_plot[3,1] <- "open pollinated"
#seed_plot[3,3:4] <- confint(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$opv[dta_bal$messenger == "ctrl"], na.rm=T)
#seed_plot[3,2] <-  summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$opv[dta_bal$messenger == "ctrl"], na.rm=T)

##combiner
res_itt_pract[13,1,h]  <-  mean(dta_bal$combiner[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[14,1,h]  <-  sd(dta_bal$combiner[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[13,2,h]  <- summary(lm(as.formula(paste("combiner", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[14,2,h]  <- summary(lm(as.formula(paste("combiner", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[13,3,h]  <- ifelse(totrep >0, RI("combiner",treatment , dta_bal, nr_repl = totrep,h), summary(lm(as.formula(paste("combiner", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

### bought seed
res_itt_pract[15,1,h]  <-  mean(dta_bal$bought_seed[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[16,1,h]  <-  sd(dta_bal$bought_seed[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[15,2,h]  <- summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[16,2,h]  <- summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[15,3,h]  <- ifelse(totrep >0, RI("bought_seed",treatment , dta_bal, nr_repl = totrep,h), summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
#seed_plot[4,1] <- "bought seed"
#seed_plot[4,3:4] <- confint(lm(as.formula(paste("bought_seed", treatment, sep ="~"), level=.9), data=dta_bal))[2,] /  mean(dta_bal$bought_seed[dta_bal$messenger == "ctrl"], na.rm=T)
#seed_plot[4,2] <-  summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$bought_seed[dta_bal$messenger == "ctrl"], na.rm=T)


#### used chemicals
res_itt_pract[17,1,h]  <-  mean(dta_bal$chem[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[18,1,h]  <-  sd(dta_bal$chem[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[17,2,h]  <- summary(lm(as.formula(paste("chem", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[18,2,h]  <- summary(lm(as.formula(paste("chem", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[17,3,h]  <- ifelse(totrep >0, RI("chem",treatment , dta_bal, nr_repl = totrep,h), summary(lm(as.formula(paste("chem", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

###hired labour
res_itt_pract[19,1,h]  <-  mean(dta_bal$labour[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[20,1,h]  <-  sd(dta_bal$labour[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[19,2,h]  <- summary(lm(as.formula(paste("labour", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[20,2,h]  <- summary(lm(as.formula(paste("labour", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[19,3,h]  <- ifelse(totrep >0, RI("labour",treatment , dta_bal, nr_repl = totrep,h), summary(lm(as.formula(paste("labour", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

#if (totrep >0) {
#res_h0_pract[1:7,4,h] <- FSR_RI( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment ,dta_bal, pvals =  res_h0_pract[,3,h] , nr_repl_pi = 100)

#res_h0_pract[1:7,4,h] <- FSR_OLS( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment,dta_bal, nr_repl = totrep)[[4]]

indexer <-  FW_index(treatment,c("day_one","space","striga","weed", "fert","impseed"),dta_bal, nr_repl=totrep)
res_itt_pract[21,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
res_itt_pract[22,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
res_itt_pract[21,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_pract[22,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_pract[21,3,h] <-  indexer[[2]]

#prod_plot[2,1] <- "adoption"
#prod_plot[2,3:4] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
#prod_plot[2,2] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])


#res_itt_pract <- Bcorr( c("day_one","space","striga","weed", "fert","impseed", "combiner","bought_seed","chem","labour"),dta_bal, res_itt_pract ,h)
#res_itt_fert <- Bcorr(c("fert_dap","fert_urea","fert_org"),dta_bal, res_itt_fert ,h)
#res_itt_seed <- Bcorr(c("hybrid","opv"),dta_bal, res_itt_seed ,h)

RI_FWER(c("day_one","space","striga","weed", "fert","impseed"),indep = treatment,dta_bal, res_itt_know[c(1,3,5,7,9,11),3,h], totrep,h_int=h)

RI_FWER(c("fert_dap","fert_urea","fert_org"),indep = treatment,dta_bal, res_itt_fert[c(1,3,5),3,h],totrep,h_int=h)

RI_FWER(c("hybrid","opv"),indep = treatment,dta_bal, res_itt_seed[c(1,3),3,h],totrep,h_int=h)
}




