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
res_know <- array(NA, c(11,4,7)) 
rownames(res_know) <- c("know_space","","know_combine","","know_weed","", "know_armyworm","","know_ind","","padj")
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
mod <- lm(as.formula(paste("index",treat,sep="~")) ,weights=unlist(dta[w_int2]), data=data)

					
if (nr_repl > 0) { 
	data$index <- as.vector(data$index)
	sig <- RI("index" ,treat , contr_vars, w_int= w_int2, data, nr_repl = nr_repl,h_int)
} else {
	sig <- summary(lm(as.formula(paste("index",treat,sep="~")) ,weights=unlist(dta[w_int2]), data=data))$coefficients[2,4]
}
return(list(mod,sig, data))
}

## drop the control
dta <- subset(dta, messenger != "ctrl")

dta_copy <- dta
totrep <- 0
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

dta$weights <- 1

dta$weights[dta$recipient == "male"] <- 1038/1040

treatment <- "(recipient == 'couple') +ivr+sms+as.factor(messenger)" 
##uncomment to include controls for imbalance - use no spaces in ctrls
#dta <- merge(dta_copy,baseline, by="hhid")
#ctrls <- "yield+maizeage+maizeeduc+maizehh_no+maizeprinfo_receiv_spouse" 

} else if (h==2) {
############################################ H2: empower: rec=male vs rec=couple or woman #########################################################
dta <- dta_copy
#treatment <- "(recipient != 'male') +ivr+sms+ as.factor(messenger)" 
dta <- merge(dta_copy,baseline, by="hhid")
#treatment <- "(recipient == 'couple') +ivr+sms+as.factor(messenger)"
#ctrls <- "yield+maizeage+maizeeduc+maizeprinput_use" 
} else if (h==3) {
############################################ H3: empower a1: rec=male vs rec=female ###################################################
dta <- subset(dta_copy, recipient == "male" | recipient == "female")
treatment <- "(recipient == 'female') +ivr+sms+as.factor(messenger)" 
} else if (h==4) {
############################################ H4: empower a2: rec=male vs rec=couple ###################################################
dta <- subset(dta_copy, recipient == "male" | recipient == "couple")
treatment <- "(recipient == 'couple') +ivr+sms+ as.factor(messenger)"

##is this equal to just comparing female to couple???

} else if (h==5) {
############################################ H5: promote collective approach ###################################################
dta <- dta_copy
ctrls<- NULL
treatment <- "(messenger == 'couple') +ivr+sms+as.factor(recipient)"
} else if (h==6) {
############################################ H6: challenge gender stereotype ###################################################
dta <- dta_copy
ctrls<- NULL
treatment <- "(messenger != 'male') +ivr+sms+as.factor(recipient)"
} else if (h==7) {
############################################ H7: homophily ###################################################
dta <- subset(dta_copy, recipient != "couple" & messenger != "couple")
ctrls<- NULL
treatment <- "(messenger == recipient) +ivr+sms"
}
print(h)
############################### knowledge  ############################

res_know[1,1,h] <- ifelse(h ==1, wtd.mean(dta$know_space_j[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.mean(dta$know_space_j[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$know_space_j[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.mean(dta$know_space_j[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.mean(dta$know_space_j[dta$messenger != dta$recipient],weights[dta$messenger != dta$recipient], na.rm=T)))))
res_know[2,1,h] <- ifelse(h ==1, wtd.sd(dta$know_space_j[dta$recipient == "male" | dta$recipient == "female"], dta$sd[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.sd(dta$know_space_j[dta$recipient == "male"],dta$sd[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$know_space_j[dta$messenger == "male" | dta$messenger == "female"],dta$sd[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.sd(dta$know_space_j[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.sd(dta$know_space_j[dta$messenger != dta$recipient],weights[dta$messenger != dta$recipient], na.rm=T)))))
res_know[1,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_space_j",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("know_space_j",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_know[2,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_space_j",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("know_space_j",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_know[1,3,h] <- ifelse(totrep >0, RI("know_space_j",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_space_j",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("know_space_j",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_know[2,3,h] <- nobs(lm(as.formula(paste("know_space_j",treatment, sep="~")) ,data=dta))

res_know[3,1,h] <- ifelse(h ==1, wtd.mean(dta$know_combine_j[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.mean(dta$know_combine_j[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$know_combine_j[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.mean(dta$know_combine_j[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.mean(dta$know_combine_j[dta$messenger != dta$recipient],weights[dta$messenger != dta$recipient], na.rm=T)))))
res_know[4,1,h] <- ifelse(h ==1, wtd.sd(dta$know_combine_j[dta$recipient == "male" | dta$recipient == "female"], dta$sd[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.sd(dta$know_combine_j[dta$recipient == "male"],dta$sd[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$know_combine_j[dta$messenger == "male" | dta$messenger == "female"],dta$sd[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.sd(dta$know_combine_j[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.sd(dta$know_combine_j[dta$messenger != dta$recipient],weights[dta$messenger != dta$recipient], na.rm=T)))))
res_know[3,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_combine_j",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("know_combine_j",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_know[4,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_combine_j",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("know_combine_j",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_know[3,3,h] <- ifelse(totrep >0, RI("know_combine_j",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_combine_j",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("know_combine_j",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_know[4,3,h] <- nobs(lm(as.formula(paste("know_combine_j",treatment, sep="~")) ,data=dta))

res_know[5,1,h] <- ifelse(h ==1, wtd.mean(dta$know_weed_j[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.mean(dta$know_weed_j[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$know_weed_j[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.mean(dta$know_weed_j[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.mean(dta$know_weed_j[dta$messenger != dta$recipient],weights[dta$messenger != dta$recipient], na.rm=T)))))
res_know[6,1,h] <- ifelse(h ==1, wtd.sd(dta$know_weed_j[dta$recipient == "male" | dta$recipient == "female"], dta$sd[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.sd(dta$know_weed_j[dta$recipient == "male"],dta$sd[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$know_weed_j[dta$messenger == "male" | dta$messenger == "female"],dta$sd[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.sd(dta$know_weed_j[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.sd(dta$know_weed_j[dta$messenger != dta$recipient],weights[dta$messenger != dta$recipient], na.rm=T)))))
res_know[5,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_weed_j",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("know_weed_j",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_know[6,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_weed_j",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("know_weed_j",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_know[5,3,h] <- ifelse(totrep >0, RI("know_weed_j",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_weed_j",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("know_weed_j",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_know[6,3,h] <- nobs(lm(as.formula(paste("know_weed_j",treatment, sep="~")) ,data=dta))

res_know[7,1,h] <- ifelse(h ==1, wtd.mean(dta$know_armyworm_j[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.mean(dta$know_armyworm_j[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.mean(dta$know_armyworm_j[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.mean(dta$know_armyworm_j[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.mean(dta$know_armyworm_j[dta$messenger != dta$recipient],weights[dta$messenger != dta$recipient], na.rm=T)))))
res_know[8,1,h] <- ifelse(h ==1, wtd.sd(dta$know_armyworm_j[dta$recipient == "male" | dta$recipient == "female"], dta$sd[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.sd(dta$know_armyworm_j[dta$recipient == "male"],dta$sd[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.sd(dta$know_armyworm_j[dta$messenger == "male" | dta$messenger == "female"],dta$sd[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.sd(dta$know_armyworm_j[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.sd(dta$know_armyworm_j[dta$messenger != dta$recipient],weights[dta$messenger != dta$recipient], na.rm=T)))))
res_know[7,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_armyworm_j",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("know_armyworm_j",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_know[8,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_armyworm_j",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("know_armyworm_j",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_know[7,3,h] <- ifelse(totrep >0, RI("know_armyworm_j",treatment , ctrls,w_int="weights", dta, nr_repl = totrep, h),ifelse(is.null(ctrls),summary(lm(as.formula(paste("know_armyworm_j",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("know_armyworm_j",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_know[8,3,h] <- nobs(lm(as.formula(paste("know_armyworm_j",treatment, sep="~")) ,data=dta))

indexer <- FW_index(treatment, c("know_space_j", "know_combine_j", "know_weed_j"),ctrls,w_int="weights",dta, nr_repl=totrep,h_int = h)

res_know[9,1,h] <- ifelse(h ==1, wtd.mean(indexer[[3]]$index[dta$recipient == "male" | dta$recipient == "female"], dta$weights[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.mean(indexer[[3]]$index[dta$recipient == "male"],dta$weights[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.mean(indexer[[3]]$index[dta$messenger == "male" | dta$messenger == "female"],dta$weights[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.mean(indexer[[3]]$index[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.mean(indexer[[3]]$index[dta$messenger != dta$recipient],weights[dta$messenger != dta$recipient], na.rm=T)))))

res_know[10,1,h] <- ifelse(h ==1, wtd.sd(indexer[[3]]$index[dta$recipient == "male" | dta$recipient == "female"], dta$sd[dta$recipient == "male" | dta$recipient == "female"], na.rm=T),ifelse(h %in% c(2,3,4), wtd.sd(indexer[[3]]$index[dta$recipient == "male"],dta$sd[dta$recipient == "male"], na.rm=T), ifelse( h == 5, wtd.sd(indexer[[3]]$index[dta$messenger == "male" | dta$messenger == "female"],dta$sd[dta$messenger == "male" | dta$messenger == "female"], na.rm=T), ifelse(h == 6, wtd.sd(indexer[[3]]$index[dta$messenger == "male" ],dta$weights[dta$messenger == "male" ], na.rm=T), wtd.sd(indexer[[3]]$index[dta$messenger != dta$recipient],weights[dta$messenger != dta$recipient], na.rm=T)))))


res_know[9,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_know[10,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_know[9,3,h] <-  indexer[[2]]
res_know[10,3,h] <-  nobs(indexer[[1]])

plot_res[1,1,h] <- "knowledge"
plot_res[1,2,h] <- summary(indexer[[1]])$coefficients[2,1] / res_know[10,1,h]
plot_res[1,3:4,h] <- confint(indexer[[1]], level=.95)[2,]/res_know[10,1,h]
plot_res[1,5,h] <- "joint"

#res_know[11,1:3,h] <- RI_FWER(deps= c("know_space_j","know_combine_j","know_weed_j") ,indep = treatment , ctrls = ctrls,dta =dta, p_vals = res_know[c(1,3,5),3,h], nr_repl = totrep,h_int=h)
}
}
################################# practices #############################
###timely planting
res_itt_pract[1,1,h]  <- mean(dta$day_one[dta_bal$messenger == "ctrl"], na.rm=T)
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

fert_plot[1,1] <- "all fertilizer"
fert_plot[1,3:4] <- confint(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$fert[dta_bal$messenger == "ctrl"], na.rm=T)
fert_plot[1,2] <-  summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$fert[dta_bal$messenger == "ctrl"], na.rm=T)

#### fert = DAP/NPK
res_itt_fert[1,1,h]  <-  mean(dta_bal$fert_dap[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_fert[2,1,h]  <-  sd(dta_bal$fert_dap[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_fert[1,2,h]  <- summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_fert[2,2,h]  <- summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_fert[1,3,h]  <- ifelse(totrep >0, RI("fert_dap",treatment , dta_bal, nr_repl = totrep,h) , summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

fert_plot[2,1] <- "DAP/NPK"
fert_plot[2,3:4] <- confint(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$fert_dap[dta_bal$messenger == "ctrl"], na.rm=T)
fert_plot[2,2] <-  summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$fert_dap[dta_bal$messenger == "ctrl"], na.rm=T)

#### fert = urea
res_itt_fert[3,1,h]  <-  mean(dta_bal$fert_urea[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_fert[4,1,h]  <-  sd(dta_bal$fert_urea[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_fert[3,2,h]  <- summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_fert[4,2,h]  <- summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_fert[3,3,h]  <- ifelse(totrep >0, RI("fert_urea",treatment , dta_bal, nr_repl = totrep,h) , summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

fert_plot[3,1] <- "urea"
fert_plot[3,3:4] <- confint(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$fert_urea[dta_bal$messenger == "ctrl"], na.rm=T)
fert_plot[3,2] <-  summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$fert_urea[dta_bal$messenger == "ctrl"], na.rm=T)

#### fert = organic
res_itt_fert[5,1,h]  <-  mean(dta_bal$fert_org[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_fert[6,1,h]  <-  sd(dta_bal$fert_org[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_fert[5,2,h]  <-  summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_fert[6,2,h]  <- summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_fert[5,3,h]  <- ifelse(totrep >0, RI("fert_org",treatment , dta_bal, nr_repl = totrep,h), summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

fert_plot[4,1] <- "organic"
fert_plot[4,3:4] <- confint(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$fert_org[dta_bal$messenger == "ctrl"], na.rm=T)
fert_plot[4,2] <-  summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$fert_org[dta_bal$messenger == "ctrl"], na.rm=T)

##improved seed  
res_itt_pract[11,1,h]  <-  mean(dta_bal$impseed[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[12,1,h]  <-  sd(dta_bal$impseed[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[11,2,h]  <- summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[12,2,h]  <- summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[11,3,h]  <- ifelse(totrep >0, RI("impseed",treatment , dta_bal, nr_repl = totrep,h),  summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

seed_plot[1,1] <- "all seed"
seed_plot[1,3:4] <- confint(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$impseed[dta_bal$messenger == "ctrl"], na.rm=T)
seed_plot[1,2] <-  summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$impseed[dta_bal$messenger == "ctrl"], na.rm=T)



## hybrid
res_itt_seed[1,1,h]  <-  mean(dta_bal$hybrid[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_seed[2,1,h]  <-  sd(dta_bal$hybrid[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_seed[1,2,h] <- summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_seed[2,2,h] <- summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_seed[1,3,h] <- ifelse(totrep >0, RI("hybrid",treatment , dta_bal, nr_repl = totrep,h), summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
seed_plot[2,1] <- "hybrid"
seed_plot[2,3:4] <- confint(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$hybrid[dta_bal$messenger == "ctrl"], na.rm=T)
seed_plot[2,2] <-  summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$hybrid[dta_bal$messenger == "ctrl"], na.rm=T)

## opv
res_itt_seed[3,1,h]  <-  mean(dta_bal$opv[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_seed[4,1,h]  <-  sd(dta_bal$opv[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_seed[3,2,h] <- summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_seed[4,2,h] <- summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_seed[3,3,h] <- ifelse(totrep >0, RI("opv",treatment , dta_bal, nr_repl = totrep,h), summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

seed_plot[3,1] <- "open pollinated"
seed_plot[3,3:4] <- confint(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$opv[dta_bal$messenger == "ctrl"], na.rm=T)
seed_plot[3,2] <-  summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$opv[dta_bal$messenger == "ctrl"], na.rm=T)

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
seed_plot[4,1] <- "bought seed"
seed_plot[4,3:4] <- confint(lm(as.formula(paste("bought_seed", treatment, sep ="~"), level=.9), data=dta_bal))[2,] /  mean(dta_bal$bought_seed[dta_bal$messenger == "ctrl"], na.rm=T)
seed_plot[4,2] <-  summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$bought_seed[dta_bal$messenger == "ctrl"], na.rm=T)


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

prod_plot[2,1] <- "adoption"
prod_plot[2,3:4] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
prod_plot[2,2] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])


#res_itt_pract <- Bcorr( c("day_one","space","striga","weed", "fert","impseed", "combiner","bought_seed","chem","labour"),dta_bal, res_itt_pract ,h)
#res_itt_fert <- Bcorr(c("fert_dap","fert_urea","fert_org"),dta_bal, res_itt_fert ,h)
#res_itt_seed <- Bcorr(c("hybrid","opv"),dta_bal, res_itt_seed ,h)

#RI_FWER(c("day_one","space","striga","weed", "fert","impseed"),"(messenger != 'ctrl') +ivr+sms+as.factor(recipient)",dta_bal, res_itt_know[c(1,3,5,7,9,11),3,h], 10000,h_int=h)

#RI_FWER(c("fert_dap","fert_urea","fert_org"),"(messenger != 'ctrl') +ivr+sms+as.factor(recipient) ",dta_bal, res_itt_fert[c(1,3,5),3,h],10000,h_int=h)

#RI_FWER(c("hybrid","opv"),"(messenger != 'ctrl') +ivr+sms+as.factor(recipient)",dta_bal, res_itt_seed[c(1,3),3,h],10000,h_int=h)

}
