rm(list=ls())
source("/home/bjvca/data/projects/digital green/endline/data/init.R")
#source("functions.R")
#dta <- read.csv("AWS.csv")
#set totrep to zero if you do not want simulation based inferecne
totrep <- 0
library(ggplot2)
library(doParallel)
library(data.table)
library(dplyr)

set.seed(07032018)
dta <- subset(dta, !is.na(interview_status))
dta$messenger <- as.character(dta$messenger)

### indexing results arrays
res_itt_know <- array(NA, c(10,4,3)) 
rownames(res_itt_know) <- c("know_space","","know_combine","","know_weed","", "know_armyworm","","know_ind","")
res_itt_pract <- array(NA, c(22,4,3))
rownames(res_itt_pract) <- c("first_day","","space","","striga","","weed","", "use_fert","","seed","","combiner","","bought_seed","","chem","","labour","","pract_index","")
res_itt_fert <- array(NA, c(6,4,3))
rownames(res_itt_fert) <- c("use_DAP","","use_urea","","use_organic","")
res_itt_seed <- array(NA, c(4,4,3))
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

###@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### For the comparison between different ways of delivering information to a household, we only keep households where the video was also shown to the man (as generally he will be the main decision maker)
dta <- subset(dta, (recipient !="female" | messenger == "ctrl"))
### and so delivery mode needs its own RI function

RI <- function(dep, indep, dta , nr_repl = 1000) {
### determines treatmetn cell
	dta <- dta %>% 
    		mutate(treat = group_indices_(dta, .dots=c("recipient", "messenger"))) 
	### allocates unique ID based on treatment cell status and village
	dta <- dta %>% 
    		mutate(uniqID = group_indices_(dta, .dots=c("distID", "subID","vilID"))) 
	### the NULL
	crit <- summary(lm(as.formula(paste(dep,indep,sep="~")), data=dta))$coefficients[2,1]
	dta <-  data.table(cbind(dta[dep],dta[c("messenger","recipient","treat","uniqID","hhid","ivr","sms","called","totsms")]))

	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
		dta_sim <- data.table(dta)
 		setDT(dta_sim)[,perm:=sample(treat),by = (uniqID)]
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["female",]>0]),"female",ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["couple",]>0]),"couple", "ctrl")))
			return(abs(coef(lm(as.formula(paste(dep,indep,sep="~")), data=dta_sim))[2]) > abs(crit) )
	}
	return(sum(oper)/nr_repl)
}


RI_FWER <- function(deps, indep, dta ,p_vals , nr_repl = 1000) {
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
	dta <-  data.table(cbind(dta[deps],dta[c("messenger","recipient","treat","uniqID","hhid","ivr","sms","called","totsms")]))
	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
		dta_sim <- data.table(dta)
 		setDT(dta_sim)[,perm:=sample(treat),by = (uniqID)]
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["female",]>0]),"female",ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["couple",]>0]),"couple", "ctrl")))
return(unlist(lapply(deps, function(dvar) summary(lm(as.formula(paste(dvar,indep,sep="~")), data=dta_sim))$coefficients[2,4])))
				}

thresholds <- seq(0, 0.1, length.out = 10000)
type_I_rate <- sapply(thresholds, threshold_finder)
return( list=c(thresholds[max(which(type_I_rate <= 0.05))],thresholds[max(which(type_I_rate <= 0.01))], thresholds[max(which(type_I_rate <= 0.001))]))
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

FW_index <- function(treat, indexer, data,revcols = NULL, nr_repl=0) {
### function to make family wise index using covariance as weights (following http://cyrussamii.com/?p=2656)
### FW_index("messenger != 'ctrl' ", c("know_space", "know_combine", "know_weed"),dta)
data <- data[complete.cases(data[indexer]),]
x <- data[indexer]

				for(j in 1:ncol(x)){
					x[,j] <- (x[,j] - mean(x[,j]))/sd(x[,j])
				}
if(length(revcols)>0){
						x[,revcols] <-  -1*x[,revcols]
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


h <- 1
############################################ does the intervention work? (treat vs control) #########################################################

treatment <- "(messenger != 'ctrl')+ivr+sms+as.factor(recipient) + as.factor(messenger)" 
############################### knowledge  ############################
dta_bal <- dta

res_itt_know[1,1,h] <- mean(dta_bal$know_space[dta_bal$messenger == "ctrl"])
res_itt_know[2,1,h] <- sd(dta_bal$know_space[dta_bal$messenger == "ctrl"])
res_itt_know[1,2,h] <- summary(lm(as.formula(paste("know_space",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_itt_know[2,2,h] <- summary(lm(as.formula(paste("know_space",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
res_itt_know[1,3,h] <- ifelse(totrep >0, RI("know_space",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("know_space",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

res_itt_know[3,1,h] <-  mean(dta_bal$know_combine[dta_bal$messenger == "ctrl"])
res_itt_know[4,1,h] <-  sd(dta_bal$know_combine[dta_bal$messenger == "ctrl"])
res_itt_know[3,2,h] <- summary(lm(as.formula(paste("know_combine",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_itt_know[4,2,h] <- summary(lm(as.formula(paste("know_combine",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
res_itt_know[3,3,h] <-  ifelse(totrep >0, RI("know_combine",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("know_combine",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

res_itt_know[5,1,h] <-  mean(dta_bal$know_weed[dta_bal$messenger == "ctrl"])
res_itt_know[6,1,h] <-  sd(dta_bal$know_weed[dta_bal$messenger == "ctrl"])
res_itt_know[5,2,h] <- summary(lm(as.formula(paste("know_weed",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_itt_know[6,2,h] <- summary(lm(as.formula(paste("know_weed",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
res_itt_know[5,3,h] <-  ifelse(totrep >0, RI("know_weed",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("know_weed",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

res_itt_know[7,1,h] <- mean(dta_bal$know_armyworm[dta_bal$messenger == "ctrl"])
res_itt_know[8,1,h] <- sd(dta_bal$know_armyworm[dta_bal$messenger == "ctrl"])
res_itt_know[7,2,h] <- summary(lm(as.formula(paste("know_armyworm",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_itt_know[8,2,h] <- summary(lm(as.formula(paste("know_armyworm",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
res_itt_know[7,3,h] <-  ifelse(totrep >0, RI("know_armyworm",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("know_armyworm",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])
## no need to include armyworm in the index because we do not really expect an effect
indexer <- FW_index(treatment, c("know_space", "know_combine", "know_weed"),dta_bal, nr_repl=totrep)
res_itt_know[9,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
res_itt_know[10,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
res_itt_know[9,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_know[10,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_know[9,3,h] <-  indexer[[2]]

###if (wyfs_stat) {
###	res_h0_know[1:4,4,h] <- FSR_RI( c("know_space","know_combine","know_weed", "know_armyworm") ,treatment ,dta_bal, pvals = res_h0_know[1:4,3,h], nr_repl_ri = 100)[[4]]
###	} else { 
###if (totrep >0) {
###	res_h0_know[1:4,4,h] <- FSR_OLS( c("know_space","know_combine","know_weed","know_armyworm") ,treatment,dta_bal, nr_repl = totrep)[[4]]
###}
###}

prod_plot[1,1] <- "knowledge"
prod_plot[1,3:4] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
prod_plot[1,2] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])


#res_itt_know <- Bcorr(c("know_space", "know_combine", "know_weed","know_armyworm"),dta_bal, res_itt_know ,h)
#RI_FWER(c("know_space","know_combine","know_weed", "know_armyworm"),"(messenger != 'ctrl') +ivr+sms+as.factor(recipient)",dta, c(0.0001,0.0056,0.3215,0.2116), 10000)

################################# practices #############################
###timely planting
res_itt_pract[1,1,h]  <- mean(dta_bal$day_one[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[2,1,h]  <- sd(dta_bal$day_one[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[1,2,h]  <- summary(lm(as.formula(paste("day_one", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[2,2,h]  <- summary(lm(as.formula(paste("day_one", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[1,3,h]  <- ifelse(totrep >0, RI("day_one",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("day_one", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

#### used recommended spacing use on at lease one plot as reported by at least one spouse
res_itt_pract[3,1,h]  <- mean(dta_bal$space[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[4,1,h]  <- sd(dta_bal$space[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[3,2,h]  <- summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[4,2,h]  <- summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[3,3,h]  <- ifelse(totrep >0, RI("space",treatment, dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## used recommended way to fight striga - this should be changed to include info of all plots 
res_itt_pract[5,1,h]  <- mean(dta_bal$striga[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[6,1,h]  <- sd(dta_bal$striga[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[5,2,h]  <- summary(lm(as.formula(paste("striga", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[6,2,h]  <- summary(lm(as.formula(paste("striga", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[5,3,h]  <- ifelse(totrep >0, RI("striga",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("striga", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## weeded on recommended timing? - this should be changed to include info of all plots 
res_itt_pract[7,1,h]  <- mean(dta_bal$weed[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[8,1,h]  <- sd(dta_bal$weed[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[7,2,h]  <- summary(lm(as.formula(paste("weed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[8,2,h]  <- summary(lm(as.formula(paste("weed", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[7,3,h]  <- ifelse(totrep >0, RI("weed",treatment, dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("weed", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## fertilizer use
res_itt_pract[9,1,h]  <-  mean(dta_bal$fert[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[10,1,h]  <-  sd(dta_bal$fert[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[9,2,h]  <- summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[10,2,h]  <- summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[9,3,h]  <- ifelse(totrep >0, RI("fert",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

fert_plot[1,1] <- "all fertilizer"
fert_plot[1,3:4] <- confint(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$fert[dta_bal$messenger == "ctrl"], na.rm=T)
fert_plot[1,2] <-  summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$fert[dta_bal$messenger == "ctrl"], na.rm=T)

#### fert = DAP/NPK
res_itt_fert[1,1,h]  <-  mean(dta_bal$fert_dap[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_fert[2,1,h]  <-  sd(dta_bal$fert_dap[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_fert[1,2,h]  <- summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_fert[2,2,h]  <- summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_fert[1,3,h]  <- ifelse(totrep >0, RI("fert_dap",treatment , dta_bal, nr_repl = totrep) , summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

fert_plot[2,1] <- "DAP/NPK"
fert_plot[2,3:4] <- confint(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$fert_dap[dta_bal$messenger == "ctrl"], na.rm=T)
fert_plot[2,2] <-  summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$fert_dap[dta_bal$messenger == "ctrl"], na.rm=T)

#### fert = urea
res_itt_fert[3,1,h]  <-  mean(dta_bal$fert_urea[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_fert[4,1,h]  <-  sd(dta_bal$fert_urea[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_fert[3,2,h]  <- summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_fert[4,2,h]  <- summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_fert[3,3,h]  <- ifelse(totrep >0, RI("fert_urea",treatment , dta_bal, nr_repl = totrep) , summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

fert_plot[3,1] <- "urea"
fert_plot[3,3:4] <- confint(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$fert_urea[dta_bal$messenger == "ctrl"], na.rm=T)
fert_plot[3,2] <-  summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$fert_urea[dta_bal$messenger == "ctrl"], na.rm=T)

#### fert = organic
res_itt_fert[5,1,h]  <-  mean(dta_bal$fert_org[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_fert[6,1,h]  <-  sd(dta_bal$fert_org[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_fert[5,2,h]  <-  summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_fert[6,2,h]  <- summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_fert[5,3,h]  <- ifelse(totrep >0, RI("fert_org",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

fert_plot[4,1] <- "organic"
fert_plot[4,3:4] <- confint(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$fert_org[dta_bal$messenger == "ctrl"], na.rm=T)
fert_plot[4,2] <-  summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$fert_org[dta_bal$messenger == "ctrl"], na.rm=T)

##improved seed  
res_itt_pract[11,1,h]  <-  mean(dta_bal$impseed[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[12,1,h]  <-  sd(dta_bal$impseed[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[11,2,h]  <- summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[12,2,h]  <- summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[11,3,h]  <- ifelse(totrep >0, RI("impseed",treatment , dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

seed_plot[1,1] <- "all seed"
seed_plot[1,3:4] <- confint(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$impseed[dta_bal$messenger == "ctrl"], na.rm=T)
seed_plot[1,2] <-  summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$impseed[dta_bal$messenger == "ctrl"], na.rm=T)



## hybrid
res_itt_seed[1,1,h]  <-  mean(dta_bal$hybrid[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_seed[2,1,h]  <-  sd(dta_bal$hybrid[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_seed[1,2,h] <- summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_seed[2,2,h] <- summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_seed[1,3,h] <- ifelse(totrep >0, RI("hybrid",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
seed_plot[2,1] <- "hybrid"
seed_plot[2,3:4] <- confint(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$hybrid[dta_bal$messenger == "ctrl"], na.rm=T)
seed_plot[2,2] <-  summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$hybrid[dta_bal$messenger == "ctrl"], na.rm=T)

## opv
res_itt_seed[3,1,h]  <-  mean(dta_bal$opv[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_seed[4,1,h]  <-  sd(dta_bal$opv[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_seed[3,2,h] <- summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_seed[4,2,h] <- summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_seed[3,3,h] <- ifelse(totrep >0, RI("opv",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

seed_plot[3,1] <- "open pollinated"
seed_plot[3,3:4] <- confint(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$opv[dta_bal$messenger == "ctrl"], na.rm=T)
seed_plot[3,2] <-  summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$opv[dta_bal$messenger == "ctrl"], na.rm=T)

##combiner
res_itt_pract[13,1,h]  <-  mean(dta_bal$combiner[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[14,1,h]  <-  sd(dta_bal$combiner[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[13,2,h]  <- summary(lm(as.formula(paste("combiner", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[14,2,h]  <- summary(lm(as.formula(paste("combiner", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[13,3,h]  <- ifelse(totrep >0, RI("combiner",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("combiner", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

### bought seed
res_itt_pract[15,1,h]  <-  mean(dta_bal$bought_seed[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[16,1,h]  <-  sd(dta_bal$bought_seed[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[15,2,h]  <- summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[16,2,h]  <- summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[15,3,h]  <- ifelse(totrep >0, RI("bought_seed",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
seed_plot[4,1] <- "bought seed"
seed_plot[4,3:4] <- confint(lm(as.formula(paste("bought_seed", treatment, sep ="~"), level=.9), data=dta_bal))[2,] /  mean(dta_bal$bought_seed[dta_bal$messenger == "ctrl"], na.rm=T)
seed_plot[4,2] <-  summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /  mean(dta_bal$bought_seed[dta_bal$messenger == "ctrl"], na.rm=T)


#### used chemicals
res_itt_pract[17,1,h]  <-  mean(dta_bal$chem[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[18,1,h]  <-  sd(dta_bal$chem[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[17,2,h]  <- summary(lm(as.formula(paste("chem", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[18,2,h]  <- summary(lm(as.formula(paste("chem", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[17,3,h]  <- ifelse(totrep >0, RI("chem",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("chem", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

###hired labour
res_itt_pract[19,1,h]  <-  mean(dta_bal$labour[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[20,1,h]  <-  sd(dta_bal$labour[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_pract[19,2,h]  <- summary(lm(as.formula(paste("labour", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[20,2,h]  <- summary(lm(as.formula(paste("labour", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[19,3,h]  <- ifelse(totrep >0, RI("labour",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("labour", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

#if (totrep >0) {
#res_h0_pract[1:7,4,h] <- FSR_RI( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment ,dta_bal, pvals =  res_h0_pract[,3,h] , nr_repl_pi = 100)

#res_h0_pract[1:7,4,h] <- FSR_OLS( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment,dta_bal, nr_repl = totrep)[[4]]

indexer <-  FW_index(treatment,c("day_one","space","striga","weed", "fert","impseed", "combiner","bought_seed","chem","labour"),dta_bal, nr_repl=totrep)
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

#RI_FWER(c("day_one","space","striga","weed", "fert","impseed", "combiner","bought_seed","chem","labour"),"(messenger != 'ctrl') +ivr+sms+as.factor(recipient)",dta_bal, c(0.4287,0.0008,0.0132,0.7909,0.0091,0.3616,0.0767,0.3091,0.1425,0.7879), 10000)
#RI_FWER(c("fert_dap","fert_urea","fert_org"),"(messenger != 'ctrl') +ivr+sms+as.factor(recipient) ",dta_bal, c(0.3355,0.0167,0.0135),10000)
#RI_FWER(c("hybrid","opv"),"(messenger != 'ctrl') +ivr+sms+as.factor(recipient)",dta_bal, c(0.353,0.561))

################################# production ###########################
##### does the video increases production related outcomes?

#trimming is done on end result
dta_bal2 <- subset(dta_bal, prod_tot>0)
dta_bal2$log_prod_tot <- log(dta_bal2$prod_tot)
dta_trim <- trim("log_prod_tot", dta_bal2, .05)

### production
res_itt_prod[1,1,h] <- mean(dta_trim$log_prod_tot[dta_trim$messenger == "ctrl"], na.rm=T)
res_itt_prod[2,1,h] <- sd(dta_trim$log_prod_tot[dta_trim$messenger == "ctrl"], na.rm=T)
res_itt_prod[1,2,h] <- summary(lm(as.formula(paste("log_prod_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_itt_prod[2,2,h] <- summary(lm(as.formula(paste("log_prod_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,2]
res_itt_prod[1,3,h] <- ifelse(totrep >0, RI("log_prod_tot",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_prod_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

### area
dta_bal2 <- subset(dta_bal, area_tot>0)
dta_bal2$log_area_tot <- log(dta_bal2$area_tot)
dta_trim <- trim("log_area_tot", dta_bal2, .05)

res_itt_prod[3,1,h] <- mean(dta_trim$log_area_tot[dta_trim$messenger == "ctrl"], na.rm=T)
res_itt_prod[4,1,h] <- sd(dta_trim$log_area_tot[dta_trim$messenger == "ctrl"], na.rm=T)
res_itt_prod[3,2,h] <- summary(lm(as.formula(paste("log_area_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_itt_prod[4,2,h] <- summary(lm(as.formula(paste("log_area_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,2]
res_itt_prod[3,3,h] <- ifelse(totrep >0, RI("log_area_tot",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_area_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

###yield

dta_bal2 <- subset(dta_bal, yield_av >0)
dta_bal2$log_yield_av <- log(dta_bal2$yield_av)
dta_trim <- trim("log_yield_av", dta_bal2, .05)

res_itt_prod[5,1,h] <- mean(dta_trim$log_yield_av[dta_trim$messenger == "ctrl"], na.rm=T)
res_itt_prod[6,1,h] <- sd(dta_trim$log_yield_av[dta_trim$messenger == "ctrl"], na.rm=T)
res_itt_prod[5,2,h] <- summary(lm(as.formula(paste("log_yield_av",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_itt_prod[6,2,h] <- summary(lm(as.formula(paste("log_yield_av",treatment,sep = "~")), data=dta_trim))$coefficients[2,2]
res_itt_prod[5,3,h] <- ifelse(totrep >0, RI("log_yield_av",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_yield_av",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

### was yield better compared to normal year?
res_itt_prod[7,1,h] <- mean(dta_bal$yield_better[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_prod[8,1,h] <- sd(dta_bal$yield_better[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_prod[7,2,h] <- summary(lm(as.formula(paste("yield_better",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_prod[8,2,h] <- summary(lm(as.formula(paste("yield_better",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_prod[7,3,h] <- ifelse(totrep >0, RI("yield_better",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("yield_better",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])


###index
dta_bal2 <- subset(dta_bal, area_tot >0 & prod_tot>0 & yield_av >0)
dta_bal2$log_prod_tot <- log(dta_bal2$prod_tot)
dta_bal2$log_area_tot <- log(dta_bal2$area_tot)
dta_bal2$log_yield_av <- log(dta_bal2$yield_av)

dta_bal2 <- trim("log_yield_av", dta_bal2, .05)


#res_itt_prod <- Bcorr(c("log_prod_tot","log_area_tot","log_yield_av","yield_better"),dta_bal2, res_itt_prod ,h)
#RI_FWER(c("log_prod_tot","log_area_tot","log_yield_av","yield_better"),"(messenger != 'ctrl') +ivr+sms+as.factor(recipient)",dta_bal2, c(0.7791	,0.0264	,0.0154	,0.5528))

dta_bal2$log_area_tot <- -dta_bal2$log_area_tot

	indexer <- FW_index(treatment, c("log_prod_tot", "log_area_tot", "yield_better"),dta_bal2, nr_repl=totrep)
res_itt_prod[9,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
res_itt_prod[10,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
res_itt_prod[9,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_prod[10,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_prod[9,3,h] <-  indexer[[2]]

prod_plot[3,1] <- "production"
prod_plot[3,3:4] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
prod_plot[3,2] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])

################################## disposal ##########################
#### maize consumed


res_itt_disp[1,1,h] <- mean(dta_bal$cons_maize_yes[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_disp[2,1,h] <- sd(dta_bal$cons_maize_yes[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_disp[1,2,h] <- summary(lm(as.formula(paste("cons_maize_yes",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_disp[2,2,h] <- summary(lm(as.formula(paste("cons_maize_yes",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_disp[1,3,h] <- ifelse(totrep >0, RI("cons_maize_yes",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("cons_maize_yes",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

### sold maize?

res_itt_disp[3,1,h] <- mean(dta_bal$sold_maize[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_disp[4,1,h] <- sd(dta_bal$sold_maize[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_disp[3,2,h] <- summary(lm(as.formula(paste("sold_maize",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_disp[4,2,h] <- summary(lm(as.formula(paste("sold_maize",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_disp[3,3,h] <- ifelse(totrep >0, RI("sold_maize",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("sold_maize",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])
### kept maize for seed?
res_itt_disp[5,1,h] <- mean(dta_bal$save_seed[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_disp[6,1,h] <- sd(dta_bal$save_seed[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_disp[5,2,h] <- summary(lm(as.formula(paste("save_seed",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_disp[6,2,h] <- summary(lm(as.formula(paste("save_seed",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_disp[5,3,h] <- ifelse(totrep >0, RI("save_seed",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("save_seed",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

res_itt_disp <- Bcorr(c("cons_maize_yes","sold_maize","save_seed"),dta_bal, res_itt_disp ,h)


#res_h0_disp[1:3,4,h] <- FSR_OLS(c("cons_maize_yes", "sold_maize", "save_seed"),treatment,dta_bal, nr_repl = totrep)[[4]]
dta_bal2 <- dta_bal
dta_bal2$save_seed <- !dta_bal2$save_seed

indexer <- FW_index(treatment, c("cons_maize_yes", "sold_maize", "save_seed"),dta_bal2, nr_repl=totrep)
res_itt_disp[7,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
res_itt_disp[8,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
res_itt_disp[7,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_disp[8,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_disp[7,3,h] <-  indexer[[2]]

################################ welfare #############################

## better off than average
res_itt_wel[1,1,h] <- mean(dta_bal$better_av[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_wel[2,1,h] <- sd(dta_bal$better_av[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_wel[1,2,h]  <- summary(lm(as.formula(paste("better_av",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_wel[2,2,h]  <- summary(lm(as.formula(paste("better_av",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_wel[1,3,h]  <- ifelse(totrep >0, RI("better_av",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("better_av",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

## better off than 6mo
res_itt_wel[3,1,h] <- mean(dta_bal$better_6m[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_wel[4,1,h] <- sd(dta_bal$better_6m[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_wel[3,2,h]  <- summary(lm(as.formula(paste("better_6m",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_wel[4,2,h]  <- summary(lm(as.formula(paste("better_6m",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_wel[3,3,h]  <- ifelse(totrep >0, RI("better_6m",treatment , dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("better_6m",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

#can eat preferred foods
res_itt_wel[5,1,h] <- mean(dta_bal$eatpref[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_wel[6,1,h] <- sd(dta_bal$eatpref[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_wel[5,2,h]  <-  summary(lm(as.formula(paste("eatpref",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_wel[6,2,h]  <-  summary(lm(as.formula(paste("eatpref",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_wel[5,3,h]  <- ifelse(totrep >0, RI("eatpref",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("eatpref",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

#can eat enough foods
res_itt_wel[7,1,h] <- mean(dta_bal$eatenough[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_wel[8,1,h] <- sd(dta_bal$eatenough[dta_bal$messenger == "ctrl"], na.rm=T)
res_itt_wel[7,2,h]  <- summary(lm(as.formula(paste("eatenough",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_wel[8,2,h]  <- summary(lm(as.formula(paste("eatenough",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_wel[7,3,h]  <- ifelse(totrep >0, RI("eatenough",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("eatenough",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

#consumption - logged and trimmed
dta_bal2 <- subset(dta_bal, cons>0)
dta_bal2$log_cons <- log(dta_bal2$cons)
dta_trim <- trim("log_cons", dta_bal2, .05)

res_itt_wel[9,1,h] <- mean(dta_trim$log_cons[dta_trim$messenger == "ctrl"], na.rm=T)
res_itt_wel[10,1,h] <- sd(dta_trim$log_cons[dta_trim$messenger == "ctrl"], na.rm=T)
res_itt_wel[9,2,h]  <- summary(lm(as.formula(paste("log_cons",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_itt_wel[10,2,h]  <- summary(lm(as.formula(paste("log_cons",treatment,sep = "~")), data=dta_trim))$coefficients[2,2]
res_itt_wel[9,3,h]  <- ifelse(totrep >0, RI("log_cons",treatment , dta_bal2, nr_repl = totrep), summary(lm(as.formula(paste("log_cons",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

res_itt_wel <- Bcorr(c("better_av","better_6m","eatpref","eatenough","log_cons"),dta_trim, res_itt_wel ,h)


	#res_h0_wel[1:5,4,h]   <- FSR_OLS(c("better_av", "better_6m", "eatpref","eatenough","log_cons"),treatment,dta_bal, nr_repl = totrep)[[4]]
	indexer <- FW_index(treatment, c("better_av", "better_6m", "eatpref","eatenough","log_cons"),dta_trim, nr_repl=totrep)
res_itt_wel[11,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
res_itt_wel[12,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
res_itt_wel[11,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_wel[12,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_wel[11,3,h] <-  indexer[[2]]

prod_plot[4,1] <- "welfare"
prod_plot[4,3:4] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])
prod_plot[4,2] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index[indexer[[3]]$messenger == "ctrl"])

prod_plot$x <- factor(prod_plot$x, levels=rev(prod_plot$x))
prod_plot$grp <- "video"



h <- 2
############################################# for those who have seen a video, is there an additional ivr effect ##############################################
## drop the control
dta <- subset(dta, messenger != "ctrl")
treatment <- "(ivr == 'yes') + as.factor(recipient) + as.factor(messenger)+ sms" 
dta_bal <- dta

RI <- function(dep, indep, dta , nr_repl = 1000) {
### determines treatmetn cell
	dta <- dta %>% 
    		mutate(treat = group_indices_(dta, .dots=c("recipient", "messenger"))) 
	### allocates unique ID based on treatment cell status and village
	dta <- dta %>% 
    		mutate(uniqID = group_indices_(dta, .dots=c("distID", "subID","vilID"))) 
	### the NULL
	crit <- summary(lm(as.formula(paste(dep,indep,sep="~")), data=dta))$coefficients[2,1]
	dta <-  data.table(cbind(dta[dep],dta[c("messenger","recipient","treat","uniqID","hhid","ivr","sms","called","totsms")]))

	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
		dta_sim <- data.table(dta)
		setDT(dta_sim)[,ivr:=sample(ivr),by = (treat)]
		return(abs(coef(lm(as.formula(paste(dep,indep,sep="~")), data=dta_sim))[2]) > abs(crit) )
	}
	return(sum(oper)/nr_repl)
}

### and also a function to adjst standard errors
RI_FWER <- function(deps, indep, dta ,p_vals , nr_repl = 1000) {
#deps <- c("fert_dap","fert_urea","fert_org")
#indep <- treatment
#dta <- dta_bal
#pvals <- c(0.0366,0.3528,0.0024)
#nr_repl <- 1000
#RI_FWER(c("fert_dap","fert_urea","fert_org"),treatment,dta_bal, c(0.0366,0.3528,0.0024))


threshold_finder<- function(threshold){
  mean(apply(oper, 2, x <- function(x) sum(x <= threshold) > 0 ))
}
### determines treatmetn cell
	dta <- dta %>% 
    		mutate(treat = group_indices_(dta, .dots=c("recipient", "messenger"))) 
	### allocates unique ID based on treatment cell status and village
	dta <- dta %>% 
    		mutate(uniqID = group_indices_(dta, .dots=c("distID", "subID","vilID"))) 
	dta <-  data.table(cbind(dta[deps],dta[c("messenger","recipient","treat","uniqID","hhid","ivr","sms","called","totsms")]))
	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
		dta_sim <- data.table(dta)
		setDT(dta_sim)[,ivr:=sample(ivr),by = (treat)]
		return(unlist(lapply(deps, function(dvar) summary(lm(as.formula(paste(dvar,indep,sep="~")), data=dta_sim))$coefficients[2,4])))
		}

thresholds <- seq(0, 0.1, length.out = 10000)
type_I_rate <- sapply(thresholds, threshold_finder)
return( list=c(thresholds[max(which(type_I_rate <= 0.05))],thresholds[max(which(type_I_rate <= 0.01))], thresholds[max(which(type_I_rate <= 0.001))]))
}


res_itt_know[1,1,h] <- mean(dta_bal$know_space[dta_bal$ivr != "yes"])
res_itt_know[2,1,h] <- sd(dta_bal$know_space[dta_bal$ivr != "yes"])
res_itt_know[1,2,h] <- summary(lm(as.formula(paste("know_space",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_itt_know[2,2,h] <- summary(lm(as.formula(paste("know_space",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
res_itt_know[1,3,h] <- ifelse(totrep >0, RI("know_space",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("know_space",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

res_itt_know[3,1,h] <-  mean(dta_bal$know_combine[dta_bal$ivr != "yes"])
res_itt_know[4,1,h] <-  sd(dta_bal$know_combine[dta_bal$ivr != "yes"])
res_itt_know[3,2,h] <- summary(lm(as.formula(paste("know_combine",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_itt_know[4,2,h] <- summary(lm(as.formula(paste("know_combine",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
res_itt_know[3,3,h] <-  ifelse(totrep >0, RI("know_combine",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("know_combine",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

res_itt_know[5,1,h] <-  mean(dta_bal$know_weed[dta_bal$ivr != "yes"])
res_itt_know[6,1,h] <-  sd(dta_bal$know_weed[dta_bal$ivr != "yes"])
res_itt_know[5,2,h] <- summary(lm(as.formula(paste("know_weed",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_itt_know[6,2,h] <- summary(lm(as.formula(paste("know_weed",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
res_itt_know[5,3,h] <-  ifelse(totrep >0, RI("know_weed",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("know_weed",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

res_itt_know[7,1,h] <- mean(dta_bal$know_armyworm[dta_bal$ivr != "yes"])
res_itt_know[8,1,h] <- sd(dta_bal$know_armyworm[dta_bal$ivr != "yes"])
res_itt_know[7,2,h] <- summary(lm(as.formula(paste("know_armyworm",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_itt_know[8,2,h] <- summary(lm(as.formula(paste("know_armyworm",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
res_itt_know[7,3,h] <-  ifelse(totrep >0, RI("know_armyworm",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("know_armyworm",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])
## no need to include armyworm in the index because we do not really expect an effect - but even when we include the effect is sig, so keep it in
indexer <- FW_index(treatment, c("know_space", "know_combine", "know_weed"),dta_bal, nr_repl=totrep)
res_itt_know[9,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$ivr != "yes"])
res_itt_know[10,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$ivr != "yes"])
res_itt_know[9,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_know[10,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_know[9,3,h] <-  indexer[[2]]

prod_plot[5,1] <- "knowledge"
prod_plot[5,3:4] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index[indexer[[3]]$ivr != "yes"])
prod_plot[5,2] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index[indexer[[3]]$ivr != "yes"])


res_itt_know <- Bcorr(c("know_space", "know_combine", "know_weed","know_armyworm"),dta_bal, res_itt_know ,h)

###if (wyfs_stat) {
###	res_h0_know[1:4,4,h] <- FSR_RI( c("know_space","know_combine","know_weed", "know_armyworm") ,treatment ,dta_bal, pvals = res_h0_know[1:4,3,h], nr_repl_ri = 100)[[4]]
###	} else { 
###if (totrep >0) {
###	res_h0_know[1:4,4,h] <- FSR_OLS( c("know_space","know_combine","know_weed","know_armyworm") ,treatment,dta_bal, nr_repl = totrep)[[4]]
###}
###}

################################# practices #############################
###timely planting
res_itt_pract[1,1,h]  <- mean(dta_bal$day_one[dta_bal$ivr != "yes"], na.rm=T)
res_itt_pract[2,1,h]  <- sd(dta_bal$day_one[dta_bal$ivr != "yes"], na.rm=T)
res_itt_pract[1,2,h]  <- summary(lm(as.formula(paste("day_one", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[2,2,h]  <- summary(lm(as.formula(paste("day_one", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[1,3,h]  <- ifelse(totrep >0, RI("day_one",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("day_one", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 


#### used recommended spacing use on at lease one plot as reported by at least one spouse
res_itt_pract[3,1,h]  <- mean(dta_bal$space[dta_bal$ivr != "yes"], na.rm=T)
res_itt_pract[4,1,h]  <- sd(dta_bal$space[dta_bal$ivr != "yes"], na.rm=T)
res_itt_pract[3,2,h]  <- summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[4,2,h]  <- summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[3,3,h]  <- ifelse(totrep >0, RI("space",treatment, dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## used recommended way to fight striga - this should be changed to include info of all plots 
res_itt_pract[5,1,h]  <- mean(dta_bal$striga[dta_bal$ivr != "yes"], na.rm=T)
res_itt_pract[6,1,h]  <- sd(dta_bal$striga[dta_bal$ivr != "yes"], na.rm=T)
res_itt_pract[5,2,h]  <- summary(lm(as.formula(paste("striga", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[6,2,h]  <- summary(lm(as.formula(paste("striga", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[5,3,h]  <- ifelse(totrep >0, RI("striga",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("striga", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## weeded on recommended timing? - this should be changed to include info of all plots 
res_itt_pract[7,1,h]  <- mean(dta_bal$weed[dta_bal$ivr != "yes"], na.rm=T)
res_itt_pract[8,1,h]  <- sd(dta_bal$weed[dta_bal$ivr != "yes"], na.rm=T)
res_itt_pract[7,2,h]  <- summary(lm(as.formula(paste("weed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[8,2,h]  <- summary(lm(as.formula(paste("weed", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[7,3,h]  <- ifelse(totrep >0, RI("weed",treatment, dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("weed", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## fertilizer use
res_itt_pract[9,1,h]  <-  mean(dta_bal$fert[dta_bal$ivr != "yes"], na.rm=T)
res_itt_pract[10,1,h]  <-  sd(dta_bal$fert[dta_bal$ivr != "yes"], na.rm=T)
res_itt_pract[9,2,h]  <- summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[10,2,h]  <- summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[9,3,h]  <- ifelse(totrep >0, RI("fert",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])
 
fert_plot[5,1] <- "all fertilizer"
fert_plot[5,3:4] <- confint(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$fert[dta_bal$ivr != "yes"], na.rm=T)
fert_plot[5,2] <-  summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] / mean(dta_bal$fert[dta_bal$ivr != "yes"], na.rm=T)

#### fert = DAP/NPK
res_itt_fert[1,1,h]  <-  mean(dta_bal$fert_dap[dta_bal$ivr != "yes"], na.rm=T)
res_itt_fert[2,1,h]  <-  sd(dta_bal$fert_dap[dta_bal$ivr != "yes"], na.rm=T)
res_itt_fert[1,2,h]  <- summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_fert[2,2,h]  <- summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_fert[1,3,h]  <- ifelse(totrep >0, RI("fert_dap",treatment , dta_bal, nr_repl = totrep) , summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

fert_plot[6,1] <- "DAP/NPK"
fert_plot[6,3:4] <- confint(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$fert_dap[dta_bal$ivr != "yes"], na.rm=T)
fert_plot[6,2] <-  summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] / mean(dta_bal$fert_dap[dta_bal$ivr != "yes"], na.rm=T)

#### fert = urea
res_itt_fert[3,1,h]  <-  mean(dta_bal$fert_urea[dta_bal$ivr != "yes"], na.rm=T)
res_itt_fert[4,1,h]  <-  sd(dta_bal$fert_urea[dta_bal$ivr != "yes"], na.rm=T)
res_itt_fert[3,2,h]  <- summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_fert[4,2,h]  <- summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_fert[3,3,h]  <- ifelse(totrep >0, RI("fert_urea",treatment , dta_bal, nr_repl = totrep) , summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])
fert_plot[7,1] <- "urea"
fert_plot[7,3:4] <- confint(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$fert_urea[dta_bal$ivr != "yes"], na.rm=T)
fert_plot[7,2] <-  summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] / mean(dta_bal$fert_urea[dta_bal$ivr != "yes"], na.rm=T)

#### fert = organic
res_itt_fert[5,1,h]  <-  mean(dta_bal$fert_org[dta_bal$ivr != "yes"], na.rm=T)
res_itt_fert[6,1,h]  <-  sd(dta_bal$fert_org[dta_bal$ivr != "yes"], na.rm=T)
res_itt_fert[5,2,h]  <-  summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_fert[6,2,h]  <- summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_fert[5,3,h]  <- ifelse(totrep >0, RI("fert_org",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
fert_plot[8,1] <- "organic"
fert_plot[8,3:4] <- confint(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$fert_org[dta_bal$ivr != "yes"], na.rm=T)
fert_plot[8,2] <-  summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] / mean(dta_bal$fert_org[dta_bal$ivr != "yes"], na.rm=T)

##improved seed  
res_itt_pract[11,1,h]  <-  mean(dta_bal$impseed[dta_bal$ivr != "yes"], na.rm=T)
res_itt_pract[12,1,h]  <-  sd(dta_bal$impseed[dta_bal$ivr != "yes"], na.rm=T)
res_itt_pract[11,2,h]  <- summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[12,2,h]  <- summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[11,3,h]  <- ifelse(totrep >0, RI("impseed",treatment , dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
seed_plot[5,1] <- "all seed"
seed_plot[5,3:4] <- confint(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$impseed[dta_bal$ivr != "yes"], na.rm=T)
seed_plot[5,2] <-  summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /   mean(dta_bal$impseed[dta_bal$ivr != "yes"], na.rm=T)

## hybrid
res_itt_seed[1,1,h]  <-  mean(dta_bal$hybrid[dta_bal$ivr != "yes"], na.rm=T)
res_itt_seed[2,1,h]  <-  sd(dta_bal$hybrid[dta_bal$ivr != "yes"], na.rm=T)
res_itt_seed[1,2,h] <- summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_seed[2,2,h] <- summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_seed[1,3,h] <- ifelse(totrep >0, RI("hybrid",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
seed_plot[6,1] <- "hybrid"
seed_plot[6,3:4] <- confint(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$hybrid[dta_bal$ivr != "yes"], na.rm=T)
seed_plot[6,2] <-  summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /   mean(dta_bal$hybrid[dta_bal$ivr != "yes"], na.rm=T)

## opv
res_itt_seed[3,1,h]  <-  mean(dta_bal$opv[dta_bal$ivr != "yes"], na.rm=T)
res_itt_seed[4,1,h]  <-  sd(dta_bal$opv[dta_bal$ivr != "yes"], na.rm=T)
res_itt_seed[3,2,h] <- summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_seed[4,2,h] <- summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_seed[3,3,h] <- ifelse(totrep >0, RI("opv",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

seed_plot[7,1] <- "open pollinated"
seed_plot[7,3:4] <- confint(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$opv[dta_bal$ivr != "yes"], na.rm=T)
seed_plot[7,2] <-  summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /   mean(dta_bal$opv[dta_bal$ivr != "yes"], na.rm=T)


##combiner
res_itt_pract[13,1,h]  <-  mean(dta_bal$combiner[dta_bal$ivr != "yes"], na.rm=T)
res_itt_pract[14,1,h]  <-  sd(dta_bal$combiner[dta_bal$ivr != "yes"], na.rm=T)
res_itt_pract[13,2,h]  <- summary(lm(as.formula(paste("combiner", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[14,2,h]  <- summary(lm(as.formula(paste("combiner", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[13,3,h]  <- ifelse(totrep >0, RI("combiner",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("combiner", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

### bought seed
res_itt_pract[15,1,h]  <-  mean(dta_bal$bought_seed[dta_bal$ivr != "yes"], na.rm=T)
res_itt_pract[16,1,h]  <-  sd(dta_bal$bought_seed[dta_bal$ivr != "yes"], na.rm=T)
res_itt_pract[15,2,h]  <- summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[16,2,h]  <- summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[15,3,h]  <- ifelse(totrep >0, RI("bought_seed",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

seed_plot[8,1] <- "bought seed"
seed_plot[8,3:4] <- confint(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /  mean(dta_bal$bought_seed[dta_bal$ivr != "yes"], na.rm=T)
seed_plot[8,2] <-  summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] /   mean(dta_bal$bought_seed[dta_bal$ivr != "yes"], na.rm=T)


#### used chemicals
res_itt_pract[17,1,h]  <-  mean(dta_bal$chem[dta_bal$ivr != "yes"], na.rm=T)
res_itt_pract[18,1,h]  <-  sd(dta_bal$chem[dta_bal$ivr != "yes"], na.rm=T)
res_itt_pract[17,2,h]  <- summary(lm(as.formula(paste("chem", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[18,2,h]  <- summary(lm(as.formula(paste("chem", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[17,3,h]  <- ifelse(totrep >0, RI("chem",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("chem", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

###hired labour
res_itt_pract[19,1,h]  <-  mean(dta_bal$labour[dta_bal$ivr != "yes"], na.rm=T)
res_itt_pract[20,1,h]  <-  sd(dta_bal$labour[dta_bal$ivr != "yes"], na.rm=T)
res_itt_pract[19,2,h]  <- summary(lm(as.formula(paste("labour", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[20,2,h]  <- summary(lm(as.formula(paste("labour", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[19,3,h]  <- ifelse(totrep >0, RI("labour",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("labour", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

#if (totrep >0) {
#res_h0_pract[1:7,4,h] <- FSR_RI( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment ,dta_bal, pvals =  res_h0_pract[,3,h] , nr_repl_pi = 100)

#res_h0_pract[1:7,4,h] <- FSR_OLS( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment,dta_bal, nr_repl = totrep)[[4]]

indexer <-  FW_index(treatment,c("space","striga","weed", "fert","impseed", "combiner","bought_seed","chem","labour","day_one"),dta_bal, nr_repl=totrep)
res_itt_pract[21,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$ivr != "yes"])
res_itt_pract[22,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$ivr != "yes"])
res_itt_pract[21,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_pract[22,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_pract[21,3,h] <-  indexer[[2]]


prod_plot[6,1] <- "adoption"
prod_plot[6,3:4] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index[indexer[[3]]$ivr != "yes"])
prod_plot[6,2] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index[indexer[[3]]$ivr != "yes"])

#res_itt_pract <- Bcorr( c("day_one","space","striga","weed", "fert","impseed", "combiner","bought_seed","chem","labour"),dta_bal, res_itt_pract ,h)
#res_itt_fert <- Bcorr(c("fert_dap","fert_urea","fert_org"),dta_bal, res_itt_fert ,h)
#res_itt_seed <- Bcorr(c("hybrid","opv"),dta_bal, res_itt_seed ,h)

#RI_FWER(c("day_one","space","striga","weed", "fert","impseed", "combiner","bought_seed","chem","labour"),treatment,dta_bal, c())
#RI_FWER(c("fert_dap","fert_urea","fert_org"),treatment,dta_bal, c(0.0366,0.3528,0.0024),10000)

#RI_FWER(c("hybrid","opv"),treatment,dta_bal, c(0.0092,0.9699),10000)

################################# production ###########################
##### does the video increases production related outcomes?

#trimming is done on end result
dta_bal2 <- subset(dta_bal, prod_tot>0)
dta_bal2$log_prod_tot <- log(dta_bal2$prod_tot)
dta_trim <- trim("log_prod_tot", dta_bal2, .05)

### production
res_itt_prod[1,1,h] <- mean(dta_trim$log_prod_tot[dta_trim$ivr != "yes"], na.rm=T)
res_itt_prod[2,1,h] <- sd(dta_trim$log_prod_tot[dta_trim$ivr != "yes"], na.rm=T)
res_itt_prod[1,2,h] <- summary(lm(as.formula(paste("log_prod_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_itt_prod[2,2,h] <- summary(lm(as.formula(paste("log_prod_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,2]
res_itt_prod[1,3,h] <- ifelse(totrep >0, RI("log_prod_tot",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_prod_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])


### area
dta_bal2 <- subset(dta_bal, area_tot>0)
dta_bal2$log_area_tot <- log(dta_bal2$area_tot)
dta_trim <- trim("log_area_tot", dta_bal2, .05)

res_itt_prod[3,1,h] <- mean(dta_trim$log_area_tot[dta_trim$ivr != "yes"], na.rm=T)
res_itt_prod[4,1,h] <- sd(dta_trim$log_area_tot[dta_trim$ivr != "yes"], na.rm=T)
res_itt_prod[3,2,h] <- summary(lm(as.formula(paste("log_area_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_itt_prod[4,2,h] <- summary(lm(as.formula(paste("log_area_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,2]
res_itt_prod[3,3,h] <- ifelse(totrep >0, RI("log_area_tot",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_area_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

###yield

dta_bal2 <- subset(dta_bal, yield_av >0)
dta_bal2$log_yield_av <- log(dta_bal2$yield_av)
dta_trim <- trim("log_yield_av", dta_bal2, .05)

res_itt_prod[5,1,h] <- mean(dta_trim$log_yield_av[dta_trim$ivr != "yes"], na.rm=T)
res_itt_prod[6,1,h] <- sd(dta_trim$log_yield_av[dta_trim$ivr != "yes"], na.rm=T)
res_itt_prod[5,2,h] <- summary(lm(as.formula(paste("log_yield_av",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_itt_prod[6,2,h] <- summary(lm(as.formula(paste("log_yield_av",treatment,sep = "~")), data=dta_trim))$coefficients[2,2]
res_itt_prod[5,3,h] <- ifelse(totrep >0, RI("log_yield_av",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_yield_av",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

### was yield better compared to normal year?
res_itt_prod[7,1,h] <- mean(dta_bal$yield_better[dta_bal$ivr != "yes"], na.rm=T)
res_itt_prod[8,1,h] <- sd(dta_bal$yield_better[dta_bal$ivr != "yes"], na.rm=T)
res_itt_prod[7,2,h] <- summary(lm(as.formula(paste("yield_better",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_prod[8,2,h] <- summary(lm(as.formula(paste("yield_better",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_prod[7,3,h] <- ifelse(totrep >0, RI("yield_better",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("yield_better",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

###index
dta_bal2 <- subset(dta_bal, area_tot >0 & prod_tot>0 & yield_av >0)
dta_bal2$log_prod_tot <- log(dta_bal2$prod_tot)
dta_bal2$log_area_tot <- log(dta_bal2$area_tot)
dta_bal2$log_yield_av <- log(dta_bal2$yield_av)

dta_bal2 <- trim("log_yield_av", dta_bal2, .05)
#res_itt_prod <- Bcorr(c("log_prod_tot","log_area_tot","log_yield_av","yield_better"),dta_bal2, res_itt_prod ,h)
#RI_FWER(c("log_prod_tot","log_area_tot","log_yield_av","yield_better"),treatment,dta_bal2, c(0.121,0.4377,0.0148,0.411))

#res_h0_prod[1:4,4,h] <- FSR_OLS( c("log_prod_tot", "log_area_tot", "log_yield_av","yield_better") ,treatment,dta_bal, nr_repl = totrep)[[4]]

dta_bal2$log_area_tot <- -dta_bal2$log_area_tot

	indexer <- FW_index(treatment, c("log_prod_tot", "log_area_tot", "yield_better"),dta_bal2, nr_repl=totrep)
res_itt_prod[9,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$ivr != "yes"])
res_itt_prod[10,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$ivr != "yes"])
res_itt_prod[9,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_prod[10,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_prod[9,3,h] <-  indexer[[2]]


prod_plot[7,1] <- "production"
prod_plot[7,3:4] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index[indexer[[3]]$ivr != "yes"])
prod_plot[7,2] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index[indexer[[3]]$ivr != "yes"])

################################## disposal ##########################
#### maize consumed


res_itt_disp[1,1,h] <- mean(dta_bal$cons_maize_yes[dta_bal$ivr != "yes"], na.rm=T)
res_itt_disp[2,1,h] <- sd(dta_bal$cons_maize_yes[dta_bal$ivr != "yes"], na.rm=T)
res_itt_disp[1,2,h] <- summary(lm(as.formula(paste("cons_maize_yes",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_disp[2,2,h] <- summary(lm(as.formula(paste("cons_maize_yes",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_disp[1,3,h] <- ifelse(totrep >0, RI("cons_maize_yes",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("cons_maize_yes",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

### sold maize?

res_itt_disp[3,1,h] <- mean(dta_bal$sold_maize[dta_bal$ivr != "yes"], na.rm=T)
res_itt_disp[4,1,h] <- sd(dta_bal$sold_maize[dta_bal$ivr != "yes"], na.rm=T)
res_itt_disp[3,2,h] <- summary(lm(as.formula(paste("sold_maize",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_disp[4,2,h] <- summary(lm(as.formula(paste("sold_maize",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_disp[3,3,h] <- ifelse(totrep >0, RI("sold_maize",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("sold_maize",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])
### kept maize for seed?
res_itt_disp[5,1,h] <- mean(dta_bal$save_seed[dta_bal$ivr != "yes"], na.rm=T)
res_itt_disp[6,1,h] <- sd(dta_bal$save_seed[dta_bal$ivr != "yes"], na.rm=T)
res_itt_disp[5,2,h] <- summary(lm(as.formula(paste("save_seed",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_disp[6,2,h] <- summary(lm(as.formula(paste("save_seed",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_disp[5,3,h] <- ifelse(totrep >0, RI("save_seed",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("save_seed",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

res_itt_disp <- Bcorr(c("cons_maize_yes","sold_maize","save_seed"),dta_bal, res_itt_disp ,h)

#res_h0_disp[1:3,4,h] <- FSR_OLS(c("cons_maize_yes", "sold_maize", "save_seed"),treatment,dta_bal, nr_repl = totrep)[[4]]
dta_bal2 <- dta_bal
dta_bal2$save_seed <- !dta_bal2$save_seed

indexer <- FW_index(treatment, c("cons_maize_yes", "sold_maize", "save_seed"),dta_bal2, nr_repl=totrep)
res_itt_disp[7,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$ivr != "yes"])
res_itt_disp[8,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$ivr != "yes"])
res_itt_disp[7,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_disp[8,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_disp[7,3,h] <-  indexer[[2]]

################################ welfare #############################

## better off than average
res_itt_wel[1,1,h] <- mean(dta_bal$better_av[dta_bal$ivr != "yes"], na.rm=T)
res_itt_wel[2,1,h] <- sd(dta_bal$better_av[dta_bal$ivr != "yes"], na.rm=T)
res_itt_wel[1,2,h]  <- summary(lm(as.formula(paste("better_av",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_wel[2,2,h]  <- summary(lm(as.formula(paste("better_av",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_wel[1,3,h]  <- ifelse(totrep >0, RI("better_av",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("better_av",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

## better off than 6mo
res_itt_wel[3,1,h] <- mean(dta_bal$better_6m[dta_bal$ivr != "yes"], na.rm=T)
res_itt_wel[4,1,h] <- sd(dta_bal$better_6m[dta_bal$ivr != "yes"], na.rm=T)
res_itt_wel[3,2,h]  <- summary(lm(as.formula(paste("better_6m",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_wel[4,2,h]  <- summary(lm(as.formula(paste("better_6m",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_wel[3,3,h]  <- ifelse(totrep >0, RI("better_6m",treatment , dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("better_6m",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

#can eat preferred foods
res_itt_wel[5,1,h] <- mean(dta_bal$eatpref[dta_bal$ivr != "yes"], na.rm=T)
res_itt_wel[6,1,h] <- sd(dta_bal$eatpref[dta_bal$ivr != "yes"], na.rm=T)
res_itt_wel[5,2,h]  <-  summary(lm(as.formula(paste("eatpref",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_wel[6,2,h]  <-  summary(lm(as.formula(paste("eatpref",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_wel[5,3,h]  <- ifelse(totrep >0, RI("eatpref",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("eatpref",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

#can eat enough foods
res_itt_wel[7,1,h] <- mean(dta_bal$eatenough[dta_bal$ivr != "yes"], na.rm=T)
res_itt_wel[8,1,h] <- sd(dta_bal$eatenough[dta_bal$ivr != "yes"], na.rm=T)
res_itt_wel[7,2,h]  <- summary(lm(as.formula(paste("eatenough",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_wel[8,2,h]  <- summary(lm(as.formula(paste("eatenough",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_wel[7,3,h]  <- ifelse(totrep >0, RI("eatenough",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("eatenough",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

#consumption - logged and trimmed
dta_bal2 <- subset(dta_bal, cons>0)
dta_bal2$log_cons <- log(dta_bal2$cons)
dta_trim <- trim("log_cons", dta_bal2, .05)

res_itt_wel[9,1,h] <- mean(dta_trim$log_cons[dta_trim$ivr != "yes"], na.rm=T)
res_itt_wel[10,1,h] <- sd(dta_trim$log_cons[dta_trim$ivr != "yes"], na.rm=T)
res_itt_wel[9,2,h]  <- summary(lm(as.formula(paste("log_cons",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_itt_wel[10,2,h]  <- summary(lm(as.formula(paste("log_cons",treatment,sep = "~")), data=dta_trim))$coefficients[2,2]
res_itt_wel[9,3,h]  <- ifelse(totrep >0, RI("log_cons",treatment , dta_bal2, nr_repl = totrep), summary(lm(as.formula(paste("log_cons",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

res_itt_wel <- Bcorr(c("better_av","better_6m","eatpref","eatenough","log_cons"),dta_trim, res_itt_wel ,h)

indexer <- FW_index(treatment, c("better_av", "better_6m", "eatpref","eatenough","log_cons"),dta_trim, nr_repl=totrep)
res_itt_wel[11,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$ivr != "yes"])
res_itt_wel[12,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$ivr != "yes"])
res_itt_wel[11,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_wel[12,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_wel[11,3,h] <-  indexer[[2]]


prod_plot[8,1] <- "welfare"
prod_plot[8,3:4] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index[indexer[[3]]$ivr != "yes"])
prod_plot[8,2] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index[indexer[[3]]$ivr != "yes"])

prod_plot$grp[5:8] <- "ivr"



################################################## is there and additional sms effect ###################################################
h <- 3
dta <- subset(dta, ivr == "yes")
treatment <- "(sms=='yes') + as.factor(recipient) + as.factor(messenger)"
############################### knowledge  ############################
dta_bal <- dta

RI <- function(dep, indep, dta , nr_repl = 1000) {
### determines treatmetn cell
	dta <- dta %>% 
    		mutate(treat = group_indices_(dta, .dots=c("recipient", "messenger"))) 
	### allocates unique ID based on treatment cell status and village
	dta <- dta %>% 
    		mutate(uniqID = group_indices_(dta, .dots=c("distID", "subID","vilID"))) 
	### the NULL
	crit <- summary(lm(as.formula(paste(dep,indep,sep="~")), data=dta))$coefficients[2,1]
	dta <-  data.table(cbind(dta[dep],dta[c("messenger","recipient","treat","uniqID","hhid","ivr","sms","called","totsms")]))

	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
		dta_sim <- data.table(dta)
		setDT(dta_sim)[,sms:=sample(sms),by = (treat)]
		return(abs(coef(lm(as.formula(paste(dep,indep,sep="~")), data=dta_sim))[2]) > abs(crit) )
	}
	return(sum(oper)/nr_repl)
}

### and also a function to adjst standard errors
RI_FWER <- function(deps, indep, dta ,p_vals , nr_repl = 1000) {
#deps <- c("fert_dap","fert_urea","fert_org")
#indep <- treatment
#dta <- dta_bal
#pvals <- c(0.0366,0.3528,0.0024)
#nr_repl <- 1000
#RI_FWER(c("fert_dap","fert_urea","fert_org"),treatment,dta_bal, c(0.0366,0.3528,0.0024))


threshold_finder<- function(threshold){
  mean(apply(oper, 2, x <- function(x) sum(x <= threshold) > 0 ))
}
### determines treatmetn cell
	dta <- dta %>% 
    		mutate(treat = group_indices_(dta, .dots=c("recipient", "messenger"))) 
	### allocates unique ID based on treatment cell status and village
	dta <- dta %>% 
    		mutate(uniqID = group_indices_(dta, .dots=c("distID", "subID","vilID"))) 
	dta <-  data.table(cbind(dta[deps],dta[c("messenger","recipient","treat","uniqID","hhid","ivr","sms","called","totsms")]))
	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
		dta_sim <- data.table(dta)
		setDT(dta_sim)[,sms:=sample(sms),by = (treat)]
		return(unlist(lapply(deps, function(dvar) summary(lm(as.formula(paste(dvar,indep,sep="~")), data=dta_sim))$coefficients[2,4])))
		}

thresholds <- seq(0, 0.1, length.out = 10000)
type_I_rate <- sapply(thresholds, threshold_finder)
return( list=c(thresholds[max(which(type_I_rate <= 0.05))],thresholds[max(which(type_I_rate <= 0.01))], thresholds[max(which(type_I_rate <= 0.001))]))
}


res_itt_know[1,1,h] <- mean(dta_bal$know_space[dta_bal$sms != "yes"])
res_itt_know[2,1,h] <- sd(dta_bal$know_space[dta_bal$sms != "yes"])
res_itt_know[1,2,h] <- summary(lm(as.formula(paste("know_space",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_itt_know[2,2,h] <- summary(lm(as.formula(paste("know_space",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
res_itt_know[1,3,h] <- ifelse(totrep >0, RI("know_space",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("know_space",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

res_itt_know[3,1,h] <-  mean(dta_bal$know_combine[dta_bal$sms != "yes"])
res_itt_know[4,1,h] <-  sd(dta_bal$know_combine[dta_bal$sms != "yes"])
res_itt_know[3,2,h] <- summary(lm(as.formula(paste("know_combine",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_itt_know[4,2,h] <- summary(lm(as.formula(paste("know_combine",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
res_itt_know[3,3,h] <-  ifelse(totrep >0, RI("know_combine",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("know_combine",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

res_itt_know[5,1,h] <-  mean(dta_bal$know_weed[dta_bal$sms != "yes"])
res_itt_know[6,1,h] <-  sd(dta_bal$know_weed[dta_bal$sms != "yes"])
res_itt_know[5,2,h] <- summary(lm(as.formula(paste("know_weed",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_itt_know[6,2,h] <- summary(lm(as.formula(paste("know_weed",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
res_itt_know[5,3,h] <-  ifelse(totrep >0, RI("know_weed",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("know_weed",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

res_itt_know[7,1,h] <- mean(dta_bal$know_armyworm[dta_bal$sms != "yes"])
res_itt_know[8,1,h] <- sd(dta_bal$know_armyworm[dta_bal$sms != "yes"])
res_itt_know[7,2,h] <- summary(lm(as.formula(paste("know_armyworm",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_itt_know[8,2,h] <- summary(lm(as.formula(paste("know_armyworm",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
res_itt_know[7,3,h] <-  ifelse(totrep >0, RI("know_armyworm",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("know_armyworm",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])
## no need to include armyworm in the index because we do not really expect an effect - but even when we include the effect is sig, so keep it in
indexer <- FW_index(treatment, c("know_space", "know_combine", "know_weed"),dta_bal, nr_repl=totrep)
res_itt_know[9,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$sms != "yes"])
res_itt_know[10,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$sms != "yes"])
res_itt_know[9,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_know[10,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_know[9,3,h] <-  indexer[[2]]

res_itt_know <- Bcorr(c("know_space", "know_combine", "know_weed","know_armyworm"),dta_bal, res_itt_know ,h)


prod_plot[9,1] <- "knowledge"
prod_plot[9,3:4] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index[indexer[[3]]$sms != "yes"])
prod_plot[9,2] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index[indexer[[3]]$sms != "yes"])


###if (wyfs_stat) {
###	res_h0_know[1:4,4,h] <- FSR_RI( c("know_space","know_combine","know_weed", "know_armyworm") ,treatment ,dta_bal, pvals = res_h0_know[1:4,3,h], nr_repl_ri = 100)[[4]]
###	} else { 
###if (totrep >0) {
###	res_h0_know[1:4,4,h] <- FSR_OLS( c("know_space","know_combine","know_weed","know_armyworm") ,treatment,dta_bal, nr_repl = totrep)[[4]]
###}
###}

################################# practices #############################
###timely planting
res_itt_pract[1,1,h]  <- mean(dta_bal$day_one[dta_bal$sms != "yes"], na.rm=T)
res_itt_pract[2,1,h]  <- sd(dta_bal$day_one[dta_bal$sms != "yes"], na.rm=T)
res_itt_pract[1,2,h]  <- summary(lm(as.formula(paste("day_one", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[2,2,h]  <- summary(lm(as.formula(paste("day_one", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[1,3,h]  <- ifelse(totrep >0, RI("day_one",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("day_one", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

#### used recommended spacing use on at lease one plot as reported by at least one spouse
res_itt_pract[3,1,h]  <- mean(dta_bal$space[dta_bal$sms != "yes"], na.rm=T)
res_itt_pract[4,1,h]  <- sd(dta_bal$space[dta_bal$sms != "yes"], na.rm=T)
res_itt_pract[3,2,h]  <- summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[4,2,h]  <- summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[3,3,h]  <- ifelse(totrep >0, RI("space",treatment, dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## used recommended way to fight striga - this should be changed to include info of all plots 
res_itt_pract[5,1,h]  <- mean(dta_bal$striga[dta_bal$sms != "yes"], na.rm=T)
res_itt_pract[6,1,h]  <- sd(dta_bal$striga[dta_bal$sms != "yes"], na.rm=T)
res_itt_pract[5,2,h]  <- summary(lm(as.formula(paste("striga", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[6,2,h]  <- summary(lm(as.formula(paste("striga", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[5,3,h]  <- ifelse(totrep >0, RI("striga",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("striga", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## weeded on recommended timing? - this should be changed to include info of all plots 
res_itt_pract[7,1,h]  <- mean(dta_bal$weed[dta_bal$sms != "yes"], na.rm=T)
res_itt_pract[8,1,h]  <- sd(dta_bal$weed[dta_bal$sms != "yes"], na.rm=T)
res_itt_pract[7,2,h]  <- summary(lm(as.formula(paste("weed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[8,2,h]  <- summary(lm(as.formula(paste("weed", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[7,3,h]  <- ifelse(totrep >0, RI("weed",treatment, dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("weed", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## fertilizer use
res_itt_pract[9,1,h]  <-  mean(dta_bal$fert[dta_bal$sms != "yes"], na.rm=T)
res_itt_pract[10,1,h]  <-  sd(dta_bal$fert[dta_bal$sms != "yes"], na.rm=T)
res_itt_pract[9,2,h]  <- summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[10,2,h]  <- summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[9,3,h]  <- ifelse(totrep >0, RI("fert",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

fert_plot[9,1] <- "all fertilizer"
fert_plot[9,3:4] <- confint(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /   mean(dta_bal$fert[dta_bal$sms != "yes"], na.rm=T)
fert_plot[9,2] <-  summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] / mean(dta_bal$fert[dta_bal$sms != "yes"], na.rm=T)


#### fert = DAP/NPK
res_itt_fert[1,1,h]  <-  mean(dta_bal$fert_dap[dta_bal$sms != "yes"], na.rm=T)
res_itt_fert[2,1,h]  <-  sd(dta_bal$fert_dap[dta_bal$sms != "yes"], na.rm=T)
res_itt_fert[1,2,h]  <- summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_fert[2,2,h]  <- summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_fert[1,3,h]  <- ifelse(totrep >0, RI("fert_dap",treatment , dta_bal, nr_repl = totrep) , summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

fert_plot[10,1] <- "DAP/NPK"
fert_plot[10,3:4] <- confint(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /   mean(dta_bal$fert_dap[dta_bal$sms != "yes"], na.rm=T)
fert_plot[10,2] <-  summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] / mean(dta_bal$fert_dap[dta_bal$sms != "yes"], na.rm=T)

#### fert = urea
res_itt_fert[3,1,h]  <-  mean(dta_bal$fert_urea[dta_bal$sms != "yes"], na.rm=T)
res_itt_fert[4,1,h]  <-  sd(dta_bal$fert_urea[dta_bal$sms != "yes"], na.rm=T)
res_itt_fert[3,2,h]  <- summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_fert[4,2,h]  <- summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_fert[3,3,h]  <- ifelse(totrep >0, RI("fert_urea",treatment , dta_bal, nr_repl = totrep) , summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

fert_plot[11,1] <- "urea"
fert_plot[11,3:4] <- confint(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /   mean(dta_bal$fert_urea[dta_bal$sms != "yes"], na.rm=T)
fert_plot[11,2] <-  summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] / mean(dta_bal$fert_urea[dta_bal$sms != "yes"], na.rm=T)
#### fert = organic
res_itt_fert[5,1,h]  <-  mean(dta_bal$fert_org[dta_bal$sms != "yes"], na.rm=T)
res_itt_fert[6,1,h]  <-  sd(dta_bal$fert_org[dta_bal$sms != "yes"], na.rm=T)
res_itt_fert[5,2,h]  <-  summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_fert[6,2,h]  <- summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_fert[5,3,h]  <- ifelse(totrep >0, RI("fert_org",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
fert_plot[12,1] <- "organic"
fert_plot[12,3:4] <- confint(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal), level=.9)[2,] /   mean(dta_bal$fert_org[dta_bal$sms != "yes"], na.rm=T)
fert_plot[12,2] <-  summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,1] / mean(dta_bal$fert_org[dta_bal$sms != "yes"], na.rm=T)

##improved seed  
res_itt_pract[11,1,h]  <-  mean(dta_bal$impseed[dta_bal$sms != "yes"], na.rm=T)
res_itt_pract[12,1,h]  <-  sd(dta_bal$impseed[dta_bal$sms != "yes"], na.rm=T)
res_itt_pract[11,2,h]  <- summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[12,2,h]  <- summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[11,3,h]  <- ifelse(totrep >0, RI("impseed",treatment , dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 



## hybrid
res_itt_seed[1,1,h]  <-  mean(dta_bal$hybrid[dta_bal$sms != "yes"], na.rm=T)
res_itt_seed[2,1,h]  <-  sd(dta_bal$hybrid[dta_bal$sms != "yes"], na.rm=T)
res_itt_seed[1,2,h] <- summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_seed[2,2,h] <- summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_seed[1,3,h] <- ifelse(totrep >0, RI("hybrid",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

## opv
res_itt_seed[3,1,h]  <-  mean(dta_bal$opv[dta_bal$sms != "yes"], na.rm=T)
res_itt_seed[4,1,h]  <-  sd(dta_bal$opv[dta_bal$sms != "yes"], na.rm=T)
res_itt_seed[3,2,h] <- summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_seed[4,2,h] <- summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_seed[3,3,h] <- ifelse(totrep >0, RI("opv",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 


##combiner
res_itt_pract[13,1,h]  <-  mean(dta_bal$combiner[dta_bal$sms != "yes"], na.rm=T)
res_itt_pract[14,1,h]  <-  sd(dta_bal$combiner[dta_bal$sms != "yes"], na.rm=T)
res_itt_pract[13,2,h]  <- summary(lm(as.formula(paste("combiner", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[14,2,h]  <- summary(lm(as.formula(paste("combiner", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[13,3,h]  <- ifelse(totrep >0, RI("combiner",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("combiner", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

### bought seed
res_itt_pract[15,1,h]  <-  mean(dta_bal$bought_seed[dta_bal$sms != "yes"], na.rm=T)
res_itt_pract[16,1,h]  <-  sd(dta_bal$bought_seed[dta_bal$sms != "yes"], na.rm=T)
res_itt_pract[15,2,h]  <- summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[16,2,h]  <- summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[15,3,h]  <- ifelse(totrep >0, RI("bought_seed",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 


#### used chemicals
res_itt_pract[17,1,h]  <-  mean(dta_bal$chem[dta_bal$sms != "yes"], na.rm=T)
res_itt_pract[18,1,h]  <-  sd(dta_bal$chem[dta_bal$sms != "yes"], na.rm=T)
res_itt_pract[17,2,h]  <- summary(lm(as.formula(paste("chem", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[18,2,h]  <- summary(lm(as.formula(paste("chem", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[17,3,h]  <- ifelse(totrep >0, RI("chem",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("chem", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

###hired labour
res_itt_pract[19,1,h]  <-  mean(dta_bal$labour[dta_bal$sms != "yes"], na.rm=T)
res_itt_pract[20,1,h]  <-  sd(dta_bal$labour[dta_bal$sms != "yes"], na.rm=T)
res_itt_pract[19,2,h]  <- summary(lm(as.formula(paste("labour", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_itt_pract[20,2,h]  <- summary(lm(as.formula(paste("labour", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
res_itt_pract[19,3,h]  <- ifelse(totrep >0, RI("labour",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("labour", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

#if (totrep >0) {
#res_h0_pract[1:7,4,h] <- FSR_RI( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment ,dta_bal, pvals =  res_h0_pract[,3,h] , nr_repl_pi = 100)

#res_h0_pract[1:7,4,h] <- FSR_OLS( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment,dta_bal, nr_repl = totrep)[[4]]

indexer <-  FW_index(treatment,c("space","striga","weed", "fert","impseed", "combiner","bought_seed","chem","labour","day_one"),dta_bal, nr_repl=totrep)
res_itt_pract[21,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$sms != "yes"])
res_itt_pract[22,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$sms != "yes"])
res_itt_pract[21,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_pract[22,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_pract[21,3,h] <-  indexer[[2]]

prod_plot[10,1] <- "adoption"
prod_plot[10,3:4] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index[indexer[[3]]$sms != "yes"])
prod_plot[10,2] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index[indexer[[3]]$sms != "yes"])

res_itt_pract <- Bcorr( c("day_one","space","striga","weed", "fert","impseed", "combiner","bought_seed","chem","labour"),dta_bal, res_itt_pract ,h)
res_itt_fert <- Bcorr(c("fert_dap","fert_urea","fert_org"),dta_bal, res_itt_fert ,h)
res_itt_seed <- Bcorr(c("hybrid","opv"),dta_bal, res_itt_seed ,h)
#RI_FWER(c("hybrid","opv"),treatment,dta_bal, c(0.0389,0.14),10000)

################################# production ###########################
##### does the video increases production related outcomes?

#trimming is done on end result
dta_bal2 <- subset(dta_bal, prod_tot>0)
dta_bal2$log_prod_tot <- log(dta_bal2$prod_tot)
dta_trim <- trim("log_prod_tot", dta_bal2, .05)

### production
res_itt_prod[1,1,h] <- mean(dta_trim$log_prod_tot[dta_trim$sms != "yes"], na.rm=T)
res_itt_prod[2,1,h] <- sd(dta_trim$log_prod_tot[dta_trim$sms != "yes"], na.rm=T)
res_itt_prod[1,2,h] <- summary(lm(as.formula(paste("log_prod_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_itt_prod[2,2,h] <- summary(lm(as.formula(paste("log_prod_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,2]
res_itt_prod[1,3,h] <- ifelse(totrep >0, RI("log_prod_tot",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_prod_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

### area
dta_bal2 <- subset(dta_bal, area_tot>0)
dta_bal2$log_area_tot <- log(dta_bal2$area_tot)
dta_trim <- trim("log_area_tot", dta_bal2, .05)

res_itt_prod[3,1,h] <- mean(dta_trim$log_area_tot[dta_trim$sms != "yes"], na.rm=T)
res_itt_prod[4,1,h] <- sd(dta_trim$log_area_tot[dta_trim$sms != "yes"], na.rm=T)
res_itt_prod[3,2,h] <- summary(lm(as.formula(paste("log_area_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_itt_prod[4,2,h] <- summary(lm(as.formula(paste("log_area_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,2]
res_itt_prod[3,3,h] <- ifelse(totrep >0, RI("log_area_tot",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_area_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])


###yield

dta_bal2 <- subset(dta_bal, yield_av >0)
dta_bal2$log_yield_av <- log(dta_bal2$yield_av)
dta_trim <- trim("log_yield_av", dta_bal2, .05)

res_itt_prod[5,1,h] <- mean(dta_trim$log_yield_av[dta_trim$sms != "yes"], na.rm=T)
res_itt_prod[6,1,h] <- sd(dta_trim$log_yield_av[dta_trim$sms != "yes"], na.rm=T)
res_itt_prod[5,2,h] <- summary(lm(as.formula(paste("log_yield_av",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_itt_prod[6,2,h] <- summary(lm(as.formula(paste("log_yield_av",treatment,sep = "~")), data=dta_trim))$coefficients[2,2]
res_itt_prod[5,3,h] <- ifelse(totrep >0, RI("log_yield_av",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_yield_av",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

### was yield better compared to normal year?
res_itt_prod[7,1,h] <- mean(dta_bal$yield_better[dta_bal$sms != "yes"], na.rm=T)
res_itt_prod[8,1,h] <- sd(dta_bal$yield_better[dta_bal$sms != "yes"], na.rm=T)
res_itt_prod[7,2,h] <- summary(lm(as.formula(paste("yield_better",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_prod[8,2,h] <- summary(lm(as.formula(paste("yield_better",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_prod[7,3,h] <- ifelse(totrep >0, RI("yield_better",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("yield_better",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

###index
dta_bal2 <- subset(dta_bal, area_tot >0 & prod_tot>0 & yield_av >0)
dta_bal2$log_prod_tot <- log(dta_bal2$prod_tot)
dta_bal2$log_area_tot <- log(dta_bal2$area_tot)
dta_bal2$log_yield_av <- log(dta_bal2$yield_av)

dta_bal2 <- trim("log_yield_av", dta_bal2, .05)

res_itt_prod <- Bcorr(c("log_prod_tot","log_area_tot","log_yield_av","yield_better"),dta_bal2, res_itt_prod ,h)

#res_h0_prod[1:4,4,h] <- FSR_OLS( c("log_prod_tot", "log_area_tot", "log_yield_av","yield_better") ,treatment,dta_bal, nr_repl = totrep)[[4]]

dta_bal2$log_area_tot <- -dta_bal2$log_area_tot

	indexer <- FW_index(treatment, c("log_prod_tot", "log_area_tot", "yield_better"),dta_bal2, nr_repl=totrep)
res_itt_prod[9,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$sms != "yes"])
res_itt_prod[10,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$sms != "yes"])
res_itt_prod[9,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_prod[10,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_prod[9,3,h] <-  indexer[[2]]

prod_plot[11,1] <- "production"
prod_plot[11,3:4] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index[indexer[[3]]$sms != "yes"])
prod_plot[11,2] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index[indexer[[3]]$sms != "yes"])

################################## disposal ##########################
#### maize consumed


res_itt_disp[1,1,h] <- mean(dta_bal$cons_maize_yes[dta_bal$sms != "yes"], na.rm=T)
res_itt_disp[2,1,h] <- sd(dta_bal$cons_maize_yes[dta_bal$sms != "yes"], na.rm=T)
res_itt_disp[1,2,h] <- summary(lm(as.formula(paste("cons_maize_yes",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_disp[2,2,h] <- summary(lm(as.formula(paste("cons_maize_yes",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_disp[1,3,h] <- ifelse(totrep >0, RI("cons_maize_yes",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("cons_maize_yes",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

### sold maize?

res_itt_disp[3,1,h] <- mean(dta_bal$sold_maize[dta_bal$sms != "yes"], na.rm=T)
res_itt_disp[4,1,h] <- sd(dta_bal$sold_maize[dta_bal$sms != "yes"], na.rm=T)
res_itt_disp[3,2,h] <- summary(lm(as.formula(paste("sold_maize",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_disp[4,2,h] <- summary(lm(as.formula(paste("sold_maize",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_disp[3,3,h] <- ifelse(totrep >0, RI("sold_maize",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("sold_maize",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])
### kept maize for seed?
res_itt_disp[5,1,h] <- mean(dta_bal$save_seed[dta_bal$sms != "yes"], na.rm=T)
res_itt_disp[6,1,h] <- sd(dta_bal$save_seed[dta_bal$sms != "yes"], na.rm=T)
res_itt_disp[5,2,h] <- summary(lm(as.formula(paste("save_seed",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_disp[6,2,h] <- summary(lm(as.formula(paste("save_seed",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_disp[5,3,h] <- ifelse(totrep >0, RI("save_seed",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("save_seed",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

res_itt_disp <- Bcorr(c("cons_maize_yes","sold_maize","save_seed"),dta_bal, res_itt_disp ,h)

dta_bal2 <- dta_bal
dta_bal2$save_seed <- !dta_bal2$save_seed

indexer <- FW_index(treatment, c("cons_maize_yes", "sold_maize", "save_seed"),dta_bal2, nr_repl=totrep)
res_itt_disp[7,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$sms != "yes"])
res_itt_disp[8,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$sms != "yes"])
res_itt_disp[7,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_disp[8,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_disp[7,3,h] <-  indexer[[2]]

################################ welfare #############################

## better off than average
res_itt_wel[1,1,h] <- mean(dta_bal$better_av[dta_bal$sms != "yes"], na.rm=T)
res_itt_wel[2,1,h] <- sd(dta_bal$better_av[dta_bal$sms != "yes"], na.rm=T)
res_itt_wel[1,2,h]  <- summary(lm(as.formula(paste("better_av",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_wel[2,2,h]  <- summary(lm(as.formula(paste("better_av",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_wel[1,3,h]  <- ifelse(totrep >0, RI("better_av",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("better_av",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

## better off than 6mo
res_itt_wel[3,1,h] <- mean(dta_bal$better_6m[dta_bal$sms != "yes"], na.rm=T)
res_itt_wel[4,1,h] <- sd(dta_bal$better_6m[dta_bal$sms != "yes"], na.rm=T)
res_itt_wel[3,2,h]  <- summary(lm(as.formula(paste("better_6m",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_wel[4,2,h]  <- summary(lm(as.formula(paste("better_6m",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_wel[3,3,h]  <- ifelse(totrep >0, RI("better_6m",treatment , dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("better_6m",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

#can eat preferred foods
res_itt_wel[5,1,h] <- mean(dta_bal$eatpref[dta_bal$sms != "yes"], na.rm=T)
res_itt_wel[6,1,h] <- sd(dta_bal$eatpref[dta_bal$sms != "yes"], na.rm=T)
res_itt_wel[5,2,h]  <-  summary(lm(as.formula(paste("eatpref",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_wel[6,2,h]  <-  summary(lm(as.formula(paste("eatpref",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_wel[5,3,h]  <- ifelse(totrep >0, RI("eatpref",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("eatpref",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

#can eat enough foods
res_itt_wel[7,1,h] <- mean(dta_bal$eatenough[dta_bal$sms != "yes"], na.rm=T)
res_itt_wel[8,1,h] <- sd(dta_bal$eatenough[dta_bal$sms != "yes"], na.rm=T)
res_itt_wel[7,2,h]  <- summary(lm(as.formula(paste("eatenough",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_itt_wel[8,2,h]  <- summary(lm(as.formula(paste("eatenough",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_itt_wel[7,3,h]  <- ifelse(totrep >0, RI("eatenough",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("eatenough",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

#consumption - logged and trimmed
dta_bal2 <- subset(dta_bal, cons>0)
dta_bal2$log_cons <- log(dta_bal2$cons)
dta_trim <- trim("log_cons", dta_bal2, .05)

res_itt_wel[9,1,h] <- mean(dta_trim$log_cons[dta_trim$sms != "yes"], na.rm=T)
res_itt_wel[10,1,h] <- sd(dta_trim$log_cons[dta_trim$sms != "yes"], na.rm=T)
res_itt_wel[9,2,h]  <- summary(lm(as.formula(paste("log_cons",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_itt_wel[10,2,h]  <- summary(lm(as.formula(paste("log_cons",treatment,sep = "~")), data=dta_trim))$coefficients[2,2]
res_itt_wel[9,3,h]  <- ifelse(totrep >0, RI("log_cons",treatment , dta_bal2, nr_repl = totrep), summary(lm(as.formula(paste("log_cons",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

res_itt_wel <- Bcorr(c("better_av","better_6m","eatpref","eatenough","log_cons"),dta_trim, res_itt_wel ,h)

	indexer <- FW_index(treatment, c("better_av", "better_6m", "eatpref","eatenough","log_cons"),dta_trim, nr_repl=totrep)
res_itt_wel[11,1,h] <-  mean(indexer[[3]]$index[indexer[[3]]$sms != "yes"])
res_itt_wel[12,1,h] <-  sd(indexer[[3]]$index[indexer[[3]]$sms != "yes"])
res_itt_wel[11,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_wel[12,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_wel[11,3,h] <-  indexer[[2]]

prod_plot[12,1] <- "welfare"
prod_plot[12,3:4] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index[indexer[[3]]$sms != "yes"])
prod_plot[12,2] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index[indexer[[3]]$sms != "yes"])

prod_plot$grp[9:12] <- "sms"

prod_plot$x <- factor(prod_plot$x, levels=rev(prod_plot$x))
prod_plot$grp <- factor(prod_plot$grp, levels = c("video","ivr", "sms"))

pdf("/home/bjvca/data/projects/digital green/endline/results/summaryplot_mode.pdf")
credplot.gg(prod_plot)
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









