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

prod_plot <- array(NA, c(6,4,6))
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
df <- data.frame(cbind(c("husband","wife","together"),c(wtd.mean(dta$share_plots_man_decide, dta$weight_av) ,
wtd.mean(dta$share_plots_woman_decide, dta$weight_av),
wtd.mean(dta$share_plots_both_decide, dta$weight_av)  )))
names(df) <- c("decision","share")
df$share <- as.numeric(as.character(df$share))
df$share <- round(df$share,2)
pdf("/home/bjvca/data/projects/digital green/papers/DP_cooperation/results/decision_to_grow_maize.pdf")
par(mfrow=c(1,3),mar=c(10,4.1,4.1,2.1)) 

barplot(df$share,
main = "Couple reports decisions \nare made by:",
xlab = "",
ylab = "share of plots",
names.arg = df$decision,
col = "darkred", ylim=c(0,0.5), cex.axis=1.5, cex.names=1.5, cex.lab = 1.5,las=2)

dta_copy <- dta
df <- data.frame(cbind(c("husband","wife","together"),c(wtd.mean(dta$share_man_man_decide, dta$weight_av) ,
wtd.mean(dta$share_woman_man_decide, dta$weight_av),
wtd.mean(dta$share_both_man_decide, dta$weight_av)  )))
names(df) <- c("decision","share")
df$share <- as.numeric(as.character(df$share))
df$share <- round(df$share,2)

barplot(df$share,
main = "Husband reports decisions \nare made by:",
xlab = "",
ylab = "",
names.arg = df$decision,
col = "darkred", ylim=c(0,0.5), cex.axis=1.5, cex.names=1.5, axes=F,las=2)

df <- data.frame(cbind(c("husband","wife","together"),c(wtd.mean(dta$share_man_woman_decide, dta$weight_av) ,
wtd.mean(dta$share_woman_woman_decide, dta$weight_av),
wtd.mean(dta$share_both_woman_decide, dta$weight_av)  )))
names(df) <- c("decision","share")
df$share <- as.numeric(as.character(df$share))
df$share <- round(df$share,2)


#grid.arrange(p1, p2,p3, ncol=3)

barplot(df$share,
main = "Wife reports decisions \nare made by:",
xlab = "",
ylab = "",
names.arg = df$decision,
col = "darkred", ylim=c(0,0.5), cex.axis=1.5, cex.names=1.5, axes=F, las=2)
dev.off()
 
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


################################   ############################

res_itt_know[1,1,h] <- mean(dta_bal$share_plots_both_decide,na.rm=T)
res_itt_know[2,1,h] <- sd(dta_bal$share_plots_both_decide,na.rm=T)
res_itt_know[1,2,h] <- summary(lm(as.formula(paste(paste("share_plots_both_decide",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_know[2,2,h] <- summary(lm(as.formula(paste(paste("share_plots_both_decide",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_know[1,3,h] <- ifelse(totrep >0, RI("share_plots_both_decide", treatment ,ctrls, dta_bal,  totrep, "weight"),summary(lm(as.formula(paste(paste("share_plots_both_decide",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,4])
res_itt_know[1,4,h] <- nobs(lm(as.formula(paste(paste("share_plots_both_decide",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))

es_itt_know[1,1,h] <- mean(dta_bal$share_plots_both_decide,na.rm=T)
res_itt_know[2,1,h] <- sd(dta_bal$share_plots_both_decide,na.rm=T)
res_itt_know[1,2,h] <- summary(lm(as.formula(paste(paste("share_plots_both_decide",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,1]
res_itt_know[2,2,h] <- summary(lm(as.formula(paste(paste("share_plots_both_decide",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,2]
res_itt_know[1,3,h] <- ifelse(totrep >0, RI("share_plots_both_decide", treatment ,ctrls, dta_bal,  totrep, "weight"),summary(lm(as.formula(paste(paste("share_plots_both_decide",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))$coefficients[2,4])
res_itt_know[1,4,h] <- nobs(lm(as.formula(paste(paste("share_plots_both_decide",treatment, sep="~"),ctrls,sep="+")) ,data=dta_bal, weights = weight))



indexer <- FW_index(treatment, c("share_plots_both_decide", "better_6m", "eatpref","eatenough","log_cons"),ctrls,w_int2 = "weight", data =dta_trim, nr_repl=totrep )
res_itt_wel[11,1,h] <-  mean(indexer[[3]]$index)
res_itt_wel[12,1,h] <-  sd(indexer[[3]]$index)
res_itt_wel[11,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_itt_wel[12,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_itt_wel[11,3,h] <-  indexer[[2]]
res_itt_wel[11,4,h] <-  nobs(indexer[[1]])

prod_plot[6,1,h] <- "welfare"
prod_plot[6,3:4,h] <- confint(indexer[[1]], level=.9)[2,]/ sd(indexer[[3]]$index)
prod_plot[6,2,h] <- summary(indexer[[1]])$coefficients[2,1] / sd(indexer[[3]]$index)



res_itt_wel[13,1:3,h] <- RI_FWER(c("better_av", "better_6m", "eatpref","eatenough","log_cons"),indep = treatment, ctrls=ctrls ,dta =dta_trim, p_vals = res_itt_wel[c(1,3,5,7,9),3,h], nr_repl = totrep, w_int="weight")



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
plotter$grp <- 1

plotter$x <-  factor(plotter$x, levels=(c('welfare','production','seed','fertilizer','adoption','knowledge')))
pdf("/home/bjvca/data/projects/digital green/papers/DP_cooperation/results/assynm_summary.pdf")
credplot.gg(plotter,'SDs','reducing info asymmetry',levels(plotter$x),.25)+ theme(legend.position = "none")
dev.off()

plotter <- data.frame(prod_plot[,,4])
plotter$y <- as.numeric(as.character(plotter$y))
plotter$ylo <- as.numeric(as.character(plotter$ylo))
plotter$yhi <- as.numeric(as.character(plotter$yhi))
plotter$grp <- 1

plotter$x <-  factor(plotter$x, levels=(c('welfare','production','seed','fertilizer','adoption','knowledge')))
pdf("/home/bjvca/data/projects/digital green/papers/DP_cooperation/results/HHapproach_summary.pdf")
credplot.gg(plotter,'SDs','project HH approach',levels(plotter$x),.25)+ theme(legend.position = "none")

