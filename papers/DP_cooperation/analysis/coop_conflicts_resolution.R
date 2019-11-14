rm(list=ls())
stopCluster(cl)
source("/home/bjvca/data/projects/digital green/papers/DP_cooperation/analysis/init_gender.R")
baseline <-  read.csv("/home/bjvca/data/projects/digital green/baseline/base_merge.cvs")

## uncomment lines below if run on AWS
#wget https://www.dropbox.com/s/z9p0qd4t29xiy14/AWS_coop.csv?dl=0
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

res_coop_conflict <- array(NA, c(4,4,7)) 
rownames(res_coop_conflict) <- c("Spouses tell each other in case of disagreement (1=never ... 5=always)","","Spouses discuss and come to agreement (Yes/No)","")

conf_plot <- array(NA, c(2,5,7))
colnames(conf_plot) <-  c("x","y","ylo","yhi","grp")

cl <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
registerDoParallel(cl)

### function definitions 
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
		return(abs(ifelse(is.null(ctrls),summary(lm(as.formula(paste(dep,indep,sep="~")), weights=weights, data=data.frame(dta_sim)))$coefficients[2,1],summary(lm(as.formula(paste(paste(dep,indep,sep="~"), ctrls,sep="+")), weights=weights, data=dta_sim))$coefficients[2,1])) > abs(crit) )
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
return(unlist(lapply(deps, function(dvar)  summary(lm(as.formula(paste(dvar,indep,sep="~")), weights=weights,data=dta_sim))$coefficients[2,4])))
} else {
return(unlist(lapply(deps, function(dvar)  summary(lm(as.formula(paste(paste(dvar,indep,sep="~"),ctrls, sep="+")), weights=weights,data=dta_sim))$coefficients[2,4])))
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


#set totrep to zero if you do not want simulation based inferecne
totrep <- 0
####

for (h in 1:6) {
if (h==1) {
############################################ H1: info asymmetry: rec=individual vs rec=couple #########################################################
dta <- dta_copy
dta$weights <- 1
dta$weights[dta$recipient == "female"] <-  1131/1144
dta$weights[dta$recipient == "male"] <- 1
dta <- merge(dta,baseline, by="hhid")

ctrls <- "maizeage+maizeeduc+maizehh_no+maizeprinfo_receiv+maizeprinfo_receiv_spouse+maizeprinfo_receiv_spouse+maizemobile" 

treatment <- "(recipient == 'couple') +as.factor(messenger)*ivr*sms + ivr*(recipient == 'couple')*sms" 
} else if (h==4) {
############################################ H2: promote collective approach ###################################################
dta <- dta_copy
dta$weights <- 1
dta$weights[dta$messenger == "female"] <-  1
dta$weights[dta$messenger == "male"] <- 1102/1114
treatment <- "(messenger == 'couple') +as.factor(recipient)*ivr*sms + ivr*(messenger == 'couple')*sms"
dta <- merge(dta,baseline, by="hhid")
ctrls <- "maizeprinput_use" 
} else if (h==2) {
############################################ H1a: info assym men vs couple ###################################################
dta <- subset(dta_copy, recipient != "female")
dta$weights <- 1
treatment <- "(recipient == 'couple') +as.factor(messenger)*ivr*sms + ivr*(recipient == 'couple')*sms" 
dta <- merge(dta,baseline, by="hhid")

ctrls <- "maizeprinfo_receiv_spouse+maizeprinput_use" 
} else if (h==3) {
############################################ H1b: info assym women vs couple ###################################################
dta <- subset(dta_copy, recipient != "male")
dta$weights <- 1
treatment <- "(recipient == 'couple') +as.factor(messenger)*ivr*sms + ivr*(recipient == 'couple')*sms" 
dta <- merge(dta,baseline, by="hhid")

ctrls <- "maizeage+maizeeduc+maizehh_no+maizeprinfo_receiv+maizemobile" 
} else if (h==5) {
############################################ H2a: info assym men vs couple ###################################################
dta <- subset(dta_copy, messenger != "female")
dta$weights <- 1
treatment <- "(messenger == 'couple') +as.factor(recipient)*ivr*sms + ivr*(messenger == 'couple')*sms" 
dta <- merge(dta,baseline, by="hhid")
ctrls <- "maizeprinput_use" 
} else if (h==6) {
############################################ H2b: info assym women vs couple ###################################################
dta <- subset(dta_copy, messenger != "male")
dta$weights <- 1
treatment <- "(messenger == 'couple') +as.factor(recipient)*ivr*sms + ivr*(messenger == 'couple')*sms"
dta <- merge(dta,baseline, by="hhid")
ctrls <- "maizeprinput_use" 
}

print(h)

################################################### tell_each_other and spouses_listen  #####################################################



wtd.mean(as.numeric(dta$maizeeduc>2), na.rm=T)
sqrt(wtd.var(as.numeric(dta$maizeeduc>2), na.rm=T))






res_coop_conflict[1,1,h] <- wtd.mean(as.numeric(dta$tell_each_other), dta$weight_av, na.rm=T) 
res_coop_conflict[2,1,h] <- sqrt(wtd.var(as.numeric(dta$tell_each_other), dta$weight_av, na.rm=T))
res_coop_conflict[1,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("tell_each_other",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("tell_each_other",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_coop_conflict[2,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("tell_each_other",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("tell_each_other",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_coop_conflict[1,3,h] <- ifelse(totrep >0, RI("tell_each_other",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("tell_each_other",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("tell_each_other",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_coop_conflict[2,3,h] <- nobs(lm(as.formula(paste("tell_each_other",treatment, sep="~")) ,data=dta))

conf_plot[1,1,h] <- "Spouses tell each other"
conf_plot[1,3:4,h] <- confint(lm(as.formula(paste(paste("tell_each_other",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))[2,]/ res_coop_conflict[2,1,h]
conf_plot[1,2,h] <- res_coop_conflict[1,2,h] / res_coop_conflict[2,1,h]


res_coop_conflict[3,1,h] <- wtd.mean(as.numeric(dta$spouses_listen), dta$weight_av, na.rm=T) 
res_coop_conflict[4,1,h] <- sqrt(wtd.var(as.numeric(dta$spouses_listen), dta$weight_av, na.rm=T))
res_coop_conflict[3,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("spouses_listen",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,1],summary(lm(as.formula(paste(paste("spouses_listen",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,1])
res_coop_conflict[4,2,h] <- ifelse(is.null(ctrls),summary(lm(as.formula(paste("spouses_listen",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,2],summary(lm(as.formula(paste(paste("spouses_listen",treatment, sep="~"),ctrls, sep="+")) ,weights=weights,data=dta))$coefficients[2,2])
res_coop_conflict[3,3,h] <- ifelse(totrep >0, RI("spouses_listen",treatment , ctrls,w_int="weights", dta, nr_repl = totrep),ifelse(is.null(ctrls),summary(lm(as.formula(paste("spouses_listen",treatment, sep="~")) ,weights=weights,data=dta))$coefficients[2,4],summary(lm(as.formula(paste(paste("spouses_listen",treatment, sep="~"),ctrls, sep="+")),weights=weights,data=dta))$coefficients[2,4]))
res_coop_conflict[4,3,h] <- nobs(lm(as.formula(paste("spouses_listen",treatment, sep="~")) ,data=dta))

conf_plot[2,1,h] <- "Spouses listen to each other"
conf_plot[2,3:4,h] <- confint(lm(as.formula(paste(paste("spouses_listen",treatment, sep="~"),ctrls,sep="+")) ,data=dta, weights = weights))[2,]/ res_coop_conflict[4,1,h]
conf_plot[2,2,h] <- res_coop_conflict[3,2,h] / res_coop_conflict[4,1,h]

}


save(res_coop_conflict, file = "res_coop_conflict.RData")

print(res_coop_conflict)

##plotting
plotter <- data.frame(conf_plot[,,1])
plotter$y <- as.numeric(as.character(plotter$y))
plotter$ylo <- as.numeric(as.character(plotter$ylo))
plotter$yhi <- as.numeric(as.character(plotter$yhi))
plotter$grp <- "reducing info asymmetry"

plotter2 <- data.frame(conf_plot[,,4])
plotter2$y <- as.numeric(as.character(plotter2$y))
plotter2$ylo <- as.numeric(as.character(plotter2$ylo))
plotter2$yhi <- as.numeric(as.character(plotter2$yhi))
plotter2$grp <- "HH cooperative approach"

plotter <- rbind(plotter,plotter2)

plotter$x <-  factor(plotter$x, levels=rev((c('Spouses tell each other','Spouses listen to each other'))))
png("/home/bjvca/data/projects/digital green/papers/DP_cooperation/results/conflict_resolution.png", units="px", height=3200, width= 6400, res=600)

credplot.gg(plotter,'SDs','',levels(plotter$x),.5)
dev.off()

## analysis of likert scales for telling each other
#bin it
dta <- dta_copy
dta <- subset(dta, interview_status == "couple interviewed")
dta$tell_each_other_binned <- cut(dta$tell_each_other, c(1,2.5,3.5,4.5,5))
levels(dta$tell_each_other_binned) <- c("Rarely","Sometimes","Mostly","Always")
pdf("/home/bjvca/data/projects/digital green/papers/DP_cooperation/results/likert_plot_tell_each_other.pdf")
par(mfrow=c(1,3), xpd=NA) 
colfunc<-colorRampPalette(c("#104E8B", "#6E8DAB","#CCCCCC"))
barplot(prop.table(table(dta$tell_each_other_binned, dta$recipient=="couple"),2), col=colfunc(4), main="Reducing information \nasymmetry", names.arg=c("Ctrl","Treat"), cex.main=1.5,cex.axis=1.5, cex.names=1.5)
dta <- subset(dta, interview_status == "couple interviewed")
barplot(prop.table(table(dta$tell_each_other_binned, dta$messenger=="male"),2), col=colfunc(4), main="HH cooperative \napproach", names.arg=c("Ctrl","Treat"), cex.main=1.5,cex.axis=1.5, cex.names=1.5)
barplot(prop.table(table(dta$tell_each_other_binned, dta$messenger=="male"),2), col=colfunc(4),plot=F)
legend("right", legend=rev(c("Rarely","Sometimes","Mostly","Always")), cex=1.5, bty="n", fill=rev(colfunc(4)), inset=c(-1.3,0))
dev.off()

chisq.test(table(dta$tell_each_other_binned, dta$recipient=="couple")) 
wilcox.test(as.numeric(dta$tell_each_other_binned)~dta$recipient=="couple", mu=0)

dta <- dta_copy
dta <- subset(dta_copy, messenger != "female")
dta <- subset(dta, interview_status == "couple interviewed")
dta$tell_each_other_binned <- cut(dta$tell_each_other, c(1,2.5,3.5,4.5,5))
levels(dta$tell_each_other_binned) <- c("Rarely","Sometimes","Mostly","Always")
pdf("/home/bjvca/data/projects/digital green/papers/DP_cooperation/results/likert_plot_tell_each_other_presentation.pdf")
par(mfrow=c(1,3), xpd=NA) 
colfunc<-colorRampPalette(c("red","yellow","green"))
barplot(prop.table(table(dta$tell_each_other_binned, dta$recipient=="male"),2), col=colfunc(4), main="Reducing information \nasymmetry", names.arg=c("Ctrl","Treat"), cex.main=1.5,cex.axis=1.5, cex.names=1.5)

barplot(prop.table(table(dta$tell_each_other_binned, dta$messenger=="male"),2), col=colfunc(4), main="HH cooperative \napproach", names.arg=c("Ctrl","Treat"), cex.main=1.5,cex.axis=1.5, cex.names=1.5)
barplot(prop.table(table(dta$tell_each_other_binned, dta$messenger=="male"),2), col=colfunc(4),plot=F)
legend("right", legend=rev(c("Rarely","Sometimes","Mostly","Always")), cex=1.5, bty="n", fill=rev(colfunc(4)), inset=c(-1.3,0))
dev.off()

chisq.test(table(  dta$messenger=="couple",dta$tell_each_other_binned)) 
wilcox.test(as.numeric(dta$tell_each_other_binned)~dta$messenger=="male", mu=0)

colours <- c("#CCCCCC", "#6E8DAB", "#104E8B")


