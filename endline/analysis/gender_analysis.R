rm(list=ls())
source("/home/bjvca/data/projects/digital green/endline/data/init.R")
#source("functions.R")
#dta <- read.csv("AWS.csv")

#set totrep to zero if you do not want simulation based inferecne
totrep <- 0

set.seed(07032018)
dta <- subset(dta, !is.na(interview_status))
dta$messenger <- as.character(dta$messenger)

######################################################################### indexing results arrays ##################################################################
res_know <- array(NA, c(10,4,4)) 
rownames(res_know) <- c("know_space","","know_combine","","know_weed","", "know_armyworm","","know_ind","")
res_know_m <- array(NA, c(10,4,4)) 
rownames(res_know_m) <- c("know_space","","know_combine","","know_weed","", "know_armyworm","","know_ind","")
res_know_w <- array(NA, c(10,4,4)) 
rownames(res_know_w) <- c("know_space","","know_combine","","know_weed","", "know_armyworm","","know_ind","")
res_decision_b <- array(NA, c(10,4,4)) 
rownames(res_decision_b) <- c("know_space","","know_combine","","know_weed","", "know_armyworm","","know_ind","")

res_decision_b <- array(NA, c(4,4,4)) 
rownames(res_decision_b) <- c("both tell","","agreement","")
res_decision_m <- array(NA, c(4,4,4)) 
rownames(res_decision_m) <- c("man_tells_wife","","wife_listens","")
res_decision_w <- array(NA, c(4,4,4)) 
rownames(res_decision_w) <- c("wife_tells_man","","man_listens","")

res_dec_m <- array(NA, c(20,4,4))
rownames(res_dec_m) <- c("first_day","","space","","striga","","weed","", "use_fert","","seed","","combiner","","bought_seed","","chem","","index","")
res_dec_w <- array(NA, c(20,4,4))
rownames(res_dec_w) <- c("first_day","","space","","striga","","weed","", "use_fert","","seed","","combiner","","bought_seed","","chem","","index","")
res_dec_b <- array(NA, c(20,4,4))
rownames(res_dec_b) <- c("first_day","","space","","striga","","weed","", "use_fert","","seed","","combiner","","bought_seed","","chem","","index","")

res_hh_pract <- array(NA, c(22,4,4))
rownames(res_hh_pract) <- c("first_day","","space","","striga","","weed","", "use_fert","","seed","","combiner","","bought_seed","","chem","","labour","","pract_index","")
res_hh_fert <- array(NA, c(6,4,4))
rownames(res_hh_fert) <- c("use_DAP","","use_urea","","use_organic","")
res_hh_seed <- array(NA, c(4,4,4))
rownames(res_hh_seed) <- c("hybrid","","opv","")
res_pract_m <- array(NA, c(20,4,4))
rownames(res_pract_m) <- c("first_day","","space","","striga","","weed","", "use_fert","","seed","","combiner","","bought_seed","","chem","","index","")
res_pract_w <- array(NA, c(20,4,4))
rownames(res_pract_w) <- c("first_day","","space","","striga","","weed","", "use_fert","","seed","","combiner","","bought_seed","","chem","","index","")
res_pract_b <- array(NA, c(20,4,4))
rownames(res_pract_b) <- c("first_day","","space","","striga","","weed","", "use_fert","","seed","","combiner","","bought_seed","","chem","","index","")
## drop the control
dta <- subset(dta, messenger != "ctrl")

############################################################### function definitions (RI) #################################################################
### a function to calculate the single sided RI p-values
### this only works for all observations!!!
RI <- function(dep, indep, dta , nr_repl = 1000,h=h) {
	#indep <-  "(messenger != 'male')+ivr+sms+as.factor(recipient) + as.factor(messenger)"

#dep <- "know_space_m"
#dta <- dta_bal
#nr_repl <- 1000

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
if (h==1) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","female")
} else if (h==2) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","couple")
} else if (h==3) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female")
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["female",]>0]),"female","couple"))

} else if (h==4) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", "male")
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["female",]>0]),"female","couple"))

}

		return(abs(coef(lm(as.formula(paste(dep,indep,sep="~")), data=dta_sim))[2]) > abs(crit) )
	}
	return(sum(oper)/nr_repl)
}


plot_RI_dec <- function(data, man,treatment, nr_repl = 1000, h=h) {
#data <- dta_bal
#man <- "dectime_man"
#treatment <-  "(messenger == 'female') +ivr+sms+as.factor(recipient)+ as.factor(messenger)"
#nr_repl <- 0
#plot_RI(dta, man = "dec_woman", out_sp1 =c("grp1a55a" ,"grp2b55b", "grp3c55b","grp4d55b","grp5e55b"),out_sp2 =c("spouse2grp_sp1f55a","spouse2grp_sp2g55b","spouse2grp_sp3h55b","spouse2group_sp4j55b","spouse2grp5_sp5k55b"),treatment , 100)


##function to perfrom RI om plot level
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

space_ind <- reshape(data[c("messenger","recipient","gender1","ivr","sms","called","totsms","hhid","distID", "subID","vilID", dec_vars)], varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
	crit <- summary(lm(as.formula(paste("decide",treatment,sep="~")), data=space_ind))$coefficients[2,1]
print( summary(lm(as.formula(paste("decide",treatment,sep="~")), data=space_ind)))
mean_male <- ifelse(h <=2,mean(space_ind$decide[space_ind$messenger == "male"], na.rm=T),mean(space_ind$decide[space_ind$recipient == "male"], na.rm=T))
sd_male <- ifelse(h <=2,sd(space_ind$decide[space_ind$messenger == "male"], na.rm=T),sd(space_ind$decide[space_ind$recipient == "male"], na.rm=T))


dta <- space_ind[!duplicated(space_ind$hhid),]
dta <- dta %>% mutate(treat = group_indices_(dta, .dots=c("recipient", "messenger"))) 
### allocates unique ID based on treatment cell status and village
dta <- dta %>% mutate(uniqID = group_indices_(dta, .dots=c("distID", "subID","vilID"))) 

	### the NULL

	dta$time <- NULL
	dta<- data.table(dta)
	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
 		dta_sim <- merge(space_ind,setDT(dta)[,perm:=sample(treat),by = (uniqID)][,c("hhid","perm")], by="hhid")
if (h==1) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","female")
} else if (h==2) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","couple")
} else if (h==3) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female")
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["female",]>0]),"female","couple"))

} else if (h==4) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", "male")
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["female",]>0]),"female","couple"))

}
		return(abs(coef(lm(as.formula(paste("decide",treatment,sep="~")), data=dta_sim))[2]) > abs(crit) )
	}
	return(list(summary(lm(as.formula(paste("decide",treatment,sep="~")), data=space_ind))$coefficients[2,1],summary(lm(as.formula(paste("decide",treatment,sep="~")), data=space_ind))$coefficients[2,2], summary(lm(as.formula(paste("decide",treatment,sep="~")), data=space_ind))$coefficients[2,4],sum(oper)/nr_repl,mean_male,sd_male))
}

plot_RI <- function(data, man, out_sp1,out_sp2,treatment,nr_repl = 1000, trimlog=FALSE, h=h) {
##function to perfrom RI om plot level
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

space_ind <- merge(merge(reshape(data[c("messenger","recipient","gender1","ivr","sms","called","totsms","hhid","distID", "subID","vilID", dec_vars)], varying = dec_vars,v.names="decide", idvar="hhid", direction="long"), reshape(data[c(out_sp1,"hhid")], varying = out_sp1,v.names="outcome_sp1", idvar="hhid", direction="long"), by=c("hhid","time")),reshape(data[c(out_sp2,"hhid")], varying = out_sp2,v.names="outcome_sp2", idvar="hhid", direction="long"), by=c("hhid","time"))

space_ind$outcome <- NA
if (grepl("woman", man)) {
space_ind$outcome <- ifelse(space_ind$gender1=="woman",space_ind$outcome_sp1, space_ind$outcome_sp2)
} else if  (grepl("both", man)) {
#inelegant way to test if variable is not binary
if (length(table(space_ind$outcome_sp1 ))>3 ) {
space_ind$outcome <- rowMeans(cbind(space_ind$outcome_sp1, space_ind$outcome_sp2), na.rm=T)
} else {
space_ind$outcome <- rowSums(cbind(space_ind$outcome_sp1, space_ind$outcome_sp2), na.rm=T)>0
space_ind$outcome[is.na(space_ind$outcome_sp1) & is.na(space_ind$outcome_sp2)] <- NA
}

} else {
space_ind$outcome <- ifelse(space_ind$gender1=="man",space_ind$outcome_sp1, space_ind$outcome_sp2)
}

if (trimlog==TRUE) {
space_ind <- subset(space_ind, outcome>0)
space_ind$outcome <- log(space_ind$outcome)

space_ind <- trim("outcome", space_ind, .05)
}

space_ind <- subset(space_ind,decide == 1)
	crit <- summary(lm(as.formula(paste("outcome",treatment,sep="~")), data=space_ind))$coefficients[2,1]
print( summary(lm(as.formula(paste("outcome",treatment,sep="~")), data=space_ind)))
mean_male <- ifelse(h <=2,mean(space_ind$outcome[space_ind$messenger == "male"], na.rm=T),mean(space_ind$outcome[space_ind$recipient == "male"], na.rm=T))
sd_male <- ifelse(h <=2,sd(space_ind$outcome[space_ind$messenger == "male"], na.rm=T),sd(space_ind$outcome[space_ind$recipient == "male"], na.rm=T))

dta <- space_ind[!duplicated(space_ind$hhid),]


dta <- dta %>% mutate(treat = group_indices_(dta, .dots=c("recipient", "messenger"))) 
### allocates unique ID based on treatment cell status and village
dta <- dta %>% mutate(uniqID = group_indices_(dta, .dots=c("distID", "subID","vilID"))) 

	### the NULL

	dta$time <- NULL
	dta<- data.table(dta)
	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
 		dta_sim <- merge(space_ind,setDT(dta)[,perm:=sample(treat),by = (uniqID)][,c("hhid","perm")], by="hhid")
		if (h==1) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","female")
} else if (h==2) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","couple")
} else if (h==3) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female")
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["female",]>0]),"female","couple"))

} else if (h==4) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", "male")
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["female",]>0]),"female","couple"))

}
		return(abs(coef(lm(as.formula(paste("outcome",treatment,sep="~")), data=dta_sim))[2]) > abs(crit) )
	}
	return(list(summary(lm(as.formula(paste("outcome",treatment,sep="~")), data=space_ind))$coefficients[2,1],summary(lm(as.formula(paste("outcome",treatment,sep="~")), data=space_ind))$coefficients[2,2], summary(lm(as.formula(paste("outcome",treatment,sep="~")), data=space_ind))$coefficients[2,4],sum(oper)/nr_repl,mean_male,sd_male))
}


trim <- function(var, dataset, trim_perc=.1) {
### function for triming a dataset *dataset* on a variable *var*
return( subset(dataset,dataset[var] > quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)), na.rm=T)[1] & dataset[var] < quantile(dataset[var], c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[2]) )
}

FW_index <- function(treat, indexer, data,revcols = NULL, nr_repl=0, h_ind= h) {
### function to make family wise index using covariance as weights (following http://cyrussamii.com/?p=2656)
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
	sig <- RI("index" ,treat , data, nr_repl = nr_repl,h_ind)
} else {
	sig <- summary(lm(as.formula(paste("index",treat,sep="~")) , data=data))$coefficients[2,4]
}
return(list(mod,sig, data))
}


RI_FWER <- function(deps, indep, dta ,p_vals , nr_repl = 1000, h_ind = h) {
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
if (h_ind==1) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","female")
} else if (h_ind==2) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","couple")
} else if (h_ind==3) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female")
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["female",]>0]),"female","couple"))

} else if (h_ind==4) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", "male")
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["female",]>0]),"female","couple"))

}
return(unlist(lapply(deps, function(dvar) summary(lm(as.formula(paste(dvar,indep,sep="~")), data=dta_sim))$coefficients[2,4])))
				}

thresholds <- seq(0, 0.1, length.out = 10000)
type_I_rate <- sapply(thresholds, threshold_finder)
return( list=c(thresholds[max(which(type_I_rate <= 0.05))],thresholds[max(which(type_I_rate <= 0.01))], thresholds[max(which(type_I_rate <= 0.001))]))
}


plot_RI_dec_FWER <- function(man, treatment, data,p_vals, nr_repl = 1000, h=h) {
#plot_RI_dec_FWER(man= c("dectime_man","decspace_man","decstriga_man", "decweed_man", "decfert_man", "decseed_man","deccombiner_man","decbuyseed_man","decchem_man"), treatment, data=dta_bal,p_vals=c(0.00000,0.00000,0.00000,0.00000,0.00000,0.00400,0.00000,0.04990,	0.00260), nr_repl = 100, h=3)

##function to perfrom RI_FWER om plot level
threshold_finder<- function(threshold){
  mean(apply(oper, 2, x <- function(x) sum(x <= threshold) > 0 ))
}
dec_vars <- paste(man[1],paste("_pl",1:5, sep=""), sep="")

space_ind <- reshape(data[c("messenger","recipient","gender1","ivr","sms","called","totsms","hhid","distID", "subID","vilID", dec_vars)], varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(space_ind)[dim(space_ind)[2]] <- man[1]

for (dep_ind in man[2:length(man)]) {
dec_vars <- paste(dep_ind,paste("_pl",1:5, sep=""), sep="")
## merge long to space_ind
dec_vars <- paste(dep_ind,paste("_pl",1:5, sep=""), sep="")
space_ind <- merge(space_ind,reshape(data[c("hhid", dec_vars)], varying = dec_vars,v.names="decide", idvar="hhid", direction="long"), by =c("hhid","time") )
names(space_ind)[dim(space_ind)[2]] <- dep_ind
}

dta <- space_ind[!duplicated(space_ind$hhid),]
dta <- dta %>% mutate(treat = group_indices_(dta, .dots=c("recipient", "messenger"))) 
### allocates unique ID based on treatment cell status and village
dta <- dta %>% mutate(uniqID = group_indices_(dta, .dots=c("distID", "subID","vilID"))) 

	### the NULL

	dta$time <- NULL
	dta<- data.table(dta)
	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
 		dta_sim <- merge(space_ind,data.frame(setDT(dta)[,perm:=sample(treat),by = (uniqID)][,c("hhid","perm")]), by="hhid")
if (h==1) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","female")
} else if (h==2) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male","couple")
} else if (h==3) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female")
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["female",]>0]),"female","couple"))

} else if (h==4) {
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", "male")
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["female",]>0]),"female","couple"))

}
return(unlist(lapply(man, function(dvar) summary(lm(as.formula(paste(dvar,treatment,sep="~")), data=dta_sim))$coefficients[2,4])))
			}		

thresholds <- seq(0, 0.1, length.out = 10000)
type_I_rate <- sapply(thresholds, threshold_finder)
return( list=c(thresholds[max(which(type_I_rate <= 0.05))],thresholds[max(which(type_I_rate <= 0.01))], thresholds[max(which(type_I_rate <= 0.001))]))
}


######################################## some data transformations for plot level analysis ############################################

dta[c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26")] <- dta[c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26")]==3
dta[c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26")] <- dta[c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26")] == 3

dta[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")] <- dta[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")] == "Yes"
dta[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29")] <- dta[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29")] == "Yes"

dta[ paste("dec_man",paste("_pl",1:5, sep=""), sep="")] <- dta[ paste("dec_man",paste("_pl",1:5, sep=""), sep="")] >0
dta[ paste("dec_woman",paste("_pl",1:5, sep=""), sep="")] <- dta[ paste("dec_man",paste("_pl",1:5, sep=""), sep="")] >0
dta[ paste("dec_both",paste("_pl",1:5, sep=""), sep="")] <- dta[ paste("dec_man",paste("_pl",1:5, sep=""), sep="")] >0

dta[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")] <- dta[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")] == "Yes"
dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42")] <- dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42")] == "Yes"

dta[c("grp1a55a" ,"grp2b55b", "grp3c55b","grp4d55b","grp5e55b")]  <- dta[c("grp1a55a" ,"grp2b55b", "grp3c55b","grp4d55b","grp5e55b")] == "Yes"
dta[ c("spouse2grp_sp1f55a","spouse2grp_sp2g55b","spouse2grp_sp3h55b","spouse2group_sp4j55b","spouse2grp5_sp5k55b")] <- dta[ c("spouse2grp_sp1f55a","spouse2grp_sp2g55b","spouse2grp_sp3h55b","spouse2group_sp4j55b","spouse2grp5_sp5k55b")]=="Yes"

dta[c("grp1a151","grp2b55b","grp3c55b","grp4d55b","grp5e55b")] <- dta[c("grp1a151","grp2b55b","grp3c55b","grp4d55b","grp5e55b")] =="Yes"
dta[c("spouse2grp_sp1f151","spouse2grp_sp2g151","spouse2grp_sp3h151","spouse2group_sp4j151","spouse2grp5_sp5k151")] <- dta[c("spouse2grp_sp1f151","spouse2grp_sp2g151","spouse2grp_sp3h151","spouse2group_sp4j151","spouse2grp5_sp5k151")] =="Yes"

dta[c("grp1seed_purchase1","grp2seed_purchase2","grp3seed_purchase3","grp4seed_purchase4","grp5seed_purchase5")] <- dta[c("grp1seed_purchase1","grp2seed_purchase2","grp3seed_purchase3","grp4seed_purchase4","grp5seed_purchase5")]=="Yes"
dta[c("spouse2grp_sp1seed_purchasesp1", "spouse2grp_sp2seed_purchase_sp2","spouse2grp_sp3seed_purchasesp3","spouse2group_sp4seed_purchasesp4","spouse2grp5_sp5seed_purchasesp5")] <- dta[c("spouse2grp_sp1seed_purchasesp1","spouse2grp_sp2seed_purchase_sp2","spouse2grp_sp3seed_purchasesp3","spouse2group_sp4seed_purchasesp4", "spouse2grp5_sp5seed_purchasesp5")]=="Yes"

c("spouse2grp_sp1f55a","spouse2grp_sp2g55b","spouse2grp_sp3h55b","spouse2group_sp4j55b","spouse2grp5_sp5k55b")

dta$combiner_sp1_pl1 <- NA
dta$combiner_sp1_pl2  <- NA
dta$combiner_sp1_pl3  <- NA
dta$combiner_sp1_pl4  <- NA
dta$combiner_sp1_pl5  <- NA
dta[c("combiner_sp1_pl1","combiner_sp1_pl2","combiner_sp1_pl3","combiner_sp1_pl4","combiner_sp1_pl5")]<- dta[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")] *  dta[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")]

dta$combiner_sp2_pl1 <- NA
dta$combiner_sp2_pl2  <- NA
dta$combiner_sp2_pl3  <- NA
dta$combiner_sp2_pl4  <- NA
dta$combiner_sp2_pl5  <- NA
dta[c("combiner_sp2_pl1","combiner_sp2_pl2","combiner_sp2_pl3","combiner_sp2_pl4","combiner_sp2_pl5")]<- dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42")] *  dta[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29")]

dta[c("grp1days1","grp2days2","grp3days3","grp4days4", "grp5days5")] <- dta[c("grp1days1","grp2days2","grp3days3","grp4days4", "grp5days5")] ==1
dta[c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5")] <- dta[c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5")] ==1


##############################################   here the analysis starts ###########################################################
### we thest 4 hypotheses - status quo (control) vs something
### h1: messenger is male vs messenger is female
### h2: messenger is male vs messenger is couple
### h3: recipient is male vs recipient is female
### h4: recipeint is male vs recipient is couple


for (h in 1:4) {
if (h==1) {
############################################ compare male messenger to female messenger #########################################################
dta_bal <- subset(dta, messenger == "male" | messenger == "female")
treatment <- "(messenger == 'female') +ivr+sms+as.factor(recipient)+ as.factor(messenger)" 
} else if (h==2) {
############################################# compare male messenger to couple messenger  ##############################################
dta_bal <- subset(dta, messenger == "male" | messenger == "couple")
treatment <- "(messenger == 'couple') +ivr+sms+as.factor(recipient)+ as.factor(messenger)" 
} else if (h==3) {
##################################################  compare male recipient to female recipient ###################################################
dta_bal <- subset(dta, recipient == "male" | recipient == "female")
treatment <- "(recipient == 'female') +ivr+sms+as.factor(recipient)+ as.factor(messenger)" 

} else if (h==4) {
##################################################  compare male recipient to couple recipient ###################################################
dta_bal <- subset(dta, recipient == "male" | recipient == "couple")
treatment <- "(recipient == 'couple') +ivr+sms+as.factor(recipient)+ as.factor(messenger)"
}

############################### knowledge  ############################
#res_know[1,1,h] <- ifelse(h <=2, mean(dta_bal$know_space[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$know_space[dta_bal$recipient == "male"], na.rm=T))
#res_know[2,1,h] <-  ifelse(h <=2, sd(dta_bal$know_space[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$know_space[dta_bal$recipient == "male"], na.rm=T))
#res_know[1,2,h] <- summary(lm(as.formula(paste("know_space",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
#res_know[2,2,h] <- summary(lm(as.formula(paste("know_space",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
#res_know[1,3,h] <- ifelse(totrep >0, RI("know_space",treatment , dta_bal, nr_repl = totrep, h),summary(lm(as.formula(paste("know_space",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

#res_know[3,1,h] <- ifelse(h <=2, mean(dta_bal$know_combine[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$know_combine[dta_bal$recipient == "male"], na.rm=T))
#res_know[4,1,h] <-  ifelse(h <=2, sd(dta_bal$know_combine[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$know_combine[dta_bal$recipient == "male"], na.rm=T))
#res_know[3,2,h] <- summary(lm(as.formula(paste("know_combine",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
#res_know[4,2,h] <- summary(lm(as.formula(paste("know_combine",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
#res_know[3,3,h] <-  ifelse(totrep >0, RI("know_combine",treatment , dta_bal, nr_repl = totrep, h),summary(lm(as.formula(paste("know_combine",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

#res_know[5,1,h] <- ifelse(h <=2, mean(dta_bal$know_weed[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$know_weed[dta_bal$recipient == "male"], na.rm=T))
#res_know[6,1,h] <-  ifelse(h <=2, sd(dta_bal$know_weed[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$know_weed[dta_bal$recipient == "male"], na.rm=T))
#res_know[5,2,h] <- summary(lm(as.formula(paste("know_weed",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
#res_know[6,2,h] <- summary(lm(as.formula(paste("know_weed",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
#res_know[5,3,h] <-  ifelse(totrep >0, RI("know_weed",treatment , dta_bal, nr_repl = totrep, h),summary(lm(as.formula(paste("know_weed",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

#res_know[7,1,h] <- ifelse(h <=2, mean(dta_bal$know_armyworm[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$know_armyworm[dta_bal$recipient == "male"], na.rm=T))
#res_know[8,1,h] <-  ifelse(h <=2, sd(dta_bal$know_armyworm[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$know_armyworm[dta_bal$recipient == "male"], na.rm=T))
#res_know[7,2,h] <- summary(lm(as.formula(paste("know_armyworm",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
#res_know[8,2,h] <- summary(lm(as.formula(paste("know_armyworm",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
#res_know[7,3,h] <-  ifelse(totrep >0, RI("know_weed",treatment , dta_bal, nr_repl = totrep, h),summary(lm(as.formula(paste("know_armyworm",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])


#indexer <- FW_index(treatment, c("know_space", "know_combine", "know_weed","know_armyworm"),dta_bal, nr_repl=totrep,h)
#res_know[9,1,h] <- ifelse(h <=2, mean(indexer[[3]]$index[indexer[[3]]$messenger == "male"], na.rm=T), mean(indexer[[3]]$index[indexer[[3]]$recipient == "male"], na.rm=T))
#res_know[10,1,h] <- ifelse(h <=2, sd(indexer[[3]]$index[indexer[[3]]$messenger == "male"], na.rm=T), sd(indexer[[3]]$index[indexer[[3]]$recipient == "male"], na.rm=T))
#res_know[9,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
#res_know[10,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
#res_know[9,3,h] <-  indexer[[2]]

#RI_FWER(c("know_space","know_combine","know_weed", "know_armyworm"),treatment,dta_bal, c(0.0061	,0.126,0.381,0.3876), h)

###individual
#res_know_m[1,1,h] <- ifelse(h <=2, mean(dta_bal$know_space_m[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$know_space_m[dta_bal$recipient == "male"], na.rm=T))
#res_know_m[2,1,h] <-  ifelse(h <=2, sd(dta_bal$know_space_m[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$know_space_m[dta_bal$recipient == "male"], na.rm=T))
#res_know_m[1,2,h] <- summary(lm(as.formula(paste("know_space_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
#res_know_m[2,2,h] <- summary(lm(as.formula(paste("know_space_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
#res_know_m[1,3,h] <- ifelse(totrep >0, RI("know_space_m",treatment , dta_bal, nr_repl = totrep, h),summary(lm(as.formula(paste("know_space_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

#res_know_m[3,1,h] <- ifelse(h <=2, mean(dta_bal$know_combine_m[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$know_combine_m[dta_bal$recipient == "male"], na.rm=T))
#res_know_m[4,1,h] <-  ifelse(h <=2, sd(dta_bal$know_combine_m[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$know_combine_m[dta_bal$recipient == "male"], na.rm=T))
#res_know_m[3,2,h] <- summary(lm(as.formula(paste("know_combine_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
#res_know_m[4,2,h] <- summary(lm(as.formula(paste("know_combine_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
#res_know_m[3,3,h] <-  ifelse(totrep >0, RI("know_combine_m",treatment , dta_bal, nr_repl = totrep, h),summary(lm(as.formula(paste("know_combine_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

#res_know_m[5,1,h] <- ifelse(h <=2, mean(dta_bal$know_weed_m[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$know_weed_m[dta_bal$recipient == "male"], na.rm=T))
#res_know_m[6,1,h] <-  ifelse(h <=2, sd(dta_bal$know_weed_m[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$know_weed_m[dta_bal$recipient == "male"], na.rm=T))
#res_know_m[5,2,h] <- summary(lm(as.formula(paste("know_weed_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
#res_know_m[6,2,h] <- summary(lm(as.formula(paste("know_weed_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
#res_know_m[5,3,h] <-  ifelse(totrep >0, RI("know_weed_m",treatment , dta_bal, nr_repl = totrep, h),summary(lm(as.formula(paste("know_weed_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

#res_know_m[7,1,h] <- ifelse(h <=2, mean(dta_bal$know_armyworm_m[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$know_armyworm_m[dta_bal$recipient == "male"], na.rm=T))
#res_know_m[8,1,h] <-  ifelse(h <=2, sd(dta_bal$know_armyworm_m[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$know_armyworm_m[dta_bal$recipient == "male"], na.rm=T))
#res_know_m[7,2,h] <- summary(lm(as.formula(paste("know_armyworm_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
#res_know_m[8,2,h] <- summary(lm(as.formula(paste("know_armyworm_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
#res_know_m[7,3,h] <-  ifelse(totrep >0, RI("know_weed_m",treatment , dta_bal, nr_repl = totrep, h),summary(lm(as.formula(paste("know_armyworm_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])


#indexer <- FW_index(treatment, c("know_space_m", "know_combine_m", "know_weed_m","know_armyworm_m"),dta_bal, nr_repl=totrep,h)
#res_know_m[9,1,h] <- ifelse(h <=2, mean(indexer[[3]]$index[indexer[[3]]$messenger == "male"], na.rm=T), mean(indexer[[3]]$index[indexer[[3]]$recipient == "male"], na.rm=T))
#res_know_m[10,1,h] <- ifelse(h <=2, sd(indexer[[3]]$index[indexer[[3]]$messenger == "male"], na.rm=T), sd(indexer[[3]]$index[indexer[[3]]$recipient == "male"], na.rm=T))
#res_know_m[9,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
#res_know_m[10,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
#res_know_m[9,3,h] <-  indexer[[2]]

# h=1: RI_FWER(c("know_space_m","know_combine_m","know_weed_m", "know_armyworm_m"),treatment,dta_bal, c(0.0228,0.0412,0.6773,0.6726), 10000, h)
# h=4: RI_FWER(c("know_space_m","know_combine_m","know_weed_m", "know_armyworm_m"),treatment,dta_bal, c(0.0361,0.6153,0.122,0.1206), 10000, h)

#res_know_w[1,1,h] <- ifelse(h <=2, mean(dta_bal$know_space_w[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$know_space_w[dta_bal$recipient == "male"], na.rm=T))
#res_know_w[2,1,h] <-  ifelse(h <=2, sd(dta_bal$know_space_w[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$know_space_w[dta_bal$recipient == "male"], na.rm=T))
#res_know_w[1,2,h] <- summary(lm(as.formula(paste("know_space_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
#res_know_w[2,2,h] <- summary(lm(as.formula(paste("know_space_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
#res_know_w[1,3,h] <- ifelse(totrep >0, RI("know_space_w",treatment , dta_bal, nr_repl = totrep, h),summary(lm(as.formula(paste("know_space_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

#res_know_w[3,1,h] <- ifelse(h <=2, mean(dta_bal$know_combine_w[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$know_combine_w[dta_bal$recipient == "male"], na.rm=T))
#res_know_w[4,1,h] <-  ifelse(h <=2, sd(dta_bal$know_combine_w[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$know_combine_w[dta_bal$recipient == "male"], na.rm=T))
#res_know_w[3,2,h] <- summary(lm(as.formula(paste("know_combine_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
#res_know_w[4,2,h] <- summary(lm(as.formula(paste("know_combine_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
#res_know_w[3,3,h] <-  ifelse(totrep >0, RI("know_combine_w",treatment , dta_bal, nr_repl = totrep, h),summary(lm(as.formula(paste("know_combine_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

#res_know_w[5,1,h] <- ifelse(h <=2, mean(dta_bal$know_weed_w[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$know_weed_w[dta_bal$recipient == "male"], na.rm=T))
#res_know_w[6,1,h] <-  ifelse(h <=2, sd(dta_bal$know_weed_w[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$know_weed_w[dta_bal$recipient == "male"], na.rm=T))
#res_know_w[5,2,h] <- summary(lm(as.formula(paste("know_weed_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
#res_know_w[6,2,h] <- summary(lm(as.formula(paste("know_weed_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
#res_know_w[5,3,h] <-  ifelse(totrep >0, RI("know_weed_w",treatment , dta_bal, nr_repl = totrep, h),summary(lm(as.formula(paste("know_weed_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

#res_know_w[7,1,h] <- ifelse(h <=2, mean(dta_bal$know_armyworm_w[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$know_armyworm_w[dta_bal$recipient == "male"], na.rm=T))
#res_know_w[8,1,h] <-  ifelse(h <=2, sd(dta_bal$know_armyworm_w[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$know_armyworm_w[dta_bal$recipient == "male"], na.rm=T))
#res_know_w[7,2,h] <- summary(lm(as.formula(paste("know_armyworm_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
#res_know_w[8,2,h] <- summary(lm(as.formula(paste("know_armyworm_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
#res_know_w[7,3,h] <-  ifelse(totrep >0, RI("know_weed_w",treatment , dta_bal, nr_repl = totrep, h),summary(lm(as.formula(paste("know_armyworm_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])


#indexer <- FW_index(treatment, c("know_space_w", "know_combine_w", "know_weed_w","know_armyworm_w"),dta_bal, nr_repl=totrep,h)
#res_know_w[9,1,h] <- ifelse(h <=2, mean(indexer[[3]]$index[indexer[[3]]$messenger == "male"], na.rm=T), mean(indexer[[3]]$index[indexer[[3]]$recipient == "male"], na.rm=T))
#res_know_w[10,1,h] <- ifelse(h <=2, sd(indexer[[3]]$index[indexer[[3]]$messenger == "male"], na.rm=T), sd(indexer[[3]]$index[indexer[[3]]$recipient == "male"], na.rm=T))
#res_know_w[9,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
#res_know_w[10,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
#res_know_w[9,3,h] <-  indexer[[2]]

#h3:  RI_FWER(c("know_space_w","know_combine_w","know_weed_w", "know_armyworm_w"),treatment,dta_bal, c(0,0.0029,0.1761,0.1644), 10000, h)
#h4:  RI_FWER(c("know_space_w","know_combine_w","know_weed_w", "know_armyworm_w"),treatment,dta_bal, c(0,0.0063,0.9036,0.8981), 10000, h)

#### adoption at household level (copy paste from delivery)
################################ practices #############################
###timely planting

#res_hh_pract[1,1,h]  <- ifelse(h <=2, mean(dta_bal$day_one[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$day_one[dta_bal$recipient == "male"], na.rm=T))
#res_hh_pract[2,1,h]  <- ifelse(h <=2, sd(dta_bal$day_one[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$day_one[dta_bal$recipient == "male"], na.rm=T))
#res_hh_pract[1,2,h]  <- summary(lm(as.formula(paste("day_one", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
#res_hh_pract[2,2,h]  <- summary(lm(as.formula(paste("day_one", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
#res_hh_pract[1,3,h]  <- ifelse(totrep >0, RI("day_one",treatment , dta_bal, nr_repl = totrep,h), summary(lm(as.formula(paste("day_one", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

##### used recommended spacing use on at lease one plot as reported by at least one spouse
#res_hh_pract[3,1,h]  <- ifelse(h <=2, mean(dta_bal$space[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$space[dta_bal$recipient == "male"], na.rm=T))
#res_hh_pract[4,1,h]  <- ifelse(h <=2, sd(dta_bal$space[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$space[dta_bal$recipient == "male"], na.rm=T))
#res_hh_pract[3,2,h]  <- summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
#res_hh_pract[4,2,h]  <- summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
#res_hh_pract[3,3,h]  <- ifelse(totrep >0, RI("space",treatment, dta_bal, nr_repl = totrep , h),  summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

### used recommended way to fight striga - this should be changed to include info of all plots 
#res_hh_pract[5,1,h]  <- ifelse(h <=2, mean(dta_bal$striga[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$striga[dta_bal$recipient == "male"], na.rm=T))
#res_hh_pract[6,1,h]  <- ifelse(h <=2, sd(dta_bal$striga[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$striga[dta_bal$recipient == "male"], na.rm=T))
#res_hh_pract[5,2,h]  <- summary(lm(as.formula(paste("striga", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
#res_hh_pract[6,2,h]  <- summary(lm(as.formula(paste("striga", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
#res_hh_pract[5,3,h]  <- ifelse(totrep >0, RI("striga",treatment , dta_bal, nr_repl = totrep , h), summary(lm(as.formula(paste("striga", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

### weeded on recommended timing? - this should be changed to include info of all plots 
#res_hh_pract[7,1,h]  <- ifelse(h <=2, mean(dta_bal$weed[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$weed[dta_bal$recipient == "male"], na.rm=T))
#res_hh_pract[8,1,h]  <- ifelse(h <=2, sd(dta_bal$weed[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$weed[dta_bal$recipient == "male"], na.rm=T))
#res_hh_pract[7,2,h]  <- summary(lm(as.formula(paste("weed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
#res_hh_pract[8,2,h]  <- summary(lm(as.formula(paste("weed", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
#res_hh_pract[7,3,h]  <- ifelse(totrep >0, RI("weed",treatment, dta_bal, nr_repl = totrep , h),summary(lm(as.formula(paste("weed", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

### fertilizer use
#res_hh_pract[9,1,h]  <- ifelse(h <=2, mean(dta_bal$fert[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$fert[dta_bal$recipient == "male"], na.rm=T))
#res_hh_pract[10,1,h]  <- ifelse(h <=2, sd(dta_bal$fert[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$fert[dta_bal$recipient == "male"], na.rm=T))
#res_hh_pract[9,2,h]  <- summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
#res_hh_pract[10,2,h]  <- summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
#res_hh_pract[9,3,h]  <- ifelse(totrep >0, RI("fert",treatment , dta_bal, nr_repl = totrep , h), summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

##### fert = DAP/NPK
#res_hh_fert[1,1,h]  <- ifelse(h <=2, mean(dta_bal$fert_dap[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$fert_dap[dta_bal$recipient == "male"], na.rm=T))
#res_hh_fert[2,1,h]  <- ifelse(h <=2, sd(dta_bal$fert_dap[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$fert_dap[dta_bal$recipient == "male"], na.rm=T))
#res_hh_fert[1,2,h]  <- summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
#res_hh_fert[2,2,h]  <- summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
#res_hh_fert[1,3,h]  <- ifelse(totrep >0, RI("fert_dap",treatment , dta_bal, nr_repl = totrep , h) , summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

##### fert = urea
#res_hh_fert[3,1,h]  <- ifelse(h <=2, mean(dta_bal$fert_urea[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$fert_urea[dta_bal$recipient == "male"], na.rm=T))
#res_hh_fert[4,1,h]  <- ifelse(h <=2, sd(dta_bal$fert_urea[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$fert_urea[dta_bal$recipient == "male"], na.rm=T))
#res_hh_fert[3,2,h]  <- summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
#res_hh_fert[4,2,h]  <- summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
#res_hh_fert[3,3,h]  <- ifelse(totrep >0, RI("fert_urea",treatment , dta_bal, nr_repl = totrep , h) , summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

##### fert = organic
#res_hh_fert[5,1,h]  <- ifelse(h <=2, mean(dta_bal$fert_org[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$fert_org[dta_bal$recipient == "male"], na.rm=T))
#res_hh_fert[6,1,h]  <- ifelse(h <=2, sd(dta_bal$fert_org[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$fert_org[dta_bal$recipient == "male"], na.rm=T))
#res_hh_fert[5,2,h]  <-  summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
#res_hh_fert[6,2,h]  <- summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
#res_hh_fert[5,3,h]  <- ifelse(totrep >0, RI("fert_org",treatment , dta_bal, nr_repl = totrep , h), summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

###improved seed  
#res_hh_pract[11,1,h]  <- ifelse(h <=2, mean(dta_bal$impseed[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$impseed[dta_bal$recipient == "male"], na.rm=T))
#res_hh_pract[12,1,h]  <- ifelse(h <=2, sd(dta_bal$impseed[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$impseed[dta_bal$recipient == "male"], na.rm=T))
#res_hh_pract[11,2,h]  <- summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
#res_hh_pract[12,2,h]  <- summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
#res_hh_pract[11,3,h]  <- ifelse(totrep >0, RI("impseed",treatment , dta_bal, nr_repl = totrep , h),  summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

### hybrid
#res_hh_seed[1,1,h]  <- ifelse(h <=2, mean(dta_bal$hybrid[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$hybrid[dta_bal$recipient == "male"], na.rm=T))
#res_hh_seed[2,1,h]  <- ifelse(h <=2, sd(dta_bal$hybrid[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$hybrid[dta_bal$recipient == "male"], na.rm=T))
#res_hh_seed[1,2,h] <- summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
#res_hh_seed[2,2,h] <- summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
#res_hh_seed[1,3,h] <- ifelse(totrep >0, RI("hybrid",treatment , dta_bal, nr_repl = totrep , h), summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

### opv
#res_hh_seed[3,1,h]  <- ifelse(h <=2, mean(dta_bal$opv[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$opv[dta_bal$recipient == "male"], na.rm=T))
#res_hh_seed[4,1,h]  <- ifelse(h <=2, sd(dta_bal$opv[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$opv[dta_bal$recipient == "male"], na.rm=T))
#res_hh_seed[3,2,h] <- summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
#res_hh_seed[4,2,h] <- summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
#res_hh_seed[3,3,h] <- ifelse(totrep >0, RI("opv",treatment , dta_bal, nr_repl = totrep , h), summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

###combiner
#res_hh_pract[13,1,h]  <- ifelse(h <=2, mean(dta_bal$combiner[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$combiner[dta_bal$recipient == "male"], na.rm=T))
#res_hh_pract[14,1,h]  <- ifelse(h <=2, sd(dta_bal$combiner[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$combiner[dta_bal$recipient == "male"], na.rm=T))
#res_hh_pract[13,2,h]  <- summary(lm(as.formula(paste("combiner", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
#res_hh_pract[14,2,h]  <- summary(lm(as.formula(paste("combiner", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
#res_hh_pract[13,3,h]  <- ifelse(totrep >0, RI("combiner",treatment , dta_bal, nr_repl = totrep , h), summary(lm(as.formula(paste("combiner", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

#### bought seed
#res_hh_pract[15,1,h]  <- ifelse(h <=2, mean(dta_bal$bought_seed[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$bought_seed[dta_bal$recipient == "male"], na.rm=T))
#res_hh_pract[16,1,h]  <- ifelse(h <=2, sd(dta_bal$bought_seed[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$bought_seed[dta_bal$recipient == "male"], na.rm=T))
#res_hh_pract[15,2,h]  <- summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
#res_hh_pract[16,2,h]  <- summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
#res_hh_pract[15,3,h]  <- ifelse(totrep >0, RI("bought_seed",treatment , dta_bal, nr_repl = totrep , h), summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

##### used chemicals
#res_hh_pract[17,1,h]  <- ifelse(h <=2, mean(dta_bal$chem[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$chem[dta_bal$recipient == "male"], na.rm=T))
#res_hh_pract[18,1,h]  <- ifelse(h <=2, sd(dta_bal$chem[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$chem[dta_bal$recipient == "male"], na.rm=T))
#res_hh_pract[17,2,h]  <- summary(lm(as.formula(paste("chem", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
#res_hh_pract[18,2,h]  <- summary(lm(as.formula(paste("chem", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
#res_hh_pract[17,3,h]  <- ifelse(totrep >0, RI("chem",treatment , dta_bal, nr_repl = totrep , h), summary(lm(as.formula(paste("chem", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

####hired labour
#res_hh_pract[19,1,h]  <- ifelse(h <=2, mean(dta_bal$labour[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$labour[dta_bal$recipient == "male"], na.rm=T))
#res_hh_pract[20,1,h]  <- ifelse(h <=2, sd(dta_bal$labour[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$labour[dta_bal$recipient == "male"], na.rm=T))
#res_hh_pract[19,2,h]  <- summary(lm(as.formula(paste("labour", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
#res_hh_pract[20,2,h]  <- summary(lm(as.formula(paste("labour", treatment, sep ="~")), data=dta_bal))$coefficients[2,2]
#res_hh_pract[19,3,h]  <- ifelse(totrep >0, RI("labour",treatment , dta_bal, nr_repl = totrep , h), summary(lm(as.formula(paste("labour", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

##if (totrep >0) {
##res_h0_pract[1:7,4,h] <- FSR_RI( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment ,dta_bal, pvals =  res_h0_pract[,3,h] , nr_repl_pi = 100)

##res_h0_pract[1:7,4,h] <- FSR_OLS( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment,dta_bal, nr_repl = totrep)[[4]]

#indexer <-  FW_index(treatment,c("day_one","space","striga","weed", "fert","impseed", "combiner","bought_seed","chem","labour"),dta_bal, nr_repl=totrep, h)

#res_hh_pract[21,1,h] <- ifelse(h <=2, mean(indexer[[3]]$index[indexer[[3]]$messenger == "male"], na.rm=T), mean(indexer[[3]]$index[indexer[[3]]$recipient == "male"], na.rm=T))
#res_hh_pract[22,1,h] <- ifelse(h <=2, sd(indexer[[3]]$index[indexer[[3]]$messenger == "male"], na.rm=T), sd(indexer[[3]]$index[indexer[[3]]$recipient == "male"], na.rm=T))
#res_hh_pract[21,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
#res_hh_pract[22,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
#res_hh_pract[21,3,h] <-  indexer[[2]]

#### agreement
#res_decision_b[1,1,h]  <- ifelse(h <=2, mean(dta_bal$both_tell[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$both_tell[dta_bal$recipient == "male"], na.rm=T))
#res_decision_b[2,1,h]  <- ifelse(h <=2, sd(dta_bal$both_tell[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$both_tell[dta_bal$recipient == "male"], na.rm=T))
#res_decision_b[1,2,h]  <- summary(lm(as.formula(paste("both_tell",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
#res_decision_b[2,2,h]  <- summary(lm(as.formula(paste("both_tell",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
#res_decision_b[1,3,h]  <- ifelse(totrep >0, RI("both_tell",treatment , dta_bal, nr_repl = totrep, h), summary(lm(as.formula(paste("both_tell",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])


#res_decision_b[3,1,h]  <- ifelse(h <=2, mean(dta_bal$spouses_listen[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$spouses_listen[dta_bal$recipient == "male"], na.rm=T))
#res_decision_b[4,1,h]  <- ifelse(h <=2, sd(dta_bal$spouses_listen[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$spouses_listen[dta_bal$recipient == "male"], na.rm=T))
#res_decision_b[3,2,h]  <- summary(lm(as.formula(paste("spouses_listen",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
#res_decision_b[4,2,h]  <- summary(lm(as.formula(paste("spouses_listen",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
#res_decision_b[3,3,h]  <- ifelse(totrep >0, RI("spouses_listen",treatment , dta_bal, nr_repl = totrep, h), summary(lm(as.formula(paste("spouses_listen",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])


#res_decision_m[1,1,h]  <- ifelse(h <=2, mean(dta_bal$man_tells_wife[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$man_tells_wife[dta_bal$recipient == "male"], na.rm=T))
#res_decision_m[2,1,h]  <- ifelse(h <=2, sd(dta_bal$man_tells_wife[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$man_tells_wife[dta_bal$recipient == "male"], na.rm=T))
#res_decision_m[1,2,h]  <- summary(lm(as.formula(paste("man_tells_wife",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
#res_decision_m[2,2,h]  <- summary(lm(as.formula(paste("man_tells_wife",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
#res_decision_m[1,3,h]  <- ifelse(totrep >0, RI("man_tells_wife",treatment , dta_bal, nr_repl = totrep, h), summary(lm(as.formula(paste("man_tells_wife",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])


#res_decision_m[3,1,h]  <- ifelse(h <=2, mean(dta_bal$wife_listens[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$wife_listens[dta_bal$recipient == "male"], na.rm=T))
#res_decision_m[4,1,h]  <- ifelse(h <=2, sd(dta_bal$wife_listens[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$wife_listens[dta_bal$recipient == "male"], na.rm=T))
#res_decision_m[3,2,h]  <- summary(lm(as.formula(paste("wife_listens",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
#res_decision_m[4,2,h]  <- summary(lm(as.formula(paste("wife_listens",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
#res_decision_m[3,3,h]  <- ifelse(totrep >0, RI("wife_listens",treatment , dta_bal, nr_repl = totrep, h), summary(lm(as.formula(paste("wife_listens",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

#res_decision_w[1,1,h]  <- ifelse(h <=2, mean(dta_bal$wife_tells_man[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$wife_tells_man[dta_bal$recipient == "male"], na.rm=T))
#res_decision_w[2,1,h]  <- ifelse(h <=2, sd(dta_bal$wife_tells_man[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$wife_tells_man[dta_bal$recipient == "male"], na.rm=T))
#res_decision_w[1,2,h]  <- summary(lm(as.formula(paste("wife_tells_man",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
#res_decision_w[2,2,h]  <- summary(lm(as.formula(paste("wife_tells_man",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
#res_decision_w[1,3,h]  <- ifelse(totrep >0, RI("wife_tells_man",treatment , dta_bal, nr_repl = totrep, h), summary(lm(as.formula(paste("wife_tells_man",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

#res_decision_w[3,1,h]  <- ifelse(h <=2, mean(dta_bal$man_listens[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$man_listens[dta_bal$recipient == "male"], na.rm=T))
#res_decision_w[4,1,h]  <- ifelse(h <=2, sd(dta_bal$man_listens[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$man_listens[dta_bal$recipient == "male"], na.rm=T))
#res_decision_w[3,2,h]  <- summary(lm(as.formula(paste("man_listens",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
#res_decision_w[4,2,h]  <- summary(lm(as.formula(paste("man_listens",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
#res_decision_w[3,3,h]  <- ifelse(totrep >0, RI("man_listens",treatment , dta_bal, nr_repl = totrep, h), summary(lm(as.formula(paste("man_listens",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

######## decisions
#results <- plot_RI_dec(dta_bal,"dectime_man",treatment, nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_m[1,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_m[1,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_m[2,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"decspace_man",treatment, nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_m[3,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_m[3,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_m[4,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"decstriga_man",treatment, nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_m[5,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_m[5,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_m[6,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"decweed_man",treatment, nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_m[7,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_m[7,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_m[8,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"decfert_man",treatment,  nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_m[9,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_m[9,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_m[10,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"decseed_man",treatment, nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_m[11,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_m[11,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_m[12,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"deccombiner_man",treatment, nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_m[13,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_m[13,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_m[14,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"decbuyseed_man",treatment, nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_m[15,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_m[15,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_m[16,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"decchem_man",treatment,  nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_m[17,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_m[17,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_m[18,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"dec_man_d",treatment, nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_m[19,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_m[19,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_m[20,1:2,h] <- unlist(results[c(6,2)])


h <- 3
dta_bal <- subset(dta, recipient == "male" | recipient == "female")
treatment <- "(recipient == 'female') +ivr+sms+as.factor(recipient)+ as.factor(messenger)" 

plot_RI_dec_FWER(man= c("dectime_man","decspace_man","decstriga_man", "decweed_man", "decfert_man", "decseed_man","deccombiner_man","decbuyseed_man","decchem_man"), treatment, data=dta_bal,p_vals=c(0.00000,0.00000,0.00000,0.00000,0.00000,0.00400,0.00000,0.04990,	0.00260), nr_repl = 10000, h=3)
plot_RI_dec_FWER(man= c("dectime_man","decspace_woman","decstriga_woman", "decweed_woman", "decfert_woman", "decseed_woman","deccombiner_woman","decbuyseed_woman","decchem_woman"), treatment, data=dta_bal,p_vals=c(0,0,0,0,0,0,0,0,0.0104), nr_repl = 10000, h=3)

h <- 4
dta_bal <- subset(dta, recipient == "male" | recipient == "couple")
treatment <- "(recipient == 'couple') +ivr+sms+as.factor(recipient)+ as.factor(messenger)"
plot_RI_dec_FWER(man= c("dectime_man","decspace_man","decstriga_man", "decweed_man", "decfert_man", "decseed_man","deccombiner_man","decbuyseed_man","decchem_man"), treatment, data=dta_bal,p_vals=c(0,0,0.0001,0,0,0.0095,0.0012,0.0002,0.0147,0), nr_repl = 10000, h=4)
plot_RI_dec_FWER(man= c("dectime_man","decspace_woman","decstriga_woman", "decweed_woman", "decfert_woman", "decseed_woman","deccombiner_woman","decbuyseed_woman","decchem_woman"), treatment, data=dta_bal,p_vals=c(0.0648,0.223,0.0127,0.1041,0.0093,0.6691,0.273,0.4974,	0.6122,	0.0496), nr_repl = 10000, h=4)
plot_RI_dec_FWER(man= c("dectime_both","decspace_both","decstriga_both", "decweed_both", "decfert_both", "decseed_both","deccombiner_both","decbuyseed_both","decchem_both"), treatment, data=dta_bal,p_vals=c(0.612,0.1766,0.0054,0.0592,0.0846,0.3051,0.1783,0.0682,0.2052,0.5847), nr_repl = 10000, h=4)

##space_ind <- reshape(dta[c("messenger","recipient","gender1","ivr","sms","called","totsms","hhid","distID", "subID","vilID", dec_vars)], varying = dec_vars,v.names="dectime_man", idvar="hhid", direction="long")

##dec_vars <- paste("decspace_man",paste("_pl",1:5, sep=""), sep="")

##space_ind2 <- reshape(dta[c("messenger","recipient","gender1","ivr","sms","called","totsms","hhid","distID", "subID","vilID", dec_vars)], varying = dec_vars,v.names="decspace_man", idvar="hhid", direction="long")[c("hhid","time","decspace_man")]

##dec_vars <- paste("decstriga_man",paste("_pl",1:5, sep=""), sep="")

##space_ind3 <- reshape(dta[c("messenger","recipient","gender1","ivr","sms","called","totsms","hhid","distID", "subID","vilID", dec_vars)], varying = dec_vars,v.names="decstriga_man", idvar="hhid", direction="long")[c("hhid","time","decstriga_man")]

##dec_vars <- paste("decweed_man",paste("_pl",1:5, sep=""), sep="")

##space_ind4 <- reshape(dta[c("messenger","recipient","gender1","ivr","sms","called","totsms","hhid","distID", "subID","vilID", dec_vars)], varying = dec_vars,v.names="decweed_man", idvar="hhid", direction="long")[c("hhid","time","decweed_man")]


##dta_ind <-  merge(merge(merge(space_ind, space_ind2, by=c("hhid","time")),space_ind3, by=c("hhid","time")),space_ind4, by=c("hhid","time"))
#### this works but it is very slow....
##system.time(reser <- FSR_RI_plot2(c("dectime_man","decspace_man", "decstriga_man", "decweed_man"), treatment, dta_ind, pvals = c(0.0215,0.0492,0.2630,0.0175),100))

######## decisions woman
#results <- plot_RI_dec(dta_bal,"dectime_woman",treatment, nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_w[1,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_w[1,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_w[2,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"decspace_woman",treatment,  nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_w[3,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_w[3,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_w[4,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"decstriga_woman",treatment,  nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_w[5,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_w[5,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_w[6,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"decweed_woman",treatment,  nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_w[7,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_w[7,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_w[8,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"decfert_woman",treatment,  nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_w[9,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_w[9,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_w[10,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"decseed_woman",treatment,  nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_w[11,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_w[11,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_w[12,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"deccombiner_woman",treatment, nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_w[13,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_w[13,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_w[14,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"decbuyseed_woman",treatment,  nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_w[15,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_w[15,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_w[16,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"decchem_woman",treatment,  nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_w[17,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_w[17,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_w[18,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"dec_woman_d",treatment,  nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_w[19,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_w[19,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_w[20,1:2,h] <- unlist(results[c(6,2)])


######## decisions both
#results <- plot_RI_dec(dta_bal,"dectime_both",treatment, nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_b[1,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_b[1,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_b[2,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"decspace_both",treatment,  nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_b[3,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_b[3,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_b[4,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"decstriga_both",treatment,  nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_b[5,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_b[5,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_b[6,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"decweed_both",treatment,  nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_b[7,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_b[7,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_b[8,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"decfert_both",treatment,  nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_b[9,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_b[9,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_b[10,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"decseed_both",treatment,  nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_b[11,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_b[11,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_b[12,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"deccombiner_both",treatment,  nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_b[13,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_b[13,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_b[14,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"decbuyseed_both",treatment,  nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_b[15,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_b[15,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_b[16,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"decchem_both",treatment,  nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_b[17,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_b[17,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_b[18,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI_dec(dta_bal,"dec_both_d",treatment, nr_repl = totrep, h=h)
#if (totrep>0) {
#res_dec_b[19,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_dec_b[19,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_dec_b[20,1:2,h] <- unlist(results[c(6,2)])

############################### practices #############################
#####plant immediately after rain on plot managed by - man - woman - both

#results <- plot_RI(dta, man = "dectime_man", out_sp1 =c("grp1days1","grp2days2","grp3days3","grp4days4", "grp5days5"),out_sp2 =c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5"),treatment, totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_m[1,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_m[1,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_m[2,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI(dta, man = "dectime_woman", out_sp1 =c("grp1days1","grp2days2","grp3days3","grp4days4", "grp5days5"),out_sp2 =c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5"),treatment , totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_w[1,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_w[1,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_w[2,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI(dta, man = "dectime_both", out_sp1 =c("grp1days1","grp2days2","grp3days3","grp4days4", "grp5days5"),out_sp2 =c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5"),treatment , totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_b[1,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_b[1,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_b[2,1:2,h] <- unlist(results[c(6,2)])

#### used recommended spacing on plot managed by - man - woman - both

#results <-  plot_RI(dta, man = "decspace_man", out_sp1 = c("grp1a201","grp2b201","grp3c201","grp4d201","grp5e201"),out_sp2 = c("spouse2grp_sp1f201","spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201","spouse2grp5_sp5k201"),treatment , totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_m[3,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_m[3,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_m[4,1:2,h] <- unlist(results[c(6,2)])


#results <- plot_RI(dta, man = "decspace_woman", out_sp1 = c("grp1a201","grp2b201","grp3c201","grp4d201","grp5e201"),out_sp2 = c("spouse2grp_sp1f201","spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201","spouse2grp5_sp5k201"),treatment, totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_w[3,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_w[3,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_w[4,1:2,h] <- unlist(results[c(6,2)])


#results <-  plot_RI(dta, man = "decspace_both", out_sp1 = c("grp1a201","grp2b201","grp3c201","grp4d201","grp5e201"),out_sp2 = c("spouse2grp_sp1f201","spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201","spouse2grp5_sp5k201"),treatment , totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_b[3,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_b[3,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_b[4,1:2,h] <- unlist(results[c(6,2)])

#### used recommended way to fight striga - this should be changed to include info of all plots 

#results <- plot_RI(dta, man = "decstriga_man", out_sp1 = c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241"),out_sp2 = c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241"),treatment , totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_m[5,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_m[5,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_m[6,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI(dta, man = "decstriga_woman", out_sp1 = c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241"),out_sp2 = c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241"),treatment , totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_w[5,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_w[5,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_w[6,1:2,h] <- unlist(results[c(6,2)])


#results <- plot_RI(dta, man = "decstriga_both", out_sp1 = c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241"),out_sp2 = c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241"),treatment , totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_b[5,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_b[5,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_b[6,1:2,h] <- unlist(results[c(6,2)])

#### weeded on recommended timing? - this should be changed to include info of all plots 

#results <- plot_RI(dta, man = "decweed_man", out_sp1 = c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26"),out_sp2 = c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26"),treatment  , totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_m[7,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_m[7,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_m[8,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI(dta, man = "decweed_woman", out_sp1 = c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26"),out_sp2 = c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26"),treatment  , totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_w[7,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_w[7,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_w[8,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI(dta, man = "decweed_both", out_sp1 = c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26"),out_sp2 = c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26"),treatment  , totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_b[7,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_b[7,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_b[8,1:2,h] <- unlist(results[c(6,2)])


##### fertilizer use
##### any fertlizer used on a plot?
##### but how to define who decided/managed?
#results <- plot_RI(dta, man = "dec_man_d", out_sp1 = c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29") ,out_sp2 = c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29"),treatment   , totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_m[9,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_m[9,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_m[10,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI(dta, man = "dec_woman_d", out_sp1 = c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29"),out_sp2 = c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29"),treatment   , totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_w[9,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_w[9,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_w[10,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI(dta, man = "dec_both_d", out_sp1 = c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29"),out_sp2 = c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29"),treatment   , totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_b[9,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_b[9,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_b[10,1:2,h] <- unlist(results[c(6,2)])


#####improved seed  

#results <- plot_RI(dta, man = "dec_man_d", out_sp1 = c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42") ,out_sp2 =c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42"),treatment , totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_m[11,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_m[11,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_m[12,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI(dta, man = "dec_woman_d", out_sp1 = c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42"),out_sp2 = c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42"),treatment  , totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_w[11,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_w[11,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_w[12,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI(dta, man = "dec_both_d", out_sp1 = c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42"),out_sp2 = c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42"),treatment , totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_b[11,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_b[11,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_b[12,1:2,h] <- unlist(results[c(6,2)])


#####combiner
#results <- plot_RI(dta, man = "dec_man_d", out_sp1 = c("combiner_sp1_pl1","combiner_sp1_pl2","combiner_sp1_pl3","combiner_sp1_pl4","combiner_sp1_pl5") ,out_sp2 =c("combiner_sp2_pl1","combiner_sp2_pl2","combiner_sp2_pl3","combiner_sp2_pl4","combiner_sp2_pl5"),treatment, totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_m[13,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_m[13,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_m[14,1:2,h] <- unlist(results[c(6,2)])


#results <- plot_RI(dta, man = "dec_woman_d", out_sp1 =  c("combiner_sp1_pl1","combiner_sp1_pl2","combiner_sp1_pl3","combiner_sp1_pl4","combiner_sp1_pl5"),out_sp2 = c("combiner_sp2_pl1","combiner_sp2_pl2","combiner_sp2_pl3","combiner_sp2_pl4","combiner_sp2_pl5"),treatment, totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_w[13,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_w[13,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_w[14,1:2,h] <- unlist(results[c(6,2)])



#results <- plot_RI(dta, man = "dec_both_d", out_sp1 = c("combiner_sp1_pl1","combiner_sp1_pl2","combiner_sp1_pl3","combiner_sp1_pl4","combiner_sp1_pl5"),out_sp2 = c("combiner_sp2_pl1","combiner_sp2_pl2","combiner_sp2_pl3","combiner_sp2_pl4","combiner_sp2_pl5"),treatment, totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_b[13,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_b[13,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_b[14,1:2,h] <- unlist(results[c(6,2)])

###### bought seed

#results <- plot_RI(dta, man = "dec_man_d", out_sp1 =c("grp1seed_purchase1","grp2seed_purchase2","grp3seed_purchase3","grp4seed_purchase4","grp5seed_purchase5"),out_sp2 =c("spouse2grp_sp1seed_purchasesp1", "spouse2grp_sp2seed_purchase_sp2","spouse2grp_sp3seed_purchasesp3","spouse2group_sp4seed_purchasesp4","spouse2grp5_sp5seed_purchasesp5"),treatment, totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_m[15,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_m[15,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_m[16,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI(dta, man = "dec_woman_d", out_sp1 =c("grp1seed_purchase1","grp2seed_purchase2","grp3seed_purchase3","grp4seed_purchase4","grp5seed_purchase5"),out_sp2 =c("spouse2grp_sp1seed_purchasesp1", "spouse2grp_sp2seed_purchase_sp2","spouse2grp_sp3seed_purchasesp3","spouse2group_sp4seed_purchasesp4","spouse2grp5_sp5seed_purchasesp5"),treatment , totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_w[15,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_w[15,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_w[16,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI(dta, man = "dec_both_d", out_sp1 =c("grp1seed_purchase1","grp2seed_purchase2","grp3seed_purchase3","grp4seed_purchase4","grp5seed_purchase5"),out_sp2 =c("spouse2grp_sp1seed_purchasesp1", "spouse2grp_sp2seed_purchase_sp2","spouse2grp_sp3seed_purchasesp3","spouse2group_sp4seed_purchasesp4","spouse2grp5_sp5seed_purchasesp5"),treatment ,totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_b[15,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_b[15,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_b[16,1:2,h] <- unlist(results[c(6,2)])

####### used chemicals

#results <- plot_RI(dta, man = "dec_man_d", out_sp1 =c("grp1a55a" ,"grp2b55b", "grp3c55b","grp4d55b","grp5e55b"),out_sp2 =c("spouse2grp_sp1f55a","spouse2grp_sp2g55b","spouse2grp_sp3h55b","spouse2group_sp4j55b","spouse2grp5_sp5k55b"),treatment ,totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_m[17,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_m[17,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_m[18,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI(dta, man = "dec_woman_d", out_sp1 =c("grp1a55a" ,"grp2b55b", "grp3c55b","grp4d55b","grp5e55b"),out_sp2 =c("spouse2grp_sp1f55a","spouse2grp_sp2g55b","spouse2grp_sp3h55b","spouse2group_sp4j55b","spouse2grp5_sp5k55b"),treatment ,totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_w[17,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_w[17,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_w[18,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI(dta, man = "dec_both_d", out_sp1 =c("grp1a55a" ,"grp2b55b", "grp3c55b","grp4d55b","grp5e55b"),out_sp2 =c("spouse2grp_sp1f55a","spouse2grp_sp2g55b","spouse2grp_sp3h55b","spouse2group_sp4j55b","spouse2grp5_sp5k55b"),treatment ,totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_b[17,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_b[17,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_b[18,1:2,h] <- unlist(results[c(6,2)])


######hired labour

#results <- plot_RI(dta, man = "dec_man_d", out_sp1 =c("grp1a151","grp2b55b","grp3c55b","grp4d55b","grp5e55b"),out_sp2 =c("spouse2grp_sp1f151","spouse2grp_sp2g151","spouse2grp_sp3h151","spouse2group_sp4j151","spouse2grp5_sp5k151"),treatment ,totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_m[19,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_m[19,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_m[20,1:2,h] <- unlist(results[c(6,2)])


#results <- plot_RI(dta, man = "dec_woman_d", out_sp1 =c("grp1a151","grp2b55b","grp3c55b","grp4d55b","grp5e55b"),out_sp2 =c("spouse2grp_sp1f151","spouse2grp_sp2g151","spouse2grp_sp3h151","spouse2group_sp4j151","spouse2grp5_sp5k151"),treatment ,totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_w[19,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_w[19,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_w[20,1:2,h] <- unlist(results[c(6,2)])

#results <- plot_RI(dta, man = "dec_both_d", out_sp1 =c("grp1a151","grp2b55b","grp3c55b","grp4d55b","grp5e55b"),out_sp2 =c("spouse2grp_sp1f151","spouse2grp_sp2g151","spouse2grp_sp3h151","spouse2group_sp4j151","spouse2grp5_sp5k151"),treatment ,totrep,trimlog=F,h)
#if (totrep>0) {
#res_pract_b[19,1:3,h] <- unlist(results[c(5,1,4)])
#} else {
#res_pract_b[19,1:3,h] <- unlist(results[c(5,1,3)])
#}
#res_pract_b[20,1:2,h] <- unlist(results[c(6,2)])





################################## production ###########################
#### does the video increases production related outcomes?

##trimming is done on end result
#dta_bal2 <- subset(dta_bal, prod_tot_fm>0)
#dta_bal2$log_prod_tot_fm <- log(dta_bal2$prod_tot_fm)
#dta_trim <- trim("log_prod_tot_fm", dta_bal2, .05)

#### production
#res_prod_fm[1,1,h] <- summary(lm(as.formula(paste("log_prod_tot_fm",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
#res_prod_fm[1,2,h] <- summary(lm(as.formula(paste("log_prod_tot_fm",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
#res_prod_fm[1,3,h] <- ifelse(totrep >0, RI("log_prod_tot_fm",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_prod_tot_fm",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

#### area

#dta_bal2 <- subset(dta_bal, area_tot_fm>0)
#dta_bal2$log_area_tot_fm <- log(dta_bal2$area_tot_fm)

#dta_trim <- trim("log_area_tot_fm", dta_bal2, .05)

#res_prod_fm[2,1,h] <- summary(lm(as.formula(paste("log_area_tot_fm",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
#res_prod_fm[2,2,h] <- summary(lm(as.formula(paste("log_area_tot_fm",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
#res_prod_fm[2,3,h] <- ifelse(totrep >0, RI("log_area_tot_fm",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_area_tot_fm",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

#alternative - this is at plot level
#results <- plot_RI(dta_bal, man = "mgt_man", out_sp1 =c("prod_pl1_sp1","prod_pl2_sp1","prod_pl3_sp1","prod_pl4_sp1", "prod_pl5_sp1"),out_sp2 =c("prod_pl1_sp2","prod_pl2_sp2","prod_pl3_sp2","prod_pl4_sp2", "prod_pl5_sp2"),treatment , repl, trimlog = TRUE)
#res_prod_mm[1,1:3,h] <- unlist(results[c(1,2,4)])

#results <- plot_RI(dta_bal, man = "mgt_man", out_sp1 =c("area_pl1_sp1","area_pl2_sp1","area_pl3_sp1","area_pl4_sp1", "area_pl5_sp1"),out_sp2 =c("area_pl1_sp2","area_pl2_sp2","area_pl3_sp2","area_pl4_sp2", "area_pl5_sp2"),treatment , repl, trimlog = TRUE)
#res_prod_mm[2,1:3,h] <- unlist(results[c(1,2,4)])

#results <- plot_RI(dta_bal, man = "mgt_man", out_sp1 =paste("yield_sp1",paste("_pl",1:5, sep=""), sep=""),out_sp2 =paste("yield_sp2",paste("_pl",1:5, sep=""), sep=""),treatment , repl, trimlog = TRUE)
#res_prod_mm[3,1:3,h] <- unlist(results[c(1,2,4)])

#results <- plot_RI(dta_bal, man = "mgt_man", out_sp1 =paste("yield_better_sp1",paste("_pl",1:5, sep=""), sep=""),out_sp2 =paste("yield_better_sp2",paste("_pl",1:5, sep=""), sep=""),treatment , repl, trimlog = FALSE)
#res_prod_mm[4,1:3,h] <- unlist(results[c(1,2,4)])

#results <- plot_RI(dta_bal, man = "mgt_woman", out_sp1 =c("prod_pl1_sp1","prod_pl2_sp1","prod_pl3_sp1","prod_pl4_sp1", "prod_pl5_sp1"),out_sp2 =c("prod_pl1_sp2","prod_pl2_sp2","prod_pl3_sp2","prod_pl4_sp2", "prod_pl5_sp2"),treatment , repl, trimlog = TRUE)
#res_prod_fm[1,1:3,h] <- unlist(results[c(1,2,4)])

#results <- plot_RI(dta_bal, man = "mgt_woman", out_sp1 =c("area_pl1_sp1","area_pl2_sp1","area_pl3_sp1","area_pl4_sp1", "area_pl5_sp1"),out_sp2 =c("area_pl1_sp2","area_pl2_sp2","area_pl3_sp2","area_pl4_sp2", "area_pl5_sp2"),treatment , repl, trimlog = TRUE)
#res_prod_fm[2,1:3,h] <- unlist(results[c(1,2,4)])

#results <- plot_RI(dta_bal, man = "mgt_woman", out_sp1 =paste("yield_sp1",paste("_pl",1:5, sep=""), sep=""),out_sp2 =paste("yield_sp2",paste("_pl",1:5, sep=""), sep=""),treatment , repl, trimlog = TRUE)
#res_prod_fm[3,1:3,h] <- unlist(results[c(1,2,4)])

#results <- plot_RI(dta_bal, man = "mgt_woman", out_sp1 =paste("yield_better_sp1",paste("_pl",1:5, sep=""), sep=""),out_sp2 =paste("yield_better_sp2",paste("_pl",1:5, sep=""), sep=""),treatment , repl, trimlog = FALSE)
#res_prod_fm[4,1:3,h] <- unlist(results[c(1,2,4)])

#results <- plot_RI(dta_bal, man = "mgt_both", out_sp1 =c("prod_pl1_sp1","prod_pl2_sp1","prod_pl3_sp1","prod_pl4_sp1", "prod_pl5_sp1"),out_sp2 =c("prod_pl1_sp2","prod_pl2_sp2","prod_pl3_sp2","prod_pl4_sp2", "prod_pl5_sp2"),treatment , repl, trimlog = TRUE)
#res_prod_bm[1,1:3,h] <- unlist(results[c(1,2,4)])

#results <- plot_RI(dta_bal, man = "mgt_both", out_sp1 =c("area_pl1_sp1","area_pl2_sp1","area_pl3_sp1","area_pl4_sp1", "area_pl5_sp1"),out_sp2 =c("area_pl1_sp2","area_pl2_sp2","area_pl3_sp2","area_pl4_sp2", "area_pl5_sp2"),treatment , repl, trimlog = TRUE)
#res_prod_bm[2,1:3,h] <- unlist(results[c(1,2,4)])

#results <- plot_RI(dta_bal, man = "mgt_both", out_sp1 =paste("yield_sp1",paste("_pl",1:5, sep=""), sep=""),out_sp2 =paste("yield_sp2",paste("_pl",1:5, sep=""), sep=""),treatment , repl, trimlog = TRUE)
#res_prod_bm[3,1:3,h] <- unlist(results[c(1,2,4)])

#results <- plot_RI(dta_bal, man = "mgt_both", out_sp1 =paste("yield_better_sp1",paste("_pl",1:5, sep=""), sep=""),out_sp2 =paste("yield_better_sp2",paste("_pl",1:5, sep=""), sep=""),treatment , repl, trimlog = FALSE)
#res_prod_bm[4,1:3,h] <- unlist(results[c(1,2,4)])


}


