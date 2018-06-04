rm(list=ls())
source("/home/bjvca/data/projects/digital green/endline/data/init.R")
#source("functions.R")
#dta <- read.csv("AWS.csv")

#set totrep to zero if you do not want simulation based inferecne
totrep <- 10000
#set this to true if you want to run WYFSR
wyfs_stat <- FALSE

set.seed(07032018)
dta <- subset(dta, !is.na(interview_status))
dta$messenger <- as.character(dta$messenger)

### indexing results arrays
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
## drop the control
dta <- subset(dta, messenger != "ctrl")


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



for (h in 1:4) {
if (h==1) {
############################################ compare male messenger to female messenger #########################################################
dta_bal <- subset(dta, messenger == "male" | messenger == "female")
treatment <- "(messenger == 'female') +ivr+sms+as.factor(recipient)+ as.factor(messenger)" 
} else if (h==2) {
############################################# compare male messenger to female messenger  ##############################################
dta_bal <- subset(dta, messenger == "male" | messenger == "couple")
treatment <- "(messenger == 'couple') +ivr+sms+as.factor(recipient)+ as.factor(messenger)" 
} else if (h==3) {
##################################################  compare male recipient to female recipient ###################################################
dta_bal <- subset(dta, recipient == "male" | recipient == "female")
treatment <- "(recipient == 'female') +ivr+sms+as.factor(recipient)+ as.factor(messenger)" 

} else if (h==4) {
############################################################## gender matching ###############################################################
dta_bal <- subset(dta, recipient == "male" | recipient == "couple")
treatment <- "(recipient == 'couple') +ivr+sms+as.factor(recipient)+ as.factor(messenger)"
}

############################### knowledge  ############################

res_know_m[1,1,h] <- ifelse(h <=2, mean(dta_bal$know_space_m[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$know_space_m[dta_bal$recipient == "male"], na.rm=T))
res_know_m[2,1,h] <-  ifelse(h <=2, sd(dta_bal$know_space_m[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$know_space_m[dta_bal$recipient == "male"], na.rm=T))
res_know_m[1,2,h] <- summary(lm(as.formula(paste("know_space_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_know_m[2,2,h] <- summary(lm(as.formula(paste("know_space_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
res_know_m[1,3,h] <- ifelse(totrep >0, RI("know_space_m",treatment , dta_bal, nr_repl = totrep, h),summary(lm(as.formula(paste("know_space_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

res_know_m[3,1,h] <- ifelse(h <=2, mean(dta_bal$know_combine_m[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$know_combine_m[dta_bal$recipient == "male"], na.rm=T))
res_know_m[4,1,h] <-  ifelse(h <=2, sd(dta_bal$know_combine_m[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$know_combine_m[dta_bal$recipient == "male"], na.rm=T))
res_know_m[3,2,h] <- summary(lm(as.formula(paste("know_combine_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_know_m[4,2,h] <- summary(lm(as.formula(paste("know_combine_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
res_know_m[3,3,h] <-  ifelse(totrep >0, RI("know_combine_m",treatment , dta_bal, nr_repl = totrep, h),summary(lm(as.formula(paste("know_combine_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

res_know_m[5,1,h] <- ifelse(h <=2, mean(dta_bal$know_weed_m[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$know_weed_m[dta_bal$recipient == "male"], na.rm=T))
res_know_m[6,1,h] <-  ifelse(h <=2, sd(dta_bal$know_weed_m[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$know_weed_m[dta_bal$recipient == "male"], na.rm=T))
res_know_m[5,2,h] <- summary(lm(as.formula(paste("know_weed_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_know_m[6,2,h] <- summary(lm(as.formula(paste("know_weed_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
res_know_m[5,3,h] <-  ifelse(totrep >0, RI("know_weed_m",treatment , dta_bal, nr_repl = totrep, h),summary(lm(as.formula(paste("know_weed_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

res_know_m[7,1,h] <- ifelse(h <=2, mean(dta_bal$know_armyworm_m[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$know_armyworm_m[dta_bal$recipient == "male"], na.rm=T))
res_know_m[8,1,h] <-  ifelse(h <=2, sd(dta_bal$know_armyworm_m[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$know_armyworm_m[dta_bal$recipient == "male"], na.rm=T))
res_know_m[7,2,h] <- summary(lm(as.formula(paste("know_armyworm_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_know_m[8,2,h] <- summary(lm(as.formula(paste("know_armyworm_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
res_know_m[7,3,h] <-  ifelse(totrep >0, RI("know_weed_m",treatment , dta_bal, nr_repl = totrep, h),summary(lm(as.formula(paste("know_armyworm_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])


indexer <- FW_index(treatment, c("know_space_m", "know_combine_m", "know_weed_m","know_armyworm_m"),dta_bal, nr_repl=totrep)
res_know_m[9,1,h] <- ifelse(h <=2, mean(indexer[[3]]$index[indexer[[3]]$messenger == "male"], na.rm=T), mean(indexer[[3]]$index[indexer[[3]]$recipient == "male"], na.rm=T))
res_know_m[10,1,h] <- ifelse(h <=2, sd(indexer[[3]]$index[indexer[[3]]$messenger == "male"], na.rm=T), sd(indexer[[3]]$index[indexer[[3]]$recipient == "male"], na.rm=T))
res_know_m[9,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_know_m[10,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_know_m[9,3,h] <-  indexer[[2]]


res_know_w[1,1,h] <- ifelse(h <=2, mean(dta_bal$know_space_w[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$know_space_w[dta_bal$recipient == "male"], na.rm=T))
res_know_w[2,1,h] <-  ifelse(h <=2, sd(dta_bal$know_space_w[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$know_space_w[dta_bal$recipient == "male"], na.rm=T))
res_know_w[1,2,h] <- summary(lm(as.formula(paste("know_space_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_know_w[2,2,h] <- summary(lm(as.formula(paste("know_space_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
res_know_w[1,3,h] <- ifelse(totrep >0, RI("know_space_w",treatment , dta_bal, nr_repl = totrep, h),summary(lm(as.formula(paste("know_space_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

res_know_w[3,1,h] <- ifelse(h <=2, mean(dta_bal$know_combine_w[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$know_combine_w[dta_bal$recipient == "male"], na.rm=T))
res_know_w[4,1,h] <-  ifelse(h <=2, sd(dta_bal$know_combine_w[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$know_combine_w[dta_bal$recipient == "male"], na.rm=T))
res_know_w[3,2,h] <- summary(lm(as.formula(paste("know_combine_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_know_w[4,2,h] <- summary(lm(as.formula(paste("know_combine_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
res_know_w[3,3,h] <-  ifelse(totrep >0, RI("know_combine_w",treatment , dta_bal, nr_repl = totrep, h),summary(lm(as.formula(paste("know_combine_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

res_know_w[5,1,h] <- ifelse(h <=2, mean(dta_bal$know_weed_w[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$know_weed_w[dta_bal$recipient == "male"], na.rm=T))
res_know_w[6,1,h] <-  ifelse(h <=2, sd(dta_bal$know_weed_w[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$know_weed_w[dta_bal$recipient == "male"], na.rm=T))
res_know_w[5,2,h] <- summary(lm(as.formula(paste("know_weed_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_know_w[6,2,h] <- summary(lm(as.formula(paste("know_weed_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
res_know_w[5,3,h] <-  ifelse(totrep >0, RI("know_weed_w",treatment , dta_bal, nr_repl = totrep, h),summary(lm(as.formula(paste("know_weed_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

res_know_w[7,1,h] <- ifelse(h <=2, mean(dta_bal$know_armyworm_w[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$know_armyworm_w[dta_bal$recipient == "male"], na.rm=T))
res_know_w[8,1,h] <-  ifelse(h <=2, sd(dta_bal$know_armyworm_w[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$know_armyworm_w[dta_bal$recipient == "male"], na.rm=T))
res_know_w[7,2,h] <- summary(lm(as.formula(paste("know_armyworm_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_know_w[8,2,h] <- summary(lm(as.formula(paste("know_armyworm_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,2]
res_know_w[7,3,h] <-  ifelse(totrep >0, RI("know_weed_w",treatment , dta_bal, nr_repl = totrep, h),summary(lm(as.formula(paste("know_armyworm_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])


indexer <- FW_index(treatment, c("know_space_w", "know_combine_w", "know_weed_w","know_armyworm_w"),dta_bal, nr_repl=totrep)
res_know_w[9,1,h] <- ifelse(h <=2, mean(indexer[[3]]$index[indexer[[3]]$messenger == "male"], na.rm=T), mean(indexer[[3]]$index[indexer[[3]]$recipient == "male"], na.rm=T))
res_know_w[10,1,h] <- ifelse(h <=2, sd(indexer[[3]]$index[indexer[[3]]$messenger == "male"], na.rm=T), sd(indexer[[3]]$index[indexer[[3]]$recipient == "male"], na.rm=T))
res_know_w[9,2,h] <-  summary(indexer[[1]])$coefficients[2,1]
res_know_w[10,2,h] <-  summary(indexer[[1]])$coefficients[2,2]
res_know_w[9,3,h] <-  indexer[[2]]

### agreement
res_decision_b[1,1,h]  <- ifelse(h <=2, mean(dta_bal$both_tell[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$both_tell[dta_bal$recipient == "male"], na.rm=T))
res_decision_b[2,1,h]  <- ifelse(h <=2, sd(dta_bal$both_tell[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$both_tell[dta_bal$recipient == "male"], na.rm=T))
res_decision_b[1,2,h]  <- summary(lm(as.formula(paste("both_tell",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_decision_b[2,2,h]  <- summary(lm(as.formula(paste("both_tell",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_decision_b[1,3,h]  <- ifelse(totrep >0, RI("both_tell",treatment , dta_bal, nr_repl = totrep, h), summary(lm(as.formula(paste("both_tell",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])


res_decision_b[3,1,h]  <- ifelse(h <=2, mean(dta_bal$spouses_listen[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$spouses_listen[dta_bal$recipient == "male"], na.rm=T))
res_decision_b[4,1,h]  <- ifelse(h <=2, sd(dta_bal$spouses_listen[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$spouses_listen[dta_bal$recipient == "male"], na.rm=T))
res_decision_b[3,2,h]  <- summary(lm(as.formula(paste("spouses_listen",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_decision_b[4,2,h]  <- summary(lm(as.formula(paste("spouses_listen",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_decision_b[3,3,h]  <- ifelse(totrep >0, RI("spouses_listen",treatment , dta_bal, nr_repl = totrep, h), summary(lm(as.formula(paste("spouses_listen",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])


res_decision_m[1,1,h]  <- ifelse(h <=2, mean(dta_bal$man_tells_wife[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$man_tells_wife[dta_bal$recipient == "male"], na.rm=T))
res_decision_m[2,1,h]  <- ifelse(h <=2, sd(dta_bal$man_tells_wife[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$man_tells_wife[dta_bal$recipient == "male"], na.rm=T))
res_decision_m[1,2,h]  <- summary(lm(as.formula(paste("man_tells_wife",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_decision_m[2,2,h]  <- summary(lm(as.formula(paste("man_tells_wife",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_decision_m[1,3,h]  <- ifelse(totrep >0, RI("man_tells_wife",treatment , dta_bal, nr_repl = totrep, h), summary(lm(as.formula(paste("man_tells_wife",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])


res_decision_m[3,1,h]  <- ifelse(h <=2, mean(dta_bal$wife_listens[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$wife_listens[dta_bal$recipient == "male"], na.rm=T))
res_decision_m[4,1,h]  <- ifelse(h <=2, sd(dta_bal$wife_listens[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$wife_listens[dta_bal$recipient == "male"], na.rm=T))
res_decision_m[3,2,h]  <- summary(lm(as.formula(paste("wife_listens",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_decision_m[4,2,h]  <- summary(lm(as.formula(paste("wife_listens",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_decision_m[3,3,h]  <- ifelse(totrep >0, RI("wife_listens",treatment , dta_bal, nr_repl = totrep, h), summary(lm(as.formula(paste("wife_listens",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

res_decision_w[1,1,h]  <- ifelse(h <=2, mean(dta_bal$wife_tells_man[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$wife_tells_man[dta_bal$recipient == "male"], na.rm=T))
res_decision_w[2,1,h]  <- ifelse(h <=2, sd(dta_bal$wife_tells_man[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$wife_tells_man[dta_bal$recipient == "male"], na.rm=T))
res_decision_w[1,2,h]  <- summary(lm(as.formula(paste("wife_tells_man",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_decision_w[2,2,h]  <- summary(lm(as.formula(paste("wife_tells_man",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_decision_w[1,3,h]  <- ifelse(totrep >0, RI("wife_tells_man",treatment , dta_bal, nr_repl = totrep, h), summary(lm(as.formula(paste("wife_tells_man",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

res_decision_w[3,1,h]  <- ifelse(h <=2, mean(dta_bal$man_listens[dta_bal$messenger == "male"], na.rm=T),mean(dta_bal$man_listens[dta_bal$recipient == "male"], na.rm=T))
res_decision_w[4,1,h]  <- ifelse(h <=2, sd(dta_bal$man_listens[dta_bal$messenger == "male"], na.rm=T),sd(dta_bal$man_listens[dta_bal$recipient == "male"], na.rm=T))
res_decision_w[3,2,h]  <- summary(lm(as.formula(paste("man_listens",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_decision_w[4,2,h]  <- summary(lm(as.formula(paste("man_listens",treatment,sep = "~")), data=dta_bal))$coefficients[2,2]
res_decision_w[3,3,h]  <- ifelse(totrep >0, RI("man_listens",treatment , dta_bal, nr_repl = totrep, h), summary(lm(as.formula(paste("man_listens",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

####### decisions
results <- plot_RI_dec(dta_bal,"dectime_man",treatment, nr_repl = totrep, h=h)
res_dec_m[1,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_m[2,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"decspace_man",treatment, nr_repl = totrep, h=h)
res_dec_m[3,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_m[4,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"decstriga_man",treatment, nr_repl = totrep, h=h)
res_dec_m[5,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_m[6,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"decweed_man",treatment, nr_repl = totrep, h=h)
res_dec_m[7,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_m[8,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"decfert_man",treatment,  nr_repl = totrep, h=h)
res_dec_m[9,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_m[10,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"decseed_man",treatment, nr_repl = totrep, h=h)
res_dec_m[11,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_m[12,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"deccombiner_man",treatment, nr_repl = totrep, h=h)
res_dec_m[13,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_m[14,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"decbuyseed_man",treatment, nr_repl = totrep, h=h)
res_dec_m[15,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_m[16,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"decchem_man",treatment,  nr_repl = totrep, h=h)
res_dec_m[17,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_m[18,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"dec_man_d",treatment, nr_repl = totrep, h=h)
res_dec_m[19,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_m[20,1:2,h] <- unlist(results[c(6,2)])


#space_ind <- reshape(dta[c("messenger","recipient","gender1","ivr","sms","called","totsms","hhid","distID", "subID","vilID", dec_vars)], varying = dec_vars,v.names="dectime_man", idvar="hhid", direction="long")

#dec_vars <- paste("decspace_man",paste("_pl",1:5, sep=""), sep="")

#space_ind2 <- reshape(dta[c("messenger","recipient","gender1","ivr","sms","called","totsms","hhid","distID", "subID","vilID", dec_vars)], varying = dec_vars,v.names="decspace_man", idvar="hhid", direction="long")[c("hhid","time","decspace_man")]

#dec_vars <- paste("decstriga_man",paste("_pl",1:5, sep=""), sep="")

#space_ind3 <- reshape(dta[c("messenger","recipient","gender1","ivr","sms","called","totsms","hhid","distID", "subID","vilID", dec_vars)], varying = dec_vars,v.names="decstriga_man", idvar="hhid", direction="long")[c("hhid","time","decstriga_man")]

#dec_vars <- paste("decweed_man",paste("_pl",1:5, sep=""), sep="")

#space_ind4 <- reshape(dta[c("messenger","recipient","gender1","ivr","sms","called","totsms","hhid","distID", "subID","vilID", dec_vars)], varying = dec_vars,v.names="decweed_man", idvar="hhid", direction="long")[c("hhid","time","decweed_man")]


#dta_ind <-  merge(merge(merge(space_ind, space_ind2, by=c("hhid","time")),space_ind3, by=c("hhid","time")),space_ind4, by=c("hhid","time"))
### this works but it is very slow....
#system.time(reser <- FSR_RI_plot2(c("dectime_man","decspace_man", "decstriga_man", "decweed_man"), treatment, dta_ind, pvals = c(0.0215,0.0492,0.2630,0.0175),100))

####### decisions woman
results <- plot_RI_dec(dta_bal,"dectime_woman",treatment, nr_repl = totrep, h=h)
res_dec_w[1,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_w[2,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"decspace_woman",treatment,  nr_repl = totrep, h=h)
res_dec_w[3,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_w[4,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"decstriga_woman",treatment,  nr_repl = totrep, h=h)
res_dec_w[5,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_w[6,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"decweed_woman",treatment,  nr_repl = totrep, h=h)
res_dec_w[7,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_w[8,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"decfert_woman",treatment,  nr_repl = totrep, h=h)
res_dec_w[9,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_w[10,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"decseed_woman",treatment,  nr_repl = totrep, h=h)
res_dec_w[11,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_w[12,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"deccombiner_woman",treatment, nr_repl = totrep, h=h)
res_dec_w[13,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_w[14,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"decbuyseed_woman",treatment,  nr_repl = totrep, h=h)
res_dec_w[15,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_w[16,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"decchem_woman",treatment,  nr_repl = totrep, h=h)
res_dec_w[17,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_w[18,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"dec_woman_d",treatment,  nr_repl = totrep, h=h)
res_dec_w[19,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_w[20,1:2,h] <- unlist(results[c(6,2)])


####### decisions both
results <- plot_RI_dec(dta_bal,"dectime_both",treatment, nr_repl = totrep, h=h)
res_dec_b[1,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_b[2,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"decspace_both",treatment,  nr_repl = totrep, h=h)
res_dec_b[3,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_b[4,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"decstriga_both",treatment,  nr_repl = totrep, h=h)
res_dec_b[5,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_b[6,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"decweed_both",treatment,  nr_repl = totrep, h=h)
res_dec_b[7,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_b[8,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"decfert_both",treatment,  nr_repl = totrep, h=h)
res_dec_b[9,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_b[10,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"decseed_both",treatment,  nr_repl = totrep, h=h)
res_dec_b[11,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_b[12,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"deccombiner_both",treatment,  nr_repl = totrep, h=h)
res_dec_b[13,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_b[14,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"decbuyseed_both",treatment,  nr_repl = totrep, h=h)
res_dec_b[15,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_b[16,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"decchem_both",treatment,  nr_repl = totrep, h=h)
res_dec_b[17,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_b[18,1:2,h] <- unlist(results[c(6,2)])

results <- plot_RI_dec(dta_bal,"dec_both_d",treatment, nr_repl = totrep, h=h)
res_dec_b[19,1:3,h] <- ifelse(totrep>0, unlist(results[c(5,1,3)]),unlist(results[c(5,1,3)]))
res_dec_b[20,1:2,h] <- unlist(results[c(6,2)])
}


