rm(list=ls())
source("/home/bjvca/data/projects/digital green/endline/data/init.R")
#set totrep to zero if you do not want simulation based inferecne
totrep <- 0
#set this to true if you want to run WYFSR
wyfs_stat <- FALSE

set.seed(07032018)
dta <- subset(dta, !is.na(interview_status))
dta$messenger <- as.character(dta$messenger)

### indexing results arrays
res_know_m <- array(NA, c(5,4,4)) 
rownames(res_know_m) <- c("know_space","know_combine","know_weed", "know_armyworm","know_ind")
res_know_w <- array(NA, c(5,4,4)) 
rownames(res_know_w) <- c("know_space","know_combine","know_weed", "know_armyworm","know_ind")

res_pract_mm <- array(NA, c(15,4,4))
rownames(res_pract_mm) <- c("space","striga","weed", "use_fert","use_DAP","use_urea","use_organic","seed","hybrid","opv","combiner","bought_seed","chem","labour","pract_index")
res_pract_wm <- array(NA, c(15,4,4))
rownames(res_pract_wm) <- c("space","striga","weed", "use_fert","use_DAP","use_urea","use_organic","seed","hybrid","opv","combiner","bought_seed","chem","labour","pract_index")
res_pract_wi <- array(NA, c(15,4,4))
rownames(res_pract_wi) <- c("space","striga","weed", "use_fert","use_DAP","use_urea","use_organic","seed","hybrid","opv","combiner","bought_seed","chem","labour","pract_index")
res_pract_bm <- array(NA, c(15,4,4))
rownames(res_pract_bm) <- c("space","striga","weed", "use_fert","use_DAP","use_urea","use_organic","seed","hybrid","opv","combiner","bought_seed","chem","labour","pract_index")
res_pract_wm_share <- array(NA, c(15,4,4))
rownames(res_pract_wm_share) <- c("space","striga","weed", "use_fert","use_DAP","use_urea","use_organic","seed","hybrid","opv","combiner","bought_seed","chem","labour","pract_index")
res_pract_wm_share2 <- array(NA, c(15,4,4))
rownames(res_pract_wm_share2) <- c("space","striga","weed", "use_fert","use_DAP","use_urea","use_organic","seed","hybrid","opv","combiner","bought_seed","chem","labour","pract_index")



res_prod_fm <- array(NA, c(5,4,4))
rownames(res_prod_fm) <- c("prod","area","yield","yield_better","prod_index")
res_prod_fi <- array(NA, c(5,4,4))
rownames(res_prod_fi) <- c("prod","area","yield","yield_better","prod_index")
res_prod_mm <- array(NA, c(5,4,4))
rownames(res_prod_mm) <- c("prod","area","yield","yield_better","prod_index")
res_prod_bm <- array(NA, c(5,4,4))
rownames(res_prod_bm) <- c("prod","area","yield","yield_better","prod_index")
res_prod_wm_share1 <- array(NA, c(5,4,4))
rownames(res_prod_wm_share1) <- c("prod","area","yield","yield_better","prod_index")
res_prod_wm_share2 <- array(NA, c(5,4,4))
rownames(res_prod_wm_share2) <- c("prod","area","yield","yield_better","prod_index")


res_h0_wel <-  array(NA, c(6,4,4))
rownames(res_h0_wel) <- c("better_av","better_6m","eatpref","eatenough","log_cons","welfare_index")
res_h0_disp <-  array(NA, c(4,4,4))
rownames(res_h0_disp) <- c("cons_maize","sold_maize","saved_seed","disp_index")
res_decision <- array(NA, c(6,4,4))
rownames(res_decision) <- c("dec_male","decide_female","female_involved","decide_joint", "both_tell","spouses_listen")

# run this analysis for 4 hypotheses:
# 1: T-C
# 2: recipient == couple
# 3: messenger == couple
# 4: gender matching
for (h in 1:4) {
if (h==1) {
############################################ does the intervention work? (treat vs control) #########################################################
### remember to balance data over treatment cells, this involves taking random samples from each cell, so set seed
### remove missings due to attrition first, othewise we will sample missings
s_h1 <- min(table(dta$messenger[dta$messenger != "ctrl"], dta$recipient[dta$messenger != "ctrl"]))
dta_bal <- rbind(dta[dta$messenger=="ctrl",],
 dta[dta$messenger=="couple" & dta$recipient == "couple",][sample( nrow(dta[dta$messenger=="couple" & dta$recipient == "couple",]),s_h1),],
 dta[dta$messenger=="male" & dta$recipient == "couple",][sample( nrow(dta[dta$messenger=="male" & dta$recipient == "couple",]),s_h1),],
 dta[dta$messenger=="female" & dta$recipient == "couple",][sample( nrow(dta[dta$messenger=="female" & dta$recipient == "couple",]),s_h1),],
 dta[dta$messenger=="female" & dta$recipient == "male",][sample( nrow(dta[dta$messenger=="female" & dta$recipient == "male",]),s_h1),],
 dta[dta$messenger=="female" & dta$recipient == "female",][sample( nrow(dta[dta$messenger=="female" & dta$recipient == "female",]),s_h1),],
 dta[dta$messenger=="male" & dta$recipient == "male",][sample( nrow(dta[dta$messenger=="male" & dta$recipient == "male",]),s_h1),],
 dta[dta$messenger=="male" & dta$recipient == "female",][sample( nrow(dta[dta$messenger=="male" & dta$recipient == "female",]),s_h1),],
 dta[dta$messenger=="couple" & dta$recipient == "male",][sample( nrow(dta[dta$messenger=="couple" & dta$recipient == "male",]),s_h1),],
 dta[dta$messenger=="couple" & dta$recipient == "female",][sample( nrow(dta[dta$messenger=="couple" & dta$recipient == "female",]),s_h1),])

treatment <- "(messenger != 'ctrl') +ivr+sms+as.factor(recipient) + called + (totsms >0)" 
} else if (h==2) {
############################################# reducing information asymmetries ##############################################
## drop the control
dta <- subset(dta, messenger != "ctrl")
## sample size for balance H0
s_h0 <- min(table(dta$messenger, dta$recipient)[,1])
## sample size for balance H1
s_h1 <- min(table(dta$messenger, dta$recipient)[,-1])

dta_bal <- rbind( dta[dta$messenger=="couple" & dta$recipient == "couple",][sample( nrow(dta[dta$messenger=="couple" & dta$recipient == "couple",]),s_h0),],
 dta[dta$messenger=="male" & dta$recipient == "couple",][sample( nrow(dta[dta$messenger=="male" & dta$recipient == "couple",]),s_h0),],
 dta[dta$messenger=="female" & dta$recipient == "couple",][sample( nrow(dta[dta$messenger=="female" & dta$recipient == "couple",]),s_h0),],

 dta[dta$messenger=="female" & dta$recipient == "male",][sample( nrow(dta[dta$messenger=="female" & dta$recipient == "male",]),s_h1),],
 dta[dta$messenger=="female" & dta$recipient == "female",][sample( nrow(dta[dta$messenger=="female" & dta$recipient == "female",]),s_h1),],
 dta[dta$messenger=="male" & dta$recipient == "male",][sample( nrow(dta[dta$messenger=="male" & dta$recipient == "male",]),s_h1),],
 dta[dta$messenger=="male" & dta$recipient == "female",][sample( nrow(dta[dta$messenger=="male" & dta$recipient == "female",]),s_h1),],
 dta[dta$messenger=="couple" & dta$recipient == "male",][sample( nrow(dta[dta$messenger=="couple" & dta$recipient == "male",]),s_h1),],
 dta[dta$messenger=="couple" & dta$recipient == "female",][sample( nrow(dta[dta$messenger=="couple" & dta$recipient == "female",]),s_h1),])

treatment <- "(recipient == 'couple') +ivr+sms+as.factor(messenger) + called + (totsms >0)"
} else if (h==3) {
################################################## Projecting cooperative approach ###################################################
## sample size for balance H0 -  basically table(dta$recipient[dta$messenger == "couple"])
s_h0 <- min(table(dta$messenger, dta$recipient)[1,])

## sample size for balance H1
s_h1 <- min(table(dta$messenger, dta$recipient)[-1,])

dta_bal <- rbind( dta[dta$messenger=="couple" & dta$recipient == "couple",][sample( nrow(dta[dta$messenger=="couple" & dta$recipient == "couple",]),s_h0),],
 dta[dta$messenger=="male" & dta$recipient == "couple",][sample( nrow(dta[dta$messenger=="male" & dta$recipient == "couple",]),s_h1),],
 dta[dta$messenger=="female" & dta$recipient == "couple",][sample( nrow(dta[dta$messenger=="female" & dta$recipient == "couple",]),s_h1),],

 dta[dta$messenger=="female" & dta$recipient == "male",][sample( nrow(dta[dta$messenger=="female" & dta$recipient == "male",]),s_h1),],
 dta[dta$messenger=="female" & dta$recipient == "female",][sample( nrow(dta[dta$messenger=="female" & dta$recipient == "female",]),s_h1),],
 dta[dta$messenger=="male" & dta$recipient == "male",][sample( nrow(dta[dta$messenger=="male" & dta$recipient == "male",]),s_h1),],
 dta[dta$messenger=="male" & dta$recipient == "female",][sample( nrow(dta[dta$messenger=="male" & dta$recipient == "female",]),s_h1),],
 dta[dta$messenger=="couple" & dta$recipient == "male",][sample( nrow(dta[dta$messenger=="couple" & dta$recipient == "male",]),s_h0),],
 dta[dta$messenger=="couple" & dta$recipient == "female",][sample( nrow(dta[dta$messenger=="couple" & dta$recipient == "female",]),s_h0),])
treatment <- "(messenger == 'couple')+ivr+sms+as.factor(recipient) + called + (totsms >0)" 

} else if (h==4) {
############################################################## gender matching ###############################################################
dta <- subset(dta, messenger != "ctrl")
dta$recipient <- as.character(dta$recipient)

dta <- subset(dta, recipient != "couple" & messenger != "couple")
s_h0 <- min(table(dta$recipient, dta$messenger))

dta_bal <- rbind(
 dta[dta$messenger=="female" & dta$recipient == "male",][sample( nrow(dta[dta$messenger=="female" & dta$recipient == "male",]),s_h0),],
 dta[dta$messenger=="female" & dta$recipient == "female",][sample( nrow(dta[dta$messenger=="female" & dta$recipient == "female",]),s_h0),],
 dta[dta$messenger=="male" & dta$recipient == "male",][sample( nrow(dta[dta$messenger=="male" & dta$recipient == "male",]),s_h0),],
 dta[dta$messenger=="male" & dta$recipient == "female",][sample( nrow(dta[dta$messenger=="male" & dta$recipient == "female",]),s_h0),])

treatment <- "(messenger == recipient) +ivr+sms + called + (totsms >0)"
}

############################### knowledge  ############################

res_know_m[1,1,h] <- summary(lm(as.formula(paste("know_space_m",treatment, sep="~")) ,data=dta_bal))$coefficients[1,1]
res_know_m[1,2,h] <- summary(lm(as.formula(paste("know_space_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_know_m[1,3,h] <- ifelse(totrep >0, RI("know_space_m",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("know_space_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

res_know_m[2,1,h] <- summary(lm(as.formula(paste("know_combine_m",treatment, sep="~")) ,data=dta_bal))$coefficients[1,1]
res_know_m[2,2,h] <- summary(lm(as.formula(paste("know_combine_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_know_m[2,3,h] <-  ifelse(totrep >0, RI("know_combine_m",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("know_combine_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

res_know_m[3,1,h] <- summary(lm(as.formula(paste("know_weed_m",treatment, sep="~")) ,data=dta_bal))$coefficients[1,1]
res_know_m[3,2,h] <- summary(lm(as.formula(paste("know_weed_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_know_m[3,3,h] <-  ifelse(totrep >0, RI("know_weed_m",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("know_weed_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

res_know_m[4,1,h] <- summary(lm(as.formula(paste("know_armyworm_m",treatment, sep="~")) ,data=dta_bal))$coefficients[1,1]
res_know_m[4,2,h] <- summary(lm(as.formula(paste("know_armyworm_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_know_m[4,3,h] <-  ifelse(totrep >0, RI("know_weed_m",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("know_armyworm_m",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

#indexer <- FW_index(treatment, c("know_space_m", "know_combine_m", "know_weed_m","know_armyworm_m"),dta_bal, nr_repl=totrep)
#res_know_m[5,1,h] <-  indexer[[1]]$coefficients[1,1]
#res_know_m[5,2,h] <-  indexer[[1]]$coefficients[2,1]
#res_know_m[5,3,h] <-  indexer[[2]]

#if (wyfs_stat) {
#	res_know_m[1:4,4,h] <- FSR_RI(c("know_space_m", "know_combine_m", "know_weed_m","know_armyworm_m"),treatment ,dta_bal, pvals = res_know_m[1:4,3,h], nr_repl_ri = 100)[[4]]
#	} else { 
#if (totrep >0) {
#	res_know_m[1:4,4,h] <- FSR_OLS( c("know_space_m", "know_combine_m", "know_weed_m","know_armyworm_m") ,treatment,dta_bal, nr_repl = totrep)[[4]]
#}
#}

res_know_w[1,1,h] <- summary(lm(as.formula(paste("know_space_w",treatment, sep="~")) ,data=dta_bal))$coefficients[1,1]
res_know_w[1,2,h] <- summary(lm(as.formula(paste("know_space_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_know_w[1,3,h] <- ifelse(totrep >0, RI("know_space_w",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("know_space_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

res_know_w[2,1,h] <- summary(lm(as.formula(paste("know_combine_w",treatment, sep="~")) ,data=dta_bal))$coefficients[1,1]
res_know_w[2,2,h] <- summary(lm(as.formula(paste("know_combine_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_know_w[2,3,h] <-  ifelse(totrep >0, RI("know_combine_w",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("know_combine_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

res_know_w[3,1,h] <- summary(lm(as.formula(paste("know_weed_w",treatment, sep="~")) ,data=dta_bal))$coefficients[1,1]
res_know_w[3,2,h] <- summary(lm(as.formula(paste("know_weed_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_know_w[3,3,h] <-  ifelse(totrep >0, RI("know_weed_w",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("know_weed_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

res_know_w[4,1,h] <- summary(lm(as.formula(paste("know_armyworm_w",treatment, sep="~")) ,data=dta_bal))$coefficients[1,1]
res_know_w[4,2,h] <- summary(lm(as.formula(paste("know_armyworm_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_know_w[4,3,h] <-  ifelse(totrep >0, RI("know_weed_w",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("know_armyworm_w",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

#indexer <- FW_index(treatment, c("know_space_w", "know_combine_w", "know_weed_w","know_armyworm_w"),dta_bal, nr_repl=totrep)
#res_know_w[5,1,h] <-  indexer[[1]]$coefficients[1,1]
#res_know_w[5,2,h] <-  indexer[[1]]$coefficients[2,1]
#res_know_w[5,3,h] <-  indexer[[2]]

#if (wyfs_stat) {
#	res_know_w[1:4,4,h] <- FSR_RI( c("know_space_w","know_combine_w","know_weed_w", "know_armyworm_w") ,treatment ,dta_bal, pvals = res_h0_know[1:4,3,h], nr_repl_ri = 100)[[4]]
#	} else { 
#if (totrep >0) {
#	res_know_w[1:4,4,h] <- FSR_OLS( c("know_space_w","know_combine_w","know_weed_w", "know_armyworm_w"),treatment,dta_bal, nr_repl = totrep)[[4]]
#}
#}

############################### practices #############################
### used recommended spacing use on at lease one plot as reported by at least one spouse

res_pract_mm[1,1,h]  <- summary(lm(as.formula(paste("space_mm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_mm[1,2,h]  <- summary(lm(as.formula(paste("space_mm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_mm[1,3,h]  <- ifelse(totrep >0, RI("space_mm",treatment, dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## used recommended way to fight striga - this should be changed to include info of all plots 
res_pract_mm[2,1,h]  <- summary(lm(as.formula(paste("striga", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_mm[2,2,h]  <- summary(lm(as.formula(paste("striga", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_mm[2,3,h]  <- ifelse(totrep >0, RI("striga",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("striga", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## weeded on recommended timing? - this should be changed to include info of all plots 
res_pract_mm[3,1,h]  <- summary(lm(as.formula(paste("weed", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_mm[3,2,h]  <- summary(lm(as.formula(paste("weed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_mm[3,3,h]  <- ifelse(totrep >0, RI("weed",treatment, dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("weed", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## fertilizer use
res_pract_mm[4,1,h]  <- summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_mm[4,2,h]  <- summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_mm[4,3,h]  <- ifelse(totrep >0, RI("fert",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
#### fert = DAP/NPK
res_pract_mm[5,1,h]  <- summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_mm[5,2,h]  <- summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_mm[5,3,h]  <- ifelse(totrep >0, RI("fert_dap",treatment , dta_bal, nr_repl = totrep) , summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

#### fert = urea
res_pract_mm[6,1,h]  <- summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_mm[6,2,h]  <- summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_mm[6,3,h]  <- ifelse(totrep >0, RI("fert_urea",treatment , dta_bal, nr_repl = totrep) , summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

#### fert = organic
res_pract_mm[7,1,h]  <-  summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_mm[7,2,h]  <- summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_mm[7,3,h]  <- ifelse(totrep >0, RI("fert_org",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

##improved seed  
res_pract_mm[8,1,h]  <- summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_mm[8,2,h]  <- summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_mm[8,3,h]  <- ifelse(totrep >0, RI("impseed",treatment , dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 


## hybrid
res_pract_mm[9,1,h] <- summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_mm[9,2,h] <- summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_mm[9,3,h] <- ifelse(totrep >0, RI("hybrid",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

## opv
res_pract_mm[10,1,h] <- summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_mm[10,2,h] <- summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_mm[10,3,h] <- ifelse(totrep >0, RI("opv",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

##combiner
res_pract_mm[11,1,h]  <- summary(lm(as.formula(paste("combiner", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_mm[11,2,h]  <- summary(lm(as.formula(paste("combiner", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_mm[11,3,h]  <- ifelse(totrep >0, RI("combiner",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("combiner", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

### bought seed
res_pract_mm[12,1,h]  <- summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_mm[12,2,h]  <- summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_mm[12,3,h]  <- ifelse(totrep >0, RI("bought_seed",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("bought_seed", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

#### used chemicals
res_pract_mm[13,1,h]  <- summary(lm(as.formula(paste("chem_mm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_mm[13,2,h]  <- summary(lm(as.formula(paste("chem_mm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_mm[13,3,h]  <- ifelse(totrep >0, RI("chem_mm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("chem_mm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

###hired labour
res_pract_mm[14,1,h]  <- summary(lm(as.formula(paste("labour_mm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_mm[14,2,h]  <- summary(lm(as.formula(paste("labour_mm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_mm[14,3,h]  <- ifelse(totrep >0, RI("labour_mm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("labour_mm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

##if (totrep >0) {
##res_h0_pract[1:7,4,h] <- FSR_RI( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment ,dta_bal, pvals =  res_h0_pract[,3,h] , nr_repl_pi = 100)

##res_h0_pract[1:7,4,h] <- FSR_OLS( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment,dta_bal, nr_repl = totrep)[[4]]

indexer <-  FW_index(treatment,c("space_mm","striga_mm","weed_mm", "fert_mm","impseed_mm", "combiner_mm","bought_seed_mm","chem_mm","labour_mm"),dta_bal, nr_repl=totrep)
res_pract_mm[15,1,h] <-  indexer[[1]]$coefficients[1,1]
res_pract_mm[15,2,h] <-  indexer[[1]]$coefficients[2,1]
res_pract_mm[15,3,h] <-  indexer[[2]]

res_pract_wm[1,1,h]  <- summary(lm(as.formula(paste("space_wm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm[1,2,h]  <- summary(lm(as.formula(paste("space_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm[1,3,h]  <- ifelse(totrep >0, RI("space_wm",treatment, dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## used recommended way to fight striga - this should be changed to include info of all plots 
res_pract_wm[2,1,h]  <- summary(lm(as.formula(paste("striga_wm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm[2,2,h]  <- summary(lm(as.formula(paste("striga_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm[2,3,h]  <- ifelse(totrep >0, RI("striga_wm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("striga_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## weeded on recommended timing? - this should be changed to include info of all plots 
res_pract_wm[3,1,h]  <- summary(lm(as.formula(paste("weed_wm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm[3,2,h]  <- summary(lm(as.formula(paste("weed_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm[3,3,h]  <- ifelse(totrep >0, RI("weed_wm",treatment, dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("weed_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## fertilizer use
res_pract_wm[4,1,h]  <- summary(lm(as.formula(paste("fert_wm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm[4,2,h]  <- summary(lm(as.formula(paste("fert_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm[4,3,h]  <- ifelse(totrep >0, RI("fert_wm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("fert_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
#### fert = DAP/NPK
res_pract_wm[5,1,h]  <- summary(lm(as.formula(paste("fert_dap_wm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm[5,2,h]  <- summary(lm(as.formula(paste("fert_dap_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm[5,3,h]  <- ifelse(totrep >0, RI("fert_dap_wm",treatment , dta_bal, nr_repl = totrep) , summary(lm(as.formula(paste("fert_dap_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

#### fert = urea
res_pract_wm[6,1,h]  <- summary(lm(as.formula(paste("fert_urea_wm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm[6,2,h]  <- summary(lm(as.formula(paste("fert_urea_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm[6,3,h]  <- ifelse(totrep >0, RI("fert_urea_wm",treatment , dta_bal, nr_repl = totrep) , summary(lm(as.formula(paste("fert_urea_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

#### fert = organic
res_pract_wm[7,1,h]  <-  summary(lm(as.formula(paste("fert_org_wm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm[7,2,h]  <- summary(lm(as.formula(paste("fert_org_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm[7,3,h]  <- ifelse(totrep >0, RI("fert_org_wm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("fert_org_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

##improved seed  
res_pract_wm[8,1,h]  <- summary(lm(as.formula(paste("impseed_wm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm[8,2,h]  <- summary(lm(as.formula(paste("impseed_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm[8,3,h]  <- ifelse(totrep >0, RI("impseed_wm",treatment , dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("impseed_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 


## hybrid
res_pract_wm[9,1,h] <- summary(lm(as.formula(paste("hybrid_wm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm[9,2,h] <- summary(lm(as.formula(paste("hybrid_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm[9,3,h] <- ifelse(totrep >0, RI("hybrid_wm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("hybrid_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

## opv
res_pract_wm[10,1,h] <- summary(lm(as.formula(paste("opv_wm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm[10,2,h] <- summary(lm(as.formula(paste("opv_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm[10,3,h] <- ifelse(totrep >0, RI("opv_wm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("opv_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

##combiner
res_pract_wm[11,1,h]  <- summary(lm(as.formula(paste("combiner_wm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm[11,2,h]  <- summary(lm(as.formula(paste("combiner_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm[11,3,h]  <- ifelse(totrep >0, RI("combiner_wm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("combiner_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

### bought seed
res_pract_wm[12,1,h]  <- summary(lm(as.formula(paste("bought_seed_wm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm[12,2,h]  <- summary(lm(as.formula(paste("bought_seed_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm[12,3,h]  <- ifelse(totrep >0, RI("bought_seed_wm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("bought_seed_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

#### used chemicals
res_pract_wm[13,1,h]  <- summary(lm(as.formula(paste("chem_wm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm[13,2,h]  <- summary(lm(as.formula(paste("chem_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm[13,3,h]  <- ifelse(totrep >0, RI("chem_wm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("chem_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

###hired labour
res_pract_wm[14,1,h]  <- summary(lm(as.formula(paste("labour_wm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm[14,2,h]  <- summary(lm(as.formula(paste("labour_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm[14,3,h]  <- ifelse(totrep >0, RI("labour_wm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("labour_wm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

##if (totrep >0) {
##res_h0_pract[1:7,4,h] <- FSR_RI( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment ,dta_bal, pvals =  res_h0_pract[,3,h] , nr_repl_pi = 100)

##res_h0_pract[1:7,4,h] <- FSR_OLS( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment,dta_bal, nr_repl = totrep)[[4]]

indexer <-  FW_index(treatment,c("space_wm","striga_wm","weed_wm", "fert_wm","impseed_wm", "combiner_wm","bought_seed_wm","chem_wm","labour_wm"),dta_bal, nr_repl=totrep)
res_pract_wm[15,1,h] <-  indexer[[1]]$coefficients[1,1]
res_pract_wm[15,2,h] <-  indexer[[1]]$coefficients[2,1]
res_pract_wm[15,3,h] <-  indexer[[2]]
res_pract_wi[1,1,h]  <- summary(lm(as.formula(paste("space_wi", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wi[1,2,h]  <- summary(lm(as.formula(paste("space_wi", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wi[1,3,h]  <- ifelse(totrep >0, RI("space_wi",treatment, dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## used recommended way to fight striga - this should be changed to include info of all plots 
res_pract_wi[2,1,h]  <- summary(lm(as.formula(paste("striga_wi", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wi[2,2,h]  <- summary(lm(as.formula(paste("striga_wi", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wi[2,3,h]  <- ifelse(totrep >0, RI("striga_wi",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("striga_wi", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## weeded on recommended timing? - this should be changed to include info of all plots 
res_pract_wi[3,1,h]  <- summary(lm(as.formula(paste("weed_wi", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wi[3,2,h]  <- summary(lm(as.formula(paste("weed_wi", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wi[3,3,h]  <- ifelse(totrep >0, RI("weed_wi",treatment, dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("weed_wi", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## fertilizer use
res_pract_wi[4,1,h]  <- summary(lm(as.formula(paste("fert_wi", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wi[4,2,h]  <- summary(lm(as.formula(paste("fert_wi", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wi[4,3,h]  <- ifelse(totrep >0, RI("fert_wi",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("fert_wi", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
#### fert = DAP/NPK
res_pract_wi[5,1,h]  <- summary(lm(as.formula(paste("fert_dap_wi", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wi[5,2,h]  <- summary(lm(as.formula(paste("fert_dap_wi", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wi[5,3,h]  <- ifelse(totrep >0, RI("fert_dap_wi",treatment , dta_bal, nr_repl = totrep) , summary(lm(as.formula(paste("fert_dap_wi", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

#### fert = urea
res_pract_wi[6,1,h]  <- summary(lm(as.formula(paste("fert_urea_wi", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wi[6,2,h]  <- summary(lm(as.formula(paste("fert_urea_wi", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wi[6,3,h]  <- ifelse(totrep >0, RI("fert_urea_wi",treatment , dta_bal, nr_repl = totrep) , summary(lm(as.formula(paste("fert_urea_wi", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

#### fert = organic
res_pract_wi[7,1,h]  <-  summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wi[7,2,h]  <- summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wi[7,3,h]  <- ifelse(totrep >0, RI("fert_org",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

##improved seed  
res_pract_wi[8,1,h]  <- summary(lm(as.formula(paste("impseed_wi", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wi[8,2,h]  <- summary(lm(as.formula(paste("impseed_wi", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wi[8,3,h]  <- ifelse(totrep >0, RI("impseed_wi",treatment , dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("impseed_wi", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 


## hybrid
res_pract_wi[9,1,h] <- summary(lm(as.formula(paste("hybrid_wi", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wi[9,2,h] <- summary(lm(as.formula(paste("hybrid_wi", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wi[9,3,h] <- ifelse(totrep >0, RI("hybrid_wi",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("hybrid_wi", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

## opv
res_pract_wi[10,1,h] <- summary(lm(as.formula(paste("opv_wi", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wi[10,2,h] <- summary(lm(as.formula(paste("opv_wi", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wi[10,3,h] <- ifelse(totrep >0, RI("opv_wi",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("opv_wi", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

##combiner
res_pract_wi[11,1,h]  <- summary(lm(as.formula(paste("combiner_wi", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wi[11,2,h]  <- summary(lm(as.formula(paste("combiner_wi", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wi[11,3,h]  <- ifelse(totrep >0, RI("combiner_wi",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("combiner_wi", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

### bought seed
res_pract_wi[12,1,h]  <- summary(lm(as.formula(paste("bought_seed_wi", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wi[12,2,h]  <- summary(lm(as.formula(paste("bought_seed_wi", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wi[12,3,h]  <- ifelse(totrep >0, RI("bought_seed_wi",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("bought_seed_wi", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

#### used chemicals
res_pract_wi[13,1,h]  <- summary(lm(as.formula(paste("chem_wi", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wi[13,2,h]  <- summary(lm(as.formula(paste("chem_wi", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wi[13,3,h]  <- ifelse(totrep >0, RI("chem_wi",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("chem_wi", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

###hired labour
res_pract_wi[14,1,h]  <- summary(lm(as.formula(paste("labour_wi", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wi[14,2,h]  <- summary(lm(as.formula(paste("labour_wi", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wi[14,3,h]  <- ifelse(totrep >0, RI("labour_wi",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("labour_wi", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

##if (totrep >0) {
##res_h0_pract[1:7,4,h] <- FSR_RI( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment ,dta_bal, pvals =  res_h0_pract[,3,h] , nr_repl_pi = 100)

##res_h0_pract[1:7,4,h] <- FSR_OLS( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment,dta_bal, nr_repl = totrep)[[4]]

indexer <-  FW_index(treatment,c("space_wi","striga_wi","weed_wi", "fert_wi","impseed_wi", "combiner_wi","bought_seed_wi","chem_wi","labour_wi"),dta_bal, nr_repl=totrep)
res_pract_wi[15,1,h] <-  indexer[[1]]$coefficients[1,1]
res_pract_wi[15,2,h] <-  indexer[[1]]$coefficients[2,1]
res_pract_wi[15,3,h] <-  indexer[[2]]
res_pract_bm[1,1,h]  <- summary(lm(as.formula(paste("space_bm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_bm[1,2,h]  <- summary(lm(as.formula(paste("space_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_bm[1,3,h]  <- ifelse(totrep >0, RI("space_bm",treatment, dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## used recommended way to fight striga - this should be changed to include info of all plots 
res_pract_bm[2,1,h]  <- summary(lm(as.formula(paste("striga_bm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_bm[2,2,h]  <- summary(lm(as.formula(paste("striga_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_bm[2,3,h]  <- ifelse(totrep >0, RI("striga_bm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("striga_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## weeded on recommended timing? - this should be changed to include info of all plots 
res_pract_bm[3,1,h]  <- summary(lm(as.formula(paste("weed_bm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_bm[3,2,h]  <- summary(lm(as.formula(paste("weed_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_bm[3,3,h]  <- ifelse(totrep >0, RI("weed_bm",treatment, dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("weed_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## fertilizer use
res_pract_bm[4,1,h]  <- summary(lm(as.formula(paste("fert_bm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_bm[4,2,h]  <- summary(lm(as.formula(paste("fert_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_bm[4,3,h]  <- ifelse(totrep >0, RI("fert_bm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("fert_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
#### fert = DAP/NPK
res_pract_bm[5,1,h]  <- summary(lm(as.formula(paste("fert_dap_bm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_bm[5,2,h]  <- summary(lm(as.formula(paste("fert_dap_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_bm[5,3,h]  <- ifelse(totrep >0, RI("fert_dap_bm",treatment , dta_bal, nr_repl = totrep) , summary(lm(as.formula(paste("fert_dap_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

#### fert = urea
res_pract_bm[6,1,h]  <- summary(lm(as.formula(paste("fert_urea_bm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_bm[6,2,h]  <- summary(lm(as.formula(paste("fert_urea_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_bm[6,3,h]  <- ifelse(totrep >0, RI("fert_urea_bm",treatment , dta_bal, nr_repl = totrep) , summary(lm(as.formula(paste("fert_urea_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

#### fert = organic
res_pract_bm[7,1,h]  <-  summary(lm(as.formula(paste("fert_org_bm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_bm[7,2,h]  <- summary(lm(as.formula(paste("fert_org_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_bm[7,3,h]  <- ifelse(totrep >0, RI("fert_org_bm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("fert_org_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

##improved seed  
res_pract_bm[8,1,h]  <- summary(lm(as.formula(paste("impseed_bm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_bm[8,2,h]  <- summary(lm(as.formula(paste("impseed_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_bm[8,3,h]  <- ifelse(totrep >0, RI("impseed_bm",treatment , dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("impseed_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 


## hybrid
res_pract_bm[9,1,h] <- summary(lm(as.formula(paste("hybrid_bm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_bm[9,2,h] <- summary(lm(as.formula(paste("hybrid_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_bm[9,3,h] <- ifelse(totrep >0, RI("hybrid_bm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("hybrid_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

## opv
res_pract_bm[10,1,h] <- summary(lm(as.formula(paste("opv_bm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_bm[10,2,h] <- summary(lm(as.formula(paste("opv_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_bm[10,3,h] <- ifelse(totrep >0, RI("opv_bm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("opv_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

##combiner
res_pract_bm[11,1,h]  <- summary(lm(as.formula(paste("combiner_bm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_bm[11,2,h]  <- summary(lm(as.formula(paste("combiner_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_bm[11,3,h]  <- ifelse(totrep >0, RI("combiner_bm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("combiner_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

### bought seed
res_pract_bm[12,1,h]  <- summary(lm(as.formula(paste("bought_seed_bm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_bm[12,2,h]  <- summary(lm(as.formula(paste("bought_seed_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_bm[12,3,h]  <- ifelse(totrep >0, RI("bought_seed_bm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("bought_seed_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

#### used chemicals
res_pract_bm[13,1,h]  <- summary(lm(as.formula(paste("chem_bm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_bm[13,2,h]  <- summary(lm(as.formula(paste("chem_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_bm[13,3,h]  <- ifelse(totrep >0, RI("chem_bm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("chem_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

###hired labour
res_pract_bm[14,1,h]  <- summary(lm(as.formula(paste("labour_bm", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_bm[14,2,h]  <- summary(lm(as.formula(paste("labour_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_bm[14,3,h]  <- ifelse(totrep >0, RI("labour_bm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("labour_bm", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

##if (totrep >0) {
##res_h0_pract[1:7,4,h] <- FSR_RI( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment ,dta_bal, pvals =  res_h0_pract[,3,h] , nr_repl_pi = 100)

##res_h0_pract[1:7,4,h] <- FSR_OLS( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment,dta_bal, nr_repl = totrep)[[4]]

indexer <-  FW_index(treatment,c("space_bm","striga_bm","weed_bm", "fert_bm","impseed_bm", "combiner_bm","bought_seed_bm","chem_bm","labour_bm"),dta_bal, nr_repl=totrep)
res_pract_bm[15,1,h] <-  indexer[[1]]$coefficients[1,1]
res_pract_bm[15,2,h] <-  indexer[[1]]$coefficients[2,1]
res_pract_bm[15,3,h] <-  indexer[[2]]

res_pract_wm_share[1,1,h]  <- summary(lm(as.formula(paste("space_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share[1,2,h]  <- summary(lm(as.formula(paste("space_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share[1,3,h]  <- ifelse(totrep >0, RI("space_wm_share",treatment, dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## used recommended way to fight striga - this should be changed to include info of all plots 
res_pract_wm_share[2,1,h]  <- summary(lm(as.formula(paste("striga_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share[2,2,h]  <- summary(lm(as.formula(paste("striga_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share[2,3,h]  <- ifelse(totrep >0, RI("striga_wm_share",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("striga_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## weeded on recommended timing? - this should be changed to include info of all plots 
res_pract_wm_share[3,1,h]  <- summary(lm(as.formula(paste("weed_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share[3,2,h]  <- summary(lm(as.formula(paste("weed_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share[3,3,h]  <- ifelse(totrep >0, RI("weed_wm_share",treatment, dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("weed_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## fertilizer use
res_pract_wm_share[4,1,h]  <- summary(lm(as.formula(paste("fert_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share[4,2,h]  <- summary(lm(as.formula(paste("fert_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share[4,3,h]  <- ifelse(totrep >0, RI("fert_wm_share",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("fert_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
#### fert = DAP/NPK
res_pract_wm_share[5,1,h]  <- summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share[5,2,h]  <- summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share[5,3,h]  <- ifelse(totrep >0, RI("fert_dap",treatment , dta_bal, nr_repl = totrep) , summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

#### fert = urea
res_pract_wm_share[6,1,h]  <- summary(lm(as.formula(paste("fert_urea_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share[6,2,h]  <- summary(lm(as.formula(paste("fert_urea_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share[6,3,h]  <- ifelse(totrep >0, RI("fert_urea_wm_share",treatment , dta_bal, nr_repl = totrep) , summary(lm(as.formula(paste("fert_urea_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

#### fert = organic
res_pract_wm_share[7,1,h]  <-  summary(lm(as.formula(paste("fert_org_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share[7,2,h]  <- summary(lm(as.formula(paste("fert_org_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share[7,3,h]  <- ifelse(totrep >0, RI("fert_org_wm_share",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("fert_org_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

##improved seed  
res_pract_wm_share[8,1,h]  <- summary(lm(as.formula(paste("impseed_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share[8,2,h]  <- summary(lm(as.formula(paste("impseed_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share[8,3,h]  <- ifelse(totrep >0, RI("impseed_wm_share",treatment , dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("impseed_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 


## hybrid
res_pract_wm_share[9,1,h] <- summary(lm(as.formula(paste("hybrid_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share[9,2,h] <- summary(lm(as.formula(paste("hybrid_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share[9,3,h] <- ifelse(totrep >0, RI("hybrid_wm_share",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("hybrid_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

## opv
res_pract_wm_share[10,1,h] <- summary(lm(as.formula(paste("opv_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share[10,2,h] <- summary(lm(as.formula(paste("opv_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share[10,3,h] <- ifelse(totrep >0, RI("opv_wm_share",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("opv_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

##combiner
res_pract_wm_share[11,1,h]  <- summary(lm(as.formula(paste("combiner_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share[11,2,h]  <- summary(lm(as.formula(paste("combiner_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share[11,3,h]  <- ifelse(totrep >0, RI("combiner_wm_share",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("combiner_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

### bought seed
res_pract_wm_share[12,1,h]  <- summary(lm(as.formula(paste("bought_seed_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share[12,2,h]  <- summary(lm(as.formula(paste("bought_seed_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share[12,3,h]  <- ifelse(totrep >0, RI("bought_seed_wm_share",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("bought_seed_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

#### used chemicals
res_pract_wm_share[13,1,h]  <- summary(lm(as.formula(paste("chem_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share[13,2,h]  <- summary(lm(as.formula(paste("chem_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share[13,3,h]  <- ifelse(totrep >0, RI("chem_wm_share",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("chem_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

###hired labour
res_pract_wm_share[14,1,h]  <- summary(lm(as.formula(paste("labour_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share[14,2,h]  <- summary(lm(as.formula(paste("labour_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share[14,3,h]  <- ifelse(totrep >0, RI("labour_wm_share",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("labour_wm_share", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

##if (totrep >0) {
##res_h0_pract[1:7,4,h] <- FSR_RI( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment ,dta_bal, pvals =  res_h0_pract[,3,h] , nr_repl_pi = 100)

##res_h0_pract[1:7,4,h] <- FSR_OLS( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment,dta_bal, nr_repl = totrep)[[4]]

indexer <-  FW_index(treatment,c("space_wm_share","striga_wm_share","weed_wm_share", "fert_wm_share","impseed_wm_share", "combiner_wm_share","bought_seed_wm_share","chem_wm_share","labour_wm_share"),dta_bal, nr_repl=totrep)
res_pract_wm_share[15,1,h] <-  indexer[[1]]$coefficients[1,1]
res_pract_wm_share[15,2,h] <-  indexer[[1]]$coefficients[2,1]
res_pract_wm_share[15,3,h] <-  indexer[[2]]

res_pract_wm_share2[1,1,h]  <- summary(lm(as.formula(paste("space_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share2[1,2,h]  <- summary(lm(as.formula(paste("space_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share2[1,3,h]  <- ifelse(totrep >0, RI("space_wm_share2",treatment, dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## used recommended way to fight striga - this should be changed to include info of all plots 
res_pract_wm_share2[2,1,h]  <- summary(lm(as.formula(paste("striga_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share2[2,2,h]  <- summary(lm(as.formula(paste("striga_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share2[2,3,h]  <- ifelse(totrep >0, RI("striga_wm_share2",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("striga_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## weeded on recommended timing? - this should be changed to include info of all plots 
res_pract_wm_share2[3,1,h]  <- summary(lm(as.formula(paste("weed_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share2[3,2,h]  <- summary(lm(as.formula(paste("weed_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share2[3,3,h]  <- ifelse(totrep >0, RI("weed_wm_share2",treatment, dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("weed_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## fertilizer use
res_pract_wm_share2[4,1,h]  <- summary(lm(as.formula(paste("fert_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share2[4,2,h]  <- summary(lm(as.formula(paste("fert_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share2[4,3,h]  <- ifelse(totrep >0, RI("fert_wm_share2",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("fert_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
#### fert = DAP/NPK
res_pract_wm_share2[5,1,h]  <- summary(lm(as.formula(paste("fert_dap_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share2[5,2,h]  <- summary(lm(as.formula(paste("fert_dap_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share2[5,3,h]  <- ifelse(totrep >0, RI("fert_dap_wm_share2",treatment , dta_bal, nr_repl = totrep) , summary(lm(as.formula(paste("fert_dap_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

#### fert = urea
res_pract_wm_share2[6,1,h]  <- summary(lm(as.formula(paste("fert_urea_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share2[6,2,h]  <- summary(lm(as.formula(paste("fert_urea_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share2[6,3,h]  <- ifelse(totrep >0, RI("fert_urea_wm_share2",treatment , dta_bal, nr_repl = totrep) , summary(lm(as.formula(paste("fert_urea_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

#### fert = organic
res_pract_wm_share2[7,1,h]  <-  summary(lm(as.formula(paste("fert_org_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share2[7,2,h]  <- summary(lm(as.formula(paste("fert_org_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share2[7,3,h]  <- ifelse(totrep >0, RI("fert_org_wm_share2",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("fert_org_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

##improved seed  
res_pract_wm_share2[8,1,h]  <- summary(lm(as.formula(paste("impseed_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share2[8,2,h]  <- summary(lm(as.formula(paste("impseed_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share2[8,3,h]  <- ifelse(totrep >0, RI("impseed_wm_share2",treatment , dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("impseed_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 


## hybrid
res_pract_wm_share2[9,1,h] <- summary(lm(as.formula(paste("hybrid_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share2[9,2,h] <- summary(lm(as.formula(paste("hybrid_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share2[9,3,h] <- ifelse(totrep >0, RI("hybrid_wm_share2",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("hybrid_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

## opv
res_pract_wm_share2[10,1,h] <- summary(lm(as.formula(paste("opv_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share2[10,2,h] <- summary(lm(as.formula(paste("opv_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share2[10,3,h] <- ifelse(totrep >0, RI("opv_wm_share2",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("opv_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

##combiner
res_pract_wm_share2[11,1,h]  <- summary(lm(as.formula(paste("combiner_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share2[11,2,h]  <- summary(lm(as.formula(paste("combiner_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share2[11,3,h]  <- ifelse(totrep >0, RI("combiner_wm_share2",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("combiner_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

### bought seed
res_pract_wm_share2[12,1,h]  <- summary(lm(as.formula(paste("bought_seed_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share2[12,2,h]  <- summary(lm(as.formula(paste("bought_seed_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share2[12,3,h]  <- ifelse(totrep >0, RI("bought_seed_wm_share2",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("bought_seed_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

#### used chemicals
res_pract_wm_share2[13,1,h]  <- summary(lm(as.formula(paste("chem_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share2[13,2,h]  <- summary(lm(as.formula(paste("chem_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share2[13,3,h]  <- ifelse(totrep >0, RI("chem_wm_share2",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("chem_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

###hired labour
res_pract_wm_share2[14,1,h]  <- summary(lm(as.formula(paste("labour_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_pract_wm_share2[14,2,h]  <- summary(lm(as.formula(paste("labour_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_pract_wm_share2[14,3,h]  <- ifelse(totrep >0, RI("labour_wm_share2",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("labour_wm_share2", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

##if (totrep >0) {
##res_h0_pract[1:7,4,h] <- FSR_RI( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment ,dta_bal, pvals =  res_h0_pract[,3,h] , nr_repl_pi = 100)

##res_h0_pract[1:7,4,h] <- FSR_OLS( c("space","striga","weed", "fert","impseed", "combiner","bought_seed") ,treatment,dta_bal, nr_repl = totrep)[[4]]

indexer <-  FW_index(treatment,c("space_wm_share2","striga_wm_share2","weed_wm_share2", "fert_wm_share2","impseed_wm_share2", "combiner_wm_share2","bought_seed_wm_share2","chem_wm_share2","labour_wm_share2"),dta_bal, nr_repl=totrep)
res_pract_wm_share2[15,1,h] <-  indexer[[1]]$coefficients[1,1]
res_pract_wm_share2[15,2,h] <-  indexer[[1]]$coefficients[2,1]
res_pract_wm_share2[15,3,h] <-  indexer[[2]]
############################### production ###########################
### does the video increases production related outcomes?

#trimming is done on end result
dta_bal2 <- subset(dta_bal, prod_tot_fm>0)
dta_bal2$log_prod_tot_fm <- log(dta_bal2$prod_tot_fm)
dta_trim <- trim("log_prod_tot_fm", dta_bal2, .1)

### production
res_prod_fm[1,1,h] <- summary(lm(as.formula(paste("log_prod_tot_fm",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
res_prod_fm[1,2,h] <- summary(lm(as.formula(paste("log_prod_tot_fm",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_prod_fm[1,3,h] <- ifelse(totrep >0, RI("log_prod_tot_fm",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_prod_tot_fm",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

### area

dta_bal2 <- subset(dta_bal, area_tot_fm>0)
dta_bal2$log_area_tot_fm <- log(dta_bal2$area_tot_fm)

dta_trim <- trim("log_area_tot_fm", dta_bal2, .1)

res_prod_fm[2,1,h] <- summary(lm(as.formula(paste("log_area_tot_fm",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
res_prod_fm[2,2,h] <- summary(lm(as.formula(paste("log_area_tot_fm",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_prod_fm[2,3,h] <- ifelse(totrep >0, RI("log_area_tot_fm",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_area_tot_fm",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

###yield

dta_bal2 <- subset(dta_bal, yield_av_fm >0)
dta_bal2$log_yield_av_fm <- log(dta_bal2$yield_av_fm)
dta_trim <- trim("log_yield_av_fm", dta_bal2, .1)

res_prod_fm[3,1,h] <- summary(lm(as.formula(paste("log_yield_av_fm",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
res_prod_fm[3,2,h] <- summary(lm(as.formula(paste("log_yield_av_fm",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_prod_fm[3,3,h] <- ifelse(totrep >0, RI("log_yield_av_fm",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_yield_av_fm",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

### was yield better compared to normal year?
summary(lm(yield_better_fm~messenger != "ctrl", data=dta_bal))

res_prod_fm[4,1,h] <- summary(lm(as.formula(paste("yield_better_fm",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
res_prod_fm[4,2,h] <- summary(lm(as.formula(paste("yield_better_fm",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_prod_fm[4,3,h] <- ifelse(totrep >0, RI("yield_better_fm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("yield_better_fm",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

dta_bal2 <- subset(dta_bal, area_tot_fm >0 & prod_tot_fm>0 & yield_av_fm >0)
dta_bal2$log_prod_tot_fm <- log(dta_bal2$prod_tot_fm)
dta_bal2$log_area_tot_fm <- log(dta_bal2$area_tot_fm)
dta_bal2$log_yield_av_fm <- log(dta_bal2$yield_av_fm)

#dta_bal2 <- trim("log_prod_tot", dta_bal2, .1)
#dta_bal2 <- trim("log_area_tot", dta_bal2, .1)
dta_bal2 <- trim("log_yield_av_fm", dta_bal2, .1)


#res_prod_fm[1:4,4,h] <- FSR_OLS( c("log_prod_tot", "log_area_tot", "log_yield_av","yield_better") ,treatment,dta_bal, nr_repl = totrep)[[4]]

#if (totrep >0) {
#dta_bal2$log_area_tot <- -dta_bal2$log_area_tot

#	indexer <- FW_index(treatment, c("log_prod_tot_fm", "log_area_tot_fm", "log_yield_av_fm","yield_better_fm"),dta_bal2, nr_repl=totrep)
#	res_prod_fm[5,1,h] <-  indexer[[1]]$coefficients[1,1]
#	res_prod_fm[5,2,h] <-  indexer[[1]]$coefficients[2,1]
#	res_prod_fm[5,3,h] <-  indexer[[2]]
#	}

########################## wi#################################
#trimming is done on end result
dta_bal2 <- subset(dta_bal, prod_tot_fi>0)
dta_bal2$log_prod_tot_fi <- log(dta_bal2$prod_tot_fi)
dta_trim <- trim("log_prod_tot_fi", dta_bal2, .1)

### production
res_prod_fi[1,1,h] <- summary(lm(as.formula(paste("log_prod_tot_fi",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
res_prod_fi[1,2,h] <- summary(lm(as.formula(paste("log_prod_tot_fi",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_prod_fi[1,3,h] <- ifelse(totrep >0, RI("log_prod_tot_fi",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_prod_tot_fi",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

### area

dta_bal2 <- subset(dta_bal, area_tot_fi>0)
dta_bal2$log_area_tot_fi <- log(dta_bal2$area_tot_fi)

dta_trim <- trim("log_area_tot_fi", dta_bal2, .1)

res_prod_fi[2,1,h] <- summary(lm(as.formula(paste("log_area_tot_fi",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
res_prod_fi[2,2,h] <- summary(lm(as.formula(paste("log_area_tot_fi",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_prod_fi[2,3,h] <- ifelse(totrep >0, RI("log_area_tot_fi",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_area_tot_fi",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

###yield

dta_bal2 <- subset(dta_bal, yield_av_fi >0)
dta_bal2$log_yield_av_fi <- log(dta_bal2$yield_av_fi)
dta_trim <- trim("log_yield_av_fi", dta_bal2, .1)

res_prod_fi[3,1,h] <- summary(lm(as.formula(paste("log_yield_av_fi",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
res_prod_fi[3,2,h] <- summary(lm(as.formula(paste("log_yield_av_fi",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_prod_fi[3,3,h] <- ifelse(totrep >0, RI("log_yield_av_fi",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_yield_av_fi",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

### was yield better compared to normal year?
summary(lm(yield_better_fi~messenger != "ctrl", data=dta_bal))

res_prod_fi[4,1,h] <- summary(lm(as.formula(paste("yield_better_fi",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
res_prod_fi[4,2,h] <- summary(lm(as.formula(paste("yield_better_fi",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_prod_fi[4,3,h] <- ifelse(totrep >0, RI("yield_better_fi",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("yield_better_fi",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

dta_bal2 <- subset(dta_bal, area_tot_fi >0 & prod_tot_fi>0 & yield_av_fi >0)
dta_bal2$log_prod_tot_fi <- log(dta_bal2$prod_tot_fi)
dta_bal2$log_area_tot_fi <- log(dta_bal2$area_tot_fi)
dta_bal2$log_yield_av_fi <- log(dta_bal2$yield_av_fi)

#dta_bal2 <- trim("log_prod_tot", dta_bal2, .1)
#dta_bal2 <- trim("log_area_tot", dta_bal2, .1)
dta_bal2 <- trim("log_yield_av_fi", dta_bal2, .1)


#res_prod_fi[1:4,4,h] <- FSR_OLS( c("log_prod_tot", "log_area_tot", "log_yield_av","yield_better") ,treatment,dta_bal, nr_repl = totrep)[[4]]

#if (totrep >0) {
#dta_bal2$log_area_tot <- -dta_bal2$log_area_tot

#	indexer <- FW_index(treatment, c("log_prod_tot_fi", "log_area_tot_fi", "log_yield_av_fi","yield_better_fi"),dta_bal2, nr_repl=totrep)
#	res_prod_fi[5,1,h] <-  indexer[[1]]$coefficients[1,1]
#	res_prod_fi[5,2,h] <-  indexer[[1]]$coefficients[2,1]
#	res_prod_fi[5,3,h] <-  indexer[[2]]
#	}

######################## mm#################################
#trimming is done on end result
dta_bal2 <- subset(dta_bal, prod_tot_mm>0)
dta_bal2$log_prod_tot_mm <- log(dta_bal2$prod_tot_mm)
dta_trim <- trim("log_prod_tot_mm", dta_bal2, .1)

### production
res_prod_mm[1,1,h] <- summary(lm(as.formula(paste("log_prod_tot_mm",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
res_prod_mm[1,2,h] <- summary(lm(as.formula(paste("log_prod_tot_mm",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_prod_mm[1,3,h] <- ifelse(totrep >0, RI("log_prod_tot_mm",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_prod_tot_mm",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

### area

dta_bal2 <- subset(dta_bal, area_tot_mm>0)
dta_bal2$log_area_tot_mm <- log(dta_bal2$area_tot_mm)

dta_trim <- trim("log_area_tot_mm", dta_bal2, .1)

res_prod_mm[2,1,h] <- summary(lm(as.formula(paste("log_area_tot_mm",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
res_prod_mm[2,2,h] <- summary(lm(as.formula(paste("log_area_tot_mm",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_prod_mm[2,3,h] <- ifelse(totrep >0, RI("log_area_tot_mm",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_area_tot_mm",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

###yield

dta_bal2 <- subset(dta_bal, yield_av_mm >0)
dta_bal2$log_yield_av_mm <- log(dta_bal2$yield_av_mm)
dta_trim <- trim("log_yield_av_mm", dta_bal2, .1)

res_prod_mm[3,1,h] <- summary(lm(as.formula(paste("log_yield_av_mm",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
res_prod_mm[3,2,h] <- summary(lm(as.formula(paste("log_yield_av_mm",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_prod_mm[3,3,h] <- ifelse(totrep >0, RI("log_yield_av_mm",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_yield_av_mm",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

### was yield better compared to normal year?
summary(lm(yield_better_mm~messenger != "ctrl", data=dta_bal))

res_prod_mm[4,1,h] <- summary(lm(as.formula(paste("yield_better_mm",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
res_prod_mm[4,2,h] <- summary(lm(as.formula(paste("yield_better_mm",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_prod_mm[4,3,h] <- ifelse(totrep >0, RI("yield_better_mm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("yield_better_mm",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

dta_bal2 <- subset(dta_bal, area_tot_mm >0 & prod_tot_mm>0 & yield_av_mm >0)
dta_bal2$log_prod_tot_mm <- log(dta_bal2$prod_tot_mm)
dta_bal2$log_area_tot_mm <- log(dta_bal2$area_tot_mm)
dta_bal2$log_yield_av_mm <- log(dta_bal2$yield_av_mm)

#dta_bal2 <- trim("log_prod_tot", dta_bal2, .1)
#dta_bal2 <- trim("log_area_tot", dta_bal2, .1)
dta_bal2 <- trim("log_yield_av_mm", dta_bal2, .1)


#res_prod_mm[1:4,4,h] <- FSR_OLS( c("log_prod_tot", "log_area_tot", "log_yield_av","yield_better") ,treatment,dta_bal, nr_repl = totrep)[[4]]

#if (totrep >0) {
#dta_bal2$log_area_tot <- -dta_bal2$log_area_tot

#	indexer <- FW_index(treatment, c("log_prod_tot_mm", "log_area_tot_mm", "log_yield_av_mm","yield_better_mm"),dta_bal2, nr_repl=totrep)
#	res_prod_mm[5,1,h] <-  indexer[[1]]$coefficients[1,1]
#	res_prod_mm[5,2,h] <-  indexer[[1]]$coefficients[2,1]
#	res_prod_mm[5,3,h] <-  indexer[[2]]
#	}

##################### bm#################################
#trimming is done on end result
dta_bal2 <- subset(dta_bal, prod_tot_bm>0)
dta_bal2$log_prod_tot_bm <- log(dta_bal2$prod_tot_bm)
dta_trim <- trim("log_prod_tot_bm", dta_bal2, .1)

### production
res_prod_bm[1,1,h] <- summary(lm(as.formula(paste("log_prod_tot_bm",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
res_prod_bm[1,2,h] <- summary(lm(as.formula(paste("log_prod_tot_bm",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_prod_bm[1,3,h] <- ifelse(totrep >0, RI("log_prod_tot_bm",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_prod_tot_bm",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

### area

dta_bal2 <- subset(dta_bal, area_tot_bm>0)
dta_bal2$log_area_tot_bm <- log(dta_bal2$area_tot_bm)

dta_trim <- trim("log_area_tot_bm", dta_bal2, .1)

res_prod_bm[2,1,h] <- summary(lm(as.formula(paste("log_area_tot_bm",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
res_prod_bm[2,2,h] <- summary(lm(as.formula(paste("log_area_tot_bm",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_prod_bm[2,3,h] <- ifelse(totrep >0, RI("log_area_tot_bm",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_area_tot_bm",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

###yield

dta_bal2 <- subset(dta_bal, yield_av_bm >0)
dta_bal2$log_yield_av_bm <- log(dta_bal2$yield_av_bm)
dta_trim <- trim("log_yield_av_bm", dta_bal2, .1)

res_prod_bm[3,1,h] <- summary(lm(as.formula(paste("log_yield_av_bm",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
res_prod_bm[3,2,h] <- summary(lm(as.formula(paste("log_yield_av_bm",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_prod_bm[3,3,h] <- ifelse(totrep >0, RI("log_yield_av_bm",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_yield_av_bm",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

### was yield better compared to normal year?
summary(lm(yield_better_bm~messenger != "ctrl", data=dta_bal))

res_prod_bm[4,1,h] <- summary(lm(as.formula(paste("yield_better_bm",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
res_prod_bm[4,2,h] <- summary(lm(as.formula(paste("yield_better_bm",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_prod_bm[4,3,h] <- ifelse(totrep >0, RI("yield_better_bm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("yield_better_bm",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

dta_bal2 <- subset(dta_bal, area_tot_bm >0 & prod_tot_bm>0 & yield_av_bm >0)
dta_bal2$log_prod_tot_bm <- log(dta_bal2$prod_tot_bm)
dta_bal2$log_area_tot_bm <- log(dta_bal2$area_tot_bm)
dta_bal2$log_yield_av_bm <- log(dta_bal2$yield_av_bm)

#dta_bal2 <- trim("log_prod_tot", dta_bal2, .1)
#dta_bal2 <- trim("log_area_tot", dta_bal2, .1)
dta_bal2 <- trim("log_yield_av_bm", dta_bal2, .1)


#res_prod_bm[1:4,4,h] <- FSR_OLS( c("log_prod_tot", "log_area_tot", "log_yield_av","yield_better") ,treatment,dta_bal, nr_repl = totrep)[[4]]

#if (totrep >0) {
#dta_bal2$log_area_tot <- -dta_bal2$log_area_tot

#	indexer <- FW_index(treatment, c("log_prod_tot_bm", "log_area_tot_bm", "log_yield_av_bm","yield_better_bm"),dta_bal2, nr_repl=totrep)
#	res_prod_bm[5,1,h] <-  indexer[[1]]$coefficients[1,1]
#	res_prod_bm[5,2,h] <-  indexer[[1]]$coefficients[2,1]
#	res_prod_bm[5,3,h] <-  indexer[[2]]
#	}

#######################
################## wmshare1#################################
#trimming is done on end result
dta_bal2 <- subset(dta_bal, prod_tot_wm_share1>0)
dta_bal2$log_prod_tot_wm_share1 <- log(dta_bal2$prod_tot_wm_share1)
dta_trim <- trim("log_prod_tot_wm_share1", dta_bal2, .1)

### production
res_prod_wm_share1[1,1,h] <- summary(lm(as.formula(paste("log_prod_tot_wm_share1",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
res_prod_wm_share1[1,2,h] <- summary(lm(as.formula(paste("log_prod_tot_wm_share1",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_prod_wm_share1[1,3,h] <- ifelse(totrep >0, RI("log_prod_tot_wm_share1",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_prod_tot_wm_share1",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

### area

dta_bal2 <- subset(dta_bal, area_tot_wm_share1>0)
dta_bal2$log_area_tot_wm_share1 <- log(dta_bal2$area_tot_wm_share1)

dta_trim <- trim("log_area_tot_wm_share1", dta_bal2, .1)

res_prod_wm_share1[2,1,h] <- summary(lm(as.formula(paste("log_area_tot_wm_share1",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
res_prod_wm_share1[2,2,h] <- summary(lm(as.formula(paste("log_area_tot_wm_share1",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_prod_wm_share1[2,3,h] <- ifelse(totrep >0, RI("log_area_tot_wm_share1",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_area_tot_wm_share1",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

###yield

dta_bal2 <- subset(dta_bal, yield_av_wm_share1 >0)
dta_bal2$log_yield_av_wm_share1 <- log(dta_bal2$yield_av_wm_share1)
dta_trim <- trim("log_yield_av_wm_share1", dta_bal2, .1)

res_prod_wm_share1[3,1,h] <- summary(lm(as.formula(paste("log_yield_av_wm_share1",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
res_prod_wm_share1[3,2,h] <- summary(lm(as.formula(paste("log_yield_av_wm_share1",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_prod_wm_share1[3,3,h] <- ifelse(totrep >0, RI("log_yield_av_wm_share1",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_yield_av_wm_share1",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

### was yield better compared to normal year?
summary(lm(yield_better_wm_share1~messenger != "ctrl", data=dta_bal))

res_prod_wm_share1[4,1,h] <- summary(lm(as.formula(paste("yield_better_wm_share1",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
res_prod_wm_share1[4,2,h] <- summary(lm(as.formula(paste("yield_better_wm_share1",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_prod_wm_share1[4,3,h] <- ifelse(totrep >0, RI("yield_better_wm_share1",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("yield_better_wm_share1",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

dta_bal2 <- subset(dta_bal, area_tot_wm_share1 >0 & prod_tot_wm_share1>0 & yield_av_wm_share1 >0)
dta_bal2$log_prod_tot_wm_share1 <- log(dta_bal2$prod_tot_wm_share1)
dta_bal2$log_area_tot_wm_share1 <- log(dta_bal2$area_tot_wm_share1)
dta_bal2$log_yield_av_wm_share1 <- log(dta_bal2$yield_av_wm_share1)

#dta_bal2 <- trim("log_prod_tot", dta_bal2, .1)
#dta_bal2 <- trim("log_area_tot", dta_bal2, .1)
dta_bal2 <- trim("log_yield_av_wm_share1", dta_bal2, .1)


#res_prod_wm_share1[1:4,4,h] <- FSR_OLS( c("log_prod_tot", "log_area_tot", "log_yield_av","yield_better") ,treatment,dta_bal, nr_repl = totrep)[[4]]

#if (totrep >0) {
#dta_bal2$log_area_tot <- -dta_bal2$log_area_tot

#	indexer <- FW_index(treatment, c("log_prod_tot_wm_share1", "log_area_tot_wm_share1", "log_yield_av_wm_share1","yield_better_wm_share1"),dta_bal2, nr_repl=totrep)
#	res_prod_wm_share1[5,1,h] <-  indexer[[1]]$coefficients[1,1]
#	res_prod_wm_share1[5,2,h] <-  indexer[[1]]$coefficients[2,1]
#	res_prod_wm_share1[5,3,h] <-  indexer[[2]]
#	}

############### wmshare2#################################
#trimming is done on end result
dta_bal2 <- subset(dta_bal, prod_tot_wm_share2>0)
dta_bal2$log_prod_tot_wm_share2 <- log(dta_bal2$prod_tot_wm_share2)
dta_trim <- trim("log_prod_tot_wm_share2", dta_bal2, .1)

### production
res_prod_wm_share2[1,1,h] <- summary(lm(as.formula(paste("log_prod_tot_wm_share2",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
res_prod_wm_share2[1,2,h] <- summary(lm(as.formula(paste("log_prod_tot_wm_share2",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_prod_wm_share2[1,3,h] <- ifelse(totrep >0, RI("log_prod_tot_wm_share2",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_prod_tot_wm_share2",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

### area

dta_bal2 <- subset(dta_bal, area_tot_wm_share2>0)
dta_bal2$log_area_tot_wm_share2 <- log(dta_bal2$area_tot_wm_share2)

dta_trim <- trim("log_area_tot_wm_share2", dta_bal2, .1)

res_prod_wm_share2[2,1,h] <- summary(lm(as.formula(paste("log_area_tot_wm_share2",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
res_prod_wm_share2[2,2,h] <- summary(lm(as.formula(paste("log_area_tot_wm_share2",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_prod_wm_share2[2,3,h] <- ifelse(totrep >0, RI("log_area_tot_wm_share2",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_area_tot_wm_share2",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

###yield

dta_bal2 <- subset(dta_bal, yield_av_wm_share2 >0)
dta_bal2$log_yield_av_wm_share2 <- log(dta_bal2$yield_av_wm_share2)
dta_trim <- trim("log_yield_av_wm_share2", dta_bal2, .1)

res_prod_wm_share2[3,1,h] <- summary(lm(as.formula(paste("log_yield_av_wm_share2",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
res_prod_wm_share2[3,2,h] <- summary(lm(as.formula(paste("log_yield_av_wm_share2",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_prod_wm_share2[3,3,h] <- ifelse(totrep >0, RI("log_yield_av_wm_share2",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_yield_av_wm_share2",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

### was yield better compared to normal year?
summary(lm(yield_better_wm_share2~messenger != "ctrl", data=dta_bal))

res_prod_wm_share2[4,1,h] <- summary(lm(as.formula(paste("yield_better_wm_share2",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
res_prod_wm_share2[4,2,h] <- summary(lm(as.formula(paste("yield_better_wm_share2",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_prod_wm_share2[4,3,h] <- ifelse(totrep >0, RI("yield_better_wm_share2",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("yield_better_wm_share2",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

dta_bal2 <- subset(dta_bal, area_tot_wm_share2 >0 & prod_tot_wm_share2>0 & yield_av_wm_share2 >0)
dta_bal2$log_prod_tot_wm_share2 <- log(dta_bal2$prod_tot_wm_share2)
dta_bal2$log_area_tot_wm_share2 <- log(dta_bal2$area_tot_wm_share2)
dta_bal2$log_yield_av_wm_share2 <- log(dta_bal2$yield_av_wm_share2)

#dta_bal2 <- trim("log_prod_tot", dta_bal2, .1)
#dta_bal2 <- trim("log_area_tot", dta_bal2, .1)
dta_bal2 <- trim("log_yield_av_wm_share2", dta_bal2, .1)


#res_prod_wm_share2[1:4,4,h] <- FSR_OLS( c("log_prod_tot", "log_area_tot", "log_yield_av","yield_better") ,treatment,dta_bal, nr_repl = totrep)[[4]]

#if (totrep >0) {
#dta_bal2$log_area_tot <- -dta_bal2$log_area_tot

#	indexer <- FW_index(treatment, c("log_prod_tot_wm_share2", "log_area_tot_wm_share2", "log_yield_av_wm_share2","yield_better_wm_share2"),dta_bal2, nr_repl=totrep)
#	res_prod_wm_share2[5,1,h] <-  indexer[[1]]$coefficients[1,1]
#	res_prod_wm_share2[5,2,h] <-  indexer[[1]]$coefficients[2,1]
#	res_prod_wm_share2[5,3,h] <-  indexer[[2]]
#	}




















######################### disposal ##########################
### maize consumed

res_h0_disp[1,1,h] <- summary(lm(as.formula(paste("cons_maize_yes",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
res_h0_disp[1,2,h] <- summary(lm(as.formula(paste("cons_maize_yes",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_h0_disp[1,3,h] <- ifelse(totrep >0, RI("cons_maize_yes",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("cons_maize_yes",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

### sold maize?

res_h0_disp[2,1,h] <- summary(lm(as.formula(paste("sold_maize",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
res_h0_disp[2,2,h] <- summary(lm(as.formula(paste("sold_maize",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_h0_disp[2,3,h] <- ifelse(totrep >0, RI("sold_maize",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("sold_maize",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])
### kept maize for seed?

res_h0_disp[3,1,h] <- summary(lm(as.formula(paste("save_seed",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
res_h0_disp[3,2,h] <- summary(lm(as.formula(paste("save_seed",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_h0_disp[3,3,h] <- ifelse(totrep >0, RI("save_seed",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("save_seed",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

#res_h0_disp[1:3,4,h] <- FSR_OLS(c("cons_maize_yes", "sold_maize", "save_seed"),treatment,dta_bal, nr_repl = totrep)[[4]]

dta_bal$save_seed <- !dta_bal$save_seed

#	indexer <- FW_index(treatment, c("cons_maize_yes", "sold_maize", "save_seed"),dta_bal2, nr_repl=totrep)
#	res_h0_disp[4,1,h] <-  indexer[[1]]$coefficients[1,1]
#	res_h0_disp[4,2,h] <-  indexer[[1]]$coefficients[2,1]
#	res_h0_disp[4,3,h] <-  indexer[[2]]


################################ welfare #############################

## better off than average
res_h0_wel[1,1,h]  <- summary(lm(as.formula(paste("better_av",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
res_h0_wel[1,2,h]  <- summary(lm(as.formula(paste("better_av",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_h0_wel[1,3,h]  <- ifelse(totrep >0, RI("better_av",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("better_av",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])
## better off than 6mo
res_h0_wel[2,1,h]  <- summary(lm(as.formula(paste("better_6m",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
res_h0_wel[2,2,h]  <- summary(lm(as.formula(paste("better_6m",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_h0_wel[2,3,h]  <- ifelse(totrep >0, RI("better_6m",treatment , dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("better_6m",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

res_h0_wel[3,1,h]  <-  summary(lm(as.formula(paste("eatpref",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
res_h0_wel[3,2,h]  <-  summary(lm(as.formula(paste("eatpref",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_h0_wel[3,3,h]  <- ifelse(totrep >0, RI("eatpref",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("eatpref",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

res_h0_wel[4,1,h]  <- summary(lm(as.formula(paste("eatenough",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
res_h0_wel[4,2,h]  <- summary(lm(as.formula(paste("eatenough",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_h0_wel[4,3,h]  <- ifelse(totrep >0, RI("eatenough",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("eatenough",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

#consumption - logged and trimmed
dta_bal2 <- subset(dta_bal, cons>0)
dta_bal2$log_cons <- log(dta_bal2$cons)
dta_trim <- trim("log_cons", dta_bal2)

res_h0_wel[5,1,h]  <- summary(lm(as.formula(paste("log_cons",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
res_h0_wel[5,2,h]  <- summary(lm(as.formula(paste("log_cons",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_h0_wel[5,3,h]  <- ifelse(totrep >0, RI("log_cons",treatment , dta_bal2, nr_repl = totrep), summary(lm(as.formula(paste("log_cons",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])


#	res_h0_wel[1:5,4,h]   <- FSR_OLS(c("better_av", "better_6m", "eatpref","eatenough","log_cons"),treatment,dta_bal, nr_repl = totrep)[[4]]
#	indexer <- FW_index(treatment, c("better_av", "better_6m", "eatpref","eatenough","log_cons"),dta_bal2, nr_repl=totrep)
#	res_h0_wel[6,1,h] <-  indexer[[1]]$coefficients[1,1]
#	res_h0_wel[6,2,h] <-  indexer[[1]]$coefficients[2,1]
#	res_h0_wel[6,3,h] <-  indexer[[2]]
###### decisions
#dta$nr_man_plots <- rowSums(dta[c("mgt_man_pl1","mgt_man_pl2","mgt_man_pl3","mgt_man_pl4","mgt_man_pl5")], na.rm=T)
#dta$nr_woman_plots <- rowSums(dta[c("mgt_woman_pl1","mgt_woman_pl2","mgt_woman_pl3","mgt_woman_pl4","mgt_woman_pl5")], na.rm=T)
#dta$nr_woman_involved_plots <- rowSums(dta[c("mgt_woman_involved_pl1","mgt_woman_involved_pl2","mgt_woman_involved_pl3","mgt_woman_involved_pl4","mgt_woman_involved_pl5")], na.rm=T)
#dta$nr_joint_plots <- rowSums(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")], na.rm=T)


#res_decision[1,1,h]  <- summary(lm(as.formula(paste("(nr_man_plots > 0)",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
#res_decision[1,2,h]  <- summary(lm(as.formula(paste("(nr_man_plots > 0)",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
#res_decision[1,3,h]  <- ifelse(totrep >0, RI("(nr_man_plots > 0)",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("(nr_man_plots > 0)",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])
### better off than 6mo
#res_decision[2,1,h]  <- summary(lm(as.formula(paste("(nr_woman_plots > 0)",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
#res_decision[2,2,h]  <- summary(lm(as.formula(paste("(nr_woman_plots > 0)",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
#res_decision[2,3,h]  <- ifelse(totrep >0, RI("(nr_woman_plots > 0)",treatment , dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("(nr_woman_plots > 0)",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

#res_decision[3,1,h]  <-  summary(lm(as.formula(paste("(nr_woman_involved_plots>0)",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
#res_decision[3,2,h]  <-  summary(lm(as.formula(paste("(nr_woman_involved_plots>0)",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
#res_decision[3,3,h]  <- ifelse(totrep >0, RI("(nr_woman_involved_plots>0)",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("(nr_woman_involved_plots>0)",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

#res_decision[4,1,h]  <- summary(lm(as.formula(paste("(nr_joint_plots >0)",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
#res_decision[4,2,h]  <- summary(lm(as.formula(paste("(nr_joint_plots >0)",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
#res_decision[4,3,h]  <- ifelse(totrep >0, RI("(nr_joint_plots >0)",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("(nr_joint_plots >0)",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

#res_decision[5,1,h]  <- summary(lm(as.formula(paste("both_tell",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
#res_decision[5,2,h]  <- summary(lm(as.formula(paste("both_tell",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
#res_decision[5,3,h]  <- ifelse(totrep >0, RI("both_tell",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("both_tell",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

#res_decision[6,1,h]  <- summary(lm(as.formula(paste("spouses_listen",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
#res_decision[6,2,h]  <- summary(lm(as.formula(paste("spouses_listen",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
#res_decision[6,3,h]  <- ifelse(totrep >0, RI("spouses_listen",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("spouses_listen",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

}




