source("/home/bjvca/data/projects/digital green/endline/data/init.R")

totrep <- 1000
set.seed(07032018)
dta <- subset(dta, !is.na(interview_status))

dta$messenger <- as.character(dta$messenger)

### indexing results arrays
### indexing results arrays
res_h0_know <- array(NA, c(5,4,4)) 
rownames(res_h0_know) <- c("know_space","know_combine","know_weed", "know_armyworm","know_ind")
res_h0_pract <- array(NA, c(10,4,4))
rownames(res_h0_pract) <- c("space","striga","weed", "use_fert","log_kg_fert","log_kg_fert_acre","seed","log_kg_seed","log_kg_seed_acre","combiner")
res_h0_fert  <- array(NA, c(9,4,4))
rownames(res_h0_fert) <- c("use_DAP","use_urea","use_organic","log_kg_DAP","log_kg_urea","log_kg_organic","log_kg_DAP_ac","log_kg_urea_ac","log_kg_organic_ac")
res_h0_seed  <- array(NA, c(10,4,4))
rownames(res_h0_seed) <- c("bazooka","longe10h","longe5","longe4","hybrid","kg_hybrid","kg_hybrid_ac","opv","kg_opv","kg_opv_ac")
res_h0_prod <- array(NA, c(5,4,4))
rownames(res_h0_prod) <- c("prod","area","yield","yield_better","prod_index")
res_h0_wel <-  array(NA, c(6,4,4))
rownames(res_h0_wel) <- c("better_av","better_6m","eatpref","eatenough","log_cons","welfare_index")

s_h1 <- min(table(dta$messenger[dta$messenger != "ctrl"], dta$recipient[dta$messenger != "ctrl"]))
dta <- rbind(dta[dta$messenger=="ctrl",],
 dta[dta$messenger=="couple" & dta$recipient == "couple",][sample( nrow(dta[dta$messenger=="couple" & dta$recipient == "couple",]),s_h1),],
 dta[dta$messenger=="male" & dta$recipient == "couple",][sample( nrow(dta[dta$messenger=="male" & dta$recipient == "couple",]),s_h1),],
 dta[dta$messenger=="female" & dta$recipient == "couple",][sample( nrow(dta[dta$messenger=="female" & dta$recipient == "couple",]),s_h1),],
 dta[dta$messenger=="female" & dta$recipient == "male",][sample( nrow(dta[dta$messenger=="female" & dta$recipient == "male",]),s_h1),],
 dta[dta$messenger=="female" & dta$recipient == "female",][sample( nrow(dta[dta$messenger=="female" & dta$recipient == "female",]),s_h1),],
 dta[dta$messenger=="male" & dta$recipient == "male",][sample( nrow(dta[dta$messenger=="male" & dta$recipient == "male",]),s_h1),],
 dta[dta$messenger=="male" & dta$recipient == "female",][sample( nrow(dta[dta$messenger=="male" & dta$recipient == "female",]),s_h1),],
 dta[dta$messenger=="couple" & dta$recipient == "male",][sample( nrow(dta[dta$messenger=="couple" & dta$recipient == "male",]),s_h1),],
 dta[dta$messenger=="couple" & dta$recipient == "female",][sample( nrow(dta[dta$messenger=="couple" & dta$recipient == "female",]),s_h1),])

dta_save <- dta
totrep <- 0

# run this analysis for 3 hypotheses:

for (h in 1:3) {
if (h==1) {
############################################ does the intervention work? (treat vs control) #########################################################

treatment <- "messenger != 'ctrl'" 
} else if (h==2) {
############################################# is there an additional ivr effect ##############################################
## drop the control
dta <- subset(dta, messenger != "ctrl")

treatment <- "ivr == 'yes'" 
} else if (h==3) {
################################################## is there and additional sms effect ###################################################
dta <- subset(dta, ivr == "yes")
treatment <- "sms == 'yes'" 
} 
############################## knowledge  ############################
dta_bal <- dta
#set to zero if you do not want RI based inference - then parametric p-values will be recorded in matrices

### knowledge at aggreagate level - defined as follows: if one person was interviewed and the person got it right, then known. If both were interviewed
### both need to know
dta_bal$know_space <- FALSE
dta_bal$know_space[dta_bal$a1==1 & dta_bal$interview_status == "one individual interviewed"] <- TRUE 
dta_bal$know_space[dta_bal$a1==1  & dta_bal$spouse2f1==1 & dta_bal$interview_status == "couple interviewed"] <- TRUE 

res_h0_know[1,1,h] <- summary(lm(as.formula(paste("know_space",treatment, sep="~")) ,data=dta_bal))$coefficients[1,1]
res_h0_know[1,2,h] <- summary(lm(as.formula(paste("know_space",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_h0_know[1,3,h] <- ifelse(totrep >0, RI("know_space",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("know_space",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])


dta_bal$know_combine <- FALSE
dta_bal$know_combine[dta_bal$a2==3 & dta_bal$interview_status == "one individual interviewed"] <- TRUE 
dta_bal$know_combine[dta_bal$a2==3  & dta_bal$spouse2f2==3 & dta_bal$interview_status == "couple interviewed"] <- TRUE

res_h0_know[2,1,h] <- summary(lm(as.formula(paste("know_combine",treatment, sep="~")) ,data=dta_bal))$coefficients[1,1]
res_h0_know[2,2,h] <- summary(lm(as.formula(paste("know_combine",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_h0_know[2,3,h] <-  ifelse(totrep >0, RI("know_combine",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("know_combine",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

dta_bal$know_weed <- FALSE
dta_bal$know_weed[dta_bal$a3==2 & dta_bal$interview_status == "one individual interviewed"] <- TRUE 
dta_bal$know_weed[dta_bal$a3==2  & dta_bal$spouse2f3==2 & dta_bal$interview_status == "couple interviewed"] <- TRUE

res_h0_know[3,1,h] <- summary(lm(as.formula(paste("know_weed",treatment, sep="~")) ,data=dta_bal))$coefficients[1,1]
res_h0_know[3,2,h] <- summary(lm(as.formula(paste("know_weed",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_h0_know[3,3,h] <-  ifelse(totrep >0, RI("know_weed",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("know_weed",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

dta_bal$know_armyworm <- FALSE
dta_bal$know_armyworm[dta_bal$a4==3 & dta_bal$interview_status == "one individual interviewed"] <- TRUE 
dta_bal$know_armyworm[dta_bal$a4==3  & dta_bal$spouse2f4==3 & dta_bal$interview_status == "couple interviewed"] <- TRUE

res_h0_know[4,1,h] <- summary(lm(as.formula(paste("know_armyworm",treatment, sep="~")) ,data=dta_bal))$coefficients[1,1]
res_h0_know[4,2,h] <- summary(lm(as.formula(paste("know_armyworm",treatment, sep="~")) ,data=dta_bal))$coefficients[2,1]
res_h0_know[4,3,h] <-  ifelse(totrep >0, RI("know_weed",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("know_armyworm",treatment, sep="~")) ,data=dta_bal))$coefficients[2,4])

if (totrep >0) {
	res_h0_know[1:4,4,h] <- FSR_RI( c("know_space","know_combine","know_weed", "know_armyworm") ,treatment ,dta_bal, pvals = res_h0_know[,3], nr_repl_ri = 1000)

	indexer <- FW_index(treatment, c("know_space", "know_combine", "know_weed","know_armyworm"),dta_bal, nr_repl=totrep)
	res_h0_know[5,1,h] <-  indexer[[1]]$coefficients[1,1]
	res_h0_know[5,2,h] <-  indexer[[1]]$coefficients[2,1]
	res_h0_know[5,3,h] <-  indexer[[2]]
	}

############################### practices #############################

## used recommended spacing use on at lease one plot as reported by at least one spouse

res_h0_pract[1,1,h]  <- summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_h0_pract[1,2,h]  <- summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_h0_pract[1,3,h]  <- ifelse(totrep >0, RI("space",treatment, dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("space", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## used recommended way to fight striga - this should be changed to include info of all plots 
res_h0_pract[2,1,h]  <- summary(lm(as.formula(paste("striga", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_h0_pract[2,2,h]  <- summary(lm(as.formula(paste("striga", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_h0_pract[2,3,h]  <- ifelse(totrep >0, RI("striga",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("striga", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## weeded on recommended timing? - this should be changed to include info of all plots 
res_h0_pract[3,1,h]  <- summary(lm(as.formula(paste("weed", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_h0_pract[3,2,h]  <- summary(lm(as.formula(paste("weed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_h0_pract[3,3,h]  <- ifelse(totrep >0, RI("weed",treatment, dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("weed", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

## fertilizer use
res_h0_pract[4,1,h]  <- summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_h0_pract[4,2,h]  <- summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_h0_pract[4,3,h]  <- ifelse(totrep >0, RI("fert",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("fert", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

##improved seed  
res_h0_pract[7,1,h]  <- summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_h0_pract[7,2,h]  <- summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_h0_pract[7,3,h]  <- ifelse(totrep >0, RI("impseed",treatment , dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 



### zoom in on fertilizers

#### fert = DAP/NPK
res_h0_fert[1,1,h]  <- summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_h0_fert[1,2,h]  <- summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_h0_fert[1,3,h]  <- ifelse(totrep >0, RI("fert_dap",treatment , dta_bal, nr_repl = totrep) , summary(lm(as.formula(paste("fert_dap", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

#### fert = urea
res_h0_fert[2,1,h]  <- summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_h0_fert[2,2,h]  <- summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_h0_fert[2,3,h]  <- ifelse(totrep >0, RI("fert_urea",treatment , dta_bal, nr_repl = totrep) , summary(lm(as.formula(paste("fert_urea", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])

#### fert = organic
res_h0_fert[3,1,h]  <-  summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_h0_fert[3,2,h]  <- summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_h0_fert[3,3,h]  <- ifelse(totrep >0, RI("fert_org",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("fert_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
if (totrep >0) {
res_h0_fert[1:3,4,h]  <- FSR_RI(c("fert_dap","fert_urea","fert_org"),treatment,dta_bal,pvals = res_h0_fert[,3,h], nr_repl_ri = 100)
}

#### quantities used
#### fert = DAP/NPK
res_h0_fert[4,1,h]  <- summary(lm(as.formula(paste("log(kg_dap)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_dap>0,]))$coefficients[1,1]
res_h0_fert[4,2,h]  <- summary(lm(as.formula(paste("log(kg_dap)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_dap>0,]))$coefficients[2,1]
res_h0_fert[4,3,h]  <- ifelse(totrep >0, RI("log(kg_dap)",treatment , dta_bal[dta_bal$kg_dap>0,], nr_repl = totrep) , summary(lm(as.formula(paste("log(kg_dap)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_dap>0,]))$coefficients[2,4])

res_h0_fert[7,1,h]  <- summary(lm(as.formula(paste("log(kg_dap/area_tot)", treatment, sep ="~")), data=dta_bal[(dta_bal$kg_dap/dta_bal$area_tot)>0,]))$coefficients[1,1]
res_h0_fert[7,2,h]  <- summary(lm(as.formula(paste("log(kg_dap/area_tot)", treatment, sep ="~")), data=dta_bal[(dta_bal$kg_dap/dta_bal$area_tot)>0,]))$coefficients[2,1]
res_h0_fert[7,3,h]  <- ifelse(totrep >0, RI("log(kg_dap/area_tot)",treatment , dta_bal[(dta_bal$kg_dap/dta_bal$area_tot)>0,], nr_repl = totrep) , summary(lm(as.formula(paste("log(kg_dap/area_tot)", treatment, sep ="~")), data=dta_bal[(dta_bal$kg_dap/dta_bal$area_tot)>0,]))$coefficients[2,4])

#### fert = urea
res_h0_fert[5,1,h]  <- summary(lm(as.formula(paste("log(kg_urea)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_urea>0,]))$coefficients[1,1]
res_h0_fert[5,2,h]  <- summary(lm(as.formula(paste("log(kg_urea)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_urea>0,]))$coefficients[2,1]
res_h0_fert[5,3,h]  <- ifelse(totrep >0, RI("log(kg_urea)",treatment , dta_bal[dta_bal$kg_urea>0,], nr_repl = totrep) , summary(lm(as.formula(paste("log(kg_urea)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_urea>0,]))$coefficients[2,4])

res_h0_fert[8,1,h]  <- summary(lm(as.formula(paste("log(kg_urea/area_tot)", treatment, sep ="~")), data=dta_bal[(dta_bal$kg_urea/dta_bal$area_tot)>0,]))$coefficients[1,1]
res_h0_fert[8,2,h]  <- summary(lm(as.formula(paste("log(kg_urea/area_tot)", treatment, sep ="~")), data=dta_bal[(dta_bal$kg_urea/dta_bal$area_tot)>0,]))$coefficients[2,1]
res_h0_fert[8,3,h]  <- ifelse(totrep >0, RI("log(kg_urea/area_tot)",treatment , dta_bal[(dta_bal$kg_urea/dta_bal$area_tot)>0,], nr_repl = totrep) , summary(lm(as.formula(paste("log(kg_urea/area_tot)", treatment, sep ="~")), data=dta_bal[(dta_bal$kg_urea/dta_bal$area_tot)>0,]))$coefficients[2,4])

#### fert = organic
res_h0_fert[6,1,h]  <-  summary(lm(as.formula(paste("bags_org", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_h0_fert[6,2,h]  <- summary(lm(as.formula(paste("bags_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_h0_fert[6,3,h]  <- ifelse(totrep >0, RI("bags_org",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("bags_org", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

res_h0_fert[9,1,h]  <-  summary(lm(as.formula(paste("bags_org/area_tot", treatment, sep ="~")), data=dta_bal[(dta_bal$bags_org/dta_bal$area_tot)>0,]))$coefficients[1,1]
res_h0_fert[9,2,h]  <- summary(lm(as.formula(paste("bags_org/area_tot", treatment, sep ="~")), data=dta_bal[(dta_bal$bags_org/dta_bal$area_tot)>0,]))$coefficients[2,1]
res_h0_fert[9,3,h]  <- ifelse(totrep >0, RI("bags_org/area_tot",treatment , dta_bal[(dta_bal$bags_org/dta_bal$area_tot)>0,], nr_repl = totrep), summary(lm(as.formula(paste("bags_org/area_tot", treatment, sep ="~")), data=dta_bal[(dta_bal$bags_org/dta_bal$area_tot)>0,]))$coefficients[2,4]) 

## total inorganic fertilizer (kg)
res_h0_pract[5,1,h]  <- summary(lm(as.formula(paste("log(kg_inorg)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_inorg>0,]))$coefficients[1,1]
res_h0_pract[5,2,h]  <- summary(lm(as.formula(paste("log(kg_inorg)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_inorg>0,]))$coefficients[2,1]
res_h0_pract[5,3,h]  <- ifelse(totrep >0, RI("log(kg_inorg)",treatment , dta_bal[dta_bal$kg_inorg>0,], nr_repl = totrep), summary(lm(as.formula(paste("kg_inorg", treatment, sep ="~")), data=dta_bal[dta_bal$kg_inorg>0,]))$coefficients[2,4]) 

## inorganic fertilizer application rate
res_h0_pract[6,1,h]  <- summary(lm(as.formula(paste("log(kg_ac_inorg)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_ac_inorg>0,]))$coefficients[1,1]
res_h0_pract[6,2,h]  <- summary(lm(as.formula(paste("log(kg_ac_inorg)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_ac_inorg>0,]))$coefficients[2,1]
res_h0_pract[6,3,h]  <- ifelse(totrep >0, RI("log(kg_ac_inorg)",treatment , dta_bal[dta_bal$kg_ac_inorg>0,], nr_repl = totrep), summary(lm(as.formula(paste("log(kg_ac_inorg)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_ac_inorg>0,]))$coefficients[2,4]) 


### zoom in on improved seed

## bazooka
res_h0_seed[1,1,h] <-  summary(lm(as.formula(paste("bazooka", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_h0_seed[1,2,h] <- summary(lm(as.formula(paste("bazooka", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_h0_seed[1,3,h] <- ifelse(totrep >0, RI("bazooka",treatment , dta_bal, nr_repl = totrep) ,summary(lm(as.formula(paste("bazooka", treatment, sep ="~")), data=dta_bal))$coefficients[2,4])
##longe10h
res_h0_seed[2,1,h] <- summary(lm(as.formula(paste("longe10h", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_h0_seed[2,2,h] <- summary(lm(as.formula(paste("longe10h", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_h0_seed[2,3,h] <- ifelse(totrep >0, RI("longe10h",treatment , dta_bal, nr_repl = totrep),summary(lm(as.formula(paste("longe10h", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
## longe5
res_h0_seed[3,1,h] <- summary(lm(as.formula(paste("longe5", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_h0_seed[3,2,h] <- summary(lm(as.formula(paste("longe5", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_h0_seed[3,3,h] <- ifelse(totrep >0, RI("longe5",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("longe5", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
## longe4
res_h0_seed[4,1,h] <- summary(lm(as.formula(paste("longe4", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_h0_seed[4,2,h] <- summary(lm(as.formula(paste("longe4", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_h0_seed[4,3,h] <- ifelse(totrep >0, RI("longe4",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("longe4", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
## hybrid
res_h0_seed[5,1,h] <- summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_h0_seed[5,2,h] <- summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_h0_seed[5,3,h] <- ifelse(totrep >0, RI("hybrid",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("hybrid", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 
## inorganic hybrid kg
res_h0_seed[6,1,h]  <- summary(lm(as.formula(paste("log(kg_hybrid)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_hybrid>0,]))$coefficients[1,1]
res_h0_seed[6,2,h]  <- summary(lm(as.formula(paste("log(kg_hybrid)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_hybrid>0,]))$coefficients[2,1]
res_h0_seed[6,3,h]  <- ifelse(totrep >0, RI("log(kg_hybrid)",treatment , dta_bal[dta_bal$kg_hybrid>0,], nr_repl = totrep), summary(lm(as.formula(paste("kg_hybrid", treatment, sep ="~")), data=dta_bal[dta_bal$kg_hybrid>0,]))$coefficients[2,4]) 

## inorganic hybrid application rate
res_h0_seed[7,1,h]  <- summary(lm(as.formula(paste("log(kg_hybrid/area_tot)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_hybrid/dta_bal$area_tot>0,]))$coefficients[1,1]
res_h0_seed[7,2,h]  <- summary(lm(as.formula(paste("log(kg_hybrid/area_tot)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_hybrid/dta_bal$area_tot>0,]))$coefficients[2,1]
res_h0_seed[7,3,h]  <- ifelse(totrep >0, RI("log(kg_hybrid/area_tot)",treatment , dta_bal[dta_bal$kg_hybrid/dta_bal$area_tot>0,], nr_repl = totrep), summary(lm(as.formula(paste("log(kg_hybrid/area_tot)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_hybrid/dta_bal$area_tot>0,]))$coefficients[2,4])


## opv
res_h0_seed[8,1,h] <- summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_h0_seed[8,2,h] <- summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_h0_seed[8,3,h] <- ifelse(totrep >0, RI("opv",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("opv", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

## inorganic opv kg
res_h0_seed[9,1,h]  <- summary(lm(as.formula(paste("log(kg_opv)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_opv>0,]))$coefficients[1,1]
res_h0_seed[9,2,h]  <- summary(lm(as.formula(paste("log(kg_opv)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_opv>0,]))$coefficients[2,1]
res_h0_seed[9,3,h]  <- ifelse(totrep >0, RI("log(kg_opv)",treatment , dta_bal[dta_bal$kg_opv>0,], nr_repl = totrep), summary(lm(as.formula(paste("kg_opv", treatment, sep ="~")), data=dta_bal[dta_bal$kg_opv>0,]))$coefficients[2,4]) 

## inorganic hybrid application rate
res_h0_seed[10,1,h]  <- summary(lm(as.formula(paste("log(kg_opv/area_tot)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_opv/dta_bal$area_tot>0,]))$coefficients[1,1]
res_h0_seed[10,2,h]  <- summary(lm(as.formula(paste("log(kg_opv/area_tot)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_opv/dta_bal$area_tot>0,]))$coefficients[2,1]
res_h0_seed[10,3,h]  <- ifelse(totrep >0, RI("log(kg_opv/area_tot)",treatment , dta_bal[dta_bal$kg_opv/dta_bal$area_tot>0,], nr_repl = totrep), summary(lm(as.formula(paste("log(kg_opv/area_tot)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_opv/dta_bal$area_tot>0,]))$coefficients[2,4])

 
if (totrep >0) {
res_h0_seed[1:4,4,h]  <- FSR_RI( c("bazooka","longe10h","longe5","longe4") ,treatment ,dta_bal, pvals = res_h0_seed[,3,h] , nr_repl_ri = 100)
}

## total impseed  (kg)
res_h0_pract[8,1,h]  <- summary(lm(as.formula(paste("log(kg_impseed)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_impseed>0,]))$coefficients[1,1]
res_h0_pract[8,2,h]  <- summary(lm(as.formula(paste("log(kg_impseed)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_impseed>0,]))$coefficients[2,1]
res_h0_pract[8,3,h]  <- ifelse(totrep >0, RI("log(kg_impseed)",treatment , dta_bal[dta_bal$kg_impseed>0,], nr_repl = totrep), summary(lm(as.formula(paste("kg_impseed", treatment, sep ="~")), data=dta_bal[dta_bal$kg_impseed>0,]))$coefficients[2,4]) 

## inorganic impseed application rate
res_h0_pract[9,1,h]  <- summary(lm(as.formula(paste("log(kg_impseed/area_tot)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_impseed/dta_bal$area_tot>0,]))$coefficients[1,1]
res_h0_pract[9,2,h]  <- summary(lm(as.formula(paste("log(kg_impseed/area_tot)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_impseed/dta_bal$area_tot>0,]))$coefficients[2,1]
res_h0_pract[9,3,h]  <- ifelse(totrep >0, RI("log(kg_impseed/area_tot)",treatment , dta_bal[dta_bal$kg_impseed/dta_bal$area_tot>0,], nr_repl = totrep), summary(lm(as.formula(paste("log(kg_impseed/area_tot)", treatment, sep ="~")), data=dta_bal[dta_bal$kg_impseed/dta_bal$area_tot>0,]))$coefficients[2,4]) 

## inorganic impseed application rate
res_h0_pract[10,1,h]  <- summary(lm(as.formula(paste("combiner", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_h0_pract[10,2,h]  <- summary(lm(as.formula(paste("combiner", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_h0_pract[10,3,h]  <- ifelse(totrep >0, RI("combiner",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("combiner", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

if (totrep >0) {
res_h0_pract[1:5,4,h] <- FSR_RI( c("space","striga","weed", "fert","impseed") ,treatment ,dta_bal, pvals =  res_h0_pract[,3,h] , nr_repl_pi = 100)


indexer <-  FW_index(treatment, c("space", "striga", "weed","fert","impseed"),dta_bal, nr_repl=totrep)
res_h0_pract[11,1,h] <-  indexer[[1]]$coefficients[1,1]
res_h0_pract[11,2,h] <-  indexer[[1]]$coefficients[2,1]
res_h0_pract[11,3,h] <-  indexer[[2]]
}

############################### production ###########################
### does the video increases production related outcomes?

#trimming is done on end result
dta_bal2 <- subset(dta_bal, prod_tot_man>0)
dta_bal2$log_prod_tot <- log(dta_bal2$prod_tot_man)
dta_trim <- trim("log_prod_tot", dta_bal2, .1)

### production
res_h0_prod[1,1,h] <- summary(lm(as.formula(paste("log_prod_tot",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
res_h0_prod[1,2,h] <- summary(lm(as.formula(paste("log_prod_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_h0_prod[1,3,h] <- ifelse(totrep >0, RI("log_prod_tot",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_prod_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

### area
summary(lm(dta_bal$area_tot_man~dta_bal$messenger != "ctrl"))
dta_bal2 <- subset(dta_bal, area_tot_man>0)
dta_bal2$log_area_tot <- log(dta_bal2$area_tot_man)

dta_trim <- trim("log_area_tot", dta_bal2, .1)

res_h0_prod[2,1,h] <- summary(lm(as.formula(paste("log_area_tot",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
res_h0_prod[2,2,h] <- summary(lm(as.formula(paste("log_area_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_h0_prod[2,3,h] <- ifelse(totrep >0, RI("log_area_tot",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_area_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

###yield
summary(lm(dta_bal$yield_av_man~dta_bal$messenger != "ctrl"))
dta_bal2 <- subset(dta_bal, yield_av_man >0)
dta_bal2$log_yield_av <- log(dta_bal2$yield_av_man)
dta_trim <- trim("log_yield_av", dta_bal2, .1)

res_h0_prod[3,1,h] <- summary(lm(as.formula(paste("log_yield_av",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
res_h0_prod[3,2,h] <- summary(lm(as.formula(paste("log_yield_av",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_h0_prod[3,3,h] <- ifelse(totrep >0, RI("log_yield_av",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_yield_av",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

### was yield better compared to normal year?
summary(lm(yield_better_man~messenger != "ctrl", data=dta_bal))

res_h0_prod[4,1,h] <- summary(lm(as.formula(paste("yield_better_man",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
res_h0_prod[4,2,h] <- summary(lm(as.formula(paste("yield_better_man",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_h0_prod[4,3,h] <- ifelse(totrep >0, RI("yield_better_man",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("yield_better_man",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

dta_bal2 <- subset(dta_bal, area_tot_man >0 & prod_tot_man>0 & yield_av_man >0)
dta_bal2$log_prod_tot <- log(dta_bal2$prod_tot_man)
dta_bal2$log_area_tot <- log(dta_bal2$area_tot_man)
dta_bal2$log_yield_av <- log(dta_bal2$yield_av_man)

dta_bal2 <- trim("log_prod_tot", dta_bal2, .1)
dta_bal2 <- trim("log_area_tot", dta_bal2, .1)
dta_bal2 <- trim("log_yield_av", dta_bal2, .1)

if (totrep >0) {
	res_h0_prod[1:4,4] <- FSR_RI(c("log_prod_tot","log_area_tot","log_yield_av","yield_better_man"), treatment ,dta_bal2,pvals = res_h0_prod[,3,h], nr_repl_ri = 100)
	indexer <- FW_index(treatment, c("log_prod", "log_area", "log_yield","yield_better_man"),dta_bal2, nr_repl=totrep)
	res_h0_prod[5,1,h] <-  indexer[[1]]$coefficients[1,1]
	res_h0_prod[5,2,h] <-  indexer[[1]]$coefficients[2,1]
	res_h0_prod[5,3,h] <-  indexer[[2]]
	}

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

if (totrep >0) {
	res_h0_wel[1:5,4,h]  <-  FSR_RI(c("better_av","better_6m","eatpref","eatenough","log_cons"),treatment,dta_trim,pvals = res_h0_wel[1:4,4,h], nr_repl_ri = 100)
	indexer <- FW_index(treatment, c("better_av", "better_6m", "eatpref","eatenough","log_cons"),dta_bal2, nr_repl=totrep)
	res_h0_wel[6,1,h] <-  indexer[[1]]$coefficients[1,1]
	res_h0_wel[6,2,h] <-  indexer[[1]]$coefficients[2,1]
	res_h0_wel[6,3,h] <-  indexer[[2]]
	}

}

####  now do this better - eg use only those that actually saw the video before they started planting - use IV to estimate IVR impact - use actual messages sent
dta <-  dta_save


### analysis for callers to ivr
dta <- subset(dta, messenger != "ctrl")
ivr_log <- read.csv("/home/bjvca/data/projects/digital green/endline/data/raw/ivr_log.csv")
callers <- data.frame(names(table(ivr_log$Phone.Number)))
names(callers) <- "tel"
tels <- read.csv("/home/bjvca/data/projects/digital green/endline/data/working/tels.csv")[c("HHID","tel")]
tels$tel <- paste("256",tels$tel, sep="")
callers <- merge(callers,tels)
dta <- merge(dta, callers,by.x="hhid", by.y="HHID" ,all.x=T)


summary(lm(prod_tot~!is.na(tel),data=dta))

  

### analysis for sms
dta <- subset(dta, ivr == "yes")

### read in sms log
sms_log <- read.csv("/home/bjvca/data/projects/digital green/endline/data/raw/sms_log.csv")[c("Subscriber.Phone", "Scheduled.Date", "Status")]
      
## only keep the ones that were delivered
sms_log <- subset(sms_log, Status == "Finished (complete)")

## get IDS of phone numbers for merging log to dataset
IDs <- read.csv("/home/bjvca/data/projects/digital green/baseline/tel.csv")[c("HHID","tel")]
IDs$tel <- paste("256",IDs$tel, sep="")
sms_log <- merge(IDs, sms_log, by.x = "tel", by.y="Subscriber.Phone")
sms_log <- reshape(sms_log,v.names = "Scheduled.Date", idvar = "HHID",timevar="Scheduled.Date", direction = "wide")

sms_log$rec_weed_third_8 <- !is.na(sms_log$"Scheduled.Date.2017-10-17") | !is.na(sms_log$"Scheduled.Date.2017-10-25")
sms_log$rec_urea_7 <- !is.na(sms_log$"Scheduled.Date.2017-09-26") 
sms_log$rec_weed_second_6 <- !is.na(sms_log$"Scheduled.Date.2017-09-19") 
sms_log$rec_striga_5 <- !is.na(sms_log$"Scheduled.Date.2017-09-14") 
sms_log$rec_weed_first_4 <- !is.na(sms_log$"Scheduled.Date.2017-09-12")  | !is.na(sms_log$"Scheduled.Date.2017-09-13") ### the sms guys made a big mistake here, about 600 households got sms 3 twice and did not get 4
sms_log$rec_seed_1 <- !is.na(sms_log$"Scheduled.Date.2017-08-29") | !is.na(sms_log$"Scheduled.Date.2017-08-31") 
sms_log$rec_spacing_2 <- !is.na(sms_log$"Scheduled.Date.2017-09-05") |  !is.na(sms_log$"Scheduled.Date.2017-09-06")
sms_log$rec_gapfill_3 <- !is.na(sms_log$"Scheduled.Date.2017-09-07") | !is.na(sms_log$"Scheduled.Date.2017-09-08") | !is.na(sms_log$"Scheduled.Date.2017-09-11")

sms_log <- sms_log[c("HHID","rec_seed_1", "rec_spacing_2" ,"rec_gapfill_3", "rec_weed_first_4", "rec_striga_5", "rec_weed_second_6", "rec_urea_7", "rec_weed_third_8")] 
sms_log$totsms <- rowSums(sms_log[2:9])
dta <- merge(dta, sms_log, by.x="hhid", by.y="HHID", all.x=T)
dta$totsms[is.na(dta$totsms)] <- 0

summary(lm(prod_tot~totsms,data=dta))
summary(lm(area_tot~totsms,data=dta))
summary(lm(yield_av~totsms,data=dta))
  




