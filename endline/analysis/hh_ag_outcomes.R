source("/home/bjvca/data/projects/digital green/endline/data/init.R")

totrep <- 0
set.seed(07032018)
dta <- subset(dta, !is.na(interview_status))

dta$messenger <- as.character(dta$messenger)

### indexing results arrays
res_h0_know <- array(NA, c(5,4,4)) 
rownames(res_h0_know) <- c("know_space","know_combine","know_weed", "know_armyworm","know_ind")
res_h0_pract <- array(NA, c(6,4,4))
rownames(res_h0_pract) <- c("space","striga","weed", "fert","seed","ind_pract")
res_h0_fert  <- array(NA, c(3,4,4))
rownames(res_h0_fert) <- c("DAP","urea","organic")
res_h0_seed  <- array(NA, c(4,4,4))
rownames(res_h0_seed) <- c("bazooka","longe10h","longe5","longe4")
res_h0_prod <- array(NA, c(5,4,4))
rownames(res_h0_prod) <- c("prod","area","yield","yield_worse","prod_index")
res_h0_wel <-  array(NA, c(6,4,4))
rownames(res_h0_wel) <- c("better_av","better_6m","eatpref","eatenough","log_cons","welfare_index")

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

treatment <- "messenger != 'ctrl'" 
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

treatment <- "recipient == 'couple'" 
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
treatment <- "messenger == 'couple'"
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

treatment <- "messenger == recipient"
}

############################### knowledge  ############################

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
res_h0_pract[5,1,h]  <- summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[1,1]
res_h0_pract[5,2,h]  <- summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,1]
res_h0_pract[5,3,h]  <- ifelse(totrep >0, RI("impseed",treatment , dta_bal, nr_repl = totrep),  summary(lm(as.formula(paste("impseed", treatment, sep ="~")), data=dta_bal))$coefficients[2,4]) 

if (totrep >0) {
res_h0_pract[1:5,4,h] <- FSR_RI( c("space","striga","weed", "fert","impseed") ,treatment ,dta_bal, pvals =  res_h0_pract[,3,h] , nr_repl_pi = 100)


indexer <-  FW_index(treatment, c("space", "striga", "weed","fert","impseed"),dta_bal, nr_repl=totrep)
res_h0_pract[6,1,h] <-  indexer[[1]]$coefficients[1,1]
res_h0_pract[6,2,h] <-  indexer[[1]]$coefficients[2,1]
res_h0_pract[6,3,h] <-  indexer[[2]]
}

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
if (totrep >0) {
res_h0_seed[1:4,4,h]  <- FSR_RI( c("bazooka","longe10h","longe5","longe4") ,treatment ,dta_bal, pvals = res_h0_seed[,3,h] , nr_repl_ri = 100)
}

############################### production ###########################
### does the video increases production related outcomes?

#trimming is done on end result
dta_bal2 <- subset(dta_bal, prod_tot>0)
dta_bal2$log_prod_tot <- log(dta_bal2$prod_tot)
dta_trim <- trim("log_prod_tot", dta_bal2, .1)

### production
res_h0_prod[1,1,h] <- summary(lm(as.formula(paste("log_prod_tot",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
res_h0_prod[1,2,h] <- summary(lm(as.formula(paste("log_prod_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_h0_prod[1,3,h] <- ifelse(totrep >0, RI("log_prod_tot",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_prod_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

### area
summary(lm(dta_bal$area_tot~dta_bal$messenger != "ctrl"))
dta_bal2 <- subset(dta_bal, area_tot>0)
dta_bal2$log_area_tot <- log(dta_bal2$area_tot)

dta_trim <- trim("log_area_tot", dta_bal2, .1)

res_h0_prod[2,1,h] <- summary(lm(as.formula(paste("log_area_tot",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
res_h0_prod[2,2,h] <- summary(lm(as.formula(paste("log_area_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_h0_prod[2,3,h] <- ifelse(totrep >0, RI("log_area_tot",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_area_tot",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

###yield
summary(lm(dta_bal$yield_av~dta_bal$messenger != "ctrl"))
dta_bal2 <- subset(dta_bal, yield_av >0)
dta_bal2$log_yield_av <- log(dta_bal2$yield_av)
dta_trim <- trim("log_yield_av", dta_bal2, .1)

res_h0_prod[3,1,h] <- summary(lm(as.formula(paste("log_yield_av",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
res_h0_prod[3,2,h] <- summary(lm(as.formula(paste("log_yield_av",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
res_h0_prod[3,3,h] <- ifelse(totrep >0, RI("log_yield_av",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_yield_av",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

### was yield worse compared to normal year?
summary(lm(yield_worse~messenger != "ctrl", data=dta_bal))

res_h0_prod[4,1,h] <- summary(lm(as.formula(paste("yield_worse",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
res_h0_prod[4,2,h] <- summary(lm(as.formula(paste("yield_worse",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
res_h0_prod[4,3,h] <- ifelse(totrep >0, RI("yield_worse",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("yield_worse",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

dta_bal2 <- subset(dta_bal, area_tot >0 & prod_tot>0 & yield_av >0)
dta_bal2$log_prod_tot <- log(dta_bal2$prod_tot)
dta_bal2$log_area_tot <- log(dta_bal2$area_tot)
dta_bal2$log_yield_av <- log(dta_bal2$yield_av)

dta_bal2 <- trim("log_prod_tot", dta_bal2, .1)
dta_bal2 <- trim("log_area_tot", dta_bal2, .1)
dta_bal2 <- trim("log_yield_av", dta_bal2, .1)

if (totrep >0) {
	res_h0_prod[1:4,4] <- FSR_RI(c("log_prod_tot","log_area_tot","log_yield_av","yield_worse"), treatment ,dta_bal2,pvals = res_h0_prod[,3,h], nr_repl_ri = 100)
	indexer <- FW_index(treatment, c("log_prod", "log_area", "log_yield","yield_worse"),dta_bal2, nr_repl=totrep)
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




