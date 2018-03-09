### Some initial explorations when first batch of data came in
dta <- read.csv("/home/bjvca/data/projects/digital green/endline/data/endline.csv")
dta <- subset(dta, femalehead == 0)
 

# the variable video_shown does not have info so let's just merge in from the sampling list
treats <- read.csv("/home/bjvca/data/projects/digital green/midline/list_sec.csv")
dta <- merge(treats, dta, by="hhid", all.y=T)

### who recieved the video?
dta$recipient.y <- NULL
names(dta)[names(dta) == 'recipient.x'] <- 'recipient'

### make a measure for production
### calculate plot level production - intercropping percentages
dta$grp1a14[dta$grp1a12 == "No"] <- 100
dta$grp2b14[dta$grp2b12 == "No"] <- 100
dta$grp3c14[dta$grp3c12 == "No"] <- 100
dta$grp4d14[dta$grp4d12 == "No"] <- 100
dta$grp5e14[dta$grp5e12 == "No"] <- 100
dta[c("grp1a14","grp2b14","grp3c14","grp4d14", "grp5e14")] <- lapply(dta[c("grp1a14","grp2b14","grp3c14","grp4d14", "grp5e14")], function(x) replace(x, x == 999, NA) )

dta$spouse2grp_sp1f14[dta$spouse2grp_sp1f12 == "No"] <- 100
dta$spouse2grp_sp2g14[dta$spouse2grp_sp2g12 == "No"] <- 100
dta$spouse2grp_sp3h14[dta$spouse2grp_sp3h12 == "No"] <- 100
dta$spouse2group_sp4j14[dta$spouse2group_sp4j12 == "No"] <- 100
dta$spouse2grp5_sp5k14[dta$spouse2grp5_sp5k12 == "No"] <- 100

dta[c("spouse2grp_sp1f14","spouse2grp_sp2g14","spouse2grp_sp3h14","spouse2group_sp4j14", "spouse2grp5_sp5k14")] <- lapply(dta[c("spouse2grp_sp1f14","spouse2grp_sp2g14","spouse2grp_sp3h14","spouse2group_sp4j14", "spouse2grp5_sp5k14")], function(x) replace(x,x == 999,NA) )

#production - bags x kg per bag - spouse 1
dta[c("grp1a16","grp2b16","grp3c16","grp4d16", "grp5e16")] <- lapply(dta[c("grp1a16","grp2b16","grp3c16","grp4d16", "grp5e16")], function(x) replace(x, x == 999, NA) )
dta[c("grp1a17","grp2b17","grp3c17","grp4d17", "grp5e17")] <- lapply(dta[c("grp1a17","grp2b17","grp3c17","grp4d17", "grp5e17")], function(x) replace(x, x == 999, NA) )

dta$prod_pl1_sp1 <- dta$grp1a16 * dta$grp1a17
dta$prod_pl2_sp1 <- dta$grp2b16 * dta$grp2b17
dta$prod_pl3_sp1 <- dta$grp3c16 * dta$grp3c17
dta$prod_pl4_sp1 <- dta$grp4d16 * dta$grp4d17
dta$prod_pl5_sp1 <- dta$grp5e16 * dta$grp5e17

dta$prod_tot_sp1 <- rowSums(dta[c("prod_pl1_sp1","prod_pl2_sp1","prod_pl3_sp1","prod_pl4_sp1", "prod_pl5_sp1")], na.rm=T)
dta$prod_tot_sp1[dta$prod_tot_sp1 == 0] <- NA

dta[c("spouse2grp_sp1f16","spouse2grp_sp2g16","spouse2grp_sp3h16","spouse2group_sp4j16", "spouse2grp5_sp5k16")] <- lapply(dta[c("spouse2grp_sp1f16","spouse2grp_sp2g16","spouse2grp_sp3h16","spouse2group_sp4j16", "spouse2grp5_sp5k16")], function(x) replace(x, x == 999, NA) )
dta[c("spouse2grp_sp1f17","spouse2grp_sp2g17","spouse2grp_sp3h17","spouse2group_sp4j17", "spouse2grp5_sp5k17")] <- lapply(dta[c("spouse2grp_sp1f17","spouse2grp_sp2g17","spouse2grp_sp3h17","spouse2group_sp4j17", "spouse2grp5_sp5k17")], function(x) replace(x, x == 999, NA) )

dta$prod_pl1_sp2 <- dta$spouse2grp_sp1f16 * dta$spouse2grp_sp1f17
dta$prod_pl2_sp2 <- dta$spouse2grp_sp2g16 * dta$spouse2grp_sp2g17
dta$prod_pl3_sp2 <- dta$spouse2grp_sp3h16 * dta$spouse2grp_sp3h17
dta$prod_pl4_sp2 <- dta$spouse2group_sp4j16 * dta$spouse2group_sp4j17
dta$prod_pl5_sp2 <- dta$spouse2grp5_sp5k16 * dta$spouse2grp5_sp5k17

dta$prod_tot_sp2 <- rowSums(dta[c("prod_pl1_sp2","prod_pl2_sp2","prod_pl3_sp2","prod_pl4_sp2", "prod_pl5_sp2")], na.rm=T)
dta$prod_tot_sp2[dta$prod_tot_sp2 == 0] <- NA

dta$prod_tot <-  rowMeans(dta[c("prod_tot_sp1","prod_tot_sp2")], na.rm=T)

### now scale by area and take intercropping into account 


dta[c("grp1a11","grp2b11","grp3c11","grp4d11", "grp5e11")] <- lapply(dta[c("grp1a11","grp2b11","grp3c11","grp4d11", "grp5e11")], function(x) replace(x, x == 999, NA) )

dta[c("spouse2grp_sp1f11","spouse2grp_sp2g11","spouse2grp_sp3h11","spouse2group_sp4j11", "spouse2grp5_sp5k11")] <- lapply(dta[c("spouse2grp_sp1f11","spouse2grp_sp2g11","spouse2grp_sp3h11","spouse2group_sp4j11", "spouse2grp5_sp5k11")], function(x) replace(x,x == 999,NA) )

dta$area_pl1_sp1 <-(dta$grp1a11 * ((dta$grp1a14) /100))
dta$area_pl2_sp1 <-(dta$grp2b11 * ((dta$grp2b14) /100))
dta$area_pl3_sp1 <-(dta$grp3c11 * ((dta$grp3c14) /100))
dta$area_pl4_sp1 <-(dta$grp4d11 * ((dta$grp4d14) /100))
dta$area_pl5_sp1 <-(dta$grp5e11 * ((dta$grp5e14) /100))


dta$yield_pl1_sp1 <- dta$prod_pl1_sp1 / (dta$grp1a11 * ((dta$grp1a14) /100))
dta$yield_pl2_sp1 <- dta$prod_pl2_sp1 / (dta$grp2b11 * ((dta$grp2b14) /100))
dta$yield_pl3_sp1 <- dta$prod_pl3_sp1 / (dta$grp3c11 * ((dta$grp3c14) /100))
dta$yield_pl4_sp1 <- dta$prod_pl4_sp1 / (dta$grp4d11 * ((dta$grp4d14) /100))
dta$yield_pl5_sp1 <- dta$prod_pl5_sp1 / (dta$grp5e11 * ((dta$grp5e14) /100))
dta$area_tot_sp1 <- rowSums(dta[c("area_pl1_sp1","area_pl2_sp1","area_pl3_sp1","area_pl4_sp1", "area_pl5_sp1")], na.rm=T)
dta$yield_av_sp1 <- rowMeans(dta[c("yield_pl1_sp1","yield_pl2_sp1","yield_pl3_sp1","yield_pl4_sp1", "yield_pl5_sp1")], na.rm=T)

dta$area_pl1_sp2 <- (dta$spouse2grp_sp1f11 * ((dta$spouse2grp_sp1f14) /100))
dta$area_pl2_sp2 <- (dta$spouse2grp_sp2g11 * ((dta$spouse2grp_sp2g14) /100))
dta$area_pl3_sp2 <- (dta$spouse2grp_sp3h11 * ((dta$spouse2grp_sp3h14) /100))
dta$area_pl4_sp2 <- (dta$spouse2group_sp4j11 * ((dta$spouse2group_sp4j14) /100))
dta$area_pl5_sp2 <- (dta$spouse2grp5_sp5k11 * ((dta$spouse2grp5_sp5k14) /100))

dta$yield_pl1_sp2 <- dta$prod_pl1_sp2 / (dta$spouse2grp_sp1f11 * ((dta$spouse2grp_sp1f14) /100))
dta$yield_pl2_sp2 <- dta$prod_pl2_sp2 / (dta$spouse2grp_sp2g11 * ((dta$spouse2grp_sp2g14) /100))
dta$yield_pl3_sp2 <- dta$prod_pl3_sp2 / (dta$spouse2grp_sp3h11 * ((dta$spouse2grp_sp3h14) /100))
dta$yield_pl4_sp2 <- dta$prod_pl4_sp2 / (dta$spouse2group_sp4j11 * ((dta$spouse2group_sp4j14) /100))
dta$yield_pl5_sp2 <- dta$prod_pl5_sp2 / (dta$spouse2grp5_sp5k11 * ((dta$spouse2grp5_sp5k14) /100))

dta$area_tot_sp2 <- rowSums(dta[c("area_pl1_sp2","area_pl2_sp2","area_pl3_sp2","area_pl4_sp2", "area_pl5_sp2")], na.rm=T)
dta$yield_av_sp2 <- rowMeans(dta[c("yield_pl1_sp2","yield_pl2_sp2","yield_pl3_sp2","yield_pl4_sp2", "yield_pl5_sp2")], na.rm=T)

dta$area_tot <- rowMeans(dta[c("area_tot_sp1","area_tot_sp2")], na.rm=T)
dta$yield_av <- rowMeans(dta[c("yield_av_sp1","yield_av_sp2")], na.rm=T)


### average days after rain
dta[c("grp1days19","grp2days2","grp3days3","grp4days4", "grp5days5")] <- lapply(dta[c("grp1days19","grp2days2","grp3days3","grp4days4", "grp5days5")], function(x) replace(x, x == 999 | x == 98, NA) )


dta[c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5")] <- lapply(dta[c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5")], function(x) replace(x,x == 999  | x == 98,NA) )

dta$days_av_sp1 <- rowMeans(dta[c("grp1days19","grp2days2","grp3days3","grp4days4", "grp5days5")], na.rm=T)
dta$days_av_sp2 <- rowMeans(dta[c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5")], na.rm=T)
dta$days_av <- rowMeans(dta[c("days_av_sp1", "days_av_sp2")])


#### calculated consumption expenditure
dta[c("maize_value", "sorghum_value", "millet_value", "rice_value", "cassava_value", "sweetpotatoes_value", "beans_value", "gnuts_value", "fruits_value", "veg_value", "sugar_value", "cooking_oil_value", "soap_value", "airtime_value")] <- 
lapply(dta[c("maize_value", "sorghum_value", "millet_value", "rice_value", "cassava_value", "sweetpotatoes_value", "beans_value", "gnuts_value", "fruits_value", "veg_value", "sugar_value", "cooking_oil_value", "soap_value", "airtime_value")], function(x) replace(x, x == 999, NA) )

dta$cons_sp1 <- rowSums(dta[c("maize_value", "sorghum_value", "millet_value", "rice_value", "cassava_value", "sweetpotatoes_value", "beans_value", "gnuts_value", "fruits_value", "veg_value", "sugar_value", "cooking_oil_value", "soap_value", "airtime_value")], na.rm=T)

dta[c("spouse2maize_value_sp", "spouse2sorghum_value_sp", "spouse2millet_value_sp", "spouse2rice_value_sp", "spouse2cassava_value_sp", "spouse2sweetpotatoes_value_sp", "spouse2beans_value_sp", "spouse2gnuts_value_sp", "spouse2fruits_value_sp", "spouse2veg_value_sp", "spouse2sugar_value_sp", "spouse2cooking_oil_value_sp", "spouse2soap_value_sp", "spouse2airtime_value_sp")] <- 
lapply(dta[c("spouse2maize_value_sp", "spouse2sorghum_value_sp", "spouse2millet_value_sp", "spouse2rice_value_sp", "spouse2cassava_value_sp", "spouse2sweetpotatoes_value_sp", "spouse2beans_value_sp", "spouse2gnuts_value_sp", "spouse2fruits_value_sp", "spouse2veg_value_sp", "spouse2sugar_value_sp", "spouse2cooking_oil_value_sp", "spouse2soap_value_sp", "spouse2airtime_value_sp")], function(x) replace(x, x == 999, NA) )

dta$cons_sp2 <- rowSums(dta[c("spouse2maize_value_sp", "spouse2sorghum_value_sp", "spouse2millet_value_sp", "spouse2rice_value_sp", "spouse2cassava_value_sp", "spouse2sweetpotatoes_value_sp", "spouse2beans_value_sp", "spouse2gnuts_value_sp", "spouse2fruits_value_sp", "spouse2veg_value_sp", "spouse2sugar_value_sp", "spouse2cooking_oil_value_sp", "spouse2soap_value_sp", "spouse2airtime_value_sp")], na.rm=T)

dta$cons <- dta$cons_sp1 + dta$cons_sp2
dta$cons[dta$cons == 0] <- NA

### a function to calculate the RI p-values
RI <- function(dep, indep, dta , nr_repl = 1000) {
	library(dplyr)
	library(doParallel)
	cl <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
	registerDoParallel(cl)
	dta <- dta %>% 
    		mutate(treat = group_indices_(dta, .dots=c("recipient", "messenger"))) 
	dta <- dta %>% 
    		mutate(uniqID = group_indices_(dta, .dots=c("distID", "subID","vilID"))) 

	crit <- summary(lm(as.formula(paste(dep,indep,sep="~")), data=dta))$coefficients[2,1]
	dta_sim <- dta
	
	oper <- foreach (repl = 1:nr_repl,.combine=cbind) %dopar% {
 		resample <- function(x, ...) x[sample.int(length(x), ...)]
		dta_sim$perm <- unlist(sapply(names(table(dta$uniqID)), function(x) resample(dta$treat[dta$uniq==x])))
		dta_sim$messenger[dta_sim$perm == 4  |dta_sim$perm == 8  |dta_sim$perm == 12  ] <- "male"
		dta_sim$messenger[dta_sim$perm == 3  |dta_sim$perm == 7  |dta_sim$perm == 11  ] <- "female"
		dta_sim$messenger[dta_sim$perm == 1  |dta_sim$perm == 5  |dta_sim$perm == 9  ] <- "female"
		dta_sim$messenger[dta_sim$perm == 2  |dta_sim$perm == 6  |dta_sim$perm == 10  ] <- "ctrl"
		dta_sim$recipient[dta_sim$perm == 9  |dta_sim$perm == 10  |dta_sim$perm == 11 |dta_sim$perm == 12  ] <- "male"
		dta_sim$recipient[dta_sim$perm == 5  |dta_sim$perm == 6  |dta_sim$perm == 7 |dta_sim$perm == 8  ] <- "female"
		dta_sim$recipient[dta_sim$perm == 1  |dta_sim$perm == 2  |dta_sim$perm == 3 |dta_sim$perm == 4  ] <- "couple"
		return( if (crit >0) {
			summary(lm(as.formula(paste(dep,indep,sep="~")), data=dta_sim))$coefficients[2,1] > crit 
			} else {
			summary(lm(as.formula(paste(dep,indep,sep="~")), data=dta_sim))$coefficients[2,1] < crit 
			} )
	}
	print(sum(oper)/nr_repl)
	stopCluster(cl)
}



############################################ does the intervention work? (treat vs control) #########################################################
### remember to balance data over treatment cells, this involves taking random samples from each cell, so set seed
set.seed(07032018)
dta$messenger <- as.character(dta$messenger)
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



############################### production ###########################
### production
t.test(dta_bal$prod_tot~dta_bal$messenger == "ctrl")
### area
t.test(dta_bal$area_tot~dta_bal$messenger == "ctrl")
#RI("area_tot","messenger == 'ctrl'" , dta_bal)
###yield
t.test(dta_bal$yield_av~dta_bal$messenger == "ctrl")
#RI("yield_av","messenger == 'ctrl'" , dta_bal)
dta_trim <- subset(dta_bal, dta_bal$yield_av > quantile(dta_bal$yield_av,c(.05,.95), na.rm=T)[1] & dta_bal$yield_av < quantile(dta_bal$yield_av, c(.05,.95),na.rm=T)[2])
t.test(dta_trim$yield_av~dta_trim$messenger == "ctrl")
#RI("yield_av","messenger == 'ctrl'" , dta_trim)
################################ welfare #############################

### total household consumption
t.test(dta_bal$cons~dta_bal$messenger == "ctrl")
dta_trim <- subset(dta_bal, dta_bal$cons > quantile(dta_bal$cons,c(.05,.95), na.rm=T)[1] & dta_bal$cons < quantile(dta_bal$cons, c(.05,.95),na.rm=T)[2])
t.test(dta_trim$cons~dta_trim$messenger == "ctrl")

## better off than others
t.test(dta_bal$q409==1~dta_bal$messenger == "ctrl")
prop.test(t(table(dta_bal$q409==1,dta_bal$messenger == "ctrl")))

## better off than 6 months ago
t.test(dta_bal$q110==1~dta_bal$messenger == "ctrl")
prop.test(t(table(dta_bal$q110==1,dta_bal$messenger == "ctrl")))


############################### practices #############################
## planted number of days after rain
t.test(dta_bal$days_av~dta_bal$messenger == "ctrl")

## used recommended spacing
t.test(dta_bal$grp1a201==1~dta_bal$messenger == "ctrl")
#RI("grp1a201==1","messenger == 'ctrl'" , dta_bal)

## used recommended way to fight striga
t.test(dta_bal$grp1a241==1~dta_bal$messenger == "ctrl")
#RI("grp1a241==1","messenger == 'ctrl'" , dta_bal)

## weeded on recommended timing?
t.test(dta_bal$grp1a26==1~dta_bal$messenger == "ctrl")
## used fertilizer 
t.test(dta_bal$grp1a29=="Yes"~dta_bal$messenger == "ctrl")
#RI("grp1a29=='Yes' ","messenger == 'ctrl'" , dta_bal)
##improved seed
t.test(dta_bal$grp1a42=="Yes"~dta_bal$messenger == "ctrl")
## chemicals
t.test(dta_bal$grp1a55a=="Yes"~dta_bal$messenger == "ctrl")


### knowledge - 1 if both know correct anwer
t.test((dta_bal$a1==1 & dta_bal$spouse2f1==1)~dta_bal$messenger == "ctrl")
t.test((dta_bal$a2==3 & dta_bal$spouse2f2==3)~dta_bal$messenger == "ctrl")
t.test((dta_bal$a3==2 & dta_bal$spouse2f3==2)~dta_bal$messenger == "ctrl")
#this was not in the video
t.test((dta_bal$a4==3 & dta_bal$spouse2f4==3)~dta_bal$messenger == "ctrl")


############################################# reducing information asymmetries ##############################################
## drop the control
dta <- subset(dta, messenger != "ctrl")
#### make sure to balance before doing tests, but we should actually also do this for the above treatment control comparison
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


############################### production ###########################
### production
t.test(dta_bal$prod_tot~dta_bal$recipient == "couple")
### area
t.test(dta_bal$area_tot~dta_bal$recipient == "couple")
#RI("area_tot","messenger == 'ctrl'" , dta_bal)
###yield
t.test(dta_bal$yield_av~dta_bal$recipient == "couple")
#RI("yield_av","messenger == 'ctrl'" , dta_bal)
dta_trim <- subset(dta_bal, dta_bal$yield_av > quantile(dta_bal$yield_av,c(.05,.95), na.rm=T)[1] & dta_bal$yield_av < quantile(dta_bal$yield_av, c(.05,.95),na.rm=T)[2])
t.test(dta_trim$yield_av~dta_trim$recipient == "couple")
#RI("yield_av","messenger == 'ctrl'" , dta_trim)

### decided together to plant maize on first plot
t.test((dta_bal$grp1a10==3 & dta_bal$spouse2grp_sp1f10==3) ~dta_bal$recipient == "couple")

################################ welfare #############################

### total household consumption
t.test(dta_bal$cons~dta_bal$recipient == "couple")
dta_trim <- subset(dta_bal, dta_bal$cons > quantile(dta_bal$cons,c(.05,.95), na.rm=T)[1] & dta_bal$cons < quantile(dta_bal$cons, c(.05,.95),na.rm=T)[2])
t.test(dta_trim$cons~dta_trim$recipient == "couple")

## better off than others
t.test(dta_bal$q409==1~dta_bal$recipient == "couple")
prop.test(t(table(dta_bal$q409==1,dta_bal$messenger == "ctrl")))

## better off than 6 months ago
t.test(dta_bal$q110==1~dta_bal$recipient == "couple")
prop.test(t(table(dta_bal$q110==1,dta_bal$messenger == "ctrl")))



## average number of days after rain
t.test(dta_bal$days_av~dta_bal$recipient == "couple")

## used recommended spacing
t.test(dta_bal$grp1a201==1~dta_bal$recipient == "couple")
## used recommended way to fight striga
t.test(dta_bal$grp1a241==1~dta_bal$recipient == "couple")
t.test((dta_bal$grp1a25==3 & dta_bal$spouse2grp_sp1f25==3) ~dta_bal$recipient == "couple")
## weeded on recommended timing?
t.test(dta_bal$grp1a26==1~dta_bal$recipient == "couple")

## used fertilizer 
t.test(dta_bal$grp1a29=="Yes"~dta_bal$recipient == "couple")
##agree on what fertilizer to use
dta_bal$agreefert <- (dta_bal$grp1a31==3 & dta_bal$spouse2grp_sp1f31==3) | (dta_bal$grp1a30a==3 & dta_bal$spouse2grp_sp1f30a==3) | (dta_bal$grp1a37a==3 & dta_bal$spouse2grp_sp1f37a==3)
#this assumes that if they do not use fertilizer they also do not agree to use fertilizer
dta_bal$agreefert[is.na(dta_bal$agreefert)] <- FALSE
t.test(dta_bal$agreefert== TRUE~dta_bal$recipient == "couple")

#agree on what money to use to buy fertilizer
t.test((dta$grp1a40==3 & dta$spouse2grp_sp1f40==3) ~dta$recipient == "couple")

### used improved seed
t.test(dta_bal$grp1a42=="Yes"~dta_bal$recipient == "couple")

## chemicals
t.test(dta_bal$grp1a55a=="Yes"~dta_bal$recipient == "couple")

t.test((dta_bal$q100 <3 & dta_bal$spouse2r100< 3)~dta_bal$recipient == "couple")

### knowledge - 1 if both know correct anwer
t.test((dta_bal$a1==1 & dta_bal$spouse2f1==1)~dta_bal$recipient == "couple")
t.test((dta_bal$a2==3 & dta_bal$spouse2f2==3)~dta_bal$recipient == "couple")
t.test((dta_bal$a3==2 & dta_bal$spouse2f3==2)~dta_bal$recipient == "couple")
#this was not in the video
t.test((dta_bal$a4==3 & dta_bal$spouse2f4==3)~dta_bal$recipient == "couple")

################################################### Projecting cooperative approach ###################################################

#### make sure to balance before doing tests

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

### more effective if cooperative approach is projected?
### more effective if targetted to both male and female?
t.test(dta_bal$grp1a16*dta_bal$grp1a17 ~ dta_bal$messenger == "couple")
### decided together to plant maize on this plot 
t.test((dta_bal$grp1a10==3 & dta_bal$spouse2grp_sp1f10==3) ~dta_bal$messenger == "couple")
## used recommended spacing
t.test(dta_bal$grp1a201==1~dta_bal$messenger == "couple")
## used recommended way to fight striga
t.test(dta_bal$grp1a241==1~dta_bal$messenger == "couple")
t.test((dta_bal$grp1a25==3 & dta_bal$spouse2grp_sp1f25==3) ~dta_bal$messenger == "couple")
## weeded on recommended timing?
t.test(dta_bal$grp1a26==1~dta_bal$messenger == "couple")

## used fertilizer 
t.test(dta_bal$grp1a29=="Yes"~dta_bal$messenger == "couple")
##agree on what fertilizer to use
dta_bal$agreefert <- (dta_bal$grp1a31==3 & dta_bal$spouse2grp_sp1f31==3) | (dta_bal$grp1a30a==3 & dta_bal$spouse2grp_sp1f30a==3) | (dta_bal$grp1a37a==3 & dta_bal$spouse2grp_sp1f37a==3)
#this assumes that if they do not use fertilizer they also do not agree to use fertilizer
dta_bal$agreefert[is.na(dta_bal$agreefert)] <- FALSE
t.test(dta_bal$agreefert== TRUE~dta_bal$messenger == "couple")

#agree on what money to use to buy fertilizer
t.test((dta$grp1a40==3 & dta$spouse2grp_sp1f40==3) ~dta$messenger == "couple")

### used improved seed
t.test(dta_bal$grp1a42=="Yes"~dta_bal$messenger == "couple")

## chemicals
t.test(dta_bal$grp1a55a=="Yes"~dta_bal$messenger == "couple")



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


dta$matched <- NA
dta$matched[dta$recipient == "male" & dta$messenger=="male" | dta$recipient == "female" & dta$messenger=="female"] <- TRUE
dta$matched[dta$recipient == "male" & dta$messenger=="female" | dta$recipient == "female" & dta$messenger=="male"] <- FALSE

dta_bal$matched <- NA
dta_bal$matched[dta_bal$recipient == "male" & dta_bal$messenger=="male" | dta_bal$recipient == "female" & dta_bal$messenger=="female"] <- TRUE
dta_bal$matched[dta_bal$recipient == "male" & dta_bal$messenger=="female" | dta_bal$recipient == "female" & dta_bal$messenger=="male"] <- FALSE

### more effective if cooperative approach is projected?
### more effective if targetted to both male and female?
t.test(dta_bal$grp1a16*dta_bal$grp1a17 ~ dta_bal$matched)
### decided together to plant maize on this plot 
t.test((dta_bal$grp1a10==3 & dta_bal$spouse2grp_sp1f10==3) ~dta_bal$matched)
## used recommended spacing
t.test(dta_bal$grp1a201==1~dta_bal$messenger == "couple")
## used recommended way to fight striga
t.test(dta_bal$grp1a241==1~dta_bal$messenger == "couple")
t.test((dta_bal$grp1a25==3 & dta_bal$spouse2grp_sp1f25==3) ~dta_bal$matched)
## weeded on recommended timing?
t.test(dta_bal$grp1a26==1~dta_bal$matched)

## used fertilizer 
t.test(dta_bal$grp1a29=="Yes"~dta_bal$matched)
##agree on what fertilizer to use
dta_bal$agreefert <- (dta_bal$grp1a31==3 & dta_bal$spouse2grp_sp1f31==3) | (dta_bal$grp1a30a==3 & dta_bal$spouse2grp_sp1f30a==3) | (dta_bal$grp1a37a==3 & dta_bal$spouse2grp_sp1f37a==3)
#this assumes that if they do not use fertilizer they also do not agree to use fertilizer
dta_bal$agreefert[is.na(dta_bal$agreefert)] <- FALSE
t.test(dta_bal$agreefert== TRUE~dta_bal$matched)

#agree on what money to use to buy fertilizer
t.test((dta$grp1a40==3 & dta$spouse2grp_sp1f40==3) ~dta_bal$matched)

### used improved seed
t.test(dta_bal$grp1a42=="Yes"~dta_bal$matched)

## chemicals
t.test(dta_bal$grp1a55a=="Yes"~dta_bal$matched)







dta$messenge =="male" & dta$recipient == "female"


