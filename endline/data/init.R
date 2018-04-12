###This file does most of the preparations - it also loads functions that are used in subsequent analyses
### first run anonymize.R on raw data to remove identifiers
rm(list=ls())
dta <- read.csv("/home/bjvca/data/projects/digital green/endline/data/endline.csv")
###drop all female headed households 
dta <- subset(dta, femalehead == 0)
 

# the variable video_shown does not have info so let's just merge in from the sampling list
treats <- read.csv("/home/bjvca/data/projects/digital green/midline/list_sec.csv")
dta <- merge(treats, dta, by="hhid", all=T)


#write.csv(dta, "/home/bjvca/data/projects/digital green/endline/data/working/AWS.csv")
## Amazon Elastic Cloud: uncomment next line and run from here
#dta <- read.csv("AWS.csv")
	library(dplyr)
	library(doParallel)
	cl <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
	registerDoParallel(cl)

### who recieved the video?
dta$recipient.y <- NULL
names(dta)[names(dta) == 'recipient.x'] <- 'recipient'
dta$messenger.y <- NULL
names(dta)[names(dta) == 'messenger.x'] <- 'messenger'

### make a measure for production
### calculate plot level production - intercropping percentages
### if intercropped you get a percentage
### if pure stand we set intercropped percentage to 100
### for spouse 1
dta$grp1a14[dta$grp1a12 == "No"] <- 100
dta$grp2b14[dta$grp2b12 == "No"] <- 100
dta$grp3c14[dta$grp3c12 == "No"] <- 100
dta$grp4d14[dta$grp4d12 == "No"] <- 100
dta$grp5e14[dta$grp5e12 == "No"] <- 100
dta[c("grp1a14","grp2b14","grp3c14","grp4d14", "grp5e14")] <- lapply(dta[c("grp1a14","grp2b14","grp3c14","grp4d14", "grp5e14")], function(x) replace(x, x == 999, NA) )
### for spouse 2
dta$spouse2grp_sp1f14[dta$spouse2grp_sp1f12 == "No"] <- 100
dta$spouse2grp_sp2g14[dta$spouse2grp_sp2g12 == "No"] <- 100
dta$spouse2grp_sp3h14[dta$spouse2grp_sp3h12 == "No"] <- 100
dta$spouse2group_sp4j14[dta$spouse2group_sp4j12 == "No"] <- 100
dta$spouse2grp5_sp5k14[dta$spouse2grp5_sp5k12 == "No"] <- 100

dta[c("spouse2grp_sp1f14","spouse2grp_sp2g14","spouse2grp_sp3h14","spouse2group_sp4j14", "spouse2grp5_sp5k14")] <- lapply(dta[c("spouse2grp_sp1f14","spouse2grp_sp2g14","spouse2grp_sp3h14","spouse2group_sp4j14", "spouse2grp5_sp5k14")], function(x) replace(x,x == 999,NA) )

#production - bags x kg per bag - spouse 1
dta[c("grp1a16","grp2b16","grp3c16","grp4d16", "grp5e16")] <- lapply(dta[c("grp1a16","grp2b16","grp3c16","grp4d16", "grp5e16")], function(x) replace(x, x == 999, NA) )
dta[c("grp1a17","grp2b17","grp3c17","grp4d17", "grp5e17")] <- lapply(dta[c("grp1a17","grp2b17","grp3c17","grp4d17", "grp5e17")], function(x) replace(x, x == 999, NA) )

## note: if number of bags was zero, bagsize is missing (set here to zero)
dta$grp1a17[dta$grp1a16 == 0] <- 0
dta$grp2b17[dta$grp2b16 == 0] <- 0
dta$grp3c17[dta$grp3c16 == 0] <- 0
dta$grp4d17[dta$grp4d16 == 0] <- 0
dta$grp5e17[dta$grp5e16 == 0] <- 0

dta$prod_pl1_sp1 <- dta$grp1a16 * dta$grp1a17
dta$prod_pl2_sp1 <- dta$grp2b16 * dta$grp2b17
dta$prod_pl3_sp1 <- dta$grp3c16 * dta$grp3c17
dta$prod_pl4_sp1 <- dta$grp4d16 * dta$grp4d17
dta$prod_pl5_sp1 <- dta$grp5e16 * dta$grp5e17
### how do we handle missings? The below will also produce missings if nothing has been produced
dta$prod_tot_sp1 <- rowSums(dta[c("prod_pl1_sp1","prod_pl2_sp1","prod_pl3_sp1","prod_pl4_sp1", "prod_pl5_sp1")], na.rm=T)
### so explicitly make missing if nothing was produced
dta$prod_tot_sp1[is.na(dta$grp1a16) & is.na(dta$grp2b16) &is.na(dta$grp3c16) &is.na(dta$grp4d16) &is.na(dta$grp5e16) ] <- NA

dta[c("spouse2grp_sp1f16","spouse2grp_sp2g16","spouse2grp_sp3h16","spouse2group_sp4j16", "spouse2grp5_sp5k16")] <- lapply(dta[c("spouse2grp_sp1f16","spouse2grp_sp2g16","spouse2grp_sp3h16","spouse2group_sp4j16", "spouse2grp5_sp5k16")], function(x) replace(x, x == 999, NA) )
dta[c("spouse2grp_sp1f17","spouse2grp_sp2g17","spouse2grp_sp3h17","spouse2group_sp4j17", "spouse2grp5_sp5k17")] <- lapply(dta[c("spouse2grp_sp1f17","spouse2grp_sp2g17","spouse2grp_sp3h17","spouse2group_sp4j17", "spouse2grp5_sp5k17")], function(x) replace(x, x == 999, NA) )
## note: if number of bags was zero, bagsize is missing (set here to zero)
dta$spouse2grp_sp1f17[dta$spouse2grp_sp1f16] <- 0 
dta$spouse2grp_sp2g17[dta$spouse2grp_sp2g16] <- 0 
dta$spouse2grp_sp3h17[dta$spouse2grp_sp3h16] <- 0 
dta$spouse2group_sp4j17[dta$spouse2group_sp4j16] <- 0 
dta$spouse2grp5_sp5k17[dta$spouse2grp5_sp5k16] <- 0 

dta$prod_pl1_sp2 <- dta$spouse2grp_sp1f16 * dta$spouse2grp_sp1f17
dta$prod_pl2_sp2 <- dta$spouse2grp_sp2g16 * dta$spouse2grp_sp2g17
dta$prod_pl3_sp2 <- dta$spouse2grp_sp3h16 * dta$spouse2grp_sp3h17
dta$prod_pl4_sp2 <- dta$spouse2group_sp4j16 * dta$spouse2group_sp4j17
dta$prod_pl5_sp2 <- dta$spouse2grp5_sp5k16 * dta$spouse2grp5_sp5k17

dta$prod_tot_sp2 <- rowSums(dta[c("prod_pl1_sp2","prod_pl2_sp2","prod_pl3_sp2","prod_pl4_sp2", "prod_pl5_sp2")], na.rm=T)
dta$prod_tot_sp2[is.na(dta$spouse2grp_sp1f16) & is.na(dta$spouse2grp_sp2g16) & is.na(dta$spouse2grp_sp3h16) & is.na(dta$spouse2group_sp4j16) & is.na(dta$spouse2grp5_sp5k16)] <- NA
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

## same deal here, this will be zero of nothing was produced, but this can now safely be replaced by NA
dta$area_tot_sp1 <- rowSums(dta[c("area_pl1_sp1","area_pl2_sp1","area_pl3_sp1","area_pl4_sp1", "area_pl5_sp1")], na.rm=T)
dta$area_tot_sp1[dta$area_tot_sp1 == 0] <- NA
## here dividing by 0 yield NaN anyway
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
dta$area_tot_sp2[dta$area_tot_sp2 == 0] <- NA
## /0 yields Nan
dta$yield_av_sp2 <- rowMeans(dta[c("yield_pl1_sp2","yield_pl2_sp2","yield_pl3_sp2","yield_pl4_sp2", "yield_pl5_sp2")], na.rm=T)

dta$area_tot <- rowMeans(dta[c("area_tot_sp1","area_tot_sp2")], na.rm=T)
dta$yield_av <- rowMeans(dta[c("yield_av_sp1","yield_av_sp2")], na.rm=T)

## yield worse compared to normal year?
dta[c("grp1a18","grp2b18","grp3c18","grp4d18", "grp5e18")] <- lapply(dta[c("grp1a18","grp2b18","grp3c18","grp4d18", "grp5e18")], function(x) replace(x, is.na(x), 0) )
dta$yield_worse_sp1 <- FALSE
dta$yield_worse_sp1 <- (dta$grp1a18 ==4 | dta$grp1a18==5) |  (dta$grp2b18 ==4 | dta$grp2b18==5) | (dta$grp3c18 ==4 | dta$grp3c18==5) | (dta$grp4d18 ==4 | dta$grp4d18==5) | (dta$grp5e18 ==4 | dta$grp5e18==5)

dta[c("spouse2grp_sp1f18","spouse2grp_sp2g18","spouse2grp_sp3h18","spouse2group_sp4j18", "spouse2grp5_sp5k18")] <- lapply(dta[c("spouse2grp_sp1f18","spouse2grp_sp2g18","spouse2grp_sp3h18","spouse2group_sp4j18", "spouse2grp5_sp5k18")], function(x) replace(x, is.na(x),0) )
dta$yield_worse_sp2 <- FALSE
dta$yield_worse_sp2 <- (dta$spouse2grp_sp1f18 ==4 | dta$spouse2grp_sp1f18==5) |  (dta$spouse2grp_sp2g18 ==4 | dta$spouse2grp_sp2g18==5) | (dta$spouse2grp_sp3h18 ==4 | dta$spouse2grp_sp3h18==5) | (dta$spouse2group_sp4j18 ==4 | dta$spouse2group_sp4j18==5) | (dta$spouse2grp5_sp5k18 ==4 | dta$spouse2grp5_sp5k18==5)

dta$yield_worse <- FALSE
# any spouse reports worse yield on any plot
dta$yield_worse <- dta$yield_worse_sp1 |  dta$yield_worse_sp2

### average days after rain
dta[c("grp1days19","grp2days2","grp3days3","grp4days4", "grp5days5")] <- lapply(dta[c("grp1days19","grp2days2","grp3days3","grp4days4", "grp5days5")], function(x) replace(x, x == 999 | x == 98, NA) )

dta[c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5")] <- lapply(dta[c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5")], function(x) replace(x,x == 999  | x == 98,NA) )

dta$days_av_sp1 <- rowMeans(dta[c("grp1days19","grp2days2","grp3days3","grp4days4", "grp5days5")], na.rm=T)
dta$days_av_sp2 <- rowMeans(dta[c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5")], na.rm=T)

dta$days_av <- rowMeans(dta[c("days_av_sp1", "days_av_sp2")])

### use recommended spacing on *any* plot
dta[c("grp1a201","grp2b201","grp3c201","grp4d201", "grp5e201")] <- lapply(dta[c("grp1a201","grp2b201","grp3c201","grp4d201", "grp5e201")], function(x) replace(x,is.na(x), 0) )
dta$space_sp1 <- (dta$grp1a201 + dta$grp2b201+ dta$grp3c201+ dta$grp4d201+ dta$grp5e201) > 0

dta[c("spouse2grp_sp1f201","spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201", "spouse2grp5_sp5k201")] <- lapply(dta[c("spouse2grp_sp1f201","spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201", "spouse2grp5_sp5k201")], function(x) replace(x,is.na(x),0) )
dta$space_sp2 <-  (dta$spouse2grp_sp1f201 + dta$spouse2grp_sp2g201 + dta$spouse2grp_sp3h201 + dta$spouse2group_sp4j201 + dta$spouse2grp5_sp5k201) >0 
dta$space <- (dta$space_sp1 + dta$space_sp2) > 0

### use recommended practice to fight striga on *any* plot
dta[c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241")] <- lapply(dta[c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241")], function(x) replace(x,is.na(x), 0) )
dta$striga_sp1 <- (dta$grp1a241 + dta$grp2b241+ dta$grp3c241+ dta$grp4d241+ dta$grp5e241) > 0

dta[c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241")] <- lapply(dta[c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241")], function(x) replace(x,is.na(x),0) )
dta$striga_sp2 <-  (dta$spouse2grp_sp1f241 + dta$spouse2grp_sp2g241 + dta$spouse2grp_sp3h241 + dta$spouse2group_sp4j241 + dta$spouse2grp5_sp5k241) >0 
dta$striga <- (dta$striga_sp1 + dta$striga_sp2) > 0

### use recommended weeding practice
dta[c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26")] <- lapply(dta[c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26")] , function(x) replace(x,is.na(x) | x != 1 , 0) )
dta$weed_sp1 <- (dta$grp1a26 + dta$grp2b26 + dta$grp3c26 + dta$grp4d26+ dta$grp5e26) > 0

dta[c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26")] <- lapply(dta[c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26")], function(x) replace(x,is.na(x) | x !=1  ,0) )
dta$weed_sp2 <-  (dta$spouse2grp_sp1f26 + dta$spouse2grp_sp2g26 + dta$spouse2grp_sp3h26 + dta$spouse2group_sp4j26 + dta$spouse2grp5_sp5k26) >0 
dta$weed <- (dta$weed_sp1 + dta$weed_sp2) > 0

### fertilizer use on at least one plot
dta$grp1a29[is.na(dta$grp1a29)] <- "No"
dta$grp2b29[is.na(dta$grp2b29)] <- "No"
dta$grp3c29[is.na(dta$grp3c29)] <- "No"
dta$grp4d29[is.na(dta$grp4d29)] <- "No"
dta$grp5e29[is.na(dta$grp5e29)] <- "No"


dta$fert_sp1 <- ((dta$grp1a29 == "Yes") + (dta$grp2b29  == "Yes") + (dta$grp3c29  == "Yes") + (dta$grp4d29  == "Yes") + (dta$grp5e29  == "Yes")) > 0

dta$spouse2grp_sp1f29[is.na(dta$spouse2grp_sp1f29)] <- "No"
dta$spouse2grp_sp2g29[is.na(dta$spouse2grp_sp2g29)] <- "No"
dta$spouse2grp_sp3h29[is.na(dta$spouse2grp_sp3h29)] <- "No"
dta$spouse2group_sp4j29[is.na(dta$spouse2group_sp4j29)] <- "No"
dta$spouse2grp5_sp5k29[is.na(dta$spouse2grp5_sp5k29)] <- "No"

dta$fert_sp2 <- ((dta$spouse2grp_sp1f29 == "Yes") + (dta$spouse2grp_sp2g29  == "Yes") + (dta$spouse2grp_sp3h29  == "Yes") + (dta$spouse2group_sp4j29  == "Yes") + (dta$spouse2grp5_sp5k29  == "Yes")) > 0

dta$fert <- (dta$fert_sp1 + dta$fert_sp2) > 0

### should we also look at individual fertilizer categories, or at least differentiate between organic and inorganic fertilizer?
### should we also look at individual fertilizer categories, or at least differentiate between organic and inorganic fertilizer?
### dap/npk == 1
dta$grp1a301[is.na(dta$grp1a301)] <- 0
dta$grp2b301[is.na(dta$grp2b301)] <- 0
dta$grp3c301[is.na(dta$grp3c301)] <- 0
dta$grp4d301[is.na(dta$grp4d301)] <- 0
dta$grp5e301[is.na(dta$grp5e301)] <- 0

dta$fert_dap_sp1 <- ( dta$grp1a301 + dta$grp2b301 + dta$grp3c301 + dta$grp4d301 + dta$grp5e301 ) > 0

dta$spouse2grp_sp1f301[is.na(dta$spouse2grp_sp1f301)] <- 0
dta$spouse2grp_sp2g301[is.na(dta$spouse2grp_sp2g301)] <- 0
dta$spouse2grp_sp3h301[is.na(dta$spouse2grp_sp3h301)] <- 0
dta$spouse2group_sp4j301[is.na(dta$spouse2group_sp4j301)] <- 0
dta$spouse2grp5_sp5k301[is.na(dta$spouse2grp5_sp5k301)] <- 0

dta$fert_dap_sp2 <- ( dta$spouse2grp_sp1f301 + dta$spouse2grp_sp2g301 + dta$spouse2grp_sp3h301 + dta$spouse2group_sp4j301 + dta$spouse2grp5_sp5k301  ) > 0

dta$fert_dap <- (dta$fert_dap_sp1 + dta$fert_dap_sp2) > 0
### should we also look at individual fertilizer categories, or at least differentiate between organic and inorganic fertilizer?
### urea == 2
dta$grp1a302[is.na(dta$grp1a302)] <- 0
dta$grp2b302[is.na(dta$grp2b302)] <- 0
dta$grp3c302[is.na(dta$grp3c302)] <- 0
dta$grp4d302[is.na(dta$grp4d302)] <- 0
dta$grp5e302[is.na(dta$grp5e302)] <- 0

dta$fert_urea_sp1 <- ( dta$grp1a302 + dta$grp2b302 + dta$grp3c302 + dta$grp4d302 + dta$grp5e302 ) > 0

dta$spouse2grp_sp1f302[is.na(dta$spouse2grp_sp1f302)] <- 0
dta$spouse2grp_sp2g302[is.na(dta$spouse2grp_sp2g302)] <- 0
dta$spouse2grp_sp3h302[is.na(dta$spouse2grp_sp3h302)] <- 0
dta$spouse2group_sp4j302[is.na(dta$spouse2group_sp4j302)] <- 0
dta$spouse2grp5_sp5k302[is.na(dta$spouse2grp5_sp5k302)] <- 0

dta$fert_urea_sp2 <- ( dta$spouse2grp_sp1f302 + dta$spouse2grp_sp2g302 + dta$spouse2grp_sp3h302 + dta$spouse2group_sp4j302 + dta$spouse2grp5_sp5k302  ) > 0

dta$fert_urea <- (dta$fert_urea_sp1 + dta$fert_urea_sp2) > 0

### organic == 3
dta$grp1a303[is.na(dta$grp1a303)] <- 0
dta$grp2b303[is.na(dta$grp2b303)] <- 0
dta$grp3c303[is.na(dta$grp3c303)] <- 0
dta$grp4d303[is.na(dta$grp4d303)] <- 0
dta$grp5e303[is.na(dta$grp5e303)] <- 0

dta$fert_org_sp1 <- ( dta$grp1a303 + dta$grp2b303 + dta$grp3c303 + dta$grp4d303 + dta$grp5e303 ) > 0

dta$spouse2grp_sp1f303[is.na(dta$spouse2grp_sp1f303)] <- 0
dta$spouse2grp_sp2g303[is.na(dta$spouse2grp_sp2g303)] <- 0
dta$spouse2grp_sp3h303[is.na(dta$spouse2grp_sp3h303)] <- 0
dta$spouse2group_sp4j303[is.na(dta$spouse2group_sp4j303)] <- 0
dta$spouse2grp5_sp5k303[is.na(dta$spouse2grp5_sp5k303)] <- 0

dta$fert_org_sp2 <- ( dta$spouse2grp_sp1f303 + dta$spouse2grp_sp2g303 + dta$spouse2grp_sp3h303 + dta$spouse2group_sp4j303 + dta$spouse2grp5_sp5k303  ) > 0

dta$fert_org <- (dta$fert_org_sp1 + dta$fert_org_sp2) > 0


### use of improved seed (OPV or hybrid) on at least one plot
dta$grp1a42[is.na(dta$grp1a42)] <- "No"
dta$grp2b42[is.na(dta$grp2b42)] <- "No"
dta$grp3c42[is.na(dta$grp3c42)] <- "No"
dta$grp4d42[is.na(dta$grp4d42)] <- "No"
dta$grp5e42[is.na(dta$grp5e42)] <- "No"


dta$impseed_sp1 <- ((dta$grp1a42 == "Yes") + (dta$grp2b42  == "Yes") + (dta$grp3c42  == "Yes") + (dta$grp4d42  == "Yes") + (dta$grp5e42  == "Yes")) > 0

dta$spouse2grp_sp1f42[is.na(dta$spouse2grp_sp1f42)] <- "No"
dta$spouse2grp_sp2g42[is.na(dta$spouse2grp_sp2g42)] <- "No"
dta$spouse2grp_sp3h42[is.na(dta$spouse2grp_sp3h42)] <- "No"
dta$spouse2group_sp4j42[is.na(dta$spouse2group_sp4j42)] <- "No"
dta$spouse2grp5_sp5k42[is.na(dta$spouse2grp5_sp5k42)] <- "No"

dta$impseed_sp2 <- ((dta$spouse2grp_sp1f42 == "Yes") + (dta$spouse2grp_sp2g42  == "Yes") + (dta$spouse2grp_sp3h42  == "Yes") + (dta$spouse2group_sp4j42  == "Yes") + (dta$spouse2grp5_sp5k42  == "Yes")) > 0

dta$impseed <- (dta$impseed_sp1 + dta$impseed_sp2) > 0


### use of hybird
##use bazooka on any plot?
dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")] <- lapply(dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")], function(x) replace(x, is.na(x), 0))

dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")] <- lapply(dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")], function(x) replace(x,is.na(x)  ,0) )

dta$bazooka_sp1 <- rowSums(dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]) > 0 
dta$bazooka_sp2 <- rowSums(dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]) > 0 
dta$bazooka <- dta$bazooka_sp1 | dta$bazooka_sp2 

##use longue10h on any plot?
dta[c("grp1a432","grp2b432","grp3c432","grp4d432", "grp5e432")] <- lapply(dta[c("grp1a432","grp2b432","grp3c432","grp4d432", "grp5e432")], function(x) replace(x, is.na(x), 0))
dta[c("spouse2grp_sp1f432","spouse2grp_sp2g432","spouse2grp_sp3h432","spouse2group_sp4j432", "spouse2grp5_sp5k432")] <- lapply(dta[c("spouse2grp_sp1f432","spouse2grp_sp2g432","spouse2grp_sp3h432","spouse2group_sp4j432", "spouse2grp5_sp5k432")], function(x) replace(x,is.na(x)  ,0) )

dta$longe10h_sp1 <- rowSums(dta[c("grp1a432","grp2b432","grp3c432","grp4d432", "grp5e432")])
dta$longe10h_sp2 <- rowSums(dta[c("spouse2grp_sp1f432","spouse2grp_sp2g432","spouse2grp_sp3h432","spouse2group_sp4j432", "spouse2grp5_sp5k432")])
dta$longe10h <-  dta$longe10h_sp1  | dta$longe10h_sp2 

##use other hybrid on any plot
dta[c("grp1a436","grp2b436","grp3c436","grp4d436", "grp5e436")] <- lapply(dta[c("grp1a436","grp2b436","grp3c436","grp4d436", "grp5e436")], function(x) replace(x, is.na(x), 0))
dta[c("spouse2grp_sp1f436","spouse2grp_sp2g436","spouse2grp_sp3h436","spouse2group_sp4j436", "spouse2grp5_sp5k436")] <- lapply(dta[c("spouse2grp_sp1f436","spouse2grp_sp2g436","spouse2grp_sp3h436","spouse2group_sp4j436", "spouse2grp5_sp5k436")], function(x) replace(x,is.na(x)  ,0) )

dta$other_hybrid_sp1  <- rowSums(dta[c("grp1a436","grp2b436","grp3c436","grp4d436", "grp5e436")] )
dta$other_hybrid_sp2 <- rowSums(dta[c("spouse2grp_sp1f436","spouse2grp_sp2g436","spouse2grp_sp3h436","spouse2group_sp4j436", "spouse2grp5_sp5k436")])
dta$other_hybrid <- dta$other_hybrid_sp1 | dta$other_hybrid_sp2

dta$hybrid <-  dta$bazooka | dta$longe10h | dta$other_hybrid

### use of OPV
#use longe5 on any plot?
dta[c("grp1a433","grp2b433","grp3c433","grp4d433", "grp5e433")] <- lapply(dta[c("grp1a433","grp2b433","grp3c433","grp4d433", "grp5e433")], function(x) replace(x, is.na(x), 0))

dta[c("spouse2grp_sp1f433","spouse2grp_sp2g433","spouse2grp_sp3h433","spouse2group_sp4j433", "spouse2grp5_sp5k433")] <- lapply(dta[c("spouse2grp_sp1f433","spouse2grp_sp2g433","spouse2grp_sp3h433","spouse2group_sp4j433", "spouse2grp5_sp5k433")], function(x) replace(x,is.na(x)  ,0) )

dta$longe5_sp1 <- rowSums(dta[c("grp1a433","grp2b433","grp3c433","grp4d433", "grp5e433")]) > 0 
dta$longe5_sp2 <- rowSums(dta[c("spouse2grp_sp1f433","spouse2grp_sp2g433","spouse2grp_sp3h433","spouse2group_sp4j433", "spouse2grp5_sp5k433")]) > 0 
dta$longe5 <- dta$longe5_sp1 | dta$longe5_sp2 

#use longe4 on any plot?
dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")] <- lapply(dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")], function(x) replace(x, is.na(x), 0))

dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")] <- lapply(dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")], function(x) replace(x,is.na(x)  ,0) )

dta$longe4_sp1 <- rowSums(dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]) > 0 
dta$longe4_sp2 <- rowSums(dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]) > 0 
dta$longe4 <- dta$longe4_sp1 | dta$longe4_sp2 

#use other OPV on any plot?
dta[c("grp1a435","grp2b435","grp3c435","grp4d435", "grp5e435")] <- lapply(dta[c("grp1a435","grp2b435","grp3c435","grp4d435", "grp5e435")], function(x) replace(x, is.na(x), 0))

dta[c("spouse2grp_sp1f435","spouse2grp_sp2g435","spouse2grp_sp3h435","spouse2group_sp4j435", "spouse2grp5_sp5k435")] <- lapply(dta[c("spouse2grp_sp1f435","spouse2grp_sp2g435","spouse2grp_sp3h435","spouse2group_sp4j435", "spouse2grp5_sp5k435")], function(x) replace(x,is.na(x)  ,0) )

dta$other_opv_sp1 <- rowSums(dta[c("grp1a435","grp2b435","grp3c435","grp4d435", "grp5e435")]) > 0 
dta$other_opv_sp2 <- rowSums(dta[c("spouse2grp_sp1f435","spouse2grp_sp2g435","spouse2grp_sp3h435","spouse2group_sp4j435", "spouse2grp5_sp5k435")]) > 0 
dta$other_opv <- dta$other_opv_sp1 | dta$other_opv_sp2 

dta$opv <-  dta$longe5 | dta$longe4 | dta$other_opv

### use of chemical on at least one plot
dta$grp1a55a[is.na(dta$grp1a55a)] <- "No"
dta$grp2b55b[is.na(dta$grp2b55b)] <- "No"
dta$grp3c55b[is.na(dta$grp3c55b)] <- "No"
dta$grp4d55b[is.na(dta$grp4d55b)] <- "No"
dta$grp5e55b[is.na(dta$grp5e55b)] <- "No"

dta$chem_sp1 <- ((dta$grp1a55a == "Yes") + (dta$grp2b55b  == "Yes") + (dta$grp3c55b  == "Yes") + (dta$grp4d55b  == "Yes") + (dta$grp5e55b  == "Yes")) > 0

dta$spouse2grp_sp1f55a[is.na(dta$spouse2grp_sp1f55a)] <- "No"
dta$spouse2grp_sp2g55b[is.na(dta$spouse2grp_sp2g55b)] <- "No"
dta$spouse2grp_sp3h55b[is.na(dta$spouse2grp_sp3h55b)] <- "No"
dta$spouse2group_sp4j55b[is.na(dta$spouse2group_sp4j55b)] <- "No"
dta$spouse2grp5_sp5k55b[is.na(dta$spouse2grp5_sp5k55b)] <- "No"

dta$chem_sp2 <- ((dta$spouse2grp_sp1f55a == "Yes") + (dta$spouse2grp_sp2g55b  == "Yes") + (dta$spouse2grp_sp3h55b  == "Yes") + (dta$spouse2group_sp4j55b  == "Yes") + (dta$spouse2grp5_sp5k55b  == "Yes")) > 0

dta$chem <- (dta$chem_sp1 + dta$chem_sp2) > 0

## hired labour on at least one plot
dta$grp1a151[is.na(dta$grp1a151)] <- "No"
dta$grp2b151[is.na(dta$grp2b151)] <- "No"
dta$grp3c151[is.na(dta$grp3c151)] <- "No"
dta$grp4d151[is.na(dta$grp4d151)] <- "No"
dta$grp5e151[is.na(dta$grp5e151)] <- "No"

dta$labour_sp1 <- ((dta$grp1a151 == "Yes") + (dta$grp2b55b  == "Yes") + (dta$grp3c55b  == "Yes") + (dta$grp4d55b  == "Yes") + (dta$grp5e55b  == "Yes")) > 0

dta$spouse2grp_sp1f151[is.na(dta$spouse2grp_sp1f151)] <- "No"
dta$spouse2grp_sp2g151[is.na(dta$spouse2grp_sp2g151)] <- "No"
dta$spouse2grp_sp3h151[is.na(dta$spouse2grp_sp3h151)] <- "No"
dta$spouse2group_sp4j151[is.na(dta$spouse2group_sp4j151)] <- "No"
dta$spouse2grp5_sp5k151[is.na(dta$spouse2grp5_sp5k151)] <- "No"

dta$labour_sp2 <- ((dta$spouse2grp_sp1f151 == "Yes") + (dta$spouse2grp_sp2g151  == "Yes") + (dta$spouse2grp_sp3h151  == "Yes") + (dta$spouse2group_sp4j151  == "Yes") + (dta$spouse2grp5_sp5k151  == "Yes")) > 0

dta$labour <- (dta$labour_sp1 + dta$labour_sp2) > 0

#### calculated consumption expenditure
dta[c("maize_value", "sorghum_value", "millet_value", "rice_value", "cassava_value", "sweetpotatoes_value", "beans_value", "gnuts_value", "fruits_value", "veg_value", "sugar_value", "cooking_oil_value", "soap_value", "airtime_value")] <- 
lapply(dta[c("maize_value", "sorghum_value", "millet_value", "rice_value", "cassava_value", "sweetpotatoes_value", "beans_value", "gnuts_value", "fruits_value", "veg_value", "sugar_value", "cooking_oil_value", "soap_value", "airtime_value")], function(x) replace(x, x == 999, NA) )

dta$cons_sp1 <- rowSums(dta[c("maize_value", "sorghum_value", "millet_value", "rice_value", "cassava_value", "sweetpotatoes_value", "beans_value", "gnuts_value", "fruits_value", "veg_value", "sugar_value", "cooking_oil_value", "soap_value", "airtime_value")], na.rm=T)

dta[c("spouse2maize_value_sp", "spouse2sorghum_value_sp", "spouse2millet_value_sp", "spouse2rice_value_sp", "spouse2cassava_value_sp", "spouse2sweetpotatoes_value_sp", "spouse2beans_value_sp", "spouse2gnuts_value_sp", "spouse2fruits_value_sp", "spouse2veg_value_sp", "spouse2sugar_value_sp", "spouse2cooking_oil_value_sp", "spouse2soap_value_sp", "spouse2airtime_value_sp")] <- 
lapply(dta[c("spouse2maize_value_sp", "spouse2sorghum_value_sp", "spouse2millet_value_sp", "spouse2rice_value_sp", "spouse2cassava_value_sp", "spouse2sweetpotatoes_value_sp", "spouse2beans_value_sp", "spouse2gnuts_value_sp", "spouse2fruits_value_sp", "spouse2veg_value_sp", "spouse2sugar_value_sp", "spouse2cooking_oil_value_sp", "spouse2soap_value_sp", "spouse2airtime_value_sp")], function(x) replace(x, x == 999, NA) )

dta$cons_sp2 <- rowSums(dta[c("spouse2maize_value_sp", "spouse2sorghum_value_sp", "spouse2millet_value_sp", "spouse2rice_value_sp", "spouse2cassava_value_sp", "spouse2sweetpotatoes_value_sp", "spouse2beans_value_sp", "spouse2gnuts_value_sp", "spouse2fruits_value_sp", "spouse2veg_value_sp", "spouse2sugar_value_sp", "spouse2cooking_oil_value_sp", "spouse2soap_value_sp", "spouse2airtime_value_sp")], na.rm=T)

dta$cons <- dta$cons_sp1 + dta$cons_sp2
dta$cons[dta$cons == 0] <- NA

### better off than average?
dta$better_av <- rowSums(dta[c("q409","spouse2r409")], na.rm=T) ==1

### better off than 6 mo ago?
dta$better_6m <- rowSums(dta[c("q110","spouse2r110")], na.rm=T) ==1


### can eat preferred food
dta$eatpref <- NA
dta$eatpref <- dta$q111=="No"
dta$eatpref[is.na(dta$eatpref)] <-  dta$spouse2r111[is.na(dta$eatpref)] == "No"

### has enough food to eat? 
dta$eatenough <- NA
dta$eatenough <- dta$q112=="No"
dta$eatenough[is.na(dta$eatenough)] <-  dta$spouse2r112[is.na(dta$eatenough)] == "No"

### how to determine if a plot is female managed?







########################################### function definitions #################################
## RI: a function to calculate the single sided RI p-values
## FSR_RI: Randomization inference (RI) implementation of the Westfall-Young (1993) Free Stepdown Resampling procedure
## trim: function for triming a dataset *dataset* on a variable *var*
## FW_index:

### a function to calculate the single sided RI p-values
RI <- function(dep, indep, dta , nr_repl = 1000) {
	### determines treatmetn cell
	dta <- dta %>% 
    		mutate(treat = group_indices_(dta, .dots=c("recipient", "messenger"))) 
	### allocates unique ID based on treatment cell status and village
	dta <- dta %>% 
    		mutate(uniqID = group_indices_(dta, .dots=c("distID", "subID","vilID"))) 
	### the NULL
	crit <- summary(lm(as.formula(paste(dep,indep,sep="~")), data=dta))$coefficients[2,1]
	dta_sim <- dta
	
	oper <- foreach (repl = 1:nr_repl,.combine=cbind) %dopar% {
 		resample <- function(x, ...) x[sample.int(length(x), ...)]
		dta_sim$perm <- unlist(sapply(names(table(dta$uniqID)), function(x) resample(dta$treat[dta$uniq==x])))
		dta_sim$messenger[dta_sim$perm == 1  |dta_sim$perm == 3  |dta_sim$perm == 5  ] <- "male"
		dta_sim$messenger[dta_sim$perm == 2  |dta_sim$perm == 4  |dta_sim$perm == 6  ] <- "female"
		dta_sim$messenger[dta_sim$perm == 8  |dta_sim$perm == 9  |dta_sim$perm == 7 ] <- "couple"
		dta_sim$messenger[dta_sim$perm == 10  |dta_sim$perm == 11  |dta_sim$perm == 12  ] <- "ctrl"
		dta_sim$recipient[dta_sim$perm == 1  |dta_sim$perm == 2  |dta_sim$perm == 8 |dta_sim$perm == 10  ] <- "male"
		dta_sim$recipient[dta_sim$perm == 3  |dta_sim$perm == 4  |dta_sim$perm == 9 |dta_sim$perm == 11  ] <- "female"
		dta_sim$recipient[dta_sim$perm == 5  |dta_sim$perm == 6  |dta_sim$perm == 7 |dta_sim$perm == 12  ] <- "couple"
		return( if (crit >0) {
			summary(lm(as.formula(paste(dep,indep,sep="~")), data=dta_sim))$coefficients[2,1] > crit 
			} else {
			summary(lm(as.formula(paste(dep,indep,sep="~")), data=dta_sim))$coefficients[2,1] < crit 
			} )
	}
	return(sum(oper)/nr_repl)
}

### this can go
FSR2 <- function(deps, indep, dta ,pvals = NULL, nr_repl = 1000) {
# use: FSR2( c("know_space","know_combine","know_weed") ,"messenger != 'ctrl'" ,dta_bal, nr_repl = totrep)
	### determines treatmetn cell

#deps <- c("space","striga","weed", "fert","impseed")
#indep <- "messenger != 'ctrl'"
#dta <- dta_bal
# pvals <- c(0,0,0.2727,0.0036,0.155)
#nr_repl <- 100
	dta <- dta %>% 
    		mutate(treat = group_indices_(dta, .dots=c("recipient", "messenger"))) 
	### allocates unique ID based on treatment cell status and village
	dta <- dta %>% 
    		mutate(uniqID = group_indices_(dta, .dots=c("distID", "subID","vilID"))) 
	### the NULL
### this should be estimated seperately:
beta <- array(NA,length(deps))
pval <- array(NA,length(deps))

if (is.null(pvals)) {
for (i in 1:length(deps)) {
pval[i] <- summary(lm(as.formula(paste(deps[i],indep,sep="~")), data=dta))$coefficients[2,4]
}
} else {
pval <- pvals
}	
	Ord <- order(pval)
pval <- pval[Ord]
deps <- deps[Ord]
	dta_sim <- dta
	NSnps <- length(deps)
	TestStatResamp <- matrix(nrow=nr_repl, ncol=NSnps)
	TestStatResamp2 <- matrix(nrow=nr_repl, ncol=NSnps)

oper <- foreach (repl = 1:nr_repl,.combine=cbind) %dopar% {

#	for (i in 1:nr_repl) {
 		resample <- function(x, ...) x[sample.int(length(x), ...)]
		dta_sim$perm <- unlist(sapply(names(table(dta$uniqID)), function(x) resample(dta$treat[dta$uniq==x])))
		dta_sim$messenger[dta_sim$perm == 1  |dta_sim$perm == 3  |dta_sim$perm == 5  ] <- "male"
		dta_sim$messenger[dta_sim$perm == 2  |dta_sim$perm == 4  |dta_sim$perm == 6  ] <- "female"
		dta_sim$messenger[dta_sim$perm == 8  |dta_sim$perm == 9  |dta_sim$perm == 7 ] <- "couple"
		dta_sim$messenger[dta_sim$perm == 10  |dta_sim$perm == 11  |dta_sim$perm == 12  ] <- "ctrl"
		dta_sim$recipient[dta_sim$perm == 1  |dta_sim$perm == 2  |dta_sim$perm == 8 |dta_sim$perm == 10  ] <- "male"
		dta_sim$recipient[dta_sim$perm == 3  |dta_sim$perm == 4  |dta_sim$perm == 9 |dta_sim$perm == 11  ] <- "female"
		dta_sim$recipient[dta_sim$perm == 5  |dta_sim$perm == 6  |dta_sim$perm == 7 |dta_sim$perm == 12  ] <- "couple"
		for (j in 1:length(deps)) {
			TestStatResamp[i,j] <- summary(lm(as.formula(paste(deps[j],indep,sep="~")), data=dta_sim))$coefficients[2,4]
		}
		for (j in 1:length(deps)) {
			TestStatResamp2[i,j] <- min(TestStatResamp[i,j:length(deps)])
		}
		return(TestStatResamp2)
		}

Padj <- apply(t(matrix(rep(pval,nr_repl),NSnps)) > TestStatResamp2, 2, mean)
Padj1 <- Padj
Padj2 <- Padj
		for (j in 1:length(deps)) {
			Padj2[j] <- max(Padj1[1:j])
		}
return(list(Ord, deps,Padj1, Padj2))
}


FSR_RI <- function(deps, indep, dta ,pvals = NULL, nr_repl_ri = 1000, nr_repl_pi = nr_repl_ri ) {
# Randomization inference (RI) implementation of the Westfall-Young (1993) Free Stepdown Resampling procedure for correcting for multiple inference. This should be used when RI was used to determine p-values for differences between treatment and control (indep) for a family of outcome variables (deps). It uses as inputs the RI p-values of the seperate tests for the outcomes in the family (pvals). We differentiate between the number of replications used to determine the RI p-values (nr_repl_ri), and the number of replications to do the actual adjustment nr_repl_pi
# example use: FSR_RI( c("space","striga","weed", "fert","impseed"),"messenger != 'ctrl'" ,dta_bal, c(0,0,0.27,0.00,0.16), nr_repl_ri = 100, nr_repl_pi=100)
# this function uses dplyr and supports parallel computing; add following as preamble:
#	library(dplyr)
#	library(doParallel)
#	cl <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
#	registerDoParallel(cl)
### important note: p-value significant digits need to correspond to the number of simulations nr_repl_pi
### eg: if pval = 0.001, then it is advised to set nr_repl_pi >= 1000
## The function assumes p-values are determened using RI on the difference between treatment and control (ie. calculating the proportion of randomizations where the T-C difference exceeds the acutal T-C difference) for each hypothesis seperately. These are the p-values that need to be supplied in pvals.
## Next, p-values are determined based in nr_repl_ri randomizations. For each outcome, the difference between treatment and control is compared to the acutal difference and proportions are again determined. Note that here, each time one and the same randomization draw is used to test the different outcomes seperately. Once p-values are determined using these nr_repl_ri draws, monotonicity is enfored with respect to the original ordering of p-values.
## the above is then repeated nr_repl_pi, and the resulting simulated p-values are compared to the acutal p-values from pvals (determining the proportion of simulated p-values that are smaller than the actual p-values)
# finally, monotonicity is enfored uisng successive maximization

### determine treatment cell based on cominations in 2 factorial design
dta <- dta %>% mutate(treat = group_indices_(dta, .dots=c("recipient", "messenger"))) 
### allocates unique ID based on treatment cell status and village
dta <- dta %>% mutate(uniqID = group_indices_(dta, .dots=c("distID", "subID","vilID"))) 

beta <- array(NA,length(deps))
pval <- array(NA,length(deps))
for (i in 1:length(deps)) {
beta[i]  <- summary(lm(as.formula(paste(deps[i],indep,sep="~")), data=dta))$coefficients[2,1]
}
pval <- pvals
Ord <- order(pval)
pval <- pval[Ord]
deps <- deps[Ord]
beta <- beta[Ord]
dta_sim <- dta
NSnps <- length(deps)

oper <- foreach (repl = 1:(nr_repl_ri*nr_repl_ri),.combine=cbind) %dopar% {
 		resample <- function(x, ...) x[sample.int(length(x), ...)]
		dta_sim$perm <- unlist(sapply(names(table(dta$uniqID)), function(x) resample(dta$treat[dta$uniq==x])))
		dta_sim$messenger[dta_sim$perm == 1  |dta_sim$perm == 3  |dta_sim$perm == 5  ] <- "male"
		dta_sim$messenger[dta_sim$perm == 2  |dta_sim$perm == 4  |dta_sim$perm == 6  ] <- "female"
		dta_sim$messenger[dta_sim$perm == 8  |dta_sim$perm == 9  |dta_sim$perm == 7 ] <- "couple"
		dta_sim$messenger[dta_sim$perm == 10  |dta_sim$perm == 11  |dta_sim$perm == 12  ] <- "ctrl"
		dta_sim$recipient[dta_sim$perm == 1  |dta_sim$perm == 2  |dta_sim$perm == 8 |dta_sim$perm == 10  ] <- "male"
		dta_sim$recipient[dta_sim$perm == 3  |dta_sim$perm == 4  |dta_sim$perm == 9 |dta_sim$perm == 11  ] <- "female"
		dta_sim$recipient[dta_sim$perm == 5  |dta_sim$perm == 6  |dta_sim$perm == 7 |dta_sim$perm == 12  ] <- "couple"
		### this returns n-deps x (nr_repl_ri*nr_repl_ri) of treatment-control differences
		return(unlist(lapply(deps, function(dvar) summary(lm(as.formula(paste(dvar,indep,sep="~")), data=dta_sim))$coefficients[2,1])))
}

oper <- data.frame(t(oper))
#difference between treatment and control is compared to the acutal difference
for (i in 1:NSnps) {
if (beta[i] > 0) {
	oper[,i] <- oper[,i ] > beta[i]
} else {
	oper[ ,i] <- oper[,i ] < beta[i]
}
}
## now devide in blocks of nr_repl_ri
TestStatResamp <- matrix(nrow=nr_repl_pi, ncol=NSnps)
TestStatResamp2 <- matrix(nrow=nr_repl_pi, ncol=NSnps)
oper$ind <-  rep(1:nr_repl_pi,each=nr_repl_ri)
## and calcualte p-values 
for (i in 1:nr_repl_pi) {
	TestStatResamp[i,] <- colMeans(oper[oper$ind == i, 1:NSnps])
	for (j in 1:length(deps)) {
		## enforce monotonicity wrt original ordering by calculating successive minima
		TestStatResamp2[i,j] <- min(TestStatResamp[i,j:length(deps)])
	}
}
#resulting simulated p-values are compared to the acutal p-values from pvals (determining the proportion of simulated p-values that are smaller than the actual p-values)
Padj <- apply(t(matrix(rep(pval,dim(TestStatResamp2)[1] ),NSnps)) > TestStatResamp2, 2, mean)
Padj1 <- Padj
Padj2 <- Padj
		for (j in 1:length(deps)) {
		#enfore monotonicity one final time using successive maximization
			Padj2[j] <- max(Padj1[1:j])
		}
## return ordering, dependent variables, adjusted p-vals before and after final monotonicity enforcement
return(list(Ord, deps,Padj1, Padj2))
}



trim <- function(var, dataset, trim_perc=.1) {
### function for triming a dataset *dataset* on a variable *var*
return( subset(dataset,dataset[var] > quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)), na.rm=T)[1] & dataset[var] < quantile(dataset[var], c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[2]) )
}

FW_index <- function(treat, indexer, data, nr_repl=1000) {
### function to make family wise index using covariance as weights (following)
### FW_index("messenger != 'ctrl' ", c("log_prod_tot", "log_area_tot", "log_yield_av"),dta_bal2)
colSd <- function (x, na.rm=FALSE) apply(X=x, MARGIN=2, FUN=sd, na.rm=na.rm)

data[indexer] <- (data[indexer] - colMeans(data[indexer], na.rm=T))/colSd(data[indexer], na.rm=T)

mat <- var(cbind(data[indexer]), na.rm=T)
diag(mat) <- 0
rs <- 1/rowSums(mat )
data$index <- t( t(as.matrix(rs/sum(rs))) %*% t(cbind(data[indexer])))
mod <- summary(lm(as.formula(paste("index", treat, sep="~")), data=data ) )
data$index <- as.vector(data$index)
sig <- RI("index" ,treat , data, nr_repl = nr_repl)
return(list(mod,sig))
}










