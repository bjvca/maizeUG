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

## note: if number of bags was zero, bagsize is missing so we set here to zero:
dta$grp1a17[dta$grp1a16 == 0] <- 0
dta$grp2b17[dta$grp2b16 == 0] <- 0
dta$grp3c17[dta$grp3c16 == 0] <- 0
dta$grp4d17[dta$grp4d16 == 0] <- 0
dta$grp5e17[dta$grp5e16 == 0] <- 0
#prod = bags x kg per bag
dta$prod_pl1_sp1 <- dta$grp1a16 * dta$grp1a17
dta$prod_pl2_sp1 <- dta$grp2b16 * dta$grp2b17
dta$prod_pl3_sp1 <- dta$grp3c16 * dta$grp3c17
dta$prod_pl4_sp1 <- dta$grp4d16 * dta$grp4d17
dta$prod_pl5_sp1 <- dta$grp5e16 * dta$grp5e17
### how do we handle missings? The below will also produce missings if nothing has been produced
dta$prod_tot_sp1 <- rowSums(dta[c("prod_pl1_sp1","prod_pl2_sp1","prod_pl3_sp1","prod_pl4_sp1", "prod_pl5_sp1")], na.rm=T)
### so explicitly make missing if nothing was produced
dta$prod_tot_sp1[is.na(dta$grp1a16) & is.na(dta$grp2b16) &is.na(dta$grp3c16) &is.na(dta$grp4d16) &is.na(dta$grp5e16) ] <- NA

### now for spouse

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
## problem with taking mean is that this may increase measurement error of plot is managed by only one person. Let us assume the men know, unless woman is interviewed first and says she makes the decisions and man agrees or man is interviewed first and he states it is women and woman agress

dta$mgt_pl1 <- "man"
dta$mgt_pl1[dta$grp1a10==1 & dta$spouse2grp_sp1f10==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl1[dta$grp1a10==2 & dta$spouse2grp_sp1f10==1 & dta$person_interviewed=="man"] <- "woman"
dta$mgt_pl2 <- "man"
dta$mgt_pl2[dta$grp2b10==1 & dta$spouse2grp_sp2g10==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl2[dta$grp2b10==2 & dta$spouse2grp_sp2g10==1 & dta$person_interviewed=="man"] <- "woman"
dta$mgt_pl3 <- "man"
dta$mgt_pl3[dta$grp3c10==1 & dta$spouse2grp_sp3h10==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl3[dta$grp3c10==2 & dta$spouse2grp_sp3h10==1 & dta$person_interviewed=="man"] <- "woman"
dta$mgt_pl4 <- "man"
dta$mgt_pl4[dta$grp4d10==1 & dta$spouse2group_sp4j10==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl4[dta$grp4d10==2 & dta$spouse2group_sp4j10==1 & dta$person_interviewed=="man"] <- "woman"
dta$mgt_pl5 <- "man"
dta$mgt_pl5[dta$grp5e10==1 & dta$spouse2grp5_sp5k10==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl5[dta$grp5e10==2 & dta$spouse2grp5_sp5k10==1 & dta$person_interviewed=="man"] <- "woman"


dta$prod_pl1 <- dta$grp1a16 * dta$grp1a17
dta$prod_pl1[is.na(dta$prod_pl1) |  dta$mgt_pl1 == "woman"] <- dta$spouse2grp_sp1f16[is.na(dta$prod_pl1) |  dta$mgt_pl1 == "woman"] * dta$spouse2grp_sp1f17[is.na(dta$prod_pl1) |  dta$mgt_pl1 == "woman"]

dta$prod_pl2 <- dta$grp2b16 * dta$grp2b17
dta$prod_pl2[is.na(dta$prod_pl2) |  dta$mgt_pl2 == "woman"] <- dta$spouse2grp_sp2g16[is.na(dta$prod_pl2) |  dta$mgt_pl2 == "woman"]  * dta$spouse2grp_sp2g17[is.na(dta$prod_pl2) |  dta$mgt_pl2 == "woman"] 

dta$prod_pl3 <- dta$grp3c16 * dta$grp3c17
dta$prod_pl3[is.na(dta$prod_pl3) |  dta$mgt_pl3 == "woman"] <-  dta$spouse2grp_sp3h16[is.na(dta$prod_pl3) |  dta$mgt_pl3 == "woman"] * dta$spouse2grp_sp3h17[is.na(dta$prod_pl3) |  dta$mgt_pl3 == "woman"]


dta$prod_pl4 <- dta$grp4d16 * dta$grp4d17
dta$prod_pl4[is.na(dta$prod_pl4) |  dta$mgt_pl4 == "woman"] <- dta$spouse2group_sp4j16[is.na(dta$prod_pl4) |  dta$mgt_pl4 == "woman"] * dta$spouse2group_sp4j17[is.na(dta$prod_pl4) |  dta$mgt_pl4 == "woman"]

dta$prod_pl5 <- dta$grp5e16 * dta$grp5e17
dta$prod_pl5[is.na(dta$prod_pl5) |  dta$mgt_pl5 == "woman"] <- dta$spouse2grp5_sp5k16[is.na(dta$prod_pl5) |  dta$mgt_pl5 == "woman"] * dta$spouse2grp5_sp5k17[is.na(dta$prod_pl5) |  dta$mgt_pl5 == "woman"]

dta$prod_tot_man <- rowSums(dta[c("prod_pl1","prod_pl2","prod_pl3","prod_pl4", "prod_pl5")], na.rm=T)
dta$prod_tot_man[is.na(dta$prod_pl1) & is.na(dta$prod_pl2) & is.na(dta$prod_pl3) & is.na(dta$prod_pl4) & is.na(dta$prod_pl5)] <- NA


### now scale by area and take intercropping into account 
dta[c("grp1a11","grp2b11","grp3c11","grp4d11", "grp5e11")] <- lapply(dta[c("grp1a11","grp2b11","grp3c11","grp4d11", "grp5e11")], function(x) replace(x, x == 999, NA) )

dta[c("spouse2grp_sp1f11","spouse2grp_sp2g11","spouse2grp_sp3h11","spouse2group_sp4j11", "spouse2grp5_sp5k11")] <- lapply(dta[c("spouse2grp_sp1f11","spouse2grp_sp2g11","spouse2grp_sp3h11","spouse2group_sp4j11", "spouse2grp5_sp5k11")], function(x) replace(x,x == 999,NA) )

dta$area_pl1_sp1 <-(dta$grp1a11 * ((dta$grp1a14) /100))
dta$area_pl2_sp1 <-(dta$grp2b11 * ((dta$grp2b14) /100))
dta$area_pl3_sp1 <-(dta$grp3c11 * ((dta$grp3c14) /100))
dta$area_pl4_sp1 <-(dta$grp4d11 * ((dta$grp4d14) /100))
dta$area_pl5_sp1 <-(dta$grp5e11 * ((dta$grp5e14) /100))
## same deal here, this will be zero of nothing was produced, but this can now safely be replaced by NA
dta$area_tot_sp1 <- rowSums(dta[c("area_pl1_sp1","area_pl2_sp1","area_pl3_sp1","area_pl4_sp1", "area_pl5_sp1")], na.rm=T)
dta$area_tot_sp1[dta$area_tot_sp1 == 0] <- NA
dta$yield_av_sp1 <- dta$prod_tot_sp1/dta$area_tot_sp1

###alternative using plot manager
dta$area_pl1 <-(dta$grp1a11 * ((dta$grp1a14) /100))
dta$area_pl1[is.na(dta$area_pl1) |  dta$mgt_pl1 == "woman"] <-  (dta$spouse2grp_sp1f11[is.na(dta$area_pl1) |  dta$mgt_pl1 == "woman"] * ((dta$spouse2grp_sp1f14[is.na(dta$area_pl1) |  dta$mgt_pl1 == "woman"]) /100))
dta$area_pl2 <-(dta$grp2b11 * ((dta$grp2b14) /100))
dta$area_pl2[is.na(dta$area_pl2) |  dta$mgt_pl2 == "woman"] <-   (dta$spouse2grp_sp2g11[is.na(dta$area_pl2) |  dta$mgt_pl2 == "woman"]  * ((dta$spouse2grp_sp2g14[is.na(dta$area_pl2) |  dta$mgt_pl2 == "woman"] ) /100))
dta$area_pl3 <-(dta$grp3c11 * ((dta$grp3c14) /100))
dta$area_pl3[is.na(dta$area_pl3) |  dta$mgt_pl3 == "woman"] <-  (dta$spouse2grp_sp3h11[is.na(dta$area_pl3) |  dta$mgt_pl3 == "woman"] * ((dta$spouse2grp_sp3h14[is.na(dta$area_pl3) |  dta$mgt_pl3 == "woman"]) /100))
dta$area_pl4 <-(dta$grp4d11 * ((dta$grp4d14) /100))
dta$area_pl4[is.na(dta$area_pl4) |  dta$mgt_pl4 == "woman"] <-  (dta$spouse2group_sp4j11[is.na(dta$area_pl4) |  dta$mgt_pl4 == "woman"] * ((dta$spouse2group_sp4j14[is.na(dta$area_pl4) |  dta$mgt_pl4 == "woman"]) /100))
dta$area_pl5 <-(dta$grp5e11 * ((dta$grp5e14) /100))
dta$area_pl5[is.na(dta$area_pl5) |  dta$mgt_pl5 == "woman"] <-  (dta$spouse2grp5_sp5k11[is.na(dta$area_pl5) |  dta$mgt_pl5 == "woman"] * ((dta$spouse2grp5_sp5k14[is.na(dta$area_pl5) |  dta$mgt_pl5 == "woman"]) /100))

dta$area_tot_man <- rowSums(dta[c("area_pl1","area_pl2","area_pl3","area_pl4", "area_pl5")], na.rm=T)
dta$area_tot_man[is.na(dta$area_pl1) & is.na(dta$area_pl2) & is.na(dta$area_pl3) & is.na(dta$area_pl4) & is.na(dta$area_pl5)] <- NA
#########

dta$yield_av_man <- dta$prod_tot_man / dta$area_tot_man


dta$area_pl1_sp2 <- (dta$spouse2grp_sp1f11 * ((dta$spouse2grp_sp1f14) /100))
dta$area_pl2_sp2 <- (dta$spouse2grp_sp2g11 * ((dta$spouse2grp_sp2g14) /100))
dta$area_pl3_sp2 <- (dta$spouse2grp_sp3h11 * ((dta$spouse2grp_sp3h14) /100))
dta$area_pl4_sp2 <- (dta$spouse2group_sp4j11 * ((dta$spouse2group_sp4j14) /100))
dta$area_pl5_sp2 <- (dta$spouse2grp5_sp5k11 * ((dta$spouse2grp5_sp5k14) /100))

dta$area_tot_sp2 <- rowSums(dta[c("area_pl1_sp2","area_pl2_sp2","area_pl3_sp2","area_pl4_sp2", "area_pl5_sp2")], na.rm=T)
dta$area_tot_sp2[dta$area_tot_sp2 == 0] <- NA
## /0 yields Nan
dta$yield_av_sp2 <- dta$prod_tot_sp2/dta$area_tot_sp2

dta$area_tot <- rowMeans(dta[c("area_tot_sp1","area_tot_sp2")], na.rm=T)

dta$yield_av <- rowMeans(cbind(dta$yield_av_sp1, dta$yield_av_sp2), na.rm=T)
dta$yield_av[dta$yield_av==0] <- NA

## yield better compared to normal year?
dta$yield_better_sp1 <- FALSE
dta$yield_better_sp1 <- rowSums(cbind((dta$grp1a18 ==1 | dta$grp1a18==2) ,  (dta$grp2b18 ==1 | dta$grp2b18==2), (dta$grp3c18 ==1 | dta$grp3c18==2), (dta$grp4d18 ==1 | dta$grp4d18==2), (dta$grp5e18 ==1 | dta$grp5e18==2)), na.rm=T)>0

dta$yield_better_sp1[is.na(dta$grp1a18 ==1 | dta$grp1a18==2) &  is.na(dta$grp2b18 ==1 | dta$grp2b18==2) & is.na(dta$grp3c18 ==1 | dta$grp3c18==2) &is.na(dta$grp4d18 ==1 | dta$grp4d18==2) & is.na(dta$grp5e18 ==1 | dta$grp5e18==2)] <- NA

dta$yield_better_sp2 <- FALSE
dta$yield_better_sp2 <- rowSums(cbind((dta$spouse2grp_sp1f18 ==1 | dta$spouse2grp_sp1f18==2),  (dta$spouse2grp_sp2g18 ==1 | dta$spouse2grp_sp2g18==2), (dta$spouse2grp_sp3h18 ==1 | dta$spouse2grp_sp3h18==2), (dta$spouse2group_sp4j18 ==1 | dta$spouse2group_sp4j18==2), (dta$spouse2grp5_sp5k18 ==1 | dta$spouse2grp5_sp5k18==2)), na.rm=T) > 0

dta$yield_better_sp2[is.na(dta$spouse2grp_sp1f18 ==1 | dta$spouse2grp_sp1f18==2) & is.na(dta$spouse2grp_sp2g18 ==1 | dta$spouse2grp_sp2g18==2) & is.na(dta$spouse2grp_sp3h18 ==1 | dta$spouse2grp_sp3h18==2) & is.na(dta$spouse2group_sp4j18 ==1 | dta$spouse2group_sp4j18==2) & is.na(dta$spouse2grp5_sp5k18 ==1 | dta$spouse2grp5_sp5k18==2)] <- NA

dta$yield_better <- FALSE
# any spouse reports worse yield on any plot
dta$yield_better <- rowSums(dta[c("yield_better_sp1","yield_better_sp2")], na.rm=T) > 0
dta$yield_better[is.na(dta$yield_better_sp1) & is.na(dta$yield_better_sp2)] <- NA


dta$yield_better_pl1 <- (dta$grp1a18 ==1 | dta$grp1a18==2)
dta$yield_better_pl1[is.na(dta$yield_better_pl1) |  dta$mgt_pl1 == "woman"] <- (dta$spouse2grp_sp1f18 ==1 | dta$spouse2grp_sp1f18==2)[is.na(dta$yield_better_pl1) |  dta$mgt_pl1 == "woman"]

dta$yield_better_pl2 <- (dta$grp2b18 ==1 | dta$grp2b18==2)
dta$yield_better_pl2[is.na(dta$yield_better_pl2) |  dta$mgt_pl2 == "woman"] <- (dta$spouse2grp_sp2g18 ==1 | dta$spouse2grp_sp2g18==2)[is.na(dta$yield_better_pl2) |  dta$mgt_pl2 == "woman"]


dta$yield_better_pl3 <- (dta$grp3c18 ==1 | dta$grp3c18==2)
dta$yield_better_pl3[is.na(dta$yield_better_pl3) |  dta$mgt_pl3 == "woman"] <- (dta$spouse2grp_sp3h18 ==1 | dta$spouse2grp_sp3h18==2)[is.na(dta$yield_better_pl3) |  dta$mgt_pl3 == "woman"]

dta$yield_better_pl4 <- (dta$grp3c18 ==1 | dta$grp3c18==2)
dta$yield_better_pl4[is.na(dta$yield_better_pl4) |  dta$mgt_pl4 == "woman"] <- (dta$spouse2group_sp4j18 ==1 | dta$spouse2group_sp4j18==2)[is.na(dta$yield_better_pl4) |  dta$mgt_pl4 == "woman"]

dta$yield_better_pl5 <- (dta$grp3c18 ==1 | dta$grp3c18==2)
dta$yield_better_pl5[is.na(dta$yield_better_pl5) |  dta$mgt_pl5 == "woman"] <- (dta$spouse2grp5_sp5k18 ==1 | dta$spouse2grp5_sp5k18==2)[is.na(dta$yield_better_pl5) |  dta$mgt_pl5 == "woman"]
dta$yield_better_man <- rowSums(dta[c("yield_better_pl1","yield_better_pl2","yield_better_pl3","yield_better_pl4","yield_better_pl5")], na.rm=T)>0


### planted immediately after the rain
names(dta)[names(dta) == 'grp1days19'] <- 'grp1days1'
"grp1days19"
# replace to high number if before rain or dont know as we will use the minimum (first plot)
dta[c("grp1days19","grp2days2","grp3days3","grp4days4", "grp5days5")] <- lapply(dta[c("grp1days1","grp2days2","grp3days3","grp4days4", "grp5days5")], function(x) replace(x, is.na(x), 999) )

dta[c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5")] <- lapply(dta[c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5")], function(x) replace(x,is.na(x), 999) )

dta$days_min_sp1 <- do.call(pmin,dta[c("grp1days1","grp2days2","grp3days3","grp4days4", "grp5days5")])

dta$days_min_sp2 <- do.call(pmin,dta[c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5")])
dta$days_min <- do.call(pmin,dta[c("days_min_sp1", "days_min_sp2")])
dta$days_min[dta$days_min > 30] <- NA
##depending on who decided on days (decide1)
dta$mgt_pl1 <- "man"
dta$mgt_pl1[dta$grp1decide1==1 & dta$spouse2grp_sp1decide_sp1==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl1[dta$grp1decide1==2 & dta$spouse2grp_sp1decide_sp1==1 & dta$person_interviewed=="man"] <- "woman"
dta$days_pl1 <- dta$grp1days1
dta$days_pl1[(dta$days_pl1==999) |  (dta$mgt_pl1 == "woman")] <- dta$spouse2grp_sp1days1[(dta$days_pl1==999) |  (dta$mgt_pl1 == "woman")]

dta$mgt_pl2 <- "man"
dta$mgt_pl2[dta$grp2decide2==1 & dta$spouse2grp_sp2decide_sp2==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl2[dta$grp2decide2==2 & dta$spouse2grp_sp2decide_sp2==1 & dta$person_interviewed=="man"] <- "woman"
dta$days_pl2 <- dta$grp2days2
dta$days_pl2[(dta$days_pl2==999) |  (dta$mgt_pl2 == "woman")] <- dta$spouse2grp_sp2days_sp2[(dta$days_pl2==999) |  (dta$mgt_pl2 == "woman")]

dta$mgt_pl3 <- "man"
dta$mgt_pl3[dta$grp3decide3==1 & dta$spouse2grp_sp3decide_sp3==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl3[dta$grp3decide3==2 & dta$spouse2grp_sp3decide_sp3==1 & dta$person_interviewed=="man"] <- "woman"
dta$days_pl3 <- dta$grp3days3
dta$days_pl3[(dta$days_pl3==999) |  (dta$mgt_pl3 == "woman")] <- dta$spouse2grp_sp3days_sp3[(dta$days_pl3==999) |  (dta$mgt_pl3 == "woman")]

dta$mgt_pl4 <- "man"
dta$mgt_pl4[dta$grp4decide4==1 & dta$spouse2grp_sp4decide_sp4==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl4[dta$grp4decide4==2 & dta$spouse2grp_sp4decide_sp4==1 & dta$person_interviewed=="man"] <- "woman"
dta$days_pl4 <- dta$grp4days4
dta$days_pl4[(dta$days_pl4==999) |  (dta$mgt_pl4 == "woman")] <- dta$spouse2group_sp4dayssp4[(dta$days_pl4==999) |  (dta$mgt_pl4 == "woman")]


dta$mgt_pl5 <- "man"
dta$mgt_pl5[dta$grp5decide5==1 & dta$spouse2grp_sp5decide_sp5==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl5[dta$grp5decide5==2 & dta$spouse2grp_sp5decide_sp5==1 & dta$person_interviewed=="man"] <- "woman"
dta$days_pl5 <- dta$grp5days5
dta$days_pl5[(dta$days_pl5==999) |  (dta$mgt_pl5 == "woman")] <- dta$spouse2grp5_sp5dayssp5[(dta$days_pl5==999) |  (dta$mgt_pl5 == "woman")]

dta$days_min_mgt <- do.call(pmin,dta[c("days_pl1","days_pl2","days_pl3","days_pl4", "days_pl5")])
dta$days_min_mgt[dta$days_min_mgt > 30] <- NA
### use recommended spacing on *any* plot
dta$space_sp1 <- rowSums(dta[c("grp1a201","grp2b201","grp3c201","grp4d201","grp5e201")], na.rm=T) > 0

dta$space_sp2 <- rowSums(dta[c("spouse2grp_sp1f201","spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201","spouse2grp5_sp5k201")], na.rm=T) >0 
dta$space <- (dta$space_sp1 + dta$space_sp2) > 0


## depending on who decided on spacing (A23)
dta$mgt_pl1 <- "man"
dta$mgt_pl1[dta$grp1a23==1 & dta$spouse2grp_sp1f23==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl1[dta$grp1a23==2 & dta$spouse2grp_sp1f23==1 & dta$person_interviewed=="man"] <- "woman"

dta$space_pl1 <- dta$grp1a201 
dta$space_pl1[is.na(dta$space_pl1) |  dta$mgt_pl1 == "woman"] <- dta$spouse2grp_sp1f201[is.na(dta$space_pl1) |  dta$mgt_pl1 == "woman"]

dta$mgt_pl2 <- "man"
dta$mgt_pl2[dta$grp2b23==1 & dta$spouse2grp_sp2g23==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl2[dta$grp2b23==2 & dta$spouse2grp_sp2g23==1 & dta$person_interviewed=="man"] <- "woman"

dta$space_pl2 <- dta$grp2b201 
dta$space_pl2[is.na(dta$space_pl2) |  dta$mgt_pl2 == "woman"] <- dta$spouse2grp_sp2g201[is.na(dta$space_pl2) |  dta$mgt_pl2 == "woman"]

dta$mgt_pl3 <- "man"
dta$mgt_pl3[dta$grp3c23==1 & dta$spouse2grp_sp3h23==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl3[dta$grp3c23==2 & dta$spouse2grp_sp3h23==1 & dta$person_interviewed=="man"] <- "woman"

dta$space_pl3 <- dta$grp3c201 
dta$space_pl3[is.na(dta$space_pl3) |  dta$mgt_pl3 == "woman"] <- dta$spouse2grp_sp3h201[is.na(dta$space_pl3) |  dta$mgt_pl3 == "woman"]

dta$mgt_pl4 <- "man"
dta$mgt_pl4[dta$grp4e23==1 & dta$spouse2group_sp4j23==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl4[dta$grp4e23==2 & dta$spouse2group_sp4j23==1 & dta$person_interviewed=="man"] <- "woman"

dta$space_pl4 <- dta$grp4d201 
dta$space_pl4[is.na(dta$space_pl4) |  dta$mgt_pl4 == "woman"] <- dta$spouse2group_sp4j201[is.na(dta$space_pl4) |  dta$mgt_pl4 == "woman"]

dta$mgt_pl5 <- "man"
dta$mgt_pl5[dta$grp5f23==1 & dta$spouse2grp5_sp5k23==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl5[dta$grp5f23==2 & dta$spouse2grp5_sp5k23==1 & dta$person_interviewed=="man"] <- "woman"

dta$space_pl5 <- dta$grp5e201 
dta$space_pl5[is.na(dta$space_pl5) |  dta$mgt_pl5 == "woman"] <- dta$spouse2grp5_sp5k201[is.na(dta$space_pl5) |  dta$mgt_pl5 == "woman"]

dta$space_mgt <- rowSums(dta[c("space_pl1","space_pl2","space_pl3","space_pl4","space_pl5")], na.rm=T)>0
### use recommended practice to fight striga on *any* plot

dta$striga_sp1 <- rowSums(dta[c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241")], na.rm=T) > 0
dta$striga_sp2 <-  rowSums(dta[c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241")], na.rm=T) >0 
dta$striga <- (dta$striga_sp1 + dta$striga_sp2) > 0

dta$mgt_pl1 <- "man"
dta$mgt_pl1[dta$grp1a25==1 & dta$spouse2grp_sp1f25==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl1[dta$grp1a25==2 & dta$spouse2grp_sp1f25==1 & dta$person_interviewed=="man"] <- "woman"
dta$striga_pl1 <- dta$grp1a241
dta$striga_pl1[is.na(dta$striga_pl1) |  dta$mgt_pl1 == "woman"] <- dta$spouse2grp_sp1f241[is.na(dta$striga_pl1) |  dta$mgt_pl1 == "woman"]

dta$mgt_pl2 <- "man"
dta$mgt_pl2[dta$grp1a25==1 & dta$spouse2grp_sp1f25==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl2[dta$grp1a25==2 & dta$spouse2grp_sp1f25==1 & dta$person_interviewed=="man"] <- "woman"
dta$striga_pl2 <- dta$grp2b241
dta$striga_pl2[is.na(dta$striga_pl2) |  dta$mgt_pl2 == "woman"] <- dta$spouse2grp_sp2g241[is.na(dta$striga_pl2) |  dta$mgt_pl2 == "woman"]

dta$mgt_pl3 <- "man"
dta$mgt_pl3[dta$grp1a25==1 & dta$spouse2grp_sp1f25==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl3[dta$grp1a25==2 & dta$spouse2grp_sp1f25==1 & dta$person_interviewed=="man"] <- "woman"
dta$striga_pl3 <- dta$grp3c241
dta$striga_pl3[is.na(dta$striga_pl3) |  dta$mgt_pl3 == "woman"] <- dta$spouse2grp_sp3h241[is.na(dta$striga_pl3) |  dta$mgt_pl3 == "woman"]

dta$mgt_pl4 <- "man"
dta$mgt_pl4[dta$grp1a25==1 & dta$spouse2grp_sp1f25==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl4[dta$grp1a25==2 & dta$spouse2grp_sp1f25==1 & dta$person_interviewed=="man"] <- "woman"
dta$striga_pl4 <- dta$grp4d241
dta$striga_pl4[is.na(dta$striga_pl4) |  dta$mgt_pl4 == "woman"] <- dta$spouse2group_sp4j241[is.na(dta$striga_pl4) |  dta$mgt_pl4 == "woman"]

dta$mgt_pl5 <- "man"
dta$mgt_pl5[dta$grp1a25==1 & dta$spouse2grp_sp1f25==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl5[dta$grp1a25==2 & dta$spouse2grp_sp1f25==1 & dta$person_interviewed=="man"] <- "woman"
dta$striga_pl5 <- dta$grp5e241
dta$striga_pl5[is.na(dta$striga_pl5) |  dta$mgt_pl5 == "woman"] <- dta$spouse2grp5_sp5k241[is.na(dta$striga_pl5) |  dta$mgt_pl5 == "woman"]

dta$striga_mgt <- rowSums(dta[c("striga_pl1","striga_pl2","striga_pl3","striga_pl4","striga_pl5")], na.rm=T)>0
### use recommended weeding practice: we recommend first weeding after 18-20 days, which is in the 3rd week - option 3
dta$weed_sp1 <- rowSums(dta[c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26")] == 3, na.rm=T) > 0
dta$weed_sp2 <- rowSums(dta[c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26")] == 3  ,na.rm=T) >0 
dta$weed <- (dta$weed_sp1 + dta$weed_sp2) > 0

dta$mgt_pl1 <- "man"
dta$mgt_pl1[dta$grp1a27==1 & dta$spouse2grp_sp1f27==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl1[dta$grp1a27==2 & dta$spouse2grp_sp1f27==1 & dta$person_interviewed=="man"] <- "woman"
dta$weed_pl1 <- dta$grp1a26 == 3
dta$weed_pl1[is.na(dta$striga_pl1) |  dta$mgt_pl1 == "woman"] <- dta$spouse2grp_sp1f26[is.na(dta$striga_pl1) |  dta$mgt_pl1 == "woman"] ==3

dta$mgt_pl2 <- "man"
dta$mgt_pl2[dta$grp1a27==1 & dta$spouse2grp_sp1f27==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl2[dta$grp1a27==2 & dta$spouse2grp_sp1f27==1 & dta$person_interviewed=="man"] <- "woman"
dta$weed_pl2 <- dta$grp2b26 == 3
dta$weed_pl2[is.na(dta$striga_pl2) |  dta$mgt_pl2 == "woman"] <- dta$spouse2grp_sp2g26[is.na(dta$striga_pl2) |  dta$mgt_pl2 == "woman"] ==3

dta$mgt_pl3 <- "man"
dta$mgt_pl3[dta$grp1a27==1 & dta$spouse2grp_sp1f27==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl3[dta$grp1a27==2 & dta$spouse2grp_sp1f27==1 & dta$person_interviewed=="man"] <- "woman"
dta$weed_pl3 <- dta$grp3c26 == 3
dta$weed_pl3[is.na(dta$striga_pl3) |  dta$mgt_pl3 == "woman"] <- dta$spouse2grp_sp3h26[is.na(dta$striga_pl3) |  dta$mgt_pl3 == "woman"] ==3

dta$mgt_pl4 <- "man"
dta$mgt_pl4[dta$grp1a27==1 & dta$spouse2grp_sp1f27==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl4[dta$grp1a27==2 & dta$spouse2grp_sp1f27==1 & dta$person_interviewed=="man"] <- "woman"
dta$weed_pl4 <- dta$grp4d26 == 3
dta$weed_pl4[is.na(dta$striga_pl4) |  dta$mgt_pl4 == "woman"] <- dta$spouse2group_sp4j26[is.na(dta$striga_pl4) |  dta$mgt_pl4 == "woman"] ==3

dta$mgt_pl5 <- "man"
dta$mgt_pl5[dta$grp1a27==1 & dta$spouse2grp_sp1f27==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl5[dta$grp1a27==2 & dta$spouse2grp_sp1f27==1 & dta$person_interviewed=="man"] <- "woman"
dta$weed_pl5 <- dta$grp5e26 == 3
dta$weed_pl5[is.na(dta$striga_pl5) |  dta$mgt_pl5 == "woman"] <- dta$spouse2grp5_sp5k26[is.na(dta$striga_pl5) |  dta$mgt_pl5 == "woman"] ==3

dta$weed_mgt <- rowSums(dta[c("weed_pl1","weed_pl2","weed_pl3","weed_pl4","weed_pl5")], na.rm=T) > 0

### fertilizer use on at least one plot
dta$fert_sp1 <- rowSums(dta[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")] == "Yes", na.rm=T ) > 0
dta$fert_sp2 <- rowSums(dta[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29")] == "Yes", na.rm=T) > 0

dta$fert <- (dta$fert_sp1 + dta$fert_sp2) > 0

### use the person who is generally managing the plot to determine if fertilizer was used

dta$mgt_pl1 <- "man"
dta$mgt_pl1[dta$grp1a10==1 & dta$spouse2grp_sp1f10==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl1[dta$grp1a10==2 & dta$spouse2grp_sp1f10==1 & dta$person_interviewed=="man"] <- "woman"
dta$mgt_pl2 <- "man"
dta$mgt_pl2[dta$grp2b10==1 & dta$spouse2grp_sp2g10==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl2[dta$grp2b10==2 & dta$spouse2grp_sp2g10==1 & dta$person_interviewed=="man"] <- "woman"
dta$mgt_pl3 <- "man"
dta$mgt_pl3[dta$grp3c10==1 & dta$spouse2grp_sp3h10==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl3[dta$grp3c10==2 & dta$spouse2grp_sp3h10==1 & dta$person_interviewed=="man"] <- "woman"
dta$mgt_pl4 <- "man"
dta$mgt_pl4[dta$grp4d10==1 & dta$spouse2group_sp4j10==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl4[dta$grp4d10==2 & dta$spouse2group_sp4j10==1 & dta$person_interviewed=="man"] <- "woman"
dta$mgt_pl5 <- "man"
dta$mgt_pl5[dta$grp5e10==1 & dta$spouse2grp5_sp5k10==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl5[dta$grp5e10==2 & dta$spouse2grp5_sp5k10==1 & dta$person_interviewed=="man"] <- "woman"

dta$fert_pl1 <- dta$grp1a29 == "Yes"
dta$fert_pl1[is.na(dta$fert_pl1) |  dta$mgt_pl1 == "woman"] <- dta$spouse2grp_sp1f29[is.na(dta$fert_pl1) |  dta$mgt_pl1 == "woman"] == "Yes"
dta$fert_pl2 <- dta$grp2b29 == "Yes"
dta$fert_pl2[is.na(dta$fert_pl2) |  dta$mgt_pl2 == "woman"] <- dta$spouse2grp_sp2g29[is.na(dta$fert_pl2) |  dta$mgt_pl2 == "woman"] == "Yes"
dta$fert_pl3 <- dta$grp3c29 == "Yes"
dta$fert_pl3[is.na(dta$fert_pl3) |  dta$mgt_pl3 == "woman"] <- dta$spouse2grp_sp3h29[is.na(dta$fert_pl3) |  dta$mgt_pl3 == "woman"] == "Yes"
dta$fert_pl4 <- dta$grp4d29 == "Yes"
dta$fert_pl4[is.na(dta$fert_pl4) |  dta$mgt_pl4 == "woman"] <- dta$spouse2group_sp4j29[is.na(dta$fert_pl4) |  dta$mgt_pl4 == "woman"] == "Yes"
dta$fert_pl5 <- dta$grp5e29 == "Yes"
dta$fert_pl5[is.na(dta$fert_pl5) |  dta$mgt_pl5 == "woman"] <- dta$spouse2grp5_sp5k29[is.na(dta$fert_pl5) |  dta$mgt_pl5 == "woman"] == "Yes"

dta$fert_mgt <- rowSums(dta[c("fert_pl1","fert_pl2","fert_pl3","fert_pl4","fert_pl5")], na.rm=T) > 0

### use of improved seed (OPV or hybrid) on at least one plot
dta$impseed_sp1 <- rowSums(dta[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")] == "Yes", na.rm=T) > 0
dta$impseed_sp2 <- rowSums(dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42")] == "Yes", na.rm=T) > 0

dta$impseed <- (dta$impseed_sp1 + dta$impseed_sp2) > 0

dta$impseed_pl1 <- dta$grp1a42 == "Yes"
dta$impseed_pl1[is.na(dta$impseed_pl1) |  dta$mgt_pl1 == "woman"] <- dta$spouse2grp_sp1f42[is.na(dta$impseed_pl1) |  dta$mgt_pl1 == "woman"] == "Yes"
dta$impseed_pl2 <- dta$grp2b42 == "Yes"
dta$impseed_pl2[is.na(dta$impseed_pl2) |  dta$mgt_pl2 == "woman"] <- dta$spouse2grp_sp2g42[is.na(dta$impseed_pl2) |  dta$mgt_pl2 == "woman"] == "Yes"
dta$impseed_pl3 <- dta$grp3c42 == "Yes"
dta$impseed_pl3[is.na(dta$impseed_pl3) |  dta$mgt_pl3 == "woman"] <- dta$spouse2grp_sp3h42[is.na(dta$impseed_pl3) |  dta$mgt_pl3 == "woman"] == "Yes"
dta$impseed_pl4 <- dta$grp4d42 == "Yes"
dta$impseed_pl4[is.na(dta$impseed_pl4) |  dta$mgt_pl4 == "woman"] <- dta$spouse2group_sp4j42[is.na(dta$impseed_pl4) |  dta$mgt_pl4 == "woman"] == "Yes"
dta$impseed_pl5 <- dta$grp5e42 == "Yes"
dta$impseed_pl5[is.na(dta$impseed_pl5) |  dta$mgt_pl5 == "woman"] <- dta$spouse2grp5_sp5k42[is.na(dta$impseed_pl5) |  dta$mgt_pl5 == "woman"] == "Yes"

dta$impseed_mgt <- rowSums(dta[c("impseed_pl1","impseed_pl2","impseed_pl3","impseed_pl4","impseed_pl5")], na.rm=T) > 0


### should we also look at individual fertilizer categories, or at least differentiate between organic and inorganic fertilizer?
### dap/npk == 1
dta$fert_dap_sp1 <- rowSums( dta[c("grp1a301","grp2b301","grp3c301","grp4d301","grp5e301")],na.rm=T  ) > 0
dta$fert_dap_sp2 <- rowSums( dta[c("spouse2grp_sp1f301","spouse2grp_sp2g301","spouse2grp_sp3h301","spouse2group_sp4j301","spouse2grp5_sp5k301")],na.rm=T) > 0

dta$fert_dap <- (dta$fert_dap_sp1 + dta$fert_dap_sp2) > 0

### amount of dap applied:

dta[c("grp1a37","grp2b37","grp3c37","grp4d37", "grp5e37")] <- lapply(dta[c("grp1a37","grp2b37","grp3c37","grp4d37", "grp5e37")], function(x) replace(x, x== 999, NA) )
dta$kg_dap_sp1 <- rowSums(dta[c("grp1a37","grp2b37","grp3c37","grp4d37", "grp5e37")], na.rm=T)

dta[c("spouse2grp_sp1f37","spouse2grp_sp2g37","spouse2grp_sp3h37","spouse2group_sp4j37", "spouse2grp5_sp5k37")] <- lapply(dta[c("spouse2grp_sp1f37","spouse2grp_sp2g37","spouse2grp_sp3h37","spouse2group_sp4j37", "spouse2grp5_sp5k37")], function(x) replace(x, x== 999, NA) )
dta$kg_dap_sp2 <- rowSums(dta[c("spouse2grp_sp1f37","spouse2grp_sp2g37","spouse2grp_sp3h37","spouse2group_sp4j37", "spouse2grp5_sp5k37")], na.rm=T)

dta$kg_dap <- (dta$kg_dap_sp1 + dta$kg_dap_sp2)/2
### dap management indicator: here use who actually applied the fertilizer (a32)
dta$mgt_pl1 <- "man"
dta$mgt_pl1[dta$grp1a32==1 & dta$spouse2grp_sp1f32==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl1[dta$grp1a32==2 & dta$spouse2grp_sp1f32==1 & dta$person_interviewed=="man"] <- "woman"
dta$kg_dap_pl1 <- dta$grp1a37
dta$kg_dap_pl1[is.na(dta$kg_dap_pl1) | dta$mgt_pl1 == "woman"]  <- dta$spouse2grp_sp1f37[is.na(dta$kg_dap_pl1) | dta$mgt_pl1 == "woman"] 

dta$mgt_pl2 <- "man"
dta$mgt_pl2[dta$grp2b32==1 & dta$spouse2grp_sp2g32==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl2[dta$grp2b32==2 & dta$spouse2grp_sp2g32==1 & dta$person_interviewed=="man"] <- "woman"
dta$kg_dap_pl2 <- dta$grp2b37
dta$kg_dap_pl2[is.na(dta$kg_dap_pl2)| dta$mgt_pl2 == "woman"]  <- dta$spouse2grp_sp2g37[is.na(dta$kg_dap_pl2) | dta$mgt_pl2 == "woman"] 

dta$mgt_pl3 <- "man"
dta$mgt_pl3[dta$grp3c32==1 & dta$spouse2grp_sp3h32==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl3[dta$grp3c32==2 & dta$spouse2grp_sp3h32==1 & dta$person_interviewed=="man"] <- "woman"
dta$kg_dap_pl3 <- dta$grp3c37
dta$kg_dap_pl3[is.na(dta$kg_dap_pl3)| dta$mgt_pl3 == "woman"]  <- dta$spouse2grp_sp3h37[is.na(dta$kg_dap_pl3) | dta$mgt_pl3 == "woman"] 

dta$mgt_pl4 <- "man"
dta$mgt_pl4[dta$grp4d32==1 & dta$spouse2group_sp4j32==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl4[dta$grp4d32==2 & dta$spouse2group_sp4j32==1 & dta$person_interviewed=="man"] <- "woman"
dta$kg_dap_pl4 <- dta$grp4d37
dta$kg_dap_pl4[is.na(dta$kg_dap_pl4) | dta$mgt_pl4 == "woman"]  <- dta$spouse2group_sp4j37[is.na(dta$kg_dap_pl4) | dta$mgt_pl4 == "woman"] 

dta$mgt_pl5 <- "man"
dta$mgt_pl5[dta$grp5e32==1 & dta$spouse2grp5_sp5k32==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl5[dta$grp5e32==2 & dta$spouse2grp5_sp5k32==1 & dta$person_interviewed=="man"] <- "woman"
dta$kg_dap_pl5 <- dta$grp5e37
dta$kg_dap_pl5[is.na(dta$kg_dap_pl5) | dta$mgt_pl5 == "woman"]  <- dta$spouse2grp5_sp5k37[is.na(dta$kg_dap_pl5) | dta$mgt_pl5 == "woman"] 

dta$kg_dap_mgt <- rowSums(dta[c("kg_dap_pl1","kg_dap_pl2","kg_dap_pl3","kg_dap_pl4","kg_dap_pl5")], na.rm=T)
dta$kg_dap_ac_mgt <- rowMeans(dta[c("kg_dap_pl1","kg_dap_pl2","kg_dap_pl3","kg_dap_pl4","kg_dap_pl5")]/dta[c("area_pl1","area_pl2","area_pl3","area_pl4","area_pl5")], na.rm=T)
### should we also look at individual fertilizer categories, or at least differentiate between organic and inorganic fertilizer?
### urea == 2
dta$fert_urea_sp1 <- rowSums(dta[c("grp1a302","grp2b302","grp3c302","grp4d302","grp5e302")], na.rm=T) > 0
dta$fert_urea_sp2 <-rowSums(dta[c("spouse2grp_sp1f302","spouse2grp_sp2g302","spouse2grp_sp3h302","spouse2group_sp4j302","spouse2grp5_sp5k302")], na.rm=T  ) > 0

dta$fert_urea <- rowSums(dta[c("fert_urea_sp1","fert_urea_sp2")], na.rm=T) > 0

dta[c("grp1a36g","grp2b36g","grp3c36g","grp4d36g", "grp5e36g")] <- lapply(dta[c("grp1a36g","grp2b36g","grp3c36g","grp4d36g", "grp5e36g")], function(x) replace(x, x== 999, NA) )
dta$kg_urea_sp1 <-rowSums(dta[c("grp1a36g","grp2b36g","grp3c36g","grp4d36g", "grp5e36g")], na.rm=T)

dta[c("spouse2grp_sp1f36g","spouse2grp_sp2g36g","spouse2grp_sp3h36g","spouse2group_sp4j36g", "spouse2grp5_sp5k36g")] <- lapply(dta[c("spouse2grp_sp1f36g","spouse2grp_sp2g36g","spouse2grp_sp3h36g","spouse2group_sp4j36g", "spouse2grp5_sp5k36g")], function(x) replace(x, x== 999, NA) )
dta$kg_urea_sp2 <-rowSums(dta[c("spouse2grp_sp1f36g","spouse2grp_sp2g36g","spouse2grp_sp3h36g","spouse2group_sp4j36g", "spouse2grp5_sp5k36g")], na.rm=T)

dta$kg_urea <- (dta$kg_urea_sp1 + dta$kg_urea_sp2)/2
### dap management indicator: here use who actually applied the fertilizer (a31b)
dta$mgt_pl1 <- "man"
dta$mgt_pl1[dta$grp1a31b==1 & dta$spouse2grp_sp1f31b==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl1[dta$grp1a31b==2 & dta$spouse2grp_sp1f31b==1 & dta$person_interviewed=="man"] <- "woman"
dta$kg_urea_pl1 <- dta$grp1a36g
dta$kg_urea_pl1[is.na(dta$kg_urea_pl1) | dta$mgt_pl1 == "woman"]  <- dta$spouse2grp_sp1f36g[is.na(dta$kg_urea_pl1) | dta$mgt_pl1 == "woman"] 

dta$mgt_pl2 <- "man"
dta$mgt_pl2[dta$grp2b31b==1 & dta$spouse2grp_sp2g31b==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl2[dta$grp2b31b==2 & dta$spouse2grp_sp2g31b==1 & dta$person_interviewed=="man"] <- "woman"
dta$kg_urea_pl2 <- dta$grp2b36g
dta$kg_urea_pl2[is.na(dta$kg_urea_pl2) | dta$mgt_pl2 == "woman"]  <- dta$spouse2grp_sp2g36g[is.na(dta$kg_urea_pl2) | dta$mgt_pl2 == "woman"] 

dta$mgt_pl3 <- "man"
dta$mgt_pl3[dta$grp3c31b==1 & dta$spouse2grp_sp3h31b==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl3[dta$grp3c31b==2 & dta$spouse2grp_sp3h31b==1 & dta$person_interviewed=="man"] <- "woman"
dta$kg_urea_pl3 <- dta$grp3c36g
dta$kg_urea_pl3[is.na(dta$kg_urea_pl3) | dta$mgt_pl3 == "woman"]  <- dta$spouse2grp_sp3h36g[is.na(dta$kg_urea_pl3) | dta$mgt_pl3 == "woman"] 

dta$mgt_pl4 <- "man"
dta$mgt_pl4[dta$grp4d31b==1 & dta$spouse2group_sp4j31b==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl4[dta$grp4d31b==2 & dta$spouse2group_sp4j31b==1 & dta$person_interviewed=="man"] <- "woman"
dta$kg_urea_pl4 <- dta$grp4d36g
dta$kg_urea_pl4[is.na(dta$kg_urea_pl4) | dta$mgt_pl4 == "woman"]  <- dta$spouse2group_sp4j36g[is.na(dta$kg_urea_pl4) | dta$mgt_pl4 == "woman"] 

dta$mgt_pl5 <- "man"
dta$mgt_pl5[dta$grp5e31b==1 & dta$spouse2grp5_sp5k31b==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl5[dta$grp5e31b==2 & dta$spouse2grp5_sp5k31b==1 & dta$person_interviewed=="man"] <- "woman"
dta$kg_urea_pl5 <- dta$grp5e36g
dta$kg_urea_pl5[is.na(dta$kg_urea_pl5) | dta$mgt_pl5 == "woman"]  <- dta$spouse2grp5_sp5k36g[is.na(dta$kg_urea_pl5) | dta$mgt_pl5 == "woman"]

dta$kg_urea_mgt <- rowSums(dta[c("kg_urea_pl1","kg_urea_pl2","kg_urea_pl3","kg_urea_pl4","kg_urea_pl5")], na.rm=T) 
dta$kg_urea_ac_mgt <- rowMeans(dta[c("kg_urea_pl1","kg_urea_pl2","kg_urea_pl3","kg_urea_pl4","kg_urea_pl5")]/dta[c("area_pl1","area_pl2","area_pl3","area_pl4","area_pl5")], na.rm=T)

###totals for mineral fertilizers and application rates
dta$kg_inorg <- dta$kg_dap + dta$kg_urea
dta$kg_ac_inorg <- dta$kg_inorg/dta$area_tot

###
dta[c("kg_urea_pl1","kg_urea_pl2","kg_urea_pl3","kg_urea_pl4","kg_urea_pl5")] <- lapply(dta[c("kg_urea_pl1","kg_urea_pl2","kg_urea_pl3","kg_urea_pl4","kg_urea_pl5")], function(x) replace(x, is.na(x), 0) )
 dta[c("kg_dap_pl1","kg_dap_pl2","kg_dap_pl3","kg_dap_pl4","kg_dap_pl5")]  <- lapply( dta[c("kg_dap_pl1","kg_dap_pl2","kg_dap_pl3","kg_dap_pl4","kg_dap_pl5")], function(x) replace(x, is.na(x), 0) )
dta$kg_inorg_mgt <- rowSums(dta[c("kg_urea_pl1","kg_urea_pl2","kg_urea_pl3","kg_urea_pl4","kg_urea_pl5")] + dta[c("kg_dap_pl1","kg_dap_pl2","kg_dap_pl3","kg_dap_pl4","kg_dap_pl5")], na.rm=T)
dta$kg_ac_inorg_mgt <-  rowMeans((dta[c("kg_urea_pl1","kg_urea_pl2","kg_urea_pl3","kg_urea_pl4","kg_urea_pl5")] + dta[c("kg_dap_pl1","kg_dap_pl2","kg_dap_pl3","kg_dap_pl4","kg_dap_pl5")])/dta[c("area_pl1","area_pl2","area_pl3","area_pl4","area_pl5")], na.rm=T)

### organic == 3
dta$fert_org_sp1 <- rowSums(dta[c("grp1a303","grp2b303","grp3c303","grp4d303","grp5e303")],na.rm=T) > 0
dta$fert_org_sp2 <- rowSums(dta[c("spouse2grp_sp1f303","spouse2grp_sp2g303","spouse2grp_sp3h303","spouse2group_sp4j303","spouse2grp5_sp5k303")], na.rm=T) > 0
dta$fert_org <- (dta$fert_org_sp1 + dta$fert_org_sp2) > 0
#
## quantity organic fertilizer measured in bags
dta[c("grp1a37g","grp2b37g","grp3c37g","grp4d37g", "grp5e37g")] <- lapply(dta[c("grp1a37g","grp2b37g","grp3c37g","grp4d37g", "grp5e37g")], function(x) replace(x,x== 999, NA) )
dta$bags_org_sp1 <-rowSums(dta[c("grp1a37g","grp2b37g","grp3c37g","grp4d37g", "grp5e37g")], na.rm=T)

dta[c("spouse2grp_sp1f37g","spouse2grp_sp2g37g","spouse2grp_sp3h37g","spouse2group_sp4j37g", "spouse2grp5_sp5k37g")] <- lapply(dta[c("spouse2grp_sp1f37g","spouse2grp_sp2g37g","spouse2grp_sp3h37g","spouse2group_sp4j37g", "spouse2grp5_sp5k37g")], function(x) replace(x, x== 999, NA) )
dta$bags_org_sp2 <-rowSums(dta[c("spouse2grp_sp1f37g","spouse2grp_sp2g37g","spouse2grp_sp3h37g","spouse2group_sp4j37g", "spouse2grp5_sp5k37g")], na.rm=T)
dta$bags_org <- (dta$bags_org_sp1 + dta$bags_org_sp2)/2

### organic management indicator: here use who actually applied the fertilizer (a37b)
dta$mgt_pl1 <- "man"
dta$mgt_pl1[dta$grp1a37b==1 & dta$spouse2grp_sp1f37b==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl1[dta$grp1a37b==2 & dta$spouse2grp_sp1f37b==1 & dta$person_interviewed=="man"] <- "woman"
dta$bags_org_pl1 <- dta$grp1a37g
dta$bags_org_pl1[is.na(dta$bags_org_pl1) | dta$mgt_pl1 == "woman"]  <- dta$spouse2grp_sp1f37g[is.na(dta$bags_org_pl1) | dta$mgt_pl1 == "woman"] 

dta$mgt_pl2 <- "man"
dta$mgt_pl2[dta$grp2b37b==1 & dta$spouse2grp_sp2g37b==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl2[dta$grp2b37b==2 & dta$spouse2grp_sp2g37b==1 & dta$person_interviewed=="man"] <- "woman"
dta$bags_org_pl2 <- dta$grp2b37g
dta$bags_org_pl2[is.na(dta$bags_org_pl2) | dta$mgt_pl2 == "woman"]  <- dta$spouse2grp_sp2g37g[is.na(dta$bags_org_pl2) | dta$mgt_pl2 == "woman"] 

dta$mgt_pl3 <- "man"
dta$mgt_pl3[dta$grp3c37b==1 & dta$spouse2grp_sp3h37b==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl3[dta$grp3c37b==2 & dta$spouse2grp_sp3h37b==1 & dta$person_interviewed=="man"] <- "woman"
dta$bags_org_pl3 <- dta$grp3c37g
dta$bags_org_pl3[is.na(dta$bags_org_pl3) | dta$mgt_pl3 == "woman"]  <- dta$spouse2grp_sp3h37g[is.na(dta$bags_org_pl3) | dta$mgt_pl3 == "woman"] 

dta$mgt_pl4 <- "man"
dta$mgt_pl4[dta$grp4d37b==1 & dta$spouse2group_sp4j37b==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl4[dta$grp4d37b==2 & dta$spouse2group_sp4j37b==1 & dta$person_interviewed=="man"] <- "woman"
dta$bags_org_pl4 <- dta$grp4d37g
dta$bags_org_pl4[is.na(dta$bags_org_pl4) | dta$mgt_pl4 == "woman"]  <- dta$spouse2group_sp4j37g[is.na(dta$bags_org_pl4) | dta$mgt_pl4 == "woman"] 

dta$mgt_pl5 <- "man"
dta$mgt_pl5[dta$grp5e37b==1 & dta$spouse2grp5_sp5k37b==2 & dta$person_interviewed=="woman"] <- "woman"
dta$mgt_pl5[dta$grp5e37b==2 & dta$spouse2grp5_sp5k37b==1 & dta$person_interviewed=="man"] <- "woman"
dta$bags_org_pl5 <- dta$grp5e37g
dta$bags_org_pl5[is.na(dta$bags_org_pl5) | dta$mgt_pl5 == "woman"]  <- dta$spouse2grp5_sp5k37g[is.na(dta$bags_org_pl5) | dta$mgt_pl5 == "woman"] 

dta$bags_org_mgt <- rowSums(dta[c("bags_org_pl1","bags_org_pl2","bags_org_pl3","bags_org_pl4","bags_org_pl5")], na.rm=T)
dta$bags_org_ac_mgt <- rowMeans(dta[c("bags_org_pl1","bags_org_pl2","bags_org_pl3","bags_org_pl4","bags_org_pl5")]/dta[c("area_pl1","area_pl2","area_pl3","area_pl4","area_pl5")], na.rm=T)

### use of hybird
##use longue10h on any plot?
dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")] <- lapply(dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")], function(x) replace(x, is.na(x), 0))

dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")] <- lapply(dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")], function(x) replace(x,is.na(x)  ,0) )

dta$longe10h_sp1 <- rowSums(dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]) > 0 
dta$longe10h_sp2 <- rowSums(dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]) > 0 
dta$longe10h <- dta$longe10h_sp1 | dta$longe10h_sp2 

dta[c("grp1a43d","grp2b43d","grp3c43d","grp4d43d", "grp5e43d")] <- lapply(dta[c("grp1a43d","grp2b43d","grp3c43d","grp4d43d", "grp5e43d")], function(x) replace(x,is.na(x) | x== 999, 0) )
dta$kg_longe10h_sp1 <-rowSums(dta[c("grp1a43d","grp2b43d","grp3c43d","grp4d43d", "grp5e43d")], na.rm=T)

dta[c("spouse2grp_sp1f43d","spouse2grp_sp2g43d","spouse2grp_sp3h43d","spouse2group_sp4j43d", "spouse2grp5_sp5k43d")] <- lapply(dta[c("spouse2grp_sp1f43d","spouse2grp_sp2g43d","spouse2grp_sp3h43d","spouse2group_sp4j43d", "spouse2grp5_sp5k43d")], function(x) replace(x,is.na(x) | x== 999, 0) )
dta$kg_longe10h_sp2 <-rowSums(dta[c("spouse2grp_sp1f43d","spouse2grp_sp2g43d","spouse2grp_sp3h43d","spouse2group_sp4j43d", "spouse2grp5_sp5k43d")], na.rm=T)
dta$kg_longe10h <- (dta$kg_longe10h_sp1 + dta$kg_longe10h_sp2)/2

##use bazooka on any plot?
dta[c("grp1a432","grp2b432","grp3c432","grp4d432", "grp5e432")] <- lapply(dta[c("grp1a432","grp2b432","grp3c432","grp4d432", "grp5e432")], function(x) replace(x, is.na(x), 0))
dta[c("spouse2grp_sp1f432","spouse2grp_sp2g432","spouse2grp_sp3h432","spouse2group_sp4j432", "spouse2grp5_sp5k432")] <- lapply(dta[c("spouse2grp_sp1f432","spouse2grp_sp2g432","spouse2grp_sp3h432","spouse2group_sp4j432", "spouse2grp5_sp5k432")], function(x) replace(x,is.na(x)  ,0) )

dta$bazooka_sp1 <- rowSums(dta[c("grp1a432","grp2b432","grp3c432","grp4d432", "grp5e432")])
dta$bazooka_sp2 <- rowSums(dta[c("spouse2grp_sp1f432","spouse2grp_sp2g432","spouse2grp_sp3h432","spouse2group_sp4j432", "spouse2grp5_sp5k432")])
dta$bazooka <-  dta$bazooka_sp1  | dta$bazooka_sp2 

dta[c("grp1a43k","grp2b43k","grp3c43k","grp4d43k", "grp5e43k")] <- lapply(dta[c("grp1a43k","grp2b43k","grp3c43k","grp4d43k", "grp5e43k")], function(x) replace(x,is.na(x) | x== 999, 0) )
dta$kg_bazooka_sp1 <-rowSums(dta[c("grp1a43k","grp2b43k","grp3c43k","grp4d43k", "grp5e43k")], na.rm=T)

dta[c("spouse2grp_sp1f43k","spouse2grp_sp2g43k","spouse2grp_sp3h43k","spouse2group_sp4j43k", "spouse2grp5_sp5k43k")] <- lapply(dta[c("spouse2grp_sp1f43k","spouse2grp_sp2g43k","spouse2grp_sp3h43k","spouse2group_sp4j43k", "spouse2grp5_sp5k43k")], function(x) replace(x,is.na(x) | x== 999, 0) )
dta$kg_bazooka_sp2 <-rowSums(dta[c("spouse2grp_sp1f43k","spouse2grp_sp2g43k","spouse2grp_sp3h43k","spouse2group_sp4j43k", "spouse2grp5_sp5k43k")], na.rm=T)
dta$kg_bazooka <- (dta$kg_bazooka_sp1 + dta$kg_bazooka_sp2)/2

##use other hybrid on any plot
dta[c("grp1a436","grp2b436","grp3c436","grp4d436", "grp5e436")] <- lapply(dta[c("grp1a436","grp2b436","grp3c436","grp4d436", "grp5e436")], function(x) replace(x, is.na(x), 0))
dta[c("spouse2grp_sp1f436","spouse2grp_sp2g436","spouse2grp_sp3h436","spouse2group_sp4j436", "spouse2grp5_sp5k436")] <- lapply(dta[c("spouse2grp_sp1f436","spouse2grp_sp2g436","spouse2grp_sp3h436","spouse2group_sp4j436", "spouse2grp5_sp5k436")], function(x) replace(x,is.na(x)  ,0) )

dta$other_hybrid_sp1  <- rowSums(dta[c("grp1a436","grp2b436","grp3c436","grp4d436", "grp5e436")] )
dta$other_hybrid_sp2 <- rowSums(dta[c("spouse2grp_sp1f436","spouse2grp_sp2g436","spouse2grp_sp3h436","spouse2group_sp4j436", "spouse2grp5_sp5k436")])
dta$other_hybrid <- dta$other_hybrid_sp1 | dta$other_hybrid_sp2

dta[c("grp1a53","grp2b53","grp3c53","grp4d53", "grp5e53")] <- lapply(dta[c("grp1a53","grp2b53","grp3c53","grp4d53", "grp5e53")], function(x) replace(x,is.na(x) | x== 999, 0) )
dta$kg_other_hybrid_sp1 <-rowSums(dta[c("grp1a53","grp2b53","grp3c53","grp4d53", "grp5e53")], na.rm=T)

dta[c("spouse2grp_sp1f53","spouse2grp_sp2g53","spouse2grp_sp3h53","spouse2group_sp4j53", "spouse2grp5_sp5k53")] <- lapply(dta[c("spouse2grp_sp1f53","spouse2grp_sp2g53","spouse2grp_sp3h53","spouse2group_sp4j53", "spouse2grp5_sp5k53")], function(x) replace(x,is.na(x) | x== 999, 0) )
dta$kg_other_hybrid_sp2 <-rowSums(dta[c("spouse2grp_sp1f53","spouse2grp_sp2g53","spouse2grp_sp3h53","spouse2group_sp4j53", "spouse2grp5_sp5k53")], na.rm=T)
dta$kg_other_hybrid <- (dta$kg_other_hybrid_sp1 + dta$kg_other_hybrid_sp2)/2

dta$hybrid <-  dta$bazooka | dta$longe10h | dta$other_hybrid
dta$kg_hybrid <- rowSums(dta[c("kg_longe10h","kg_bazooka","kg_other_hybrid")])

### use of OPV
#use longe5 on any plot?
dta[c("grp1a433","grp2b433","grp3c433","grp4d433", "grp5e433")] <- lapply(dta[c("grp1a433","grp2b433","grp3c433","grp4d433", "grp5e433")], function(x) replace(x, is.na(x), 0))

dta[c("spouse2grp_sp1f433","spouse2grp_sp2g433","spouse2grp_sp3h433","spouse2group_sp4j433", "spouse2grp5_sp5k433")] <- lapply(dta[c("spouse2grp_sp1f433","spouse2grp_sp2g433","spouse2grp_sp3h433","spouse2group_sp4j433", "spouse2grp5_sp5k433")], function(x) replace(x,is.na(x)  ,0) )

dta$longe5_sp1 <- rowSums(dta[c("grp1a433","grp2b433","grp3c433","grp4d433", "grp5e433")]) > 0 
dta$longe5_sp2 <- rowSums(dta[c("spouse2grp_sp1f433","spouse2grp_sp2g433","spouse2grp_sp3h433","spouse2group_sp4j433", "spouse2grp5_sp5k433")]) > 0 
dta$longe5 <- dta$longe5_sp1 | dta$longe5_sp2 

dta[c("grp1a43r","grp2b43r","grp3c43r","grp4d43r", "grp5e43r")] <- lapply(dta[c("grp1a43r","grp2b43r","grp3c43r","grp4d43r", "grp5e43r")], function(x) replace(x,is.na(x) | x== 999, 0) )
dta$kg_longe5_sp1 <-rowSums(dta[c("grp1a43r","grp2b43r","grp3c43r","grp4d43r", "grp5e43r")], na.rm=T)

dta[c("spouse2grp_sp1f43r","spouse2grp_sp2g43r","spouse2grp_sp3h43r","spouse2group_sp4j43r", "spouse2grp5_sp5k43r")] <- lapply(dta[c("spouse2grp_sp1f43r","spouse2grp_sp2g43r","spouse2grp_sp3h43r","spouse2group_sp4j43r", "spouse2grp5_sp5k43r")], function(x) replace(x,is.na(x) | x== 999, 0) )
dta$kg_longe5_sp2 <-rowSums(dta[c("spouse2grp_sp1f43r","spouse2grp_sp2g43r","spouse2grp_sp3h43r","spouse2group_sp4j43r", "spouse2grp5_sp5k43r")], na.rm=T)
dta$kg_longe5 <- (dta$kg_longe5_sp1 + dta$kg_longe5_sp2)/2

#use longe4 on any plot?
dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")] <- lapply(dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")], function(x) replace(x, is.na(x), 0))

dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")] <- lapply(dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")], function(x) replace(x,is.na(x)  ,0) )

dta$longe4_sp1 <- rowSums(dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]) > 0 
dta$longe4_sp2 <- rowSums(dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]) > 0 
dta$longe4 <- dta$longe4_sp1 | dta$longe4_sp2 

dta[c("grp1a43y","grp2b43y","grp3c43y","grp4d43y", "grp5e43y")] <- lapply(dta[c("grp1a43y","grp2b43y","grp3c43y","grp4d43y", "grp5e43y")], function(x) replace(x,is.na(x) | x== 999, 0) )
dta$kg_longe4_sp1 <-rowSums(dta[c("grp1a43y","grp2b43y","grp3c43y","grp4d43y", "grp5e43y")], na.rm=T)

dta[c("spouse2grp_sp1f43y","spouse2grp_sp2g43y","spouse2grp_sp3h43y","spouse2group_sp4j43y", "spouse2grp5_sp5k43y")] <- lapply(dta[c("spouse2grp_sp1f43y","spouse2grp_sp2g43y","spouse2grp_sp3h43y","spouse2group_sp4j43y", "spouse2grp5_sp5k43y")], function(x) replace(x,is.na(x) | x== 999, 0) )
dta$kg_longe4_sp2 <-rowSums(dta[c("spouse2grp_sp1f43y","spouse2grp_sp2g43y","spouse2grp_sp3h43y","spouse2group_sp4j43y", "spouse2grp5_sp5k43y")], na.rm=T)
dta$kg_longe4 <- (dta$kg_longe4_sp1 + dta$kg_longe4_sp2)/2

#use other OPV on any plot?
dta[c("grp1a435","grp2b435","grp3c435","grp4d435", "grp5e435")] <- lapply(dta[c("grp1a435","grp2b435","grp3c435","grp4d435", "grp5e435")], function(x) replace(x, is.na(x), 0))

dta[c("spouse2grp_sp1f435","spouse2grp_sp2g435","spouse2grp_sp3h435","spouse2group_sp4j435", "spouse2grp5_sp5k435")] <- lapply(dta[c("spouse2grp_sp1f435","spouse2grp_sp2g435","spouse2grp_sp3h435","spouse2group_sp4j435", "spouse2grp5_sp5k435")], function(x) replace(x,is.na(x)  ,0) )

dta$other_opv_sp1 <- rowSums(dta[c("grp1a435","grp2b435","grp3c435","grp4d435", "grp5e435")]) > 0 
dta$other_opv_sp2 <- rowSums(dta[c("spouse2grp_sp1f435","spouse2grp_sp2g435","spouse2grp_sp3h435","spouse2group_sp4j435", "spouse2grp5_sp5k435")]) > 0 
dta$other_opv <- dta$other_opv_sp1 | dta$other_opv_sp2 


dta[c("grp1a47","grp2b47","grp3c47","grp4d47", "grp5e47")] <- lapply(dta[c("grp1a47","grp2b47","grp3c47","grp4d47", "grp5e47")], function(x) replace(x,is.na(x) | x== 999, 0) )
dta$kg_other_opv_sp1 <-rowSums(dta[c("grp1a47","grp2b47","grp3c47","grp4d47", "grp5e47")], na.rm=T)

dta[c("spouse2grp_sp1f47","spouse2grp_sp2g47","spouse2grp_sp3h47","spouse2group_sp4j47", "spouse2grp5_sp5k47")] <- lapply(dta[c("spouse2grp_sp1f47","spouse2grp_sp2g47","spouse2grp_sp3h47","spouse2group_sp4j47", "spouse2grp5_sp5k47")], function(x) replace(x,is.na(x) | x== 999, 0) )
dta$kg_other_opv_sp2 <-rowSums(dta[c("spouse2grp_sp1f47","spouse2grp_sp2g47","spouse2grp_sp3h47","spouse2group_sp4j47", "spouse2grp5_sp5k47")], na.rm=T)
dta$kg_other_opv <- (dta$kg_other_opv_sp1 + dta$kg_other_opv_sp2)/2

dta$opv <-  dta$longe5 | dta$longe4 | dta$other_opv
dta$kg_opv <- rowSums(dta[c("kg_longe5","kg_longe4","kg_other_opv")])

dta$kg_impseed <- dta$kg_hybrid + dta$kg_opv

### combiner: use improved seed + fertilizer on at least one plot (A29==yes & A42==yes)

dta$combiner_sp1 <- rowSums( cbind((dta$grp1a29=="Yes" & dta$grp1a42 == "Yes"),(dta$grp2b29=="Yes" & dta$grp2b42 == "Yes"),(dta$grp3c29=="Yes" & dta$grp3c42 == "Yes"),(dta$grp4d29=="Yes" & dta$grp4d42 == "Yes"),(dta$grp5e29=="Yes" & dta$grp5e42 == "Yes")), na.rm=T) > 0

dta$combiner_sp2 <- rowSums( cbind((dta$spouse2grp_sp1f29=="Yes" & dta$spouse2grp_sp1f42 == "Yes"),(dta$spouse2grp_sp2g29=="Yes" & dta$spouse2grp_sp2g42 == "Yes"),(dta$spouse2grp_sp3h29=="Yes" & dta$spouse2grp_sp3h42 == "Yes"),(dta$spouse2group_sp4j29=="Yes" & dta$spouse2group_sp4j42 == "Yes"),(dta$spouse2grp5_sp5k29=="Yes" & dta$spouse2grp5_sp5k42 == "Yes")), na.rm=T) > 0

dta$combiner <- dta$combiner_sp1 | dta$combiner_sp2

dta$combiner_mgt <- rowSums(dta[c("fert_pl1","fert_pl2","fert_pl3","fert_pl4","fert_pl5")]*dta[c("impseed_pl1", "impseed_pl2","impseed_pl3","impseed_pl4","impseed_pl5")], na.rm=T ) > 0


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










