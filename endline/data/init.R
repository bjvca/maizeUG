###This file does most of the preparations - it also loads functions that are used in subsequent analyses
### first run anonymize.R on raw data to remove identifiers

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
	library(data.table)
	cl <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
	registerDoParallel(cl)

### who recieved the video?
dta$recipient.y <- NULL
names(dta)[names(dta) == 'recipient.x'] <- 'recipient'
dta$messenger.y <- NULL
names(dta)[names(dta) == 'messenger.x'] <- 'messenger'
dta <- subset(dta,!is.na(recipient))

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
### missing code was 999 -> set to missing
dta[c("grp1a14","grp2b14","grp3c14","grp4d14", "grp5e14")] <- lapply(dta[c("grp1a14","grp2b14","grp3c14","grp4d14", "grp5e14")], function(x) replace(x, x == 999, NA) )
### for spouse 2
dta$spouse2grp_sp1f14[dta$spouse2grp_sp1f12 == "No"] <- 100
dta$spouse2grp_sp2g14[dta$spouse2grp_sp2g12 == "No"] <- 100
dta$spouse2grp_sp3h14[dta$spouse2grp_sp3h12 == "No"] <- 100
dta$spouse2group_sp4j14[dta$spouse2group_sp4j12 == "No"] <- 100
dta$spouse2grp5_sp5k14[dta$spouse2grp5_sp5k12 == "No"] <- 100
### missing code was 999 -> set to missing
dta[c("spouse2grp_sp1f14","spouse2grp_sp2g14","spouse2grp_sp3h14","spouse2group_sp4j14", "spouse2grp5_sp5k14")] <- lapply(dta[c("spouse2grp_sp1f14","spouse2grp_sp2g14","spouse2grp_sp3h14","spouse2group_sp4j14", "spouse2grp5_sp5k14")], function(x) replace(x,x == 999,NA) )

#production - bags x kg per bag - spouse 1
### missing code was 999 -> set to missing
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
## take mean of production reported by both spouses as total production
dta$prod_tot <-  rowMeans(dta[c("prod_tot_sp1","prod_tot_sp2")], na.rm=T)

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

dta$yield_av <- rowMeans(dta[c("yield_av_sp1","yield_av_sp2")], na.rm=T)

##################################### interlude: calc measures of plot management ##########################
###who manages plots?
dta$mgt_man_pl1 <-  rowSums(cbind((dta$grp1a10==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f10==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$mgt_man_pl1[is.na( (dta$grp1a10==1 ) ) & is.na((dta$spouse2grp_sp1f10==1 ) )] <- NA
dta$mgt_woman_pl1 <-  rowSums(cbind((dta$grp1a10==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1f10==1  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_woman_pl1[is.na((dta$grp1a10==1 ) ) & is.na((dta$spouse2grp_sp1f10==1 ))] <- NA
dta$mgt_woman_involved_pl1 <- rowSums(cbind((dta$grp1a10%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1f10%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_woman_involved_pl1[is.na((dta$grp1a10==1 ) ) & is.na((dta$spouse2grp_sp1f10==1  & dta$person_interviewed=="man"))] <- NA
##woman says it is both and man says it is both
dta$mgt_both_pl1 <- rowSums(cbind( (rowSums(cbind((dta$grp1a10==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp1f10==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp1a10==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f10==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$mgt_both_pl1[is.na((dta$grp1a10==3 )) & is.na((dta$spouse2grp_sp1f10==3 ))] <- NA

dta$mgt_man_pl2 <-  rowSums(cbind((dta$grp2b10==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g10==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$mgt_man_pl2[is.na( (dta$grp2b10==1 ) ) & is.na((dta$spouse2grp_sp2g10==1 ) )] <- NA
dta$mgt_woman_pl2 <-  rowSums(cbind((dta$grp2b10==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2g10==1  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_woman_pl2[is.na((dta$grp2b10==1 ) ) & is.na((dta$spouse2grp_sp2g10==1 ))] <- NA
dta$mgt_woman_involved_pl2 <-  rowSums(cbind((dta$grp2b10%in%c(1,3,5)   & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2g10%in%c(1,3,5)   & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_woman_involved_pl2[is.na((dta$grp2b10==1 ) ) & is.na((dta$spouse2grp_sp2g10==1 ))] <- NA
##woman says it is both and man says it is both
dta$mgt_both_pl2 <- rowSums(cbind( (rowSums(cbind((dta$grp2b10==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp2g10==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp2b10==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g10==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$mgt_both_pl2[is.na((dta$grp2b10==3 )) & is.na((dta$spouse2grp_sp2g10==3 ))] <- NA

dta$mgt_man_pl3 <-  rowSums(cbind((dta$grp3c10==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h10==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$mgt_man_pl3[is.na( (dta$grp3c10==1 ) ) & is.na((dta$spouse2grp_sp3h10==1 ) )] <- NA
dta$mgt_woman_pl3 <-  rowSums(cbind((dta$grp3c10==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3h10==1  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_woman_pl3[is.na((dta$grp3c10==1 ) ) & is.na((dta$spouse2grp_sp3h10==1 ))] <- NA
dta$mgt_woman_involved_pl3 <-  rowSums(cbind((dta$grp3c10%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3h10%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_woman_involved_pl3[is.na((dta$grp3c10==1 ) ) & is.na((dta$spouse2grp_sp3h10==1 ))] <- NA
##woman says it is both and man says it is both
dta$mgt_both_pl3 <- rowSums(cbind( (rowSums(cbind((dta$grp3c10==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp3h10==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp3c10==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h10==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$mgt_both_pl3[is.na((dta$grp3c10==3 )) & is.na((dta$spouse2grp_sp3h10==3 ))] <- NA

dta$mgt_man_pl4 <-  rowSums(cbind((dta$grp4d10==1  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j10==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$mgt_man_pl4[is.na( (dta$grp4d10==1 ) ) & is.na((dta$spouse2group_sp4j10==1 ) )] <- NA
dta$mgt_woman_pl4 <-  rowSums(cbind((dta$grp4d10==1  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4j10==1  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_woman_pl4[is.na((dta$grp4d10==1 ) ) & is.na((dta$spouse2group_sp4j10==1 ))] <- NA
dta$mgt_woman_involved_pl4 <-  rowSums(cbind((dta$grp4d10%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4j10%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_woman_invovled_pl4[is.na((dta$grp4d10==1 ) ) & is.na((dta$spouse2group_sp4j10==1 ))] <- NA
##woman says it is both and man says it is both
dta$mgt_both_pl4 <- rowSums(cbind( (rowSums(cbind((dta$grp4d10==3  & dta$person_interviewed=="woman") , (dta$spouse2group_sp4j10==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp4d10==3  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j10==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$mgt_both_pl4[is.na((dta$grp4d10==3 )) & is.na((dta$spouse2group_sp4j10==3 ))] <- NA

dta$mgt_man_pl5 <-  rowSums(cbind((dta$grp5e10==1  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k10==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$mgt_man_pl5[is.na( (dta$grp5e10==1 ) ) & is.na((dta$spouse2grp5_sp5k10==1 ) )] <- NA
dta$mgt_woman_pl5 <-  rowSums(cbind((dta$grp5e10==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5k10==1  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_woman_pl5[is.na((dta$grp5e10==1 ) ) & is.na((dta$spouse2grp5_sp5k10==1 ))] <- NA
dta$mgt_woman_invovled_pl5 <-  rowSums(cbind((dta$grp5e10%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5k10%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_woman_involved_pl5[is.na((dta$grp5e10==1 ) ) & is.na((dta$spouse2grp5_sp5k10==1 ))] <- NA
##woman says it is both and man says it is both
dta$mgt_both_pl5 <- rowSums(cbind( (rowSums(cbind((dta$grp5e10==3  & dta$person_interviewed=="woman") , (dta$spouse2grp5_sp5k10==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp5e10==3  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k10==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$mgt_both_pl5[is.na((dta$grp5e10==3 )) & is.na((dta$spouse2grp5_sp5k10==3 ))] <- NA

dta$nr_man_plots_decmaize <- rowSums(dta[c("mgt_man_pl1","mgt_man_pl2","mgt_man_pl3","mgt_man_pl4","mgt_man_pl5")], na.rm=T)
dta$nr_woman_plots_decmaize <- rowSums(dta[c("mgt_woman_pl1","mgt_woman_pl2","mgt_woman_pl3","mgt_woman_pl4","mgt_woman_pl5")], na.rm=T)
dta$nr_woman_involved_plots_decmaize <- rowSums(dta[c("mgt_woman_involved_pl1","mgt_woman_involved_pl2","mgt_woman_involved_pl3","mgt_woman_involved_pl4","mgt_woman_involved_pl5")], na.rm=T)
dta$nr_joint_plots_decmaize <- rowSums(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")], na.rm=T)

########################################################################################
###who decided to start planting maize at particular time (decide1)
dta$dectime_man_pl1 <-  rowSums(cbind((dta$grp1decide1==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1decide_sp1==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$dectime_man_pl1[is.na( (dta$grp1decide1==1 ) ) & is.na((dta$spouse2grp_sp1decide_sp1==1 ) )] <- NA
dta$dectime_woman_pl1 <-  rowSums(cbind((dta$grp1decide1==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1decide_sp1==1  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_woman_pl1[is.na((dta$grp1decide1==1 ) ) & is.na((dta$spouse2grp_sp1decide_sp1==1 ))] <- NA
dta$dectime_woman_involved_pl1 <- rowSums(cbind((dta$grp1decide1%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1decide_sp1%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_woman_involved_pl1[is.na((dta$grp1decide1==1 ) ) & is.na((dta$spouse2grp_sp1decide_sp1==1 ))] <- NA
##woman says it is both and man says it is both
dta$dectime_both_pl1 <- rowSums(cbind( (rowSums(cbind((dta$grp1decide1==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp1decide_sp1==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp1decide1==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1decide_sp1==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$dectime_both_pl1[is.na((dta$grp1decide1==3 )) & is.na((dta$spouse2grp_sp1decide_sp1==3 ))] <- NA

dta$dectime_man_pl2 <-  rowSums(cbind((dta$grp2decide2==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2decide_sp2==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$dectime_man_pl2[is.na( (dta$grp2decide2==1 ) ) & is.na((dta$spouse2grp_sp2decide_sp2==1 ) )] <- NA
dta$dectime_woman_pl2 <-  rowSums(cbind((dta$grp2decide2==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2decide_sp2==1  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_woman_pl2[is.na((dta$grp2decide2==1 ) ) & is.na((dta$spouse2grp_sp2decide_sp2==1 ))] <- NA
dta$dectime_woman_involved_pl2 <-  rowSums(cbind((dta$grp2decide2%in%c(1,3,5)   & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2decide_sp2%in%c(1,3,5)   & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_woman_involved_pl2[is.na((dta$grp2decide2==1 ) ) & is.na((dta$spouse2grp_sp2decide_sp2==1 ))] <- NA
##woman says it is both and man says it is both
dta$dectime_both_pl2 <- rowSums(cbind( (rowSums(cbind((dta$grp2decide2==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp2decide_sp2==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp2decide2==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2decide_sp2==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$dectime_both_pl2[is.na((dta$grp2decide2==3 )) & is.na((dta$spouse2grp_sp2decide_sp2==3 ))] <- NA

dta$dectime_man_pl3 <-  rowSums(cbind((dta$grp3decide3==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3days_sp3==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$dectime_man_pl3[is.na( (dta$grp3decide3==1 ) ) & is.na((dta$spouse2grp_sp3days_sp3==1 ) )] <- NA
dta$dectime_woman_pl3 <-  rowSums(cbind((dta$grp3decide3==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3days_sp3==1  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_woman_pl3[is.na((dta$grp3decide3==1 ) ) & is.na((dta$spouse2grp_sp3days_sp3==1 ))] <- NA
dta$dectime_woman_involved_pl3 <-  rowSums(cbind((dta$grp3decide3%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3days_sp3%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_woman_involved_pl3[is.na((dta$grp3decide3==1 ) ) & is.na((dta$spouse2grp_sp3days_sp3==1 ))] <- NA
##woman says it is both and man says it is both
dta$dectime_both_pl3 <- rowSums(cbind( (rowSums(cbind((dta$grp3decide3==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp3days_sp3==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp3decide3==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3days_sp3==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$dectime_both_pl3[is.na((dta$grp3decide3==3 )) & is.na((dta$spouse2grp_sp3days_sp3==3 ))] <- NA

dta$dectime_man_pl4 <-  rowSums(cbind((dta$grp4decide4==1  & dta$person_interviewed=="man") , (dta$spouse2group_sp4dayssp4==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$dectime_man_pl4[is.na( (dta$grp4decide4==1 ) ) & is.na((dta$spouse2group_sp4dayssp4==1 ) )] <- NA
dta$dectime_woman_pl4 <-  rowSums(cbind((dta$grp4decide4==1  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4dayssp4==1  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_woman_pl4[is.na((dta$grp4decide4==1 ) ) & is.na((dta$spouse2group_sp4dayssp4==1 ))] <- NA
dta$dectime_woman_involved_pl4 <-  rowSums(cbind((dta$grp4decide4%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4dayssp4%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_woman_invovled_pl4[is.na((dta$grp4decide4==1 ) ) & is.na((dta$spouse2group_sp4dayssp4==1 ))] <- NA
##woman says it is both and man says it is both
dta$dectime_both_pl4 <- rowSums(cbind( (rowSums(cbind((dta$grp4decide4==3  & dta$person_interviewed=="woman") , (dta$spouse2group_sp4dayssp4==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp4decide4==3  & dta$person_interviewed=="man") , (dta$spouse2group_sp4dayssp4==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$dectime_both_pl4[is.na((dta$grp4decide4==3 )) & is.na((dta$spouse2group_sp4dayssp4==3 ))] <- NA

dta$dectime_man_pl5 <-  rowSums(cbind((dta$grp5decide5==1  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5dayssp5==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$dectime_man_pl5[is.na( (dta$grp5decide5==1 ) ) & is.na((dta$spouse2grp5_sp5dayssp5==1 ) )] <- NA
dta$dectime_woman_pl5 <-  rowSums(cbind((dta$grp5decide5==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5dayssp5==1  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_woman_pl5[is.na((dta$grp5decide5==1 ) ) & is.na((dta$spouse2grp5_sp5dayssp5==1 ))] <- NA
dta$dectime_woman_invovled_pl5 <-  rowSums(cbind((dta$grp5decide5%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5dayssp5%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_woman_involved_pl5[is.na((dta$grp5decide5==1 ) ) & is.na((dta$spouse2grp5_sp5dayssp5==1 ))] <- NA
##woman says it is both and man says it is both
dta$dectime_both_pl5 <- rowSums(cbind( (rowSums(cbind((dta$grp5decide5==3  & dta$person_interviewed=="woman") , (dta$spouse2grp5_sp5dayssp5==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp5decide5==3  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5dayssp5==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$dectime_both_pl5[is.na((dta$grp5decide5==3 )) & is.na((dta$spouse2grp5_sp5dayssp5==3 ))] <- NA

dta$nr_man_plots_dectime <- rowSums(dta[c("dectime_man_pl1","dectime_man_pl2","dectime_man_pl3","dectime_man_pl4","dectime_man_pl5")], na.rm=T)
dta$nr_woman_plots_dectime <- rowSums(dta[c("dectime_woman_pl1","dectime_woman_pl2","dectime_woman_pl3","dectime_woman_pl4","dectime_woman_pl5")], na.rm=T)
dta$nr_woman_involved_plots_dectime <- rowSums(dta[c("dectime_woman_involved_pl1","dectime_woman_involved_pl2","dectime_woman_involved_pl3","dectime_woman_involved_pl4","dectime_woman_involved_pl5")], na.rm=T)
dta$nr_joint_plots_dectime <- rowSums(dta[c("dectime_both_pl1","dectime_both_pl2","dectime_both_pl3","dectime_both_pl4","dectime_both_pl5")], na.rm=T)

########################################################################################
###who decides on spacing

###who decides on spacing

dta$decspace_man_pl1 <-  rowSums(cbind((dta$grp1a23==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f23==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decspace_man_pl1[is.na( (dta$grp1a23==1 ) ) & is.na((dta$spouse2grp_sp1f23==1 ) )] <- NA
dta$decspace_woman_pl1 <-  rowSums(cbind((dta$grp1a23==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1f23==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_woman_pl1[is.na((dta$grp1a23==1 ) ) & is.na((dta$spouse2grp_sp1f23==1 ))] <- NA
dta$decspace_woman_involved_pl1 <- rowSums(cbind((dta$grp1a23%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1f23%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_woman_involved_pl1[is.na((dta$grp1a23==1 ) ) & is.na((dta$spouse2grp_sp1f23==1 ))] <- NA
##woman says it is both and man says it is both
dta$decspace_both_pl1 <- rowSums(cbind( (rowSums(cbind((dta$grp1a23==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp1f23==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp1a23==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f23==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decspace_both_pl1[is.na((dta$grp1a23==3 )) & is.na((dta$spouse2grp_sp1f23==3 ))] <- NA

dta$decspace_man_pl2 <-  rowSums(cbind((dta$grp2b23==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g23==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decspace_man_pl2[is.na( (dta$grp2b23==1 ) ) & is.na((dta$spouse2grp_sp2g23==1 ) )] <- NA
dta$decspace_woman_pl2 <-  rowSums(cbind((dta$grp2b23==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2g23==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_woman_pl2[is.na((dta$grp2b23==1 ) ) & is.na((dta$spouse2grp_sp2g23==1 ))] <- NA
dta$decspace_woman_involved_pl2 <-  rowSums(cbind((dta$grp2b23%in%c(1,3,5)   & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2g23%in%c(1,3,5)   & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_woman_involved_pl2[is.na((dta$grp2b23==1 ) ) & is.na((dta$spouse2grp_sp2g23==1 ))] <- NA
##woman says it is both and man says it is both
dta$decspace_both_pl2 <- rowSums(cbind( (rowSums(cbind((dta$grp2b23==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp2g23==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp2b23==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g23==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decspace_both_pl2[is.na((dta$grp2b23==3 )) & is.na((dta$spouse2grp_sp2g23==3 ))] <- NA

dta$decspace_man_pl3 <-  rowSums(cbind((dta$grp3c23==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h23==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decspace_man_pl3[is.na( (dta$grp3c23==1 ) ) & is.na((dta$spouse2grp_sp3h23==1 ) )] <- NA
dta$decspace_woman_pl3 <-  rowSums(cbind((dta$grp3c23==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3h23==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_woman_pl3[is.na((dta$grp3c23==1 ) ) & is.na((dta$spouse2grp_sp3h23==1 ))] <- NA
dta$decspace_woman_involved_pl3 <-  rowSums(cbind((dta$grp3c23%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3h23%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_woman_involved_pl3[is.na((dta$grp3c23==1 ) ) & is.na((dta$spouse2grp_sp3h23==1 ))] <- NA
##woman says it is both and man says it is both
dta$decspace_both_pl3 <- rowSums(cbind( (rowSums(cbind((dta$grp3c23==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp3h23==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp3c23==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h23==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decspace_both_pl3[is.na((dta$grp3c23==3 )) & is.na((dta$spouse2grp_sp3h23==3 ))] <- NA

dta$decspace_man_pl4 <-  rowSums(cbind((dta$grp4d23==1  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j23==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decspace_man_pl4[is.na( (dta$grp4d23==1 ) ) & is.na((dta$spouse2group_sp4j23==1 ) )] <- NA
dta$decspace_woman_pl4 <-  rowSums(cbind((dta$grp4d23==1  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4j23==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_woman_pl4[is.na((dta$grp4d23==1 ) ) & is.na((dta$spouse2group_sp4j23==1 ))] <- NA
dta$decspace_woman_involved_pl4 <-  rowSums(cbind((dta$grp4d23%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4j23%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_woman_invovled_pl4[is.na((dta$grp4d23==1 ) ) & is.na((dta$spouse2group_sp4j23==1 ))] <- NA
##woman says it is both and man says it is both
dta$decspace_both_pl4 <- rowSums(cbind( (rowSums(cbind((dta$grp4d23==3  & dta$person_interviewed=="woman") , (dta$spouse2group_sp4j23==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp4d23==3  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j23==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decspace_both_pl4[is.na((dta$grp4d23==3 )) & is.na((dta$spouse2group_sp4j23==3 ))] <- NA

dta$decspace_man_pl5 <-  rowSums(cbind((dta$grp5e23==1  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k23==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decspace_man_pl5[is.na( (dta$grp5e23==1 ) ) & is.na((dta$spouse2grp5_sp5k23==1 ) )] <- NA
dta$decspace_woman_pl5 <-  rowSums(cbind((dta$grp5e23==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5k23==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_woman_pl5[is.na((dta$grp5e23==1 ) ) & is.na((dta$spouse2grp5_sp5k23==1 ))] <- NA
dta$decspace_woman_invovled_pl5 <-  rowSums(cbind((dta$grp5e23%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5k23%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_woman_involved_pl5[is.na((dta$grp5e23==1 ) ) & is.na((dta$spouse2grp5_sp5k23==1 ))] <- NA
##woman says it is both and man says it is both
dta$decspace_both_pl5 <- rowSums(cbind( (rowSums(cbind((dta$grp5e23==3  & dta$person_interviewed=="woman") , (dta$spouse2grp5_sp5k23==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp5e23==3  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k23==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decspace_both_pl5[is.na((dta$grp5e23==3 )) & is.na((dta$spouse2grp5_sp5k23==3 ))] <- NA

dta$nr_man_plots_decspace <- rowSums(dta[c("decspace_man_pl1","decspace_man_pl2","decspace_man_pl3","decspace_man_pl4","decspace_man_pl5")], na.rm=T)
dta$nr_woman_plots_decspace <- rowSums(dta[c("decspace_woman_pl1","decspace_woman_pl2","decspace_woman_pl3","decspace_woman_pl4","decspace_woman_pl5")], na.rm=T)
dta$nr_woman_involved_plots_decspace <- rowSums(dta[c("decspace_woman_involved_pl1","decspace_woman_involved_pl2","decspace_woman_involved_pl3","decspace_woman_involved_pl4","decspace_woman_involved_pl5")], na.rm=T)
dta$nr_joint_plots_decspace <- rowSums(dta[c("decspace_both_pl1","decspace_both_pl2","decspace_both_pl3","decspace_both_pl4","decspace_both_pl5")], na.rm=T)
############################

###who decides on striga

dta$decstriga_man_pl1 <-  rowSums(cbind((dta$grp1a25==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f25==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decstriga_man_pl1[is.na( (dta$grp1a25==1 ) ) & is.na((dta$spouse2grp_sp1f25==1 ) )] <- NA
dta$decstriga_woman_pl1 <-  rowSums(cbind((dta$grp1a25==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1f25==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_woman_pl1[is.na((dta$grp1a25==1 ) ) & is.na((dta$spouse2grp_sp1f25==1 ))] <- NA
dta$decstriga_woman_involved_pl1 <- rowSums(cbind((dta$grp1a25%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1f25%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_woman_involved_pl1[is.na((dta$grp1a25==1 ) ) & is.na((dta$spouse2grp_sp1f25==1 ))] <- NA
##woman says it is both and man says it is both
dta$decstriga_both_pl1 <- rowSums(cbind( (rowSums(cbind((dta$grp1a25==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp1f25==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp1a25==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f25==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decstriga_both_pl1[is.na((dta$grp1a25==3 )) & is.na((dta$spouse2grp_sp1f25==3 ))] <- NA

dta$decstriga_man_pl2 <-  rowSums(cbind((dta$grp2b25==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g25==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decstriga_man_pl2[is.na( (dta$grp2b25==1 ) ) & is.na((dta$spouse2grp_sp2g25==1 ) )] <- NA
dta$decstriga_woman_pl2 <-  rowSums(cbind((dta$grp2b25==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2g25==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_woman_pl2[is.na((dta$grp2b25==1 ) ) & is.na((dta$spouse2grp_sp2g25==1 ))] <- NA
dta$decstriga_woman_involved_pl2 <-  rowSums(cbind((dta$grp2b25%in%c(1,3,5)   & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2g25%in%c(1,3,5)   & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_woman_involved_pl2[is.na((dta$grp2b25==1 ) ) & is.na((dta$spouse2grp_sp2g25==1 ))] <- NA
##woman says it is both and man says it is both
dta$decstriga_both_pl2 <- rowSums(cbind( (rowSums(cbind((dta$grp2b25==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp2g25==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp2b25==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g25==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decstriga_both_pl2[is.na((dta$grp2b25==3 )) & is.na((dta$spouse2grp_sp2g25==3 ))] <- NA

dta$decstriga_man_pl3 <-  rowSums(cbind((dta$grp3c25==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h25==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decstriga_man_pl3[is.na( (dta$grp3c25==1 ) ) & is.na((dta$spouse2grp_sp3h25==1 ) )] <- NA
dta$decstriga_woman_pl3 <-  rowSums(cbind((dta$grp3c25==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3h25==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_woman_pl3[is.na((dta$grp3c25==1 ) ) & is.na((dta$spouse2grp_sp3h25==1 ))] <- NA
dta$decstriga_woman_involved_pl3 <-  rowSums(cbind((dta$grp3c25%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3h25%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_woman_involved_pl3[is.na((dta$grp3c25==1 ) ) & is.na((dta$spouse2grp_sp3h25==1 ))] <- NA
##woman says it is both and man says it is both
dta$decstriga_both_pl3 <- rowSums(cbind( (rowSums(cbind((dta$grp3c25==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp3h25==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp3c25==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h25==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decstriga_both_pl3[is.na((dta$grp3c25==3 )) & is.na((dta$spouse2grp_sp3h25==3 ))] <- NA

dta$decstriga_man_pl4 <-  rowSums(cbind((dta$grp4d25==1  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j25==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decstriga_man_pl4[is.na( (dta$grp4d25==1 ) ) & is.na((dta$spouse2group_sp4j25==1 ) )] <- NA
dta$decstriga_woman_pl4 <-  rowSums(cbind((dta$grp4d25==1  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4j25==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_woman_pl4[is.na((dta$grp4d25==1 ) ) & is.na((dta$spouse2group_sp4j25==1 ))] <- NA
dta$decstriga_woman_involved_pl4 <-  rowSums(cbind((dta$grp4d25%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4j25%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_woman_invovled_pl4[is.na((dta$grp4d25==1 ) ) & is.na((dta$spouse2group_sp4j25==1 ))] <- NA
##woman says it is both and man says it is both
dta$decstriga_both_pl4 <- rowSums(cbind( (rowSums(cbind((dta$grp4d25==3  & dta$person_interviewed=="woman") , (dta$spouse2group_sp4j25==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp4d25==3  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j25==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decstriga_both_pl4[is.na((dta$grp4d25==3 )) & is.na((dta$spouse2group_sp4j25==3 ))] <- NA

dta$decstriga_man_pl5 <-  rowSums(cbind((dta$grp5e25==1  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k25==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decstriga_man_pl5[is.na( (dta$grp5e25==1 ) ) & is.na((dta$spouse2grp5_sp5k25==1 ) )] <- NA
dta$decstriga_woman_pl5 <-  rowSums(cbind((dta$grp5e25==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5k25==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_woman_pl5[is.na((dta$grp5e25==1 ) ) & is.na((dta$spouse2grp5_sp5k25==1 ))] <- NA
dta$decstriga_woman_invovled_pl5 <-  rowSums(cbind((dta$grp5e25%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5k25%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_woman_involved_pl5[is.na((dta$grp5e25==1 ) ) & is.na((dta$spouse2grp5_sp5k25==1 ))] <- NA
##woman says it is both and man says it is both
dta$decstriga_both_pl5 <- rowSums(cbind( (rowSums(cbind((dta$grp5e25==3  & dta$person_interviewed=="woman") , (dta$spouse2grp5_sp5k25==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp5e25==3  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k25==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decstriga_both_pl5[is.na((dta$grp5e25==3 )) & is.na((dta$spouse2grp5_sp5k25==3 ))] <- NA

dta$nr_man_plots_decstriga <- rowSums(dta[c("decstriga_man_pl1","decstriga_man_pl2","decstriga_man_pl3","decstriga_man_pl4","decstriga_man_pl5")], na.rm=T)
dta$nr_woman_plots_decstriga <- rowSums(dta[c("decstriga_woman_pl1","decstriga_woman_pl2","decstriga_woman_pl3","decstriga_woman_pl4","decstriga_woman_pl5")], na.rm=T)
dta$nr_woman_involved_plots_decstriga <- rowSums(dta[c("decstriga_woman_involved_pl1","decstriga_woman_involved_pl2","decstriga_woman_involved_pl3","decstriga_woman_involved_pl4","decstriga_woman_involved_pl5")], na.rm=T)
dta$nr_joint_plots_decstriga <- rowSums(dta[c("decstriga_both_pl1","decstriga_both_pl2","decstriga_both_pl3","decstriga_both_pl4","decstriga_both_pl5")], na.rm=T)

##########################################

###who decides on weed

dta$decweed_man_pl1 <-  rowSums(cbind((dta$grp1a27==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f27==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decweed_man_pl1[is.na( (dta$grp1a27==1 ) ) & is.na((dta$spouse2grp_sp1f27==1 ) )] <- NA
dta$decweed_woman_pl1 <-  rowSums(cbind((dta$grp1a27==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1f27==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_woman_pl1[is.na((dta$grp1a27==1 ) ) & is.na((dta$spouse2grp_sp1f27==1 ))] <- NA
dta$decweed_woman_involved_pl1 <- rowSums(cbind((dta$grp1a27%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1f27%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_woman_involved_pl1[is.na((dta$grp1a27==1 ) ) & is.na((dta$spouse2grp_sp1f27==1 ))] <- NA
##woman says it is both and man says it is both
dta$decweed_both_pl1 <- rowSums(cbind( (rowSums(cbind((dta$grp1a27==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp1f27==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp1a27==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f27==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decweed_both_pl1[is.na((dta$grp1a27==3 )) & is.na((dta$spouse2grp_sp1f27==3 ))] <- NA

dta$decweed_man_pl2 <-  rowSums(cbind((dta$grp2b27==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g27==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decweed_man_pl2[is.na( (dta$grp2b27==1 ) ) & is.na((dta$spouse2grp_sp2g27==1 ) )] <- NA
dta$decweed_woman_pl2 <-  rowSums(cbind((dta$grp2b27==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2g27==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_woman_pl2[is.na((dta$grp2b27==1 ) ) & is.na((dta$spouse2grp_sp2g27==1 ))] <- NA
dta$decweed_woman_involved_pl2 <-  rowSums(cbind((dta$grp2b27%in%c(1,3,5)   & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2g27%in%c(1,3,5)   & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_woman_involved_pl2[is.na((dta$grp2b27==1 ) ) & is.na((dta$spouse2grp_sp2g27==1 ))] <- NA
##woman says it is both and man says it is both
dta$decweed_both_pl2 <- rowSums(cbind( (rowSums(cbind((dta$grp2b27==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp2g27==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp2b27==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g27==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decweed_both_pl2[is.na((dta$grp2b27==3 )) & is.na((dta$spouse2grp_sp2g27==3 ))] <- NA

dta$decweed_man_pl3 <-  rowSums(cbind((dta$grp3c27==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h27==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decweed_man_pl3[is.na( (dta$grp3c27==1 ) ) & is.na((dta$spouse2grp_sp3h27==1 ) )] <- NA
dta$decweed_woman_pl3 <-  rowSums(cbind((dta$grp3c27==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3h27==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_woman_pl3[is.na((dta$grp3c27==1 ) ) & is.na((dta$spouse2grp_sp3h27==1 ))] <- NA
dta$decweed_woman_involved_pl3 <-  rowSums(cbind((dta$grp3c27%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3h27%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_woman_involved_pl3[is.na((dta$grp3c27==1 ) ) & is.na((dta$spouse2grp_sp3h27==1 ))] <- NA
##woman says it is both and man says it is both
dta$decweed_both_pl3 <- rowSums(cbind( (rowSums(cbind((dta$grp3c27==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp3h27==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp3c27==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h27==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decweed_both_pl3[is.na((dta$grp3c27==3 )) & is.na((dta$spouse2grp_sp3h27==3 ))] <- NA

dta$decweed_man_pl4 <-  rowSums(cbind((dta$grp4d27==1  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j27==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decweed_man_pl4[is.na( (dta$grp4d27==1 ) ) & is.na((dta$spouse2group_sp4j27==1 ) )] <- NA
dta$decweed_woman_pl4 <-  rowSums(cbind((dta$grp4d27==1 ) ,  (dta$spouse2group_sp4j27==1 )), na.rm=T)
dta$decweed_woman_pl4[is.na((dta$grp4d27==1) ) & is.na((dta$spouse2group_sp4j27==1  ))] <- NA
dta$decweed_woman_involved_pl4 <-  rowSums(cbind((dta$grp4d27%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4j27%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_woman_invovled_pl4[is.na((dta$grp4d27==1 ) ) & is.na((dta$spouse2group_sp4j27==1 ))] <- NA
##woman says it is both and man says it is both
dta$decweed_both_pl4 <- rowSums(cbind( (rowSums(cbind((dta$grp4d27==3  & dta$person_interviewed=="woman") , (dta$spouse2group_sp4j27==3 )), na.rm=T) > 0),(rowSums(cbind((dta$grp4d27==3  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j27==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decweed_both_pl4[is.na((dta$grp4d27==3 )) & is.na((dta$spouse2group_sp4j27==3))] <- NA

dta$decweed_man_pl5 <-  rowSums(cbind((dta$grp5e27==1 ) , (dta$spouse2grp5_sp5k27==1 )), na.rm=T)
dta$decweed_man_pl5[is.na( (dta$grp5e27==1 ) ) & is.na((dta$spouse2grp5_sp5k27==1) )] <- NA
dta$decweed_woman_pl5 <-  rowSums(cbind((dta$grp5e27==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5k27==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_woman_pl5[is.na((dta$grp5e27==1 ) ) & is.na((dta$spouse2grp5_sp5k27==1 ))] <- NA
dta$decweed_woman_invovled_pl5 <-  rowSums(cbind((dta$grp5e27%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5k27%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_woman_involved_pl5[is.na((dta$grp5e27==1 ) ) & is.na((dta$spouse2grp5_sp5k27==1 ))] <- NA
##woman says it is both and man says it is both
dta$decweed_both_pl5 <- rowSums(cbind( (rowSums(cbind((dta$grp5e27==3  & dta$person_interviewed=="woman") , (dta$spouse2grp5_sp5k27==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp5e27==3  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k27==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decweed_both_pl5[is.na((dta$grp5e27==3)) & is.na((dta$spouse2grp5_sp5k27==3))] <- NA

dta$nr_man_plots_decweed <- rowSums(dta[c("decweed_man_pl1","decweed_man_pl2","decweed_man_pl3","decweed_man_pl4","decweed_man_pl5")], na.rm=T)
dta$nr_woman_plots_decweed <- rowSums(dta[c("decweed_woman_pl1","decweed_woman_pl2","decweed_woman_pl3","decweed_woman_pl4","decweed_woman_pl5")], na.rm=T)
dta$nr_woman_involved_plots_decweed <- rowSums(dta[c("decweed_woman_involved_pl1","decweed_woman_involved_pl2","decweed_woman_involved_pl3","decweed_woman_involved_pl4","decweed_woman_involved_pl5")], na.rm=T)
dta$nr_joint_plots_decweed <- rowSums(dta[c("decweed_both_pl1","decweed_both_pl2","decweed_both_pl3","decweed_both_pl4","decweed_both_pl5")], na.rm=T)

dta$dec_man_pl1 <- rowMeans(dta[c("mgt_man_pl1", "dectime_man_pl1", "decspace_man_pl1", "decstriga_man_pl1", "decweed_man_pl1")],na.rm=T)
dta$dec_man_pl1[is.na(dta$mgt_man_pl1) & is.na(dta$dectime_man_pl1) & is.na(dta$decspace_man_pl1) & is.na(dta$decstriga_man_pl1) & is.na(dta$decweed_man_pl1)] <- NA
dta$dec_man_pl2 <- rowMeans(dta[c("mgt_man_pl2", "dectime_man_pl2", "decspace_man_pl2", "decstriga_man_pl2", "decweed_man_pl2")],na.rm=T)
dta$dec_man_pl2[is.na(dta$mgt_man_pl2) & is.na(dta$dectime_man_pl2) & is.na(dta$decspace_man_pl2) & is.na(dta$decstriga_man_pl2) & is.na(dta$decweed_man_pl2)] <- NA
dta$dec_man_pl3 <- rowMeans(dta[c("mgt_man_pl3", "dectime_man_pl3", "decspace_man_pl3", "decstriga_man_pl3", "decweed_man_pl3")],na.rm=T)
dta$dec_man_pl3[is.na(dta$mgt_man_pl3) & is.na(dta$dectime_man_pl3) & is.na(dta$decspace_man_pl3) & is.na(dta$decstriga_man_pl3) & is.na(dta$decweed_man_pl3)] <- NA
dta$dec_man_pl4 <- rowMeans(dta[c("mgt_man_pl4", "dectime_man_pl4", "decspace_man_pl4", "decstriga_man_pl4", "decweed_man_pl4")],na.rm=T)
dta$dec_man_pl4[is.na(dta$mgt_man_pl4) & is.na(dta$dectime_man_pl4) & is.na(dta$decspace_man_pl4) & is.na(dta$decstriga_man_pl4) & is.na(dta$decweed_man_pl4)] <- NA
dta$dec_man_pl5 <- rowMeans(dta[c("mgt_man_pl5", "dectime_man_pl5", "decspace_man_pl5", "decstriga_man_pl5", "decweed_man_pl5")],na.rm=T)
dta$dec_man_pl5[is.na(dta$mgt_man_pl5) & is.na(dta$dectime_man_pl5) & is.na(dta$decspace_man_pl5) & is.na(dta$decstriga_man_pl5) & is.na(dta$decweed_man_pl5)] <- NA

dta$dec_woman_pl1 <- rowMeans(dta[c("mgt_woman_pl1", "dectime_woman_pl1", "decspace_woman_pl1", "decstriga_woman_pl1", "decweed_woman_pl1")],na.rm=T)
dta$dec_woman_pl1[is.na(dta$mgt_woman_pl1) & is.na(dta$dectime_woman_pl1) & is.na(dta$decspace_woman_pl1) & is.na(dta$decstriga_woman_pl1) & is.na(dta$decweed_woman_pl1)] <- NA
dta$dec_woman_pl2 <- rowMeans(dta[c("mgt_woman_pl2", "dectime_woman_pl2", "decspace_woman_pl2", "decstriga_woman_pl2", "decweed_woman_pl2")],na.rm=T)
dta$dec_woman_pl2[is.na(dta$mgt_woman_pl2) & is.na(dta$dectime_woman_pl2) & is.na(dta$decspace_woman_pl2) & is.na(dta$decstriga_woman_pl2) & is.na(dta$decweed_woman_pl2)] <- NA
dta$dec_woman_pl3 <- rowMeans(dta[c("mgt_woman_pl3", "dectime_woman_pl3", "decspace_woman_pl3", "decstriga_woman_pl3", "decweed_woman_pl3")],na.rm=T)
dta$dec_woman_pl3[is.na(dta$mgt_woman_pl3) & is.na(dta$dectime_woman_pl3) & is.na(dta$decspace_woman_pl3) & is.na(dta$decstriga_woman_pl3) & is.na(dta$decweed_woman_pl3)] <- NA
dta$dec_woman_pl4 <- rowMeans(dta[c("mgt_woman_pl4", "dectime_woman_pl4", "decspace_woman_pl4", "decstriga_woman_pl4", "decweed_woman_pl4")],na.rm=T)
dta$dec_woman_pl4[is.na(dta$mgt_woman_pl4) & is.na(dta$dectime_woman_pl4) & is.na(dta$decspace_woman_pl4) & is.na(dta$decstriga_woman_pl4) & is.na(dta$decweed_woman_pl4)] <- NA
dta$dec_woman_pl5 <- rowMeans(dta[c("mgt_woman_pl5", "dectime_woman_pl5", "decspace_woman_pl5", "decstriga_woman_pl5", "decweed_woman_pl5")],na.rm=T)
dta$dec_woman_pl5[is.na(dta$mgt_woman_pl5) & is.na(dta$dectime_woman_pl5) & is.na(dta$decspace_woman_pl5) & is.na(dta$decstriga_woman_pl5) & is.na(dta$decweed_woman_pl5)] <- NA

dta$dec_woman_involved_pl1 <- rowMeans(dta[c("mgt_woman_involved_pl1", "dectime_woman_involved_pl1", "decspace_woman_involved_pl1", "decstriga_woman_involved_pl1", "decweed_woman_involved_pl1")],na.rm=T)
dta$dec_woman_involved_pl2 <- rowMeans(dta[c("mgt_woman_involved_pl2", "dectime_woman_involved_pl2", "decspace_woman_involved_pl2", "decstriga_woman_involved_pl2", "decweed_woman_involved_pl2")],na.rm=T)
dta$dec_woman_involved_pl3 <- rowMeans(dta[c("mgt_woman_involved_pl3", "dectime_woman_involved_pl3", "decspace_woman_involved_pl3", "decstriga_woman_involved_pl3", "decweed_woman_involved_pl3")],na.rm=T)
dta$dec_woman_involved_pl4 <- rowMeans(dta[c("mgt_woman_involved_pl4", "dectime_woman_involved_pl4", "decspace_woman_involved_pl4", "decstriga_woman_involved_pl4", "decweed_woman_involved_pl4")],na.rm=T)
dta$dec_woman_involved_pl5 <- rowMeans(dta[c("mgt_woman_involved_pl5", "dectime_woman_involved_pl5", "decspace_woman_involved_pl5", "decstriga_woman_involved_pl5", "decweed_woman_involved_pl5")],na.rm=T)

dta$dec_both_pl1 <- rowMeans(dta[c("mgt_both_pl1", "dectime_both_pl1", "decspace_both_pl1", "decstriga_both_pl1", "decweed_both_pl1")],na.rm=T)
dta$dec_both_pl1[is.na(dta$mgt_both_pl1) & is.na(dta$dectime_both_pl1) & is.na(dta$decspace_both_pl1) & is.na(dta$decstriga_both_pl1) & is.na(dta$decweed_both_pl1)] <- NA
dta$dec_both_pl2 <- rowMeans(dta[c("mgt_both_pl2", "dectime_both_pl2", "decspace_both_pl2", "decstriga_both_pl2", "decweed_both_pl2")],na.rm=T)
dta$dec_both_pl2[is.na(dta$mgt_both_pl2) & is.na(dta$dectime_both_pl2) & is.na(dta$decspace_both_pl2) & is.na(dta$decstriga_both_pl2) & is.na(dta$decweed_both_pl2)] <- NA
dta$dec_both_pl3 <- rowMeans(dta[c("mgt_both_pl3", "dectime_both_pl3", "decspace_both_pl3", "decstriga_both_pl3", "decweed_both_pl3")],na.rm=T)
dta$dec_both_pl3[is.na(dta$mgt_both_pl3) & is.na(dta$dectime_both_pl3) & is.na(dta$decspace_both_pl3) & is.na(dta$decstriga_both_pl3) & is.na(dta$decweed_both_pl3)] <- NA
dta$dec_both_pl4 <- rowMeans(dta[c("mgt_both_pl4", "dectime_both_pl4", "decspace_both_pl4", "decstriga_both_pl4", "decweed_both_pl4")],na.rm=T)
dta$dec_both_pl4[is.na(dta$mgt_both_pl4) & is.na(dta$dectime_both_pl4) & is.na(dta$decspace_both_pl4) & is.na(dta$decstriga_both_pl4) & is.na(dta$decweed_both_pl4)] <- NA
dta$dec_both_pl5 <- rowMeans(dta[c("mgt_both_pl5", "dectime_both_pl5", "decspace_both_pl5", "decstriga_both_pl5", "decweed_both_pl5")],na.rm=T)
dta$dec_both_pl5[is.na(dta$mgt_both_pl5) & is.na(dta$dectime_both_pl5) & is.na(dta$decspace_both_pl5) & is.na(dta$decstriga_both_pl5) & is.na(dta$decweed_both_pl5)] <- NA

### as dummy

dta[paste("dec_man_d",paste("_pl",1:5, sep=""), sep="")] <- dta[paste("dec_man",paste("_pl",1:5, sep=""), sep="")] > 0
dta[paste("dec_woman_d",paste("_pl",1:5, sep=""), sep="")] <- dta[paste("dec_woman",paste("_pl",1:5, sep=""), sep="")] > 0
dta[paste("dec_both_d",paste("_pl",1:5, sep=""), sep="")] <- dta[paste("dec_both",paste("_pl",1:5, sep=""), sep="")] > 0

### now engagement based on work 
dta[c("grp1field1a50a", "grp1field1a50b", "grp1field1a50c", "grp1field1a50d", "grp1field1a50e", "grp1field1a50f", "grp1field1a50g")] <- lapply(dta[c("grp1field1a50a", "grp1field1a50b", "grp1field1a50c", "grp1field1a50d", "grp1field1a50e", "grp1field1a50f", "grp1field1a50g")], function(x) replace(x, x == 999, NA) )
dta$time_pl1_sp1 <- rowSums(dta[c("grp1field1a50a", "grp1field1a50b", "grp1field1a50c", "grp1field1a50d", "grp1field1a50e", "grp1field1a50f", "grp1field1a50g")], na.rm=T)
dta$time_pl1_sp1[rowSums(is.na(dta[c("grp1field1a50a", "grp1field1a50b", "grp1field1a50c", "grp1field1a50d", "grp1field1a50e", "grp1field1a50f", "grp1field1a50g")]))==7] <- NA

dta[c("grp2field3b50a", "grp2field3b50b", "grp2field3b50c", "grp2field3b50d", "grp2field3b50e", "grp2field3b50f", "grp2field3b50g")] <- lapply(dta[c("grp2field3b50a", "grp2field3b50b", "grp2field3b50c", "grp2field3b50d", "grp2field3b50e", "grp2field3b50f", "grp2field3b50g")], function(x) replace(x, x == 999, NA) )
dta$time_pl2_sp1 <- rowSums(dta[c("grp2field3b50a", "grp2field3b50b", "grp2field3b50c", "grp2field3b50d", "grp2field3b50e", "grp2field3b50f", "grp2field3b50g")], na.rm=T)
dta$time_pl2_sp1[rowSums(is.na(dta[c("grp2field3b50a", "grp2field3b50b", "grp2field3b50c", "grp2field3b50d", "grp2field3b50e", "grp2field3b50f", "grp2field3b50g")]))==7] <- NA

dta[c("grp3field7c50a", "grp3field7c50b", "grp3field7c50c", "grp3field7c50d", "grp3field7c50e", "grp3field7c50f", "grp3field7c50g")] <- lapply(dta[c("grp3field7c50a", "grp3field7c50b", "grp3field7c50c", "grp3field7c50d", "grp3field7c50e", "grp3field7c50f", "grp3field7c50g")], function(x) replace(x, x == 999, NA) )
dta$time_pl3_sp1 <- rowSums(dta[c("grp3field7c50a", "grp3field7c50b", "grp3field7c50c", "grp3field7c50d", "grp3field7c50e", "grp3field7c50f", "grp3field7c50g")], na.rm=T)
dta$time_pl3_sp1[rowSums(is.na(dta[c("grp3field7c50a", "grp3field7c50b", "grp3field7c50c", "grp3field7c50d", "grp3field7c50e", "grp3field7c50f", "grp3field7c50g")]))==7] <- NA

dta[c("grp4field9d50a", "grp4field9d50b", "grp4field9d50c", "grp4field9d50d", "grp4field9d50e", "grp4field9d50f", "grp4field9d50g")] <- lapply(dta[c("grp4field9d50a", "grp4field9d50b", "grp4field9d50c", "grp4field9d50d", "grp4field9d50e", "grp4field9d50f", "grp4field9d50g")], function(x) replace(x, x == 999, NA) ) 
dta$time_pl4_sp1 <- rowSums(dta[c("grp4field9d50a", "grp4field9d50b", "grp4field9d50c", "grp4field9d50d", "grp4field9d50e", "grp4field9d50f", "grp4field9d50g")], na.rm=T)
dta$time_pl4_sp1[rowSums(is.na(dta[c("grp4field9d50a", "grp4field9d50b", "grp4field9d50c", "grp4field9d50d", "grp4field9d50e", "grp4field9d50f", "grp4field9d50g")]))==7] <- NA

dta[c("grp5field11e50a", "grp5field11e50b", "grp5field11e50c", "grp5field11e50d", "grp5field11e50e", "grp5field11e50f", "grp5field11e50g")] <- lapply(dta[c("grp5field11e50a", "grp5field11e50b", "grp5field11e50c", "grp5field11e50d", "grp5field11e50e", "grp5field11e50f", "grp5field11e50g")], function(x) replace(x, x == 999, NA) )
dta$time_pl5_sp1 <- rowSums(dta[c("grp5field11e50a", "grp5field11e50b", "grp5field11e50c", "grp5field11e50d", "grp5field11e50e", "grp5field11e50f", "grp5field11e50g")], na.rm=T)
dta$time_pl5_sp1[rowSums(is.na(dta[c("grp5field11e50a", "grp5field11e50b", "grp5field11e50c", "grp5field11e50d", "grp5field11e50e", "grp5field11e50f", "grp5field11e50g")]))==7] <- NA

##spouse

dta[c("spouse2grp_sp1field_sp1f50a", "spouse2grp_sp1field_sp1f50b", "spouse2grp_sp1field_sp1f50c", "spouse2grp_sp1field_sp1f50d", "spouse2grp_sp1field_sp1f50e", "spouse2grp_sp1field_sp1f50f", "spouse2grp_sp1field_sp1f50g")]  <- lapply(dta[c("spouse2grp_sp1field_sp1f50a", "spouse2grp_sp1field_sp1f50b", "spouse2grp_sp1field_sp1f50c", "spouse2grp_sp1field_sp1f50d", "spouse2grp_sp1field_sp1f50e", "spouse2grp_sp1field_sp1f50f", "spouse2grp_sp1field_sp1f50g")], function(x) replace(x, x == 999, NA) )
dta$time_pl1_sp2 <- rowSums(dta[c("spouse2grp_sp1field_sp1f50a", "spouse2grp_sp1field_sp1f50b", "spouse2grp_sp1field_sp1f50c", "spouse2grp_sp1field_sp1f50d", "spouse2grp_sp1field_sp1f50e", "spouse2grp_sp1field_sp1f50f", "spouse2grp_sp1field_sp1f50g")], na.rm=T)
dta$time_pl1_sp2[rowSums(is.na(dta[c("spouse2grp_sp1field_sp1f50a", "spouse2grp_sp1field_sp1f50b", "spouse2grp_sp1field_sp1f50c", "spouse2grp_sp1field_sp1f50d", "spouse2grp_sp1field_sp1f50e", "spouse2grp_sp1field_sp1f50f", "spouse2grp_sp1field_sp1f50g")]))==7] <- NA

dta[c("spouse2grp_sp2field_sp3g50a", "spouse2grp_sp2field_sp3g50b", "spouse2grp_sp2field_sp3g50c", "spouse2grp_sp2field_sp3g50d", "spouse2grp_sp2field_sp3g50e", "spouse2grp_sp2field_sp3g50f", "spouse2grp_sp2field_sp3g50g")]  <- lapply(dta[c("spouse2grp_sp2field_sp3g50a", "spouse2grp_sp2field_sp3g50b", "spouse2grp_sp2field_sp3g50c", "spouse2grp_sp2field_sp3g50d", "spouse2grp_sp2field_sp3g50e", "spouse2grp_sp2field_sp3g50f", "spouse2grp_sp2field_sp3g50g")], function(x) replace(x, x == 999, NA) )
dta$time_pl2_sp2 <- rowSums(dta[c("spouse2grp_sp2field_sp3g50a", "spouse2grp_sp2field_sp3g50b", "spouse2grp_sp2field_sp3g50c", "spouse2grp_sp2field_sp3g50d", "spouse2grp_sp2field_sp3g50e", "spouse2grp_sp2field_sp3g50f", "spouse2grp_sp2field_sp3g50g")], na.rm=T)
dta$time_pl2_sp2[rowSums(is.na(dta[c("spouse2grp_sp2field_sp3g50a", "spouse2grp_sp2field_sp3g50b", "spouse2grp_sp2field_sp3g50c", "spouse2grp_sp2field_sp3g50d", "spouse2grp_sp2field_sp3g50e", "spouse2grp_sp2field_sp3g50f", "spouse2grp_sp2field_sp3g50g")]))==7] <- NA

dta[c("spouse2grp_sp3field_sp5h50a", "spouse2grp_sp3field_sp5h50b", "spouse2grp_sp3field_sp5h50c", "spouse2grp_sp3field_sp5h50d", "spouse2grp_sp3field_sp5h50e", "spouse2grp_sp3field_sp5h50f", "spouse2grp_sp3field_sp5h50g")]  <- lapply(dta[c("spouse2grp_sp3field_sp5h50a", "spouse2grp_sp3field_sp5h50b", "spouse2grp_sp3field_sp5h50c", "spouse2grp_sp3field_sp5h50d", "spouse2grp_sp3field_sp5h50e", "spouse2grp_sp3field_sp5h50f", "spouse2grp_sp3field_sp5h50g")] , function(x) replace(x, x == 999, NA) )
dta$time_pl3_sp2 <- rowSums(dta[c("spouse2grp_sp3field_sp5h50a", "spouse2grp_sp3field_sp5h50b", "spouse2grp_sp3field_sp5h50c", "spouse2grp_sp3field_sp5h50d", "spouse2grp_sp3field_sp5h50e", "spouse2grp_sp3field_sp5h50f", "spouse2grp_sp3field_sp5h50g")], na.rm=T)
dta$time_pl3_sp2[rowSums(is.na(dta[c("spouse2grp_sp3field_sp5h50a", "spouse2grp_sp3field_sp5h50b", "spouse2grp_sp3field_sp5h50c", "spouse2grp_sp3field_sp5h50d", "spouse2grp_sp3field_sp5h50e", "spouse2grp_sp3field_sp5h50f", "spouse2grp_sp3field_sp5h50g")]))==7] <- NA

dta[c("spouse2group_sp4field_sp9j50a", "spouse2group_sp4field_sp9j50b", "spouse2group_sp4field_sp9j50c", "spouse2group_sp4field_sp9j50d", "spouse2group_sp4field_sp9j50e", "spouse2group_sp4field_sp9j50f", "spouse2group_sp4field_sp9j50g")]  <- lapply(dta[c("spouse2group_sp4field_sp9j50a", "spouse2group_sp4field_sp9j50b", "spouse2group_sp4field_sp9j50c", "spouse2group_sp4field_sp9j50d", "spouse2group_sp4field_sp9j50e", "spouse2group_sp4field_sp9j50f", "spouse2group_sp4field_sp9j50g")] , function(x) replace(x, x == 999, NA) )
dta$time_pl4_sp2 <- rowSums(dta[c("spouse2group_sp4field_sp9j50a", "spouse2group_sp4field_sp9j50b", "spouse2group_sp4field_sp9j50c", "spouse2group_sp4field_sp9j50d", "spouse2group_sp4field_sp9j50e", "spouse2group_sp4field_sp9j50f", "spouse2group_sp4field_sp9j50g")], na.rm=T)
dta$time_pl4_sp2[rowSums(is.na(dta[c("spouse2group_sp4field_sp9j50a", "spouse2group_sp4field_sp9j50b", "spouse2group_sp4field_sp9j50c", "spouse2group_sp4field_sp9j50d", "spouse2group_sp4field_sp9j50e", "spouse2group_sp4field_sp9j50f", "spouse2group_sp4field_sp9j50g")]))==7] <- NA

dta[c("spouse2grp5_sp5field_sp11k50a", "spouse2grp5_sp5field_sp11k50b", "spouse2grp5_sp5field_sp11k50c", "spouse2grp5_sp5field_sp11k50d", "spouse2grp5_sp5field_sp11k50e", "spouse2grp5_sp5field_sp11k50f", "spouse2grp5_sp5field_sp11k50g")]  <- lapply(dta[c("spouse2grp5_sp5field_sp11k50a", "spouse2grp5_sp5field_sp11k50b", "spouse2grp5_sp5field_sp11k50c", "spouse2grp5_sp5field_sp11k50d", "spouse2grp5_sp5field_sp11k50e", "spouse2grp5_sp5field_sp11k50f", "spouse2grp5_sp5field_sp11k50g")], function(x) replace(x, x == 999, NA) )
dta$time_pl5_sp2 <- rowSums(dta[c("spouse2grp5_sp5field_sp11k50a", "spouse2grp5_sp5field_sp11k50b", "spouse2grp5_sp5field_sp11k50c", "spouse2grp5_sp5field_sp11k50d", "spouse2grp5_sp5field_sp11k50e", "spouse2grp5_sp5field_sp11k50f", "spouse2grp5_sp5field_sp11k50g")], na.rm=T)
dta$time_pl5_sp2[rowSums(is.na(dta[c("spouse2grp5_sp5field_sp11k50a", "spouse2grp5_sp5field_sp11k50b", "spouse2grp5_sp5field_sp11k50c", "spouse2grp5_sp5field_sp11k50d", "spouse2grp5_sp5field_sp11k50e", "spouse2grp5_sp5field_sp11k50f", "spouse2grp5_sp5field_sp11k50g")]))==7] <- NA

#time male
dta$time_male_pl1[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)] <- dta$time_pl1_sp1[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)]
dta$time_male_pl1[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)] <- dta$time_pl1_sp2[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)]

dta$time_female_pl1[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)] <- dta$time_pl1_sp1[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)]
dta$time_female_pl1[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)] <- dta$time_pl1_sp2[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)]

dta$time_male_pl2[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)] <- dta$time_pl2_sp1[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)]
dta$time_male_pl2[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)] <- dta$time_pl2_sp2[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)]

dta$time_female_pl2[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)] <- dta$time_pl2_sp1[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)]
dta$time_female_pl2[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)] <- dta$time_pl2_sp2[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)]

dta$time_male_pl3[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)] <- dta$time_pl3_sp1[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)]
dta$time_male_pl3[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)] <- dta$time_pl3_sp2[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)]

dta$time_female_pl3[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)] <- dta$time_pl3_sp1[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)]
dta$time_female_pl3[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)] <- dta$time_pl3_sp2[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)]

dta$time_male_pl4[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)] <- dta$time_pl4_sp1[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)]
dta$time_male_pl4[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)] <- dta$time_pl4_sp2[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)]

dta$time_female_pl4[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)] <- dta$time_pl4_sp1[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)]
dta$time_female_pl4[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)] <- dta$time_pl4_sp2[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)]

dta$time_male_pl5[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)] <- dta$time_pl5_sp1[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)]
dta$time_male_pl5[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)] <- dta$time_pl5_sp2[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)]

dta$time_female_pl5[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)] <- dta$time_pl5_sp1[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)]
dta$time_female_pl5[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)] <- dta$time_pl5_sp2[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)]

dta$wshare_pl1 <- dta$time_female_pl1/(dta$time_female_pl1 +dta$time_male_pl1)
dta$wshare_pl2 <- dta$time_female_pl2/(dta$time_female_pl2 +dta$time_male_pl2)
dta$wshare_pl3 <- dta$time_female_pl3/(dta$time_female_pl3 +dta$time_male_pl3)
dta$wshare_pl4 <- dta$time_female_pl4/(dta$time_female_pl4 +dta$time_male_pl4)
dta$wshare_pl5 <- dta$time_female_pl5/(dta$time_female_pl5 +dta$time_male_pl5)
### alternative measure: time allocation as estimated by woman
### now engagement based on work 
##spouse
dta[c("grp1field2a50h","grp1field2a50i","grp1field2a50j","grp1field2a50k","grp1field2a50l","grp1field2a50m", "grp1field2a50n")]  <- lapply(dta[c("grp1field2a50h","grp1field2a50i","grp1field2a50j","grp1field2a50k","grp1field2a50l","grp1field2a50m", "grp1field2a50n")], function(x) replace(x, x == 999, NA) )
dta$time_pl1_sp1_o <- rowSums(dta[c("grp1field2a50h","grp1field2a50i","grp1field2a50j","grp1field2a50k","grp1field2a50l","grp1field2a50m", "grp1field2a50n")], na.rm=T)
dta$time_pl1_sp1_o[rowSums(is.na(dta[c("grp1field2a50h","grp1field2a50i","grp1field2a50j","grp1field2a50k","grp1field2a50l","grp1field2a50m" ,"grp1field2a50n")]))==7] <- NA

dta[c("grp2field4b50h","grp2field4b50i","grp2field4b50j","grp2field4b50k","grp2field4b50l","grp2field4b50m" ,"grp2field4b50n")]  <- lapply(dta[c("grp2field4b50h","grp2field4b50i","grp2field4b50j","grp2field4b50k","grp2field4b50l","grp2field4b50m" ,"grp2field4b50n")], function(x) replace(x, x == 999, NA) )
dta$time_pl2_sp1_o <- rowSums(dta[c("grp2field4b50h","grp2field4b50i","grp2field4b50j","grp2field4b50k","grp2field4b50l","grp2field4b50m", "grp2field4b50n")], na.rm=T)
dta$time_pl2_sp1_o[rowSums(is.na(dta[c("grp2field4b50h","grp2field4b50i","grp2field4b50j","grp2field4b50k","grp2field4b50l","grp2field4b50m" ,"grp2field4b50n")]))==7] <- NA

dta[c("grp3field8c50h","grp3field8c50i","grp3field8c50j","grp3field8c50k","grp3field8c50l","grp3field8c50m", "grp3field8c50n")]  <- lapply(dta[c("grp3field8c50h","grp3field8c50i","grp3field8c50j","grp3field8c50k","grp3field8c50l","grp3field8c50m", "grp3field8c50n")] , function(x) replace(x, x == 999, NA) )
dta$time_pl3_sp1_o <- rowSums(dta[c("grp3field8c50h","grp3field8c50i","grp3field8c50j","grp3field8c50k","grp3field8c50l","grp3field8c50m","grp3field8c50n")], na.rm=T)
dta$time_pl3_sp1_o[rowSums(is.na(dta[c("grp3field8c50h","grp3field8c50i","grp3field8c50j","grp3field8c50k","grp3field8c50l","grp3field8c50m", "grp3field8c50n")]))==7] <- NA

dta[c("grp4field10d50h","grp4field10d50i","grp4field10d50j","grp4field10d50k","grp4field10d50l","grp4field10d50m" ,"grp4field10d50n")]  <- lapply(dta[c("grp4field10d50h","grp4field10d50i","grp4field10d50j","grp4field10d50k","grp4field10d50l","grp4field10d50m" ,"grp4field10d50n")], function(x) replace(x, x == 999, NA) )
dta$time_pl4_sp1_o <- rowSums(dta[c("grp4field10d50h","grp4field10d50i","grp4field10d50j","grp4field10d50k","grp4field10d50l","grp4field10d50m" ,"grp4field10d50n")], na.rm=T)
dta$time_pl4_sp1_o[rowSums(is.na(dta[c("grp4field10d50h","grp4field10d50i","grp4field10d50j","grp4field10d50k","grp4field10d50l","grp4field10d50m", "grp4field10d50n")]))==7] <- NA

dta[c("grp5field12e50h","grp5field12e50i","grp5field12e50j","grp5field12e50k","grp5field12e50l","grp5field12e50m" ,"grp5field12e50n")]  <- lapply(dta[c("grp5field12e50h","grp5field12e50i","grp5field12e50j","grp5field12e50k","grp5field12e50l","grp5field12e50m", "grp5field12e50n")], function(x) replace(x, x == 999, NA) )
dta$time_pl5_sp1_o <- rowSums(dta[c("grp5field12e50h","grp5field12e50i","grp5field12e50j","grp5field12e50k","grp5field12e50l","grp5field12e50m" ,"grp5field12e50n")], na.rm=T)
dta$time_pl5_sp1_o[rowSums(is.na(dta[c("grp5field12e50h","grp5field12e50i","grp5field12e50j","grp5field12e50k","grp5field12e50l","grp5field12e50m", "grp5field12e50n")]))==7] <- NA

##spouse
dta[c("spouse2grp_sp1field_sp2f50h", "spouse2grp_sp1field_sp2f50i", "spouse2grp_sp1field_sp2f50j", "spouse2grp_sp1field_sp2f50k", "spouse2grp_sp1field_sp2f50l", "spouse2grp_sp1field_sp2f50m", "spouse2grp_sp1field_sp2f50n")]  <- lapply(dta[c("spouse2grp_sp1field_sp2f50h", "spouse2grp_sp1field_sp2f50i", "spouse2grp_sp1field_sp2f50j", "spouse2grp_sp1field_sp2f50k", "spouse2grp_sp1field_sp2f50l", "spouse2grp_sp1field_sp2f50m", "spouse2grp_sp1field_sp2f50n")] , function(x) replace(x, x == 999, NA) )
dta$time_pl1_sp2_o <- rowSums(dta[c("spouse2grp_sp1field_sp2f50h", "spouse2grp_sp1field_sp2f50i", "spouse2grp_sp1field_sp2f50j", "spouse2grp_sp1field_sp2f50k", "spouse2grp_sp1field_sp2f50l", "spouse2grp_sp1field_sp2f50m", "spouse2grp_sp1field_sp2f50n")] , na.rm=T)
dta$time_pl1_sp2_o[rowSums(is.na(dta[c("spouse2grp_sp1field_sp2f50h", "spouse2grp_sp1field_sp2f50i", "spouse2grp_sp1field_sp2f50j", "spouse2grp_sp1field_sp2f50k", "spouse2grp_sp1field_sp2f50l", "spouse2grp_sp1field_sp2f50m", "spouse2grp_sp1field_sp2f50n")] ))==7] <- NA

dta[c("spouse2grp_sp2field_sp4g50h", "spouse2grp_sp2field_sp4g50i", "spouse2grp_sp2field_sp4g50j", "spouse2grp_sp2field_sp4g50k", "spouse2grp_sp2field_sp4g50l", "spouse2grp_sp2field_sp4g50m", "spouse2grp_sp2field_sp4g50n")]  <- lapply(dta[c("spouse2grp_sp2field_sp4g50h", "spouse2grp_sp2field_sp4g50i", "spouse2grp_sp2field_sp4g50j", "spouse2grp_sp2field_sp4g50k", "spouse2grp_sp2field_sp4g50l", "spouse2grp_sp2field_sp4g50m", "spouse2grp_sp2field_sp4g50n")], function(x) replace(x, x == 999, NA) )
dta$time_pl2_sp2_o <- rowSums(dta[c("spouse2grp_sp2field_sp4g50h", "spouse2grp_sp2field_sp4g50i", "spouse2grp_sp2field_sp4g50j", "spouse2grp_sp2field_sp4g50k", "spouse2grp_sp2field_sp4g50l", "spouse2grp_sp2field_sp4g50m", "spouse2grp_sp2field_sp4g50n")], na.rm=T)
dta$time_pl2_sp2_o[rowSums(is.na(dta[c("spouse2grp_sp2field_sp4g50h", "spouse2grp_sp2field_sp4g50i", "spouse2grp_sp2field_sp4g50j", "spouse2grp_sp2field_sp4g50k", "spouse2grp_sp2field_sp4g50l", "spouse2grp_sp2field_sp4g50m", "spouse2grp_sp2field_sp4g50n")]))==7] <- NA

dta[c("spouse2grp_sp3field_sp6h50h", "spouse2grp_sp3field_sp6h50i", "spouse2grp_sp3field_sp6h50j", "spouse2grp_sp3field_sp6h50k", "spouse2grp_sp3field_sp6h50l", "spouse2grp_sp3field_sp6h50m", "spouse2grp_sp3field_sp6h50n")]  <- lapply(dta[c("spouse2grp_sp3field_sp6h50h", "spouse2grp_sp3field_sp6h50i", "spouse2grp_sp3field_sp6h50j", "spouse2grp_sp3field_sp6h50k", "spouse2grp_sp3field_sp6h50l", "spouse2grp_sp3field_sp6h50m", "spouse2grp_sp3field_sp6h50n")] , function(x) replace(x, x == 999, NA) )
dta$time_pl3_sp2_o <- rowSums(dta[c("spouse2grp_sp3field_sp6h50h", "spouse2grp_sp3field_sp6h50i", "spouse2grp_sp3field_sp6h50j", "spouse2grp_sp3field_sp6h50k", "spouse2grp_sp3field_sp6h50l", "spouse2grp_sp3field_sp6h50m", "spouse2grp_sp3field_sp6h50n")], na.rm=T)
dta$time_pl3_sp2_o[rowSums(is.na(dta[c("spouse2grp_sp3field_sp6h50h", "spouse2grp_sp3field_sp6h50i", "spouse2grp_sp3field_sp6h50j", "spouse2grp_sp3field_sp6h50k", "spouse2grp_sp3field_sp6h50l", "spouse2grp_sp3field_sp6h50m", "spouse2grp_sp3field_sp6h50n")]))==7] <- NA

dta[c("spouse2group_sp4field_sp10j50h", "spouse2group_sp4field_sp10j50i", "spouse2group_sp4field_sp10j50j", "spouse2group_sp4field_sp10j50k", "spouse2group_sp4field_sp10j50l", "spouse2group_sp4field_sp10j50m", "spouse2group_sp4field_sp10j50n")]  <- lapply(dta[c("spouse2group_sp4field_sp10j50h", "spouse2group_sp4field_sp10j50i", "spouse2group_sp4field_sp10j50j", "spouse2group_sp4field_sp10j50k", "spouse2group_sp4field_sp10j50l", "spouse2group_sp4field_sp10j50m", "spouse2group_sp4field_sp10j50n")], function(x) replace(x, x == 999, NA) )
dta$time_pl4_sp2_o <- rowSums(dta[c("spouse2group_sp4field_sp10j50h", "spouse2group_sp4field_sp10j50i", "spouse2group_sp4field_sp10j50j", "spouse2group_sp4field_sp10j50k", "spouse2group_sp4field_sp10j50l", "spouse2group_sp4field_sp10j50m", "spouse2group_sp4field_sp10j50n")], na.rm=T)
dta$time_pl4_sp2_o[rowSums(is.na(dta[c("spouse2group_sp4field_sp10j50h", "spouse2group_sp4field_sp10j50i", "spouse2group_sp4field_sp10j50j", "spouse2group_sp4field_sp10j50k", "spouse2group_sp4field_sp10j50l", "spouse2group_sp4field_sp10j50m", "spouse2group_sp4field_sp10j50n")]))==7] <- NA

dta[c("spouse2grp5_sp5field_sp12k50h", "spouse2grp5_sp5field_sp12k50i", "spouse2grp5_sp5field_sp12k50j", "spouse2grp5_sp5field_sp12k50k", "spouse2grp5_sp5field_sp12k50l", "spouse2grp5_sp5field_sp12k50m", "spouse2grp5_sp5field_sp12k50n")]  <- lapply(dta[c("spouse2grp5_sp5field_sp12k50h", "spouse2grp5_sp5field_sp12k50i", "spouse2grp5_sp5field_sp12k50j", "spouse2grp5_sp5field_sp12k50k", "spouse2grp5_sp5field_sp12k50l", "spouse2grp5_sp5field_sp12k50m", "spouse2grp5_sp5field_sp12k50n")], function(x) replace(x, x == 999, NA) )
dta$time_pl5_sp2_o <- rowSums(dta[c("spouse2grp5_sp5field_sp12k50h", "spouse2grp5_sp5field_sp12k50i", "spouse2grp5_sp5field_sp12k50j", "spouse2grp5_sp5field_sp12k50k", "spouse2grp5_sp5field_sp12k50l", "spouse2grp5_sp5field_sp12k50m", "spouse2grp5_sp5field_sp12k50n")], na.rm=T)
dta$time_pl5_sp2_o[rowSums(is.na(dta[c("spouse2grp5_sp5field_sp12k50h", "spouse2grp5_sp5field_sp12k50i", "spouse2grp5_sp5field_sp12k50j", "spouse2grp5_sp5field_sp12k50k", "spouse2grp5_sp5field_sp12k50l", "spouse2grp5_sp5field_sp12k50m", "spouse2grp5_sp5field_sp12k50n")]))==7] <- NA

#time male
dta$time2_male_pl1[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)] <- dta$time_pl1_sp2_o[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)]
dta$time2_male_pl1[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)] <- dta$time_pl1_sp1_o[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)]

dta$time2_female_pl1[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)] <- dta$time_pl1_sp1[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)]
dta$time2_female_pl1[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)] <- dta$time_pl1_sp2[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)]

dta$time2_male_pl2[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)] <- dta$time_pl2_sp2_o[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)]
dta$time2_male_pl2[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)] <- dta$time_pl2_sp1_o[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)]

dta$time2_female_pl2[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)] <- dta$time_pl2_sp1[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)]
dta$time2_female_pl2[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)] <- dta$time_pl2_sp2[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)]

dta$time2_male_pl3[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)] <- dta$time_pl3_sp2_o[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)]
dta$time2_male_pl3[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)] <- dta$time_pl3_sp1_o[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)]

dta$time2_female_pl3[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)] <- dta$time_pl3_sp1[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)]
dta$time2_female_pl3[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)] <- dta$time_pl3_sp2[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)]

dta$time2_male_pl4[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)] <- dta$time_pl4_sp2_o[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)]
dta$time2_male_pl4[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)] <- dta$time_pl4_sp1_o[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)]

dta$time2_female_pl4[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)] <- dta$time_pl4_sp1[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)]
dta$time2_female_pl4[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)] <- dta$time_pl4_sp2[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)]

dta$time2_male_pl5[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)] <- dta$time_pl5_sp2_o[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)]
dta$time2_male_pl5[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)] <- dta$time_pl5_sp1_o[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)]

dta$time2_female_pl5[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)] <- dta$time_pl5_sp1[dta$person_interviewed=="woman" & !is.na(dta$person_interviewed)]
dta$time2_female_pl5[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)] <- dta$time_pl5_sp2[dta$person_interviewed=="man" & !is.na(dta$person_interviewed)]

dta$wshare2_pl1 <- dta$time2_female_pl1/(dta$time2_female_pl1 +dta$time2_male_pl1)
dta$wshare2_pl2 <- dta$time2_female_pl2/(dta$time2_female_pl2 +dta$time2_male_pl2)
dta$wshare2_pl3 <- dta$time2_female_pl3/(dta$time2_female_pl3 +dta$time2_male_pl3)
dta$wshare2_pl4 <- dta$time2_female_pl4/(dta$time2_female_pl4 +dta$time2_male_pl4)
dta$wshare2_pl5 <- dta$time2_female_pl5/(dta$time2_female_pl5 +dta$time2_male_pl5)

### female managed production
dta$prod_tot_sp1 <- rowSums(dta[c("prod_pl1_sp1","prod_pl2_sp1","prod_pl3_sp1","prod_pl4_sp1", "prod_pl5_sp1")]*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)
, na.rm=T)
dta$prod_tot_sp1[rowSums(is.na(dta[c("prod_pl1_sp1","prod_pl2_sp1","prod_pl3_sp1","prod_pl4_sp1", "prod_pl5_sp1")]*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$prod_tot_sp1[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0] <- NA
dta$prod_tot_sp2 <- rowSums(dta[c("prod_pl1_sp2","prod_pl2_sp2","prod_pl3_sp2","prod_pl4_sp2", "prod_pl5_sp2")]*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)
, na.rm=T)
dta$prod_tot_sp2[rowSums(is.na(dta[c("prod_pl1_sp2","prod_pl2_sp2","prod_pl3_sp2","prod_pl4_sp2", "prod_pl5_sp2")]*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$prod_tot_sp2[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0] <- NA
## take mean of production reported by both spouses as total production
dta$prod_tot_fm <-  rowMeans(dta[c("prod_tot_sp1","prod_tot_sp2")], na.rm=T)

### female involved production
dta$prod_tot_sp1 <- rowSums(dta[c("prod_pl1_sp1","prod_pl2_sp1","prod_pl3_sp1","prod_pl4_sp1", "prod_pl5_sp1")]*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$prod_tot_sp1[rowSums(is.na(dta[c("prod_pl1_sp1","prod_pl2_sp1","prod_pl3_sp1","prod_pl4_sp1", "prod_pl5_sp1")]*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA

dta$prod_tot_sp1[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0] <- NA

dta$prod_tot_sp2 <- rowSums(dta[c("prod_pl1_sp2","prod_pl2_sp2","prod_pl3_sp2","prod_pl4_sp2", "prod_pl5_sp2")]*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$prod_tot_sp2[rowSums(is.na(dta[c("prod_pl1_sp2","prod_pl2_sp2","prod_pl3_sp2","prod_pl4_sp2", "prod_pl5_sp2")]*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$prod_tot_sp2[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0] <- NA
## take mean of production reported by both spouses as total production
dta$prod_tot_fi <-  rowMeans(dta[c("prod_tot_sp1","prod_tot_sp2")], na.rm=T)

### male managed production
dta$prod_tot_sp1 <- rowSums(dta[c("prod_pl1_sp1","prod_pl2_sp1","prod_pl3_sp1","prod_pl4_sp1", "prod_pl5_sp1")]*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)
, na.rm=T)
dta$prod_tot_sp1[rowSums(is.na(dta[c("prod_pl1_sp1","prod_pl2_sp1","prod_pl3_sp1","prod_pl4_sp1", "prod_pl5_sp1")]*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
### so explicitly make missing if nothing was produced
dta$prod_tot_sp1[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0] <- NA

dta$prod_tot_sp2 <- rowSums(dta[c("prod_pl1_sp2","prod_pl2_sp2","prod_pl3_sp2","prod_pl4_sp2", "prod_pl5_sp2")]*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)
, na.rm=T)
dta$prod_tot_sp2[rowSums(is.na(dta[c("prod_pl1_sp2","prod_pl2_sp2","prod_pl3_sp2","prod_pl4_sp2", "prod_pl5_sp2")]*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$prod_tot_sp2[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0] <- NA
## take mean of production reported by both spouses as total production
dta$prod_tot_mm <-  rowMeans(dta[c("prod_tot_sp1","prod_tot_sp2")], na.rm=T)

### jointly managed production
dta$prod_tot_sp1 <- rowSums(dta[c("prod_pl1_sp1","prod_pl2_sp1","prod_pl3_sp1","prod_pl4_sp1", "prod_pl5_sp1")]*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)
, na.rm=T)
dta$prod_tot_sp1[rowSums(is.na(dta[c("prod_pl1_sp1","prod_pl2_sp1","prod_pl3_sp1","prod_pl4_sp1", "prod_pl5_sp1")]*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
### so explicitly make missing if nothing was produced
dta$prod_tot_sp1[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0] <- NA

dta$prod_tot_sp2 <- rowSums(dta[c("prod_pl1_sp2","prod_pl2_sp2","prod_pl3_sp2","prod_pl4_sp2", "prod_pl5_sp2")]*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)
, na.rm=T)
dta$prod_tot_sp2[rowSums(is.na(dta[c("prod_pl1_sp2","prod_pl2_sp2","prod_pl3_sp2","prod_pl4_sp2", "prod_pl5_sp2")]*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$prod_tot_sp2[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0] <- NA
## take mean of production reported by both spouses as total production
dta$prod_tot_bm <-  rowMeans(dta[c("prod_tot_sp1","prod_tot_sp2")], na.rm=T)

## wshare>.5 production
dta$prod_tot_sp1 <- rowSums(dta[c("prod_pl1_sp1","prod_pl2_sp1","prod_pl3_sp1","prod_pl4_sp1", "prod_pl5_sp1")]*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >.5), na.rm=T)
dta$prod_tot_sp1[rowSums(is.na(dta[c("prod_pl1_sp1","prod_pl2_sp1","prod_pl3_sp1","prod_pl4_sp1", "prod_pl5_sp1")]*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >.5)))==5] <- NA
### so explicitly make missing if nothing was produced
dta$prod_tot_sp1[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]>.5,na.rm=T)==0] <- NA

dta$prod_tot_sp2 <- rowSums(dta[c("prod_pl1_sp2","prod_pl2_sp2","prod_pl3_sp2","prod_pl4_sp2", "prod_pl5_sp2")]*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >.5)
, na.rm=T)
dta$prod_tot_sp2[rowSums(is.na(dta[c("prod_pl1_sp2","prod_pl2_sp2","prod_pl3_sp2","prod_pl4_sp2", "prod_pl5_sp2")]*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >.5)))==5] <- NA
dta$prod_tot_sp2[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]>.5,na.rm=T)==0] <- NA
## take mean of production reported by both spouses as total production
dta$prod_tot_wm_share1 <-  rowMeans(dta[c("prod_tot_sp1","prod_tot_sp2")], na.rm=T)

## wshare2>.5 production
dta$prod_tot_sp1 <- rowSums(dta[c("prod_pl1_sp1","prod_pl2_sp1","prod_pl3_sp1","prod_pl4_sp1", "prod_pl5_sp1")]*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >.5)
, na.rm=T)
dta$prod_tot_sp1[rowSums(is.na(dta[c("prod_pl1_sp1","prod_pl2_sp1","prod_pl3_sp1","prod_pl4_sp1", "prod_pl5_sp1")]*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >.5)))==5] <- NA
### so explicitly make missing if nothing was produced
dta$prod_tot_sp1[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]>.5,na.rm=T)==0] <- NA

dta$prod_tot_sp2 <- rowSums(dta[c("prod_pl1_sp2","prod_pl2_sp2","prod_pl3_sp2","prod_pl4_sp2", "prod_pl5_sp2")]*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >.5)
, na.rm=T)
dta$prod_tot_sp2[rowSums(is.na(dta[c("prod_pl1_sp2","prod_pl2_sp2","prod_pl3_sp2","prod_pl4_sp2", "prod_pl5_sp2")]*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >.5)))==5] <- NA
dta$prod_tot_sp2[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]>.5,na.rm=T)==0] <- NA
## take mean of production reported by both spouses as total production
dta$prod_tot_wm_share2 <-  rowMeans(dta[c("prod_tot_sp1","prod_tot_sp2")], na.rm=T)



### female managed area
dta$area_tot_sp1 <- rowSums(dta[c("area_pl1_sp1","area_pl2_sp1","area_pl3_sp1","area_pl4_sp1", "area_pl5_sp1")]*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)
, na.rm=T)
dta$area_tot_sp1[rowSums(is.na(dta[c("area_pl1_sp1","area_pl2_sp1","area_pl3_sp1","area_pl4_sp1", "area_pl5_sp1")]*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$area_tot_sp1[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$area_tot_sp2 <- rowSums(dta[c("area_pl1_sp2","area_pl2_sp2","area_pl3_sp2","area_pl4_sp2", "area_pl5_sp2")]*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)
, na.rm=T)
dta$area_tot_sp2[rowSums(is.na(dta[c("area_pl1_sp2","area_pl2_sp2","area_pl3_sp2","area_pl4_sp2", "area_pl5_sp2")]*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$area_tot_sp2[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA
## take mean of areauction reported by both spouses as total areauction
dta$area_tot_fm <-  rowMeans(dta[c("area_tot_sp1","area_tot_sp2")], na.rm=T)
dta$yield_av_fm <- dta$prod_tot_fm/dta$area_tot_fm
## female involved area

dta$area_tot_sp1 <- rowSums(dta[c("area_pl1_sp1","area_pl2_sp1","area_pl3_sp1","area_pl4_sp1", "area_pl5_sp1")]*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)
, na.rm=T)
dta$area_tot_sp1[rowSums(is.na(dta[c("area_pl1_sp1","area_pl2_sp1","area_pl3_sp1","area_pl4_sp1", "area_pl5_sp1")]*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$area_tot_sp1[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$area_tot_sp2 <- rowSums(dta[c("area_pl1_sp2","area_pl2_sp2","area_pl3_sp2","area_pl4_sp2", "area_pl5_sp2")]*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)
, na.rm=T)
dta$area_tot_sp2[rowSums(is.na(dta[c("area_pl1_sp2","area_pl2_sp2","area_pl3_sp2","area_pl4_sp2", "area_pl5_sp2")]*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$area_tot_sp2[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA
## take mean of areauction reported by both spouses as total areauction
dta$area_tot_fi <-  rowMeans(dta[c("area_tot_sp1","area_tot_sp2")], na.rm=T)
dta$yield_av_fi <- dta$prod_tot_fi/dta$area_tot_fi


### male managed production
dta$area_tot_sp1 <- rowSums(dta[c("area_pl1_sp1","area_pl2_sp1","area_pl3_sp1","area_pl4_sp1", "area_pl5_sp1")]*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)
, na.rm=T)
dta$area_tot_sp1[rowSums(is.na(dta[c("area_pl1_sp1","area_pl2_sp1","area_pl3_sp1","area_pl4_sp1", "area_pl5_sp1")]*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
### so explicitly make missing if nothing was produced
dta$area_tot_sp1[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$area_tot_sp2 <- rowSums(dta[c("area_pl1_sp2","area_pl2_sp2","area_pl3_sp2","area_pl4_sp2", "area_pl5_sp2")]*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)
, na.rm=T)
dta$area_tot_sp2[rowSums(is.na(dta[c("area_pl1_sp2","area_pl2_sp2","area_pl3_sp2","area_pl4_sp2", "area_pl5_sp2")]*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$area_tot_sp2[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA
## take mean of areauction reported by both spouses as total areauction
dta$area_tot_mm <-  rowMeans(dta[c("area_tot_sp1","area_tot_sp2")], na.rm=T)
dta$yield_av_mm <- dta$prod_tot_mm/dta$area_tot_mm
### jointly managed production
dta$area_tot_sp1 <- rowSums(dta[c("area_pl1_sp1","area_pl2_sp1","area_pl3_sp1","area_pl4_sp1", "area_pl5_sp1")]*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)
, na.rm=T)
dta$area_tot_sp1[rowSums(is.na(dta[c("area_pl1_sp1","area_pl2_sp1","area_pl3_sp1","area_pl4_sp1", "area_pl5_sp1")]*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
### so explicitly make missing if nothing was produced
dta$area_tot_sp1[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$area_tot_sp2 <- rowSums(dta[c("area_pl1_sp2","area_pl2_sp2","area_pl3_sp2","area_pl4_sp2", "area_pl5_sp2")]*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)
, na.rm=T)
dta$area_tot_sp2[rowSums(is.na(dta[c("area_pl1_sp2","area_pl2_sp2","area_pl3_sp2","area_pl4_sp2", "area_pl5_sp2")]*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$area_tot_sp2[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA
## take mean of areauction reported by both spouses as total areauction
dta$area_tot_bm <-  rowMeans(dta[c("area_tot_sp1","area_tot_sp2")], na.rm=T)
dta$yield_av_bm <- dta$prod_tot_bm/dta$area_tot_bm
## wshare>.5 production
dta$area_tot_sp1 <- rowSums(dta[c("area_pl1_sp1","area_pl2_sp1","area_pl3_sp1","area_pl4_sp1", "area_pl5_sp1")]*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >.5)
, na.rm=T)
dta$area_tot_sp1[rowSums(is.na(dta[c("area_pl1_sp1","area_pl2_sp1","area_pl3_sp1","area_pl4_sp1", "area_pl5_sp1")]*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >.5)))==5] <- NA
### so explicitly make missing if nothing was produced
dta$area_tot_sp1[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]>.5,na.rm=T)==0] <- NA

dta$area_tot_sp2 <- rowSums(dta[c("area_pl1_sp2","area_pl2_sp2","area_pl3_sp2","area_pl4_sp2", "area_pl5_sp2")]*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >.5)
, na.rm=T)
dta$area_tot_sp2[rowSums(is.na(dta[c("area_pl1_sp2","area_pl2_sp2","area_pl3_sp2","area_pl4_sp2", "area_pl5_sp2")]*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >.5)))==5] <- NA
dta$area_tot_sp2[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]>.5,na.rm=T)==0] <- NA
## take mean of production reported by both spouses as total areauction
dta$area_tot_wm_share1 <-  rowMeans(dta[c("area_tot_sp1","area_tot_sp2")], na.rm=T)
dta$yield_av_wm_share1 <- dta$prod_tot_wm_share1/dta$area_tot_wm_share1
## wshare2>.5 areauction
dta$area_tot_sp1 <- rowSums(dta[c("area_pl1_sp1","area_pl2_sp1","area_pl3_sp1","area_pl4_sp1", "area_pl5_sp1")]*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >.5)
, na.rm=T)
dta$area_tot_sp1[rowSums(is.na(dta[c("area_pl1_sp1","area_pl2_sp1","area_pl3_sp1","area_pl4_sp1", "area_pl5_sp1")]*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >.5)))==5] <- NA
### so explicitly make missing if nothing was produced
dta$area_tot_sp1[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]>.5,na.rm=T)==0] <- NA

dta$area_tot_sp2 <- rowSums(dta[c("area_pl1_sp2","area_pl2_sp2","area_pl3_sp2","area_pl4_sp2", "area_pl5_sp2")]*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >.5)
, na.rm=T)
dta$area_tot_sp2[rowSums(is.na(dta[c("area_pl1_sp2","area_pl2_sp2","area_pl3_sp2","area_pl4_sp2", "area_pl5_sp2")]*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >.5)))==5] <- NA
dta$area_tot_sp2[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]>.5,na.rm=T)==0] <- NA
## take mean of areauction reported by both spouses as total areauction
dta$area_tot_wm_share2 <-  rowMeans(dta[c("area_tot_sp1","area_tot_sp2")], na.rm=T)
dta$yield_av_wm_share2 <- dta$prod_tot_wm_share2/dta$area_tot_wm_share2

###### yield better compared to normal year?
dta$yield_better_sp1 <- rowSums(cbind((dta$grp1a18 ==1 | dta$grp1a18==2) ,  (dta$grp2b18 ==1 | dta$grp2b18==2), (dta$grp3c18 ==1 | dta$grp3c18==2), (dta$grp4d18 ==1 | dta$grp4d18==2), (dta$grp5e18 ==1 | dta$grp5e18==2)), na.rm=T)>0

dta$yield_better_sp1[is.na(dta$grp1a18 ==1 | dta$grp1a18==2) &  is.na(dta$grp2b18 ==1 | dta$grp2b18==2) & is.na(dta$grp3c18 ==1 | dta$grp3c18==2) &is.na(dta$grp4d18 ==1 | dta$grp4d18==2) & is.na(dta$grp5e18 ==1 | dta$grp5e18==2)] <- NA

dta$yield_better_sp2 <- rowSums(cbind((dta$spouse2grp_sp1f18 ==1 | dta$spouse2grp_sp1f18==2),  (dta$spouse2grp_sp2g18 ==1 | dta$spouse2grp_sp2g18==2), (dta$spouse2grp_sp3h18 ==1 | dta$spouse2grp_sp3h18==2), (dta$spouse2group_sp4j18 ==1 | dta$spouse2group_sp4j18==2), (dta$spouse2grp5_sp5k18 ==1 | dta$spouse2grp5_sp5k18==2)), na.rm=T) > 0

dta$yield_better_sp2[is.na(dta$spouse2grp_sp1f18 ==1 | dta$spouse2grp_sp1f18==2) & is.na(dta$spouse2grp_sp2g18 ==1 | dta$spouse2grp_sp2g18==2) & is.na(dta$spouse2grp_sp3h18 ==1 | dta$spouse2grp_sp3h18==2) & is.na(dta$spouse2group_sp4j18 ==1 | dta$spouse2group_sp4j18==2) & is.na(dta$spouse2grp5_sp5k18 ==1 | dta$spouse2grp5_sp5k18==2)] <- NA


dta$yield_better_sp1_pl1 <-  (dta$grp1a18 ==1 | dta$grp1a18==2)
dta$yield_better_sp1_pl2 <-  (dta$grp2b18 ==1 | dta$grp2b18==2)
dta$yield_better_sp1_pl3 <- (dta$grp3c18 ==1 | dta$grp3c18==2) 
dta$yield_better_sp1_pl4 <- (dta$grp4d18 ==1 | dta$grp4d18==2)
dta$yield_better_sp1_pl5 <- (dta$grp5e18 ==1 | dta$grp5e18==2)

dta$yield_better_sp2_pl1 <- (dta$spouse2grp_sp1f18 ==1 | dta$spouse2grp_sp1f18==2)
dta$yield_better_sp2_pl2 <-  (dta$spouse2grp_sp2g18 ==1 | dta$spouse2grp_sp2g18==2)
dta$yield_better_sp2_pl3 <- (dta$spouse2grp_sp3h18 ==1 | dta$spouse2grp_sp3h18==2)
dta$yield_better_sp2_pl4 <- (dta$spouse2group_sp4j18 ==1 | dta$spouse2group_sp4j18==2)
dta$yield_better_sp2_pl5 <- (dta$spouse2grp5_sp5k18 ==1 | dta$spouse2grp5_sp5k18==2)
 
# any spouse reports better yield on any plot
dta$yield_better <- rowSums(dta[c("yield_better_sp1","yield_better_sp2")], na.rm=T) > 0
dta$yield_better[is.na(dta$yield_better_sp1) & is.na(dta$yield_better_sp2)] <- NA

## female managed production
dta$yield_better_sp1 <- rowSums(cbind((dta$grp1a18 ==1 | dta$grp1a18==2) ,  (dta$grp2b18 ==1 | dta$grp2b18==2), (dta$grp3c18 ==1 | dta$grp3c18==2), (dta$grp4d18 ==1 | dta$grp4d18==2), (dta$grp5e18 ==1 | dta$grp5e18==2))*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)
, na.rm=T)
dta$yield_better_sp1[rowSums(is.na(cbind((dta$grp1a18 ==1 | dta$grp1a18==2) ,  (dta$grp2b18 ==1 | dta$grp2b18==2), (dta$grp3c18 ==1 | dta$grp3c18==2), (dta$grp4d18 ==1 | dta$grp4d18==2), (dta$grp5e18 ==1 | dta$grp5e18==2))*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$yield_better_sp1[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$yield_better_sp2 <- rowSums(cbind((dta$spouse2grp_sp1f18 ==1 | dta$spouse2grp_sp1f18==2),  (dta$spouse2grp_sp2g18 ==1 | dta$spouse2grp_sp2g18==2), (dta$spouse2grp_sp3h18 ==1 | dta$spouse2grp_sp3h18==2), (dta$spouse2group_sp4j18 ==1 | dta$spouse2group_sp4j18==2), (dta$spouse2grp5_sp5k18 ==1 | dta$spouse2grp5_sp5k18==2))*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)
, na.rm=T)
dta$area_tot_sp2[rowSums(is.na(cbind((dta$spouse2grp_sp1f18 ==1 | dta$spouse2grp_sp1f18==2),  (dta$spouse2grp_sp2g18 ==1 | dta$spouse2grp_sp2g18==2), (dta$spouse2grp_sp3h18 ==1 | dta$spouse2grp_sp3h18==2), (dta$spouse2group_sp4j18 ==1 | dta$spouse2group_sp4j18==2), (dta$spouse2grp5_sp5k18 ==1 | dta$spouse2grp5_sp5k18==2))*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$yield_better_sp2[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA
## take mean of areauction reported by both spouses as total areauction
dta$yield_better_fm <-  rowSums(dta[c("yield_better_sp1","yield_better_sp2")], na.rm=T) > 0
dta$yield_better_fm[is.na(dta$yield_better_sp1) & is.na(dta$yield_better_sp2)] <- NA
### female involved
dta$yield_better_sp1 <- rowSums(cbind((dta$grp1a18 ==1 | dta$grp1a18==2) ,  (dta$grp2b18 ==1 | dta$grp2b18==2), (dta$grp3c18 ==1 | dta$grp3c18==2), (dta$grp4d18 ==1 | dta$grp4d18==2), (dta$grp5e18 ==1 | dta$grp5e18==2))*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)
, na.rm=T)
dta$yield_better_sp1[rowSums(is.na(cbind((dta$grp1a18 ==1 | dta$grp1a18==2) ,  (dta$grp2b18 ==1 | dta$grp2b18==2), (dta$grp3c18 ==1 | dta$grp3c18==2), (dta$grp4d18 ==1 | dta$grp4d18==2), (dta$grp5e18 ==1 | dta$grp5e18==2))*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$yield_better_sp1[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$yield_better_sp2 <- rowSums(cbind((dta$spouse2grp_sp1f18 ==1 | dta$spouse2grp_sp1f18==2),  (dta$spouse2grp_sp2g18 ==1 | dta$spouse2grp_sp2g18==2), (dta$spouse2grp_sp3h18 ==1 | dta$spouse2grp_sp3h18==2), (dta$spouse2group_sp4j18 ==1 | dta$spouse2group_sp4j18==2), (dta$spouse2grp5_sp5k18 ==1 | dta$spouse2grp5_sp5k18==2))*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)
, na.rm=T)
dta$area_tot_sp2[rowSums(is.na(cbind((dta$spouse2grp_sp1f18 ==1 | dta$spouse2grp_sp1f18==2),  (dta$spouse2grp_sp2g18 ==1 | dta$spouse2grp_sp2g18==2), (dta$spouse2grp_sp3h18 ==1 | dta$spouse2grp_sp3h18==2), (dta$spouse2group_sp4j18 ==1 | dta$spouse2group_sp4j18==2), (dta$spouse2grp5_sp5k18 ==1 | dta$spouse2grp5_sp5k18==2))*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$yield_better_sp2[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA
## take mean of areauction reported by both spouses as total areauction
dta$yield_better_fi <-  rowSums(dta[c("yield_better_sp1","yield_better_sp2")], na.rm=T) > 0
dta$yield_better_fi[is.na(dta$yield_better_sp1) & is.na(dta$yield_better_sp2)] <- NA


## male managed yield better
dta$yield_better_sp1 <- rowSums(cbind((dta$grp1a18 ==1 | dta$grp1a18==2) ,  (dta$grp2b18 ==1 | dta$grp2b18==2), (dta$grp3c18 ==1 | dta$grp3c18==2), (dta$grp4d18 ==1 | dta$grp4d18==2), (dta$grp5e18 ==1 | dta$grp5e18==2))*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)
, na.rm=T)
dta$yield_better_sp1[rowSums(is.na(cbind((dta$grp1a18 ==1 | dta$grp1a18==2) ,  (dta$grp2b18 ==1 | dta$grp2b18==2), (dta$grp3c18 ==1 | dta$grp3c18==2), (dta$grp4d18 ==1 | dta$grp4d18==2), (dta$grp5e18 ==1 | dta$grp5e18==2))*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$yield_better_sp1[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$yield_better_sp2 <- rowSums(cbind((dta$spouse2grp_sp1f18 ==1 | dta$spouse2grp_sp1f18==2),  (dta$spouse2grp_sp2g18 ==1 | dta$spouse2grp_sp2g18==2), (dta$spouse2grp_sp3h18 ==1 | dta$spouse2grp_sp3h18==2), (dta$spouse2group_sp4j18 ==1 | dta$spouse2group_sp4j18==2), (dta$spouse2grp5_sp5k18 ==1 | dta$spouse2grp5_sp5k18==2))*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)
, na.rm=T)
dta$area_tot_sp2[rowSums(is.na(cbind((dta$spouse2grp_sp1f18 ==1 | dta$spouse2grp_sp1f18==2),  (dta$spouse2grp_sp2g18 ==1 | dta$spouse2grp_sp2g18==2), (dta$spouse2grp_sp3h18 ==1 | dta$spouse2grp_sp3h18==2), (dta$spouse2group_sp4j18 ==1 | dta$spouse2group_sp4j18==2), (dta$spouse2grp5_sp5k18 ==1 | dta$spouse2grp5_sp5k18==2))*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$yield_better_sp2[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA
## take mean of areauction reported by both spouses as total areauction
dta$yield_better_mm <-  rowSums(dta[c("yield_better_sp1","yield_better_sp2")], na.rm=T) > 0
dta$yield_better_mm[is.na(dta$yield_better_sp1) & is.na(dta$yield_better_sp2)] <- NA

## both managed better yield
dta$yield_better_sp1 <- rowSums(cbind((dta$grp1a18 ==1 | dta$grp1a18==2) ,  (dta$grp2b18 ==1 | dta$grp2b18==2), (dta$grp3c18 ==1 | dta$grp3c18==2), (dta$grp4d18 ==1 | dta$grp4d18==2), (dta$grp5e18 ==1 | dta$grp5e18==2))*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)
, na.rm=T)
dta$yield_better_sp1[rowSums(is.na(cbind((dta$grp1a18 ==1 | dta$grp1a18==2) ,  (dta$grp2b18 ==1 | dta$grp2b18==2), (dta$grp3c18 ==1 | dta$grp3c18==2), (dta$grp4d18 ==1 | dta$grp4d18==2), (dta$grp5e18 ==1 | dta$grp5e18==2))*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$yield_better_sp1[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$yield_better_sp2 <- rowSums(cbind((dta$spouse2grp_sp1f18 ==1 | dta$spouse2grp_sp1f18==2),  (dta$spouse2grp_sp2g18 ==1 | dta$spouse2grp_sp2g18==2), (dta$spouse2grp_sp3h18 ==1 | dta$spouse2grp_sp3h18==2), (dta$spouse2group_sp4j18 ==1 | dta$spouse2group_sp4j18==2), (dta$spouse2grp5_sp5k18 ==1 | dta$spouse2grp5_sp5k18==2))*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)
, na.rm=T)
dta$area_tot_sp2[rowSums(is.na(cbind((dta$spouse2grp_sp1f18 ==1 | dta$spouse2grp_sp1f18==2),  (dta$spouse2grp_sp2g18 ==1 | dta$spouse2grp_sp2g18==2), (dta$spouse2grp_sp3h18 ==1 | dta$spouse2grp_sp3h18==2), (dta$spouse2group_sp4j18 ==1 | dta$spouse2group_sp4j18==2), (dta$spouse2grp5_sp5k18 ==1 | dta$spouse2grp5_sp5k18==2))*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$yield_better_sp2[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA
## take mean of areauction reported by both spouses as total areauction
dta$yield_better_bm <-  rowSums(dta[c("yield_better_sp1","yield_better_sp2")], na.rm=T) > 0
dta$yield_better_bm[is.na(dta$yield_better_sp1) & is.na(dta$yield_better_sp2)] <- NA
## wshare2>.5 areauction
dta$yield_better_sp1 <- rowSums(cbind((dta$grp1a18 ==1 | dta$grp1a18==2) ,  (dta$grp2b18 ==1 | dta$grp2b18==2), (dta$grp3c18 ==1 | dta$grp3c18==2), (dta$grp4d18 ==1 | dta$grp4d18==2), (dta$grp5e18 ==1 | dta$grp5e18==2))*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >.5)
, na.rm=T)
dta$yield_better_sp1[rowSums(is.na(cbind((dta$grp1a18 ==1 | dta$grp1a18==2) ,  (dta$grp2b18 ==1 | dta$grp2b18==2), (dta$grp3c18 ==1 | dta$grp3c18==2), (dta$grp4d18 ==1 | dta$grp4d18==2), (dta$grp5e18 ==1 | dta$grp5e18==2))*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >.5)))==5] <- NA
dta$yield_better_sp1[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]>.5,na.rm=T)==0] <- NA

dta$yield_better_sp2 <- rowSums(cbind((dta$spouse2grp_sp1f18 ==1 | dta$spouse2grp_sp1f18==2),  (dta$spouse2grp_sp2g18 ==1 | dta$spouse2grp_sp2g18==2), (dta$spouse2grp_sp3h18 ==1 | dta$spouse2grp_sp3h18==2), (dta$spouse2group_sp4j18 ==1 | dta$spouse2group_sp4j18==2), (dta$spouse2grp5_sp5k18 ==1 | dta$spouse2grp5_sp5k18==2))*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >.5)
, na.rm=T)
dta$area_tot_sp2[rowSums(is.na(cbind((dta$spouse2grp_sp1f18 ==1 | dta$spouse2grp_sp1f18==2),  (dta$spouse2grp_sp2g18 ==1 | dta$spouse2grp_sp2g18==2), (dta$spouse2grp_sp3h18 ==1 | dta$spouse2grp_sp3h18==2), (dta$spouse2group_sp4j18 ==1 | dta$spouse2group_sp4j18==2), (dta$spouse2grp5_sp5k18 ==1 | dta$spouse2grp5_sp5k18==2))*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >.5)))==5] <- NA
dta$yield_better_sp2[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]>.5,na.rm=T)==0] <- NA
## take mean of areauction reported by both spouses as total areauction
dta$yield_better_wm_share1 <-  rowSums(dta[c("yield_better_sp1","yield_better_sp2")], na.rm=T) > 0
dta$yield_better_wm_share1[is.na(dta$yield_better_sp1) & is.na(dta$yield_better_sp2)] <- NA

## wshare2>.5 areauction
dta$yield_better_sp1 <- rowSums(cbind((dta$grp1a18 ==1 | dta$grp1a18==2) ,  (dta$grp2b18 ==1 | dta$grp2b18==2), (dta$grp3c18 ==1 | dta$grp3c18==2), (dta$grp4d18 ==1 | dta$grp4d18==2), (dta$grp5e18 ==1 | dta$grp5e18==2))*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >.5)
, na.rm=T)
dta$yield_better_sp1[rowSums(is.na(cbind((dta$grp1a18 ==1 | dta$grp1a18==2) ,  (dta$grp2b18 ==1 | dta$grp2b18==2), (dta$grp3c18 ==1 | dta$grp3c18==2), (dta$grp4d18 ==1 | dta$grp4d18==2), (dta$grp5e18 ==1 | dta$grp5e18==2))*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >.5)))==5] <- NA
dta$yield_better_sp1[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]>.5,na.rm=T)==0] <- NA

dta$yield_better_sp2 <- rowSums(cbind((dta$spouse2grp_sp1f18 ==1 | dta$spouse2grp_sp1f18==2),  (dta$spouse2grp_sp2g18 ==1 | dta$spouse2grp_sp2g18==2), (dta$spouse2grp_sp3h18 ==1 | dta$spouse2grp_sp3h18==2), (dta$spouse2group_sp4j18 ==1 | dta$spouse2group_sp4j18==2), (dta$spouse2grp5_sp5k18 ==1 | dta$spouse2grp5_sp5k18==2))*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >.5)
, na.rm=T)
dta$area_tot_sp2[rowSums(is.na(cbind((dta$spouse2grp_sp1f18 ==1 | dta$spouse2grp_sp1f18==2),  (dta$spouse2grp_sp2g18 ==1 | dta$spouse2grp_sp2g18==2), (dta$spouse2grp_sp3h18 ==1 | dta$spouse2grp_sp3h18==2), (dta$spouse2group_sp4j18 ==1 | dta$spouse2group_sp4j18==2), (dta$spouse2grp5_sp5k18 ==1 | dta$spouse2grp5_sp5k18==2))*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >.5)))==5] <- NA
dta$yield_better_sp2[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]>.5,na.rm=T)==0] <- NA
## take mean of areauction reported by both spouses as total areauction
dta$yield_better_wm_share2 <-  rowSums(dta[c("yield_better_sp1","yield_better_sp2")], na.rm=T) > 0
dta$yield_better_wm_share2[is.na(dta$yield_better_sp1) & is.na(dta$yield_better_sp2)] <- NA

###who decides on fertilizer

dta$decfert_man_pl1 <-  rowSums(cbind((dta$grp1a31==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f31==1  & dta$person_interviewed=="woman"), (dta$grp1a30a==1  & dta$person_interviewed=="man"),(dta$spouse2grp_sp1f30a==1  & dta$person_interviewed=="woman"),(dta$grp1a37a==1  & dta$person_interviewed=="man"),(dta$spouse2grp_sp1f37a==1  & dta$person_interviewed=="woman")), na.rm=T) >0
dta$decfert_man_pl1[is.na(dta$grp1a31) & is.na(dta$spouse2grp_sp1f31) & is.na(dta$grp1a30a) & is.na(dta$spouse2grp_sp1f30a) & is.na(dta$grp1a37a) & is.na(dta$spouse2grp_sp1f37a)] <- NA
dta$decfert_woman_pl1 <- rowSums(cbind((dta$grp1a31==1  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp1f31==1  & dta$person_interviewed=="man"), (dta$grp1a30a==1  & dta$person_interviewed=="woman"),(dta$spouse2grp_sp1f30a==1  & dta$person_interviewed=="man"),(dta$grp1a37a==1  & dta$person_interviewed=="woman"),(dta$spouse2grp_sp1f37a==1  & dta$person_interviewed=="man")), na.rm=T) >0
dta$decfert_woman_pl1[is.na(dta$grp1a31) & is.na(dta$spouse2grp_sp1f31) & is.na(dta$grp1a30a) & is.na(dta$spouse2grp_sp1f30a) & is.na(dta$grp1a37a) & is.na(dta$spouse2grp_sp1f37a)] <- NA
dta$decfert_both_pl1 <-  rowSums(cbind((dta$grp1a31==3  & dta$spouse2grp_sp1f31==3), (dta$grp1a30a==3  & dta$spouse2grp_sp1f30a==3),(dta$grp1a37a==3 & dta$spouse2grp_sp1f37a==3)), na.rm=T) >0
dta$decfert_both_pl1[is.na(dta$grp1a31) & is.na(dta$spouse2grp_sp1f31) & is.na(dta$grp1a30a) & is.na(dta$spouse2grp_sp1f30a) & is.na(dta$grp1a37a) & is.na(dta$spouse2grp_sp1f37a)] <- NA

dta$decfert_man_pl2 <-  rowSums(cbind((dta$grp2b31==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g31==1  & dta$person_interviewed=="woman"), (dta$grp2b30b==1  & dta$person_interviewed=="man"),(dta$spouse2grp_sp2g30b==1  & dta$person_interviewed=="woman"),(dta$grp2b37a==1  & dta$person_interviewed=="man"),(dta$spouse2grp_sp2g37a==1  & dta$person_interviewed=="woman")), na.rm=T) >0
dta$decfert_man_pl2[is.na(dta$grp2b31) & is.na(dta$spouse2grp_sp2g31) & is.na(dta$grp2b30b) & is.na(dta$spouse2grp_sp2g30b) & is.na(dta$grp2b37a) & is.na(dta$spouse2grp_sp2g37a)] <- NA
dta$decfert_woman_pl2 <- rowSums(cbind((dta$grp2b31==1  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp2g31==1  & dta$person_interviewed=="man"), (dta$grp2b30b==1  & dta$person_interviewed=="woman"),(dta$spouse2grp_sp2g30b==1  & dta$person_interviewed=="man"),(dta$grp2b37a==1  & dta$person_interviewed=="woman"),(dta$spouse2grp_sp2g37a==1  & dta$person_interviewed=="man")), na.rm=T) >0
dta$decfert_woman_pl2[is.na(dta$grp2b31) & is.na(dta$spouse2grp_sp2g31) & is.na(dta$grp2b30b) & is.na(dta$spouse2grp_sp2g30b) & is.na(dta$grp2b37a) & is.na(dta$spouse2grp_sp2g37a)] <- NA
dta$decfert_both_pl2 <-  rowSums(cbind((dta$grp2b31==3  & dta$spouse2grp_sp2g31==3),  (dta$grp2b30b==3  & dta$spouse2grp_sp2g30b==3), (dta$grp2b37a==3  & dta$person_interviewed==3)), na.rm=T) >0
dta$decfert_both_pl2[is.na(dta$grp2b31) & is.na(dta$spouse2grp_sp2g31) & is.na(dta$grp2b30b) & is.na(dta$spouse2grp_sp2g30b) & is.na(dta$grp2b37a) & is.na(dta$spouse2grp_sp2g37a)] <- NA

dta$decfert_man_pl3 <-  rowSums(cbind((dta$grp3c31==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h31==1  & dta$person_interviewed=="woman"), (dta$grp3c30c==1  & dta$person_interviewed=="man"),(dta$spouse2grp_sp3h30c==1  & dta$person_interviewed=="woman"),(dta$grp3c37a==1  & dta$person_interviewed=="man"),(dta$spouse2grp_sp3h37a==1  & dta$person_interviewed=="woman")), na.rm=T) >0
dta$decfert_man_pl3[is.na(dta$grp3c31) & is.na(dta$spouse2grp_sp3h31) & is.na(dta$grp3c30c) & is.na(dta$spouse2grp_sp3h30c) & is.na(dta$grp3c37a) & is.na(dta$spouse2grp_sp3h37a)] <- NA
dta$decfert_woman_pl3 <-  rowSums(cbind((dta$grp3c31==1  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp3h31==1  & dta$person_interviewed=="man"), (dta$grp3c30c==1  & dta$person_interviewed=="woman"),(dta$spouse2grp_sp3h30c==1  & dta$person_interviewed=="man"),(dta$grp3c37a==1  & dta$person_interviewed=="woman"),(dta$spouse2grp_sp3h37a==1  & dta$person_interviewed=="man")), na.rm=T) >0
dta$decfert_woman_pl3[is.na(dta$grp3c31) & is.na(dta$spouse2grp_sp3h31) & is.na(dta$grp3c30c) & is.na(dta$spouse2grp_sp3h30c) & is.na(dta$grp3c37a) & is.na(dta$spouse2grp_sp3h37a)] <- NA
dta$decfert_both_pl3 <-  rowSums(cbind((dta$grp3c31==3 & dta$spouse2grp_sp3h31==3), (dta$grp3c30c==3 & dta$spouse2grp_sp3h30c==3), (dta$grp3c37a==3  & dta$spouse2grp_sp3h37a==3 )), na.rm=T) >0
dta$decfert_both_pl3[is.na(dta$grp3c31) & is.na(dta$spouse2grp_sp3h31) & is.na(dta$grp3c30c) & is.na(dta$spouse2grp_sp3h30c) & is.na(dta$grp3c37a) & is.na(dta$spouse2grp_sp3h37a)] <- NA

dta$decfert_man_pl4 <-  rowSums(cbind((dta$grp4d31==1  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j31==1  & dta$person_interviewed=="woman"), (dta$grp4d30d==1  & dta$person_interviewed=="man"),( dta$spouse2group_sp4j30j==1  & dta$person_interviewed=="woman"),(dta$grp4d37a==1  & dta$person_interviewed=="man"),(dta$spouse2group_sp4j37==1  & dta$person_interviewed=="woman")), na.rm=T) >0
dta$decfert_man_pl4[is.na(dta$grp4d31) & is.na(dta$spouse2group_sp4j31) & is.na(dta$grp4d30d) & is.na(dta$spouse2group_sp4j30j) & is.na(dta$grp4d37a) & is.na(dta$spouse2group_sp4j37)] <- NA
dta$decfert_woman_pl4 <-   rowSums(cbind((dta$grp4d31==1  & dta$person_interviewed=="woman") , (dta$spouse2group_sp4j31==1  & dta$person_interviewed=="man"), (dta$grp4d30d==1  & dta$person_interviewed=="woman"),( dta$spouse2group_sp4j30j==1  & dta$person_interviewed=="man"),(dta$grp4d37a==1  & dta$person_interviewed=="woman"),(dta$spouse2group_sp4j37==1  & dta$person_interviewed=="man")), na.rm=T) >0
dta$decfert_woman_pl4[is.na(dta$grp4d31) & is.na(dta$spouse2group_sp4j31) & is.na(dta$grp4d30d) & is.na(dta$spouse2group_sp4j30j) & is.na(dta$grp4d37a) & is.na(dta$spouse2group_sp4j37)] <- NA
dta$decfert_both_pl4 <-   rowSums(cbind((dta$grp4d31==3  & dta$dta$spouse2group_sp4j31==3), (dta$grp4d30d==3  & dta$spouse2group_sp4j30j==3),(dta$grp4d37a==3  & dta$spouse2group_sp4j37==3)), na.rm=T) >0
dta$decfert_both_pl4[is.na(dta$grp4d31) & is.na(dta$spouse2group_sp4j31) & is.na(dta$grp4d30d) & is.na(dta$spouse2group_sp4j30j) & is.na(dta$grp4d37a) & is.na(dta$spouse2group_sp4j37)] <- NA


dta$decfert_man_pl5 <-  rowSums(cbind((dta$grp5e31==1  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k31==1  & dta$person_interviewed=="woman"), (dta$grp5e30e==1  & dta$person_interviewed=="man"),( dta$spouse2grp5_sp5k30k==1  & dta$person_interviewed=="woman"),(dta$grp5e37a==1  & dta$person_interviewed=="man"),(dta$spouse2grp5_sp5k37==1  & dta$person_interviewed=="woman")), na.rm=T) >0
dta$decfert_man_pl5[is.na(dta$grp5e31) & is.na(dta$spouse2grp5_sp5k31) & is.na(dta$grp5e30e) & is.na(dta$spouse2grp5_sp5k30k) & is.na(dta$grp5e37a) & is.na(dta$spouse2grp5_sp5k37)] <- NA
dta$decfert_woman_pl5 <-     rowSums(cbind((dta$grp5e31==1  & dta$person_interviewed=="woman") , (dta$spouse2grp5_sp5k31==1  & dta$person_interviewed=="man"), (dta$grp5e30e==1  & dta$person_interviewed=="woman"),( dta$spouse2grp5_sp5k30k==1  & dta$person_interviewed=="man"),(dta$grp5e37a==1  & dta$person_interviewed=="woman"),(dta$spouse2grp5_sp5k37==1  & dta$person_interviewed=="man")), na.rm=T) >0
dta$decfert_woman_pl5[is.na(dta$grp5e31) & is.na(dta$spouse2grp5_sp5k31) & is.na(dta$grp5e30e) & is.na(dta$spouse2grp5_sp5k30k) & is.na(dta$grp5e37a) & is.na(dta$spouse2grp5_sp5k37)] <- NA
dta$decfert_both_pl5 <-  rowSums(cbind((dta$grp5e31==1  & dta$spouse2grp5_sp5k31==1), (dta$grp5e30e==1 & dta$spouse2grp5_sp5k30k==1),(dta$grp5e37a==1 & dta$spouse2grp5_sp5k37==1 )), na.rm=T) >0
dta$decfert_both_pl5[is.na(dta$grp5e31) & is.na(dta$spouse2grp5_sp5k31) & is.na(dta$grp5e30e) & is.na(dta$spouse2grp5_sp5k30k) & is.na(dta$grp5e37a) & is.na(dta$spouse2grp5_sp5k37)] <- NA

### who decides on seed:
dta$decseed_man_pl1 <-  rowSums(cbind((dta$grp1a43a==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f43a==1  & dta$person_interviewed=="woman"), (dta$grp1a43h==1  & dta$person_interviewed=="man"),(dta$spouse2grp_sp1f43h==1  & dta$person_interviewed=="woman"),(dta$grp1a43o==1  & dta$person_interviewed=="man"),(dta$spouse2grp_sp1f43o==1  & dta$person_interviewed=="woman"),(dta$grp1a43v==1  & dta$person_interviewed=="man"),(dta$spouse2grp_sp1f43v==1  & dta$person_interviewed=="woman"),(dta$grp1a44==1  & dta$person_interviewed=="man"),(dta$spouse2grp_sp1f44==1  & dta$person_interviewed=="woman"),(dta$grp1a50==1  & dta$person_interviewed=="man"),(dta$spouse2grp_sp1f50==1  & dta$person_interviewed=="woman")), na.rm=T) >0


dta$decseed_man_pl1[is.na(dta$grp1a43a) & is.na(dta$spouse2grp_sp1f43a) & is.na(dta$grp1a43h) & is.na(dta$spouse2grp_sp1f43h) & is.na(dta$grp1a43o) & is.na(dta$spouse2grp_sp1f43o) 
& is.na(dta$grp1a43v) & is.na(dta$spouse2grp_sp1f43v)
& is.na(dta$grp1a44) & is.na(dta$spouse2grp_sp1f44)
& is.na(dta$grp1a50) & is.na(dta$spouse2grp_sp1f50)
] <- NA

dta$decseed_woman_pl1 <- rowSums(cbind((dta$grp1a43a==1  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp1f43a==1  & dta$person_interviewed=="man"), (dta$grp1a43h==1  & dta$person_interviewed=="woman"),(dta$spouse2grp_sp1f43h==1  & dta$person_interviewed=="man"),(dta$grp1a43o==1  & dta$person_interviewed=="woman"),(dta$spouse2grp_sp1f43o==1  & dta$person_interviewed=="man"),(dta$grp1a43v==1  & dta$person_interviewed=="woman"),(dta$spouse2grp_sp1f43v==1  & dta$person_interviewed=="man"),(dta$grp1a44==1  & dta$person_interviewed=="woman"),(dta$spouse2grp_sp1f44==1  & dta$person_interviewed=="man"),(dta$grp1a50==1  & dta$person_interviewed=="woman"),(dta$spouse2grp_sp1f50==1  & dta$person_interviewed=="man")), na.rm=T) >0


dta$decseed_woman_pl1[is.na(dta$grp1a43a) & is.na(dta$spouse2grp_sp1f43a) & is.na(dta$grp1a43h) & is.na(dta$spouse2grp_sp1f43h) & is.na(dta$grp1a43o) & is.na(dta$spouse2grp_sp1f43o) 
& is.na(dta$grp1a43v) & is.na(dta$spouse2grp_sp1f43v)
& is.na(dta$grp1a44) & is.na(dta$spouse2grp_sp1f44)
& is.na(dta$grp1a50) & is.na(dta$spouse2grp_sp1f50)
] <- NA

dta$decseed_both_pl1 <- rowSums(cbind((dta$grp1a43a==3 & dta$spouse2grp_sp1f43a==3), (dta$grp1a43h==3  & dta$spouse2grp_sp1f43h==3),(dta$grp1a43o==3  & dta$spouse2grp_sp1f43o==3 ),(dta$grp1a43v==3  & dta$spouse2grp_sp1f43v==3),(dta$grp1a44==3  & dta$spouse2grp_sp1f44==3),(dta$grp1a50==3  & dta$spouse2grp_sp1f50==3)), na.rm=T) >0
dta$decseed_both_pl1[is.na(dta$grp1a43a) & is.na(dta$spouse2grp_sp1f43a) & is.na(dta$grp1a43h) & is.na(dta$spouse2grp_sp1f43h) & is.na(dta$grp1a43o) & is.na(dta$spouse2grp_sp1f43o) 
& is.na(dta$grp1a43v) & is.na(dta$spouse2grp_sp1f43v)
& is.na(dta$grp1a44) & is.na(dta$spouse2grp_sp1f44)
& is.na(dta$grp1a50) & is.na(dta$spouse2grp_sp1f50)
] <- NA

### plot2
dta$decseed_man_pl2 <-  rowSums(cbind((dta$grp2b43a==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g43a==1  & dta$person_interviewed=="woman"), (dta$grp2b43h==1  & dta$person_interviewed=="man"),(dta$spouse2grp_sp2g43h==1  & dta$person_interviewed=="woman"),(dta$grp2b43o==1  & dta$person_interviewed=="man"),(dta$spouse2grp_sp2g43o==1  & dta$person_interviewed=="woman"),(dta$grp2b43v==1  & dta$person_interviewed=="man"),(dta$spouse2grp_sp2g43v==1  & dta$person_interviewed=="woman"),(dta$grp2b44==1  & dta$person_interviewed=="man"),(dta$spouse2grp_sp2g44==1  & dta$person_interviewed=="woman"),(dta$grp2b50==1  & dta$person_interviewed=="man"),(dta$spouse2grp_sp2g50==1  & dta$person_interviewed=="woman")), na.rm=T) >0


dta$decseed_man_pl2[is.na(dta$grp2b43a) & is.na(dta$spouse2grp_sp2g43a) & is.na(dta$grp2b43h) & is.na(dta$spouse2grp_sp2g43h) & is.na(dta$grp2b43o) & is.na(dta$spouse2grp_sp2g43o) 
& is.na(dta$grp2b43v) & is.na(dta$spouse2grp_sp2g43v)
& is.na(dta$grp2b44) & is.na(dta$spouse2grp_sp2g44)
& is.na(dta$grp2b50) & is.na(dta$spouse2grp_sp2g50)
] <- NA

dta$decseed_woman_pl2 <- rowSums(cbind((dta$grp2b43a==1  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp2g43a==1  & dta$person_interviewed=="man"), (dta$grp2b43h==1  & dta$person_interviewed=="woman"),(dta$spouse2grp_sp2g43h==1  & dta$person_interviewed=="man"),(dta$grp2b43o==1  & dta$person_interviewed=="woman"),(dta$spouse2grp_sp2g43o==1  & dta$person_interviewed=="man"),(dta$grp2b43v==1  & dta$person_interviewed=="woman"),(dta$spouse2grp_sp2g43v==1  & dta$person_interviewed=="man"),(dta$grp2b44==1  & dta$person_interviewed=="woman"),(dta$spouse2grp_sp2g44==1  & dta$person_interviewed=="man"),(dta$grp2b50==1  & dta$person_interviewed=="woman"),(dta$spouse2grp_sp2g50==1  & dta$person_interviewed=="man")), na.rm=T) >0


dta$decseed_woman_pl2[is.na(dta$grp2b43a) & is.na(dta$spouse2grp_sp2g43a) & is.na(dta$grp2b43h) & is.na(dta$spouse2grp_sp2g43h) & is.na(dta$grp2b43o) & is.na(dta$spouse2grp_sp2g43o) 
& is.na(dta$grp2b43v) & is.na(dta$spouse2grp_sp2g43v)
& is.na(dta$grp2b44) & is.na(dta$spouse2grp_sp2g44)
& is.na(dta$grp2b50) & is.na(dta$spouse2grp_sp2g50)
] <- NA

dta$decseed_both_pl2 <- rowSums(cbind((dta$grp2b43a==3 & dta$spouse2grp_sp2g43a==3), (dta$grp2b43h==3  & dta$spouse2grp_sp2g43h==3),(dta$grp2b43o==3  & dta$spouse2grp_sp2g43o==3 ),(dta$grp2b43v==3  & dta$spouse2grp_sp2g43v==3),(dta$grp2b44==3  & dta$spouse2grp_sp2g44==3),(dta$grp2b50==3  & dta$spouse2grp_sp2g50==3)), na.rm=T) >0
dta$decseed_both_pl2[is.na(dta$grp2b43a) & is.na(dta$spouse2grp_sp2g43a) & is.na(dta$grp2b43h) & is.na(dta$spouse2grp_sp2g43h) & is.na(dta$grp2b43o) & is.na(dta$spouse2grp_sp2g43o) 
& is.na(dta$grp2b43v) & is.na(dta$spouse2grp_sp2g43v)
& is.na(dta$grp2b44) & is.na(dta$spouse2grp_sp2g44)
& is.na(dta$grp2b50) & is.na(dta$spouse2grp_sp2g50)
] <- NA

### plot 3
dta$decseed_man_pl3 <-  rowSums(cbind((dta$grp3c43a==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h43a==1  & dta$person_interviewed=="woman"), (dta$grp3c43h==1  & dta$person_interviewed=="man"),(dta$spouse2grp_sp3h43h==1  & dta$person_interviewed=="woman"),(dta$grp3c43o==1  & dta$person_interviewed=="man"),(dta$spouse2grp_sp3h43o==1  & dta$person_interviewed=="woman"),(dta$grp3c43v==1  & dta$person_interviewed=="man"),(dta$spouse2grp_sp3h43v==1  & dta$person_interviewed=="woman"),(dta$grp3c44==1  & dta$person_interviewed=="man"),(dta$spouse2grp_sp3h44==1  & dta$person_interviewed=="woman"),(dta$grp3c50==1  & dta$person_interviewed=="man"),(dta$spouse2grp_sp3h50==1  & dta$person_interviewed=="woman")), na.rm=T) >0


dta$decseed_man_pl3[is.na(dta$grp3c43a) & is.na(dta$spouse2grp_sp3h43a) & is.na(dta$grp3c43h) & is.na(dta$spouse2grp_sp3h43h) & is.na(dta$grp3c43o) & is.na(dta$spouse2grp_sp3h43o) 
& is.na(dta$grp3c43v) & is.na(dta$spouse2grp_sp3h43v)
& is.na(dta$grp3c44) & is.na(dta$spouse2grp_sp3h44)
& is.na(dta$grp3c50) & is.na(dta$spouse2grp_sp3h50)
] <- NA

dta$decseed_woman_pl3 <- rowSums(cbind((dta$grp3c43a==1  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp3h43a==1  & dta$person_interviewed=="man"), (dta$grp3c43h==1  & dta$person_interviewed=="woman"),(dta$spouse2grp_sp3h43h==1  & dta$person_interviewed=="man"),(dta$grp3c43o==1  & dta$person_interviewed=="woman"),(dta$spouse2grp_sp3h43o==1  & dta$person_interviewed=="man"),(dta$grp3c43v==1  & dta$person_interviewed=="woman"),(dta$spouse2grp_sp3h43v==1  & dta$person_interviewed=="man"),(dta$grp3c44==1  & dta$person_interviewed=="woman"),(dta$spouse2grp_sp3h44==1  & dta$person_interviewed=="man"),(dta$grp3c50==1  & dta$person_interviewed=="woman"),(dta$spouse2grp_sp3h50==1  & dta$person_interviewed=="man")), na.rm=T) >0


dta$decseed_woman_pl3[is.na(dta$grp3c43a) & is.na(dta$spouse2grp_sp3h43a) & is.na(dta$grp3c43h) & is.na(dta$spouse2grp_sp3h43h) & is.na(dta$grp3c43o) & is.na(dta$spouse2grp_sp3h43o) 
& is.na(dta$grp3c43v) & is.na(dta$spouse2grp_sp3h43v)
& is.na(dta$grp3c44) & is.na(dta$spouse2grp_sp3h44)
& is.na(dta$grp3c50) & is.na(dta$spouse2grp_sp3h50)
] <- NA

dta$decseed_both_pl3 <- rowSums(cbind((dta$grp3c43a==3 & dta$spouse2grp_sp3h43a==3), (dta$grp3c43h==3  & dta$spouse2grp_sp3h43h==3),(dta$grp3c43o==3  & dta$spouse2grp_sp3h43o==3 ),(dta$grp3c43v==3  & dta$spouse2grp_sp3h43v==3),(dta$grp3c44==3  & dta$spouse2grp_sp3h44==3),(dta$grp3c50==3  & dta$spouse2grp_sp3h50==3)), na.rm=T) >0
dta$decseed_both_pl3[is.na(dta$grp3c43a) & is.na(dta$spouse2grp_sp3h43a) & is.na(dta$grp3c43h) & is.na(dta$spouse2grp_sp3h43h) & is.na(dta$grp3c43o) & is.na(dta$spouse2grp_sp3h43o) 
& is.na(dta$grp3c43v) & is.na(dta$spouse2grp_sp3h43v)
& is.na(dta$grp3c44) & is.na(dta$spouse2grp_sp3h44)
& is.na(dta$grp3c50) & is.na(dta$spouse2grp_sp3h50)
] <- NA

#plot 4

dta$decseed_man_pl4 <-  rowSums(cbind((dta$grp4d43a==1  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j43a==1  & dta$person_interviewed=="woman"), (dta$grp4d43h==1  & dta$person_interviewed=="man"),(dta$spouse2group_sp4j43h==1  & dta$person_interviewed=="woman"),(dta$grp4d43o==1  & dta$person_interviewed=="man"),(dta$spouse2group_sp4j43o==1  & dta$person_interviewed=="woman"),(dta$grp4d43v==1  & dta$person_interviewed=="man"),(dta$spouse2group_sp4j43v==1  & dta$person_interviewed=="woman"),(dta$grp4d44==1  & dta$person_interviewed=="man"),(dta$spouse2group_sp4j44==1  & dta$person_interviewed=="woman"),(dta$grp4d50==1  & dta$person_interviewed=="man"),(dta$spouse2group_sp4j50==1  & dta$person_interviewed=="woman")), na.rm=T) >0


dta$decseed_man_pl4[is.na(dta$grp4d43a) & is.na(dta$spouse2group_sp4j43a) & is.na(dta$grp4d43h) & is.na(dta$spouse2group_sp4j43h) & is.na(dta$grp4d43o) & is.na(dta$spouse2group_sp4j43o) 
& is.na(dta$grp4d43v) & is.na(dta$spouse2group_sp4j43v)
& is.na(dta$grp4d44) & is.na(dta$spouse2group_sp4j44)
& is.na(dta$grp4d50) & is.na(dta$spouse2group_sp4j50)
] <- NA

dta$decseed_woman_pl4 <- rowSums(cbind((dta$grp4d43a==1  & dta$person_interviewed=="woman") , (dta$spouse2group_sp4j43a==1  & dta$person_interviewed=="man"), (dta$grp4d43h==1  & dta$person_interviewed=="woman"),(dta$spouse2group_sp4j43h==1  & dta$person_interviewed=="man"),(dta$grp4d43o==1  & dta$person_interviewed=="woman"),(dta$spouse2group_sp4j43o==1  & dta$person_interviewed=="man"),(dta$grp4d43v==1  & dta$person_interviewed=="woman"),(dta$spouse2group_sp4j43v==1  & dta$person_interviewed=="man"),(dta$grp4d44==1  & dta$person_interviewed=="woman"),(dta$spouse2group_sp4j44==1  & dta$person_interviewed=="man"),(dta$grp4d50==1  & dta$person_interviewed=="woman"),(dta$spouse2group_sp4j50==1  & dta$person_interviewed=="man")), na.rm=T) >0


dta$decseed_woman_pl4[is.na(dta$grp4d43a) & is.na(dta$spouse2group_sp4j43a) & is.na(dta$grp4d43h) & is.na(dta$spouse2group_sp4j43h) & is.na(dta$grp4d43o) & is.na(dta$spouse2group_sp4j43o) 
& is.na(dta$grp4d43v) & is.na(dta$spouse2group_sp4j43v)
& is.na(dta$grp4d44) & is.na(dta$spouse2group_sp4j44)
& is.na(dta$grp4d50) & is.na(dta$spouse2group_sp4j50)
] <- NA

dta$decseed_both_pl4 <- rowSums(cbind((dta$grp4d43a==3 & dta$spouse2group_sp4j43a==3), (dta$grp4d43h==3  & dta$spouse2group_sp4j43h==3),(dta$grp4d43o==3  & dta$spouse2group_sp4j43o==3 ),(dta$grp4d43v==3  & dta$spouse2group_sp4j43v==3),(dta$grp4d44==3  & dta$spouse2group_sp4j44==3),(dta$grp4d50==3  & dta$spouse2group_sp4j50==3)), na.rm=T) >0
dta$decseed_both_pl4[is.na(dta$grp4d43a) & is.na(dta$spouse2group_sp4j43a) & is.na(dta$grp4d43h) & is.na(dta$spouse2group_sp4j43h) & is.na(dta$grp4d43o) & is.na(dta$spouse2group_sp4j43o) 
& is.na(dta$grp4d43v) & is.na(dta$spouse2group_sp4j43v)
& is.na(dta$grp4d44) & is.na(dta$spouse2group_sp4j44)
& is.na(dta$grp4d50) & is.na(dta$spouse2group_sp4j50)
] <- NA

#plot 5

dta$decseed_man_pl5 <-  rowSums(cbind((dta$grp5e43a==1  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k43a==1  & dta$person_interviewed=="woman"), (dta$grp5e43h==1  & dta$person_interviewed=="man"),(dta$spouse2grp5_sp5k43h==1  & dta$person_interviewed=="woman"),(dta$grp5e43o==1  & dta$person_interviewed=="man"),(dta$spouse2grp5_sp5k43o==1  & dta$person_interviewed=="woman"),(dta$grp5e43v==1  & dta$person_interviewed=="man"),(dta$spouse2grp5_sp5k43v==1  & dta$person_interviewed=="woman"),(dta$grp5e44==1  & dta$person_interviewed=="man"),(dta$spouse2grp5_sp5k44==1  & dta$person_interviewed=="woman"),(dta$grp5e50==1  & dta$person_interviewed=="man"),(dta$spouse2grp5_sp5k50==1  & dta$person_interviewed=="woman")), na.rm=T) >0


dta$decseed_man_pl5[is.na(dta$grp5e43a) & is.na(dta$spouse2grp5_sp5k43a) & is.na(dta$grp5e43h) & is.na(dta$spouse2grp5_sp5k43h) & is.na(dta$grp5e43o) & is.na(dta$spouse2grp5_sp5k43o) 
& is.na(dta$grp5e43v) & is.na(dta$spouse2grp5_sp5k43v)
& is.na(dta$grp5e44) & is.na(dta$spouse2grp5_sp5k44)
& is.na(dta$grp5e50) & is.na(dta$spouse2grp5_sp5k50)
] <- NA

dta$decseed_woman_pl5 <- rowSums(cbind((dta$grp5e43a==1  & dta$person_interviewed=="woman") , (dta$spouse2grp5_sp5k43a==1  & dta$person_interviewed=="man"), (dta$grp5e43h==1  & dta$person_interviewed=="woman"),(dta$spouse2grp5_sp5k43h==1  & dta$person_interviewed=="man"),(dta$grp5e43o==1  & dta$person_interviewed=="woman"),(dta$spouse2grp5_sp5k43o==1  & dta$person_interviewed=="man"),(dta$grp5e43v==1  & dta$person_interviewed=="woman"),(dta$spouse2grp5_sp5k43v==1  & dta$person_interviewed=="man"),(dta$grp5e44==1  & dta$person_interviewed=="woman"),(dta$spouse2grp5_sp5k44==1  & dta$person_interviewed=="man"),(dta$grp5e50==1  & dta$person_interviewed=="woman"),(dta$spouse2grp5_sp5k50==1  & dta$person_interviewed=="man")), na.rm=T) >0


dta$decseed_woman_pl5[is.na(dta$grp5e43a) & is.na(dta$spouse2grp5_sp5k43a) & is.na(dta$grp5e43h) & is.na(dta$spouse2grp5_sp5k43h) & is.na(dta$grp5e43o) & is.na(dta$spouse2grp5_sp5k43o) 
& is.na(dta$grp5e43v) & is.na(dta$spouse2grp5_sp5k43v)
& is.na(dta$grp5e44) & is.na(dta$spouse2grp5_sp5k44)
& is.na(dta$grp5e50) & is.na(dta$spouse2grp5_sp5k50)
] <- NA

dta$decseed_both_pl5 <- rowSums(cbind((dta$grp5e43a==3 & dta$spouse2grp5_sp5k43a==3), (dta$grp5e43h==3  & dta$spouse2grp5_sp5k43h==3),(dta$grp5e43o==3  & dta$spouse2grp5_sp5k43o==3 ),(dta$grp5e43v==3  & dta$spouse2grp5_sp5k43v==3),(dta$grp5e44==3  & dta$spouse2grp5_sp5k44==3),(dta$grp5e50==3  & dta$spouse2grp5_sp5k50==3)), na.rm=T) >0
dta$decseed_both_pl5[is.na(dta$grp5e43a) & is.na(dta$spouse2grp5_sp5k43a) & is.na(dta$grp5e43h) & is.na(dta$spouse2grp5_sp5k43h) & is.na(dta$grp5e43o) & is.na(dta$spouse2grp5_sp5k43o) 
& is.na(dta$grp5e43v) & is.na(dta$spouse2grp5_sp5k43v)
& is.na(dta$grp5e44) & is.na(dta$spouse2grp5_sp5k44)
& is.na(dta$grp5e50) & is.na(dta$spouse2grp5_sp5k50)
] <- NA
## decided on combining
dta[paste("deccombiner_man",paste("_pl",1:5, sep=""), sep="")] <- dta[paste("decfert_man",paste("_pl",1:5, sep=""), sep="")] * dta[paste("decseed_man",paste("_pl",1:5, sep=""), sep="")] 
dta[paste("deccombiner_woman",paste("_pl",1:5, sep=""), sep="")] <- dta[paste("decfert_woman",paste("_pl",1:5, sep=""), sep="")] * dta[paste("decseed_woman",paste("_pl",1:5, sep=""), sep="")] 
dta[paste("deccombiner_both",paste("_pl",1:5, sep=""), sep="")] <- dta[paste("decfert_both",paste("_pl",1:5, sep=""), sep="")] * dta[paste("decseed_both",paste("_pl",1:5, sep=""), sep="")] 

## decided on buying seed
dta$decbuyseed_man_pl1 <-  rowSums(cbind((dta$grp1a43x1==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f43x1==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decbuyseed_man_pl1[is.na( (dta$grp1a43x1==1 ) ) & is.na((dta$spouse2grp_sp1f43x1==1 ) )] <- NA
dta$decbuyseed_woman_pl1 <-  rowSums(cbind((dta$grp1a43x1==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1f43x1==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decbuyseed_woman_pl1[is.na((dta$grp1a43x1==1 ) ) & is.na((dta$spouse2grp_sp1f43x1==1 ))] <- NA
##woman says it is both and man says it is both
dta$decbuyseed_both_pl1 <-  rowSums(cbind((dta$grp1a43x1 == 3),(dta$spouse2grp_sp1f43x1 == 3)), na.rm=T) > 0
dta$decbuyseed_both_pl1[is.na((dta$grp1a43x1==3 )) & is.na((dta$spouse2grp_sp1f43x1==3 ))] <- NA

dta$decbuyseed_man_pl2 <-  rowSums(cbind((dta$grp2b43x1==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g43x1==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decbuyseed_man_pl2[is.na( (dta$grp2b43x1==1 ) ) & is.na((dta$spouse2grp_sp2g43x1==1 ) )] <- NA
dta$decbuyseed_woman_pl2 <-  rowSums(cbind((dta$grp2b43x1==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2g43x1==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decbuyseed_woman_pl2[is.na((dta$grp2b43x1==1 ) ) & is.na((dta$spouse2grp_sp2g43x1==1 ))] <- NA
##woman says it is both and man says it is both
dta$decbuyseed_both_pl2 <-  rowSums(cbind((dta$grp2b43x1 == 3),(dta$spouse2grp_sp2g43x1 == 3)), na.rm=T) > 0
dta$decbuyseed_both_pl2[is.na((dta$grp2b43x1==3 )) & is.na((dta$spouse2grp_sp2g43x1==3 ))] <- NA

dta$decbuyseed_man_pl3 <-  rowSums(cbind((dta$grp3c43x1==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h43x1==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decbuyseed_man_pl3[is.na( (dta$grp3c43x1==1 ) ) & is.na((dta$spouse2grp_sp3h43x1==1 ) )] <- NA
dta$decbuyseed_woman_pl3 <-  rowSums(cbind((dta$grp3c43x1==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3h43x1==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decbuyseed_woman_pl3[is.na((dta$grp3c43x1==1 ) ) & is.na((dta$spouse2grp_sp3h43x1==1 ))] <- NA
##woman says it is both and man says it is both
dta$decbuyseed_both_pl3 <-  rowSums(cbind((dta$grp3c43x1 == 3),(dta$spouse2grp_sp3h43x1 == 3)), na.rm=T) > 0
dta$decbuyseed_both_pl3[is.na((dta$grp3c43x1==3 )) & is.na((dta$spouse2grp_sp3h43x1==3 ))] <- NA

dta$decbuyseed_man_pl4 <-  rowSums(cbind((dta$grp4d43x1==1  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j43x1==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decbuyseed_man_pl4[is.na( (dta$grp4d43x1==1 ) ) & is.na((dta$spouse2group_sp4j43x1==1 ) )] <- NA
dta$decbuyseed_woman_pl4 <-  rowSums(cbind((dta$grp4d43x1==1  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4j43x1==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decbuyseed_woman_pl4[is.na((dta$grp4d43x1==1 ) ) & is.na((dta$spouse2group_sp4j43x1==1 ))] <- NA
##woman says it is both and man says it is both
dta$decbuyseed_both_pl4 <-  rowSums(cbind((dta$grp4d43x1 == 3),(dta$spouse2group_sp4j43x1 == 3)), na.rm=T) > 0
dta$decbuyseed_both_pl4[is.na((dta$grp4d43x1==3 )) & is.na((dta$spouse2group_sp4j43x1==3 ))] <- NA

dta$decbuyseed_man_pl5 <-  rowSums(cbind((dta$grp5e43x1==1  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k43x1==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decbuyseed_man_pl5[is.na( (dta$grp5e43x1==1 ) ) & is.na((dta$spouse2grp5_sp5k43x1==1 ) )] <- NA
dta$decbuyseed_woman_pl5 <-  rowSums(cbind((dta$grp5e43x1==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5k43x1==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decbuyseed_woman_pl5[is.na((dta$grp5e43x1==1 ) ) & is.na((dta$spouse2grp5_sp5k43x1==1 ))] <- NA
##woman says it is both and man says it is both
dta$decbuyseed_both_pl5 <-  rowSums(cbind((dta$grp5e43x1 == 3),(dta$spouse2grp5_sp5k43x1 == 3)), na.rm=T) > 0
dta$decbuyseed_both_pl4[is.na((dta$grp5e43x1==3 )) & is.na((dta$spouse2grp5_sp5k43x1==3 ))] <- NA

### decide on use of chemicals

dta$decchem_man_pl1 <-  rowSums(cbind((dta$grp1a56==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f56==1  & dta$person_interviewed=="woman"), (dta$grp1a58==1  & dta$person_interviewed=="man"),(dta$spouse2grp_sp1f58==1  & dta$person_interviewed=="woman")), na.rm=T) >0
dta$decchem_man_pl1[is.na(dta$grp1a56) & is.na(dta$spouse2grp_sp1f56) & is.na(dta$grp1a58) & is.na(dta$spouse2grp_sp1f58)] <- NA

dta$decchem_man_pl2 <-  rowSums(cbind((dta$grp2b56==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g56==1  & dta$person_interviewed=="woman"), (dta$grp2b58==1  & dta$person_interviewed=="man"),(dta$spouse2grp_sp2g58==1  & dta$person_interviewed=="woman")), na.rm=T) >0
dta$decchem_man_pl2[is.na(dta$grp2b56) & is.na(dta$spouse2grp_sp2g56) & is.na(dta$grp2b58) & is.na(dta$spouse2grp_sp2g58)] <- NA

dta$decchem_man_pl3 <-  rowSums(cbind((dta$grp3c56==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h56==1  & dta$person_interviewed=="woman"), (dta$grp3c58==1  & dta$person_interviewed=="man"),(dta$spouse2grp_sp3h58==1  & dta$person_interviewed=="woman")), na.rm=T) >0
dta$decchem_man_pl3[is.na(dta$grp3c56) & is.na(dta$spouse2grp_sp3h56) & is.na(dta$grp3c58) & is.na(dta$spouse2grp_sp3h58)] <- NA

dta$decchem_man_pl4 <-  rowSums(cbind((dta$grp4d56==1  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j56==1  & dta$person_interviewed=="woman"), (dta$grp4d58==1  & dta$person_interviewed=="man"),(dta$spouse2group_sp4j58==1  & dta$person_interviewed=="woman")), na.rm=T) >0
dta$decchem_man_pl4[is.na(dta$grp4d56) & is.na(dta$spouse2group_sp4j56) & is.na(dta$grp4d58) & is.na(dta$spouse2group_sp4j58)] <- NA

dta$decchem_man_pl5 <-  rowSums(cbind((dta$grp4d56==1  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k56==1  & dta$person_interviewed=="woman"), (dta$grp4d58==1  & dta$person_interviewed=="man"),(dta$spouse2grp5_sp5k58==1  & dta$person_interviewed=="woman")), na.rm=T) >0
dta$decchem_man_pl5[is.na(dta$grp4d56) & is.na(dta$spouse2grp5_sp5k56) & is.na(dta$grp4d58) & is.na(dta$spouse2grp5_sp5k58)] <- NA

########
dta$decchem_woman_pl1 <-  rowSums(cbind((dta$grp1a56==1  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp1f56==1  & dta$person_interviewed=="man"), (dta$grp1a58==1  & dta$person_interviewed=="woman"),(dta$spouse2grp_sp1f58==1  & dta$person_interviewed=="man")), na.rm=T) >0
dta$decchem_woman_pl1[is.na(dta$grp1a56) & is.na(dta$spouse2grp_sp1f56) & is.na(dta$grp1a58) & is.na(dta$spouse2grp_sp1f58)] <- NA

dta$decchem_woman_pl2 <-  rowSums(cbind((dta$grp2b56==1  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp2g56==1  & dta$person_interviewed=="man"), (dta$grp2b58==1  & dta$person_interviewed=="woman"),(dta$spouse2grp_sp2g58==1  & dta$person_interviewed=="man")), na.rm=T) >0
dta$decchem_woman_pl2[is.na(dta$grp2b56) & is.na(dta$spouse2grp_sp2g56) & is.na(dta$grp2b58) & is.na(dta$spouse2grp_sp2g58)] <- NA

dta$decchem_woman_pl3 <-  rowSums(cbind((dta$grp3c56==1  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp3h56==1  & dta$person_interviewed=="man"), (dta$grp3c58==1  & dta$person_interviewed=="woman"),(dta$spouse2grp_sp3h58==1  & dta$person_interviewed=="man")), na.rm=T) >0
dta$decchem_woman_pl3[is.na(dta$grp3c56) & is.na(dta$spouse2grp_sp3h56) & is.na(dta$grp3c58) & is.na(dta$spouse2grp_sp3h58)] <- NA

dta$decchem_woman_pl4 <-  rowSums(cbind((dta$grp4d56==1  & dta$person_interviewed=="woman") , (dta$spouse2group_sp4j56==1  & dta$person_interviewed=="man"), (dta$grp4d58==1  & dta$person_interviewed=="woman"),(dta$spouse2group_sp4j58==1  & dta$person_interviewed=="man")), na.rm=T) >0
dta$decchem_woman_pl4[is.na(dta$grp4d56) & is.na(dta$spouse2group_sp4j56) & is.na(dta$grp4d58) & is.na(dta$spouse2group_sp4j58)] <- NA

dta$decchem_woman_pl5 <-  rowSums(cbind((dta$grp4d56==1  & dta$person_interviewed=="woman") , (dta$spouse2grp5_sp5k56==1  & dta$person_interviewed=="man"), (dta$grp4d58==1  & dta$person_interviewed=="woman"),(dta$spouse2grp5_sp5k58==1  & dta$person_interviewed=="man")), na.rm=T) >0
dta$decchem_woman_pl5[is.na(dta$grp4d56) & is.na(dta$spouse2grp5_sp5k56) & is.na(dta$grp4d58) & is.na(dta$spouse2grp5_sp5k58)] <- NA

##
dta$decchem_both_pl1 <-  rowSums(cbind((dta$grp1a56==3  & dta$spouse2grp_sp1f56==3), (dta$grp1a58==3  & dta$spouse2grp_sp1f58==3 )), na.rm=T) >0
dta$decchem_both_pl1[is.na(dta$grp1a56) & is.na(dta$spouse2grp_sp1f56) & is.na(dta$grp1a58) & is.na(dta$spouse2grp_sp1f58)] <- NA

dta$decchem_both_pl2 <-  rowSums(cbind((dta$grp2b56==3 & dta$spouse2grp_sp2g56==3),(dta$grp2b58==3 & dta$spouse2grp_sp2g58==3)), na.rm=T) >0
dta$decchem_both_pl2[is.na(dta$grp2b56) & is.na(dta$spouse2grp_sp2g56) & is.na(dta$grp2b58) & is.na(dta$spouse2grp_sp2g58)] <- NA

dta$decchem_both_pl3 <-  rowSums(cbind((dta$grp3c56==3  & dta$spouse2grp_sp3h56==3),(dta$grp3c58==3  & dta$spouse2grp_sp3h58==3)), na.rm=T) >0
dta$decchem_both_pl3[is.na(dta$grp3c56) & is.na(dta$spouse2grp_sp3h56) & is.na(dta$grp3c58) & is.na(dta$spouse2grp_sp3h58)] <- NA

dta$decchem_both_pl4 <-  rowSums(cbind((dta$grp4d56==3  & dta$spouse2group_sp4j56==3), (dta$grp4d58==3  & dta$spouse2group_sp4j58==3 )), na.rm=T) >0
dta$decchem_both_pl4[is.na(dta$grp4d56) & is.na(dta$spouse2group_sp4j56) & is.na(dta$grp4d58) & is.na(dta$spouse2group_sp4j58)] <- NA

dta$decchem_both_pl5 <-  rowSums(cbind((dta$grp4d56==3  & dta$spouse2grp5_sp5k56==3), (dta$grp4d58==3  & dta$spouse2grp5_sp5k58==3 )), na.rm=T) >0
dta$decchem_both_pl5[is.na(dta$grp4d56) & is.na(dta$spouse2grp5_sp5k56) & is.na(dta$grp4d58) & is.na(dta$spouse2grp5_sp5k58)] <- NA

################################################################ KNOWLEDGE ##############################################################
### knowledge at aggreagate level - defined as follows: weak - if at least one person got the answer correct
dta$know_space <- rowSums(dta[c("a1","spouse2f1")] == 1, na.rm=T) >0
dta$know_space[is.na(dta$a1) & is.na(dta$spouse2f1)] <- NA 

dta$know_combine <- rowSums(dta[c("a2", "spouse2f2")] == 3, na.rm=T) >0
dta$know_combine[is.na(dta$a2)  & is.na(dta$spouse2f2)] <- NA

dta$know_weed <-  rowSums(dta[c("a3","spouse2f3")]== 2, na.rm=T) > 0
dta$know_weed[is.na(dta$a3)  & is.na(dta$spouse2f3)] <- NA

dta$know_armyworm <- rowSums(dta[c("a4","spouse2f4")] == 3, na.rm=T) > 0 
dta$know_armyworm[is.na(dta$a4) & is.na(dta$spouse2f4)] <- NA

### knowledge of man:

dta$know_space_m <- NA
dta$know_space_m[!is.na(dta$person_interviewed) & dta$person_interviewed == "man"] <- dta$a1[!is.na(dta$person_interviewed) & dta$person_interviewed == "man"] ==1
dta$know_space_m[!is.na(dta$person_interviewed) & dta$person_interviewed == "woman"] <- dta$spouse2f1[!is.na(dta$person_interviewed) & dta$person_interviewed == "woman"] ==1

dta$know_combine_m <- NA
dta$know_combine_m[!is.na(dta$person_interviewed) & dta$person_interviewed == "man"] <- dta$a2[!is.na(dta$person_interviewed) & dta$person_interviewed == "man"] ==3
dta$know_combine_m[!is.na(dta$person_interviewed) & dta$person_interviewed == "woman"] <- dta$spouse2f2[!is.na(dta$person_interviewed) & dta$person_interviewed == "woman"] == 3

dta$know_weed_m <- NA
dta$know_weed_m[!is.na(dta$person_interviewed) & dta$person_interviewed == "man"] <- dta$a3[!is.na(dta$person_interviewed) & dta$person_interviewed == "man"] ==2
dta$know_weed_m[!is.na(dta$person_interviewed) & dta$person_interviewed == "woman"] <- dta$spouse2f3[!is.na(dta$person_interviewed) & dta$person_interviewed == "woman"] ==2

dta$know_armyworm_m <- NA
dta$know_armyworm_m[!is.na(dta$person_interviewed) & dta$person_interviewed == "man"] <- dta$a4[!is.na(dta$person_interviewed) & dta$person_interviewed == "man"] ==3
dta$know_armyworm_m[!is.na(dta$person_interviewed) & dta$person_interviewed == "woman"] <- dta$spouse2f4[!is.na(dta$person_interviewed) & dta$person_interviewed == "woman"] ==3

### knowledge of woman:

dta$know_space_w <- NA
dta$know_space_w[!is.na(dta$person_interviewed) & dta$person_interviewed == "woman"] <- dta$a1[!is.na(dta$person_interviewed) & dta$person_interviewed == "woman"] ==1
dta$know_space_w[!is.na(dta$person_interviewed) & dta$person_interviewed == "man"] <- dta$spouse2f1[!is.na(dta$person_interviewed) & dta$person_interviewed == "man"] ==1

dta$know_combine_w <- NA
dta$know_combine_w[!is.na(dta$person_interviewed) & dta$person_interviewed == "woman"] <- dta$a2[!is.na(dta$person_interviewed) & dta$person_interviewed == "woman"] ==3
dta$know_combine_w[!is.na(dta$person_interviewed) & dta$person_interviewed == "man"] <- dta$spouse2f2[!is.na(dta$person_interviewed) & dta$person_interviewed == "man"] == 3

dta$know_weed_w <- NA
dta$know_weed_w[!is.na(dta$person_interviewed) & dta$person_interviewed == "woman"] <- dta$a3[!is.na(dta$person_interviewed) & dta$person_interviewed == "woman"] ==2
dta$know_weed_w[!is.na(dta$person_interviewed) & dta$person_interviewed == "man"] <- dta$spouse2f3[!is.na(dta$person_interviewed) & dta$person_interviewed == "man"] ==2


dta$know_armyworm_w <- NA
dta$know_armyworm_w[!is.na(dta$person_interviewed) & dta$person_interviewed == "woman"] <- dta$a4[!is.na(dta$person_interviewed) & dta$person_interviewed == "woman"] ==3
dta$know_armyworm_w[!is.na(dta$person_interviewed) & dta$person_interviewed == "man"] <- dta$spouse2f4[!is.na(dta$person_interviewed) & dta$person_interviewed == "man"] ==3
###export knowledge by man and women here for plotting panel data
write.csv(dta[c("hhid", "messenger", "recipient", "know_space_m", "know_combine_m", "know_weed_m", "know_space_w", "know_combine_w", "know_weed_w")],"/home/bjvca/data/projects/digital green/endline/data/working/knowledge_3.csv")



################################################################ PRACTICES ##############################################################

### planted first day days after rain
names(dta)[names(dta) == 'grp1days19'] <- 'grp1days1'

dta[c("grp1days1","grp2days2","grp3days3","grp4days4", "grp5days5")] <- lapply(dta[c("grp1days1","grp2days2","grp3days3","grp4days4", "grp5days5")], function(x) replace(x, x ==999, NA) )

dta[c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5")] <- lapply(dta[c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5")], function(x) replace(x,x ==999, NA) )

dta$day_one_sp1 <- rowSums(dta[c("grp1days1","grp2days2","grp3days3","grp4days4", "grp5days5")] ==1 , na.rm=T) >0
dta$day_one_sp1[is.na(dta$grp1days1) & is.na(dta$grp2days2) & is.na(dta$grp3days3) & is.na(dta$grp4days4) & is.na(dta$grp5days5)] <- NA

dta$day_one_sp2 <- rowSums(dta[c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5")] ==1 , na.rm=T) >0
dta$day_one_sp2[is.na(dta$spouse2grp_sp1days1) & is.na(dta$spouse2grp_sp2days_sp2) & is.na(dta$spouse2grp_sp3days_sp3) & is.na(dta$spouse2group_sp4dayssp4) & is.na(dta$spouse2grp5_sp5dayssp5)] <- NA

dta$day_one <- rowSums(dta[c("day_one_sp1","day_one_sp2")], na.rm=T) > 0
dta$day_one[is.na(dta$day_one_sp1) & is.na(dta$day_one_sp2)] <- NA

### use recommended spacing on *any* plot
dta$space_sp1 <- rowSums(dta[c("grp1a201","grp2b201","grp3c201","grp4d201","grp5e201")], na.rm=T) > 0
dta$space_sp1[is.na(dta$grp1a201) & is.na(dta$grp2b201) & is.na(dta$grp3c201) & is.na(dta$grp4d201) & is.na(dta$grp5e201)] <- NA

dta$space_sp2 <- rowSums(dta[c("spouse2grp_sp1f201","spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201","spouse2grp5_sp5k201")], na.rm=T) >0 
dta$space_sp2[is.na(dta$spouse2grp_sp1f201) & is.na(dta$spouse2grp_sp2g201) & is.na(dta$spouse2grp_sp3h201) & is.na(dta$spouse2group_sp4j201) & is.na(dta$spouse2grp5_sp5k201)] <- NA 

dta$space <- rowSums(dta[c("space_sp1","space_sp2")], na.rm=T) > 0
dta$space[is.na(dta$space_sp1) & is.na(dta$space_sp2)] <- NA
#recommended way to fight striga

dta$striga_sp1 <- rowSums(dta[c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241")], na.rm=T) > 0
dta$striga_sp1[is.na(dta$grp1a241) & is.na(dta$grp2b241) & is.na(dta$grp3c241) & is.na(dta$grp4d241) & is.na(dta$grp5e241)] <- NA
dta$striga_sp2 <-  rowSums(dta[c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241")], na.rm=T) >0 
dta$striga_sp2[is.na(dta$spouse2grp_sp1f241) & is.na(dta$spouse2grp_sp2g241) & is.na(dta$spouse2grp_sp3h241) & is.na(dta$spouse2group_sp4j241) & is.na(dta$spouse2grp5_sp5k241)] <- NA
dta$striga <- rowSums(dta[c("striga_sp1","striga_sp2")], na.rm=T) > 0
dta$striga[is.na(dta$striga_sp1) & is.na(dta$striga_sp2)] <- NA

### use recommended weeding practice: we recommend first weeding after 18-20 days, which is in the 3rd week - option 3
dta$weed_sp1 <- rowSums(dta[c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26")] == 3, na.rm=T) > 0
dta$weed_sp1[is.na(dta$grp1a26) & is.na(dta$grp2b26) & is.na(dta$grp3c26) & is.na(dta$grp4d26) & is.na(dta$grp5e26)] <- NA
dta$weed_sp2 <- rowSums(dta[c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26")] == 3  ,na.rm=T) >0 
dta$weed_sp2[is.na(dta$spouse2grp_sp1f26) & is.na(dta$spouse2grp_sp2g26) & is.na(dta$spouse2grp_sp3h26) & is.na(dta$spouse2group_sp4j26) & is.na(dta$spouse2grp5_sp5k26)] <- NA
dta$weed <- rowSums(dta[c("weed_sp1","weed_sp2")], na.rm=T) > 0
dta$weed[is.na(dta$weed_sp1) & is.na(dta$weed_sp2)] <- NA

### fertilizer use on at least one plot
dta$fert_sp1 <- rowSums(dta[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")] == "Yes", na.rm=T ) > 0
dta$fert_sp1[is.na(dta$grp1a29) & is.na(dta$grp2b29) & is.na(dta$grp3c29) & is.na(dta$grp4d29) & is.na(dta$grp5e29)] <- NA 
dta$fert_sp2 <- rowSums(dta[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29")] == "Yes", na.rm=T) > 0
dta$fert_sp2[is.na(dta$spouse2grp_sp1f29) & is.na(dta$spouse2grp_sp2g29) & is.na(dta$spouse2grp_sp3h29) & is.na(dta$spouse2group_sp4j29) & is.na(dta$spouse2grp5_sp5k29)] <- NA
dta$fert <- rowSums(dta[c("fert_sp1","fert_sp2")], na.rm=T) > 0
dta$fert[is.na(dta$fert_sp1) & is.na(dta$fert_sp2)] <- NA

### use of improved seed (OPV or hybrid) on at least one plot
dta$impseed_sp1 <- rowSums(dta[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")] == "Yes", na.rm=T) > 0
dta$impseed_sp1[is.na(dta$grp1a42) & is.na(dta$grp2b42) & is.na(dta$grp3c42) & is.na(dta$grp4d42) & is.na(dta$grp5e42)] <- NA 
dta$impseed_sp2 <- rowSums(dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42")] == "Yes", na.rm=T) > 0
dta$impseed_sp2[is.na(dta$spouse2grp_sp1f42) & is.na(dta$spouse2grp_sp2g42) & is.na(dta$spouse2grp_sp3h42) & is.na(dta$spouse2group_sp4j42) & is.na(dta$spouse2grp5_sp5k42)] <- NA
dta$impseed <- rowSums(dta[c("impseed_sp1","impseed_sp2")], na.rm=T) > 0
dta$impseed[is.na(dta$impseed_sp1) & is.na(dta$impseed_sp2)] <- NA

## was seed purchased? Assume that those who did not use seed did not purchase
dta$bought_seed_sp1 <- rowSums(dta[c("grp1seed_purchase1","grp2seed_purchase2","grp3seed_purchase3","grp4seed_purchase4","grp5seed_purchase5")]=="Yes", na.rm=T) > 0
dta$bought_seed_sp2<-  rowSums(dta[c("spouse2grp_sp1seed_purchasesp1","spouse2grp_sp2seed_purchase_sp2","spouse2grp_sp3seed_purchasesp3","spouse2group_sp4seed_purchasesp4","spouse2grp5_sp5seed_purchasesp5")]=="Yes",na.rm=T) > 0

dta$bought_seed <- rowSums(dta[c("bought_seed_sp1", "bought_seed_sp2")], na.rm=T) > 0
dta$bought_seed[is.na(dta$bought_seed_sp1) & is.na(dta$bought_seed_sp2)] <- NA

### combiner: use improved seed + fertilizer on at least one plot (A29==yes & A42==yes)

dta$combiner_sp1 <- rowSums( cbind((dta$grp1a29=="Yes" & dta$grp1a42 == "Yes"),(dta$grp2b29=="Yes" & dta$grp2b42 == "Yes"),(dta$grp3c29=="Yes" & dta$grp3c42 == "Yes"),(dta$grp4d29=="Yes" & dta$grp4d42 == "Yes"),(dta$grp5e29=="Yes" & dta$grp5e42 == "Yes")), na.rm=T) > 0
dta$combiner_sp1[is.na(dta$grp1a29) & is.na(dta$grp1a42) & is.na(dta$grp2b29) & is.na(dta$grp2b42) & is.na(dta$grp3c29) & is.na(dta$grp3c42) & is.na(dta$grp4d29) & is.na(dta$grp4d42) & is.na(dta$grp5e29) & is.na(dta$grp5e42)] <- NA

dta$combiner_sp2 <- rowSums( cbind((dta$spouse2grp_sp1f29=="Yes" & dta$spouse2grp_sp1f42 == "Yes"),(dta$spouse2grp_sp2g29=="Yes" & dta$spouse2grp_sp2g42 == "Yes"),(dta$spouse2grp_sp3h29=="Yes" & dta$spouse2grp_sp3h42 == "Yes"),(dta$spouse2group_sp4j29=="Yes" & dta$spouse2group_sp4j42 == "Yes"),(dta$spouse2grp5_sp5k29=="Yes" & dta$spouse2grp5_sp5k42 == "Yes")), na.rm=T) > 0
dta$combiner_sp2[ is.na(dta$spouse2grp_sp1f29) & is.na(dta$spouse2grp_sp1f42) & is.na(dta$spouse2grp_sp2g29) & is.na(dta$spouse2grp_sp2g42) & is.na(dta$spouse2grp_sp3h29) & is.na(dta$spouse2grp_sp3h42) & is.na(dta$spouse2group_sp4j29) & is.na(dta$spouse2group_sp4j42) & is.na(dta$spouse2grp5_sp5k29) & is.na(dta$spouse2grp5_sp5k42)] <- NA

dta$combiner <- rowSums(dta[c("combiner_sp1","combiner_sp2")], na.rm=T) >0
dta$combiner[is.na(dta$combiner_sp1) & is.na(dta$combiner_sp2)] <- NA
### use of chemical on at least one plot
dta$chem_sp1 <- rowSums(dta[c("grp1a55a" ,"grp2b55b", "grp3c55b","grp4d55b","grp5e55b")] == "Yes", na.rm=T) > 0
dta$chem_sp1[is.na(dta$grp1a55a) & is.na(dta$grp2b55b) & is.na(dta$grp3c55b) & is.na(dta$grp4d55b) & is.na(dta$grp5e55b)] <- NA

dta$chem_sp2 <- rowSums(dta[ c("spouse2grp_sp1f55a","spouse2grp_sp2g55b","spouse2grp_sp3h55b","spouse2group_sp4j55b","spouse2grp5_sp5k55b")]=="Yes", na.rm=T) > 0
dta$chem_sp2[is.na(dta$spouse2grp_sp1f55a) & is.na(dta$spouse2grp_sp2g55b) & is.na(dta$spouse2grp_sp3h55b) & is.na(dta$spouse2group_sp4j55b) & is.na(dta$spouse2grp5_sp5k55b)] <- NA

dta$chem <- rowSums(dta[c("chem_sp1","chem_sp2")], na.rm=T) > 0
dta$chem[is.na(dta$chem_sp1) & is.na(dta$chem_sp2)] <- NA

## hired labour on at least one plot
dta$labour_sp1 <- rowSums(dta[c("grp1a151","grp2b55b","grp3c55b","grp4d55b","grp5e55b")] =="Yes", na.rm=T) > 0
dta$labour_sp1[is.na(dta$grp1a151) & is.na(dta$grp2b55b) & is.na(dta$grp3c55b) & is.na(dta$grp4d55b) & is.na(dta$grp5e55b)] <- NA

dta$labour_sp2 <-  rowSums(dta[c("spouse2grp_sp1f151","spouse2grp_sp2g151","spouse2grp_sp3h151","spouse2group_sp4j151","spouse2grp5_sp5k151")] =="Yes", na.rm=T) > 0
dta$labour_sp2[is.na(dta$spouse2grp_sp1f151) & is.na(dta$spouse2grp_sp2g151) & is.na(dta$spouse2grp_sp3h151) & is.na(dta$spouse2group_sp4j151) & is.na(dta$spouse2grp5_sp5k151)] <- NA

dta$labour <- rowSums(dta[c("labour_sp1","labour_sp2")], na.rm=T) > 0
dta$labour[is.na(dta$labour_sp1) & is.na(dta$labour_sp2)] <- NA

######################################practices: male managed practices
dta$space_sp1 <- rowSums((dta[c("grp1a201","grp2b201","grp3c201","grp4d201","grp5e201")]>0) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$space_sp1[rowSums(is.na((dta[c("grp1a201","grp2b201","grp3c201","grp4d201","grp5e201")]>0) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$space_sp1[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$space_sp2 <- rowSums((dta[c("spouse2grp_sp1f201", "spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201","spouse2grp5_sp5k201")]>0)*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$space_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f201", "spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201","spouse2grp5_sp5k201")]>0)*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$space_sp2[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$space_mm <-  rowSums(dta[c("space_sp1","space_sp2")], na.rm=T) > 0
dta$space_mm[is.na(dta$space_sp1) & is.na(dta$space_sp2)] <- NA

# male managed
dta$striga_sp1 <- rowSums((dta[c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241")]>0) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$striga_sp1[rowSums(is.na((dta[c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241")]>0) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$striga_sp1[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$striga_sp2 <- rowSums((dta[c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241")]>0)*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$striga_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241")]>0)*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$striga_sp2[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$striga_mm <-  rowSums(dta[c("striga_sp1","striga_sp2")], na.rm=T) > 0
dta$striga_mm[is.na(dta$striga_sp1) & is.na(dta$striga_sp2)] <- NA

##
dta$weed_sp1 <- rowSums((dta[c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26")] == 3) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$weed_sp1[rowSums(is.na((dta[c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26")] == 3) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$weed_sp1[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$weed_sp2 <- rowSums((dta[c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26")] == 3)*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$weed_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26")] == 3)*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$weed_sp2[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$weed_mm <-  rowSums(dta[c("weed_sp1","weed_sp2")], na.rm=T) > 0
dta$weed_mm[is.na(dta$weed_sp1) & is.na(dta$weed_sp2)] <- NA
##
dta$fert_sp1 <- rowSums((dta[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")] == "Yes") *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$fert_sp1[rowSums(is.na((dta[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")] == "Yes") *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$fert_sp1[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_sp2 <- rowSums((dta[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29")] == "Yes")*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$fert_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29")] == "Yes")*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$fert_sp2[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_mm <-  rowSums(dta[c("fert_sp1","fert_sp2")], na.rm=T) > 0
dta$fert_mm[is.na(dta$fert_sp1) & is.na(dta$fert_sp2)] <- NA


###
dta$impseed_sp1 <- rowSums((dta[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")] == "Yes") *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$impseed_sp1[rowSums(is.na((dta[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")] == "Yes") *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$impseed_sp1[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$impseed_sp2 <- rowSums((dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42")] == "Yes")*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$impseed_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42")] == "Yes")*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$impseed_sp2[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$impseed_mm <-  rowSums(dta[c("impseed_sp1","impseed_sp2")], na.rm=T) > 0
dta$impseed_mm[is.na(dta$impseed_sp1) & is.na(dta$impseed_sp2)] <- NA

###
dta$bought_seed_sp1 <- FALSE
dta$bought_seed_sp1 <- rowSums((cbind(dta$grp1seed_purchase1 =="Yes",dta$grp2seed_purchase2 =="Yes",dta$grp3seed_purchase3 =="Yes",dta$grp4seed_purchase4 =="Yes",dta$grp5seed_purchase5 =="Yes")) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)

dta$bought_seed_sp1[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA
dta$bought_seed_sp2 <- FALSE
dta$bought_seed_sp2 <- rowSums((cbind(dta$spouse2grp_sp1seed_purchasesp1=="Yes", dta$spouse2grp_sp2seed_purchase_sp2=="Yes", dta$spouse2grp_sp3seed_purchasesp3=="Yes", dta$spouse2group_sp4seed_purchasesp4=="Yes", dta$spouse2grp5_sp5seed_purchasesp5=="Yes"))*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)

dta$bought_seed_sp2[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$bought_seed_mm <-  rowSums(dta[c("bought_seed_sp1","bought_seed_sp2")], na.rm=T) > 0
dta$bought_seed_mm[is.na(dta$bought_seed_sp1) & is.na(dta$bought_seed_sp2)] <- NA

#
dta$combiner_sp1 <- rowSums(cbind((dta$grp1a29=="Yes" & dta$grp1a42 == "Yes"),(dta$grp2b29=="Yes" & dta$grp2b42 == "Yes"),(dta$grp3c29=="Yes" & dta$grp3c42 == "Yes"),(dta$grp4d29=="Yes" & dta$grp4d42 == "Yes"),(dta$grp5e29=="Yes" & dta$grp5e42 == "Yes")) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$combiner_sp1[rowSums(is.na(cbind((dta$grp1a29=="Yes" & dta$grp1a42 == "Yes"),(dta$grp2b29=="Yes" & dta$grp2b42 == "Yes"),(dta$grp3c29=="Yes" & dta$grp3c42 == "Yes"),(dta$grp4d29=="Yes" & dta$grp4d42 == "Yes"),(dta$grp5e29=="Yes" & dta$grp5e42 == "Yes")) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$combiner_sp1[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$combiner_sp2 <- rowSums( cbind((dta$spouse2grp_sp1f29=="Yes" & dta$spouse2grp_sp1f42 == "Yes"),(dta$spouse2grp_sp2g29=="Yes" & dta$spouse2grp_sp2g42 == "Yes"),(dta$spouse2grp_sp3h29=="Yes" & dta$spouse2grp_sp3h42 == "Yes"),(dta$spouse2group_sp4j29=="Yes" & dta$spouse2group_sp4j42 == "Yes"),(dta$spouse2grp5_sp5k29=="Yes" & dta$spouse2grp5_sp5k42 == "Yes"))*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$combiner_sp2[rowSums(is.na(( cbind((dta$spouse2grp_sp1f29=="Yes" & dta$spouse2grp_sp1f42 == "Yes"),(dta$spouse2grp_sp2g29=="Yes" & dta$spouse2grp_sp2g42 == "Yes"),(dta$spouse2grp_sp3h29=="Yes" & dta$spouse2grp_sp3h42 == "Yes"),(dta$spouse2group_sp4j29=="Yes" & dta$spouse2group_sp4j42 == "Yes"),(dta$spouse2grp5_sp5k29=="Yes" & dta$spouse2grp5_sp5k42 == "Yes")))*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$combiner_sp2[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$combiner_mm <-  rowSums(dta[c("combiner_sp1","combiner_sp2")], na.rm=T) > 0
dta$combiner_mm[is.na(dta$combiner_sp1) & is.na(dta$combiner_sp2)] <- NA

dta$chem_sp1 <- rowSums(cbind((dta$grp1a55a == "Yes") , (dta$grp2b55b  == "Yes"), (dta$grp3c55b  == "Yes"), (dta$grp4d55b  == "Yes") , (dta$grp5e55b  == "Yes")) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$chem_sp1[rowSums(is.na(cbind((dta$grp1a55a == "Yes") , (dta$grp2b55b  == "Yes"), (dta$grp3c55b  == "Yes"), (dta$grp4d55b  == "Yes") , (dta$grp5e55b  == "Yes")) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$chem_sp1[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$chem_sp2 <- rowSums( cbind((dta$spouse2grp_sp1f55a == "Yes"), (dta$spouse2grp_sp2g55b  == "Yes"), (dta$spouse2grp_sp3h55b  == "Yes"), (dta$spouse2group_sp4j55b  == "Yes") , (dta$spouse2grp5_sp5k55b  == "Yes"))*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$chem_sp2[rowSums(is.na( cbind((dta$spouse2grp_sp1f55a == "Yes"), (dta$spouse2grp_sp2g55b  == "Yes"), (dta$spouse2grp_sp3h55b  == "Yes"), (dta$spouse2group_sp4j55b  == "Yes") , (dta$spouse2grp5_sp5k55b  == "Yes"))*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$chem_sp2[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$chem_mm <-  rowSums(dta[c("chem_sp1","chem_sp2")], na.rm=T) > 0
dta$chem_mm[is.na(dta$chem_sp1) & is.na(dta$chem_sp2)] <- NA

dta$labour_sp1 <- rowSums(cbind((dta$grp1a151 == "Yes"), (dta$grp2b55b  == "Yes"), (dta$grp3c55b  == "Yes"), (dta$grp4d55b  == "Yes"), (dta$grp5e55b  == "Yes")) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$labour_sp1[rowSums(is.na(cbind((dta$grp1a151 == "Yes"), (dta$grp2b55b  == "Yes"), (dta$grp3c55b  == "Yes"), (dta$grp4d55b  == "Yes"), (dta$grp5e55b  == "Yes")) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$labour_sp1[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$labour_sp2 <- rowSums( cbind((dta$spouse2grp_sp1f151 == "Yes"),(dta$spouse2grp_sp2g151  == "Yes") , (dta$spouse2grp_sp3h151  == "Yes"), (dta$spouse2group_sp4j151  == "Yes"), (dta$spouse2grp5_sp5k151  == "Yes"))*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$labour_sp2[rowSums(is.na( cbind((dta$spouse2grp_sp1f151 == "Yes"),(dta$spouse2grp_sp2g151  == "Yes") , (dta$spouse2grp_sp3h151  == "Yes"), (dta$spouse2group_sp4j151  == "Yes"), (dta$spouse2grp5_sp5k151  == "Yes"))*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$labour_sp2[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$labour_mm <-  rowSums(dta[c("labour_sp1","labour_sp2")], na.rm=T) > 0
dta$labour_mm[is.na(dta$labour_sp1) & is.na(dta$labour_sp2)] <- NA

#######################PRACTICES - Woman managed plots
# male managed practices
dta$space_sp1 <- rowSums((dta[c("grp1a201","grp2b201","grp3c201","grp4d201","grp5e201")]>0) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$space_sp1[rowSums(is.na((dta[c("grp1a201","grp2b201","grp3c201","grp4d201","grp5e201")]>0) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$space_sp1[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$space_sp2 <- rowSums((dta[c("spouse2grp_sp1f201", "spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201","spouse2grp5_sp5k201")]>0)*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$space_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f201", "spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201","spouse2grp5_sp5k201")]>0)*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$space_sp2[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$space_wm <-  rowSums(dta[c("space_sp1","space_sp2")], na.rm=T) > 0
dta$space_wm[is.na(dta$space_sp1) & is.na(dta$space_sp2)] <- NA

# male managed
dta$striga_sp1 <- rowSums((dta[c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241")]>0) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$striga_sp1[rowSums(is.na((dta[c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241")]>0) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$striga_sp1[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$striga_sp2 <- rowSums((dta[c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241")]>0)*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$striga_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241")]>0)*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$striga_sp2[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$striga_wm <-  rowSums(dta[c("striga_sp1","striga_sp2")], na.rm=T) > 0
dta$striga_wm[is.na(dta$striga_sp1) & is.na(dta$striga_sp2)] <- NA

##
dta$weed_sp1 <- rowSums((dta[c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26")] == 3) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$weed_sp1[rowSums(is.na((dta[c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26")] == 3) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$weed_sp1[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$weed_sp2 <- rowSums((dta[c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26")] == 3)*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$weed_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26")] == 3)*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$weed_sp2[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$weed_wm <-  rowSums(dta[c("weed_sp1","weed_sp2")], na.rm=T) > 0
dta$weed_wm[is.na(dta$weed_sp1) & is.na(dta$weed_sp2)] <- NA
##
dta$fert_sp1 <- rowSums((dta[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")] == "Yes") *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$fert_sp1[rowSums(is.na((dta[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")] == "Yes") *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$fert_sp1[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_sp2 <- rowSums((dta[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29")] == "Yes")*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$fert_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29")] == "Yes")*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$fert_sp2[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_wm <-  rowSums(dta[c("fert_sp1","fert_sp2")], na.rm=T) > 0
dta$fert_wm[is.na(dta$fert_sp1) & is.na(dta$fert_sp2)] <- NA


###
dta$impseed_sp1 <- rowSums((dta[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")] == "Yes") *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$impseed_sp1[rowSums(is.na((dta[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")] == "Yes") *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$impseed_sp1[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$impseed_sp2 <- rowSums((dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42")] == "Yes")*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$impseed_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42")] == "Yes")*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$impseed_sp2[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$impseed_wm <-  rowSums(dta[c("impseed_sp1","impseed_sp2")], na.rm=T) > 0
dta$impseed_wm[is.na(dta$impseed_sp1) & is.na(dta$impseed_sp2)] <- NA

###
dta$bought_seed_sp1 <- FALSE
dta$bought_seed_sp1 <- rowSums((cbind(dta$grp1seed_purchase1 =="Yes",dta$grp2seed_purchase2 =="Yes",dta$grp3seed_purchase3 =="Yes",dta$grp4seed_purchase4 =="Yes",dta$grp5seed_purchase5 =="Yes")) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)

dta$bought_seed_sp1[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA
dta$bought_seed_sp2 <- FALSE
dta$bought_seed_sp2 <- rowSums((cbind(dta$spouse2grp_sp1seed_purchasesp1=="Yes", dta$spouse2grp_sp2seed_purchase_sp2=="Yes", dta$spouse2grp_sp3seed_purchasesp3=="Yes", dta$spouse2group_sp4seed_purchasesp4=="Yes", dta$spouse2grp5_sp5seed_purchasesp5=="Yes"))*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)

dta$bought_seed_sp2[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$bought_seed_wm <-  rowSums(dta[c("bought_seed_sp1","bought_seed_sp2")], na.rm=T) > 0
dta$bought_seed_wm[is.na(dta$bought_seed_sp1) & is.na(dta$bought_seed_sp2)] <- NA

#
dta$combiner_sp1 <- rowSums(cbind((dta$grp1a29=="Yes" & dta$grp1a42 == "Yes"),(dta$grp2b29=="Yes" & dta$grp2b42 == "Yes"),(dta$grp3c29=="Yes" & dta$grp3c42 == "Yes"),(dta$grp4d29=="Yes" & dta$grp4d42 == "Yes"),(dta$grp5e29=="Yes" & dta$grp5e42 == "Yes")) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$combiner_sp1[rowSums(is.na(cbind((dta$grp1a29=="Yes" & dta$grp1a42 == "Yes"),(dta$grp2b29=="Yes" & dta$grp2b42 == "Yes"),(dta$grp3c29=="Yes" & dta$grp3c42 == "Yes"),(dta$grp4d29=="Yes" & dta$grp4d42 == "Yes"),(dta$grp5e29=="Yes" & dta$grp5e42 == "Yes")) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$combiner_sp1[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$combiner_sp2 <- rowSums( cbind((dta$spouse2grp_sp1f29=="Yes" & dta$spouse2grp_sp1f42 == "Yes"),(dta$spouse2grp_sp2g29=="Yes" & dta$spouse2grp_sp2g42 == "Yes"),(dta$spouse2grp_sp3h29=="Yes" & dta$spouse2grp_sp3h42 == "Yes"),(dta$spouse2group_sp4j29=="Yes" & dta$spouse2group_sp4j42 == "Yes"),(dta$spouse2grp5_sp5k29=="Yes" & dta$spouse2grp5_sp5k42 == "Yes"))*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$combiner_sp2[rowSums(is.na(( cbind((dta$spouse2grp_sp1f29=="Yes" & dta$spouse2grp_sp1f42 == "Yes"),(dta$spouse2grp_sp2g29=="Yes" & dta$spouse2grp_sp2g42 == "Yes"),(dta$spouse2grp_sp3h29=="Yes" & dta$spouse2grp_sp3h42 == "Yes"),(dta$spouse2group_sp4j29=="Yes" & dta$spouse2group_sp4j42 == "Yes"),(dta$spouse2grp5_sp5k29=="Yes" & dta$spouse2grp5_sp5k42 == "Yes")))*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$combiner_sp2[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$combiner_wm <-  rowSums(dta[c("combiner_sp1","combiner_sp2")], na.rm=T) > 0
dta$combiner_wm[is.na(dta$combiner_sp1) & is.na(dta$combiner_sp2)] <- NA

dta$chem_sp1 <- rowSums(cbind((dta$grp1a55a == "Yes") , (dta$grp2b55b  == "Yes"), (dta$grp3c55b  == "Yes"), (dta$grp4d55b  == "Yes") , (dta$grp5e55b  == "Yes")) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$chem_sp1[rowSums(is.na(cbind((dta$grp1a55a == "Yes") , (dta$grp2b55b  == "Yes"), (dta$grp3c55b  == "Yes"), (dta$grp4d55b  == "Yes") , (dta$grp5e55b  == "Yes")) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$chem_sp1[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$chem_sp2 <- rowSums( cbind((dta$spouse2grp_sp1f55a == "Yes"), (dta$spouse2grp_sp2g55b  == "Yes"), (dta$spouse2grp_sp3h55b  == "Yes"), (dta$spouse2group_sp4j55b  == "Yes") , (dta$spouse2grp5_sp5k55b  == "Yes"))*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$chem_sp2[rowSums(is.na( cbind((dta$spouse2grp_sp1f55a == "Yes"), (dta$spouse2grp_sp2g55b  == "Yes"), (dta$spouse2grp_sp3h55b  == "Yes"), (dta$spouse2group_sp4j55b  == "Yes") , (dta$spouse2grp5_sp5k55b  == "Yes"))*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$chem_sp2[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$chem_wm <-  rowSums(dta[c("chem_sp1","chem_sp2")], na.rm=T) > 0
dta$chem_wm[is.na(dta$chem_sp1) & is.na(dta$chem_sp2)] <- NA

dta$labour_sp1 <- rowSums(cbind((dta$grp1a151 == "Yes"), (dta$grp2b55b  == "Yes"), (dta$grp3c55b  == "Yes"), (dta$grp4d55b  == "Yes"), (dta$grp5e55b  == "Yes")) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$labour_sp1[rowSums(is.na(cbind((dta$grp1a151 == "Yes"), (dta$grp2b55b  == "Yes"), (dta$grp3c55b  == "Yes"), (dta$grp4d55b  == "Yes"), (dta$grp5e55b  == "Yes")) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$labour_sp1[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$labour_sp2 <- rowSums( cbind((dta$spouse2grp_sp1f151 == "Yes"),(dta$spouse2grp_sp2g151  == "Yes") , (dta$spouse2grp_sp3h151  == "Yes"), (dta$spouse2group_sp4j151  == "Yes"), (dta$spouse2grp5_sp5k151  == "Yes"))*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$labour_sp2[rowSums(is.na( cbind((dta$spouse2grp_sp1f151 == "Yes"),(dta$spouse2grp_sp2g151  == "Yes") , (dta$spouse2grp_sp3h151  == "Yes"), (dta$spouse2group_sp4j151  == "Yes"), (dta$spouse2grp5_sp5k151  == "Yes"))*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$labour_sp2[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$labour_wm <-  rowSums(dta[c("labour_sp1","labour_sp2")], na.rm=T) > 0
dta$labour_wm[is.na(dta$labour_sp1) & is.na(dta$labour_sp2)] <- NA

################################### practices Women involved
dta$space_sp1 <- rowSums((dta[c("grp1a201","grp2b201","grp3c201","grp4d201","grp5e201")]>0) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$space_sp1[rowSums(is.na((dta[c("grp1a201","grp2b201","grp3c201","grp4d201","grp5e201")]>0) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$space_sp1[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$space_sp2 <- rowSums((dta[c("spouse2grp_sp1f201", "spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201","spouse2grp5_sp5k201")]>0)*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$space_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f201", "spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201","spouse2grp5_sp5k201")]>0)*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$space_sp2[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$space_wi <-  rowSums(dta[c("space_sp1","space_sp2")], na.rm=T) > 0
dta$space_wi[is.na(dta$space_sp1) & is.na(dta$space_sp2)] <- NA

# male managed
dta$striga_sp1 <- rowSums((dta[c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241")]>0) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$striga_sp1[rowSums(is.na((dta[c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241")]>0) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$striga_sp1[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$striga_sp2 <- rowSums((dta[c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241")]>0)*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$striga_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241")]>0)*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$striga_sp2[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$striga_wi <-  rowSums(dta[c("striga_sp1","striga_sp2")], na.rm=T) > 0
dta$striga_wi[is.na(dta$striga_sp1) & is.na(dta$striga_sp2)] <- NA

##
dta$weed_sp1 <- rowSums((dta[c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26")] == 3) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$weed_sp1[rowSums(is.na((dta[c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26")] == 3) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$weed_sp1[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$weed_sp2 <- rowSums((dta[c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26")] == 3)*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$weed_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26")] == 3)*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$weed_sp2[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$weed_wi <-  rowSums(dta[c("weed_sp1","weed_sp2")], na.rm=T) > 0
dta$weed_wi[is.na(dta$weed_sp1) & is.na(dta$weed_sp2)] <- NA
##
dta$fert_sp1 <- rowSums((dta[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")] == "Yes") *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$fert_sp1[rowSums(is.na((dta[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")] == "Yes") *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$fert_sp1[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_sp2 <- rowSums((dta[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29")] == "Yes")*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$fert_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29")] == "Yes")*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$fert_sp2[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_wi <-  rowSums(dta[c("fert_sp1","fert_sp2")], na.rm=T) > 0
dta$fert_wi[is.na(dta$fert_sp1) & is.na(dta$fert_sp2)] <- NA


###
dta$impseed_sp1 <- rowSums((dta[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")] == "Yes") *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$impseed_sp1[rowSums(is.na((dta[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")] == "Yes") *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$impseed_sp1[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$impseed_sp2 <- rowSums((dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42")] == "Yes")*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$impseed_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42")] == "Yes")*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$impseed_sp2[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$impseed_wi <-  rowSums(dta[c("impseed_sp1","impseed_sp2")], na.rm=T) > 0
dta$impseed_wi[is.na(dta$impseed_sp1) & is.na(dta$impseed_sp2)] <- NA

###
dta$bought_seed_sp1 <- FALSE
dta$bought_seed_sp1 <- rowSums((cbind(dta$grp1seed_purchase1 =="Yes",dta$grp2seed_purchase2 =="Yes",dta$grp3seed_purchase3 =="Yes",dta$grp4seed_purchase4 =="Yes",dta$grp5seed_purchase5 =="Yes")) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)

dta$bought_seed_sp1[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA
dta$bought_seed_sp2 <- FALSE
dta$bought_seed_sp2 <- rowSums((cbind(dta$spouse2grp_sp1seed_purchasesp1=="Yes", dta$spouse2grp_sp2seed_purchase_sp2=="Yes", dta$spouse2grp_sp3seed_purchasesp3=="Yes", dta$spouse2group_sp4seed_purchasesp4=="Yes", dta$spouse2grp5_sp5seed_purchasesp5=="Yes"))*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)

dta$bought_seed_sp2[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$bought_seed_wi <-  rowSums(dta[c("bought_seed_sp1","bought_seed_sp2")], na.rm=T) > 0
dta$bought_seed_wi[is.na(dta$bought_seed_sp1) & is.na(dta$bought_seed_sp2)] <- NA

#
dta$combiner_sp1 <- rowSums(cbind((dta$grp1a29=="Yes" & dta$grp1a42 == "Yes"),(dta$grp2b29=="Yes" & dta$grp2b42 == "Yes"),(dta$grp3c29=="Yes" & dta$grp3c42 == "Yes"),(dta$grp4d29=="Yes" & dta$grp4d42 == "Yes"),(dta$grp5e29=="Yes" & dta$grp5e42 == "Yes")) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$combiner_sp1[rowSums(is.na(cbind((dta$grp1a29=="Yes" & dta$grp1a42 == "Yes"),(dta$grp2b29=="Yes" & dta$grp2b42 == "Yes"),(dta$grp3c29=="Yes" & dta$grp3c42 == "Yes"),(dta$grp4d29=="Yes" & dta$grp4d42 == "Yes"),(dta$grp5e29=="Yes" & dta$grp5e42 == "Yes")) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$combiner_sp1[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$combiner_sp2 <- rowSums( cbind((dta$spouse2grp_sp1f29=="Yes" & dta$spouse2grp_sp1f42 == "Yes"),(dta$spouse2grp_sp2g29=="Yes" & dta$spouse2grp_sp2g42 == "Yes"),(dta$spouse2grp_sp3h29=="Yes" & dta$spouse2grp_sp3h42 == "Yes"),(dta$spouse2group_sp4j29=="Yes" & dta$spouse2group_sp4j42 == "Yes"),(dta$spouse2grp5_sp5k29=="Yes" & dta$spouse2grp5_sp5k42 == "Yes"))*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$combiner_sp2[rowSums(is.na(( cbind((dta$spouse2grp_sp1f29=="Yes" & dta$spouse2grp_sp1f42 == "Yes"),(dta$spouse2grp_sp2g29=="Yes" & dta$spouse2grp_sp2g42 == "Yes"),(dta$spouse2grp_sp3h29=="Yes" & dta$spouse2grp_sp3h42 == "Yes"),(dta$spouse2group_sp4j29=="Yes" & dta$spouse2group_sp4j42 == "Yes"),(dta$spouse2grp5_sp5k29=="Yes" & dta$spouse2grp5_sp5k42 == "Yes")))*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$combiner_sp2[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$combiner_wi <-  rowSums(dta[c("combiner_sp1","combiner_sp2")], na.rm=T) > 0
dta$combiner_wi[is.na(dta$combiner_sp1) & is.na(dta$combiner_sp2)] <- NA

dta$chem_sp1 <- rowSums(cbind((dta$grp1a55a == "Yes") , (dta$grp2b55b  == "Yes"), (dta$grp3c55b  == "Yes"), (dta$grp4d55b  == "Yes") , (dta$grp5e55b  == "Yes")) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$chem_sp1[rowSums(is.na(cbind((dta$grp1a55a == "Yes") , (dta$grp2b55b  == "Yes"), (dta$grp3c55b  == "Yes"), (dta$grp4d55b  == "Yes") , (dta$grp5e55b  == "Yes")) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$chem_sp1[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$chem_sp2 <- rowSums( cbind((dta$spouse2grp_sp1f55a == "Yes"), (dta$spouse2grp_sp2g55b  == "Yes"), (dta$spouse2grp_sp3h55b  == "Yes"), (dta$spouse2group_sp4j55b  == "Yes") , (dta$spouse2grp5_sp5k55b  == "Yes"))*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$chem_sp2[rowSums(is.na( cbind((dta$spouse2grp_sp1f55a == "Yes"), (dta$spouse2grp_sp2g55b  == "Yes"), (dta$spouse2grp_sp3h55b  == "Yes"), (dta$spouse2group_sp4j55b  == "Yes") , (dta$spouse2grp5_sp5k55b  == "Yes"))*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$chem_sp2[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$chem_wi <-  rowSums(dta[c("chem_sp1","chem_sp2")], na.rm=T) > 0
dta$chem_wi[is.na(dta$chem_sp1) & is.na(dta$chem_sp2)] <- NA

dta$labour_sp1 <- rowSums(cbind((dta$grp1a151 == "Yes"), (dta$grp2b55b  == "Yes"), (dta$grp3c55b  == "Yes"), (dta$grp4d55b  == "Yes"), (dta$grp5e55b  == "Yes")) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$labour_sp1[rowSums(is.na(cbind((dta$grp1a151 == "Yes"), (dta$grp2b55b  == "Yes"), (dta$grp3c55b  == "Yes"), (dta$grp4d55b  == "Yes"), (dta$grp5e55b  == "Yes")) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$labour_sp1[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$labour_sp2 <- rowSums( cbind((dta$spouse2grp_sp1f151 == "Yes"),(dta$spouse2grp_sp2g151  == "Yes") , (dta$spouse2grp_sp3h151  == "Yes"), (dta$spouse2group_sp4j151  == "Yes"), (dta$spouse2grp5_sp5k151  == "Yes"))*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$labour_sp2[rowSums(is.na( cbind((dta$spouse2grp_sp1f151 == "Yes"),(dta$spouse2grp_sp2g151  == "Yes") , (dta$spouse2grp_sp3h151  == "Yes"), (dta$spouse2group_sp4j151  == "Yes"), (dta$spouse2grp5_sp5k151  == "Yes"))*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$labour_sp2[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$labour_wi <-  rowSums(dta[c("labour_sp1","labour_sp2")], na.rm=T) > 0
dta$labour_wi[is.na(dta$labour_sp1) & is.na(dta$labour_sp2)] <- NA

########################## practices - managed both
dta$space_sp1 <- rowSums((dta[c("grp1a201","grp2b201","grp3c201","grp4d201","grp5e201")]>0) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$space_sp1[rowSums(is.na((dta[c("grp1a201","grp2b201","grp3c201","grp4d201","grp5e201")]>0) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$space_sp1[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$space_sp2 <- rowSums((dta[c("spouse2grp_sp1f201", "spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201","spouse2grp5_sp5k201")]>0)*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$space_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f201", "spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201","spouse2grp5_sp5k201")]>0)*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$space_sp2[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$space_bm <-  rowSums(dta[c("space_sp1","space_sp2")], na.rm=T) > 0
dta$space_bm[is.na(dta$space_sp1) & is.na(dta$space_sp2)] <- NA

# male managed
dta$striga_sp1 <- rowSums((dta[c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241")]>0) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$striga_sp1[rowSums(is.na((dta[c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241")]>0) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$striga_sp1[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$striga_sp2 <- rowSums((dta[c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241")]>0)*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$striga_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241")]>0)*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$striga_sp2[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$striga_bm <-  rowSums(dta[c("striga_sp1","striga_sp2")], na.rm=T) > 0
dta$striga_bm[is.na(dta$striga_sp1) & is.na(dta$striga_sp2)] <- NA

##
dta$weed_sp1 <- rowSums((dta[c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26")] == 3) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$weed_sp1[rowSums(is.na((dta[c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26")] == 3) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$weed_sp1[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$weed_sp2 <- rowSums((dta[c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26")] == 3)*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$weed_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26")] == 3)*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$weed_sp2[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$weed_bm <-  rowSums(dta[c("weed_sp1","weed_sp2")], na.rm=T) > 0
dta$weed_bm[is.na(dta$weed_sp1) & is.na(dta$weed_sp2)] <- NA
##
dta$fert_sp1 <- rowSums((dta[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")] == "Yes") *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$fert_sp1[rowSums(is.na((dta[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")] == "Yes") *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$fert_sp1[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_sp2 <- rowSums((dta[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29")] == "Yes")*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$fert_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29")] == "Yes")*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$fert_sp2[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_bm <-  rowSums(dta[c("fert_sp1","fert_sp2")], na.rm=T) > 0
dta$fert_bm[is.na(dta$fert_sp1) & is.na(dta$fert_sp2)] <- NA


###
dta$impseed_sp1 <- rowSums((dta[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")] == "Yes") *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$impseed_sp1[rowSums(is.na((dta[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")] == "Yes") *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$impseed_sp1[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$impseed_sp2 <- rowSums((dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42")] == "Yes")*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$impseed_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42")] == "Yes")*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$impseed_sp2[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$impseed_bm <-  rowSums(dta[c("impseed_sp1","impseed_sp2")], na.rm=T) > 0
dta$impseed_bm[is.na(dta$impseed_sp1) & is.na(dta$impseed_sp2)] <- NA

###
dta$bought_seed_sp1 <- FALSE
dta$bought_seed_sp1 <- rowSums((cbind(dta$grp1seed_purchase1 =="Yes",dta$grp2seed_purchase2 =="Yes",dta$grp3seed_purchase3 =="Yes",dta$grp4seed_purchase4 =="Yes",dta$grp5seed_purchase5 =="Yes")) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)

dta$bought_seed_sp1[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA
dta$bought_seed_sp2 <- FALSE
dta$bought_seed_sp2 <- rowSums((cbind(dta$spouse2grp_sp1seed_purchasesp1=="Yes", dta$spouse2grp_sp2seed_purchase_sp2=="Yes", dta$spouse2grp_sp3seed_purchasesp3=="Yes", dta$spouse2group_sp4seed_purchasesp4=="Yes", dta$spouse2grp5_sp5seed_purchasesp5=="Yes"))*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)

dta$bought_seed_sp2[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$bought_seed_bm <-  rowSums(dta[c("bought_seed_sp1","bought_seed_sp2")], na.rm=T) > 0
dta$bought_seed_bm[is.na(dta$bought_seed_sp1) & is.na(dta$bought_seed_sp2)] <- NA

#
dta$combiner_sp1 <- rowSums(cbind((dta$grp1a29=="Yes" & dta$grp1a42 == "Yes"),(dta$grp2b29=="Yes" & dta$grp2b42 == "Yes"),(dta$grp3c29=="Yes" & dta$grp3c42 == "Yes"),(dta$grp4d29=="Yes" & dta$grp4d42 == "Yes"),(dta$grp5e29=="Yes" & dta$grp5e42 == "Yes")) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$combiner_sp1[rowSums(is.na(cbind((dta$grp1a29=="Yes" & dta$grp1a42 == "Yes"),(dta$grp2b29=="Yes" & dta$grp2b42 == "Yes"),(dta$grp3c29=="Yes" & dta$grp3c42 == "Yes"),(dta$grp4d29=="Yes" & dta$grp4d42 == "Yes"),(dta$grp5e29=="Yes" & dta$grp5e42 == "Yes")) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$combiner_sp1[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$combiner_sp2 <- rowSums( cbind((dta$spouse2grp_sp1f29=="Yes" & dta$spouse2grp_sp1f42 == "Yes"),(dta$spouse2grp_sp2g29=="Yes" & dta$spouse2grp_sp2g42 == "Yes"),(dta$spouse2grp_sp3h29=="Yes" & dta$spouse2grp_sp3h42 == "Yes"),(dta$spouse2group_sp4j29=="Yes" & dta$spouse2group_sp4j42 == "Yes"),(dta$spouse2grp5_sp5k29=="Yes" & dta$spouse2grp5_sp5k42 == "Yes"))*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$combiner_sp2[rowSums(is.na(( cbind((dta$spouse2grp_sp1f29=="Yes" & dta$spouse2grp_sp1f42 == "Yes"),(dta$spouse2grp_sp2g29=="Yes" & dta$spouse2grp_sp2g42 == "Yes"),(dta$spouse2grp_sp3h29=="Yes" & dta$spouse2grp_sp3h42 == "Yes"),(dta$spouse2group_sp4j29=="Yes" & dta$spouse2group_sp4j42 == "Yes"),(dta$spouse2grp5_sp5k29=="Yes" & dta$spouse2grp5_sp5k42 == "Yes")))*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$combiner_sp2[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$combiner_bm <-  rowSums(dta[c("combiner_sp1","combiner_sp2")], na.rm=T) > 0
dta$combiner_bm[is.na(dta$combiner_sp1) & is.na(dta$combiner_sp2)] <- NA

dta$chem_sp1 <- rowSums(cbind((dta$grp1a55a == "Yes") , (dta$grp2b55b  == "Yes"), (dta$grp3c55b  == "Yes"), (dta$grp4d55b  == "Yes") , (dta$grp5e55b  == "Yes")) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$chem_sp1[rowSums(is.na(cbind((dta$grp1a55a == "Yes") , (dta$grp2b55b  == "Yes"), (dta$grp3c55b  == "Yes"), (dta$grp4d55b  == "Yes") , (dta$grp5e55b  == "Yes")) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$chem_sp1[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$chem_sp2 <- rowSums( cbind((dta$spouse2grp_sp1f55a == "Yes"), (dta$spouse2grp_sp2g55b  == "Yes"), (dta$spouse2grp_sp3h55b  == "Yes"), (dta$spouse2group_sp4j55b  == "Yes") , (dta$spouse2grp5_sp5k55b  == "Yes"))*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$chem_sp2[rowSums(is.na( cbind((dta$spouse2grp_sp1f55a == "Yes"), (dta$spouse2grp_sp2g55b  == "Yes"), (dta$spouse2grp_sp3h55b  == "Yes"), (dta$spouse2group_sp4j55b  == "Yes") , (dta$spouse2grp5_sp5k55b  == "Yes"))*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$chem_sp2[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$chem_bm <-  rowSums(dta[c("chem_sp1","chem_sp2")], na.rm=T) > 0
dta$chem_bm[is.na(dta$chem_sp1) & is.na(dta$chem_sp2)] <- NA

dta$labour_sp1 <- rowSums(cbind((dta$grp1a151 == "Yes"), (dta$grp2b55b  == "Yes"), (dta$grp3c55b  == "Yes"), (dta$grp4d55b  == "Yes"), (dta$grp5e55b  == "Yes")) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$labour_sp1[rowSums(is.na(cbind((dta$grp1a151 == "Yes"), (dta$grp2b55b  == "Yes"), (dta$grp3c55b  == "Yes"), (dta$grp4d55b  == "Yes"), (dta$grp5e55b  == "Yes")) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$labour_sp1[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$labour_sp2 <- rowSums( cbind((dta$spouse2grp_sp1f151 == "Yes"),(dta$spouse2grp_sp2g151  == "Yes") , (dta$spouse2grp_sp3h151  == "Yes"), (dta$spouse2group_sp4j151  == "Yes"), (dta$spouse2grp5_sp5k151  == "Yes"))*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$labour_sp2[rowSums(is.na( cbind((dta$spouse2grp_sp1f151 == "Yes"),(dta$spouse2grp_sp2g151  == "Yes") , (dta$spouse2grp_sp3h151  == "Yes"), (dta$spouse2group_sp4j151  == "Yes"), (dta$spouse2grp5_sp5k151  == "Yes"))*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$labour_sp2[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$labour_bm <-  rowSums(dta[c("labour_sp1","labour_sp2")], na.rm=T) > 0
dta$labour_bm[is.na(dta$labour_sp1) & is.na(dta$labour_sp2)] <- NA

##### practices - wm_share1
dta$space_sp1 <- rowSums((dta[c("grp1a201","grp2b201","grp3c201","grp4d201","grp5e201")]>0) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$space_sp1[rowSums(is.na((dta[c("grp1a201","grp2b201","grp3c201","grp4d201","grp5e201")]>0) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$space_sp1[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$space_sp2 <- rowSums((dta[c("spouse2grp_sp1f201", "spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201","spouse2grp5_sp5k201")]>0)*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$space_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f201", "spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201","spouse2grp5_sp5k201")]>0)*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$space_sp2[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$space_wm_share <-  rowSums(dta[c("space_sp1","space_sp2")], na.rm=T) > 0
dta$space_wm_share[is.na(dta$space_sp1) & is.na(dta$space_sp2)] <- NA

# male managed
dta$striga_sp1 <- rowSums((dta[c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241")]>0) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$striga_sp1[rowSums(is.na((dta[c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241")]>0) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$striga_sp1[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$striga_sp2 <- rowSums((dta[c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241")]>0)*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$striga_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241")]>0)*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$striga_sp2[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$striga_wm_share <-  rowSums(dta[c("striga_sp1","striga_sp2")], na.rm=T) > 0
dta$striga_wm_share[is.na(dta$striga_sp1) & is.na(dta$striga_sp2)] <- NA

##
dta$weed_sp1 <- rowSums((dta[c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26")] == 3) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$weed_sp1[rowSums(is.na((dta[c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26")] == 3) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$weed_sp1[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$weed_sp2 <- rowSums((dta[c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26")] == 3)*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$weed_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26")] == 3)*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$weed_sp2[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$weed_wm_share <-  rowSums(dta[c("weed_sp1","weed_sp2")], na.rm=T) > 0
dta$weed_wm_share[is.na(dta$weed_sp1) & is.na(dta$weed_sp2)] <- NA
##
dta$fert_sp1 <- rowSums((dta[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")] == "Yes") *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$fert_sp1[rowSums(is.na((dta[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")] == "Yes") *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$fert_sp1[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_sp2 <- rowSums((dta[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29")] == "Yes")*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$fert_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29")] == "Yes")*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$fert_sp2[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_wm_share <-  rowSums(dta[c("fert_sp1","fert_sp2")], na.rm=T) > 0
dta$fert_wm_share[is.na(dta$fert_sp1) & is.na(dta$fert_sp2)] <- NA


###
dta$impseed_sp1 <- rowSums((dta[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")] == "Yes") *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$impseed_sp1[rowSums(is.na((dta[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")] == "Yes") *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$impseed_sp1[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$impseed_sp2 <- rowSums((dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42")] == "Yes")*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$impseed_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42")] == "Yes")*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$impseed_sp2[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$impseed_wm_share <-  rowSums(dta[c("impseed_sp1","impseed_sp2")], na.rm=T) > 0
dta$impseed_wm_share[is.na(dta$impseed_sp1) & is.na(dta$impseed_sp2)] <- NA

###
dta$bought_seed_sp1 <- FALSE
dta$bought_seed_sp1 <- rowSums((cbind(dta$grp1seed_purchase1 =="Yes",dta$grp2seed_purchase2 =="Yes",dta$grp3seed_purchase3 =="Yes",dta$grp4seed_purchase4 =="Yes",dta$grp5seed_purchase5 =="Yes")) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)

dta$bought_seed_sp1[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA
dta$bought_seed_sp2 <- FALSE
dta$bought_seed_sp2 <- rowSums((cbind(dta$spouse2grp_sp1seed_purchasesp1=="Yes", dta$spouse2grp_sp2seed_purchase_sp2=="Yes", dta$spouse2grp_sp3seed_purchasesp3=="Yes", dta$spouse2group_sp4seed_purchasesp4=="Yes", dta$spouse2grp5_sp5seed_purchasesp5=="Yes"))*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)

dta$bought_seed_sp2[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$bought_seed_wm_share <-  rowSums(dta[c("bought_seed_sp1","bought_seed_sp2")], na.rm=T) > 0
dta$bought_seed_wm_share[is.na(dta$bought_seed_sp1) & is.na(dta$bought_seed_sp2)] <- NA

#
dta$combiner_sp1 <- rowSums(cbind((dta$grp1a29=="Yes" & dta$grp1a42 == "Yes"),(dta$grp2b29=="Yes" & dta$grp2b42 == "Yes"),(dta$grp3c29=="Yes" & dta$grp3c42 == "Yes"),(dta$grp4d29=="Yes" & dta$grp4d42 == "Yes"),(dta$grp5e29=="Yes" & dta$grp5e42 == "Yes")) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$combiner_sp1[rowSums(is.na(cbind((dta$grp1a29=="Yes" & dta$grp1a42 == "Yes"),(dta$grp2b29=="Yes" & dta$grp2b42 == "Yes"),(dta$grp3c29=="Yes" & dta$grp3c42 == "Yes"),(dta$grp4d29=="Yes" & dta$grp4d42 == "Yes"),(dta$grp5e29=="Yes" & dta$grp5e42 == "Yes")) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$combiner_sp1[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$combiner_sp2 <- rowSums( cbind((dta$spouse2grp_sp1f29=="Yes" & dta$spouse2grp_sp1f42 == "Yes"),(dta$spouse2grp_sp2g29=="Yes" & dta$spouse2grp_sp2g42 == "Yes"),(dta$spouse2grp_sp3h29=="Yes" & dta$spouse2grp_sp3h42 == "Yes"),(dta$spouse2group_sp4j29=="Yes" & dta$spouse2group_sp4j42 == "Yes"),(dta$spouse2grp5_sp5k29=="Yes" & dta$spouse2grp5_sp5k42 == "Yes"))*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$combiner_sp2[rowSums(is.na(( cbind((dta$spouse2grp_sp1f29=="Yes" & dta$spouse2grp_sp1f42 == "Yes"),(dta$spouse2grp_sp2g29=="Yes" & dta$spouse2grp_sp2g42 == "Yes"),(dta$spouse2grp_sp3h29=="Yes" & dta$spouse2grp_sp3h42 == "Yes"),(dta$spouse2group_sp4j29=="Yes" & dta$spouse2group_sp4j42 == "Yes"),(dta$spouse2grp5_sp5k29=="Yes" & dta$spouse2grp5_sp5k42 == "Yes")))*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$combiner_sp2[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$combiner_wm_share <-  rowSums(dta[c("combiner_sp1","combiner_sp2")], na.rm=T) > 0
dta$combiner_wm_share[is.na(dta$combiner_sp1) & is.na(dta$combiner_sp2)] <- NA

dta$chem_sp1 <- rowSums(cbind((dta$grp1a55a == "Yes") , (dta$grp2b55b  == "Yes"), (dta$grp3c55b  == "Yes"), (dta$grp4d55b  == "Yes") , (dta$grp5e55b  == "Yes")) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$chem_sp1[rowSums(is.na(cbind((dta$grp1a55a == "Yes") , (dta$grp2b55b  == "Yes"), (dta$grp3c55b  == "Yes"), (dta$grp4d55b  == "Yes") , (dta$grp5e55b  == "Yes")) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$chem_sp1[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$chem_sp2 <- rowSums( cbind((dta$spouse2grp_sp1f55a == "Yes"), (dta$spouse2grp_sp2g55b  == "Yes"), (dta$spouse2grp_sp3h55b  == "Yes"), (dta$spouse2group_sp4j55b  == "Yes") , (dta$spouse2grp5_sp5k55b  == "Yes"))*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$chem_sp2[rowSums(is.na( cbind((dta$spouse2grp_sp1f55a == "Yes"), (dta$spouse2grp_sp2g55b  == "Yes"), (dta$spouse2grp_sp3h55b  == "Yes"), (dta$spouse2group_sp4j55b  == "Yes") , (dta$spouse2grp5_sp5k55b  == "Yes"))*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$chem_sp2[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$chem_wm_share <-  rowSums(dta[c("chem_sp1","chem_sp2")], na.rm=T) > 0
dta$chem_wm_share[is.na(dta$chem_sp1) & is.na(dta$chem_sp2)] <- NA

dta$labour_sp1 <- rowSums(cbind((dta$grp1a151 == "Yes"), (dta$grp2b55b  == "Yes"), (dta$grp3c55b  == "Yes"), (dta$grp4d55b  == "Yes"), (dta$grp5e55b  == "Yes")) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$labour_sp1[rowSums(is.na(cbind((dta$grp1a151 == "Yes"), (dta$grp2b55b  == "Yes"), (dta$grp3c55b  == "Yes"), (dta$grp4d55b  == "Yes"), (dta$grp5e55b  == "Yes")) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$labour_sp1[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$labour_sp2 <- rowSums( cbind((dta$spouse2grp_sp1f151 == "Yes"),(dta$spouse2grp_sp2g151  == "Yes") , (dta$spouse2grp_sp3h151  == "Yes"), (dta$spouse2group_sp4j151  == "Yes"), (dta$spouse2grp5_sp5k151  == "Yes"))*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$labour_sp2[rowSums(is.na( cbind((dta$spouse2grp_sp1f151 == "Yes"),(dta$spouse2grp_sp2g151  == "Yes") , (dta$spouse2grp_sp3h151  == "Yes"), (dta$spouse2group_sp4j151  == "Yes"), (dta$spouse2grp5_sp5k151  == "Yes"))*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$labour_sp2[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$labour_wm_share <-  rowSums(dta[c("labour_sp1","labour_sp2")], na.rm=T) > 0
dta$labour_wm_share[is.na(dta$labour_sp1) & is.na(dta$labour_sp2)] <- NA

###################### practices _ wm share2
dta$space_sp1 <- rowSums((dta[c("grp1a201","grp2b201","grp3c201","grp4d201","grp5e201")]>0) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$space_sp1[rowSums(is.na((dta[c("grp1a201","grp2b201","grp3c201","grp4d201","grp5e201")]>0) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$space_sp1[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$space_sp2 <- rowSums((dta[c("spouse2grp_sp1f201", "spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201","spouse2grp5_sp5k201")]>0)*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$space_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f201", "spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201","spouse2grp5_sp5k201")]>0)*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$space_sp2[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$space_wm_share2 <-  rowSums(dta[c("space_sp1","space_sp2")], na.rm=T) > 0
dta$space_wm_share2[is.na(dta$space_sp1) & is.na(dta$space_sp2)] <- NA

# male managed
dta$striga_sp1 <- rowSums((dta[c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241")]>0) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$striga_sp1[rowSums(is.na((dta[c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241")]>0) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$striga_sp1[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$striga_sp2 <- rowSums((dta[c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241")]>0)*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$striga_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241")]>0)*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$striga_sp2[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$striga_wm_share2 <-  rowSums(dta[c("striga_sp1","striga_sp2")], na.rm=T) > 0
dta$striga_wm_share2[is.na(dta$striga_sp1) & is.na(dta$striga_sp2)] <- NA

##
dta$weed_sp1 <- rowSums((dta[c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26")] == 3) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$weed_sp1[rowSums(is.na((dta[c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26")] == 3) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$weed_sp1[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$weed_sp2 <- rowSums((dta[c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26")] == 3)*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$weed_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26")] == 3)*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$weed_sp2[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$weed_wm_share2 <-  rowSums(dta[c("weed_sp1","weed_sp2")], na.rm=T) > 0
dta$weed_wm_share2[is.na(dta$weed_sp1) & is.na(dta$weed_sp2)] <- NA
##
dta$fert_sp1 <- rowSums((dta[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")] == "Yes") *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$fert_sp1[rowSums(is.na((dta[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")] == "Yes") *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$fert_sp1[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_sp2 <- rowSums((dta[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29")] == "Yes")*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$fert_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29")] == "Yes")*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$fert_sp2[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_wm_share2 <-  rowSums(dta[c("fert_sp1","fert_sp2")], na.rm=T) > 0
dta$fert_wm_share2[is.na(dta$fert_sp1) & is.na(dta$fert_sp2)] <- NA


###
dta$impseed_sp1 <- rowSums((dta[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")] == "Yes") *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$impseed_sp1[rowSums(is.na((dta[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")] == "Yes") *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$impseed_sp1[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$impseed_sp2 <- rowSums((dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42")] == "Yes")*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$impseed_sp2[rowSums(is.na((dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42")] == "Yes")*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$impseed_sp2[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$impseed_wm_share2 <-  rowSums(dta[c("impseed_sp1","impseed_sp2")], na.rm=T) > 0
dta$impseed_wm_share2[is.na(dta$impseed_sp1) & is.na(dta$impseed_sp2)] <- NA

###
dta$bought_seed_sp1 <- FALSE
dta$bought_seed_sp1 <- rowSums((cbind(dta$grp1seed_purchase1 =="Yes",dta$grp2seed_purchase2 =="Yes",dta$grp3seed_purchase3 =="Yes",dta$grp4seed_purchase4 =="Yes",dta$grp5seed_purchase5 =="Yes")) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)

dta$bought_seed_sp1[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA
dta$bought_seed_sp2 <- FALSE
dta$bought_seed_sp2 <- rowSums((cbind(dta$spouse2grp_sp1seed_purchasesp1=="Yes", dta$spouse2grp_sp2seed_purchase_sp2=="Yes", dta$spouse2grp_sp3seed_purchasesp3=="Yes", dta$spouse2group_sp4seed_purchasesp4=="Yes", dta$spouse2grp5_sp5seed_purchasesp5=="Yes"))*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)

dta$bought_seed_sp2[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$bought_seed_wm_share2 <-  rowSums(dta[c("bought_seed_sp1","bought_seed_sp2")], na.rm=T) > 0
dta$bought_seed_wm_share2[is.na(dta$bought_seed_sp1) & is.na(dta$bought_seed_sp2)] <- NA

#
dta$combiner_sp1 <- rowSums(cbind((dta$grp1a29=="Yes" & dta$grp1a42 == "Yes"),(dta$grp2b29=="Yes" & dta$grp2b42 == "Yes"),(dta$grp3c29=="Yes" & dta$grp3c42 == "Yes"),(dta$grp4d29=="Yes" & dta$grp4d42 == "Yes"),(dta$grp5e29=="Yes" & dta$grp5e42 == "Yes")) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$combiner_sp1[rowSums(is.na(cbind((dta$grp1a29=="Yes" & dta$grp1a42 == "Yes"),(dta$grp2b29=="Yes" & dta$grp2b42 == "Yes"),(dta$grp3c29=="Yes" & dta$grp3c42 == "Yes"),(dta$grp4d29=="Yes" & dta$grp4d42 == "Yes"),(dta$grp5e29=="Yes" & dta$grp5e42 == "Yes")) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$combiner_sp1[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$combiner_sp2 <- rowSums( cbind((dta$spouse2grp_sp1f29=="Yes" & dta$spouse2grp_sp1f42 == "Yes"),(dta$spouse2grp_sp2g29=="Yes" & dta$spouse2grp_sp2g42 == "Yes"),(dta$spouse2grp_sp3h29=="Yes" & dta$spouse2grp_sp3h42 == "Yes"),(dta$spouse2group_sp4j29=="Yes" & dta$spouse2group_sp4j42 == "Yes"),(dta$spouse2grp5_sp5k29=="Yes" & dta$spouse2grp5_sp5k42 == "Yes"))*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$combiner_sp2[rowSums(is.na(( cbind((dta$spouse2grp_sp1f29=="Yes" & dta$spouse2grp_sp1f42 == "Yes"),(dta$spouse2grp_sp2g29=="Yes" & dta$spouse2grp_sp2g42 == "Yes"),(dta$spouse2grp_sp3h29=="Yes" & dta$spouse2grp_sp3h42 == "Yes"),(dta$spouse2group_sp4j29=="Yes" & dta$spouse2group_sp4j42 == "Yes"),(dta$spouse2grp5_sp5k29=="Yes" & dta$spouse2grp5_sp5k42 == "Yes")))*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$combiner_sp2[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$combiner_wm_share2 <-  rowSums(dta[c("combiner_sp1","combiner_sp2")], na.rm=T) > 0
dta$combiner_wm_share2[is.na(dta$combiner_sp1) & is.na(dta$combiner_sp2)] <- NA

dta$chem_sp1 <- rowSums(cbind((dta$grp1a55a == "Yes") , (dta$grp2b55b  == "Yes"), (dta$grp3c55b  == "Yes"), (dta$grp4d55b  == "Yes") , (dta$grp5e55b  == "Yes")) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$chem_sp1[rowSums(is.na(cbind((dta$grp1a55a == "Yes") , (dta$grp2b55b  == "Yes"), (dta$grp3c55b  == "Yes"), (dta$grp4d55b  == "Yes") , (dta$grp5e55b  == "Yes")) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$chem_sp1[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$chem_sp2 <- rowSums( cbind((dta$spouse2grp_sp1f55a == "Yes"), (dta$spouse2grp_sp2g55b  == "Yes"), (dta$spouse2grp_sp3h55b  == "Yes"), (dta$spouse2group_sp4j55b  == "Yes") , (dta$spouse2grp5_sp5k55b  == "Yes"))*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$chem_sp2[rowSums(is.na( cbind((dta$spouse2grp_sp1f55a == "Yes"), (dta$spouse2grp_sp2g55b  == "Yes"), (dta$spouse2grp_sp3h55b  == "Yes"), (dta$spouse2group_sp4j55b  == "Yes") , (dta$spouse2grp5_sp5k55b  == "Yes"))*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$chem_sp2[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$chem_wm_share2 <-  rowSums(dta[c("chem_sp1","chem_sp2")], na.rm=T) > 0
dta$chem_wm_share2[is.na(dta$chem_sp1) & is.na(dta$chem_sp2)] <- NA

dta$labour_sp1 <- rowSums(cbind((dta$grp1a151 == "Yes"), (dta$grp2b55b  == "Yes"), (dta$grp3c55b  == "Yes"), (dta$grp4d55b  == "Yes"), (dta$grp5e55b  == "Yes")) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$labour_sp1[rowSums(is.na(cbind((dta$grp1a151 == "Yes"), (dta$grp2b55b  == "Yes"), (dta$grp3c55b  == "Yes"), (dta$grp4d55b  == "Yes"), (dta$grp5e55b  == "Yes")) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$labour_sp1[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$labour_sp2 <- rowSums( cbind((dta$spouse2grp_sp1f151 == "Yes"),(dta$spouse2grp_sp2g151  == "Yes") , (dta$spouse2grp_sp3h151  == "Yes"), (dta$spouse2group_sp4j151  == "Yes"), (dta$spouse2grp5_sp5k151  == "Yes"))*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$labour_sp2[rowSums(is.na( cbind((dta$spouse2grp_sp1f151 == "Yes"),(dta$spouse2grp_sp2g151  == "Yes") , (dta$spouse2grp_sp3h151  == "Yes"), (dta$spouse2group_sp4j151  == "Yes"), (dta$spouse2grp5_sp5k151  == "Yes"))*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$labour_sp2[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$labour_wm_share2 <-  rowSums(dta[c("labour_sp1","labour_sp2")], na.rm=T) > 0
dta$labour_wm_share2[is.na(dta$labour_sp1) & is.na(dta$labour_sp2)] <- NA


### should we also look at individual fertilizer categories, or at least differentiate between organic and inorganic fertilizer?
### dap/npk == 1
dta$fert_dap_sp1 <- rowSums( dta[c("grp1a301","grp2b301","grp3c301","grp4d301","grp5e301")],na.rm=T  ) > 0
#only for observations we have data for the question if they use
dta$fert_dap_sp1[is.na(dta$grp1a29) & is.na(dta$grp2b29) & is.na(dta$grp3c29) & is.na(dta$grp4d29) & is.na(dta$grp5e29)] <- NA 
dta$fert_dap_sp2 <- rowSums( dta[c("spouse2grp_sp1f301","spouse2grp_sp2g301","spouse2grp_sp3h301","spouse2group_sp4j301","spouse2grp5_sp5k301")],na.rm=T) > 0
dta$fert_dap_sp2[is.na(dta$spouse2grp_sp1f29) & is.na(dta$spouse2grp_sp2g29) & is.na(dta$spouse2grp_sp3h29) & is.na(dta$spouse2group_sp4j29) & is.na(dta$spouse2grp5_sp5k29)] <- NA
dta$fert_dap <- rowSums(dta[c("fert_dap_sp1","fert_dap_sp2")], na.rm=T) > 0
dta$fert_dap[is.na(dta$fert_dap_sp1) & is.na(dta$fert_dap_sp2)] <- NA

### should we also look at individual fertilizer categories, or at least differentiate between organic and inorganic fertilizer?
### urea == 2
dta$fert_urea_sp1 <- rowSums(dta[c("grp1a302","grp2b302","grp3c302","grp4d302","grp5e302")], na.rm=T) > 0
dta$fert_urea_sp1[is.na(dta$grp1a29) & is.na(dta$grp2b29) & is.na(dta$grp3c29) & is.na(dta$grp4d29) & is.na(dta$grp5e29)] <- NA 
dta$fert_urea_sp2 <- rowSums(dta[c("spouse2grp_sp1f302","spouse2grp_sp2g302","spouse2grp_sp3h302","spouse2group_sp4j302","spouse2grp5_sp5k302")], na.rm=T  ) > 0
dta$fert_urea_sp2[is.na(dta$spouse2grp_sp1f29) & is.na(dta$spouse2grp_sp2g29) & is.na(dta$spouse2grp_sp3h29) & is.na(dta$spouse2group_sp4j29) & is.na(dta$spouse2grp5_sp5k29)] <- NA

dta$fert_urea <- rowSums(dta[c("fert_urea_sp1","fert_urea_sp2")], na.rm=T) > 0
dta$fert_urea[is.na(dta$fert_urea_sp1) & is.na(dta$fert_urea_sp2)] <- NA

### organic == 3
dta$fert_org_sp1 <- rowSums(dta[c("grp1a303","grp2b303","grp3c303","grp4d303","grp5e303")],na.rm=T) > 0
dta$fert_org_sp1[is.na(dta$grp1a29) & is.na(dta$grp2b29) & is.na(dta$grp3c29) & is.na(dta$grp4d29) & is.na(dta$grp5e29)] <- NA 
dta$fert_org_sp2 <- rowSums(dta[c("spouse2grp_sp1f303","spouse2grp_sp2g303","spouse2grp_sp3h303","spouse2group_sp4j303","spouse2grp5_sp5k303")], na.rm=T) > 0
dta$fert_org_sp2[is.na(dta$spouse2grp_sp1f29) & is.na(dta$spouse2grp_sp2g29) & is.na(dta$spouse2grp_sp3h29) & is.na(dta$spouse2group_sp4j29) & is.na(dta$spouse2grp5_sp5k29)] <- NA
dta$fert_org <- rowSums(dta[c("fert_org_sp1","fert_org_sp2")], na.rm=T) > 0
dta$fert_org[is.na(dta$fert_org_sp1) & is.na(dta$fert_org_sp2)] <- NA


### use of hybird
##use longue10h on any plot?
dta$longe10h_sp1 <- rowSums(dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")], na.rm=T) > 0 
dta$longe10h_sp1[is.na(dta$grp1a42) & is.na(dta$grp2b42) & is.na(dta$grp3c42) & is.na(dta$grp4d42) & is.na(dta$grp5e42)] <- NA 
dta$longe10h_sp2 <- rowSums(dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")], na.rm=T) > 0 
dta$longe10h_sp2[is.na(dta$spouse2grp_sp1f42) & is.na(dta$spouse2grp_sp2g42) & is.na(dta$spouse2grp_sp3h42) & is.na(dta$spouse2group_sp4j42) & is.na(dta$spouse2grp5_sp5k42)] <- NA
dta$longe10h <-  rowSums(dta[c("longe10h_sp1","longe10h_sp2")], na.rm=T) > 0
dta$longe10h[is.na(dta$longe10h_sp1) & is.na(dta$longe10h_sp2)] <- NA

##use bazooka on any plot? ### this percentage is too low to use - pool with longe10h as hybrid
dta$bazooka_sp1 <- rowSums(dta[c("grp1a432","grp2b432","grp3c432","grp4d432", "grp5e432")], na.rm=T)
dta$bazooka_sp1[is.na(dta$grp1a42) & is.na(dta$grp2b42) & is.na(dta$grp3c42) & is.na(dta$grp4d42) & is.na(dta$grp5e42)] <- NA 
dta$bazooka_sp2 <- rowSums(dta[c("spouse2grp_sp1f432","spouse2grp_sp2g432","spouse2grp_sp3h432","spouse2group_sp4j432", "spouse2grp5_sp5k432")], na.rm=T)
dta$bazooka_sp2[is.na(dta$spouse2grp_sp1f42) & is.na(dta$spouse2grp_sp2g42) & is.na(dta$spouse2grp_sp3h42) & is.na(dta$spouse2group_sp4j42) & is.na(dta$spouse2grp5_sp5k42)] <- NA
dta$bazooka <-  rowSums(dta[c("bazooka_sp1","bazooka_sp2")], na.rm=T) > 0 
dta$bazooka[is.na(dta$bazooka_sp1) & is.na(dta$bazooka_sp2)] <- NA

##use other hybrid on any plot
dta$other_hybrid_sp1  <- rowSums(dta[c("grp1a436","grp2b436","grp3c436","grp4d436", "grp5e436")], na.rm=T )
dta$other_hybrid_sp1[is.na(dta$grp1a42) & is.na(dta$grp2b42) & is.na(dta$grp3c42) & is.na(dta$grp4d42) & is.na(dta$grp5e42)] <- NA 
dta$other_hybrid_sp2 <- rowSums(dta[c("spouse2grp_sp1f436","spouse2grp_sp2g436","spouse2grp_sp3h436","spouse2group_sp4j436", "spouse2grp5_sp5k436")], na.rm=T)
dta$other_hybrid_sp2[is.na(dta$spouse2grp_sp1f42) & is.na(dta$spouse2grp_sp2g42) & is.na(dta$spouse2grp_sp3h42) & is.na(dta$spouse2group_sp4j42) & is.na(dta$spouse2grp5_sp5k42)] <- NA
dta$other_hybrid <- rowSums(dta[c("other_hybrid_sp1","other_hybrid_sp2")], na.rm=T) > 0
dta$other_hybrid[is.na(dta$other_hybrid_sp1) & is.na(dta$other_hybrid_sp2)] <- NA


### use of OPV
#use longe5 on any plot?
dta$longe5_sp1 <- rowSums(dta[c("grp1a433","grp2b433","grp3c433","grp4d433", "grp5e433")], na.rm = T) > 0 
dta$longe5_sp1[is.na(dta$grp1a42) & is.na(dta$grp2b42) & is.na(dta$grp3c42) & is.na(dta$grp4d42) & is.na(dta$grp5e42)] <- NA 
dta$longe5_sp2 <- rowSums(dta[c("spouse2grp_sp1f433","spouse2grp_sp2g433","spouse2grp_sp3h433","spouse2group_sp4j433", "spouse2grp5_sp5k433")], na.rm = T) > 0
dta$longe5_sp2[is.na(dta$spouse2grp_sp1f42) & is.na(dta$spouse2grp_sp2g42) & is.na(dta$spouse2grp_sp3h42) & is.na(dta$spouse2group_sp4j42) & is.na(dta$spouse2grp5_sp5k42)] <- NA 
dta$longe5 <- rowSums(dta[c("longe5_sp1","longe5_sp2")], na.rm=T) > 0
dta$longe5[is.na(dta$longe5_sp1) & is.na(dta$longe5_sp2)] <- NA

#use longe4 on any plot?
dta$longe4_sp1 <- rowSums(dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")], na.rm=T) > 0 
dta$longe4_sp1[is.na(dta$grp1a42) & is.na(dta$grp2b42) & is.na(dta$grp3c42) & is.na(dta$grp4d42) & is.na(dta$grp5e42)] <- NA 
dta$longe4_sp2 <- rowSums(dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")], na.rm=T) > 0 
dta$longe4_sp2[is.na(dta$spouse2grp_sp1f42) & is.na(dta$spouse2grp_sp2g42) & is.na(dta$spouse2grp_sp3h42) & is.na(dta$spouse2group_sp4j42) & is.na(dta$spouse2grp5_sp5k42)] <- NA 
dta$longe4 <- rowSums(dta[c("longe4_sp1","longe4_sp2")], na.rm=T) > 0
dta$longe4[is.na(dta$longe4_sp1) & is.na(dta$longe4_sp2)] <- NA 

#use other OPV on any plot?
dta$other_opv_sp1 <- rowSums(dta[c("grp1a435","grp2b435","grp3c435","grp4d435", "grp5e435")], na.rm=T) > 0 
dta$other_opv_sp1[is.na(dta$grp1a42) & is.na(dta$grp2b42) & is.na(dta$grp3c42) & is.na(dta$grp4d42) & is.na(dta$grp5e42)] <- NA 
dta$other_opv_sp2 <- rowSums(dta[c("spouse2grp_sp1f435","spouse2grp_sp2g435","spouse2grp_sp3h435","spouse2group_sp4j435", "spouse2grp5_sp5k435")], na.rm=T) > 0 
dta$other_opv_sp2[is.na(dta$spouse2grp_sp1f42) & is.na(dta$spouse2grp_sp2g42) & is.na(dta$spouse2grp_sp3h42) & is.na(dta$spouse2group_sp4j42) & is.na(dta$spouse2grp5_sp5k42)] <- NA 
dta$other_opv <- rowSums(dta[c("other_opv_sp1","other_opv_sp2")], na.rm=T) > 0
dta$other_opv[is.na(dta$other_opv_sp1) & is.na(dta$other_opv_sp2)] <- NA 


dta$opv <-  rowSums(dta[c("longe5","longe4","other_opv")], na.rm=T) >0
dta$opv[is.na(dta$longe5) & is.na(dta$longe4) & is.na(dta$other_opv)] <- NA
dta$hybrid <-  rowSums(dta[c("longe10h","bazooka","other_hybrid")], na.rm=T) >0
dta$hybrid[is.na(dta$longe10h) & is.na(dta$bazooka) & is.na(dta$other_hybrid)] <- NA

################################################fert/seed - male managed

dta$fert_dap_sp1 <- rowSums((dta[c("grp1a301","grp2b301","grp3c301","grp4d301","grp5e301")]>0) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$fert_dap_sp1[rowSums(is.na((dta[c("grp1a301","grp2b301","grp3c301","grp4d301","grp5e301")]>0) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$fert_dap_sp1[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_dap_sp2 <- rowSums(( dta[c("spouse2grp_sp1f301","spouse2grp_sp2g301","spouse2grp_sp3h301","spouse2group_sp4j301","spouse2grp5_sp5k301")]>0)*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$fert_dap_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f301","spouse2grp_sp2g301","spouse2grp_sp3h301","spouse2group_sp4j301","spouse2grp5_sp5k301")]>0)*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$fert_dap_sp2[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_dap_mm <-  rowSums(dta[c("fert_dap_sp1","fert_dap_sp2")], na.rm=T) > 0
dta$fert_dap_mm[is.na(dta$fert_dap_sp1) & is.na(dta$fert_dap_sp2)] <- NA
dta$fert_dap_mm[dta$fert_mm==FALSE] <- FALSE

################

dta$fert_urea_sp1 <- rowSums((dta[c("grp1a302","grp2b302","grp3c302","grp4d302","grp5e302")]>0) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$fert_urea_sp1[rowSums(is.na((dta[c("grp1a302","grp2b302","grp3c302","grp4d302","grp5e302")]>0) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$fert_urea_sp1[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_urea_sp2 <- rowSums(( dta[c("spouse2grp_sp1f302","spouse2grp_sp2g302","spouse2grp_sp3h302","spouse2group_sp4j302","spouse2grp5_sp5k302")]>0)*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$fert_urea_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f302","spouse2grp_sp2g302","spouse2grp_sp3h302","spouse2group_sp4j302","spouse2grp5_sp5k302")]>0)*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$fert_urea_sp2[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_urea_mm <-  rowSums(dta[c("fert_urea_sp1","fert_urea_sp2")], na.rm=T) > 0
dta$fert_urea_mm[is.na(dta$fert_urea_sp1) & is.na(dta$fert_urea_sp2)] <- NA
dta$fert_urea_mm[dta$fert_mm==FALSE] <- FALSE

###

dta$fert_org_sp1 <- rowSums((dta[c("grp1a303","grp2b303","grp3c303","grp4d303","grp5e303")]>0) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$fert_org_sp1[rowSums(is.na((dta[c("grp1a303","grp2b303","grp3c303","grp4d303","grp5e303")]>0) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$fert_org_sp1[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_org_sp2 <- rowSums(( dta[c("spouse2grp_sp1f303","spouse2grp_sp2g303","spouse2grp_sp3h303","spouse2group_sp4j303","spouse2grp5_sp5k303")]>0)*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$fert_org_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f303","spouse2grp_sp2g303","spouse2grp_sp3h303","spouse2group_sp4j303","spouse2grp5_sp5k303")]>0)*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$fert_org_sp2[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_org_mm <-  rowSums(dta[c("fert_org_sp1","fert_org_sp2")], na.rm=T) > 0
dta$fert_org_mm[is.na(dta$fert_org_sp1) & is.na(dta$fert_org_sp2)] <- NA
dta$fert_org_mm[dta$fert_mm==FALSE] <- FALSE

###seed

dta$longe10h_sp1 <- rowSums((dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]>0) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$longe10h_sp1[rowSums(is.na((dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]>0) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$longe10h_sp1[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe10h_sp2 <- rowSums(( dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]>0)*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$longe10h_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]>0)*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$longe10h_sp2[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe10h_mm <-  rowSums(dta[c("longe10h_sp1","longe10h_sp2")], na.rm=T) > 0
dta$longe10h_mm[is.na(dta$longe10h_sp1) & is.na(dta$longe10h_sp2)] <- NA
dta$longe10h_mm[dta$impseed_mm==FALSE] <- FALSE

dta$bazooka_sp1 <- rowSums((dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]>0) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$bazooka_sp1[rowSums(is.na((dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]>0) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$bazooka_sp1[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$bazooka_sp2 <- rowSums(( dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]>0)*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$bazooka_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]>0)*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$bazooka_sp2[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$bazooka_mm <-  rowSums(dta[c("bazooka_sp1","bazooka_sp2")], na.rm=T) > 0
dta$bazooka_mm[is.na(dta$bazooka_sp1) & is.na(dta$bazooka_sp2)] <- NA
dta$bazooka_mm[dta$impseed_mm==FALSE] <- FALSE

####

dta$other_hybrid_sp1 <- rowSums((dta[c("grp1a436","grp2b436","grp3c436","grp4d436", "grp5e436")]>0) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$other_hybrid_sp1[rowSums(is.na((dta[c("grp1a436","grp2b436","grp3c436","grp4d436", "grp5e436")]>0) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$other_hybrid_sp1[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$other_hybrid_sp2 <- rowSums(( dta[c("spouse2grp_sp1f436","spouse2grp_sp2g436","spouse2grp_sp3h436","spouse2group_sp4j436", "spouse2grp5_sp5k436")]>0)*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$other_hybrid_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f436","spouse2grp_sp2g436","spouse2grp_sp3h436","spouse2group_sp4j436", "spouse2grp5_sp5k436")]>0)*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$other_hybrid_sp2[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$other_hybrid_mm <-  rowSums(dta[c("other_hybrid_sp1","other_hybrid_sp2")], na.rm=T) > 0
dta$other_hybrid_mm[is.na(dta$other_hybrid_sp1) & is.na(dta$other_hybrid_sp2)] <- NA
dta$other_hybrid_mm[dta$impseed_mm==FALSE] <- FALSE

##########

dta$longe5_sp1 <- rowSums((dta[c("grp1a433","grp2b433","grp3c433","grp4d433", "grp5e433")]>0) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$longe5_sp1[rowSums(is.na((dta[c("grp1a433","grp2b433","grp3c433","grp4d433", "grp5e433")]>0) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$longe5_sp1[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe5_sp2 <- rowSums(( dta[c("spouse2grp_sp1f433","spouse2grp_sp2g433","spouse2grp_sp3h433","spouse2group_sp4j433", "spouse2grp5_sp5k433")]>0)*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$longe5_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f433","spouse2grp_sp2g433","spouse2grp_sp3h433","spouse2group_sp4j433", "spouse2grp5_sp5k433")]>0)*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$longe5_sp2[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe5_mm <-  rowSums(dta[c("longe5_sp1","longe5_sp2")], na.rm=T) > 0
dta$longe5_mm[is.na(dta$longe5_sp1) & is.na(dta$longe5_sp2)] <- NA
dta$longe5_mm[dta$impseed_mm==FALSE] <- FALSE

###

dta$longe4_sp1 <- rowSums((dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]>0) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$longe4_sp1[rowSums(is.na((dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]>0) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$longe4_sp1[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe4_sp2 <- rowSums(( dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]>0)*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$longe4_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]>0)*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$longe4_sp2[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe4_mm <-  rowSums(dta[c("longe4_sp1","longe4_sp2")], na.rm=T) > 0
dta$longe4_mm[is.na(dta$longe4_sp1) & is.na(dta$longe4_sp2)] <- NA
dta$longe4_mm[dta$impseed_mm==FALSE] <- FALSE

####
dta$other_opv_sp1 <- rowSums((dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]>0) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$other_opv_sp1[rowSums(is.na((dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]>0) *(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$other_opv_sp1[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$other_opv_sp2 <- rowSums(( dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]>0)*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0), na.rm=T)
dta$other_opv_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]>0)*(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")] >0)))==5] <- NA
dta$other_opv_sp2[rowSums(dta[c("dec_man_pl1","dec_man_pl2","dec_man_pl3","dec_man_pl4","dec_man_pl5")]!=0,na.rm=T)==0]<- NA

dta$other_opv_mm <-  rowSums(dta[c("other_opv_sp1","other_opv_sp2")], na.rm=T) > 0
dta$other_opv_mm[is.na(dta$other_opv_sp1) & is.na(dta$other_opv_sp2)] <- NA
dta$other_opv_mm[dta$impseed_mm==FALSE] <- FALSE

dta$hybrid_mm <- rowSums(dta[c("longe10h_mm","bazooka_mm","other_hybrid_mm")], na.rm=T) >0
dta$hybrid_mm[is.na(dta$longe10h_mm) & is.na(dta$bazooka_mm) & is.na(dta$other_hybrid_mm) ] <- NA
dta$opv_mm <- rowSums(dta[c("longe5_mm","longe4_mm","other_opv_mm")], na.rm=T) >0
dta$opv_mm[is.na(dta$longe5_mm) & is.na(dta$longe4_mm) & is.na(dta$other_opv_mm) ] <- NA


################################################fert/seed - woman managed

dta$fert_dap_sp1 <- rowSums((dta[c("grp1a301","grp2b301","grp3c301","grp4d301","grp5e301")]>0) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$fert_dap_sp1[rowSums(is.na((dta[c("grp1a301","grp2b301","grp3c301","grp4d301","grp5e301")]>0) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$fert_dap_sp1[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_dap_sp2 <- rowSums(( dta[c("spouse2grp_sp1f301","spouse2grp_sp2g301","spouse2grp_sp3h301","spouse2group_sp4j301","spouse2grp5_sp5k301")]>0)*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$fert_dap_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f301","spouse2grp_sp2g301","spouse2grp_sp3h301","spouse2group_sp4j301","spouse2grp5_sp5k301")]>0)*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$fert_dap_sp2[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_dap_wm <-  rowSums(dta[c("fert_dap_sp1","fert_dap_sp2")], na.rm=T) > 0
dta$fert_dap_wm[is.na(dta$fert_dap_sp1) & is.na(dta$fert_dap_sp2)] <- NA
dta$fert_dap_wm[dta$fert_wm==FALSE] <- FALSE

################

dta$fert_urea_sp1 <- rowSums((dta[c("grp1a302","grp2b302","grp3c302","grp4d302","grp5e302")]>0) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$fert_urea_sp1[rowSums(is.na((dta[c("grp1a302","grp2b302","grp3c302","grp4d302","grp5e302")]>0) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$fert_urea_sp1[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_urea_sp2 <- rowSums(( dta[c("spouse2grp_sp1f302","spouse2grp_sp2g302","spouse2grp_sp3h302","spouse2group_sp4j302","spouse2grp5_sp5k302")]>0)*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$fert_urea_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f302","spouse2grp_sp2g302","spouse2grp_sp3h302","spouse2group_sp4j302","spouse2grp5_sp5k302")]>0)*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$fert_urea_sp2[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_urea_wm <-  rowSums(dta[c("fert_urea_sp1","fert_urea_sp2")], na.rm=T) > 0
dta$fert_urea_wm[is.na(dta$fert_urea_sp1) & is.na(dta$fert_urea_sp2)] <- NA
dta$fert_urea_wm[dta$fert_wm==FALSE] <- FALSE

###

dta$fert_org_sp1 <- rowSums((dta[c("grp1a303","grp2b303","grp3c303","grp4d303","grp5e303")]>0) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$fert_org_sp1[rowSums(is.na((dta[c("grp1a303","grp2b303","grp3c303","grp4d303","grp5e303")]>0) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$fert_org_sp1[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_org_sp2 <- rowSums(( dta[c("spouse2grp_sp1f303","spouse2grp_sp2g303","spouse2grp_sp3h303","spouse2group_sp4j303","spouse2grp5_sp5k303")]>0)*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$fert_org_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f303","spouse2grp_sp2g303","spouse2grp_sp3h303","spouse2group_sp4j303","spouse2grp5_sp5k303")]>0)*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$fert_org_sp2[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_org_wm <-  rowSums(dta[c("fert_org_sp1","fert_org_sp2")], na.rm=T) > 0
dta$fert_org_wm[is.na(dta$fert_org_sp1) & is.na(dta$fert_org_sp2)] <- NA
dta$fert_org_wm[dta$fert_wm==FALSE] <- FALSE

###seed

dta$longe10h_sp1 <- rowSums((dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]>0) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$longe10h_sp1[rowSums(is.na((dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]>0) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$longe10h_sp1[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe10h_sp2 <- rowSums(( dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]>0)*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$longe10h_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]>0)*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$longe10h_sp2[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe10h_wm <-  rowSums(dta[c("longe10h_sp1","longe10h_sp2")], na.rm=T) > 0
dta$longe10h_wm[is.na(dta$longe10h_sp1) & is.na(dta$longe10h_sp2)] <- NA
dta$longe10h_wm[dta$impseed_wm==FALSE] <- FALSE

dta$bazooka_sp1 <- rowSums((dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]>0) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$bazooka_sp1[rowSums(is.na((dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]>0) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$bazooka_sp1[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$bazooka_sp2 <- rowSums(( dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]>0)*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$bazooka_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]>0)*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$bazooka_sp2[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$bazooka_wm <-  rowSums(dta[c("bazooka_sp1","bazooka_sp2")], na.rm=T) > 0
dta$bazooka_wm[is.na(dta$bazooka_sp1) & is.na(dta$bazooka_sp2)] <- NA
dta$bazooka_wm[dta$impseed_wm==FALSE] <- FALSE

####

dta$other_hybrid_sp1 <- rowSums((dta[c("grp1a436","grp2b436","grp3c436","grp4d436", "grp5e436")]>0) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$other_hybrid_sp1[rowSums(is.na((dta[c("grp1a436","grp2b436","grp3c436","grp4d436", "grp5e436")]>0) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$other_hybrid_sp1[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$other_hybrid_sp2 <- rowSums(( dta[c("spouse2grp_sp1f436","spouse2grp_sp2g436","spouse2grp_sp3h436","spouse2group_sp4j436", "spouse2grp5_sp5k436")]>0)*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$other_hybrid_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f436","spouse2grp_sp2g436","spouse2grp_sp3h436","spouse2group_sp4j436", "spouse2grp5_sp5k436")]>0)*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$other_hybrid_sp2[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$other_hybrid_wm <-  rowSums(dta[c("other_hybrid_sp1","other_hybrid_sp2")], na.rm=T) > 0
dta$other_hybrid_wm[is.na(dta$other_hybrid_sp1) & is.na(dta$other_hybrid_sp2)] <- NA
dta$other_hybrid_wm[dta$impseed_wm==FALSE] <- FALSE

##########

dta$longe5_sp1 <- rowSums((dta[c("grp1a433","grp2b433","grp3c433","grp4d433", "grp5e433")]>0) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$longe5_sp1[rowSums(is.na((dta[c("grp1a433","grp2b433","grp3c433","grp4d433", "grp5e433")]>0) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$longe5_sp1[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe5_sp2 <- rowSums(( dta[c("spouse2grp_sp1f433","spouse2grp_sp2g433","spouse2grp_sp3h433","spouse2group_sp4j433", "spouse2grp5_sp5k433")]>0)*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$longe5_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f433","spouse2grp_sp2g433","spouse2grp_sp3h433","spouse2group_sp4j433", "spouse2grp5_sp5k433")]>0)*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$longe5_sp2[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe5_wm <-  rowSums(dta[c("longe5_sp1","longe5_sp2")], na.rm=T) > 0
dta$longe5_wm[is.na(dta$longe5_sp1) & is.na(dta$longe5_sp2)] <- NA
dta$longe5_wm[dta$impseed_wm==FALSE] <- FALSE

###

dta$longe4_sp1 <- rowSums((dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]>0) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$longe4_sp1[rowSums(is.na((dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]>0) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$longe4_sp1[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe4_sp2 <- rowSums(( dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]>0)*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$longe4_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]>0)*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$longe4_sp2[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe4_wm <-  rowSums(dta[c("longe4_sp1","longe4_sp2")], na.rm=T) > 0
dta$longe4_wm[is.na(dta$longe4_sp1) & is.na(dta$longe4_sp2)] <- NA
dta$longe4_wm[dta$impseed_wm==FALSE] <- FALSE

####
dta$other_opv_sp1 <- rowSums((dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]>0) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$other_opv_sp1[rowSums(is.na((dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]>0) *(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$other_opv_sp1[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$other_opv_sp2 <- rowSums(( dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]>0)*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0), na.rm=T)
dta$other_opv_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]>0)*(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")] >0)))==5] <- NA
dta$other_opv_sp2[rowSums(dta[c("dec_woman_pl1","dec_woman_pl2","dec_woman_pl3","dec_woman_pl4","dec_woman_pl5")]!=0,na.rm=T)==0]<- NA

dta$other_opv_wm <-  rowSums(dta[c("other_opv_sp1","other_opv_sp2")], na.rm=T) > 0
dta$other_opv_wm[is.na(dta$other_opv_sp1) & is.na(dta$other_opv_sp2)] <- NA
dta$other_opv_wm[dta$impseed_wm==FALSE] <- FALSE

dta$hybrid_wm <- rowSums(dta[c("longe10h_wm","bazooka_wm","other_hybrid_wm")], na.rm=T) >0
dta$hybrid_wm[is.na(dta$longe10h_wm) & is.na(dta$bazooka_wm) & is.na(dta$other_hybrid_wm) ] <- NA
dta$opv_wm <- rowSums(dta[c("longe5_wm","longe4_wm","other_opv_wm")], na.rm=T) >0
dta$opv_wm[is.na(dta$longe5_wm) & is.na(dta$longe4_wm) & is.na(dta$other_opv_wm) ] <- NA

################################################fert/seed - woman involved

dta$fert_dap_sp1 <- rowSums((dta[c("grp1a301","grp2b301","grp3c301","grp4d301","grp5e301")]>0) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$fert_dap_sp1[rowSums(is.na((dta[c("grp1a301","grp2b301","grp3c301","grp4d301","grp5e301")]>0) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$fert_dap_sp1[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_dap_sp2 <- rowSums(( dta[c("spouse2grp_sp1f301","spouse2grp_sp2g301","spouse2grp_sp3h301","spouse2group_sp4j301","spouse2grp5_sp5k301")]>0)*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$fert_dap_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f301","spouse2grp_sp2g301","spouse2grp_sp3h301","spouse2group_sp4j301","spouse2grp5_sp5k301")]>0)*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$fert_dap_sp2[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_dap_wi <-  rowSums(dta[c("fert_dap_sp1","fert_dap_sp2")], na.rm=T) > 0
dta$fert_dap_wi[is.na(dta$fert_dap_sp1) & is.na(dta$fert_dap_sp2)] <- NA
dta$fert_dap_wi[dta$fert_wi==FALSE] <- FALSE

################

dta$fert_urea_sp1 <- rowSums((dta[c("grp1a302","grp2b302","grp3c302","grp4d302","grp5e302")]>0) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$fert_urea_sp1[rowSums(is.na((dta[c("grp1a302","grp2b302","grp3c302","grp4d302","grp5e302")]>0) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$fert_urea_sp1[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_urea_sp2 <- rowSums(( dta[c("spouse2grp_sp1f302","spouse2grp_sp2g302","spouse2grp_sp3h302","spouse2group_sp4j302","spouse2grp5_sp5k302")]>0)*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$fert_urea_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f302","spouse2grp_sp2g302","spouse2grp_sp3h302","spouse2group_sp4j302","spouse2grp5_sp5k302")]>0)*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$fert_urea_sp2[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_urea_wi <-  rowSums(dta[c("fert_urea_sp1","fert_urea_sp2")], na.rm=T) > 0
dta$fert_urea_wi[is.na(dta$fert_urea_sp1) & is.na(dta$fert_urea_sp2)] <- NA
dta$fert_urea_wi[dta$fert_wi==FALSE] <- FALSE

###

dta$fert_org_sp1 <- rowSums((dta[c("grp1a303","grp2b303","grp3c303","grp4d303","grp5e303")]>0) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$fert_org_sp1[rowSums(is.na((dta[c("grp1a303","grp2b303","grp3c303","grp4d303","grp5e303")]>0) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$fert_org_sp1[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_org_sp2 <- rowSums(( dta[c("spouse2grp_sp1f303","spouse2grp_sp2g303","spouse2grp_sp3h303","spouse2group_sp4j303","spouse2grp5_sp5k303")]>0)*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$fert_org_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f303","spouse2grp_sp2g303","spouse2grp_sp3h303","spouse2group_sp4j303","spouse2grp5_sp5k303")]>0)*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$fert_org_sp2[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_org_wi <-  rowSums(dta[c("fert_org_sp1","fert_org_sp2")], na.rm=T) > 0
dta$fert_org_wi[is.na(dta$fert_org_sp1) & is.na(dta$fert_org_sp2)] <- NA
dta$fert_org_wi[dta$fert_wi==FALSE] <- FALSE

###seed

dta$longe10h_sp1 <- rowSums((dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]>0) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$longe10h_sp1[rowSums(is.na((dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]>0) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$longe10h_sp1[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe10h_sp2 <- rowSums(( dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]>0)*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$longe10h_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]>0)*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$longe10h_sp2[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe10h_wi <-  rowSums(dta[c("longe10h_sp1","longe10h_sp2")], na.rm=T) > 0
dta$longe10h_wi[is.na(dta$longe10h_sp1) & is.na(dta$longe10h_sp2)] <- NA
dta$longe10h_wi[dta$impseed_wi==FALSE] <- FALSE

dta$bazooka_sp1 <- rowSums((dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]>0) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$bazooka_sp1[rowSums(is.na((dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]>0) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$bazooka_sp1[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$bazooka_sp2 <- rowSums(( dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]>0)*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$bazooka_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]>0)*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$bazooka_sp2[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$bazooka_wi <-  rowSums(dta[c("bazooka_sp1","bazooka_sp2")], na.rm=T) > 0
dta$bazooka_wi[is.na(dta$bazooka_sp1) & is.na(dta$bazooka_sp2)] <- NA
dta$bazooka_wi[dta$impseed_wi==FALSE] <- FALSE

####

dta$other_hybrid_sp1 <- rowSums((dta[c("grp1a436","grp2b436","grp3c436","grp4d436", "grp5e436")]>0) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$other_hybrid_sp1[rowSums(is.na((dta[c("grp1a436","grp2b436","grp3c436","grp4d436", "grp5e436")]>0) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$other_hybrid_sp1[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$other_hybrid_sp2 <- rowSums(( dta[c("spouse2grp_sp1f436","spouse2grp_sp2g436","spouse2grp_sp3h436","spouse2group_sp4j436", "spouse2grp5_sp5k436")]>0)*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$other_hybrid_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f436","spouse2grp_sp2g436","spouse2grp_sp3h436","spouse2group_sp4j436", "spouse2grp5_sp5k436")]>0)*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$other_hybrid_sp2[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$other_hybrid_wi <-  rowSums(dta[c("other_hybrid_sp1","other_hybrid_sp2")], na.rm=T) > 0
dta$other_hybrid_wi[is.na(dta$other_hybrid_sp1) & is.na(dta$other_hybrid_sp2)] <- NA
dta$other_hybrid_wi[dta$impseed_wi==FALSE] <- FALSE

##########

dta$longe5_sp1 <- rowSums((dta[c("grp1a433","grp2b433","grp3c433","grp4d433", "grp5e433")]>0) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$longe5_sp1[rowSums(is.na((dta[c("grp1a433","grp2b433","grp3c433","grp4d433", "grp5e433")]>0) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$longe5_sp1[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe5_sp2 <- rowSums(( dta[c("spouse2grp_sp1f433","spouse2grp_sp2g433","spouse2grp_sp3h433","spouse2group_sp4j433", "spouse2grp5_sp5k433")]>0)*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$longe5_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f433","spouse2grp_sp2g433","spouse2grp_sp3h433","spouse2group_sp4j433", "spouse2grp5_sp5k433")]>0)*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$longe5_sp2[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe5_wi <-  rowSums(dta[c("longe5_sp1","longe5_sp2")], na.rm=T) > 0
dta$longe5_wi[is.na(dta$longe5_sp1) & is.na(dta$longe5_sp2)] <- NA
dta$longe5_wi[dta$impseed_wi==FALSE] <- FALSE

###

dta$longe4_sp1 <- rowSums((dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]>0) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$longe4_sp1[rowSums(is.na((dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]>0) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$longe4_sp1[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe4_sp2 <- rowSums(( dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]>0)*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$longe4_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]>0)*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$longe4_sp2[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe4_wi <-  rowSums(dta[c("longe4_sp1","longe4_sp2")], na.rm=T) > 0
dta$longe4_wi[is.na(dta$longe4_sp1) & is.na(dta$longe4_sp2)] <- NA
dta$longe4_wi[dta$impseed_wi==FALSE] <- FALSE

####
dta$other_opv_sp1 <- rowSums((dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]>0) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$other_opv_sp1[rowSums(is.na((dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]>0) *(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$other_opv_sp1[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$other_opv_sp2 <- rowSums(( dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]>0)*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0), na.rm=T)
dta$other_opv_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]>0)*(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")] >0)))==5] <- NA
dta$other_opv_sp2[rowSums(dta[c("dec_woman_involved_pl1","dec_woman_involved_pl2","dec_woman_involved_pl3","dec_woman_involved_pl4","dec_woman_involved_pl5")]!=0,na.rm=T)==0]<- NA

dta$other_opv_wi <-  rowSums(dta[c("other_opv_sp1","other_opv_sp2")], na.rm=T) > 0
dta$other_opv_wi[is.na(dta$other_opv_sp1) & is.na(dta$other_opv_sp2)] <- NA
dta$other_opv_wi[dta$impseed_wi==FALSE] <- FALSE

dta$hybrid_wi <- rowSums(dta[c("longe10h_wi","bazooka_wi","other_hybrid_wi")], na.rm=T) >0
dta$hybrid_wi[is.na(dta$longe10h_wi) & is.na(dta$bazooka_wi) & is.na(dta$other_hybrid_wi) ] <- NA
dta$opv_wi <- rowSums(dta[c("longe5_wi","longe4_wi","other_opv_wi")], na.rm=T) >0
dta$opv_wi[is.na(dta$longe5_wi) & is.na(dta$longe4_wi) & is.na(dta$other_opv_wi) ] <- NA

################################################fert/seed - both managed

dta$fert_dap_sp1 <- rowSums((dta[c("grp1a301","grp2b301","grp3c301","grp4d301","grp5e301")]>0) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$fert_dap_sp1[rowSums(is.na((dta[c("grp1a301","grp2b301","grp3c301","grp4d301","grp5e301")]>0) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$fert_dap_sp1[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_dap_sp2 <- rowSums(( dta[c("spouse2grp_sp1f301","spouse2grp_sp2g301","spouse2grp_sp3h301","spouse2group_sp4j301","spouse2grp5_sp5k301")]>0)*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$fert_dap_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f301","spouse2grp_sp2g301","spouse2grp_sp3h301","spouse2group_sp4j301","spouse2grp5_sp5k301")]>0)*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$fert_dap_sp2[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_dap_bm <-  rowSums(dta[c("fert_dap_sp1","fert_dap_sp2")], na.rm=T) > 0
dta$fert_dap_bm[is.na(dta$fert_dap_sp1) & is.na(dta$fert_dap_sp2)] <- NA
dta$fert_dap_bm[dta$fert_bm==FALSE] <- FALSE

################

dta$fert_urea_sp1 <- rowSums((dta[c("grp1a302","grp2b302","grp3c302","grp4d302","grp5e302")]>0) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$fert_urea_sp1[rowSums(is.na((dta[c("grp1a302","grp2b302","grp3c302","grp4d302","grp5e302")]>0) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$fert_urea_sp1[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_urea_sp2 <- rowSums(( dta[c("spouse2grp_sp1f302","spouse2grp_sp2g302","spouse2grp_sp3h302","spouse2group_sp4j302","spouse2grp5_sp5k302")]>0)*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$fert_urea_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f302","spouse2grp_sp2g302","spouse2grp_sp3h302","spouse2group_sp4j302","spouse2grp5_sp5k302")]>0)*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$fert_urea_sp2[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_urea_bm <-  rowSums(dta[c("fert_urea_sp1","fert_urea_sp2")], na.rm=T) > 0
dta$fert_urea_bm[is.na(dta$fert_urea_sp1) & is.na(dta$fert_urea_sp2)] <- NA
dta$fert_urea_bm[dta$fert_bm==FALSE] <- FALSE

###

dta$fert_org_sp1 <- rowSums((dta[c("grp1a303","grp2b303","grp3c303","grp4d303","grp5e303")]>0) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$fert_org_sp1[rowSums(is.na((dta[c("grp1a303","grp2b303","grp3c303","grp4d303","grp5e303")]>0) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$fert_org_sp1[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_org_sp2 <- rowSums(( dta[c("spouse2grp_sp1f303","spouse2grp_sp2g303","spouse2grp_sp3h303","spouse2group_sp4j303","spouse2grp5_sp5k303")]>0)*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$fert_org_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f303","spouse2grp_sp2g303","spouse2grp_sp3h303","spouse2group_sp4j303","spouse2grp5_sp5k303")]>0)*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$fert_org_sp2[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_org_bm <-  rowSums(dta[c("fert_org_sp1","fert_org_sp2")], na.rm=T) > 0
dta$fert_org_bm[is.na(dta$fert_org_sp1) & is.na(dta$fert_org_sp2)] <- NA
dta$fert_org_bm[dta$fert_bm==FALSE] <- FALSE

###seed

dta$longe10h_sp1 <- rowSums((dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]>0) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$longe10h_sp1[rowSums(is.na((dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]>0) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$longe10h_sp1[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe10h_sp2 <- rowSums(( dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]>0)*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$longe10h_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]>0)*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$longe10h_sp2[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe10h_bm <-  rowSums(dta[c("longe10h_sp1","longe10h_sp2")], na.rm=T) > 0
dta$longe10h_bm[is.na(dta$longe10h_sp1) & is.na(dta$longe10h_sp2)] <- NA
dta$longe10h_bm[dta$impseed_bm==FALSE] <- FALSE

dta$bazooka_sp1 <- rowSums((dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]>0) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$bazooka_sp1[rowSums(is.na((dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]>0) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$bazooka_sp1[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$bazooka_sp2 <- rowSums(( dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]>0)*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$bazooka_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]>0)*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$bazooka_sp2[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$bazooka_bm <-  rowSums(dta[c("bazooka_sp1","bazooka_sp2")], na.rm=T) > 0
dta$bazooka_bm[is.na(dta$bazooka_sp1) & is.na(dta$bazooka_sp2)] <- NA
dta$bazooka_bm[dta$impseed_bm==FALSE] <- FALSE

####

dta$other_hybrid_sp1 <- rowSums((dta[c("grp1a436","grp2b436","grp3c436","grp4d436", "grp5e436")]>0) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$other_hybrid_sp1[rowSums(is.na((dta[c("grp1a436","grp2b436","grp3c436","grp4d436", "grp5e436")]>0) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$other_hybrid_sp1[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$other_hybrid_sp2 <- rowSums(( dta[c("spouse2grp_sp1f436","spouse2grp_sp2g436","spouse2grp_sp3h436","spouse2group_sp4j436", "spouse2grp5_sp5k436")]>0)*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$other_hybrid_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f436","spouse2grp_sp2g436","spouse2grp_sp3h436","spouse2group_sp4j436", "spouse2grp5_sp5k436")]>0)*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$other_hybrid_sp2[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$other_hybrid_bm <-  rowSums(dta[c("other_hybrid_sp1","other_hybrid_sp2")], na.rm=T) > 0
dta$other_hybrid_bm[is.na(dta$other_hybrid_sp1) & is.na(dta$other_hybrid_sp2)] <- NA
dta$other_hybrid_bm[dta$impseed_bm==FALSE] <- FALSE

##########

dta$longe5_sp1 <- rowSums((dta[c("grp1a433","grp2b433","grp3c433","grp4d433", "grp5e433")]>0) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$longe5_sp1[rowSums(is.na((dta[c("grp1a433","grp2b433","grp3c433","grp4d433", "grp5e433")]>0) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$longe5_sp1[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe5_sp2 <- rowSums(( dta[c("spouse2grp_sp1f433","spouse2grp_sp2g433","spouse2grp_sp3h433","spouse2group_sp4j433", "spouse2grp5_sp5k433")]>0)*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$longe5_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f433","spouse2grp_sp2g433","spouse2grp_sp3h433","spouse2group_sp4j433", "spouse2grp5_sp5k433")]>0)*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$longe5_sp2[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe5_bm <-  rowSums(dta[c("longe5_sp1","longe5_sp2")], na.rm=T) > 0
dta$longe5_bm[is.na(dta$longe5_sp1) & is.na(dta$longe5_sp2)] <- NA
dta$longe5_bm[dta$impseed_bm==FALSE] <- FALSE

###

dta$longe4_sp1 <- rowSums((dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]>0) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$longe4_sp1[rowSums(is.na((dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]>0) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$longe4_sp1[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe4_sp2 <- rowSums(( dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]>0)*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$longe4_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]>0)*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$longe4_sp2[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe4_bm <-  rowSums(dta[c("longe4_sp1","longe4_sp2")], na.rm=T) > 0
dta$longe4_bm[is.na(dta$longe4_sp1) & is.na(dta$longe4_sp2)] <- NA
dta$longe4_bm[dta$impseed_bm==FALSE] <- FALSE

####
dta$other_opv_sp1 <- rowSums((dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]>0) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$other_opv_sp1[rowSums(is.na((dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]>0) *(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$other_opv_sp1[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$other_opv_sp2 <- rowSums(( dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]>0)*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0), na.rm=T)
dta$other_opv_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]>0)*(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")] >0)))==5] <- NA
dta$other_opv_sp2[rowSums(dta[c("dec_both_pl1","dec_both_pl2","dec_both_pl3","dec_both_pl4","dec_both_pl5")]!=0,na.rm=T)==0]<- NA

dta$other_opv_bm <-  rowSums(dta[c("other_opv_sp1","other_opv_sp2")], na.rm=T) > 0
dta$other_opv_bm[is.na(dta$other_opv_sp1) & is.na(dta$other_opv_sp2)] <- NA
dta$other_opv_bm[dta$impseed_bm==FALSE] <- FALSE

dta$hybrid_bm <- rowSums(dta[c("longe10h_bm","bazooka_bm","other_hybrid_bm")], na.rm=T) >0
dta$hybrid_bm[is.na(dta$longe10h_bm) & is.na(dta$bazooka_bm) & is.na(dta$other_hybrid_bm) ] <- NA
dta$opv_bm <- rowSums(dta[c("longe5_bm","longe4_bm","other_opv_bm")], na.rm=T) >0
dta$opv_bm[is.na(dta$longe5_bm) & is.na(dta$longe4_bm) & is.na(dta$other_opv_bm) ] <- NA

################################################fert/seed - wshare1 managed

dta$fert_dap_sp1 <- rowSums((dta[c("grp1a301","grp2b301","grp3c301","grp4d301","grp5e301")]>0) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$fert_dap_sp1[rowSums(is.na((dta[c("grp1a301","grp2b301","grp3c301","grp4d301","grp5e301")]>0) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$fert_dap_sp1[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_dap_sp2 <- rowSums(( dta[c("spouse2grp_sp1f301","spouse2grp_sp2g301","spouse2grp_sp3h301","spouse2group_sp4j301","spouse2grp5_sp5k301")]>0)*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$fert_dap_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f301","spouse2grp_sp2g301","spouse2grp_sp3h301","spouse2group_sp4j301","spouse2grp5_sp5k301")]>0)*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$fert_dap_sp2[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_dap_wm_share <-  rowSums(dta[c("fert_dap_sp1","fert_dap_sp2")], na.rm=T) > 0
dta$fert_dap_wm_share[is.na(dta$fert_dap_sp1) & is.na(dta$fert_dap_sp2)] <- NA
dta$fert_dap_wm_share[dta$fert_wm_share==FALSE] <- FALSE

################

dta$fert_urea_sp1 <- rowSums((dta[c("grp1a302","grp2b302","grp3c302","grp4d302","grp5e302")]>0) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$fert_urea_sp1[rowSums(is.na((dta[c("grp1a302","grp2b302","grp3c302","grp4d302","grp5e302")]>0) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$fert_urea_sp1[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_urea_sp2 <- rowSums(( dta[c("spouse2grp_sp1f302","spouse2grp_sp2g302","spouse2grp_sp3h302","spouse2group_sp4j302","spouse2grp5_sp5k302")]>0)*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$fert_urea_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f302","spouse2grp_sp2g302","spouse2grp_sp3h302","spouse2group_sp4j302","spouse2grp5_sp5k302")]>0)*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$fert_urea_sp2[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_urea_wm_share <-  rowSums(dta[c("fert_urea_sp1","fert_urea_sp2")], na.rm=T) > 0
dta$fert_urea_wm_share[is.na(dta$fert_urea_sp1) & is.na(dta$fert_urea_sp2)] <- NA
dta$fert_urea_wm_share[dta$fert_wm_share==FALSE] <- FALSE

###

dta$fert_org_sp1 <- rowSums((dta[c("grp1a303","grp2b303","grp3c303","grp4d303","grp5e303")]>0) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$fert_org_sp1[rowSums(is.na((dta[c("grp1a303","grp2b303","grp3c303","grp4d303","grp5e303")]>0) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$fert_org_sp1[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_org_sp2 <- rowSums(( dta[c("spouse2grp_sp1f303","spouse2grp_sp2g303","spouse2grp_sp3h303","spouse2group_sp4j303","spouse2grp5_sp5k303")]>0)*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$fert_org_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f303","spouse2grp_sp2g303","spouse2grp_sp3h303","spouse2group_sp4j303","spouse2grp5_sp5k303")]>0)*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$fert_org_sp2[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_org_wm_share <-  rowSums(dta[c("fert_org_sp1","fert_org_sp2")], na.rm=T) > 0
dta$fert_org_wm_share[is.na(dta$fert_org_sp1) & is.na(dta$fert_org_sp2)] <- NA
dta$fert_org_wm_share[dta$fert_wm_share==FALSE] <- FALSE

###seed

dta$longe10h_sp1 <- rowSums((dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]>0) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$longe10h_sp1[rowSums(is.na((dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]>0) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$longe10h_sp1[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe10h_sp2 <- rowSums(( dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]>0)*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$longe10h_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]>0)*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$longe10h_sp2[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe10h_wm_share <-  rowSums(dta[c("longe10h_sp1","longe10h_sp2")], na.rm=T) > 0
dta$longe10h_wm_share[is.na(dta$longe10h_sp1) & is.na(dta$longe10h_sp2)] <- NA
dta$longe10h_wm_share[dta$impseed_wm_share==FALSE] <- FALSE

dta$bazooka_sp1 <- rowSums((dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]>0) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$bazooka_sp1[rowSums(is.na((dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]>0) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$bazooka_sp1[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$bazooka_sp2 <- rowSums(( dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]>0)*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$bazooka_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]>0)*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$bazooka_sp2[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$bazooka_wm_share <-  rowSums(dta[c("bazooka_sp1","bazooka_sp2")], na.rm=T) > 0
dta$bazooka_wm_share[is.na(dta$bazooka_sp1) & is.na(dta$bazooka_sp2)] <- NA
dta$bazooka_wm_share[dta$impseed_wm_share==FALSE] <- FALSE

####

dta$other_hybrid_sp1 <- rowSums((dta[c("grp1a436","grp2b436","grp3c436","grp4d436", "grp5e436")]>0) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$other_hybrid_sp1[rowSums(is.na((dta[c("grp1a436","grp2b436","grp3c436","grp4d436", "grp5e436")]>0) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$other_hybrid_sp1[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$other_hybrid_sp2 <- rowSums(( dta[c("spouse2grp_sp1f436","spouse2grp_sp2g436","spouse2grp_sp3h436","spouse2group_sp4j436", "spouse2grp5_sp5k436")]>0)*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$other_hybrid_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f436","spouse2grp_sp2g436","spouse2grp_sp3h436","spouse2group_sp4j436", "spouse2grp5_sp5k436")]>0)*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$other_hybrid_sp2[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$other_hybrid_wm_share <-  rowSums(dta[c("other_hybrid_sp1","other_hybrid_sp2")], na.rm=T) > 0
dta$other_hybrid_wm_share[is.na(dta$other_hybrid_sp1) & is.na(dta$other_hybrid_sp2)] <- NA
dta$other_hybrid_wm_share[dta$impseed_wm_share==FALSE] <- FALSE

##########

dta$longe5_sp1 <- rowSums((dta[c("grp1a433","grp2b433","grp3c433","grp4d433", "grp5e433")]>0) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$longe5_sp1[rowSums(is.na((dta[c("grp1a433","grp2b433","grp3c433","grp4d433", "grp5e433")]>0) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$longe5_sp1[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe5_sp2 <- rowSums(( dta[c("spouse2grp_sp1f433","spouse2grp_sp2g433","spouse2grp_sp3h433","spouse2group_sp4j433", "spouse2grp5_sp5k433")]>0)*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$longe5_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f433","spouse2grp_sp2g433","spouse2grp_sp3h433","spouse2group_sp4j433", "spouse2grp5_sp5k433")]>0)*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$longe5_sp2[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe5_wm_share <-  rowSums(dta[c("longe5_sp1","longe5_sp2")], na.rm=T) > 0
dta$longe5_wm_share[is.na(dta$longe5_sp1) & is.na(dta$longe5_sp2)] <- NA
dta$longe5_wm_share[dta$impseed_wm_share==FALSE] <- FALSE

###

dta$longe4_sp1 <- rowSums((dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]>0) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$longe4_sp1[rowSums(is.na((dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]>0) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$longe4_sp1[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe4_sp2 <- rowSums(( dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]>0)*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$longe4_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]>0)*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$longe4_sp2[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe4_wm_share <-  rowSums(dta[c("longe4_sp1","longe4_sp2")], na.rm=T) > 0
dta$longe4_wm_share[is.na(dta$longe4_sp1) & is.na(dta$longe4_sp2)] <- NA
dta$longe4_wm_share[dta$impseed_wm_share==FALSE] <- FALSE

####
dta$other_opv_sp1 <- rowSums((dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]>0) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$other_opv_sp1[rowSums(is.na((dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]>0) *(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$other_opv_sp1[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$other_opv_sp2 <- rowSums(( dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]>0)*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0), na.rm=T)
dta$other_opv_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]>0)*(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")] >0)))==5] <- NA
dta$other_opv_sp2[rowSums(dta[c("wshare_pl1","wshare_pl2","wshare_pl3","wshare_pl4","wshare_pl5")]!=0,na.rm=T)==0]<- NA

dta$other_opv_wm_share <-  rowSums(dta[c("other_opv_sp1","other_opv_sp2")], na.rm=T) > 0
dta$other_opv_wm_share[is.na(dta$other_opv_sp1) & is.na(dta$other_opv_sp2)] <- NA
dta$other_opv_wm_share[dta$impseed_wm_share==FALSE] <- FALSE

dta$hybrid_wm_share <- rowSums(dta[c("longe10h_wm_share","bazooka_wm_share","other_hybrid_wm_share")], na.rm=T) >0
dta$hybrid_wm_share[is.na(dta$longe10h_wm_share) & is.na(dta$bazooka_wm_share) & is.na(dta$other_hybrid_wm_share) ] <- NA
dta$opv_wm_share <- rowSums(dta[c("longe5_wm_share","longe4_wm_share","other_opv_wm_share")], na.rm=T) >0
dta$opv_wm_share[is.na(dta$longe5_wm_share) & is.na(dta$longe4_wm_share) & is.na(dta$other_opv_wm_share) ] <- NA
################################################fert/seed - wshare2

dta$fert_dap_sp1 <- rowSums((dta[c("grp1a301","grp2b301","grp3c301","grp4d301","grp5e301")]>0) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$fert_dap_sp1[rowSums(is.na((dta[c("grp1a301","grp2b301","grp3c301","grp4d301","grp5e301")]>0) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$fert_dap_sp1[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_dap_sp2 <- rowSums(( dta[c("spouse2grp_sp1f301","spouse2grp_sp2g301","spouse2grp_sp3h301","spouse2group_sp4j301","spouse2grp5_sp5k301")]>0)*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$fert_dap_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f301","spouse2grp_sp2g301","spouse2grp_sp3h301","spouse2group_sp4j301","spouse2grp5_sp5k301")]>0)*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$fert_dap_sp2[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_dap_wm_share2 <-  rowSums(dta[c("fert_dap_sp1","fert_dap_sp2")], na.rm=T) > 0
dta$fert_dap_wm_share2[is.na(dta$fert_dap_sp1) & is.na(dta$fert_dap_sp2)] <- NA
dta$fert_dap_wm_share2[dta$fert_wm_share2==FALSE] <- FALSE

################

dta$fert_urea_sp1 <- rowSums((dta[c("grp1a302","grp2b302","grp3c302","grp4d302","grp5e302")]>0) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$fert_urea_sp1[rowSums(is.na((dta[c("grp1a302","grp2b302","grp3c302","grp4d302","grp5e302")]>0) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$fert_urea_sp1[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_urea_sp2 <- rowSums(( dta[c("spouse2grp_sp1f302","spouse2grp_sp2g302","spouse2grp_sp3h302","spouse2group_sp4j302","spouse2grp5_sp5k302")]>0)*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$fert_urea_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f302","spouse2grp_sp2g302","spouse2grp_sp3h302","spouse2group_sp4j302","spouse2grp5_sp5k302")]>0)*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$fert_urea_sp2[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_urea_wm_share2 <-  rowSums(dta[c("fert_urea_sp1","fert_urea_sp2")], na.rm=T) > 0
dta$fert_urea_wm_share2[is.na(dta$fert_urea_sp1) & is.na(dta$fert_urea_sp2)] <- NA
dta$fert_urea_wm_share2[dta$fert_wm_share2==FALSE] <- FALSE

###

dta$fert_org_sp1 <- rowSums((dta[c("grp1a303","grp2b303","grp3c303","grp4d303","grp5e303")]>0) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$fert_org_sp1[rowSums(is.na((dta[c("grp1a303","grp2b303","grp3c303","grp4d303","grp5e303")]>0) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$fert_org_sp1[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_org_sp2 <- rowSums(( dta[c("spouse2grp_sp1f303","spouse2grp_sp2g303","spouse2grp_sp3h303","spouse2group_sp4j303","spouse2grp5_sp5k303")]>0)*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$fert_org_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f303","spouse2grp_sp2g303","spouse2grp_sp3h303","spouse2group_sp4j303","spouse2grp5_sp5k303")]>0)*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$fert_org_sp2[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$fert_org_wm_share2 <-  rowSums(dta[c("fert_org_sp1","fert_org_sp2")], na.rm=T) > 0
dta$fert_org_wm_share2[is.na(dta$fert_org_sp1) & is.na(dta$fert_org_sp2)] <- NA
dta$fert_org_wm_share2[dta$fert_wm_share2==FALSE] <- FALSE

###seed

dta$longe10h_sp1 <- rowSums((dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]>0) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$longe10h_sp1[rowSums(is.na((dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]>0) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$longe10h_sp1[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe10h_sp2 <- rowSums(( dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]>0)*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$longe10h_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]>0)*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$longe10h_sp2[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe10h_wm_share2 <-  rowSums(dta[c("longe10h_sp1","longe10h_sp2")], na.rm=T) > 0
dta$longe10h_wm_share2[is.na(dta$longe10h_sp1) & is.na(dta$longe10h_sp2)] <- NA
dta$longe10h_wm_share2[dta$impseed_wm_share2==FALSE] <- FALSE

dta$bazooka_sp1 <- rowSums((dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]>0) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$bazooka_sp1[rowSums(is.na((dta[c("grp1a431","grp2b431","grp3c431","grp4d431", "grp5e431")]>0) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$bazooka_sp1[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$bazooka_sp2 <- rowSums(( dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]>0)*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$bazooka_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f431","spouse2grp_sp2g431","spouse2grp_sp3h431","spouse2group_sp4j431", "spouse2grp5_sp5k431")]>0)*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$bazooka_sp2[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$bazooka_wm_share2 <-  rowSums(dta[c("bazooka_sp1","bazooka_sp2")], na.rm=T) > 0
dta$bazooka_wm_share2[is.na(dta$bazooka_sp1) & is.na(dta$bazooka_sp2)] <- NA
dta$bazooka_wm_share2[dta$impseed_wm_share2==FALSE] <- FALSE

####

dta$other_hybrid_sp1 <- rowSums((dta[c("grp1a436","grp2b436","grp3c436","grp4d436", "grp5e436")]>0) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$other_hybrid_sp1[rowSums(is.na((dta[c("grp1a436","grp2b436","grp3c436","grp4d436", "grp5e436")]>0) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$other_hybrid_sp1[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$other_hybrid_sp2 <- rowSums(( dta[c("spouse2grp_sp1f436","spouse2grp_sp2g436","spouse2grp_sp3h436","spouse2group_sp4j436", "spouse2grp5_sp5k436")]>0)*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$other_hybrid_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f436","spouse2grp_sp2g436","spouse2grp_sp3h436","spouse2group_sp4j436", "spouse2grp5_sp5k436")]>0)*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$other_hybrid_sp2[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$other_hybrid_wm_share2 <-  rowSums(dta[c("other_hybrid_sp1","other_hybrid_sp2")], na.rm=T) > 0
dta$other_hybrid_wm_share2[is.na(dta$other_hybrid_sp1) & is.na(dta$other_hybrid_sp2)] <- NA
dta$other_hybrid_wm_share2[dta$impseed_wm_share2==FALSE] <- FALSE

##########

dta$longe5_sp1 <- rowSums((dta[c("grp1a433","grp2b433","grp3c433","grp4d433", "grp5e433")]>0) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$longe5_sp1[rowSums(is.na((dta[c("grp1a433","grp2b433","grp3c433","grp4d433", "grp5e433")]>0) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$longe5_sp1[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe5_sp2 <- rowSums(( dta[c("spouse2grp_sp1f433","spouse2grp_sp2g433","spouse2grp_sp3h433","spouse2group_sp4j433", "spouse2grp5_sp5k433")]>0)*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$longe5_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f433","spouse2grp_sp2g433","spouse2grp_sp3h433","spouse2group_sp4j433", "spouse2grp5_sp5k433")]>0)*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$longe5_sp2[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe5_wm_share2 <-  rowSums(dta[c("longe5_sp1","longe5_sp2")], na.rm=T) > 0
dta$longe5_wm_share2[is.na(dta$longe5_sp1) & is.na(dta$longe5_sp2)] <- NA
dta$longe5_wm_share2[dta$impseed_wm_share2==FALSE] <- FALSE

###

dta$longe4_sp1 <- rowSums((dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]>0) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$longe4_sp1[rowSums(is.na((dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]>0) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$longe4_sp1[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe4_sp2 <- rowSums(( dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]>0)*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$longe4_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]>0)*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$longe4_sp2[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$longe4_wm_share2 <-  rowSums(dta[c("longe4_sp1","longe4_sp2")], na.rm=T) > 0
dta$longe4_wm_share2[is.na(dta$longe4_sp1) & is.na(dta$longe4_sp2)] <- NA
dta$longe4_wm_share2[dta$impseed_wm_share2==FALSE] <- FALSE

####
dta$other_opv_sp1 <- rowSums((dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]>0) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$other_opv_sp1[rowSums(is.na((dta[c("grp1a434","grp2b434","grp3c434","grp4d434", "grp5e434")]>0) *(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$other_opv_sp1[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$other_opv_sp2 <- rowSums(( dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]>0)*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0), na.rm=T)
dta$other_opv_sp2[rowSums(is.na(( dta[c("spouse2grp_sp1f434","spouse2grp_sp2g434","spouse2grp_sp3h434","spouse2group_sp4j434", "spouse2grp5_sp5k434")]>0)*(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")] >0)))==5] <- NA
dta$other_opv_sp2[rowSums(dta[c("wshare2_pl1","wshare2_pl2","wshare2_pl3","wshare2_pl4","wshare2_pl5")]!=0,na.rm=T)==0]<- NA

dta$other_opv_wm_share2 <-  rowSums(dta[c("other_opv_sp1","other_opv_sp2")], na.rm=T) > 0
dta$other_opv_wm_share2[is.na(dta$other_opv_sp1) & is.na(dta$other_opv_sp2)] <- NA
dta$other_opv_wm_share2[dta$impseed_wm_share2==FALSE] <- FALSE

dta$hybrid_wm_share2 <- rowSums(dta[c("longe10h_wm_share2","bazooka_wm_share2","other_hybrid_wm_share2")], na.rm=T) >0
dta$hybrid_wm_share2[is.na(dta$longe10h_wm_share2) & is.na(dta$bazooka_wm_share2) & is.na(dta$other_hybrid_wm_share2) ] <- NA
dta$opv_wm_share2 <- rowSums(dta[c("longe5_wm_share2","longe4_wm_share2","other_opv_wm_share2")], na.rm=T) >0
dta$opv_wm_share2[is.na(dta$longe5_wm_share2) & is.na(dta$longe4_wm_share2) & is.na(dta$other_opv_wm_share2) ] <- NA

#### calculated consumption expenditure
dta[c("maize_value", "sorghum_value", "millet_value", "rice_value", "cassava_value", "sweetpotatoes_value", "beans_value", "gnuts_value", "fruits_value", "veg_value", "sugar_value", "cooking_oil_value", "soap_value", "airtime_value")] <- 
lapply(dta[c("maize_value", "sorghum_value", "millet_value", "rice_value", "cassava_value", "sweetpotatoes_value", "beans_value", "gnuts_value", "fruits_value", "veg_value", "sugar_value", "cooking_oil_value", "soap_value", "airtime_value")], function(x) replace(x, x == 999, NA) )

dta$cons_sp1 <- rowSums(dta[c("maize_value", "sorghum_value", "millet_value", "rice_value", "cassava_value", "sweetpotatoes_value", "beans_value", "gnuts_value", "fruits_value", "veg_value", "sugar_value", "cooking_oil_value", "soap_value", "airtime_value")], na.rm=T)

dta[c("spouse2maize_value_sp", "spouse2sorghum_value_sp", "spouse2millet_value_sp", "spouse2rice_value_sp", "spouse2cassava_value_sp", "spouse2sweetpotatoes_value_sp", "spouse2beans_value_sp", "spouse2gnuts_value_sp", "spouse2fruits_value_sp", "spouse2veg_value_sp", "spouse2sugar_value_sp", "spouse2cooking_oil_value_sp", "spouse2soap_value_sp", "spouse2airtime_value_sp")] <- 
lapply(dta[c("spouse2maize_value_sp", "spouse2sorghum_value_sp", "spouse2millet_value_sp", "spouse2rice_value_sp", "spouse2cassava_value_sp", "spouse2sweetpotatoes_value_sp", "spouse2beans_value_sp", "spouse2gnuts_value_sp", "spouse2fruits_value_sp", "spouse2veg_value_sp", "spouse2sugar_value_sp", "spouse2cooking_oil_value_sp", "spouse2soap_value_sp", "spouse2airtime_value_sp")], function(x) replace(x, x == 999, NA) )

dta$cons_sp2 <- rowSums(dta[c("spouse2maize_value_sp", "spouse2sorghum_value_sp", "spouse2millet_value_sp", "spouse2rice_value_sp", "spouse2cassava_value_sp", "spouse2sweetpotatoes_value_sp", "spouse2beans_value_sp", "spouse2gnuts_value_sp", "spouse2fruits_value_sp", "spouse2veg_value_sp", "spouse2sugar_value_sp", "spouse2cooking_oil_value_sp", "spouse2soap_value_sp", "spouse2airtime_value_sp")], na.rm=T)

dta$cons <- rowMeans(dta[c("cons_sp1","cons_sp2")], na.rm=T)
dta$cons[dta$cons == 0] <- NA

dta$cons_maize_yes <- rowSums(cbind(dta$maize_cons=="Yes", dta$spouse2maize_sp=="Yes"), na.rm=T) >0
dta$cons_maize_yes[is.na(dta$maize_cons) & is.na(dta$spouse2maize_sp) ] <- NA

dta$cons_maize_val <- rowSums(dta[c("maize_value","spouse2maize_value_sp")], na.rm=T)
dta$cons_maize_val[is.na(dta$maize_cons) & is.na(dta$spouse2maize_sp) ] <- NA

##sold maize?q71
dta$sold_maize <- rowSums(cbind(dta$q71=="Yes",dta$spouse2r71=="Yes"), na.rm=T) >0
dta$sold_maize[is.na(dta$q71) & is.na(dta$spouse2r71)] <- NA

##kept maize for seed q70
dta$spouse2r70[dta$spouse2r70== "999"] <- NA
dta$q70[dta$q70== 999] <- NA
dta$spouse2r70 <- as.numeric(as.character(dta$spouse2r70))
dta$save_seed <- rowSums(dta[c("q70","spouse2r70")], na.rm=T) >0
dta$save_seed[is.na(dta$q70) & is.na(dta$spouse2r70)] <- NA

### better off than average?
dta$better_av <- rowSums(dta[c("q409","spouse2r409")], na.rm=T) ==1
dta$better_av[is.na(dta$q409) & is.na(dta$spouse2r409)] <- NA

### better off than 6 mo ago?
dta$better_6m <- rowSums(dta[c("q110","spouse2r110")], na.rm=T) ==1
dta$better_6m[is.na(dta$q110) & is.na(dta$spouse2r110)] <- NA

### can eat preferred food
### question is: In the past 1 month  were you or any household member NOT able to eat the kinds of foods you preferred because of a lack of food or resources?
dta$eatpref <- rowSums(cbind( dta$q111=="No", dta$spouse2r111=="No"), na.rm=T) > 0 
dta$eatpref[is.na(dta$q111) & is.na( dta$spouse2r111)] <- NA

### has enough food to eat? 
###question is: In the past 1 month did you or any household member have to eat fewer meals or smaller meals in a day because  of a lack of food or resources?
dta$eatenough <- rowSums(cbind( dta$q112=="No", dta$spouse2r112=="No"), na.rm=T) > 0 
dta$eatenough[is.na(dta$q112) & is.na( dta$spouse2r112)] <- NA

### communication within household
dta$man_tells_wife <- ifelse(dta$gender1=="man",dta$q100 <2,dta$spouse2r100 <2)
dta$wife_tells_man <-  ifelse(dta$gender1=="woman",dta$q100 <2,dta$spouse2r100 <2)
dta$both_tell <- dta$man_tells_wife & dta$wife_tells_man

dta$wife_listens <- ifelse(dta$gender1=="man", dta$q101 == 2,dta$spouse2r101 ==2)
dta$man_listens <- ifelse(dta$gender1=="woman", dta$q101 == 2,dta$spouse2r101 ==2)
dta$spouses_listen  <- dta$wife_listens & dta$man_listens

dta[ paste("yield_sp1",paste("_pl",1:5, sep=""), sep="")] <- dta[c("prod_pl1_sp1","prod_pl2_sp1","prod_pl3_sp1","prod_pl4_sp1", "prod_pl5_sp1")]/dta[c("area_pl1_sp1","area_pl2_sp1","area_pl3_sp1","area_pl4_sp1", "area_pl5_sp1")]
dta[ paste("yield_sp2",paste("_pl",1:5, sep=""), sep="")] <- dta[c("prod_pl1_sp2","prod_pl2_sp2","prod_pl3_sp2","prod_pl4_sp2", "prod_pl5_sp2")]/dta[c("area_pl1_sp2","area_pl2_sp2","area_pl3_sp2","area_pl4_sp2", "area_pl5_sp2")]


### merge in calls made to ivr system

ivr_log <- read.csv("/home/bjvca/data/projects/digital green/endline/data/raw/ivr_log.csv")
callers <- data.frame(names(table(ivr_log$Phone.Number)))
names(callers) <- "tel"
tels <- read.csv("/home/bjvca/data/projects/digital green/endline/data/working/tels.csv")[c("HHID","tel")]
tels$tel <- paste("256",tels$tel, sep="")
callers <- merge(callers,tels)
callers$called <- TRUE 
callers$tel <- NULL
dta <- merge(dta, callers,by.x="hhid", by.y="HHID" ,all.x=T)
dta$called[is.na(dta$called)] <- FALSE

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

write.csv(dta, "/home/bjvca/Dropbox (IFPRI)/admin/AWS.csv")

########################################### function definitions #################################
## RI: a function to calculate the single sided RI p-values
## FSR_RI: Randomization inference (RI) implementation of the Westfall-Young (1993) Free Stepdown Resampling procedure
## trim: function for triming a dataset *dataset* on a variable *var*
## FW_index:

### a function to calculate the single sided RI p-values
### this only works for all observations!!!
RI <- function(dep, indep, dta , nr_repl = 1000) {
#	indep <-  "(messenger != 'ctrl')+ivr+sms+as.factor(recipient) + as.factor(messenger)"

dep <- "know_space"
dta <- dta_bal
nr_repl <- 1000

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

		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["female",]>0]),"female",ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["couple",]>0]),"couple", "ctrl")))
		return(abs(coef(lm(as.formula(paste(dep,indep,sep="~")), data=dta_sim))[2]) > abs(crit) )
	}
	return(sum(oper)/nr_repl)
}


### a function to calculate the single sided RI p-values
RI_ivr <- function(dep, ctrl_vars = NULL, dta , nr_repl = 1000) {
	### allocates unique ID based on treatment cell status and village
	dta <- dta %>% 
    		mutate(uniqID = group_indices_(dta, .dots=c("recipient", "messenger"))) 
	### the NULL
	crit <- summary(lm(as.formula(paste(paste(dep,"(ivr=='yes')",sep="~"),ctrl_vars,sep="+")), data=dta))$coefficients[2,1]
	dta <-  data.table(cbind(dta[dep],dta[c("messenger","recipient","uniqID","hhid","ivr","sms","called","totsms")]))

	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
		dta_sim <- data.table(dta)
 		setDT(dta_sim)[,perm:=sample(ivr),by = (uniqID)]
	
		return(abs(coef(lm(as.formula(paste(paste(dep,"(perm=='yes')",sep="~"),ctrl_vars,sep="+")), data=dta_sim))[2]) > abs(crit) )
	}
	return(sum(oper)/nr_repl)
}


RI_ivreg_ivr <- function(dep, ctrl_vars = NULL, dta , nr_repl = 1000) {
	### allocates unique ID based on treatment cell status and village

	dta <- dta %>% 
    		mutate(uniqID = group_indices_(dta, .dots=c("recipient", "messenger"))) 
	### the NULL
	crit <-  coefficients(ivreg(as.formula(paste(paste(paste(paste(dep,"called", sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta))[2]
	dta <-  data.table(cbind(dta[dep],dta[c("messenger","recipient","uniqID","hhid","ivr","sms","called","totsms")]))

	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table","AER")) %dopar% {
		dta_sim <- data.table(dta)
 		setDT(dta_sim)[,instrument:=sample(ivr),by = (uniqID)]
		return(abs( coefficients(ivreg(as.formula(paste(paste(paste(paste(dep,"called", sep="~"),ctrl_vars, sep="+"),"instrument", sep="|"),ctrl_vars, sep="+")), data=dta_sim))[2]) > abs(crit) )
	}
	return(sum(oper)/nr_repl)
}



RI_sms <- function(dep, ctrl_vars=NULL, dta , nr_repl = 1000) {
	### allocates unique ID based on treatment cell status and village
	dta <- dta %>% 
    		mutate(uniqID = group_indices_(dta, .dots=c("recipient", "messenger"))) 
	### the NULL
	crit <- summary(lm(as.formula(paste(paste(dep,"(sms=='yes')",sep="~"),ctrl_vars,sep="+")), data=dta))$coefficients[2,1]
	dta <-  data.table(cbind(dta[dep],dta[c("messenger","recipient","uniqID","hhid","ivr","sms","called","totsms")]))

	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
		dta_sim <- data.table(dta)
 		setDT(dta_sim)[,perm:=sample(sms),by = (uniqID)]
	
		return(abs(coef(lm(as.formula(paste(paste(dep,"(perm=='yes')",sep="~"),ctrl_vars,sep="+")), data=dta_sim))[2]) > abs(crit) )
	}
	return(sum(oper)/nr_repl)
}

RI_ivreg_sms <- function(dep, ctrl_vars = NULL, dta , nr_repl = 1000) {
	### allocates unique ID based on treatment cell status and village

	dta <- dta %>% 
    		mutate(uniqID = group_indices_(dta, .dots=c("recipient", "messenger"))) 
	### the NULL
	crit <-  coefficients(ivreg(as.formula(paste(paste(paste(paste(dep,"(totsms > 0)", sep="~"),ctrl_vars, sep="+"),instrument, sep="|"),ctrl_vars, sep="+")), data=dta))[2]
	dta <-  data.table(cbind(dta[dep],dta[c("messenger","recipient","uniqID","hhid","ivr","sms","called","totsms")]))

	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table","AER")) %dopar% {
		dta_sim <- data.table(dta)
 		setDT(dta_sim)[,instrument:=sample(sms),by = (uniqID)]
		return(abs( coefficients(ivreg(as.formula(paste(paste(paste(paste(dep,"(totsms > 0)", sep="~"),ctrl_vars, sep="+"),"instrument", sep="|"),ctrl_vars, sep="+")), data=dta_sim))[2]) > abs(crit) )
	}
	return(sum(oper)/nr_repl)
}

#### 
#FSR_OLS <- function(deps, indep, dta, nr_repl = 1000) {
## use: FSR_OLS( c("know_space","know_combine","know_weed","know_armyworm") ,"messenger != 'ctrl'" ,dta_bal, nr_repl = totrep)


#	### determines treatmetn cell
#	dta <- dta %>% 
#    		mutate(treat = group_indices_(dta, .dots=c("recipient", "messenger"))) 
#	### allocates unique ID based on treatment cell status and village
#	dta <- dta %>% 
#    		mutate(uniqID = group_indices_(dta, .dots=c("distID", "subID","vilID"))) 
#	### the NULL
#### this should be estimated seperately:
#beta <- array(NA,length(deps))
#pval <- array(NA,length(deps))

#for (i in 1:length(deps)) {
#pval[i] <- summary(lm(as.formula(paste(deps[i],indep,sep="~")), data=dta))$coefficients[2,4]
#}
#	
#	Ord <- order(pval)
#pval <- pval[Ord]
#deps <- deps[Ord]
#	dta_sim <- dta
#	NSnps <- length(deps)
#	TestStatResamp <- matrix(nrow=nr_repl, ncol=NSnps)
#	TestStatResamp2 <- matrix(nrow=nr_repl, ncol=NSnps)

#oper <- foreach (repl = 1:nr_repl,.combine=rbind) %dopar% {

# 		resample <- function(x, ...) x[sample.int(length(x), ...)]
#		dta_sim$perm <- unlist(sapply(as.character(unique(dta$uniqID)), function(x) resample(dta$treat[dta$uniq==x])))
#		dta_sim$messenger[dta_sim$perm == 1  |dta_sim$perm == 3  |dta_sim$perm == 5  ] <- "male"
#		dta_sim$messenger[dta_sim$perm == 2  |dta_sim$perm == 4  |dta_sim$perm == 6  ] <- "female"
#		dta_sim$messenger[dta_sim$perm == 8  |dta_sim$perm == 9  |dta_sim$perm == 7 ] <- "couple"
#		dta_sim$messenger[dta_sim$perm == 10  |dta_sim$perm == 11  |dta_sim$perm == 12  ] <- "ctrl"
#		dta_sim$recipient[dta_sim$perm == 1  |dta_sim$perm == 2  |dta_sim$perm == 8 |dta_sim$perm == 10  ] <- "male"
#		dta_sim$recipient[dta_sim$perm == 3  |dta_sim$perm == 4  |dta_sim$perm == 9 |dta_sim$perm == 11  ] <- "female"
#		dta_sim$recipient[dta_sim$perm == 5  |dta_sim$perm == 6  |dta_sim$perm == 7 |dta_sim$perm == 12  ] <- "couple"
#		return(unlist(lapply(deps, function(dvar) summary(lm(as.formula(paste(dvar,indep,sep="~")), data=dta_sim))$coefficients[2,4])))
#	
#		}
#oper <- data.frame((oper))
#for (i in 1:dim(oper)[1]) {
#		for (j in 1:length(deps)) {
#			oper[i,j] <- min(oper[i,j:length(deps)])
#		}
#}
#	

#Padj <- apply(t(matrix(rep(pval,nr_repl),NSnps)) > oper, 2, mean)
#Padj1 <- Padj
#Padj2 <- Padj
#		for (j in 1:length(deps)) {
#			Padj2[j] <- max(Padj1[1:j])
#		}
#return(list(Ord, deps,Padj1, Padj2[Ord]))
#}


FSR_RI <- function(deps, indep, dta ,pvals = NULL, nr_repl_ri = 1000, nr_repl_pi = nr_repl_ri ) {
# Randomization inference (RI) implementation of the Westfall-Young (1993) Free Stepdown Resampling procedure for correcting for multiple inference. This should be used when RI was used to determine p-values for differences between treatment and control (indep) for a family of outcome variables (deps). It uses as inputs the RI p-values of the seperate tests for the outcomes in the family (pvals). We differentiate between the number of replications used to determine the RI p-values (nr_repl_ri), and the number of replications to do the actual adjustment nr_repl_pi
# example use: FSR_RI( c("space","striga","weed", "fert","impseed"),"messenger != 'ctrl'" ,dta_bal, c(0,0,0.27,0.00,0.16), nr_repl_ri = 100, nr_repl_pi=100)
# this function uses dplyr and supports parallel computing; add following as preamble:
#	library(dplyr)
#	library(doParallel)
#	cl <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
#	registerDoParallel(cl)
### important note: p-value significant digits need to correspond to the number of simulations nr_repl_pi, hence round(pvals,nr_repl_pi/100) in code below
### eg: if pval = 0.001, then it is advised to set nr_repl_pi >= 1000
## The function assumes p-values are determened using RI on the difference between treatment and control (ie. calculating the proportion of randomizations where the T-C difference exceeds the acutal T-C difference) for each hypothesis seperately. These are the p-values that need to be supplied in pvals.
## Next, p-values are determined based in nr_repl_ri randomizations. For each outcome, the difference between treatment and control is compared to the acutal difference and proportions are again determined. Note that here, each time one and the same randomization draw is used to test the different outcomes seperately. Once p-values are determined using these nr_repl_ri draws, monotonicity is enfored with respect to the original ordering of p-values.
## the above is then repeated nr_repl_pi, and the resulting simulated p-values are compared to the acutal p-values from pvals (determining the proportion of simulated p-values that are smaller than the actual p-values)
# finally, monotonicity is enfored uisng successive maximization

#testing
#deps <- c("know_space","know_combine","know_weed", "know_armyworm")
#indep <- "(messenger != 'ctrl') +ivr+sms+as.factor(recipient) + called + (totsms >0)"
#dta <- dta_bal
#pvals <- res_h0_know[1:4,3,h]
#nr_repl_ri <- 1000
#nr_repl_pi <- 1000

### determine treatment cell based on cominations in 2 factorial design
dta <- dta %>% mutate(treat = group_indices_(dta, .dots=c("recipient", "messenger"))) 
### allocates unique ID based on treatment cell status and village
dta <- dta %>% mutate(uniqID = group_indices_(dta, .dots=c("distID", "subID","vilID"))) 

beta <- array(NA,length(deps))
pval <- array(NA,length(deps))
for (i in 1:length(deps)) {
beta[i]  <- summary(lm(as.formula(paste(deps[i],indep,sep="~")), data=dta))$coefficients[2,1]
}
deps_init <- deps
pval <- pvals
Ord <- order(pval)
pval <- pval[Ord]
deps <- deps[Ord]
beta <- beta[Ord]

NSnps <- length(deps)
	dta <-  data.table(cbind(dta[deps],dta[c("messenger","recipient","treat","uniqID","hhid","ivr","sms","called","totsms")]))

oper <- foreach (repl = 1:(nr_repl_pi*nr_repl_ri),.combine=cbind,.packages = c("data.table")) %dopar% {
 	dta_sim <- data.table(dta)
setDT(dta_sim)[, perm := sample(treat), by = uniqID]
		dta_sim$recipient <- ifelse(dta_sim$perm %in% c(5,6,7,12), "couple", ifelse(dta_sim$perm %in% c(1, 2, 8, 10),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% c(1, 3, 5), "male", ifelse(dta_sim$perm %in% c(2, 4, 6),"female",ifelse(dta_sim$perm %in% c(8, 9, 7),"couple", "ctrl")))
		### this returns n-deps x (nr_repl_ri*nr_repl_pi) of treatment-control differences
		return(unlist(lapply(deps, function(dvar) coef(lm(as.formula(paste(dvar,indep,sep="~")), data=dta_sim))[2])))
}

#paste betas to oper
oper2 <- foreach (repl = 1:(nr_repl_pi),.combine=cbind,.packages = c("data.table")) %dopar% {
 	dta_sim <- data.table(dta)
setDT(dta_sim)[, perm := sample(treat), by = uniqID]
		dta_sim$recipient <- ifelse(dta_sim$perm %in% c(5,6,7,12), "couple", ifelse(dta_sim$perm %in% c(1, 2, 8, 10),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% c(1, 3, 5), "male", ifelse(dta_sim$perm %in% c(2, 4, 6),"female",ifelse(dta_sim$perm %in% c(8, 9, 7),"couple", "ctrl")))
		### this returns n-deps x (nr_repl_ri*nr_repl_pi) of treatment-control differences
		return(unlist(lapply(deps, function(dvar) coef(lm(as.formula(paste(dvar,indep,sep="~")), data=dta_sim))[2])))
}




oper <- data.frame(t(oper))
oper2 <- data.frame(t(oper2))
names(oper2) <- deps
oper2$ri_rep_nr <- 1:nr_repl_pi
oper$ri_rep_nr <-  rep(1:nr_repl_pi,each=nr_repl_ri)
oper <- merge(oper,oper2)
#difference between treatment and control is compared to the "acutal" difference for each ri 
for (i in 1:NSnps) {
	oper[,i+1] <- abs(oper[,(i+1)]) > abs(oper[,(i+1+NSnps)])
}

## now devide in blocks of nr_repl_ri
TestStatResamp <- matrix(nrow=nr_repl_pi, ncol=NSnps)
TestStatResamp2 <- matrix(nrow=nr_repl_pi, ncol=NSnps)

## and calcualte p-values 
for (i in 1:nr_repl_pi) {
	TestStatResamp[i,] <- colMeans(oper[oper$ri_rep_nr == i, 2:(NSnps+1)])
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

return(list(Ord, deps,Padj2, Padj2[match(deps_init,deps)]))
}



trim <- function(var, dataset, trim_perc=.1) {
### function for triming a dataset *dataset* on a variable *var*
return( subset(dataset,dataset[var] > quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)), na.rm=T)[1] & dataset[var] < quantile(dataset[var], c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[2]) )
}

FW_index <- function(treat, indexer, data,revcols = NULL, nr_repl=0) {
### function to make family wise index using covariance as weights (following http://cyrussamii.com/?p=2656)
### FW_index("messenger != 'ctrl' ", c("know_space", "know_combine", "know_weed"),dta)
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
	sig <- RI("index" ,treat , data, nr_repl = nr_repl)
} else {
	sig <- summary(lm(as.formula(paste("index",treat,sep="~")) , data=data))$coefficients[2,4]
}
return(list(mod,sig, data))
}



plot_RI <- function(data, man, out_sp1,out_sp2,treatment,nr_repl = 1000, trimlog=FALSE) {
#data <- dta
#man <- "dec_man_d"
#out_sp1 <- paste("yield_sp1",paste("_pl",1:5, sep=""), sep="")
#out_sp2 <-  paste("yield_sp2",paste("_pl",1:5, sep=""), sep="")
#treatment <- "messenger != 'ctrl'"
#repl <- 100
#plot_RI(dta, man = "dec_woman", out_sp1 =c("grp1a55a" ,"grp2b55b", "grp3c55b","grp4d55b","grp5e55b"),out_sp2 =c("spouse2grp_sp1f55a","spouse2grp_sp2g55b","spouse2grp_sp3h55b","spouse2group_sp4j55b","spouse2grp5_sp5k55b"),treatment , 100)
# plot_RI(dta, man = "dec_both_d", out_sp1 =paste("yield_sp1",paste("_pl",1:5, sep=""), sep=""),out_sp2 =paste("yield_sp2",paste("_pl",1:5, sep=""), sep=""),treatment , 100)


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
dta <- space_ind[!duplicated(space_ind$hhid),]


dta <- dta %>% mutate(treat = group_indices_(dta, .dots=c("recipient", "messenger"))) 
### allocates unique ID based on treatment cell status and village
dta <- dta %>% mutate(uniqID = group_indices_(dta, .dots=c("distID", "subID","vilID"))) 

	### the NULL

	dta$time <- NULL
	dta<- data.table(dta)
	oper <- foreach (repl = 1:nr_repl,.combine=cbind,.packages = c("data.table")) %dopar% {
 		dta_sim <- merge(space_ind,setDT(dta)[,perm:=sample(treat),by = (uniqID)][,c("hhid","perm")], by="hhid")
		dta_sim$recipient <- ifelse(dta_sim$perm %in% c(5,6,7,12), "couple", ifelse(dta_sim$perm %in% c(1, 2, 8, 10),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% c(1, 3, 5), "male", ifelse(dta_sim$perm %in% c(2, 4, 6),"female",ifelse(dta_sim$perm %in% c(8, 9, 7),"couple", "ctrl")))
		return(abs(coef(lm(as.formula(paste("outcome",treatment,sep="~")), data=dta_sim))[2]) > abs(crit) )
	}
	return(list(summary(lm(as.formula(paste("outcome",treatment,sep="~")), data=space_ind))$coefficients[1,1],summary(lm(as.formula(paste("outcome",treatment,sep="~")), data=space_ind))$coefficients[2,1], summary(lm(as.formula(paste("outcome",treatment,sep="~")), data=space_ind))$coefficients[2,4],sum(oper)/nr_repl))
}

plot_RI_dec <- function(data, man,treatment,h = h,nr_repl = 1000) {
data <- dta_bal
man <- "dectime_man"
#treatment <-  "(messenger == 'female') +ivr+sms+as.factor(recipient)+ as.factor(messenger)"
nr_repl <- 0
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
		dta_sim$recipient <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["couple",]>0]), "couple", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$recipient,dta$treat))[table(dta$recipient, dta$treat)["male",]>0]),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["male",]>0]), "male", ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["female",]>0]),"female",ifelse(dta_sim$perm %in% as.numeric(colnames(table(dta$messenger,dta$treat))[table(dta$messenger, dta$treat)["couple",]>0]),"couple", "ctrl")))
		return(abs(coef(lm(as.formula(paste("decide",treatment,sep="~")), data=dta_sim))[2]) > abs(crit) )
	}
	return(list(summary(lm(as.formula(paste("decide",treatment,sep="~")), data=space_ind))$coefficients[2,1],summary(lm(as.formula(paste("decide",treatment,sep="~")), data=space_ind))$coefficients[2,2], summary(lm(as.formula(paste("decide",treatment,sep="~")), data=space_ind))$coefficients[2,4],sum(oper)/nr_repl,mean_male,sd_male))
}

FSR_RI_plot <- function(deps, indep, dta_ind ,pvals = NULL, nr_repl_ri = 1000, nr_repl_pi = nr_repl_ri ) {
### this is for plot level - feed this a long dataset
# Randomization inference (RI) implementation of the Westfall-Young (1993) Free Stepdown Resampling procedure for correcting for multiple inference. This should be used when RI was used to determine p-values for differences between treatment and control (indep) for a family of outcome variables (deps). It uses as inputs the RI p-values of the seperate tests for the outcomes in the family (pvals). We differentiate between the number of replications used to determine the RI p-values (nr_repl_ri), and the number of replications to do the actual adjustment nr_repl_pi
# example use: FSR_RI( c("space","striga","weed", "fert","impseed"),"messenger != 'ctrl'" ,dta_bal, c(0,0,0.27,0.00,0.16), nr_repl_ri = 100, nr_repl_pi=100)
# this function uses dplyr and supports parallel computing; add following as preamble:
#	library(dplyr)
#	library(doParallel)
#	cl <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
#	registerDoParallel(cl)
### important note: p-value significant digits need to correspond to the number of simulations nr_repl_pi, hence round(pvals,nr_repl_pi/100) in code below
### eg: if pval = 0.001, then it is advised to set nr_repl_pi >= 1000
## The function assumes p-values are determened using RI on the difference between treatment and control (ie. calculating the proportion of randomizations where the T-C difference exceeds the acutal T-C difference) for each hypothesis seperately. These are the p-values that need to be supplied in pvals.
## Next, p-values are determined based in nr_repl_ri randomizations. For each outcome, the difference between treatment and control is compared to the acutal difference and proportions are again determined. Note that here, each time one and the same randomization draw is used to test the different outcomes seperately. Once p-values are determined using these nr_repl_ri draws, monotonicity is enfored with respect to the original ordering of p-values.
## the above is then repeated nr_repl_pi, and the resulting simulated p-values are compared to the acutal p-values from pvals (determining the proportion of simulated p-values that are smaller than the actual p-values)
# finally, monotonicity is enfored uisng successive maximization

#testing
#deps <- c("know_space","know_combine","know_weed", "know_armyworm")
#indep <- "(messenger != 'ctrl') +ivr+sms+as.factor(recipient) + called + (totsms >0)"
#dta <- dta_bal
#pvals <- res_h0_know[1:4,3,h]
#nr_repl_ri <- 1000
#nr_repl_pi <- 1000

### determine treatment cell based on cominations in 2 factorial design


beta <- array(NA,length(deps))
pval <- array(NA,length(deps))
for (i in 1:length(deps)) {
beta[i]  <- summary(lm(as.formula(paste(deps[i],indep,sep="~")), data=dta_ind))$coefficients[2,1]
}
deps_init <- deps
pval <- pvals
Ord <- order(pval)
pval <- pval[Ord]
deps <- deps[Ord]
beta <- beta[Ord]

dta <- dta_ind[!duplicated(dta_ind$hhid),]

dta <- dta %>% mutate(treat = group_indices_(dta, .dots=c("recipient", "messenger"))) 
### allocates unique ID based on treatment cell status and village
dta <- dta %>% mutate(uniqID = group_indices_(dta, .dots=c("distID", "subID","vilID"))) 

NSnps <- length(deps)
	dta <-  data.table(cbind(dta_ind[deps],dta[c("messenger","recipient","treat","uniqID","hhid","ivr","sms","called","totsms")]))
 	
oper <- foreach (repl = 1:(nr_repl_pi*nr_repl_ri),.combine=cbind,.packages = c("data.table")) %dopar% {
dta_sim <- data.table(dta)
		dta_sim <- merge(dta_ind,setDT(dta)[,perm:=sample(treat),by = (uniqID)][,c("hhid","perm")], by="hhid")
		dta_sim$recipient <- ifelse(dta_sim$perm %in% c(5,6,7,12), "couple", ifelse(dta_sim$perm %in% c(1, 2, 8, 10),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% c(1, 3, 5), "male", ifelse(dta_sim$perm %in% c(2, 4, 6),"female",ifelse(dta_sim$perm %in% c(8, 9, 7),"couple", "ctrl")))
		### this returns n-deps x (nr_repl_ri*nr_repl_pi) of treatment-control differences
		return(unlist(lapply(deps, function(dvar) coef(lm(as.formula(paste(dvar,indep,sep="~")), data=dta_sim))[2])))
}

#paste betas to oper
oper2 <- foreach (repl = 1:(nr_repl_pi),.combine=cbind,.packages = c("data.table")) %dopar% {
 	dta_sim <- data.table(dta)
dta_sim <- merge(dta_ind,setDT(dta)[,perm:=sample(treat),by = (uniqID)][,c("hhid","perm")], by="hhid")
		dta_sim$recipient <- ifelse(dta_sim$perm %in% c(5,6,7,12), "couple", ifelse(dta_sim$perm %in% c(1, 2, 8, 10),"male", "female"))
		dta_sim$messenger <- ifelse(dta_sim$perm %in% c(1, 3, 5), "male", ifelse(dta_sim$perm %in% c(2, 4, 6),"female",ifelse(dta_sim$perm %in% c(8, 9, 7),"couple", "ctrl")))
		### this returns n-deps x (nr_repl_ri*nr_repl_pi) of treatment-control differences
		return(unlist(lapply(deps, function(dvar) coef(lm(as.formula(paste(dvar,indep,sep="~")), data=dta_sim))[2])))
}




oper <- data.frame(t(oper))
oper2 <- data.frame(t(oper2))
names(oper2) <- deps
oper2$ri_rep_nr <- 1:nr_repl_pi
oper$ri_rep_nr <-  rep(1:nr_repl_pi,each=nr_repl_ri)
oper <- merge(oper,oper2)
#difference between treatment and control is compared to the "acutal" difference for each ri 
for (i in 1:NSnps) {
	oper[,i+1] <- abs(oper[,(i+1)]) > abs(oper[,(i+1+NSnps)])
}

## now devide in blocks of nr_repl_ri
TestStatResamp <- matrix(nrow=nr_repl_pi, ncol=NSnps)
TestStatResamp2 <- matrix(nrow=nr_repl_pi, ncol=NSnps)

## and calcualte p-values 
for (i in 1:nr_repl_pi) {
	TestStatResamp[i,] <- colMeans(oper[oper$ri_rep_nr == i, 2:(NSnps+1)])
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

return(list(Ord, deps,Padj2, Padj2[match(deps_init,deps)]))
}


### inter
###decsion to plant maize on plot:

dta$decplot1_m <- NA
dta$decplot2_m <- NA
dta$decplot3_m <- NA
dta$decplot4_m <- NA
dta$decplot5_m <- NA

dta[!is.na(dta$person_interviewed) & dta$person_interviewed == "man",c("decplot1_m","decplot2_m","decplot3_m", "decplot4_m", "decplot5_m")] <- dta[!is.na(dta$person_interviewed) & dta$person_interviewed == "man",c("grp1decide1","grp2decide2","grp3decide3","grp4decide4","grp5decide5")]

dta[!is.na(dta$person_interviewed) & dta$person_interviewed == "woman",c("decplot1_m","decplot2_m","decplot3_m", "decplot4_m", "decplot5_m")] <- dta[!is.na(dta$person_interviewed) & dta$person_interviewed == "woman",c("spouse2grp_sp1decide_sp1","spouse2grp_sp2decide_sp2","spouse2grp_sp3decide_sp3","spouse2group_sp4decidesp4","spouse2grp5_sp5decidesp5")]

dta$decplot1_f <- NA
dta$decplot2_f <- NA
dta$decplot3_f <- NA
dta$decplot4_f <- NA
dta$decplot5_f <- NA

dta[!is.na(dta$person_interviewed) & dta$person_interviewed == "woman",c("decplot1_f","decplot2_f","decplot3_f", "decplot4_f", "decplot5_f")] <- dta[!is.na(dta$person_interviewed) & dta$person_interviewed == "woman",c("grp1decide1","grp2decide2","grp3decide3","grp4decide4","grp5decide5")]

dta[!is.na(dta$person_interviewed) & dta$person_interviewed == "man",c("decplot1_f","decplot2_f","decplot3_f", "decplot4_f", "decplot5_f")] <- dta[!is.na(dta$person_interviewed) & dta$person_interviewed == "man",c("spouse2grp_sp1decide_sp1","spouse2grp_sp2decide_sp2","spouse2grp_sp3decide_sp3","spouse2group_sp4decidesp4","spouse2grp5_sp5decidesp5")]

dta$dectogether <- rowSums( dta[c("decplot1_m","decplot2_m","decplot3_m", "decplot4_m", "decplot5_m" )] ==  dta[c("decplot1_f","decplot2_f","decplot3_f", "decplot4_f", "decplot5_f" )] & (dta[c("decplot1_m","decplot2_m","decplot3_m", "decplot4_m", "decplot5_m" )] + dta[c("decplot1_f","decplot2_f","decplot3_f", "decplot4_f", "decplot5_f" )]==6) ,na.rm=T) >0
dta$dectogether[rowSums(is.na(dta[c("decplot1_m","decplot2_m","decplot3_m", "decplot4_m", "decplot5_m" )]))==5 &  rowSums(is.na(dta[c("decplot1_f","decplot2_f","decplot3_f", "decplot4_f", "decplot5_f" )]))==5 ]  <- NA
#strong: decide together to plant maize on all maize plots
dta$dectogether_all <- rowMeans( dta[c("decplot1_m","decplot2_m","decplot3_m", "decplot4_m", "decplot5_m" )] ==  dta[c("decplot1_f","decplot2_f","decplot3_f", 
"decplot4_f", "decplot5_f" )] & (dta[c("decplot1_m","decplot2_m","decplot3_m", "decplot4_m", "decplot5_m" )] + dta[c("decplot1_f","decplot2_f","decplot3_f", "decplot4_f", "decplot5_f" )]==6) ,na.rm=T) ==1
dta$dectogether_all[is.na(dta$dectogether_s)] <- FALSE
dta$dectogether_all[rowSums(is.na(dta[c("decplot1_m","decplot2_m","decplot3_m", "decplot4_m", "decplot5_m" )]))==5 &  rowSums(is.na(dta[c("decplot1_f","decplot2_f","decplot3_f", "decplot4_f", "decplot5_f" )]))==5 ]  <- NA

dta$decmale <-  rowSums( dta[c("decplot1_m","decplot2_m","decplot3_m", "decplot4_m", "decplot5_m" )] == 1 &  dta[c("decplot1_f","decplot2_f","decplot3_f", "decplot4_f", "decplot5_f" )]==2 ,na.rm=T) >0
dta$decmale[rowSums(is.na(dta[c("decplot1_m","decplot2_m","decplot3_m", "decplot4_m", "decplot5_m" )]))==5 &  rowSums(is.na(dta[c("decplot1_f","decplot2_f","decplot3_f", "decplot4_f", "decplot5_f" )]))==5 ]  <- NA

dta$decmale_all <-  rowMeans( dta[c("decplot1_m","decplot2_m","decplot3_m", "decplot4_m", "decplot5_m" )] == 1 &  dta[c("decplot1_f","decplot2_f","decplot3_f", "decplot4_f", "decplot5_f" )]==2 ,na.rm=T) ==1
dta$decmale_all[rowSums(is.na(dta[c("decplot1_m","decplot2_m","decplot3_m", "decplot4_m", "decplot5_m" )]))==5 &  rowSums(is.na(dta[c("decplot1_f","decplot2_f","decplot3_f", "decplot4_f", "decplot5_f" )]))==5 ]  <- NA

dta$decfemale <-  rowSums( dta[c("decplot1_m","decplot2_m","decplot3_m", "decplot4_m", "decplot5_m" )] == 2 &  dta[c("decplot1_f","decplot2_f","decplot3_f", "decplot4_f", "decplot5_f" )]==1 ,na.rm=T) >0
dta$decfemale[rowSums(is.na(dta[c("decplot1_m","decplot2_m","decplot3_m", "decplot4_m", "decplot5_m" )]))==5 &  rowSums(is.na(dta[c("decplot1_f","decplot2_f","decplot3_f", "decplot4_f", "decplot5_f" )]))==5 ]  <- NA

dta$decfemale_all <-  rowMeans( dta[c("decplot1_m","decplot2_m","decplot3_m", "decplot4_m", "decplot5_m" )] == 2 &  dta[c("decplot1_f","decplot2_f","decplot3_f", "decplot4_f", "decplot5_f" )]==1 ,na.rm=T) == 1
dta$decfemale[rowSums(is.na(dta[c("decplot1_m","decplot2_m","decplot3_m", "decplot4_m", "decplot5_m" )]))==5 &  rowSums(is.na(dta[c("decplot1_f","decplot2_f","decplot3_f", "decplot4_f", "decplot5_f" )]))==5 ]  <- NA

temp_m <- dta[c("hhid","decplot1_m","decplot2_m","decplot3_m", "decplot4_m", "decplot5_m" )]


#longdecide_m <- reshape(temp_m, varying = c("decplot1_m","decplot2_m","decplot3_m", "decplot4_m", "decplot5_m" ),v.names="decide", idvar="hhid", direction="long")

#temp_f <- dta[c("hhid","decplot1_f","decplot2_f","decplot3_f", "decplot4_f", "decplot5_f" )]


#longdecide_f <- reshape(temp_f, varying = c("decplot1_f","decplot2_f","decplot3_f", "decplot4_f", "decplot5_f" ),v.names="decide", idvar="hhid", direction="long")
#decidemaize <- cbind(longdecide_m, longdecide_f$decide)
#names(decidemaize) <- c("hhid","plot","dec_m","dec_f")
#decidemaize <- subset(decidemaize, !is.na(dec_m) | !is.na(dec_f))

#### 




