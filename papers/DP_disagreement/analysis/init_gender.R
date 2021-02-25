rm(list=ls())
### first run anonymize.R on raw data to remove identifiers
dta <- read.csv("/home/bjvca/data/projects/digital green/endline/data/endline.csv")

### drop female headed households
dta <- subset(dta, femalehead == 0)
dta$femhead <- NULL

########################################################## First order knowledge effects  ########################################################
### knowledge at aggreagate level - defined as follows: weak - if at least one person got the answer correct
dta$know_space <- rowSums(dta[c("a1","spouse2f1")] == 1, na.rm=T) >0
dta$know_space[is.na(dta$a1) & is.na(dta$spouse2f1)] <- NA 

dta$know_combine <- rowSums(dta[c("a2", "spouse2f2")] == 3, na.rm=T) >0
dta$know_combine[is.na(dta$a2)  & is.na(dta$spouse2f2)] <- NA

dta$know_weed <-  rowSums(dta[c("a3","spouse2f3")]== 2, na.rm=T) > 0
dta$know_weed[is.na(dta$a3)  & is.na(dta$spouse2f3)] <- NA

dta$know_armyworm <- rowSums(dta[c("a4","spouse2f4")] == 3, na.rm=T) > 0 
dta$know_armyworm[is.na(dta$a4) & is.na(dta$spouse2f4)] <- NA

### knowledge at aggreagate level - defined as follows: strong - both spouses got the answer correct
dta$know_space_j <- rowSums(dta[c("a1","spouse2f1")] == 1, na.rm=T) == 2
dta$know_space_j[is.na(dta$a1) & is.na(dta$spouse2f1)] <- NA 

dta$know_combine_j <- rowSums(dta[c("a2", "spouse2f2")] == 3, na.rm=T)  == 2
dta$know_combine_j[is.na(dta$a2)  & is.na(dta$spouse2f2)] <- NA

dta$know_weed_j <-  rowSums(dta[c("a3","spouse2f3")]== 2, na.rm=T)  == 2
dta$know_weed_j[is.na(dta$a3)  & is.na(dta$spouse2f3)] <- NA

dta$know_armyworm_j <- rowSums(dta[c("a4","spouse2f4")] == 3, na.rm=T)  == 2
dta$know_armyworm_j[is.na(dta$a4) & is.na(dta$spouse2f4)] <- NA

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

### communication within household
dta$man_tells_wife <- ifelse(dta$gender1=="man",dta$q100 <= 2,dta$spouse2r100 <= 2)
dta$man_tells_wife_scale <- ifelse(dta$gender1=="man",6 - dta$q100,6 - dta$spouse2r100)
dta$wife_tells_man <-  ifelse(dta$gender1=="woman",dta$q100 <= 2,dta$spouse2r100 <= 2)
dta$wife_tells_man_scale <-  ifelse(dta$gender1=="woman",6 - dta$q100,6 - dta$spouse2r100)
dta$tell_each_other <- (6 - ifelse(dta$gender1=="man",dta$q100,dta$spouse2r100) + 6 - ifelse(dta$gender1=="woman",dta$q100,dta$spouse2r100))/2
dta$both_tell <- dta$man_tells_wife & dta$wife_tells_man
dta$both_tell[is.na(dta$man_tells_wife) | is.na( dta$wife_tells_man)] <- NA

dta$wife_listens <- ifelse(dta$gender1=="man", dta$q101 == 2,dta$spouse2r101 ==2)
dta$wife_listens_scale <- ifelse(dta$gender1=="man", 6 - dta$q101, 6 - dta$spouse2r101)
dta$wife_listens[is.na(dta$wife_listens) & !is.na(dta$man_tells_wife)] <- FALSE
dta$man_listens <- ifelse(dta$gender1=="woman", dta$q101 == 2,dta$spouse2r101 ==2)
dta$man_listens_scale <- ifelse(dta$gender1=="woman", 6 - dta$q101, 6- dta$spouse2r101)
dta$man_listens[is.na(dta$man_listens) & !is.na(dta$wife_tells_man)] <- FALSE
dta$spouses_listen  <- dta$wife_listens & dta$man_listens
dta$spouses_listen[is.na(dta$wife_listens) | is.na( dta$man_listens)] <- NA
dta$spouses_listen[is.na(dta$spouses_listen) & !is.na(dta$both_tell)] <- FALSE

## make a measure for production
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
dta$spouse2grp_sp1f17[dta$spouse2grp_sp1f16 == 0] <- 0 
dta$spouse2grp_sp2g17[dta$spouse2grp_sp2g16 == 0] <- 0 
dta$spouse2grp_sp3h17[dta$spouse2grp_sp3h16 == 0] <- 0 
dta$spouse2group_sp4j17[dta$spouse2group_sp4j16 == 0] <- 0 
dta$spouse2grp5_sp5k17[dta$spouse2grp5_sp5k16 == 0] <- 0 

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
################################################################  AREA #####################################################################
dta[c("grp1a11","grp2b11","grp3c11","grp4d11", "grp5e11")] <- lapply(dta[c("grp1a11","grp2b11","grp3c11","grp4d11", "grp5e11")], function(x) replace(x, x == 999, NA) )

dta[c("spouse2grp_sp1f11","spouse2grp_sp2g11","spouse2grp_sp3h11","spouse2group_sp4j11", "spouse2grp5_sp5k11")] <- lapply(dta[c("spouse2grp_sp1f11","spouse2grp_sp2g11","spouse2grp_sp3h11","spouse2group_sp4j11", "spouse2grp5_sp5k11")], function(x) replace(x,x == 999,NA) )

dta$area_pl1_sp1 <-(dta$grp1a11 * ((dta$grp1a14) /100))
dta$area_pl2_sp1 <-(dta$grp2b11 * ((dta$grp2b14) /100))
dta$area_pl3_sp1 <-(dta$grp3c11 * ((dta$grp3c14) /100))
dta$area_pl4_sp1 <-(dta$grp4d11 * ((dta$grp4d14) /100))
dta$area_pl5_sp1 <-(dta$grp5e11 * ((dta$grp5e14) /100))
## same deal here, this will be zero of nothing was produced
dta$area_tot_sp1 <- rowSums(dta[c("area_pl1_sp1","area_pl2_sp1","area_pl3_sp1","area_pl4_sp1", "area_pl5_sp1")], na.rm=T)
dta$area_tot_sp1[is.na(dta$area_pl1_sp1) & is.na(dta$area_pl2_sp1) & is.na(dta$area_pl3_sp1) & is.na(dta$area_pl4_sp1) & is.na(dta$area_pl5_sp1)] <- NA

dta$area_pl1_sp2 <- (dta$spouse2grp_sp1f11 * ((dta$spouse2grp_sp1f14) /100))
dta$area_pl2_sp2 <- (dta$spouse2grp_sp2g11 * ((dta$spouse2grp_sp2g14) /100))
dta$area_pl3_sp2 <- (dta$spouse2grp_sp3h11 * ((dta$spouse2grp_sp3h14) /100))
dta$area_pl4_sp2 <- (dta$spouse2group_sp4j11 * ((dta$spouse2group_sp4j14) /100))
dta$area_pl5_sp2 <- (dta$spouse2grp5_sp5k11 * ((dta$spouse2grp5_sp5k14) /100))

dta$area_tot_sp2 <- rowSums(dta[c("area_pl1_sp2","area_pl2_sp2","area_pl3_sp2","area_pl4_sp2", "area_pl5_sp2")], na.rm=T)
dta$area_tot_sp2[is.na(dta$area_pl1_sp2) & is.na(dta$area_pl2_sp2) & is.na(dta$area_pl3_sp2) & is.na(dta$area_pl4_sp2) & is.na(dta$area_pl5_sp2)] <- NA

dta$area_hh_mean <- rowMeans(dta[c("area_tot_sp1","area_tot_sp2")], na.rm=T)
dta$area_hh_mean[is.na(dta$area_tot_sp1) & is.na(dta$area_tot_sp2)] <- NA
dta$area_hh_man <- NA
dta$area_hh_man[dta$person_interviewed == "man"] <- dta$area_tot_sp1[dta$person_interviewed == "man"]
dta$area_hh_man[dta$person_interviewed == "woman"] <- dta$area_tot_sp2[dta$person_interviewed == "woman"]
dta$area_hh_woman <- NA
dta$area_hh_woman[dta$person_interviewed == "woman"] <- dta$area_tot_sp1[dta$person_interviewed == "woman"]
dta$area_hh_woman[dta$person_interviewed == "man"] <- dta$area_tot_sp2[dta$person_interviewed == "man"]


################################################### time use reported by person interviewed
### self reported time use for sp1 and sp2 (basically renaming)
#prep

names(dta)[names(dta) %in% c("grp1field1a50a", "grp2field3b50a", "grp3field7c50a", "grp4field9d50a", "grp5field11e50a")] <-  c("time_prep_pl1_sp1_self","time_prep_pl2_sp1_self","time_prep_pl3_sp1_self","time_prep_pl4_sp1_self","time_prep_pl5_sp1_self")

names(dta)[names(dta) %in%  c("spouse2grp_sp1field_sp1f50a", "spouse2grp_sp2field_sp3g50a", "spouse2grp_sp3field_sp5h50a", "spouse2group_sp4field_sp9j50a", "spouse2grp5_sp5field_sp11k50a")] <-  c("time_prep_pl1_sp2_self","time_prep_pl2_sp2_self","time_prep_pl3_sp2_self","time_prep_pl4_sp2_self","time_prep_pl5_sp2_self")
#plant
names(dta)[names(dta) %in%  c("grp1field1a50b", "grp2field3b50b", "grp3field7c50b", "grp4field9d50b", "grp5field11e50b")] <-  c("time_plant_pl1_sp1_self","time_plant_pl2_sp1_self","time_plant_pl3_sp1_self","time_plant_pl4_sp1_self","time_plant_pl5_sp1_self")

names(dta)[names(dta) %in%  c("spouse2grp_sp1field_sp1f50b", "spouse2grp_sp2field_sp3g50b", "spouse2grp_sp3field_sp5h50b", "spouse2group_sp4field_sp9j50b", "spouse2grp5_sp5field_sp11k50b")] <-  c("time_plant_pl1_sp2_self","time_plant_pl2_sp2_self","time_plant_pl3_sp2_self","time_plant_pl4_sp2_self","time_plant_pl5_sp2_self")
#weed1
names(dta)[names(dta) %in%  c("grp1field1a50c", "grp2field3b50c", "grp3field7c50c", "grp4field9d50c", "grp5field11e50c")] <-  c("time_weed1_pl1_sp1_self","time_weed1_pl2_sp1_self","time_weed1_pl3_sp1_self","time_weed1_pl4_sp1_self","time_weed1_pl5_sp1_self")

names(dta)[names(dta) %in%  c("spouse2grp_sp1field_sp1f50c", "spouse2grp_sp2field_sp3g50c", "spouse2grp_sp3field_sp5h50c", "spouse2group_sp4field_sp9j50c", "spouse2grp5_sp5field_sp11k50c")] <-  c("time_weed1_pl1_sp2_self","time_weed1_pl2_sp2_self","time_weed1_pl3_sp2_self","time_weed1_pl4_sp2_self","time_weed1_pl5_sp2_self")
#weed2
names(dta)[names(dta) %in%  c("grp1field1a50d", "grp2field3b50d", "grp3field7c50d", "grp4field9d50d", "grp5field11e50d")] <-  c("time_weed2_pl1_sp1_self","time_weed2_pl2_sp1_self","time_weed2_pl3_sp1_self","time_weed2_pl4_sp1_self","time_weed2_pl5_sp1_self")

names(dta)[names(dta) %in%  c("spouse2grp_sp1field_sp1f50d", "spouse2grp_sp2field_sp3g50d", "spouse2grp_sp3field_sp5h50d", "spouse2group_sp4field_sp9j50d", "spouse2grp5_sp5field_sp11k50d")] <-  c("time_weed2_pl1_sp2_self","time_weed2_pl2_sp2_self","time_weed2_pl3_sp2_self","time_weed2_pl4_sp2_self","time_weed2_pl5_sp2_self")
#weed3
names(dta)[names(dta) %in%  c("grp1field1a50e", "grp2field3b50e", "grp3field7c50e", "grp4field9d50e", "grp5field11e50e")] <-  c("time_weed3_pl1_sp1_self","time_weed3_pl2_sp1_self","time_weed3_pl3_sp1_self","time_weed3_pl4_sp1_self","time_weed3_pl5_sp1_self")

names(dta)[names(dta) %in%  c("spouse2grp_sp1field_sp1f50e", "spouse2grp_sp2field_sp3g50e", "spouse2grp_sp3field_sp5h50e", "spouse2group_sp4field_sp9j50e", "spouse2grp5_sp5field_sp11k50e")] <-  c("time_weed3_pl1_sp2_self","time_weed3_pl2_sp2_self","time_weed3_pl3_sp2_self","time_weed3_pl4_sp2_self","time_weed3_pl5_sp2_self")
#spray
names(dta)[names(dta) %in%  c("grp1field1a50f", "grp2field3b50f", "grp3field7c50f", "grp4field9d50f", "grp5field11e50f")] <-  c("time_spray_pl1_sp1_self","time_spray_pl2_sp1_self","time_spray_pl3_sp1_self","time_spray_pl4_sp1_self","time_spray_pl5_sp1_self")

names(dta)[names(dta) %in%  c("spouse2grp_sp1field_sp1f50f", "spouse2grp_sp2field_sp3g50f", "spouse2grp_sp3field_sp5h50f", "spouse2group_sp4field_sp9j50f", "spouse2grp5_sp5field_sp11k50f")] <-  c("time_spray_pl1_sp2_self","time_spray_pl2_sp2_self","time_spray_pl3_sp2_self","time_spray_pl4_sp2_self","time_spray_pl5_sp2_self")
#harv
names(dta)[names(dta) %in%  c("grp1field1a50g", "grp2field3b50g", "grp3field7c50g", "grp4field9d50g", "grp5field11e50g")] <-  c("time_harv_pl1_sp1_self","time_harv_pl2_sp1_self","time_harv_pl3_sp1_self","time_harv_pl4_sp1_self","time_harv_pl5_sp1_self")

names(dta)[names(dta) %in%  c("spouse2grp_sp1field_sp1f50g", "spouse2grp_sp2field_sp3g50g", "spouse2grp_sp3field_sp5h50g", "spouse2group_sp4field_sp9j50g", "spouse2grp5_sp5field_sp11k50g")] <-  c("time_harv_pl1_sp2_self","time_harv_pl2_sp2_self","time_harv_pl3_sp2_self","time_harv_pl4_sp2_self","time_harv_pl5_sp2_self")

### report for spouse
#prep

names(dta)[names(dta) %in% c("grp1field2a50h", "grp2field4b50h", "grp3field8c50h", "grp4field10d50h", "grp5field12e50h")] <-  c("time_prep_pl1_sp1_other","time_prep_pl2_sp1_other","time_prep_pl3_sp1_other","time_prep_pl4_sp1_other","time_prep_pl5_sp1_other")

names(dta)[names(dta) %in%  c("spouse2grp_sp1field_sp2f50h", "spouse2grp_sp2field_sp4g50h", "spouse2grp_sp3field_sp6h50h", "spouse2group_sp4field_sp10j50h", "spouse2grp5_sp5field_sp12k50h")] <-  c("time_prep_pl1_sp2_other","time_prep_pl2_sp2_other","time_prep_pl3_sp2_other","time_prep_pl4_sp2_other","time_prep_pl5_sp2_other")

#plant
names(dta)[names(dta) %in%  c("grp1field2a50i", "grp2field4b50i", "grp3field8c50i", "grp4field10d50i", "grp5field12e50i")] <-  c("time_plant_pl1_sp1_other","time_plant_pl2_sp1_other","time_plant_pl3_sp1_other","time_plant_pl4_sp1_other","time_plant_pl5_sp1_other")

names(dta)[names(dta) %in%  c("spouse2grp_sp1field_sp2f50i", "spouse2grp_sp2field_sp4g50i", "spouse2grp_sp3field_sp6h50i", "spouse2group_sp4field_sp10j50i", "spouse2grp5_sp5field_sp12k50i")] <-  c("time_plant_pl1_sp2_other","time_plant_pl2_sp2_other","time_plant_pl3_sp2_other","time_plant_pl4_sp2_other","time_plant_pl5_sp2_other")
#weed1
names(dta)[names(dta) %in% c("grp1field2a50j", "grp2field4b50j", "grp3field8c50j", "grp4field10d50j", "grp5field12e50j")] <-  c("time_weed1_pl1_sp1_other","time_weed1_pl2_sp1_other","time_weed1_pl3_sp1_other","time_weed1_pl4_sp1_other","time_weed1_pl5_sp1_other")

names(dta)[names(dta) %in% c("spouse2grp_sp1field_sp2f50j", "spouse2grp_sp2field_sp4g50j", "spouse2grp_sp3field_sp6h50j", "spouse2group_sp4field_sp10j50j", "spouse2grp5_sp5field_sp12k50j")] <-  c("time_weed1_pl1_sp2_other","time_weed1_pl2_sp2_other","time_weed1_pl3_sp2_other","time_weed1_pl4_sp2_other","time_weed1_pl5_sp2_other")
#weed2
names(dta)[names(dta) %in%  c("grp1field2a50k", "grp2field4b50k", "grp3field8c50k", "grp4field10d50k", "grp5field12e50k")] <-  c("time_weed2_pl1_sp1_other","time_weed2_pl2_sp1_other","time_weed2_pl3_sp1_other","time_weed2_pl4_sp1_other","time_weed2_pl5_sp1_other")

names(dta)[names(dta) %in%  c("spouse2grp_sp1field_sp2f50k", "spouse2grp_sp2field_sp4g50k", "spouse2grp_sp3field_sp6h50k", "spouse2group_sp4field_sp10j50k", "spouse2grp5_sp5field_sp12k50k")] <-  c("time_weed2_pl1_sp2_other","time_weed2_pl2_sp2_other","time_weed2_pl3_sp2_other","time_weed2_pl4_sp2_other","time_weed2_pl5_sp2_other")
#weed3
names(dta)[names(dta) %in%  c("grp1field2a50l", "grp2field4b50l", "grp3field8c50l", "grp4field10d50l", "grp5field12e50l")] <-  c("time_weed3_pl1_sp1_other","time_weed3_pl2_sp1_other","time_weed3_pl3_sp1_other","time_weed3_pl4_sp1_other","time_weed3_pl5_sp1_other")

names(dta)[names(dta) %in%  c("spouse2grp_sp1field_sp2f50l", "spouse2grp_sp2field_sp4g50l", "spouse2grp_sp3field_sp6h50l", "spouse2group_sp4field_sp10j50l", "spouse2grp5_sp5field_sp12k50l")] <-  c("time_weed3_pl1_sp2_other","time_weed3_pl2_sp2_other","time_weed3_pl3_sp2_other","time_weed3_pl4_sp2_other","time_weed3_pl5_sp2_other")
#spray
names(dta)[names(dta) %in%  c("grp1field2a50m", "grp2field4b50m", "grp3field8c50m", "grp4field10d50m", "grp5field12e50m")] <-  c("time_spray_pl1_sp1_other","time_spray_pl2_sp1_other","time_spray_pl3_sp1_other","time_spray_pl4_sp1_other","time_spray_pl5_sp1_other")

names(dta)[names(dta) %in%  c("spouse2grp_sp1field_sp2f50m", "spouse2grp_sp2field_sp4g50m", "spouse2grp_sp3field_sp6h50m", "spouse2group_sp4field_sp10j50m", "spouse2grp5_sp5field_sp12k50m")] <-  c("time_spray_pl1_sp2_other","time_spray_pl2_sp2_other","time_spray_pl3_sp2_other","time_spray_pl4_sp2_other","time_spray_pl5_sp2_other")
#harv
names(dta)[names(dta) %in% c("grp1field2a50n", "grp2field4b50n", "grp3field8c50n", "grp4field10d50n", "grp5field12e50n")] <-  c("time_harv_pl1_sp1_other","time_harv_pl2_sp1_other","time_harv_pl3_sp1_other","time_harv_pl4_sp1_other","time_harv_pl5_sp1_other")

names(dta)[names(dta) %in% c("spouse2grp_sp1field_sp2f50n", "spouse2grp_sp2field_sp4g50n", "spouse2grp_sp3field_sp6h50n", "spouse2group_sp4field_sp10j50n", "spouse2grp5_sp5field_sp12k50n")] <-  c("time_harv_pl1_sp2_other","time_harv_pl2_sp2_other","time_harv_pl3_sp2_other","time_harv_pl4_sp2_other","time_harv_pl5_sp2_other")


### replace 999 by NA's
for (plot in 1:5) {
for (act in c("prep","plant","weed1","weed2","weed3","spray","harv")) {
dta[paste(paste(paste("time",act, sep="_"),plot,sep="_pl"),"sp1_self",sep="_")] <- replace(dta[paste(paste(paste("time",act, sep="_"),plot,sep="_pl"),"sp1_self",sep="_")] , dta[paste(paste(paste("time",act, sep="_"),plot,sep="_pl"),"sp1_self",sep="_")] == 999, NA)

dta[paste(paste(paste("time",act, sep="_"),plot,sep="_pl"),"sp2_self",sep="_")] <- replace(dta[paste(paste(paste("time",act, sep="_"),plot,sep="_pl"),"sp2_self",sep="_")] , dta[paste(paste(paste("time",act, sep="_"),plot,sep="_pl"),"sp2_self",sep="_")] == 999, NA)

dta[paste(paste(paste("time",act, sep="_"),plot,sep="_pl"),"sp1_other",sep="_")] <- replace(dta[paste(paste(paste("time",act, sep="_"),plot,sep="_pl"),"sp1_other",sep="_")] , dta[paste(paste(paste("time",act, sep="_"),plot,sep="_pl"),"sp1_other",sep="_")] == 999, NA)

dta[paste(paste(paste("time",act, sep="_"),plot,sep="_pl"),"sp2_other",sep="_")] <- replace(dta[paste(paste(paste("time",act, sep="_"),plot,sep="_pl"),"sp2_other",sep="_")] , dta[paste(paste(paste("time",act, sep="_"),plot,sep="_pl"),"sp2_other",sep="_")] == 999, NA)
}
}


### aggregate - how do we get a hh level measure of time spent?
###to be consistent with the other production outcomes, we calculate 3 measures of hh level time use:
### total hh level time use as reported by man
### total hh level time use as reported by woman
### averages over these two

### hh level time spent on preparing 

### as reported by man
dta$time_prep_hh_man <- NA
dta$time_prep_hh_man[dta$person_interviewed=="man"] <- rowSums(dta[c("time_prep_pl1_sp1_self","time_prep_pl2_sp1_self","time_prep_pl3_sp1_self","time_prep_pl4_sp1_self","time_prep_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="man"] + rowSums(dta[c("time_prep_pl1_sp1_other","time_prep_pl2_sp1_other","time_prep_pl3_sp1_other","time_prep_pl4_sp1_other","time_prep_pl5_sp1_other")], na.rm=T)[dta$person_interviewed=="man"]

dta$time_prep_hh_man[dta$person_interviewed=="woman"] <- rowSums(dta[c("time_prep_pl1_sp2_self","time_prep_pl2_sp2_self","time_prep_pl3_sp2_self","time_prep_pl4_sp2_self","time_prep_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="woman"] + rowSums(dta[c("time_prep_pl1_sp2_other","time_prep_pl2_sp2_other","time_prep_pl3_sp2_other","time_prep_pl4_sp2_other","time_prep_pl5_sp2_other")], na.rm=T)[dta$person_interviewed=="woman"]

dta$time_prep_hh_man[(dta$person_interviewed=="man" & rowSums(is.na(dta[c("time_prep_pl1_sp1_self","time_prep_pl2_sp1_self","time_prep_pl3_sp1_self","time_prep_pl4_sp1_self","time_prep_pl5_sp1_self","time_prep_pl1_sp1_other","time_prep_pl2_sp1_other","time_prep_pl3_sp1_other","time_prep_pl4_sp1_other","time_prep_pl5_sp1_other")]))==10)
| (dta$person_interviewed=="woman" & rowSums(is.na(dta[c("time_prep_pl1_sp2_self","time_prep_pl2_sp2_self","time_prep_pl3_sp2_self","time_prep_pl4_sp2_self","time_prep_pl5_sp2_self","time_prep_pl1_sp2_other","time_prep_pl2_sp2_other","time_prep_pl3_sp2_other","time_prep_pl4_sp2_other","time_prep_pl5_sp2_other")]))==10)] <- NA

dta$time_prep_man_man <- NA
dta$time_prep_man_man[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_prep_pl1_sp1_self","time_prep_pl2_sp1_self","time_prep_pl3_sp1_self","time_prep_pl4_sp1_self","time_prep_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_prep_man_man[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_prep_pl1_sp1_self","time_prep_pl2_sp1_self","time_prep_pl3_sp1_self","time_prep_pl4_sp1_self","time_prep_pl5_sp1_self")]))==5)]  <- NA
dta$time_prep_man_man[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_prep_pl1_sp2_self","time_prep_pl2_sp2_self","time_prep_pl3_sp2_self","time_prep_pl4_sp2_self","time_prep_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_prep_man_man[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_prep_pl1_sp2_self","time_prep_pl2_sp2_self","time_prep_pl3_sp2_self","time_prep_pl4_sp2_self","time_prep_pl5_sp2_self")]))==5)]  <- NA

dta$time_prep_woman_woman  <- NA
dta$time_prep_woman_woman [dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_prep_pl1_sp1_self","time_prep_pl2_sp1_self","time_prep_pl3_sp1_self","time_prep_pl4_sp1_self","time_prep_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_prep_woman_woman [dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_prep_pl1_sp1_self","time_prep_pl2_sp1_self","time_prep_pl3_sp1_self","time_prep_pl4_sp1_self","time_prep_pl5_sp1_self")]))==5)]  <- NA
dta$time_prep_woman_woman [dta$person_interviewed=="man"]  <- rowSums(dta[c("time_prep_pl1_sp2_self","time_prep_pl2_sp2_self","time_prep_pl3_sp2_self","time_prep_pl4_sp2_self","time_prep_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_prep_woman_woman [dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_prep_pl1_sp2_self","time_prep_pl2_sp2_self","time_prep_pl3_sp2_self","time_prep_pl4_sp2_self","time_prep_pl5_sp2_self")]))==5)]  <- NA

dta$time_plant_hh_man <- NA
dta$time_plant_hh_man[dta$person_interviewed=="man"] <- rowSums(dta[c("time_plant_pl1_sp1_self","time_plant_pl2_sp1_self","time_plant_pl3_sp1_self","time_plant_pl4_sp1_self","time_plant_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="man"] + rowSums(dta[c("time_plant_pl1_sp1_other","time_plant_pl2_sp1_other","time_plant_pl3_sp1_other","time_plant_pl4_sp1_other","time_plant_pl5_sp1_other")], na.rm=T)[dta$person_interviewed=="man"]

dta$time_plant_hh_man[dta$person_interviewed=="woman"] <- rowSums(dta[c("time_plant_pl1_sp2_self","time_plant_pl2_sp2_self","time_plant_pl3_sp2_self","time_plant_pl4_sp2_self","time_plant_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="woman"] + rowSums(dta[c("time_plant_pl1_sp2_other","time_plant_pl2_sp2_other","time_plant_pl3_sp2_other","time_plant_pl4_sp2_other","time_plant_pl5_sp2_other")], na.rm=T)[dta$person_interviewed=="woman"]

dta$time_plant_hh_man[(dta$person_interviewed=="man" & rowSums(is.na(dta[c("time_plant_pl1_sp1_self","time_plant_pl2_sp1_self","time_plant_pl3_sp1_self","time_plant_pl4_sp1_self","time_plant_pl5_sp1_self","time_plant_pl1_sp1_other","time_plant_pl2_sp1_other","time_plant_pl3_sp1_other","time_plant_pl4_sp1_other","time_plant_pl5_sp1_other")]))==10)
| (dta$person_interviewed=="woman" & rowSums(is.na(dta[c("time_plant_pl1_sp2_self","time_plant_pl2_sp2_self","time_plant_pl3_sp2_self","time_plant_pl4_sp2_self","time_plant_pl5_sp2_self","time_plant_pl1_sp2_other","time_plant_pl2_sp2_other","time_plant_pl3_sp2_other","time_plant_pl4_sp2_other","time_plant_pl5_sp2_other")]))==10)] <- NA

dta$time_plant_man <- NA
dta$time_plant_man[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_plant_pl1_sp1_self","time_plant_pl2_sp1_self","time_plant_pl3_sp1_self","time_plant_pl4_sp1_self","time_plant_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_plant_man[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_plant_pl1_sp1_self","time_plant_pl2_sp1_self","time_plant_pl3_sp1_self","time_plant_pl4_sp1_self","time_plant_pl5_sp1_self")]))==5)]  <- NA
dta$time_plant_man[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_plant_pl1_sp2_self","time_plant_pl2_sp2_self","time_plant_pl3_sp2_self","time_plant_pl4_sp2_self","time_plant_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_plant_man[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_plant_pl1_sp2_self","time_plant_pl2_sp2_self","time_plant_pl3_sp2_self","time_plant_pl4_sp2_self","time_plant_pl5_sp2_self")]))==5)]  <- NA

dta$time_plant_woman <- NA
dta$time_plant_woman[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_plant_pl1_sp1_self","time_plant_pl2_sp1_self","time_plant_pl3_sp1_self","time_plant_pl4_sp1_self","time_plant_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_plant_woman[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_plant_pl1_sp1_self","time_plant_pl2_sp1_self","time_plant_pl3_sp1_self","time_plant_pl4_sp1_self","time_plant_pl5_sp1_self")]))==5)]  <- NA
dta$time_plant_woman[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_plant_pl1_sp2_self","time_plant_pl2_sp2_self","time_plant_pl3_sp2_self","time_plant_pl4_sp2_self","time_plant_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_plant_woman[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_plant_pl1_sp2_self","time_plant_pl2_sp2_self","time_plant_pl3_sp2_self","time_plant_pl4_sp2_self","time_plant_pl5_sp2_self")]))==5)]  <- NA


dta$time_weed_hh_man <- NA
dta$time_weed_hh_man[dta$person_interviewed=="man"] <- rowSums(dta[c("time_weed1_pl1_sp1_self","time_weed1_pl2_sp1_self","time_weed1_pl3_sp1_self","time_weed1_pl4_sp1_self","time_weed1_pl5_sp1_self","time_weed2_pl1_sp1_self","time_weed2_pl2_sp1_self","time_weed2_pl3_sp1_self","time_weed2_pl4_sp1_self","time_weed2_pl5_sp1_self","time_weed3_pl1_sp1_self","time_weed3_pl2_sp1_self","time_weed3_pl3_sp1_self","time_weed3_pl4_sp1_self","time_weed3_pl5_sp1_self"
)], na.rm=T)[dta$person_interviewed=="man"] + rowSums(dta[c("time_weed1_pl1_sp1_other","time_weed1_pl2_sp1_other","time_weed1_pl3_sp1_other","time_weed1_pl4_sp1_other","time_weed1_pl5_sp1_other","time_weed2_pl1_sp1_other","time_weed2_pl2_sp1_other","time_weed2_pl3_sp1_other","time_weed2_pl4_sp1_other","time_weed2_pl5_sp1_other","time_weed3_pl1_sp1_other","time_weed3_pl2_sp1_other","time_weed3_pl3_sp1_other","time_weed3_pl4_sp1_other","time_weed3_pl5_sp1_other"
)], na.rm=T)[dta$person_interviewed=="man"]

dta$time_weed_hh_man[dta$person_interviewed=="woman"] <- rowSums(dta[c("time_weed1_pl1_sp2_self","time_weed1_pl2_sp2_self","time_weed1_pl3_sp2_self","time_weed1_pl4_sp2_self","time_weed1_pl5_sp2_self","time_weed2_pl1_sp2_self","time_weed2_pl2_sp2_self","time_weed2_pl3_sp2_self","time_weed2_pl4_sp2_self","time_weed2_pl5_sp2_self","time_weed3_pl1_sp2_self","time_weed3_pl2_sp2_self","time_weed3_pl3_sp2_self","time_weed3_pl4_sp2_self","time_weed3_pl5_sp2_self"
)], na.rm=T)[dta$person_interviewed=="woman"] + rowSums(dta[c("time_weed1_pl1_sp2_other","time_weed1_pl2_sp2_other","time_weed1_pl3_sp2_other","time_weed1_pl4_sp2_other","time_weed1_pl5_sp2_other","time_weed2_pl1_sp2_other","time_weed2_pl2_sp2_other","time_weed2_pl3_sp2_other","time_weed2_pl4_sp2_other","time_weed2_pl5_sp2_other","time_weed3_pl1_sp2_other","time_weed3_pl2_sp2_other","time_weed3_pl3_sp2_other","time_weed3_pl4_sp2_other","time_weed3_pl5_sp2_other"
)], na.rm=T)[dta$person_interviewed=="woman"]

dta$time_weed_hh_man[(dta$person_interviewed=="man" & rowSums(is.na(dta[c("time_weed1_pl1_sp1_self","time_weed1_pl2_sp1_self","time_weed1_pl3_sp1_self","time_weed1_pl4_sp1_self","time_weed1_pl5_sp1_self","time_weed2_pl1_sp1_self","time_weed2_pl2_sp1_self","time_weed2_pl3_sp1_self","time_weed2_pl4_sp1_self","time_weed2_pl5_sp1_self","time_weed3_pl1_sp1_self","time_weed3_pl2_sp1_self","time_weed3_pl3_sp1_self","time_weed3_pl4_sp1_self","time_weed3_pl5_sp1_self","time_weed1_pl1_sp1_other","time_weed1_pl2_sp1_other","time_weed1_pl3_sp1_other","time_weed1_pl4_sp1_other","time_weed1_pl5_sp1_other","time_weed2_pl1_sp1_other","time_weed2_pl2_sp1_other","time_weed2_pl3_sp1_other","time_weed2_pl4_sp1_other","time_weed2_pl5_sp1_other","time_weed3_pl1_sp1_other","time_weed3_pl2_sp1_other","time_weed3_pl3_sp1_other","time_weed3_pl4_sp1_other","time_weed3_pl5_sp1_other"
)]))==30)
| (dta$person_interviewed=="woman" & rowSums(is.na(dta[c("time_weed1_pl1_sp2_self","time_weed1_pl2_sp2_self","time_weed1_pl3_sp2_self","time_weed1_pl4_sp2_self","time_weed1_pl5_sp2_self","time_weed2_pl1_sp2_self","time_weed2_pl2_sp2_self","time_weed2_pl3_sp2_self","time_weed2_pl4_sp2_self","time_weed2_pl5_sp2_self","time_weed3_pl1_sp2_self","time_weed3_pl2_sp2_self","time_weed3_pl3_sp2_self","time_weed3_pl4_sp2_self","time_weed3_pl5_sp2_self","time_weed1_pl1_sp2_other","time_weed1_pl2_sp2_other","time_weed1_pl3_sp2_other","time_weed1_pl4_sp2_other","time_weed1_pl5_sp2_other","time_weed2_pl1_sp2_other","time_weed2_pl2_sp2_other","time_weed2_pl3_sp2_other","time_weed2_pl4_sp2_other","time_weed2_pl5_sp2_other","time_weed3_pl1_sp2_other","time_weed3_pl2_sp2_other","time_weed3_pl3_sp2_other","time_weed3_pl4_sp2_other","time_weed3_pl5_sp2_other"
)]))==30)] <- NA

dta$time_weed_man <- NA
dta$time_weed_man[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_weed1_pl1_sp1_self","time_weed1_pl2_sp1_self","time_weed1_pl3_sp1_self","time_weed1_pl4_sp1_self","time_weed1_pl5_sp1_self","time_weed2_pl1_sp1_self","time_weed2_pl2_sp1_self","time_weed2_pl3_sp1_self","time_weed2_pl4_sp1_self","time_weed2_pl5_sp1_self","time_weed3_pl1_sp1_self","time_weed3_pl2_sp1_self","time_weed3_pl3_sp1_self","time_weed3_pl4_sp1_self","time_weed3_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_weed_man[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_weed1_pl1_sp1_self","time_weed1_pl2_sp1_self","time_weed1_pl3_sp1_self","time_weed1_pl4_sp1_self","time_weed1_pl5_sp1_self","time_weed2_pl1_sp1_self","time_weed2_pl2_sp1_self","time_weed2_pl3_sp1_self","time_weed2_pl4_sp1_self","time_weed2_pl5_sp1_self","time_weed3_pl1_sp1_self","time_weed3_pl2_sp1_self","time_weed3_pl3_sp1_self","time_weed3_pl4_sp1_self","time_weed3_pl5_sp1_self")]))==15)]  <- NA
dta$time_weed_man[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_weed1_pl1_sp2_self","time_weed1_pl2_sp2_self","time_weed1_pl3_sp2_self","time_weed1_pl4_sp2_self","time_weed1_pl5_sp2_self","time_weed2_pl1_sp2_self","time_weed2_pl2_sp2_self","time_weed2_pl3_sp2_self","time_weed2_pl4_sp2_self","time_weed2_pl5_sp2_self","time_weed3_pl1_sp2_self","time_weed3_pl2_sp2_self","time_weed3_pl3_sp2_self","time_weed3_pl4_sp2_self","time_weed3_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_weed_man[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_weed1_pl1_sp2_self","time_weed1_pl2_sp2_self","time_weed1_pl3_sp2_self","time_weed1_pl4_sp2_self","time_weed1_pl5_sp2_self","time_weed2_pl1_sp2_self","time_weed2_pl2_sp2_self","time_weed2_pl3_sp2_self","time_weed2_pl4_sp2_self","time_weed2_pl5_sp2_self","time_weed3_pl1_sp2_self","time_weed3_pl2_sp2_self","time_weed3_pl3_sp2_self","time_weed3_pl4_sp2_self","time_weed3_pl5_sp2_self")]))==15)]  <- NA

dta$time_weed_woman <- NA
dta$time_weed_woman[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_weed1_pl1_sp1_self","time_weed1_pl2_sp1_self","time_weed1_pl3_sp1_self","time_weed1_pl4_sp1_self","time_weed1_pl5_sp1_self","time_weed2_pl1_sp1_self","time_weed2_pl2_sp1_self","time_weed2_pl3_sp1_self","time_weed2_pl4_sp1_self","time_weed2_pl5_sp1_self","time_weed3_pl1_sp1_self","time_weed3_pl2_sp1_self","time_weed3_pl3_sp1_self","time_weed3_pl4_sp1_self","time_weed3_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_weed_woman[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_weed1_pl1_sp1_self","time_weed1_pl2_sp1_self","time_weed1_pl3_sp1_self","time_weed1_pl4_sp1_self","time_weed1_pl5_sp1_self","time_weed2_pl1_sp1_self","time_weed2_pl2_sp1_self","time_weed2_pl3_sp1_self","time_weed2_pl4_sp1_self","time_weed2_pl5_sp1_self","time_weed3_pl1_sp1_self","time_weed3_pl2_sp1_self","time_weed3_pl3_sp1_self","time_weed3_pl4_sp1_self","time_weed3_pl5_sp1_self")]))==15)]  <- NA
dta$time_weed_woman[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_weed1_pl1_sp2_self","time_weed1_pl2_sp2_self","time_weed1_pl3_sp2_self","time_weed1_pl4_sp2_self","time_weed1_pl5_sp2_self","time_weed2_pl1_sp2_self","time_weed2_pl2_sp2_self","time_weed2_pl3_sp2_self","time_weed2_pl4_sp2_self","time_weed2_pl5_sp2_self","time_weed3_pl1_sp2_self","time_weed3_pl2_sp2_self","time_weed3_pl3_sp2_self","time_weed3_pl4_sp2_self","time_weed3_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_weed_woman[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_weed1_pl1_sp2_self","time_weed1_pl2_sp2_self","time_weed1_pl3_sp2_self","time_weed1_pl4_sp2_self","time_weed1_pl5_sp2_self","time_weed2_pl1_sp2_self","time_weed2_pl2_sp2_self","time_weed2_pl3_sp2_self","time_weed2_pl4_sp2_self","time_weed2_pl5_sp2_self","time_weed3_pl1_sp2_self","time_weed3_pl2_sp2_self","time_weed3_pl3_sp2_self","time_weed3_pl4_sp2_self","time_weed3_pl5_sp2_self")]))==15)]  <- NA


dta$time_spray_hh_man <- NA
dta$time_spray_hh_man[dta$person_interviewed=="man"] <- rowSums(dta[c("time_spray_pl1_sp1_self","time_spray_pl2_sp1_self","time_spray_pl3_sp1_self","time_spray_pl4_sp1_self","time_spray_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="man"] + rowSums(dta[c("time_spray_pl1_sp1_other","time_spray_pl2_sp1_other","time_spray_pl3_sp1_other","time_spray_pl4_sp1_other","time_spray_pl5_sp1_other")], na.rm=T)[dta$person_interviewed=="man"]
dta$time_spray_hh_man[dta$person_interviewed=="woman"] <- rowSums(dta[c("time_spray_pl1_sp2_self","time_spray_pl2_sp2_self","time_spray_pl3_sp2_self","time_spray_pl4_sp2_self","time_spray_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="woman"] + rowSums(dta[c("time_spray_pl1_sp2_other","time_spray_pl2_sp2_other","time_spray_pl3_sp2_other","time_spray_pl4_sp2_other","time_spray_pl5_sp2_other")], na.rm=T)[dta$person_interviewed=="woman"]

dta$time_spray_hh_man[(dta$person_interviewed=="man" & rowSums(is.na(dta[c("time_spray_pl1_sp1_self","time_spray_pl2_sp1_self","time_spray_pl3_sp1_self","time_spray_pl4_sp1_self","time_spray_pl5_sp1_self","time_spray_pl1_sp1_other","time_spray_pl2_sp1_other","time_spray_pl3_sp1_other","time_spray_pl4_sp1_other","time_spray_pl5_sp1_other")]))==10)
| (dta$person_interviewed=="woman" & rowSums(is.na(dta[c("time_spray_pl1_sp2_self","time_spray_pl2_sp2_self","time_spray_pl3_sp2_self","time_spray_pl4_sp2_self","time_spray_pl5_sp2_self","time_spray_pl1_sp2_other","time_spray_pl2_sp2_other","time_spray_pl3_sp2_other","time_spray_pl4_sp2_other","time_spray_pl5_sp2_other")]))==10)] <- NA


dta$time_spray_man <- NA
dta$time_spray_man[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_spray_pl1_sp1_self","time_spray_pl2_sp1_self","time_spray_pl3_sp1_self","time_spray_pl4_sp1_self","time_spray_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_spray_man[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_spray_pl1_sp1_self","time_spray_pl2_sp1_self","time_spray_pl3_sp1_self","time_spray_pl4_sp1_self","time_spray_pl5_sp1_self")]))==5)]  <- NA
dta$time_spray_man[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_spray_pl1_sp2_self","time_spray_pl2_sp2_self","time_spray_pl3_sp2_self","time_spray_pl4_sp2_self","time_spray_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_spray_man[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_spray_pl1_sp2_self","time_spray_pl2_sp2_self","time_spray_pl3_sp2_self","time_spray_pl4_sp2_self","time_spray_pl5_sp2_self")]))==5)]  <- NA

dta$time_spray_woman <- NA
dta$time_spray_woman[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_spray_pl1_sp1_self","time_spray_pl2_sp1_self","time_spray_pl3_sp1_self","time_spray_pl4_sp1_self","time_spray_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_spray_woman[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_spray_pl1_sp1_self","time_spray_pl2_sp1_self","time_spray_pl3_sp1_self","time_spray_pl4_sp1_self","time_spray_pl5_sp1_self")]))==5)]  <- NA
dta$time_spray_woman[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_spray_pl1_sp2_self","time_spray_pl2_sp2_self","time_spray_pl3_sp2_self","time_spray_pl4_sp2_self","time_spray_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_spray_woman[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_spray_pl1_sp2_self","time_spray_pl2_sp2_self","time_spray_pl3_sp2_self","time_spray_pl4_sp2_self","time_spray_pl5_sp2_self")]))==5)]  <- NA


dta$time_harv_hh_man <- NA
dta$time_harv_hh_man[dta$person_interviewed=="man"] <- rowSums(dta[c("time_harv_pl1_sp1_self","time_harv_pl2_sp1_self","time_harv_pl3_sp1_self","time_harv_pl4_sp1_self","time_harv_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="man"] + rowSums(dta[c("time_harv_pl1_sp1_other","time_harv_pl2_sp1_other","time_harv_pl3_sp1_other","time_harv_pl4_sp1_other","time_harv_pl5_sp1_other")], na.rm=T)[dta$person_interviewed=="man"]
dta$time_harv_hh_man[dta$person_interviewed=="woman"] <- rowSums(dta[c("time_harv_pl1_sp2_self","time_harv_pl2_sp2_self","time_harv_pl3_sp2_self","time_harv_pl4_sp2_self","time_harv_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="woman"] + rowSums(dta[c("time_harv_pl1_sp2_other","time_harv_pl2_sp2_other","time_harv_pl3_sp2_other","time_harv_pl4_sp2_other","time_harv_pl5_sp2_other")], na.rm=T)[dta$person_interviewed=="woman"]

dta$time_harv_hh_man[(dta$person_interviewed=="man" & rowSums(is.na(dta[c("time_harv_pl1_sp1_self","time_harv_pl2_sp1_self","time_harv_pl3_sp1_self","time_harv_pl4_sp1_self","time_harv_pl5_sp1_self","time_harv_pl1_sp1_other","time_harv_pl2_sp1_other","time_harv_pl3_sp1_other","time_harv_pl4_sp1_other","time_harv_pl5_sp1_other")]))==10)
| (dta$person_interviewed=="woman" & rowSums(is.na(dta[c("time_harv_pl1_sp2_self","time_harv_pl2_sp2_self","time_harv_pl3_sp2_self","time_harv_pl4_sp2_self","time_harv_pl5_sp2_self","time_harv_pl1_sp2_other","time_harv_pl2_sp2_other","time_harv_pl3_sp2_other","time_harv_pl4_sp2_other","time_harv_pl5_sp2_other")]))==10)] <- NA


dta$time_harv_man <- NA
dta$time_harv_man[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_harv_pl1_sp1_self","time_harv_pl2_sp1_self","time_harv_pl3_sp1_self","time_harv_pl4_sp1_self","time_harv_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_harv_man[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_harv_pl1_sp1_self","time_harv_pl2_sp1_self","time_harv_pl3_sp1_self","time_harv_pl4_sp1_self","time_harv_pl5_sp1_self")]))==5)]  <- NA
dta$time_harv_man[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_harv_pl1_sp2_self","time_harv_pl2_sp2_self","time_harv_pl3_sp2_self","time_harv_pl4_sp2_self","time_harv_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_harv_man[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_harv_pl1_sp2_self","time_harv_pl2_sp2_self","time_harv_pl3_sp2_self","time_harv_pl4_sp2_self","time_harv_pl5_sp2_self")]))==5)]  <- NA

dta$time_harv_woman <- NA
dta$time_harv_woman[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_harv_pl1_sp1_self","time_harv_pl2_sp1_self","time_harv_pl3_sp1_self","time_harv_pl4_sp1_self","time_harv_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_harv_woman[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_harv_pl1_sp1_self","time_harv_pl2_sp1_self","time_harv_pl3_sp1_self","time_harv_pl4_sp1_self","time_harv_pl5_sp1_self")]))==5)]  <- NA
dta$time_harv_woman[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_harv_pl1_sp2_self","time_harv_pl2_sp2_self","time_harv_pl3_sp2_self","time_harv_pl4_sp2_self","time_harv_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_harv_woman[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_harv_pl1_sp2_self","time_harv_pl2_sp2_self","time_harv_pl3_sp2_self","time_harv_pl4_sp2_self","time_harv_pl5_sp2_self")]))==5)]  <- NA


############################## now for woman


dta$time_prep_hh_woman <- NA
dta$time_prep_hh_woman[dta$person_interviewed=="woman"] <- rowSums(dta[c("time_prep_pl1_sp1_self","time_prep_pl2_sp1_self","time_prep_pl3_sp1_self","time_prep_pl4_sp1_self","time_prep_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="woman"] + rowSums(dta[c("time_prep_pl1_sp1_other","time_prep_pl2_sp1_other","time_prep_pl3_sp1_other","time_prep_pl4_sp1_other","time_prep_pl5_sp1_other")], na.rm=T)[dta$person_interviewed=="woman"]
dta$time_prep_hh_woman[dta$person_interviewed=="man"] <- rowSums(dta[c("time_prep_pl1_sp2_self","time_prep_pl2_sp2_self","time_prep_pl3_sp2_self","time_prep_pl4_sp2_self","time_prep_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="man"] + rowSums(dta[c("time_prep_pl1_sp2_other","time_prep_pl2_sp2_other","time_prep_pl3_sp2_other","time_prep_pl4_sp2_other","time_prep_pl5_sp2_other")], na.rm=T)[dta$person_interviewed=="man"]

dta$time_prep_hh_woman[(dta$person_interviewed=="woman" & rowSums(is.na(dta[c("time_prep_pl1_sp1_self","time_prep_pl2_sp1_self","time_prep_pl3_sp1_self","time_prep_pl4_sp1_self","time_prep_pl5_sp1_self","time_prep_pl1_sp1_other","time_prep_pl2_sp1_other","time_prep_pl3_sp1_other","time_prep_pl4_sp1_other","time_prep_pl5_sp1_other")]))==10)
| (dta$person_interviewed=="man" & rowSums(is.na(dta[c("time_prep_pl1_sp2_self","time_prep_pl2_sp2_self","time_prep_pl3_sp2_self","time_prep_pl4_sp2_self","time_prep_pl5_sp2_self","time_prep_pl1_sp2_other","time_prep_pl2_sp2_other","time_prep_pl3_sp2_other","time_prep_pl4_sp2_other","time_prep_pl5_sp2_other")]))==10)] <- NA


dta$time_plant_hh_woman <- NA
dta$time_plant_hh_woman[dta$person_interviewed=="woman"] <- rowSums(dta[c("time_plant_pl1_sp1_self","time_plant_pl2_sp1_self","time_plant_pl3_sp1_self","time_plant_pl4_sp1_self","time_plant_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="woman"] + rowSums(dta[c("time_plant_pl1_sp1_other","time_plant_pl2_sp1_other","time_plant_pl3_sp1_other","time_plant_pl4_sp1_other","time_plant_pl5_sp1_other")], na.rm=T)[dta$person_interviewed=="woman"]
dta$time_plant_hh_woman[dta$person_interviewed=="man"] <- rowSums(dta[c("time_plant_pl1_sp2_self","time_plant_pl2_sp2_self","time_plant_pl3_sp2_self","time_plant_pl4_sp2_self","time_plant_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="man"] + rowSums(dta[c("time_plant_pl1_sp2_other","time_plant_pl2_sp2_other","time_plant_pl3_sp2_other","time_plant_pl4_sp2_other","time_plant_pl5_sp2_other")], na.rm=T)[dta$person_interviewed=="man"]

dta$time_plant_hh_woman[(dta$person_interviewed=="woman" & rowSums(is.na(dta[c("time_plant_pl1_sp1_self","time_plant_pl2_sp1_self","time_plant_pl3_sp1_self","time_plant_pl4_sp1_self","time_plant_pl5_sp1_self","time_plant_pl1_sp1_other","time_plant_pl2_sp1_other","time_plant_pl3_sp1_other","time_plant_pl4_sp1_other","time_plant_pl5_sp1_other")]))==10)
| (dta$person_interviewed=="man" & rowSums(is.na(dta[c("time_plant_pl1_sp2_self","time_plant_pl2_sp2_self","time_plant_pl3_sp2_self","time_plant_pl4_sp2_self","time_plant_pl5_sp2_self","time_plant_pl1_sp2_other","time_plant_pl2_sp2_other","time_plant_pl3_sp2_other","time_plant_pl4_sp2_other","time_plant_pl5_sp2_other")]))==10)] <- NA

dta$time_weed_hh_woman <- NA
dta$time_weed_hh_woman[dta$person_interviewed=="woman"] <- rowSums(dta[c("time_weed1_pl1_sp1_self","time_weed1_pl2_sp1_self","time_weed1_pl3_sp1_self","time_weed1_pl4_sp1_self","time_weed1_pl5_sp1_self","time_weed2_pl1_sp1_self","time_weed2_pl2_sp1_self","time_weed2_pl3_sp1_self","time_weed2_pl4_sp1_self","time_weed2_pl5_sp1_self","time_weed3_pl1_sp1_self","time_weed3_pl2_sp1_self","time_weed3_pl3_sp1_self","time_weed3_pl4_sp1_self","time_weed3_pl5_sp1_self"
)], na.rm=T)[dta$person_interviewed=="woman"] + rowSums(dta[c("time_weed1_pl1_sp1_other","time_weed1_pl2_sp1_other","time_weed1_pl3_sp1_other","time_weed1_pl4_sp1_other","time_weed1_pl5_sp1_other","time_weed2_pl1_sp1_other","time_weed2_pl2_sp1_other","time_weed2_pl3_sp1_other","time_weed2_pl4_sp1_other","time_weed2_pl5_sp1_other","time_weed3_pl1_sp1_other","time_weed3_pl2_sp1_other","time_weed3_pl3_sp1_other","time_weed3_pl4_sp1_other","time_weed3_pl5_sp1_other"
)], na.rm=T)[dta$person_interviewed=="woman"]
dta$time_weed_hh_woman[dta$person_interviewed=="man"] <- rowSums(dta[c("time_weed1_pl1_sp2_self","time_weed1_pl2_sp2_self","time_weed1_pl3_sp2_self","time_weed1_pl4_sp2_self","time_weed1_pl5_sp2_self","time_weed2_pl1_sp2_self","time_weed2_pl2_sp2_self","time_weed2_pl3_sp2_self","time_weed2_pl4_sp2_self","time_weed2_pl5_sp2_self","time_weed3_pl1_sp2_self","time_weed3_pl2_sp2_self","time_weed3_pl3_sp2_self","time_weed3_pl4_sp2_self","time_weed3_pl5_sp2_self"
)], na.rm=T)[dta$person_interviewed=="man"] + rowSums(dta[c("time_weed1_pl1_sp2_other","time_weed1_pl2_sp2_other","time_weed1_pl3_sp2_other","time_weed1_pl4_sp2_other","time_weed1_pl5_sp2_other","time_weed2_pl1_sp2_other","time_weed2_pl2_sp2_other","time_weed2_pl3_sp2_other","time_weed2_pl4_sp2_other","time_weed2_pl5_sp2_other","time_weed3_pl1_sp2_other","time_weed3_pl2_sp2_other","time_weed3_pl3_sp2_other","time_weed3_pl4_sp2_other","time_weed3_pl5_sp2_other"
)], na.rm=T)[dta$person_interviewed=="man"]

dta$time_weed_hh_woman[(dta$person_interviewed=="woman" & rowSums(is.na(dta[c("time_weed1_pl1_sp1_self","time_weed1_pl2_sp1_self","time_weed1_pl3_sp1_self","time_weed1_pl4_sp1_self","time_weed1_pl5_sp1_self","time_weed2_pl1_sp1_self","time_weed2_pl2_sp1_self","time_weed2_pl3_sp1_self","time_weed2_pl4_sp1_self","time_weed2_pl5_sp1_self","time_weed3_pl1_sp1_self","time_weed3_pl2_sp1_self","time_weed3_pl3_sp1_self","time_weed3_pl4_sp1_self","time_weed3_pl5_sp1_self","time_weed1_pl1_sp1_other","time_weed1_pl2_sp1_other","time_weed1_pl3_sp1_other","time_weed1_pl4_sp1_other","time_weed1_pl5_sp1_other","time_weed2_pl1_sp1_other","time_weed2_pl2_sp1_other","time_weed2_pl3_sp1_other","time_weed2_pl4_sp1_other","time_weed2_pl5_sp1_other","time_weed3_pl1_sp1_other","time_weed3_pl2_sp1_other","time_weed3_pl3_sp1_other","time_weed3_pl4_sp1_other","time_weed3_pl5_sp1_other"
)]))==30)
| (dta$person_interviewed=="man" & rowSums(is.na(dta[c("time_weed1_pl1_sp2_self","time_weed1_pl2_sp2_self","time_weed1_pl3_sp2_self","time_weed1_pl4_sp2_self","time_weed1_pl5_sp2_self","time_weed2_pl1_sp2_self","time_weed2_pl2_sp2_self","time_weed2_pl3_sp2_self","time_weed2_pl4_sp2_self","time_weed2_pl5_sp2_self","time_weed3_pl1_sp2_self","time_weed3_pl2_sp2_self","time_weed3_pl3_sp2_self","time_weed3_pl4_sp2_self","time_weed3_pl5_sp2_self","time_weed1_pl1_sp2_other","time_weed1_pl2_sp2_other","time_weed1_pl3_sp2_other","time_weed1_pl4_sp2_other","time_weed1_pl5_sp2_other","time_weed2_pl1_sp2_other","time_weed2_pl2_sp2_other","time_weed2_pl3_sp2_other","time_weed2_pl4_sp2_other","time_weed2_pl5_sp2_other","time_weed3_pl1_sp2_other","time_weed3_pl2_sp2_other","time_weed3_pl3_sp2_other","time_weed3_pl4_sp2_other","time_weed3_pl5_sp2_other"
)]))==30)] <- NA


dta$time_spray_hh_woman <- NA
dta$time_spray_hh_woman[dta$person_interviewed=="woman"] <- rowSums(dta[c("time_spray_pl1_sp1_self","time_spray_pl2_sp1_self","time_spray_pl3_sp1_self","time_spray_pl4_sp1_self","time_spray_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="woman"] + rowSums(dta[c("time_spray_pl1_sp1_other","time_spray_pl2_sp1_other","time_spray_pl3_sp1_other","time_spray_pl4_sp1_other","time_spray_pl5_sp1_other")], na.rm=T)[dta$person_interviewed=="woman"]
dta$time_spray_hh_woman[dta$person_interviewed=="man"] <- rowSums(dta[c("time_spray_pl1_sp2_self","time_spray_pl2_sp2_self","time_spray_pl3_sp2_self","time_spray_pl4_sp2_self","time_spray_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="man"] + rowSums(dta[c("time_spray_pl1_sp2_other","time_spray_pl2_sp2_other","time_spray_pl3_sp2_other","time_spray_pl4_sp2_other","time_spray_pl5_sp2_other")], na.rm=T)[dta$person_interviewed=="man"]

dta$time_spray_hh_woman[(dta$person_interviewed=="woman" & rowSums(is.na(dta[c("time_spray_pl1_sp1_self","time_spray_pl2_sp1_self","time_spray_pl3_sp1_self","time_spray_pl4_sp1_self","time_spray_pl5_sp1_self","time_spray_pl1_sp1_other","time_spray_pl2_sp1_other","time_spray_pl3_sp1_other","time_spray_pl4_sp1_other","time_spray_pl5_sp1_other")]))==10)
| (dta$person_interviewed=="man" & rowSums(is.na(dta[c("time_spray_pl1_sp2_self","time_spray_pl2_sp2_self","time_spray_pl3_sp2_self","time_spray_pl4_sp2_self","time_spray_pl5_sp2_self","time_spray_pl1_sp2_other","time_spray_pl2_sp2_other","time_spray_pl3_sp2_other","time_spray_pl4_sp2_other","time_spray_pl5_sp2_other")]))==10)] <- NA

dta$time_harv_hh_woman <- NA
dta$time_harv_hh_woman[dta$person_interviewed=="woman"] <- rowSums(dta[c("time_harv_pl1_sp1_self","time_harv_pl2_sp1_self","time_harv_pl3_sp1_self","time_harv_pl4_sp1_self","time_harv_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="woman"] + rowSums(dta[c("time_harv_pl1_sp1_other","time_harv_pl2_sp1_other","time_harv_pl3_sp1_other","time_harv_pl4_sp1_other","time_harv_pl5_sp1_other")], na.rm=T)[dta$person_interviewed=="woman"]
dta$time_harv_hh_woman[dta$person_interviewed=="man"] <- rowSums(dta[c("time_harv_pl1_sp2_self","time_harv_pl2_sp2_self","time_harv_pl3_sp2_self","time_harv_pl4_sp2_self","time_harv_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="man"] + rowSums(dta[c("time_harv_pl1_sp2_other","time_harv_pl2_sp2_other","time_harv_pl3_sp2_other","time_harv_pl4_sp2_other","time_harv_pl5_sp2_other")], na.rm=T)[dta$person_interviewed=="man"]

dta$time_harv_hh_woman[(dta$person_interviewed=="woman" & rowSums(is.na(dta[c("time_harv_pl1_sp1_self","time_harv_pl2_sp1_self","time_harv_pl3_sp1_self","time_harv_pl4_sp1_self","time_harv_pl5_sp1_self","time_harv_pl1_sp1_other","time_harv_pl2_sp1_other","time_harv_pl3_sp1_other","time_harv_pl4_sp1_other","time_harv_pl5_sp1_other")]))==10)
| (dta$person_interviewed=="man" & rowSums(is.na(dta[c("time_harv_pl1_sp2_self","time_harv_pl2_sp2_self","time_harv_pl3_sp2_self","time_harv_pl4_sp2_self","time_harv_pl5_sp2_self","time_harv_pl1_sp2_other","time_harv_pl2_sp2_other","time_harv_pl3_sp2_other","time_harv_pl4_sp2_other","time_harv_pl5_sp2_other")]))==10)] <- NA

### take mean
dta$time_prep_hh_mean <- rowMeans(dta[c("time_prep_hh_man","time_prep_hh_woman")], na.rm=T)
dta$time_plant_hh_mean <- rowMeans(dta[c("time_plant_hh_man","time_plant_hh_woman")], na.rm=T)
dta$time_weed_hh_mean <- rowMeans(dta[c("time_weed_hh_man","time_weed_hh_woman")], na.rm=T)
dta$time_spray_hh_mean <- rowMeans(dta[c("time_spray_hh_man","time_spray_hh_woman")], na.rm=T)
dta$time_harv_hh_mean <- rowMeans(dta[c("time_harv_hh_man","time_harv_hh_woman")], na.rm=T)


## hired in labour
dta[c("grp1a152", "grp2b152", "grp3c152", "grp4d152", "grp5e152")] <- lapply(dta[c("grp1a152", "grp2b152", "grp3c152", "grp4d152", "grp5e152")], function(x) replace(x, x == 999, NA) )
dta[c("spouse2grp_sp1f152", "spouse2grp_sp2g152", "spouse2grp_sp3h152", "spouse2group_sp4j152", "spouse2grp5_sp5k152")] <- lapply(dta[c("spouse2grp_sp1f152", "spouse2grp_sp2g152", "spouse2grp_sp3h152", "spouse2group_sp4j152", "spouse2grp5_sp5k152")], function(x) replace(x, x == 999, NA) )

dta$time_hired_sp1 <- rowSums(dta[c("grp1a152", "grp2b152", "grp3c152", "grp4d152", "grp5e152")],na.rm=T)
dta$time_hired_sp1[is.na(dta$grp1a152) & is.na(dta$grp2b152) & is.na(dta$grp3c152) & is.na(dta$grp4d152) & is.na(dta$grp5e152)] <- NA
dta$time_hired_sp2 <- rowSums(dta[c("spouse2grp_sp1f152", "spouse2grp_sp2g152", "spouse2grp_sp3h152", "spouse2group_sp4j152", "spouse2grp5_sp5k152")], na.rm=T)
dta$time_hired_sp2[is.na(dta$spouse2grp_sp1f152) & is.na(dta$spouse2grp_sp2g152) & is.na(dta$spouse2grp_sp3h152) & is.na(dta$spouse2group_sp4j152) & is.na(dta$spouse2grp5_sp5k152)] <- NA

dta$time_hired_hh_mean <- rowMeans(dta[c("time_hired_sp1","time_hired_sp2")], na.rm=T)
dta$time_hired_hh_mean[is.na(dta$time_hired_sp1) & is.na(dta$time_hired_sp2)] <- NA
dta$time_hired_hh_man <- NA
dta$time_hired_hh_man[dta$person_interviewed == "man"] <- dta$time_hired_sp1[dta$person_interviewed == "man"]
dta$time_hired_hh_man[dta$person_interviewed == "woman"] <- dta$time_hired_sp2[dta$person_interviewed == "woman"]
dta$time_hired_hh_woman <- NA
dta$time_hired_hh_woman[dta$person_interviewed == "woman"] <- dta$time_hired_sp1[dta$person_interviewed == "woman"]
dta$time_hired_hh_woman[dta$person_interviewed == "man"] <- dta$time_hired_sp2[dta$person_interviewed == "man"]

dta$tot_time_hh_man <- rowSums(dta[c("time_prep_hh_man","time_plant_hh_man","time_weed_hh_man","time_spray_hh_man","time_harv_hh_man","time_hired_hh_man")], na.rm=T)
dta$tot_time_hh_man[rowSums(is.na(dta[c("time_prep_hh_man","time_plant_hh_man","time_weed_hh_man","time_spray_hh_man","time_harv_hh_man","time_hired_hh_man")]))==6] <- NA
dta$tot_time_hh_woman <- rowSums(dta[c("time_prep_hh_woman","time_plant_hh_woman","time_weed_hh_woman","time_spray_hh_woman","time_harv_hh_woman","time_hired_hh_woman")], na.rm=T)
dta$tot_time_hh_woman[rowSums(is.na(dta[c("time_prep_hh_woman","time_plant_hh_woman","time_weed_hh_woman","time_spray_hh_woman","time_harv_hh_woman","time_hired_hh_woman")]))==6] <- NA
dta$tot_time_hh_mean <- rowSums(dta[c("time_prep_hh_mean","time_plant_hh_mean","time_weed_hh_mean","time_spray_hh_mean","time_harv_hh_mean","time_hired_hh_mean")], na.rm=T)
dta$tot_time_hh_mean[rowSums(is.na(dta[c("time_prep_hh_mean","time_plant_hh_mean","time_weed_hh_mean","time_spray_hh_mean","time_harv_hh_mean","time_hired_hh_mean")]))==6] <- NA

dta$tot_time_man <- rowSums(dta[c("time_prep_man","time_plant_man","time_weed_man","time_spray_man","time_harv_man")], na.rm=T)
dta$tot_time_man[rowSums(is.na(dta[c("time_prep_man","time_plant_man","time_weed_man","time_spray_man","time_harv_man")]))==5] <- NA

dta$tot_time_woman <- rowSums(dta[c("time_prep_woman","time_plant_woman","time_weed_woman","time_spray_woman","time_harv_woman")], na.rm=T)
dta$tot_time_woman[rowSums(is.na(dta[c("time_prep_woman","time_plant_woman","time_weed_woman","time_spray_woman","time_harv_woman")]))==5] <- NA


#################################### interlude: deciscion making at plot level ##########################
###who manages plots?
dta$mgt_man_pl1 <-  rowSums(cbind((dta$grp1a10==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f10==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$mgt_man_pl1[is.na( (dta$grp1a10==1 ) ) & is.na((dta$spouse2grp_sp1f10==1 ) )] <- NA
dta$mgt_both_man_pl1 <-  rowSums(cbind((dta$grp1a10==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f10==3  & dta$person_interviewed=="woman")), na.rm=T)
dta$mgt_both_man_pl1[is.na( (dta$grp1a10==1 ) ) & is.na((dta$spouse2grp_sp1f10==1 ) )] <- NA
dta$mgt_woman_man_pl1 <-  rowSums(cbind((dta$grp1a10==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f10==2  & dta$person_interviewed=="woman")), na.rm=T)
dta$mgt_woman_man_pl1[is.na( (dta$grp1a10==1 ) ) & is.na((dta$spouse2grp_sp1f10==1 ) )] <- NA

dta$mgt_woman_pl1 <-  rowSums(cbind((dta$grp1a10==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1f10==1  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_woman_pl1[is.na((dta$grp1a10==1 ) ) & is.na((dta$spouse2grp_sp1f10==1 ))] <- NA
dta$mgt_both_woman_pl1 <-  rowSums(cbind((dta$grp1a10==3  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1f10==3  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_both_woman_pl1[is.na((dta$grp1a10==1 ) ) & is.na((dta$spouse2grp_sp1f10==1 ))] <- NA
dta$mgt_man_woman_pl1 <-  rowSums(cbind((dta$grp1a10==2  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1f10==2  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_man_woman_pl1[is.na((dta$grp1a10==1 ) ) & is.na((dta$spouse2grp_sp1f10==1 ))] <- NA
dta$mgt_woman_involved_pl1 <- rowSums(cbind((dta$grp1a10%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1f10%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_woman_involved_pl1[is.na((dta$grp1a10==1 ) ) & is.na((dta$spouse2grp_sp1f10==1  & dta$person_interviewed=="man"))] <- NA

dta$mgt_man_involved_pl1 <- rowSums(cbind((dta$grp1a10%in%c(1,3,5)  & dta$person_interviewed=="man") ,  (dta$spouse2grp_sp1f10%in%c(1,3,5)  & dta$person_interviewed=="woman")), na.rm=T)
dta$mgt_man_involved_pl1[is.na((dta$grp1a10==1 ) ) & is.na((dta$spouse2grp_sp1f10==1  & dta$person_interviewed=="woman"))] <- NA
##woman says it is both and man says it is both
dta$mgt_both_pl1 <- rowSums(cbind( (rowSums(cbind((dta$grp1a10==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp1f10==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp1a10==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f10==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$mgt_both_pl1[is.na((dta$grp1a10==3 )) & is.na((dta$spouse2grp_sp1f10==3 ))] <- NA

##woman says it is man and man says it is man
dta$mgt_man_both_pl1 <- rowSums(cbind( (rowSums(cbind((dta$grp1a10==2  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp1f10==1  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp1a10==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f10==2  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$mgt_man_both_pl1[is.na((dta$grp1a10==3 )) & is.na((dta$spouse2grp_sp1f10==3 ))] <- NA

dta$mgt_woman_both_pl1 <- rowSums(cbind( (rowSums(cbind((dta$grp1a10==1  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp1f10==2  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp1a10==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f10==1  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$mgt_woman_both_pl1[is.na((dta$grp1a10==3 )) & is.na((dta$spouse2grp_sp1f10==3 ))] <- NA


dta$mgt_man_pl2 <-  rowSums(cbind((dta$grp2b10==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g10==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$mgt_man_pl2[is.na( (dta$grp2b10==1 ) ) & is.na((dta$spouse2grp_sp2g10==1 ) )] <- NA
dta$mgt_both_man_pl2 <-  rowSums(cbind((dta$grp2b10==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g10==3  & dta$person_interviewed=="woman")), na.rm=T)
dta$mgt_both_man_pl2[is.na( (dta$grp2b10==1 ) ) & is.na((dta$spouse2grp_sp2g10==1 ) )] <- NA
dta$mgt_woman_man_pl2 <-  rowSums(cbind((dta$grp2b10==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g10==2  & dta$person_interviewed=="woman")), na.rm=T)
dta$mgt_woman_man_pl2[is.na( (dta$grp2b10==1 ) ) & is.na((dta$spouse2grp_sp2g10==1 ) )] <- NA
dta$mgt_woman_pl2 <-  rowSums(cbind((dta$grp2b10==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2g10==1  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_woman_pl2[is.na((dta$grp2b10==1 ) ) & is.na((dta$spouse2grp_sp2g10==1 ))] <- NA
dta$mgt_both_woman_pl2 <-  rowSums(cbind((dta$grp2b10==3  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2g10==3  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_both_woman_pl2[is.na((dta$grp2b10==1 ) ) & is.na((dta$spouse2grp_sp2g10==1 ))] <- NA
dta$mgt_man_woman_pl2 <-  rowSums(cbind((dta$grp2b10==2  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2g10==2  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_man_woman_pl2[is.na((dta$grp2b10==1 ) ) & is.na((dta$spouse2grp_sp2g10==1 ))] <- NA

dta$mgt_woman_involved_pl2 <-  rowSums(cbind((dta$grp2b10%in%c(1,3,5)   & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2g10%in%c(1,3,5)   & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_woman_involved_pl2[is.na((dta$grp2b10==1 ) ) & is.na((dta$spouse2grp_sp2g10==1 ))] <- NA
dta$mgt_man_involved_pl2 <-  rowSums(cbind((dta$grp2b10%in%c(1,3,5)   & dta$person_interviewed=="man") ,  (dta$spouse2grp_sp2g10%in%c(1,3,5)   & dta$person_interviewed=="woman")), na.rm=T)
dta$mgt_man_involved_pl2[is.na((dta$grp2b10==1 ) ) & is.na((dta$spouse2grp_sp2g10==1 ))] <- NA
##woman says it is both and man says it is both
dta$mgt_both_pl2 <- rowSums(cbind( (rowSums(cbind((dta$grp2b10==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp2g10==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp2b10==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g10==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$mgt_both_pl2[is.na((dta$grp2b10==3 )) & is.na((dta$spouse2grp_sp2g10==3 ))] <- NA

##woman says it is man and man says it is man
dta$mgt_man_both_pl2 <- rowSums(cbind( (rowSums(cbind((dta$grp2b10==2  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp2g10==1  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp2b10==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g10==2  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$mgt_man_both_pl2[is.na((dta$grp2b10==3 )) & is.na((dta$spouse2grp_sp2g10==3 ))] <- NA
dta$mgt_woman_both_pl2 <- rowSums(cbind( (rowSums(cbind((dta$grp2b10==1  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp2g10==2  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp2b10==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g10==1  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$mgt_woman_both_pl2[is.na((dta$grp2b10==3 )) & is.na((dta$spouse2grp_sp2g10==3 ))] <- NA


dta$mgt_man_pl3 <-  rowSums(cbind((dta$grp3c10==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h10==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$mgt_man_pl3[is.na( (dta$grp3c10==1 ) ) & is.na((dta$spouse2grp_sp3h10==1 ) )] <- NA
dta$mgt_both_man_pl3 <-  rowSums(cbind((dta$grp3c10==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h10==3  & dta$person_interviewed=="woman")), na.rm=T)
dta$mgt_both_man_pl3[is.na( (dta$grp3c10==1 ) ) & is.na((dta$spouse2grp_sp3h10==1 ) )] <- NA
dta$mgt_woman_man_pl3 <-  rowSums(cbind((dta$grp3c10==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h10==2  & dta$person_interviewed=="woman")), na.rm=T)
dta$mgt_woman_man_pl3[is.na( (dta$grp3c10==1 ) ) & is.na((dta$spouse2grp_sp3h10==1 ) )] <- NA

dta$mgt_woman_pl3 <-  rowSums(cbind((dta$grp3c10==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3h10==1  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_woman_pl3[is.na((dta$grp3c10==1 ) ) & is.na((dta$spouse2grp_sp3h10==1 ))] <- NA
dta$mgt_both_woman_pl3 <-  rowSums(cbind((dta$grp3c10==3  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3h10==3  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_both_woman_pl3[is.na((dta$grp3c10==1 ) ) & is.na((dta$spouse2grp_sp3h10==1 ))] <- NA
dta$mgt_man_woman_pl3 <-  rowSums(cbind((dta$grp3c10==2  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3h10==2  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_man_woman_pl3[is.na((dta$grp3c10==1 ) ) & is.na((dta$spouse2grp_sp3h10==1 ))] <- NA
dta$mgt_woman_involved_pl3 <-  rowSums(cbind((dta$grp3c10%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3h10%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_woman_involved_pl3[is.na((dta$grp3c10==1 ) ) & is.na((dta$spouse2grp_sp3h10==1 ))] <- NA
dta$mgt_man_involved_pl3 <-  rowSums(cbind((dta$grp3c10%in%c(1,3,5)  & dta$person_interviewed=="man") ,  (dta$spouse2grp_sp3h10%in%c(1,3,5)  & dta$person_interviewed=="woman")), na.rm=T)
dta$mgt_man_involved_pl3[is.na((dta$grp3c10==1 ) ) & is.na((dta$spouse2grp_sp3h10==1 ))] <- NA
##woman says it is both and man says it is both
dta$mgt_both_pl3 <- rowSums(cbind( (rowSums(cbind((dta$grp3c10==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp3h10==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp3c10==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h10==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$mgt_both_pl3[is.na((dta$grp3c10==3 )) & is.na((dta$spouse2grp_sp3h10==3 ))] <- NA

dta$mgt_man_both_pl3 <- rowSums(cbind( (rowSums(cbind((dta$grp3c10==2  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp3h10==1  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp3c10==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h10==2  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$mgt_man_both_pl3[is.na((dta$grp3c10==3 )) & is.na((dta$spouse2grp_sp3h10==3 ))] <- NA
dta$mgt_woman_both_pl3 <- rowSums(cbind( (rowSums(cbind((dta$grp3c10==1  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp3h10==2  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp3c10==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h10==1  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$mgt_woman_both_pl3[is.na((dta$grp3c10==3 )) & is.na((dta$spouse2grp_sp3h10==3 ))] <- NA

dta$mgt_man_pl4 <-  rowSums(cbind((dta$grp4d10==1  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j10==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$mgt_man_pl4[is.na( (dta$grp4d10==1 ) ) & is.na((dta$spouse2group_sp4j10==1 ) )] <- NA
dta$mgt_both_man_pl4 <-  rowSums(cbind((dta$grp4d10==3  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j10==3  & dta$person_interviewed=="woman")), na.rm=T)
dta$mgt_both_man_pl4[is.na( (dta$grp4d10==1 ) ) & is.na((dta$spouse2group_sp4j10==1 ) )] <- NA
dta$mgt_woman_man_pl4 <-  rowSums(cbind((dta$grp4d10==2  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j10==2  & dta$person_interviewed=="woman")), na.rm=T)
dta$mgt_woman_man_pl4[is.na( (dta$grp4d10==1 ) ) & is.na((dta$spouse2group_sp4j10==1 ) )] <- NA
dta$mgt_woman_pl4 <-  rowSums(cbind((dta$grp4d10==1  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4j10==1  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_woman_pl4[is.na((dta$grp4d10==1 ) ) & is.na((dta$spouse2group_sp4j10==1 ))] <- NA
dta$mgt_both_woman_pl4 <-  rowSums(cbind((dta$grp4d10==3  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4j10==3  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_both_woman_pl4[is.na((dta$grp4d10==1 ) ) & is.na((dta$spouse2group_sp4j10==1 ))] <- NA
dta$mgt_man_woman_pl4 <-  rowSums(cbind((dta$grp4d10==2  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4j10==2  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_man_woman_pl4[is.na((dta$grp4d10==1 ) ) & is.na((dta$spouse2group_sp4j10==1 ))] <- NA

dta$mgt_woman_involved_pl4 <-  rowSums(cbind((dta$grp4d10%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4j10%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_woman_involved_pl4[is.na((dta$grp4d10==1 ) ) & is.na((dta$spouse2group_sp4j10==1 ))] <- NA
dta$mgt_man_involved_pl4 <-  rowSums(cbind((dta$grp4d10%in%c(1,3,5)  & dta$person_interviewed=="man") ,  (dta$spouse2group_sp4j10%in%c(1,3,5)  & dta$person_interviewed=="woman")), na.rm=T)
dta$mgt_man_involved_pl4[is.na((dta$grp4d10==1 ) ) & is.na((dta$spouse2group_sp4j10==1 ))] <- NA
##woman says it is both and man says it is both
dta$mgt_both_pl4 <- rowSums(cbind( (rowSums(cbind((dta$grp4d10==3  & dta$person_interviewed=="woman") , (dta$spouse2group_sp4j10==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp4d10==3  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j10==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$mgt_both_pl4[is.na((dta$grp4d10==3 )) & is.na((dta$spouse2group_sp4j10==3 ))] <- NA

dta$mgt_man_both_pl4 <- rowSums(cbind( (rowSums(cbind((dta$grp4d10==2  & dta$person_interviewed=="woman") , (dta$spouse2group_sp4j10==1  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp4d10==1  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j10==2  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$mgt_man_both_pl4[is.na((dta$grp4d10==3 )) & is.na((dta$spouse2group_sp4j10==3 ))] <- NA

dta$mgt_woman_both_pl4 <- rowSums(cbind( (rowSums(cbind((dta$grp4d10==1  & dta$person_interviewed=="woman") , (dta$spouse2group_sp4j10==2  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp4d10==2  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j10==1  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$mgt_woman_both_pl4[is.na((dta$grp4d10==3 )) & is.na((dta$spouse2group_sp4j10==3 ))] <- NA

dta$mgt_man_pl5 <-  rowSums(cbind((dta$grp5e10==1  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k10==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$mgt_man_pl5[is.na( (dta$grp5e10==1 ) ) & is.na((dta$spouse2grp5_sp5k10==1 ) )] <- NA
dta$mgt_both_man_pl5 <-  rowSums(cbind((dta$grp5e10==3  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k10==3  & dta$person_interviewed=="woman")), na.rm=T)
dta$mgt_both_man_pl5[is.na( (dta$grp5e10==1 ) ) & is.na((dta$spouse2grp5_sp5k10==1 ) )] <- NA
dta$mgt_woman_man_pl5 <-  rowSums(cbind((dta$grp5e10==2  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k10==2  & dta$person_interviewed=="woman")), na.rm=T)
dta$mgt_woman_man_pl5[is.na( (dta$grp5e10==1 ) ) & is.na((dta$spouse2grp5_sp5k10==1 ) )] <- NA
dta$mgt_woman_pl5 <-  rowSums(cbind((dta$grp5e10==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5k10==1  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_woman_pl5[is.na((dta$grp5e10==1 ) ) & is.na((dta$spouse2grp5_sp5k10==1 ))] <- NA
dta$mgt_both_woman_pl5 <-  rowSums(cbind((dta$grp5e10==3  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5k10==3  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_both_woman_pl5[is.na((dta$grp5e10==1 ) ) & is.na((dta$spouse2grp5_sp5k10==1 ))] <- NA
dta$mgt_man_woman_pl5 <-  rowSums(cbind((dta$grp5e10==2  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5k10==2  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_man_woman_pl5[is.na((dta$grp5e10==1 ) ) & is.na((dta$spouse2grp5_sp5k10==1 ))] <- NA

dta$mgt_woman_involved_pl5 <-  rowSums(cbind((dta$grp5e10%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5k10%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$mgt_woman_involved_pl5[is.na((dta$grp5e10==1 ) ) & is.na((dta$spouse2grp5_sp5k10==1 ))] <- NA
dta$mgt_man_involved_pl5 <-  rowSums(cbind((dta$grp5e10%in%c(1,3,5)  & dta$person_interviewed=="man") ,  (dta$spouse2grp5_sp5k10%in%c(1,3,5)  & dta$person_interviewed=="woman")), na.rm=T)
dta$mgt_man_involved_pl5[is.na((dta$grp5e10==1 ) ) & is.na((dta$spouse2grp5_sp5k10==1 ))] <- NA
##woman says it is both and man says it is both
dta$mgt_both_pl5 <- rowSums(cbind( (rowSums(cbind((dta$grp5e10==3  & dta$person_interviewed=="woman") , (dta$spouse2grp5_sp5k10==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp5e10==3  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k10==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$mgt_both_pl5[is.na((dta$grp5e10==3 )) & is.na((dta$spouse2grp5_sp5k10==3 ))] <- NA

dta$mgt_man_both_pl5 <- rowSums(cbind( (rowSums(cbind((dta$grp5e10==2  & dta$person_interviewed=="woman") , (dta$spouse2grp5_sp5k10==1  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp5e10==1  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k10==2  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$mgt_man_both_pl5[is.na((dta$grp5e10==3 )) & is.na((dta$spouse2grp5_sp5k10==3 ))] <- NA
dta$mgt_woman_both_pl5 <- rowSums(cbind( (rowSums(cbind((dta$grp5e10==1  & dta$person_interviewed=="woman") , (dta$spouse2grp5_sp5k10==2  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp5e10==2  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k10==1  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$mgt_woman_both_pl5[is.na((dta$grp5e10==3 )) & is.na((dta$spouse2grp5_sp5k10==3 ))] <- NA

dta$nr_man_plots_decmaize <- rowSums(dta[c("mgt_man_pl1","mgt_man_pl2","mgt_man_pl3","mgt_man_pl4","mgt_man_pl5")], na.rm=T)
dta$nr_woman_plots_decmaize <- rowSums(dta[c("mgt_woman_pl1","mgt_woman_pl2","mgt_woman_pl3","mgt_woman_pl4","mgt_woman_pl5")], na.rm=T)
dta$nr_woman_involved_plots_decmaize <- rowSums(dta[c("mgt_woman_involved_pl1","mgt_woman_involved_pl2","mgt_woman_involved_pl3","mgt_woman_involved_pl4","mgt_woman_involved_pl5")], na.rm=T)
dta$nr_joint_plots_decmaize <- rowSums(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")], na.rm=T)
dta$all_plots_both_decide <- rowSums(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")], na.rm=T) == rowSums(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))

dta$share_plots_both_decide <- rowSums(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")], na.rm=T)/rowSums(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))

dta$share_plots_man_decide <- rowSums(dta[c("mgt_man_both_pl1","mgt_man_both_pl2","mgt_man_both_pl3","mgt_man_both_pl4","mgt_man_both_pl5")], na.rm=T)/rowSums(!is.na(dta[c("mgt_man_both_pl1","mgt_man_both_pl2","mgt_man_both_pl3","mgt_man_both_pl4","mgt_man_both_pl5")]))
dta$share_plots_woman_decide <- rowSums(dta[c("mgt_woman_both_pl1","mgt_woman_both_pl2","mgt_woman_both_pl3","mgt_woman_both_pl4","mgt_woman_both_pl5")], na.rm=T)/rowSums(!is.na(dta[c("mgt_woman_both_pl1","mgt_woman_both_pl2","mgt_woman_both_pl3","mgt_woman_both_pl4","mgt_woman_both_pl5")]))

## both man
dta$share_both_man_decide <- rowSums(dta[c("mgt_both_man_pl1","mgt_both_man_pl2","mgt_both_man_pl3","mgt_both_man_pl4","mgt_both_man_pl5")], na.rm=T)/rowSums(!is.na(dta[c("mgt_both_man_pl1","mgt_both_man_pl2","mgt_both_man_pl3","mgt_both_man_pl4","mgt_both_man_pl5")]))
### man man
dta$share_man_man_decide <- rowSums(dta[c("mgt_man_pl1","mgt_man_pl2","mgt_man_pl3","mgt_man_pl4","mgt_man_pl5")], na.rm=T)/rowSums(!is.na(dta[c("mgt_man_pl1","mgt_man_pl2","mgt_man_pl3","mgt_man_pl4","mgt_man_pl5")]))
### woman man
dta$share_woman_man_decide <- rowSums(dta[c("mgt_woman_man_pl1","mgt_woman_man_pl2","mgt_woman_man_pl3","mgt_woman_man_pl4","mgt_woman_man_pl5")], na.rm=T)/rowSums(!is.na(dta[c("mgt_woman_man_pl1","mgt_woman_man_pl2","mgt_woman_man_pl3","mgt_woman_man_pl4","mgt_woman_man_pl5")]))

## both woman
dta$share_both_woman_decide <-rowSums(dta[c("mgt_both_woman_pl1","mgt_both_woman_pl2","mgt_both_woman_pl3","mgt_both_woman_pl4","mgt_both_woman_pl5")], na.rm=T)/rowSums(!is.na(dta[c("mgt_both_woman_pl1","mgt_both_woman_pl2","mgt_both_woman_pl3","mgt_both_woman_pl4","mgt_both_woman_pl5")]))
### man woman
dta$share_man_woman_decide <-rowSums(dta[c("mgt_man_woman_pl1","mgt_man_woman_pl2","mgt_man_woman_pl3","mgt_man_woman_pl4","mgt_man_woman_pl5")], na.rm=T)/rowSums(!is.na(dta[c("mgt_man_woman_pl1","mgt_man_woman_pl2","mgt_man_woman_pl3","mgt_man_woman_pl4","mgt_man_woman_pl5")]))
### woman womman
dta$share_woman_woman_decide <-rowSums(dta[c("mgt_woman_pl1","mgt_woman_pl2","mgt_woman_pl3","mgt_woman_pl4","mgt_woman_pl5")], na.rm=T)/rowSums(!is.na(dta[c("mgt_woman_pl1","mgt_woman_pl2","mgt_woman_pl3","mgt_woman_pl4","mgt_woman_pl5")]))


########################################################################################
###who decided to start planting maize at particular time (decide1)
dta$dectime_man_pl1 <-  rowSums(cbind((dta$grp1decide1==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1decide_sp1==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$dectime_man_pl1[is.na( (dta$grp1decide1==1 ) ) & is.na((dta$spouse2grp_sp1decide_sp1==1 ) )] <- NA
dta$dectime_both_man_pl1 <-  rowSums(cbind((dta$grp1decide1==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1decide_sp1==3  & dta$person_interviewed=="woman")), na.rm=T)
dta$dectime_both_man_pl1[is.na( (dta$grp1decide1==1 ) ) & is.na((dta$spouse2grp_sp1decide_sp1==1 ) )] <- NA
dta$dectime_woman_man_pl1 <-  rowSums(cbind((dta$grp1decide1==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1decide_sp1==2  & dta$person_interviewed=="woman")), na.rm=T)
dta$dectime_woman_man_pl1[is.na( (dta$grp1decide1==1 ) ) & is.na((dta$spouse2grp_sp1decide_sp1==1 ) )] <- NA

dta$dectime_woman_pl1 <-  rowSums(cbind((dta$grp1decide1==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1decide_sp1==1  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_woman_pl1[is.na((dta$grp1decide1==1 ) ) & is.na((dta$spouse2grp_sp1decide_sp1==1 ))] <- NA
dta$dectime_both_woman_pl1 <-  rowSums(cbind((dta$grp1decide1==3  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1decide_sp1==3  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_both_woman_pl1[is.na((dta$grp1decide1==1 ) ) & is.na((dta$spouse2grp_sp1decide_sp1==1 ))] <- NA
dta$dectime_man_woman_pl1 <-  rowSums(cbind((dta$grp1decide1==2  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1decide_sp1==2  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_man_woman_pl1[is.na((dta$grp1decide1==1 ) ) & is.na((dta$spouse2grp_sp1decide_sp1==1 ))] <- NA

dta$dectime_woman_involved_pl1 <- rowSums(cbind((dta$grp1decide1%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1decide_sp1%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_woman_involved_pl1[is.na((dta$grp1decide1==1 ) ) & is.na((dta$spouse2grp_sp1decide_sp1==1 ))] <- NA
dta$dectime_man_involved_pl1 <- rowSums(cbind((dta$grp1decide1%in%c(1,3,5)  & dta$person_interviewed=="man") ,  (dta$spouse2grp_sp1decide_sp1%in%c(1,3,5)  & dta$person_interviewed=="woman")), na.rm=T)
dta$dectime_man_involved_pl1[is.na((dta$grp1decide1==1 ) ) & is.na((dta$spouse2grp_sp1decide_sp1==1 ))] <- NA
##woman says it is both and man says it is both
dta$dectime_both_pl1 <- rowSums(cbind( (rowSums(cbind((dta$grp1decide1==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp1decide_sp1==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp1decide1==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1decide_sp1==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$dectime_both_pl1[is.na((dta$grp1decide1==3 )) & is.na((dta$spouse2grp_sp1decide_sp1==3 ))] <- NA

##woman says it is man and man says it is man
dta$dectime_man_both_pl1 <- rowSums(cbind( (rowSums(cbind((dta$grp1decide1==2  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp1decide_sp1==1  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp1decide1==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1decide_sp1==2  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$dectime_man_both_pl1[is.na((dta$grp1decide1==3 )) & is.na((dta$spouse2grp_sp1decide_sp1==3 ))] <- NA

dta$dectime_woman_both_pl1 <- rowSums(cbind( (rowSums(cbind((dta$grp1decide1==1  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp1decide_sp1==2  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp1decide1==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1decide_sp1==1  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$dectime_woman_both_pl1[is.na((dta$grp1decide1==3 )) & is.na((dta$spouse2grp_sp1decide_sp1==3 ))] <- NA


dta$dectime_man_pl2 <-  rowSums(cbind((dta$grp2decide2==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2decide_sp2==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$dectime_man_pl2[is.na( (dta$grp2decide2==1 ) ) & is.na((dta$spouse2grp_sp2decide_sp2==1 ) )] <- NA
dta$dectime_both_man_pl2 <-  rowSums(cbind((dta$grp2decide2==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2decide_sp2==3  & dta$person_interviewed=="woman")), na.rm=T)
dta$dectime_both_man_pl2[is.na( (dta$grp2decide2==1 ) ) & is.na((dta$spouse2grp_sp2decide_sp2==1 ) )] <- NA
dta$dectime_woman_man_pl2 <-  rowSums(cbind((dta$grp2decide2==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2decide_sp2==2  & dta$person_interviewed=="woman")), na.rm=T)
dta$dectime_woman_man_pl2[is.na( (dta$grp2decide2==1 ) ) & is.na((dta$spouse2grp_sp2decide_sp2==1 ) )] <- NA

dta$dectime_woman_pl2 <-  rowSums(cbind((dta$grp2decide2==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2decide_sp2==1  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_woman_pl2[is.na((dta$grp2decide2==1 ) ) & is.na((dta$spouse2grp_sp2decide_sp2==1 ))] <- NA
dta$dectime_both_woman_pl2 <-  rowSums(cbind((dta$grp2decide2==3  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2decide_sp2==3  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_both_woman_pl2[is.na((dta$grp2decide2==1 ) ) & is.na((dta$spouse2grp_sp2decide_sp2==1 ))] <- NA
dta$dectime_man_woman_pl2 <-  rowSums(cbind((dta$grp2decide2==2  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2decide_sp2==2  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_man_woman_pl2[is.na((dta$grp2decide2==1 ) ) & is.na((dta$spouse2grp_sp2decide_sp2==1 ))] <- NA

dta$dectime_woman_involved_pl2 <-  rowSums(cbind((dta$grp2decide2%in%c(1,3,5)   & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2decide_sp2%in%c(1,3,5)   & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_woman_involved_pl2[is.na((dta$grp2decide2==1 ) ) & is.na((dta$spouse2grp_sp2decide_sp2==1 ))] <- NA
dta$dectime_man_involved_pl2 <-  rowSums(cbind((dta$grp2decide2%in%c(1,3,5)   & dta$person_interviewed=="man") ,  (dta$spouse2grp_sp2decide_sp2%in%c(1,3,5)   & dta$person_interviewed=="woman")), na.rm=T)
dta$dectime_man_involved_pl2[is.na((dta$grp2decide2==1 ) ) & is.na((dta$spouse2grp_sp2decide_sp2==1 ))] <- NA
##woman says it is both and man says it is both
dta$dectime_both_pl2 <- rowSums(cbind( (rowSums(cbind((dta$grp2decide2==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp2decide_sp2==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp2decide2==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2decide_sp2==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$dectime_both_pl2[is.na((dta$grp2decide2==3 )) & is.na((dta$spouse2grp_sp2decide_sp2==3 ))] <- NA

dta$dectime_man_both_pl2 <- rowSums(cbind( (rowSums(cbind((dta$grp2decide2==2  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp2decide_sp2==1  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp2decide2==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2decide_sp2==2  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$dectime_man_both_pl2[is.na((dta$grp2decide2==3 )) & is.na((dta$spouse2grp_sp2decide_sp2==3 ))] <- NA

dta$dectime_woman_both_pl2 <- rowSums(cbind( (rowSums(cbind((dta$grp2decide2==1  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp2decide_sp2==2  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp2decide2==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2decide_sp2==1  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$dectime_woman_both_pl2[is.na((dta$grp2decide2==3 )) & is.na((dta$spouse2grp_sp2decide_sp2==3 ))] <- NA

dta$dectime_man_pl3 <-  rowSums(cbind((dta$grp3decide3==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3days_sp3==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$dectime_man_pl3[is.na( (dta$grp3decide3==1 ) ) & is.na((dta$spouse2grp_sp3days_sp3==1 ) )] <- NA
dta$dectime_both_man_pl3 <-  rowSums(cbind((dta$grp3decide3==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3days_sp3==3  & dta$person_interviewed=="woman")), na.rm=T)
dta$dectime_both_man_pl3[is.na( (dta$grp3decide3==1 ) ) & is.na((dta$spouse2grp_sp3days_sp3==1 ) )] <- NA
dta$dectime_woman_man_pl3 <-  rowSums(cbind((dta$grp3decide3==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3days_sp3==2  & dta$person_interviewed=="woman")), na.rm=T)
dta$dectime_woman_man_pl3[is.na( (dta$grp3decide3==1 ) ) & is.na((dta$spouse2grp_sp3days_sp3==1 ) )] <- NA

dta$dectime_woman_pl3 <-  rowSums(cbind((dta$grp3decide3==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3days_sp3==1  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_woman_pl3[is.na((dta$grp3decide3==1 ) ) & is.na((dta$spouse2grp_sp3days_sp3==1 ))] <- NA
dta$dectime_both_woman_pl3 <-  rowSums(cbind((dta$grp3decide3==3  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3days_sp3==3  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_both_woman_pl3[is.na((dta$grp3decide3==1 ) ) & is.na((dta$spouse2grp_sp3days_sp3==1 ))] <- NA
dta$dectime_man_woman_pl3 <-  rowSums(cbind((dta$grp3decide3==2  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3days_sp3==2  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_man_woman_pl3[is.na((dta$grp3decide3==1 ) ) & is.na((dta$spouse2grp_sp3days_sp3==1 ))] <- NA

dta$dectime_woman_involved_pl3 <-  rowSums(cbind((dta$grp3decide3%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3days_sp3%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_woman_involved_pl3[is.na((dta$grp3decide3==1 ) ) & is.na((dta$spouse2grp_sp3days_sp3==1 ))] <- NA
dta$dectime_man_involved_pl3 <-  rowSums(cbind((dta$grp3decide3%in%c(1,3,5)  & dta$person_interviewed=="man") ,  (dta$spouse2grp_sp3days_sp3%in%c(1,3,5)  & dta$person_interviewed=="woman")), na.rm=T)
dta$dectime_man_involved_pl3[is.na((dta$grp3decide3==1 ) ) & is.na((dta$spouse2grp_sp3days_sp3==1 ))] <- NA

##woman says it is both and man says it is both
dta$dectime_both_pl3 <- rowSums(cbind( (rowSums(cbind((dta$grp3decide3==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp3days_sp3==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp3decide3==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3days_sp3==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$dectime_both_pl3[is.na((dta$grp3decide3==3 )) & is.na((dta$spouse2grp_sp3days_sp3==3 ))] <- NA

dta$dectime_man_both_pl3 <- rowSums(cbind( (rowSums(cbind((dta$grp3decide3==2  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp3days_sp3==1  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp3decide3==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3days_sp3==2  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$dectime_man_both_pl3[is.na((dta$grp3decide3==3 )) & is.na((dta$spouse2grp_sp3days_sp3==3 ))] <- NA

dta$dectime_woman_both_pl3 <- rowSums(cbind( (rowSums(cbind((dta$grp3decide3==1  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp3days_sp3==2  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp3decide3==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3days_sp3==1  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$dectime_woman_both_pl3[is.na((dta$grp3decide3==3 )) & is.na((dta$spouse2grp_sp3days_sp3==3 ))] <- NA

dta$dectime_man_pl4 <-  rowSums(cbind((dta$grp4decide4==1  & dta$person_interviewed=="man") , (dta$spouse2group_sp4dayssp4==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$dectime_man_pl4[is.na( (dta$grp4decide4==1 ) ) & is.na((dta$spouse2group_sp4dayssp4==1 ) )] <- NA
dta$dectime_woman_man_pl4 <-  rowSums(cbind((dta$grp4decide4==2  & dta$person_interviewed=="man") , (dta$spouse2group_sp4dayssp4==2  & dta$person_interviewed=="woman")), na.rm=T)
dta$dectime_woman_man_pl4[is.na( (dta$grp4decide4==1 ) ) & is.na((dta$spouse2group_sp4dayssp4==1 ) )] <- NA

dta$dectime_both_man_pl4 <-  rowSums(cbind((dta$grp4decide4==3  & dta$person_interviewed=="man") , (dta$spouse2group_sp4dayssp4==3  & dta$person_interviewed=="woman")), na.rm=T)
dta$dectime_both_man_pl4[is.na( (dta$grp4decide4==1 ) ) & is.na((dta$spouse2group_sp4dayssp4==1 ) )] <- NA
dta$dectime_woman_pl4 <-  rowSums(cbind((dta$grp4decide4==1  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4dayssp4==1  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_woman_pl4[is.na((dta$grp4decide4==1 ) ) & is.na((dta$spouse2group_sp4dayssp4==1 ))] <- NA
dta$dectime_both_woman_pl4 <-  rowSums(cbind((dta$grp4decide4==3  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4dayssp4==3  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_both_woman_pl4[is.na((dta$grp4decide4==1 ) ) & is.na((dta$spouse2group_sp4dayssp4==1 ))] <- NA
dta$dectime_man_woman_pl4 <-  rowSums(cbind((dta$grp4decide4==2  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4dayssp4==2  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_man_woman_pl4[is.na((dta$grp4decide4==1 ) ) & is.na((dta$spouse2group_sp4dayssp4==1 ))] <- NA

dta$dectime_woman_involved_pl4 <-  rowSums(cbind((dta$grp4decide4%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4dayssp4%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_woman_involved_pl4[is.na((dta$grp4decide4==1 ) ) & is.na((dta$spouse2group_sp4dayssp4==1 ))] <- NA
dta$dectime_man_involved_pl4 <-  rowSums(cbind((dta$grp4decide4%in%c(1,3,5)  & dta$person_interviewed=="man") ,  (dta$spouse2group_sp4dayssp4%in%c(1,3,5)  & dta$person_interviewed=="woman")), na.rm=T)
dta$dectime_man_involved_pl4[is.na((dta$grp4decide4==1 ) ) & is.na((dta$spouse2group_sp4dayssp4==1 ))] <- NA

##woman says it is both and man says it is both
dta$dectime_both_pl4 <- rowSums(cbind( (rowSums(cbind((dta$grp4decide4==3  & dta$person_interviewed=="woman") , (dta$spouse2group_sp4dayssp4==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp4decide4==3  & dta$person_interviewed=="man") , (dta$spouse2group_sp4dayssp4==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$dectime_both_pl4[is.na((dta$grp4decide4==3 )) & is.na((dta$spouse2group_sp4dayssp4==3 ))] <- NA

dta$dectime_man_both_pl4 <- rowSums(cbind( (rowSums(cbind((dta$grp4decide4==2  & dta$person_interviewed=="woman") , (dta$spouse2group_sp4dayssp4==1  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp4decide4==1  & dta$person_interviewed=="man") , (dta$spouse2group_sp4dayssp4==2  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$dectime_man_both_pl4[is.na((dta$grp4decide4==3 )) & is.na((dta$spouse2group_sp4dayssp4==3 ))] <- NA

dta$dectime_woman_both_pl4 <- rowSums(cbind( (rowSums(cbind((dta$grp4decide4==1  & dta$person_interviewed=="woman") , (dta$spouse2group_sp4dayssp4==2  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp4decide4==2  & dta$person_interviewed=="man") , (dta$spouse2group_sp4dayssp4==1  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$dectime_woman_both_pl4[is.na((dta$grp4decide4==3 )) & is.na((dta$spouse2group_sp4dayssp4==3 ))] <- NA

dta$dectime_man_pl5 <-  rowSums(cbind((dta$grp5decide5==1  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5dayssp5==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$dectime_man_pl5[is.na( (dta$grp5decide5==1 ) ) & is.na((dta$spouse2grp5_sp5dayssp5==1 ) )] <- NA
dta$dectime_woman_man_pl5 <-  rowSums(cbind((dta$grp5decide5==2  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5dayssp5==2  & dta$person_interviewed=="woman")), na.rm=T)
dta$dectime_woman_man_pl5[is.na( (dta$grp5decide5==1 ) ) & is.na((dta$spouse2grp5_sp5dayssp5==1 ) )] <- NA

dta$dectime_both_man_pl5 <-  rowSums(cbind((dta$grp5decide5==3  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5dayssp5==3  & dta$person_interviewed=="woman")), na.rm=T)
dta$dectime_both_man_pl5[is.na( (dta$grp5decide5==1 ) ) & is.na((dta$spouse2grp5_sp5dayssp5==1 ) )] <- NA
dta$dectime_woman_pl5 <-  rowSums(cbind((dta$grp5decide5==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5dayssp5==1  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_woman_pl5[is.na((dta$grp5decide5==1 ) ) & is.na((dta$spouse2grp5_sp5dayssp5==1 ))] <- NA
dta$dectime_both_woman_pl5 <-  rowSums(cbind((dta$grp5decide5==3  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5dayssp5==3  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_both_woman_pl5[is.na((dta$grp5decide5==1 ) ) & is.na((dta$spouse2grp5_sp5dayssp5==1 ))] <- NA
dta$dectime_man_woman_pl5 <-  rowSums(cbind((dta$grp5decide5==2  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5dayssp5==2  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_man_woman_pl5[is.na((dta$grp5decide5==1 ) ) & is.na((dta$spouse2grp5_sp5dayssp5==1 ))] <- NA

dta$dectime_woman_involved_pl5 <-  rowSums(cbind((dta$grp5decide5%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5dayssp5%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$dectime_woman_involved_pl5[is.na((dta$grp5decide5==1 ) ) & is.na((dta$spouse2grp5_sp5dayssp5==1 ))] <- NA
dta$dectime_man_involved_pl5 <-  rowSums(cbind((dta$grp5decide5%in%c(1,3,5)  & dta$person_interviewed=="man") ,  (dta$spouse2grp5_sp5dayssp5%in%c(1,3,5)  & dta$person_interviewed=="woman")), na.rm=T)
dta$dectime_man_involved_pl5[is.na((dta$grp5decide5==1 ) ) & is.na((dta$spouse2grp5_sp5dayssp5==1 ))] <- NA
##woman says it is both and man says it is both
dta$dectime_both_pl5 <- rowSums(cbind( (rowSums(cbind((dta$grp5decide5==3  & dta$person_interviewed=="woman") , (dta$spouse2grp5_sp5dayssp5==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp5decide5==3  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5dayssp5==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$dectime_both_pl5[is.na((dta$grp5decide5==3 )) & is.na((dta$spouse2grp5_sp5dayssp5==3 ))] <- NA

dta$dectime_man_both_pl5 <- rowSums(cbind( (rowSums(cbind((dta$grp5decide5==2  & dta$person_interviewed=="woman") , (dta$spouse2grp5_sp5dayssp5==1  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp5decide5==1  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5dayssp5==2  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$dectime_man_both_pl5[is.na((dta$grp5decide5==3 )) & is.na((dta$spouse2grp5_sp5dayssp5==3 ))] <- NA

dta$dectime_woman_both_pl5 <- rowSums(cbind( (rowSums(cbind((dta$grp5decide5==1  & dta$person_interviewed=="woman") , (dta$spouse2grp5_sp5dayssp5==2  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp5decide5==2  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5dayssp5==1  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$dectime_woman_both_pl5[is.na((dta$grp5decide5==3 )) & is.na((dta$spouse2grp5_sp5dayssp5==3 ))] <- NA

dta$nr_man_plots_dectime <- rowSums(dta[c("dectime_man_pl1","dectime_man_pl2","dectime_man_pl3","dectime_man_pl4","dectime_man_pl5")], na.rm=T)
dta$nr_woman_plots_dectime <- rowSums(dta[c("dectime_woman_pl1","dectime_woman_pl2","dectime_woman_pl3","dectime_woman_pl4","dectime_woman_pl5")], na.rm=T)
dta$nr_woman_involved_plots_dectime <- rowSums(dta[c("dectime_woman_involved_pl1","dectime_woman_involved_pl2","dectime_woman_involved_pl3","dectime_woman_involved_pl4","dectime_woman_involved_pl5")], na.rm=T)
dta$nr_joint_plots_dectime <- rowSums(dta[c("dectime_both_pl1","dectime_both_pl2","dectime_both_pl3","dectime_both_pl4","dectime_both_pl5")], na.rm=T)
dta$share_plots_both_dectime <- rowSums(dta[c("dectime_both_pl1","dectime_both_pl2","dectime_both_pl3","dectime_both_pl4","dectime_both_pl5")], na.rm=T)/rowSums(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))

dta$share_plots_man_both_dectime <- rowSums(dta[c("dectime_man_both_pl1","dectime_man_both_pl2","dectime_man_both_pl3","dectime_man_both_pl4","dectime_man_both_pl5")], na.rm=T)/rowSums(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))

dta$share_plots_woman_both_dectime <- rowSums(dta[c("dectime_woman_both_pl1","dectime_woman_both_pl2","dectime_woman_both_pl3","dectime_woman_both_pl4","dectime_woman_both_pl5")], na.rm=T)/rowSums(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))

dta$share_plots_man_dectime <- rowSums(dta[c("dectime_man_pl1","dectime_man_pl2","dectime_man_pl3","dectime_man_pl4","dectime_man_pl5")], na.rm=T)/rowSums(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))
dta$share_plots_woman_dectime <- rowSums(dta[c("dectime_woman_pl1","dectime_woman_pl2","dectime_woman_pl3","dectime_woman_pl4","dectime_woman_pl5")], na.rm=T)/rowSums(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))



########################################################################################
###who decides on spacing

###who decides on spacing

dta$decspace_man_pl1 <-  rowSums(cbind((dta$grp1a23==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f23==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decspace_man_pl1[is.na( (dta$grp1a23==1 ) ) & is.na((dta$spouse2grp_sp1f23==1 ) )] <- NA
dta$decspace_both_man_pl1 <-  rowSums(cbind((dta$grp1a23==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f23==3  & dta$person_interviewed=="woman")), na.rm=T)
dta$decspace_both_man_pl1[is.na( (dta$grp1a23==1 ) ) & is.na((dta$spouse2grp_sp1f23==1 ) )] <- NA
dta$decspace_woman_man_pl1 <-  rowSums(cbind((dta$grp1a23==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f23==2 & dta$person_interviewed=="woman")), na.rm=T)
dta$decspace_woman_man_pl1[is.na( (dta$grp1a23==1 ) ) & is.na((dta$spouse2grp_sp1f23==1 ) )] <- NA

dta$decspace_woman_pl1 <-  rowSums(cbind((dta$grp1a23==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1f23==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_woman_pl1[is.na((dta$grp1a23==1 ) ) & is.na((dta$spouse2grp_sp1f23==1 ))] <- NA
dta$decspace_both_woman_pl1 <-  rowSums(cbind((dta$grp1a23==3  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1f23==3  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_both_woman_pl1[is.na((dta$grp1a23==1 ) ) & is.na((dta$spouse2grp_sp1f23==1 ))] <- NA
dta$decspace_man_woman_pl1 <-  rowSums(cbind((dta$grp1a23==2  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1f23==2  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_man_woman_pl1[is.na((dta$grp1a23==1 ) ) & is.na((dta$spouse2grp_sp1f23==1 ))] <- NA

dta$decspace_woman_involved_pl1 <- rowSums(cbind((dta$grp1a23%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1f23%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_woman_involved_pl1[is.na((dta$grp1a23==1 ) ) & is.na((dta$spouse2grp_sp1f23==1 ))] <- NA
dta$decspace_man_involved_pl1 <- rowSums(cbind((dta$grp1a23%in%c(1,3,5)  & dta$person_interviewed=="man") ,  (dta$spouse2grp_sp1f23%in%c(1,3,5)  & dta$person_interviewed=="woman")), na.rm=T)
dta$decspace_man_involved_pl1[is.na((dta$grp1a23==1 ) ) & is.na((dta$spouse2grp_sp1f23==1 ))] <- NA
##woman says it is both and man says it is both
dta$decspace_both_pl1 <- rowSums(cbind( (rowSums(cbind((dta$grp1a23==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp1f23==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp1a23==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f23==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decspace_both_pl1[is.na((dta$grp1a23==3 )) & is.na((dta$spouse2grp_sp1f23==3 ))] <- NA

dta$decspace_man_both_pl1 <- rowSums(cbind( (rowSums(cbind((dta$grp1a23==2  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp1f23==1  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp1a23==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f23==2 & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decspace_man_both_pl1[is.na((dta$grp1a23==3 )) & is.na((dta$spouse2grp_sp1f23==3 ))] <- NA

dta$decspace_woman_both_pl1 <- rowSums(cbind( (rowSums(cbind((dta$grp1a23==1  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp1f23==2  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp1a23==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f23==1  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decspace_woman_both_pl1[is.na((dta$grp1a23==3 )) & is.na((dta$spouse2grp_sp1f23==3 ))] <- NA

dta$decspace_man_pl2 <-  rowSums(cbind((dta$grp2b23==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g23==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decspace_man_pl2[is.na( (dta$grp2b23==1 ) ) & is.na((dta$spouse2grp_sp2g23==1 ) )] <- NA
dta$decspace_woman_man_pl2 <-  rowSums(cbind((dta$grp2b23==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g23==2  & dta$person_interviewed=="woman")), na.rm=T)
dta$decspace_woman_man_pl2[is.na( (dta$grp2b23==1 ) ) & is.na((dta$spouse2grp_sp2g23==1 ) )] <- NA

dta$decspace_both_man_pl2 <-  rowSums(cbind((dta$grp2b23==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g23==3  & dta$person_interviewed=="woman")), na.rm=T)
dta$decspace_both_man_pl2[is.na( (dta$grp2b23==1 ) ) & is.na((dta$spouse2grp_sp2g23==1 ) )] <- NA
dta$decspace_woman_pl2 <-  rowSums(cbind((dta$grp2b23==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2g23==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_woman_pl2[is.na((dta$grp2b23==1 ) ) & is.na((dta$spouse2grp_sp2g23==1 ))] <- NA
dta$decspace_both_woman_pl2 <-  rowSums(cbind((dta$grp2b23==3  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2g23==3  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_both_woman_pl2[is.na((dta$grp2b23==1 ) ) & is.na((dta$spouse2grp_sp2g23==1 ))] <- NA
dta$decspace_man_woman_pl2 <-  rowSums(cbind((dta$grp2b23==2  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2g23==2  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_man_woman_pl2[is.na((dta$grp2b23==1 ) ) & is.na((dta$spouse2grp_sp2g23==1 ))] <- NA

dta$decspace_woman_involved_pl2 <-  rowSums(cbind((dta$grp2b23%in%c(1,3,5)   & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2g23%in%c(1,3,5)   & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_woman_involved_pl2[is.na((dta$grp2b23==1 ) ) & is.na((dta$spouse2grp_sp2g23==1 ))] <- NA
dta$decspace_man_involved_pl2 <-  rowSums(cbind((dta$grp2b23%in%c(1,3,5)   & dta$person_interviewed=="man") ,  (dta$spouse2grp_sp2g23%in%c(1,3,5)   & dta$person_interviewed=="woman")), na.rm=T)
dta$decspace_man_involved_pl2[is.na((dta$grp2b23==1 ) ) & is.na((dta$spouse2grp_sp2g23==1 ))] <- NA
##woman says it is both and man says it is both
dta$decspace_both_pl2 <- rowSums(cbind( (rowSums(cbind((dta$grp2b23==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp2g23==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp2b23==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g23==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decspace_both_pl2[is.na((dta$grp2b23==3 )) & is.na((dta$spouse2grp_sp2g23==3 ))] <- NA

dta$decspace_man_both_pl2 <- rowSums(cbind( (rowSums(cbind((dta$grp2b23==2  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp2g23==1  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp2b23==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g23==2  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decspace_man_both_pl2[is.na((dta$grp2b23==3 )) & is.na((dta$spouse2grp_sp2g23==3 ))] <- NA

dta$decspace_woman_both_pl2 <- rowSums(cbind( (rowSums(cbind((dta$grp2b23==1  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp2g23==2  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp2b23==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g23==1  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decspace_woman_both_pl2[is.na((dta$grp2b23==3 )) & is.na((dta$spouse2grp_sp2g23==3 ))] <- NA

dta$decspace_man_pl3 <-  rowSums(cbind((dta$grp3c23==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h23==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decspace_man_pl3[is.na( (dta$grp3c23==1 ) ) & is.na((dta$spouse2grp_sp3h23==1 ) )] <- NA
dta$decspace_woman_man_pl3 <-  rowSums(cbind((dta$grp3c23==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h23==2  & dta$person_interviewed=="woman")), na.rm=T)
dta$decspace_woman_man_pl3[is.na( (dta$grp3c23==1 ) ) & is.na((dta$spouse2grp_sp3h23==1 ) )] <- NA
dta$decspace_both_man_pl3 <-  rowSums(cbind((dta$grp3c23==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h23==3  & dta$person_interviewed=="woman")), na.rm=T)
dta$decspace_both_man_pl3[is.na( (dta$grp3c23==1 ) ) & is.na((dta$spouse2grp_sp3h23==1 ) )] <- NA
dta$decspace_woman_pl3 <-  rowSums(cbind((dta$grp3c23==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3h23==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_woman_pl3[is.na((dta$grp3c23==1 ) ) & is.na((dta$spouse2grp_sp3h23==1 ))] <- NA
dta$decspace_man_woman_pl3 <-  rowSums(cbind((dta$grp3c23==2  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3h23==2  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_man_woman_pl3[is.na((dta$grp3c23==1 ) ) & is.na((dta$spouse2grp_sp3h23==1 ))] <- NA
dta$decspace_both_woman_pl3 <-  rowSums(cbind((dta$grp3c23==3  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3h23==3  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_both_woman_pl3[is.na((dta$grp3c23==1 ) ) & is.na((dta$spouse2grp_sp3h23==1 ))] <- NA

dta$decspace_woman_involved_pl3 <-  rowSums(cbind((dta$grp3c23%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3h23%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_woman_involved_pl3[is.na((dta$grp3c23==1 ) ) & is.na((dta$spouse2grp_sp3h23==1 ))] <- NA
dta$decspace_man_involved_pl3 <-  rowSums(cbind((dta$grp3c23%in%c(1,3,5)  & dta$person_interviewed=="man") ,  (dta$spouse2grp_sp3h23%in%c(1,3,5)  & dta$person_interviewed=="woman")), na.rm=T)
dta$decspace_man_involved_pl3[is.na((dta$grp3c23==1 ) ) & is.na((dta$spouse2grp_sp3h23==1 ))] <- NA
##woman says it is both and man says it is both
dta$decspace_both_pl3 <- rowSums(cbind( (rowSums(cbind((dta$grp3c23==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp3h23==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp3c23==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h23==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decspace_both_pl3[is.na((dta$grp3c23==3 )) & is.na((dta$spouse2grp_sp3h23==3 ))] <- NA

dta$decspace_man_both_pl3 <- rowSums(cbind( (rowSums(cbind((dta$grp3c23==2  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp3h23==1  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp3c23==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h23==2  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decspace_man_both_pl3[is.na((dta$grp3c23==3 )) & is.na((dta$spouse2grp_sp3h23==3 ))] <- NA

dta$decspace_woman_both_pl3 <- rowSums(cbind( (rowSums(cbind((dta$grp3c23==1  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp3h23==2  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp3c23==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h23==1 & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decspace_woman_both_pl3[is.na((dta$grp3c23==3 )) & is.na((dta$spouse2grp_sp3h23==3 ))] <- NA

dta$decspace_man_pl4 <-  rowSums(cbind((dta$grp4d23==1  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j23==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decspace_man_pl4[is.na( (dta$grp4d23==1 ) ) & is.na((dta$spouse2group_sp4j23==1 ) )] <- NA
dta$decspace_woman_man_pl4 <-  rowSums(cbind((dta$grp4d23==2  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j23==2  & dta$person_interviewed=="woman")), na.rm=T)
dta$decspace_woman_man_pl4[is.na( (dta$grp4d23==1 ) ) & is.na((dta$spouse2group_sp4j23==1 ) )] <- NA

dta$decspace_both_man_pl4 <-  rowSums(cbind((dta$grp4d23==3  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j23==3  & dta$person_interviewed=="woman")), na.rm=T)
dta$decspace_both_man_pl4[is.na( (dta$grp4d23==1 ) ) & is.na((dta$spouse2group_sp4j23==1 ) )] <- NA
dta$decspace_woman_pl4 <-  rowSums(cbind((dta$grp4d23==1  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4j23==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_woman_pl4[is.na((dta$grp4d23==1 ) ) & is.na((dta$spouse2group_sp4j23==1 ))] <- NA
dta$decspace_man_woman_pl4 <-  rowSums(cbind((dta$grp4d23==2  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4j23==2  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_man_woman_pl4[is.na((dta$grp4d23==1 ) ) & is.na((dta$spouse2group_sp4j23==1 ))] <- NA
dta$decspace_both_woman_pl4 <-  rowSums(cbind((dta$grp4d23==3 & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4j23==3  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_both_woman_pl4[is.na((dta$grp4d23==1 ) ) & is.na((dta$spouse2group_sp4j23==1 ))] <- NA
dta$decspace_woman_involved_pl4 <-  rowSums(cbind((dta$grp4d23%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4j23%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_woman_involved_pl4[is.na((dta$grp4d23==1 ) ) & is.na((dta$spouse2group_sp4j23==1 ))] <- NA
dta$decspace_man_involved_pl4 <-  rowSums(cbind((dta$grp4d23%in%c(1,3,5)  & dta$person_interviewed=="man") ,  (dta$spouse2group_sp4j23%in%c(1,3,5)  & dta$person_interviewed=="woman")), na.rm=T)
dta$decspace_man_involved_pl4[is.na((dta$grp4d23==1 ) ) & is.na((dta$spouse2group_sp4j23==1 ))] <- NA
##woman says it is both and man says it is both
dta$decspace_both_pl4 <- rowSums(cbind( (rowSums(cbind((dta$grp4d23==3  & dta$person_interviewed=="woman") , (dta$spouse2group_sp4j23==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp4d23==3  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j23==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decspace_both_pl4[is.na((dta$grp4d23==3 )) & is.na((dta$spouse2group_sp4j23==3 ))] <- NA

dta$decspace_man_both_pl4 <- rowSums(cbind( (rowSums(cbind((dta$grp4d23==2  & dta$person_interviewed=="woman") , (dta$spouse2group_sp4j23==1  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp4d23==1  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j23==2  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decspace_man_both_pl4[is.na((dta$grp4d23==3 )) & is.na((dta$spouse2group_sp4j23==3 ))] <- NA

dta$decspace_woman_both_pl4 <- rowSums(cbind( (rowSums(cbind((dta$grp4d23==1  & dta$person_interviewed=="woman") , (dta$spouse2group_sp4j23==2 & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp4d23==2  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j23==1  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decspace_woman_both_pl4[is.na((dta$grp4d23==3 )) & is.na((dta$spouse2group_sp4j23==3 ))] <- NA

dta$decspace_man_pl5 <-  rowSums(cbind((dta$grp5e23==1  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k23==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decspace_man_pl5[is.na( (dta$grp5e23==1 ) ) & is.na((dta$spouse2grp5_sp5k23==1 ) )] <- NA
dta$decspace_woman_man_pl5 <-  rowSums(cbind((dta$grp5e23==2  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k23==2 & dta$person_interviewed=="woman")), na.rm=T)
dta$decspace_woman_man_pl5[is.na( (dta$grp5e23==1 ) ) & is.na((dta$spouse2grp5_sp5k23==1 ) )] <- NA
dta$decspace_both_man_pl5 <-  rowSums(cbind((dta$grp5e23==3  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k23==3  & dta$person_interviewed=="woman")), na.rm=T)
dta$decspace_both_man_pl5[is.na( (dta$grp5e23==1 ) ) & is.na((dta$spouse2grp5_sp5k23==1 ) )] <- NA
dta$decspace_woman_pl5 <-  rowSums(cbind((dta$grp5e23==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5k23==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_woman_pl5[is.na((dta$grp5e23==1 ) ) & is.na((dta$spouse2grp5_sp5k23==1 ))] <- NA
dta$decspace_man_woman_pl5 <-  rowSums(cbind((dta$grp5e23==2  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5k23==2  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_man_woman_pl5[is.na((dta$grp5e23==1 ) ) & is.na((dta$spouse2grp5_sp5k23==1 ))] <- NA
dta$decspace_both_woman_pl5 <-  rowSums(cbind((dta$grp5e23==3  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5k23==3 & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_both_woman_pl5[is.na((dta$grp5e23==1 ) ) & is.na((dta$spouse2grp5_sp5k23==1 ))] <- NA
dta$decspace_woman_involved_pl5 <-  rowSums(cbind((dta$grp5e23%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5k23%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$decspace_woman_involved_pl5[is.na((dta$grp5e23==1 ) ) & is.na((dta$spouse2grp5_sp5k23==1 ))] <- NA
dta$decspace_man_involved_pl5 <-  rowSums(cbind((dta$grp5e23%in%c(1,3,5)  & dta$person_interviewed=="man") ,  (dta$spouse2grp5_sp5k23%in%c(1,3,5)  & dta$person_interviewed=="woman")), na.rm=T)
dta$decspace_man_involved_pl5[is.na((dta$grp5e23==1 ) ) & is.na((dta$spouse2grp5_sp5k23==1 ))] <- NA
##woman says it is both and man says it is both
dta$decspace_both_pl5 <- rowSums(cbind( (rowSums(cbind((dta$grp5e23==3  & dta$person_interviewed=="woman") , (dta$spouse2grp5_sp5k23==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp5e23==3  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k23==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decspace_both_pl5[is.na((dta$grp5e23==3 )) & is.na((dta$spouse2grp5_sp5k23==3 ))] <- NA

dta$decspace_man_both_pl5 <- rowSums(cbind( (rowSums(cbind((dta$grp5e23==2  & dta$person_interviewed=="woman") , (dta$spouse2grp5_sp5k23==1  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp5e23==1  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k23==2  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decspace_man_both_pl5[is.na((dta$grp5e23==3 )) & is.na((dta$spouse2grp5_sp5k23==3 ))] <- NA

dta$decspace_woman_both_pl5 <- rowSums(cbind( (rowSums(cbind((dta$grp5e23==1  & dta$person_interviewed=="woman") , (dta$spouse2grp5_sp5k23==2  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp5e23==2  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k23==1  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decspace_woman_both_pl5[is.na((dta$grp5e23==3 )) & is.na((dta$spouse2grp5_sp5k23==3 ))] <- NA

dta$nr_man_plots_decspace <- rowSums(dta[c("decspace_man_pl1","decspace_man_pl2","decspace_man_pl3","decspace_man_pl4","decspace_man_pl5")], na.rm=T)
dta$nr_woman_plots_decspace <- rowSums(dta[c("decspace_woman_pl1","decspace_woman_pl2","decspace_woman_pl3","decspace_woman_pl4","decspace_woman_pl5")], na.rm=T)
dta$nr_woman_involved_plots_decspace <- rowSums(dta[c("decspace_woman_involved_pl1","decspace_woman_involved_pl2","decspace_woman_involved_pl3","decspace_woman_involved_pl4","decspace_woman_involved_pl5")], na.rm=T)
dta$nr_joint_plots_decspace <- rowSums(dta[c("decspace_both_pl1","decspace_both_pl2","decspace_both_pl3","decspace_both_pl4","decspace_both_pl5")], na.rm=T)

dta$share_plots_both_decspace <- rowSums(dta[c("decspace_both_pl1","decspace_both_pl2","decspace_both_pl3","decspace_both_pl4","decspace_both_pl5")], na.rm=T)/rowSums(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))

dta$share_plots_man_both_decspace <- rowSums(dta[c("decspace_man_both_pl1","decspace_man_both_pl2","decspace_man_both_pl3","decspace_man_both_pl4","decspace_man_both_pl5")], na.rm=T)/rowSums(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))

dta$share_plots_woman_both_decspace <- rowSums(dta[c("decspace_woman_both_pl1","decspace_woman_both_pl2","decspace_woman_both_pl3","decspace_woman_both_pl4","decspace_woman_both_pl5")], na.rm=T)/rowSums(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))

dta$share_plots_man_decspace <- dta$nr_man_plots_decspace /rowSums(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))

dta$share_plots_woman_decspace <- dta$nr_woman_plots_decspace /rowSums(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))


############################

###who decides on striga

dta$decstriga_man_pl1 <-  rowSums(cbind((dta$grp1a25==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f25==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decstriga_man_pl1[is.na( (dta$grp1a25==1 ) ) & is.na((dta$spouse2grp_sp1f25==1 ) )] <- NA
dta$decstriga_woman_man_pl1 <-  rowSums(cbind((dta$grp1a25==2 & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f25==2  & dta$person_interviewed=="woman")), na.rm=T)
dta$decstriga_woman_man_pl1[is.na( (dta$grp1a25==1 ) ) & is.na((dta$spouse2grp_sp1f25==1 ) )] <- NA

dta$decstriga_both_man_pl1 <-  rowSums(cbind((dta$grp1a25==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f25==3  & dta$person_interviewed=="woman")), na.rm=T)
dta$decstriga_both_man_pl1[is.na( (dta$grp1a25==1 ) ) & is.na((dta$spouse2grp_sp1f25==1 ) )] <- NA
dta$decstriga_woman_pl1 <-  rowSums(cbind((dta$grp1a25==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1f25==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_woman_pl1[is.na((dta$grp1a25==1 ) ) & is.na((dta$spouse2grp_sp1f25==1 ))] <- NA
dta$decstriga_man_woman_pl1 <-  rowSums(cbind((dta$grp1a25==2  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1f25==2  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_man_woman_pl1[is.na((dta$grp1a25==1 ) ) & is.na((dta$spouse2grp_sp1f25==1 ))] <- NA
dta$decstriga_both_woman_pl1 <-  rowSums(cbind((dta$grp1a25==3  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1f25==3  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_both_woman_pl1[is.na((dta$grp1a25==1 ) ) & is.na((dta$spouse2grp_sp1f25==1 ))] <- NA
dta$decstriga_woman_involved_pl1 <- rowSums(cbind((dta$grp1a25%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1f25%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_woman_involved_pl1[is.na((dta$grp1a25==1 ) ) & is.na((dta$spouse2grp_sp1f25==1 ))] <- NA
dta$decstriga_man_involved_pl1 <- rowSums(cbind((dta$grp1a25%in%c(1,3,5)  & dta$person_interviewed=="man") ,  (dta$spouse2grp_sp1f25%in%c(1,3,5)  & dta$person_interviewed=="woman")), na.rm=T)
dta$decstriga_man_involved_pl1[is.na((dta$grp1a25==1 ) ) & is.na((dta$spouse2grp_sp1f25==1 ))] <- NA
##woman says it is both and man says it is both
dta$decstriga_both_pl1 <- rowSums(cbind( (rowSums(cbind((dta$grp1a25==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp1f25==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp1a25==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f25==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decstriga_both_pl1[is.na((dta$grp1a25==3 )) & is.na((dta$spouse2grp_sp1f25==3 ))] <- NA

dta$decstriga_man_both_pl1 <- rowSums(cbind( (rowSums(cbind((dta$grp1a25==2  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp1f25==1  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp1a25==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f25==2  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decstriga_man_both_pl1[is.na((dta$grp1a25==3 )) & is.na((dta$spouse2grp_sp1f25==3 ))] <- NA

dta$decstriga_woman_both_pl1 <- rowSums(cbind( (rowSums(cbind((dta$grp1a25==1  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp1f25==2  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp1a25==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f25==1  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decstriga_woman_both_pl1[is.na((dta$grp1a25==3 )) & is.na((dta$spouse2grp_sp1f25==3 ))] <- NA

dta$decstriga_man_pl2 <-  rowSums(cbind((dta$grp2b25==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g25==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decstriga_man_pl2[is.na( (dta$grp2b25==1 ) ) & is.na((dta$spouse2grp_sp2g25==1 ) )] <- NA
dta$decstriga_woman_man_pl2 <-  rowSums(cbind((dta$grp2b25==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g25==2  & dta$person_interviewed=="woman")), na.rm=T)
dta$decstriga_woman_man_pl2[is.na( (dta$grp2b25==1 ) ) & is.na((dta$spouse2grp_sp2g25==1 ) )] <- NA
dta$decstriga_both_man_pl2 <-  rowSums(cbind((dta$grp2b25==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g25==3  & dta$person_interviewed=="woman")), na.rm=T)
dta$decstriga_both_man_pl2[is.na( (dta$grp2b25==1 ) ) & is.na((dta$spouse2grp_sp2g25==1 ) )] <- NA
dta$decstriga_woman_pl2 <-  rowSums(cbind((dta$grp2b25==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2g25==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_woman_pl2[is.na((dta$grp2b25==1 ) ) & is.na((dta$spouse2grp_sp2g25==1 ))] <- NA
dta$decstriga_man_woman_pl2 <-  rowSums(cbind((dta$grp2b25==2  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2g25==2  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_man_woman_pl2[is.na((dta$grp2b25==1 ) ) & is.na((dta$spouse2grp_sp2g25==1 ))] <- NA
dta$decstriga_both_woman_pl2 <-  rowSums(cbind((dta$grp2b25==3  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2g25==3  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_both_woman_pl2[is.na((dta$grp2b25==1 ) ) & is.na((dta$spouse2grp_sp2g25==1 ))] <- NA
dta$decstriga_woman_involved_pl2 <-  rowSums(cbind((dta$grp2b25%in%c(1,3,5)   & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2g25%in%c(1,3,5)   & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_woman_involved_pl2[is.na((dta$grp2b25==1 ) ) & is.na((dta$spouse2grp_sp2g25==1 ))] <- NA
dta$decstriga_man_involved_pl2 <-  rowSums(cbind((dta$grp2b25%in%c(1,3,5)   & dta$person_interviewed=="man") ,  (dta$spouse2grp_sp2g25%in%c(1,3,5)   & dta$person_interviewed=="woman")), na.rm=T)
dta$decstriga_man_involved_pl2[is.na((dta$grp2b25==1 ) ) & is.na((dta$spouse2grp_sp2g25==1 ))] <- NA
##woman says it is both and man says it is both
dta$decstriga_both_pl2 <- rowSums(cbind( (rowSums(cbind((dta$grp2b25==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp2g25==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp2b25==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g25==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decstriga_both_pl2[is.na((dta$grp2b25==3 )) & is.na((dta$spouse2grp_sp2g25==3 ))] <- NA

dta$decstriga_man_both_pl2 <- rowSums(cbind( (rowSums(cbind((dta$grp2b25==2  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp2g25==1 & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp2b25==1 & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g25==2  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decstriga_man_both_pl2[is.na((dta$grp2b25==3 )) & is.na((dta$spouse2grp_sp2g25==3 ))] <- NA

dta$decstriga_woman_both_pl2 <- rowSums(cbind( (rowSums(cbind((dta$grp2b25==1  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp2g25==2  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp2b25==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g25==1  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decstriga_woman_both_pl2[is.na((dta$grp2b25==3 )) & is.na((dta$spouse2grp_sp2g25==3 ))] <- NA

dta$decstriga_man_pl3 <-  rowSums(cbind((dta$grp3c25==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h25==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decstriga_man_pl3[is.na( (dta$grp3c25==1 ) ) & is.na((dta$spouse2grp_sp3h25==1 ) )] <- NA
dta$decstriga_woman_man_pl3 <-  rowSums(cbind((dta$grp3c25==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h25==2  & dta$person_interviewed=="woman")), na.rm=T)
dta$decstriga_woman_man_pl3[is.na( (dta$grp3c25==1 ) ) & is.na((dta$spouse2grp_sp3h25==1 ) )] <- NA
dta$decstriga_both_man_pl3 <-  rowSums(cbind((dta$grp3c25==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h25==3  & dta$person_interviewed=="woman")), na.rm=T)
dta$decstriga_both_man_pl3[is.na( (dta$grp3c25==1 ) ) & is.na((dta$spouse2grp_sp3h25==1 ) )] <- NA
dta$decstriga_woman_pl3 <-  rowSums(cbind((dta$grp3c25==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3h25==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_woman_pl3[is.na((dta$grp3c25==1 ) ) & is.na((dta$spouse2grp_sp3h25==1 ))] <- NA
dta$decstriga_man_woman_pl3 <-  rowSums(cbind((dta$grp3c25==2  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3h25==2  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_man_woman_pl3[is.na((dta$grp3c25==1 ) ) & is.na((dta$spouse2grp_sp3h25==1 ))] <- NA
dta$decstriga_both_woman_pl3 <-  rowSums(cbind((dta$grp3c25==3  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3h25==3  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_both_woman_pl3[is.na((dta$grp3c25==1 ) ) & is.na((dta$spouse2grp_sp3h25==1 ))] <- NA
dta$decstriga_woman_involved_pl3 <-  rowSums(cbind((dta$grp3c25%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3h25%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_woman_involved_pl3[is.na((dta$grp3c25==1 ) ) & is.na((dta$spouse2grp_sp3h25==1 ))] <- NA
dta$decstriga_man_involved_pl3 <-  rowSums(cbind((dta$grp3c25%in%c(1,3,5)  & dta$person_interviewed=="man") ,  (dta$spouse2grp_sp3h25%in%c(1,3,5)  & dta$person_interviewed=="woman")), na.rm=T)
dta$decstriga_man_involved_pl3[is.na((dta$grp3c25==1 ) ) & is.na((dta$spouse2grp_sp3h25==1 ))] <- NA
##woman says it is both and man says it is both
dta$decstriga_both_pl3 <- rowSums(cbind( (rowSums(cbind((dta$grp3c25==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp3h25==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp3c25==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h25==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decstriga_both_pl3[is.na((dta$grp3c25==3 )) & is.na((dta$spouse2grp_sp3h25==3 ))] <- NA

dta$decstriga_man_both_pl3 <- rowSums(cbind( (rowSums(cbind((dta$grp3c25==2  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp3h25==1  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp3c25==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h25==2  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decstriga_man_both_pl3[is.na((dta$grp3c25==3 )) & is.na((dta$spouse2grp_sp3h25==3 ))] <- NA

dta$decstriga_woman_both_pl3 <- rowSums(cbind( (rowSums(cbind((dta$grp3c25==1  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp3h25==2  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp3c25==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h25==1  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decstriga_woman_both_pl3[is.na((dta$grp3c25==3 )) & is.na((dta$spouse2grp_sp3h25==3 ))] <- NA

dta$decstriga_man_pl4 <-  rowSums(cbind((dta$grp4d25==1  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j25==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decstriga_man_pl4[is.na( (dta$grp4d25==1 ) ) & is.na((dta$spouse2group_sp4j25==1 ) )] <- NA
dta$decstriga_woman_man_pl4 <-  rowSums(cbind((dta$grp4d25==2  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j25==2  & dta$person_interviewed=="woman")), na.rm=T)
dta$decstriga_woman_man_pl4[is.na( (dta$grp4d25==1 ) ) & is.na((dta$spouse2group_sp4j25==1 ) )] <- NA
dta$decstriga_both_man_pl4 <-  rowSums(cbind((dta$grp4d25==3  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j25==3  & dta$person_interviewed=="woman")), na.rm=T)
dta$decstriga_both_man_pl4[is.na( (dta$grp4d25==1 ) ) & is.na((dta$spouse2group_sp4j25==1 ) )] <- NA
dta$decstriga_woman_pl4 <-  rowSums(cbind((dta$grp4d25==1  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4j25==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_woman_pl4[is.na((dta$grp4d25==1 ) ) & is.na((dta$spouse2group_sp4j25==1 ))] <- NA
dta$decstriga_man_woman_pl4 <-  rowSums(cbind((dta$grp4d25==2  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4j25==2  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_man_woman_pl4[is.na((dta$grp4d25==1 ) ) & is.na((dta$spouse2group_sp4j25==1 ))] <- NA
dta$decstriga_both_woman_pl4 <-  rowSums(cbind((dta$grp4d25==3  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4j25==3  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_both_woman_pl4[is.na((dta$grp4d25==1 ) ) & is.na((dta$spouse2group_sp4j25==1 ))] <- NA
dta$decstriga_woman_involved_pl4 <-  rowSums(cbind((dta$grp4d25%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4j25%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_woman_involved_pl4[is.na((dta$grp4d25==1 ) ) & is.na((dta$spouse2group_sp4j25==1 ))] <- NA
dta$decstriga_man_involved_pl4 <-  rowSums(cbind((dta$grp4d25%in%c(1,3,5)  & dta$person_interviewed=="man") ,  (dta$spouse2group_sp4j25%in%c(1,3,5)  & dta$person_interviewed=="woman")), na.rm=T)
dta$decstriga_man_involved_pl4[is.na((dta$grp4d25==1 ) ) & is.na((dta$spouse2group_sp4j25==1 ))] <- NA
##woman says it is both and man says it is both
dta$decstriga_both_pl4 <- rowSums(cbind( (rowSums(cbind((dta$grp4d25==3  & dta$person_interviewed=="woman") , (dta$spouse2group_sp4j25==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp4d25==3  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j25==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decstriga_both_pl4[is.na((dta$grp4d25==3 )) & is.na((dta$spouse2group_sp4j25==3 ))] <- NA

dta$decstriga_man_both_pl4 <- rowSums(cbind( (rowSums(cbind((dta$grp4d25==2  & dta$person_interviewed=="woman") , (dta$spouse2group_sp4j25==1  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp4d25==1  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j25==2 & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decstriga_man_both_pl4[is.na((dta$grp4d25==3 )) & is.na((dta$spouse2group_sp4j25==3 ))] <- NA

dta$decstriga_woman_both_pl4 <- rowSums(cbind( (rowSums(cbind((dta$grp4d25==1  & dta$person_interviewed=="woman") , (dta$spouse2group_sp4j25==2  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp4d25==2  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j25==1 & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decstriga_woman_both_pl4[is.na((dta$grp4d25==3 )) & is.na((dta$spouse2group_sp4j25==3 ))] <- NA

dta$decstriga_man_pl5 <-  rowSums(cbind((dta$grp5e25==1  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k25==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decstriga_man_pl5[is.na( (dta$grp5e25==1 ) ) & is.na((dta$spouse2grp5_sp5k25==1 ) )] <- NA
dta$decstriga_woman_man_pl5 <-  rowSums(cbind((dta$grp5e25==2  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k25==2  & dta$person_interviewed=="woman")), na.rm=T)
dta$decstriga_woman_man_pl5[is.na( (dta$grp5e25==1 ) ) & is.na((dta$spouse2grp5_sp5k25==1 ) )] <- NA
dta$decstriga_both_man_pl5 <-  rowSums(cbind((dta$grp5e25==3  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k25==3  & dta$person_interviewed=="woman")), na.rm=T)
dta$decstriga_both_man_pl5[is.na( (dta$grp5e25==1 ) ) & is.na((dta$spouse2grp5_sp5k25==1 ) )] <- NA
dta$decstriga_woman_pl5 <-  rowSums(cbind((dta$grp5e25==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5k25==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_woman_pl5[is.na((dta$grp5e25==1 ) ) & is.na((dta$spouse2grp5_sp5k25==1 ))] <- NA
dta$decstriga_man_woman_pl5 <-  rowSums(cbind((dta$grp5e25==2  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5k25==2  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_man_woman_pl5[is.na((dta$grp5e25==1 ) ) & is.na((dta$spouse2grp5_sp5k25==1 ))] <- NA
dta$decstriga_both_woman_pl5 <-  rowSums(cbind((dta$grp5e25==3  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5k25==3 & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_both_woman_pl5[is.na((dta$grp5e25==1 ) ) & is.na((dta$spouse2grp5_sp5k25==1 ))] <- NA
dta$decstriga_woman_involved_pl5 <-  rowSums(cbind((dta$grp5e25%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5k25%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$decstriga_woman_involved_pl5[is.na((dta$grp5e25==1 ) ) & is.na((dta$spouse2grp5_sp5k25==1 ))] <- NA
dta$decstriga_man_involved_pl5 <-  rowSums(cbind((dta$grp5e25%in%c(1,3,5)  & dta$person_interviewed=="man") ,  (dta$spouse2grp5_sp5k25%in%c(1,3,5)  & dta$person_interviewed=="woman")), na.rm=T)
dta$decstriga_man_involved_pl5[is.na((dta$grp5e25==1 ) ) & is.na((dta$spouse2grp5_sp5k25==1 ))] <- NA
##woman says it is both and man says it is both
dta$decstriga_both_pl5 <- rowSums(cbind( (rowSums(cbind((dta$grp5e25==3  & dta$person_interviewed=="woman") , (dta$spouse2grp5_sp5k25==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp5e25==3  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k25==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decstriga_both_pl5[is.na((dta$grp5e25==3 )) & is.na((dta$spouse2grp5_sp5k25==3 ))] <- NA

dta$decstriga_man_both_pl5 <- rowSums(cbind( (rowSums(cbind((dta$grp5e25==2  & dta$person_interviewed=="woman") , (dta$spouse2grp5_sp5k25==1  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp5e25==1  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k25==2  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decstriga_man_both_pl5[is.na((dta$grp5e25==3 )) & is.na((dta$spouse2grp5_sp5k25==3 ))] <- NA

dta$decstriga_woman_both_pl5 <- rowSums(cbind( (rowSums(cbind((dta$grp5e25==1 & dta$person_interviewed=="woman") , (dta$spouse2grp5_sp5k25==2  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp5e25==2  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k25==1 & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decstriga_woman_both_pl5[is.na((dta$grp5e25==3 )) & is.na((dta$spouse2grp5_sp5k25==3 ))] <- NA

dta$nr_man_plots_decstriga <- rowSums(dta[c("decstriga_man_pl1","decstriga_man_pl2","decstriga_man_pl3","decstriga_man_pl4","decstriga_man_pl5")], na.rm=T)
dta$share_man_plots_decstriga <- rowSums(dta[c("decstriga_man_pl1","decstriga_man_pl2","decstriga_man_pl3","decstriga_man_pl4","decstriga_man_pl5")], na.rm=T)
dta$nr_woman_plots_decstriga <- rowSums(dta[c("decstriga_woman_pl1","decstriga_woman_pl2","decstriga_woman_pl3","decstriga_woman_pl4","decstriga_woman_pl5")], na.rm=T)
dta$nr_woman_involved_plots_decstriga <- rowSums(dta[c("decstriga_woman_involved_pl1","decstriga_woman_involved_pl2","decstriga_woman_involved_pl3","decstriga_woman_involved_pl4","decstriga_woman_involved_pl5")], na.rm=T)
dta$nr_joint_plots_decstriga <- rowSums(dta[c("decstriga_both_pl1","decstriga_both_pl2","decstriga_both_pl3","decstriga_both_pl4","decstriga_both_pl5")], na.rm=T)

dta$share_plots_both_decstriga <- rowSums(dta[c("decstriga_both_pl1","decstriga_both_pl2","decstriga_both_pl3","decstriga_both_pl4","decstriga_both_pl5")], na.rm=T)/
rowSums( 
!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]) *(dta[c("grp1a243","grp2b243","grp3c243","grp4d243","grp5e243")]*dta[c("spouse2grp_sp1f243","spouse2grp_sp2g243","spouse2grp_sp3h243","spouse2group_sp4j243","spouse2grp_sp5k243")]!=1)
, na.rm=T)


dta$share_plots_man_both_decstriga <- rowSums(dta[c("decstriga_man_both_pl1","decstriga_man_both_pl2","decstriga_man_both_pl3","decstriga_man_both_pl4","decstriga_man_both_pl5")], na.rm=T)/rowSums(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))

dta$share_plots_woman_both_decstriga <- rowSums(dta[c("decstriga_woman_both_pl1","decstriga_woman_both_pl2","decstriga_woman_both_pl3","decstriga_woman_both_pl4","decstriga_woman_both_pl5")], na.rm=T)/rowSums(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))



### I need to select only plots where fertilizer was used, but as reported by the man
## replace first indicators of fert use by NA if woman was first interviewed
dta_cpy <- dta
dta_cpy[c("grp1a243","grp2b243","grp3c243","grp4d243","grp5e243")] <- lapply(dta_cpy[c("grp1a243","grp2b243","grp3c243","grp4d243","grp5e243")], function(x) replace(x, dta_cpy$person_interviewed!="man", NA) )
## replace second indicators of fert use by NA if man was first interviewed
#dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp_sp5k42")] <- lapply(dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp_sp5k42")], function(x) replace(x, dta$person_interviewed!="woman", NA) 
#replace missings in first indicator (which means that womand was first interviewed) by second indicator of fert use 
dta_cpy[c("grp1a243","grp2b243","grp3c243","grp4d243","grp5e243")][is.na(dta_cpy[c("grp1a243","grp2b243","grp3c243","grp4d243","grp5e243")])]  <- dta_cpy[c("spouse2grp_sp1f243","spouse2grp_sp2g243","spouse2grp_sp3h243","spouse2group_sp4j243","spouse2grp_sp5k243")][is.na(dta_cpy[c("grp1a243","grp2b243","grp3c243","grp4d243","grp5e243")])] 

dta$share_plots_man_decstriga <- dta$nr_man_plots_decstriga/ rowSums( (!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))*(dta_cpy[c("grp1a243","grp2b243","grp3c243","grp4d243","grp5e243")]!=1), na.rm=T)
dta$share_plots_man_decstriga[is.infinite(dta$share_plots_man_decstriga)] <- NA




##now for women
## I need to select only plots where fertilizer was used, but as reported by the man
## replace first indicators of fert use by NA if woman was first interviewed
dta_cpy <- dta
#dta_cpy[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")] <- lapply(dta_cpy[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")], function(x) replace(x, dta_cpy$person_interviewed!="man", NA) )
## replace second indicators of fert use by NA if man was first interviewed
dta_cpy[c("spouse2grp_sp1f243","spouse2grp_sp2g243","spouse2grp_sp3h243","spouse2group_sp4j243","spouse2grp_sp5k243")] <- lapply(dta_cpy[c("spouse2grp_sp1f243","spouse2grp_sp2g243","spouse2grp_sp3h243","spouse2group_sp4j243","spouse2grp_sp5k243")], function(x) replace(x, dta$person_interviewed!="woman", NA) )
#replace missings in first indicator (which means that womand was first interviewed) by second indicator of fert use 
dta_cpy[c("spouse2grp_sp1f243","spouse2grp_sp2g243","spouse2grp_sp3h243","spouse2group_sp4j243","spouse2grp_sp5k243")][is.na(dta_cpy[c("spouse2grp_sp1f243","spouse2grp_sp2g243","spouse2grp_sp3h243","spouse2group_sp4j243","spouse2grp_sp5k243")])]  <- dta_cpy[c("grp1a243","grp2b243","grp3c243","grp4d243","grp5e243")][is.na(dta_cpy[c("spouse2grp_sp1f243","spouse2grp_sp2g243","spouse2grp_sp3h243","spouse2group_sp4j243","spouse2grp_sp5k243")])] 

dta$share_plots_woman_decstriga <- dta$nr_woman_plots_decstriga / rowSums( 
(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))*(dta_cpy[c("spouse2grp_sp1f243","spouse2grp_sp2g243","spouse2grp_sp3h243","spouse2group_sp4j243","spouse2grp_sp5k243")]!=1), na.rm=T)
dta$share_plots_woman_decstriga[is.infinite(dta$share_plots_woman_decstriga)] <- NA



##########################################

###who decides on weed

dta$decweed_man_pl1 <-  rowSums(cbind((dta$grp1a27==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f27==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decweed_man_pl1[is.na( (dta$grp1a27==1 ) ) & is.na((dta$spouse2grp_sp1f27==1 ) )] <- NA
dta$decweed_woman_man_pl1 <-  rowSums(cbind((dta$grp1a27==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f27==2  & dta$person_interviewed=="woman")), na.rm=T)
dta$decweed_woman_man_pl1[is.na( (dta$grp1a27==1 ) ) & is.na((dta$spouse2grp_sp1f27==1 ) )] <- NA
dta$decweed_both_man_pl1 <-  rowSums(cbind((dta$grp1a27==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f27==3  & dta$person_interviewed=="woman")), na.rm=T)
dta$decweed_both_man_pl1[is.na( (dta$grp1a27==1 ) ) & is.na((dta$spouse2grp_sp1f27==1 ) )] <- NA
dta$decweed_woman_pl1 <-  rowSums(cbind((dta$grp1a27==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1f27==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_woman_pl1[is.na((dta$grp1a27==1 ) ) & is.na((dta$spouse2grp_sp1f27==1 ))] <- NA
dta$decweed_man_woman_pl1 <-  rowSums(cbind((dta$grp1a27==2  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1f27==2  & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_man_woman_pl1[is.na((dta$grp1a27==1 ) ) & is.na((dta$spouse2grp_sp1f27==1 ))] <- NA
dta$decweed_both_woman_pl1 <-  rowSums(cbind((dta$grp1a27==3  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1f27==3  & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_both_woman_pl1[is.na((dta$grp1a27==1 ) ) & is.na((dta$spouse2grp_sp1f27==1 ))] <- NA
dta$decweed_woman_involved_pl1 <- rowSums(cbind((dta$grp1a27%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp1f27%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_woman_involved_pl1[is.na((dta$grp1a27==1 ) ) & is.na((dta$spouse2grp_sp1f27==1 ))] <- NA
dta$decweed_man_involved_pl1 <- rowSums(cbind((dta$grp1a27%in%c(1,3,5)  & dta$person_interviewed=="man") ,  (dta$spouse2grp_sp1f27%in%c(1,3,5)  & dta$person_interviewed=="woman")), na.rm=T)
dta$decweed_man_involved_pl1[is.na((dta$grp1a27==1 ) ) & is.na((dta$spouse2grp_sp1f27==1 ))] <- NA
##woman says it is both and man says it is both
dta$decweed_both_pl1 <- rowSums(cbind( (rowSums(cbind((dta$grp1a27==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp1f27==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp1a27==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f27==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decweed_both_pl1[is.na((dta$grp1a27==3 )) & is.na((dta$spouse2grp_sp1f27==3 ))] <- NA

dta$decweed_man_both_pl1 <- rowSums(cbind( (rowSums(cbind((dta$grp1a27==2  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp1f27==1  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp1a27==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f27==2  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decweed_man_both_pl1[is.na((dta$grp1a27==3 )) & is.na((dta$spouse2grp_sp1f27==3 ))] <- NA

dta$decweed_woman_both_pl1 <- rowSums(cbind( (rowSums(cbind((dta$grp1a27==1  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp1f27==2  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp1a27==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp1f27==1  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decweed_woman_both_pl1[is.na((dta$grp1a27==3 )) & is.na((dta$spouse2grp_sp1f27==3 ))] <- NA

dta$decweed_man_pl2 <-  rowSums(cbind((dta$grp2b27==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g27==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decweed_man_pl2[is.na( (dta$grp2b27==1 ) ) & is.na((dta$spouse2grp_sp2g27==1 ) )] <- NA
dta$decweed_woman_man_pl2 <-  rowSums(cbind((dta$grp2b27==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g27==2  & dta$person_interviewed=="woman")), na.rm=T)
dta$decweed_woman_man_pl2[is.na( (dta$grp2b27==1 ) ) & is.na((dta$spouse2grp_sp2g27==1 ) )] <- NA
dta$decweed_both_man_pl2 <-  rowSums(cbind((dta$grp2b27==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g27==3  & dta$person_interviewed=="woman")), na.rm=T)
dta$decweed_both_man_pl2[is.na( (dta$grp2b27==1 ) ) & is.na((dta$spouse2grp_sp2g27==1 ) )] <- NA
dta$decweed_woman_pl2 <-  rowSums(cbind((dta$grp2b27==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2g27==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_woman_pl2[is.na((dta$grp2b27==1 ) ) & is.na((dta$spouse2grp_sp2g27==1 ))] <- NA
dta$decweed_man_woman_pl2 <-  rowSums(cbind((dta$grp2b27==2  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2g27==2  & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_man_woman_pl2[is.na((dta$grp2b27==1 ) ) & is.na((dta$spouse2grp_sp2g27==1 ))] <- NA
dta$decweed_both_woman_pl2 <-  rowSums(cbind((dta$grp2b27==3  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2g27==3  & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_both_woman_pl2[is.na((dta$grp2b27==1 ) ) & is.na((dta$spouse2grp_sp2g27==1 ))] <- NA
dta$decweed_woman_involved_pl2 <-  rowSums(cbind((dta$grp2b27%in%c(1,3,5)   & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp2g27%in%c(1,3,5)   & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_woman_involved_pl2[is.na((dta$grp2b27==1 ) ) & is.na((dta$spouse2grp_sp2g27==1 ))] <- NA
dta$decweed_man_involved_pl2 <-  rowSums(cbind((dta$grp2b27%in%c(1,3,5)   & dta$person_interviewed=="man") ,  (dta$spouse2grp_sp2g27%in%c(1,3,5)   & dta$person_interviewed=="woman")), na.rm=T)
dta$decweed_man_involved_pl2[is.na((dta$grp2b27==1 ) ) & is.na((dta$spouse2grp_sp2g27==1 ))] <- NA
##woman says it is both and man says it is both
dta$decweed_both_pl2 <- rowSums(cbind( (rowSums(cbind((dta$grp2b27==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp2g27==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp2b27==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g27==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decweed_both_pl2[is.na((dta$grp2b27==3 )) & is.na((dta$spouse2grp_sp2g27==3 ))] <- NA

dta$decweed_man_both_pl2 <- rowSums(cbind( (rowSums(cbind((dta$grp2b27==2  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp2g27==1  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp2b27==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g27==2  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decweed_man_both_pl2[is.na((dta$grp2b27==3 )) & is.na((dta$spouse2grp_sp2g27==3 ))] <- NA

dta$decweed_woman_both_pl2 <- rowSums(cbind( (rowSums(cbind((dta$grp2b27==1 & dta$person_interviewed=="woman") , (dta$spouse2grp_sp2g27==2  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp2b27==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp2g27==1  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decweed_woman_both_pl2[is.na((dta$grp2b27==3 )) & is.na((dta$spouse2grp_sp2g27==3 ))] <- NA

dta$decweed_man_pl3 <-  rowSums(cbind((dta$grp3c27==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h27==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decweed_man_pl3[is.na( (dta$grp3c27==1 ) ) & is.na((dta$spouse2grp_sp3h27==1 ) )] <- NA
dta$decweed_woman_man_pl3 <-  rowSums(cbind((dta$grp3c27==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h27==2  & dta$person_interviewed=="woman")), na.rm=T)
dta$decweed_woman_man_pl3[is.na( (dta$grp3c27==1 ) ) & is.na((dta$spouse2grp_sp3h27==1 ) )] <- NA
dta$decweed_both_man_pl3 <-  rowSums(cbind((dta$grp3c27==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h27==3  & dta$person_interviewed=="woman")), na.rm=T)
dta$decweed_both_man_pl3[is.na( (dta$grp3c27==1 ) ) & is.na((dta$spouse2grp_sp3h27==1 ) )] <- NA
dta$decweed_woman_pl3 <-  rowSums(cbind((dta$grp3c27==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3h27==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_woman_pl3[is.na((dta$grp3c27==1 ) ) & is.na((dta$spouse2grp_sp3h27==1 ))] <- NA
dta$decweed_man_woman_pl3 <-  rowSums(cbind((dta$grp3c27==2  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3h27==2 & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_man_woman_pl3[is.na((dta$grp3c27==1 ) ) & is.na((dta$spouse2grp_sp3h27==1 ))] <- NA
dta$decweed_both_woman_pl3 <-  rowSums(cbind((dta$grp3c27==3  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3h27==3  & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_both_woman_pl3[is.na((dta$grp3c27==1 ) ) & is.na((dta$spouse2grp_sp3h27==1 ))] <- NA
dta$decweed_woman_involved_pl3 <-  rowSums(cbind((dta$grp3c27%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp_sp3h27%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_woman_involved_pl3[is.na((dta$grp3c27==1 ) ) & is.na((dta$spouse2grp_sp3h27==1 ))] <- NA
dta$decweed_man_involved_pl3 <-  rowSums(cbind((dta$grp3c27%in%c(1,3,5)  & dta$person_interviewed=="man") ,  (dta$spouse2grp_sp3h27%in%c(1,3,5)  & dta$person_interviewed=="woman")), na.rm=T)
dta$decweed_man_involved_pl3[is.na((dta$grp3c27==1 ) ) & is.na((dta$spouse2grp_sp3h27==1 ))] <- NA
##woman says it is both and man says it is both
dta$decweed_both_pl3 <- rowSums(cbind( (rowSums(cbind((dta$grp3c27==3  & dta$person_interviewed=="woman") , (dta$spouse2grp_sp3h27==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp3c27==3  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h27==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decweed_both_pl3[is.na((dta$grp3c27==3 )) & is.na((dta$spouse2grp_sp3h27==3 ))] <- NA

dta$decweed_man_both_pl3 <- rowSums(cbind( (rowSums(cbind((dta$grp3c27==2 & dta$person_interviewed=="woman") , (dta$spouse2grp_sp3h27==1  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp3c27==1  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h27==2  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decweed_man_both_pl3[is.na((dta$grp3c27==3 )) & is.na((dta$spouse2grp_sp3h27==3 ))] <- NA

dta$decweed_woman_both_pl3 <- rowSums(cbind( (rowSums(cbind((dta$grp3c27==1 & dta$person_interviewed=="woman") , (dta$spouse2grp_sp3h27==2  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp3c27==2  & dta$person_interviewed=="man") , (dta$spouse2grp_sp3h27==1  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decweed_woman_both_pl3[is.na((dta$grp3c27==3 )) & is.na((dta$spouse2grp_sp3h27==3 ))] <- NA

dta$decweed_man_pl4 <-  rowSums(cbind((dta$grp4d27==1  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j27==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decweed_man_pl4[is.na( (dta$grp4d27==1 ) ) & is.na((dta$spouse2group_sp4j27==1 ) )] <- NA
dta$decweed_woman_man_pl4 <-  rowSums(cbind((dta$grp4d27==2  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j27==2  & dta$person_interviewed=="woman")), na.rm=T)
dta$decweed_woman_man_pl4[is.na( (dta$grp4d27==1 ) ) & is.na((dta$spouse2group_sp4j27==1 ) )] <- NA

dta$decweed_both_man_pl4 <-  rowSums(cbind((dta$grp4d27==3  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j27==3  & dta$person_interviewed=="woman")), na.rm=T)
dta$decweed_both_man_pl4[is.na( (dta$grp4d27==1 ) ) & is.na((dta$spouse2group_sp4j27==1 ) )] <- NA
dta$decweed_woman_pl4 <-  rowSums(cbind((dta$grp4d27==1  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4j27==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_woman_pl4[is.na((dta$grp4d27==1) ) & is.na((dta$spouse2group_sp4j27==1  ))] <- NA
dta$decweed_man_woman_pl4 <-  rowSums(cbind((dta$grp4d27==2  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4j27==2 & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_man_woman_pl4[is.na((dta$grp4d27==1) ) & is.na((dta$spouse2group_sp4j27==1  ))] <- NA
dta$decweed_both_woman_pl4 <-  rowSums(cbind((dta$grp4d27==3  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4j27==3  & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_both_woman_pl4[is.na((dta$grp4d27==1) ) & is.na((dta$spouse2group_sp4j27==1  ))] <- NA

dta$decweed_woman_involved_pl4 <-  rowSums(cbind((dta$grp4d27%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2group_sp4j27%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_woman_involved_pl4[is.na((dta$grp4d27==1 ) ) & is.na((dta$spouse2group_sp4j27==1 ))] <- NA
dta$decweed_man_involved_pl4 <-  rowSums(cbind((dta$grp4d27%in%c(1,3,5)  & dta$person_interviewed=="man") ,  (dta$spouse2group_sp4j27%in%c(1,3,5)  & dta$person_interviewed=="woman")), na.rm=T)
dta$decweed_man_involved_pl4[is.na((dta$grp4d27==1 ) ) & is.na((dta$spouse2group_sp4j27==1 ))] <- NA
##woman says it is both and man says it is both
dta$decweed_both_pl4 <- rowSums(cbind( (rowSums(cbind((dta$grp4d27==3  & dta$person_interviewed=="woman") , (dta$spouse2group_sp4j27==3 )), na.rm=T) > 0),(rowSums(cbind((dta$grp4d27==3  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j27==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decweed_both_pl4[is.na((dta$grp4d27==3 )) & is.na((dta$spouse2group_sp4j27==3))] <- NA

dta$decweed_man_both_pl4 <- rowSums(cbind( (rowSums(cbind((dta$grp4d27==2  & dta$person_interviewed=="woman") , (dta$spouse2group_sp4j27==1 )), na.rm=T) > 0),(rowSums(cbind((dta$grp4d27==1  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j27==2  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decweed_man_both_pl4[is.na((dta$grp4d27==3 )) & is.na((dta$spouse2group_sp4j27==3))] <- NA

dta$decweed_woman_both_pl4 <- rowSums(cbind( (rowSums(cbind((dta$grp4d27==1  & dta$person_interviewed=="woman") , (dta$spouse2group_sp4j27==2 )), na.rm=T) > 0),(rowSums(cbind((dta$grp4d27==2  & dta$person_interviewed=="man") , (dta$spouse2group_sp4j27==1  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decweed_woman_both_pl4[is.na((dta$grp4d27==3 )) & is.na((dta$spouse2group_sp4j27==3))] <- NA

dta$decweed_man_pl5 <-  rowSums(cbind((dta$grp5e27==1  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k27==1  & dta$person_interviewed=="woman")), na.rm=T)
dta$decweed_man_pl5[is.na( (dta$grp5e27==1 ) ) & is.na((dta$spouse2grp5_sp5k27==1) )] <- NA
dta$decweed_woman_man_pl5 <-  rowSums(cbind((dta$grp5e27==2  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k27==2 & dta$person_interviewed=="woman")), na.rm=T)
dta$decweed_woman_man_pl5[is.na( (dta$grp5e27==1 ) ) & is.na((dta$spouse2grp5_sp5k27==1) )] <- NA
dta$decweed_both_man_pl5 <-  rowSums(cbind((dta$grp5e27==3  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k27==3  & dta$person_interviewed=="woman")), na.rm=T)
dta$decweed_both_man_pl5[is.na( (dta$grp5e27==1 ) ) & is.na((dta$spouse2grp5_sp5k27==1) )] <- NA
dta$decweed_woman_pl5 <-  rowSums(cbind((dta$grp5e27==1  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5k27==1  & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_woman_pl5[is.na((dta$grp5e27==1 ) ) & is.na((dta$spouse2grp5_sp5k27==1 ))] <- NA
dta$decweed_man_woman_pl5 <-  rowSums(cbind((dta$grp5e27==2  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5k27==2 & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_man_woman_pl5[is.na((dta$grp5e27==1 ) ) & is.na((dta$spouse2grp5_sp5k27==1 ))] <- NA
dta$decweed_both_woman_pl5 <-  rowSums(cbind((dta$grp5e27==3  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5k27==3  & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_both_woman_pl5[is.na((dta$grp5e27==1 ) ) & is.na((dta$spouse2grp5_sp5k27==1 ))] <- NA
dta$decweed_woman_involved_pl5 <-  rowSums(cbind((dta$grp5e27%in%c(1,3,5)  & dta$person_interviewed=="woman") ,  (dta$spouse2grp5_sp5k27%in%c(1,3,5)  & dta$person_interviewed=="man")), na.rm=T)
dta$decweed_woman_involved_pl5[is.na((dta$grp5e27==1 ) ) & is.na((dta$spouse2grp5_sp5k27==1 ))] <- NA
dta$decweed_man_involved_pl5 <-  rowSums(cbind((dta$grp5e27%in%c(1,3,5)  & dta$person_interviewed=="man") ,  (dta$spouse2grp5_sp5k27%in%c(1,3,5)  & dta$person_interviewed=="woman")), na.rm=T)
dta$decweed_man_involved_pl5[is.na((dta$grp5e27==1 ) ) & is.na((dta$spouse2grp5_sp5k27==1 ))] <- NA
##woman says it is both and man says it is both
dta$decweed_both_pl5 <- rowSums(cbind( (rowSums(cbind((dta$grp5e27==3  & dta$person_interviewed=="woman") , (dta$spouse2grp5_sp5k27==3  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp5e27==3  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k27==3  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decweed_both_pl5[is.na((dta$grp5e27==3)) & is.na((dta$spouse2grp5_sp5k27==3))] <- NA

dta$decweed_man_both_pl5 <- rowSums(cbind( (rowSums(cbind((dta$grp5e27==2  & dta$person_interviewed=="woman") , (dta$spouse2grp5_sp5k27==1  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp5e27==1  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k27==2 & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decweed_man_both_pl5[is.na((dta$grp5e27==3)) & is.na((dta$spouse2grp5_sp5k27==3))] <- NA

dta$decweed_woman_both_pl5 <- rowSums(cbind( (rowSums(cbind((dta$grp5e27==1  & dta$person_interviewed=="woman") , (dta$spouse2grp5_sp5k27==2  & dta$person_interviewed=="man")), na.rm=T) > 0),(rowSums(cbind((dta$grp5e27==2  & dta$person_interviewed=="man") , (dta$spouse2grp5_sp5k27==1  & dta$person_interviewed=="woman")), na.rm=T) > 0)), na.rm=T)==2
dta$decweed_woman_both_pl5[is.na((dta$grp5e27==3)) & is.na((dta$spouse2grp5_sp5k27==3))] <- NA

dta$nr_man_plots_decweed <- rowSums(dta[c("decweed_man_pl1","decweed_man_pl2","decweed_man_pl3","decweed_man_pl4","decweed_man_pl5")], na.rm=T)
dta$nr_woman_plots_decweed <- rowSums(dta[c("decweed_woman_pl1","decweed_woman_pl2","decweed_woman_pl3","decweed_woman_pl4","decweed_woman_pl5")], na.rm=T)
dta$nr_woman_involved_plots_decweed <- rowSums(dta[c("decweed_woman_involved_pl1","decweed_woman_involved_pl2","decweed_woman_involved_pl3","decweed_woman_involved_pl4","decweed_woman_involved_pl5")], na.rm=T)
dta$nr_joint_plots_decweed <- rowSums(dta[c("decweed_both_pl1","decweed_both_pl2","decweed_both_pl3","decweed_both_pl4","decweed_both_pl5")], na.rm=T)

dta$share_plots_both_decweed <- rowSums(dta[c("decweed_both_pl1","decweed_both_pl2","decweed_both_pl3","decweed_both_pl4","decweed_both_pl5")], na.rm=T)/rowSums(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))

dta$share_plots_man_both_decweed <- rowSums(dta[c("decweed_man_both_pl1","decweed_man_both_pl2","decweed_man_both_pl3","decweed_man_both_pl4","decweed_man_both_pl5")], na.rm=T)/rowSums(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))

dta$share_plots_woman_both_decweed <- rowSums(dta[c("decweed_woman_both_pl1","decweed_woman_both_pl2","decweed_woman_both_pl3","decweed_woman_both_pl4","decweed_woman_both_pl5")], na.rm=T)/rowSums(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))


dta$share_plots_man_decweed <- dta$nr_man_plots_decweed /rowSums(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))

dta$share_plots_woman_decweed <-  dta$nr_woman_plots_decweed/rowSums(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))
### aggregate decsion making based on practices
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

dta$dec_man_involved_pl1 <- rowMeans(dta[c("mgt_man_involved_pl1", "dectime_man_involved_pl1", "decspace_man_involved_pl1", "decstriga_man_involved_pl1", "decweed_man_involved_pl1")],na.rm=T)
dta$dec_man_involved_pl2 <- rowMeans(dta[c("mgt_man_involved_pl2", "dectime_man_involved_pl2", "decspace_man_involved_pl2", "decstriga_man_involved_pl2", "decweed_man_involved_pl2")],na.rm=T)
dta$dec_man_involved_pl3 <- rowMeans(dta[c("mgt_man_involved_pl3", "dectime_man_involved_pl3", "decspace_man_involved_pl3", "decstriga_man_involved_pl3", "decweed_man_involved_pl3")],na.rm=T)
dta$dec_man_involved_pl4 <- rowMeans(dta[c("mgt_man_involved_pl4", "dectime_man_involved_pl4", "decspace_man_involved_pl4", "decstriga_man_involved_pl4", "decweed_man_involved_pl4")],na.rm=T)
dta$dec_man_involved_pl5 <- rowMeans(dta[c("mgt_man_involved_pl5", "dectime_man_involved_pl5", "decspace_man_involved_pl5", "decstriga_man_involved_pl5", "decweed_man_involved_pl5")],na.rm=T)


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

### as dummy - all decisions are made by man, woman, both

dta[paste("dec_man_d",paste("_pl",1:5, sep=""), sep="")] <- dta[paste("dec_man",paste("_pl",1:5, sep=""), sep="")] > 0.5
dta[paste("dec_woman_d",paste("_pl",1:5, sep=""), sep="")] <- dta[paste("dec_woman",paste("_pl",1:5, sep=""), sep="")] > 0.5
dta[paste("dec_both_d",paste("_pl",1:5, sep=""), sep="")] <- dta[paste("dec_both",paste("_pl",1:5, sep=""), sep="")] > 0.5

#who decides on fertilizer

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

dta$share_plots_both_decfert <- rowSums(dta[c("decfert_both_pl1","decfert_both_pl2","decfert_both_pl3","decfert_both_pl4","decfert_both_pl5")], na.rm=T)/ rowSums( 
(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))*((dta[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")]=="Yes")*(dta[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp_sp5k29")]=="Yes") )
, na.rm=T)


### I need to select only plots where fertilizer was used, but as reported by the man
## replace first indicators of fert use by NA if woman was first interviewed
dta_cpy <- dta
dta_cpy[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")] <- lapply(dta_cpy[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")], function(x) replace(x, dta_cpy$person_interviewed!="man", NA) )
## replace second indicators of fert use by NA if man was first interviewed
#dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp_sp5k42")] <- lapply(dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp_sp5k42")], function(x) replace(x, dta$person_interviewed!="woman", NA) 
#replace missings in first indicator (which means that womand was first interviewed) by second indicator of fert use 
dta_cpy[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")][is.na(dta_cpy[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")])]  <- dta_cpy[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp_sp5k29")][is.na(dta_cpy[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")])] 

dta$share_plots_man_decfert <- rowSums(dta[c("decfert_man_pl1","decfert_man_pl2","decfert_man_pl3","decfert_man_pl4","decfert_man_pl5")], na.rm=T)/ rowSums( 
(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))*(dta_cpy[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")]=="Yes"), na.rm=T)

##now for women
## I need to select only plots where fertilizer was used, but as reported by the man
## replace first indicators of fert use by NA if woman was first interviewed
dta_cpy <- dta
#dta_cpy[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")] <- lapply(dta_cpy[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")], function(x) replace(x, dta_cpy$person_interviewed!="man", NA) )
## replace second indicators of fert use by NA if man was first interviewed
dta_cpy[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp_sp5k29")] <- lapply(dta_cpy[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp_sp5k29")], function(x) replace(x, dta$person_interviewed!="woman", NA) )
#replace missings in first indicator (which means that womand was first interviewed) by second indicator of fert use 
dta_cpy[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp_sp5k29")][is.na(dta_cpy[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp_sp5k29")])]  <- dta_cpy[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")][is.na(dta_cpy[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp_sp5k29")])] 

dta$share_plots_woman_decfert <- rowSums(dta[c("decfert_woman_pl1","decfert_woman_pl2","decfert_woman_pl3","decfert_woman_pl4","decfert_woman_pl5")], na.rm=T)/ rowSums( 
(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))*(dta_cpy[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp_sp5k29")]=="Yes"), na.rm=T)
dta$share_plots_woman_decfert[is.infinite(dta$share_plots_woman_decfert)] <- NA


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

dta$share_plots_both_decseed <- rowSums(dta[c("decseed_both_pl1","decseed_both_pl2","decseed_both_pl3","decseed_both_pl4","decseed_both_pl5")], na.rm=T)/ rowSums( 
(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))*((dta[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")]=="Yes")*(dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp_sp5k42")]=="Yes") )
, na.rm=T)


### I need to select only plots where fertilizer was used, but as reported by the man
## replace first indicators of seed use by NA if woman was first interviewed
dta_cpy <- dta
dta_cpy[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")] <- lapply(dta_cpy[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")], function(x) replace(x, dta_cpy$person_interviewed!="man", NA) )
## replace second indicators of seed use by NA if man was first interviewed
#dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp_sp5k42")] <- lapply(dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp_sp5k42")], function(x) replace(x, dta$person_interviewed!="woman", NA) 
#replace missings in first indicator (which means that womand was first interviewed) by second indicator of seed use 
dta_cpy[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")][is.na(dta_cpy[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")])]  <- dta_cpy[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp_sp5k42")][is.na(dta_cpy[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")])] 

dta$share_plots_man_decseed <- rowSums(dta[c("decseed_man_pl1","decseed_man_pl2","decseed_man_pl3","decseed_man_pl4","decseed_man_pl5")], na.rm=T)/ rowSums( 
(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))*(dta_cpy[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")]=="Yes"), na.rm=T)

##now for women
## I need to select only plots where fertilizer was used, but as reported by the man
## replace first indicators of seed use by NA if woman was first interviewed
dta_cpy <- dta
#dta_cpy[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")] <- lapply(dta_cpy[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")], function(x) replace(x, dta_cpy$person_interviewed!="man", NA) )
## replace second indicators of seed use by NA if man was first interviewed
dta_cpy[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp_sp5k42")] <- lapply(dta_cpy[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp_sp5k42")], function(x) replace(x, dta$person_interviewed!="woman", NA) )
#replace missings in first indicator (which means that womand was first interviewed) by second indicator of seed use 
dta_cpy[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp_sp5k42")][is.na(dta_cpy[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp_sp5k42")])]  <- dta_cpy[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")][is.na(dta_cpy[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp_sp5k42")])] 

dta$share_plots_woman_decseed <- rowSums(dta[c("decseed_woman_pl1","decseed_woman_pl2","decseed_woman_pl3","decseed_woman_pl4","decseed_woman_pl5")], na.rm=T)/ rowSums( 
(!is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))*(dta_cpy[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp_sp5k42")]=="Yes"), na.rm=T)
dta$share_plots_woman_decseed[is.infinite(dta$share_plots_woman_decseed)] <- NA
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

inter <- (dta[c("grp1days1","grp2days2","grp3days3","grp4days4", "grp5days5")] +dta[c("spouse2grp_sp1days1" ,"spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5")])/2
dta$days_after_rain <- rowMeans(inter, na.rm=T)


### use recommended spacing on *any* plot
dta$space_sp1 <- rowSums(dta[c("grp1a201","grp2b201","grp3c201","grp4d201","grp5e201")], na.rm=T) > 0
dta$space_sp1[is.na(dta$grp1a201) & is.na(dta$grp2b201) & is.na(dta$grp3c201) & is.na(dta$grp4d201) & is.na(dta$grp5e201)] <- NA

dta$space_sp2 <- rowSums(dta[c("spouse2grp_sp1f201","spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201","spouse2grp5_sp5k201")], na.rm=T) >0 
dta$space_sp2[is.na(dta$spouse2grp_sp1f201) & is.na(dta$spouse2grp_sp2g201) & is.na(dta$spouse2grp_sp3h201) & is.na(dta$spouse2group_sp4j201) & is.na(dta$spouse2grp5_sp5k201)] <- NA 

dta$space <- rowSums(dta[c("space_sp1","space_sp2")], na.rm=T) > 0
dta$space[is.na(dta$space_sp1) & is.na(dta$space_sp2)] <- NA

dta$space_both <- rowSums(dta[c("space_sp1","space_sp2")], na.rm=T) == 2
dta$space_both[is.na(dta$space_sp1) & is.na(dta$space_sp2)] <- NA

#recommended way to fight striga

dta$striga_sp1 <- rowSums(dta[c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241")], na.rm=T) > 0
dta$striga_sp1[is.na(dta$grp1a241) & is.na(dta$grp2b241) & is.na(dta$grp3c241) & is.na(dta$grp4d241) & is.na(dta$grp5e241)] <- NA
dta$striga_sp2 <-  rowSums(dta[c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241")], na.rm=T) >0 
dta$striga_sp2[is.na(dta$spouse2grp_sp1f241) & is.na(dta$spouse2grp_sp2g241) & is.na(dta$spouse2grp_sp3h241) & is.na(dta$spouse2group_sp4j241) & is.na(dta$spouse2grp5_sp5k241)] <- NA
dta$striga <- rowSums(dta[c("striga_sp1","striga_sp2")], na.rm=T) > 0
dta$striga[is.na(dta$striga_sp1) & is.na(dta$striga_sp2)] <- NA

dta$striga_both <- rowSums(dta[c("striga_sp1","striga_sp2")], na.rm=T) == 2
dta$striga_both[is.na(dta$striga_sp1) & is.na(dta$striga_sp2)] <- NA

### use recommended weeding practice: we recommend first weeding after 18-20 days, which is in the 3rd week - option 3
dta$weed_sp1 <- rowSums(dta[c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26")] == 3, na.rm=T) > 0
dta$weed_sp1[is.na(dta$grp1a26) & is.na(dta$grp2b26) & is.na(dta$grp3c26) & is.na(dta$grp4d26) & is.na(dta$grp5e26)] <- NA
dta$weed_sp2 <- rowSums(dta[c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26")] == 3  ,na.rm=T) >0 
dta$weed_sp2[is.na(dta$spouse2grp_sp1f26) & is.na(dta$spouse2grp_sp2g26) & is.na(dta$spouse2grp_sp3h26) & is.na(dta$spouse2group_sp4j26) & is.na(dta$spouse2grp5_sp5k26)] <- NA
dta$weed <- rowSums(dta[c("weed_sp1","weed_sp2")], na.rm=T) > 0
dta$weed[is.na(dta$weed_sp1) & is.na(dta$weed_sp2)] <- NA

dta$weed_both <- rowSums(dta[c("weed_sp1","weed_sp2")], na.rm=T) == 2
dta$weed_both[is.na(dta$weed_sp1) & is.na(dta$weed_sp2)] <- NA

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

############################################################## PRACTICES - HH level ##############################################################

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



dta[ paste("yield_sp1",paste("_pl",1:5, sep=""), sep="")] <- dta[c("prod_pl1_sp1","prod_pl2_sp1","prod_pl3_sp1","prod_pl4_sp1", "prod_pl5_sp1")]/dta[c("area_pl1_sp1","area_pl2_sp1","area_pl3_sp1","area_pl4_sp1", "area_pl5_sp1")]
dta[ paste("yield_sp2",paste("_pl",1:5, sep=""), sep="")] <- dta[c("prod_pl1_sp2","prod_pl2_sp2","prod_pl3_sp2","prod_pl4_sp2", "prod_pl5_sp2")]/dta[c("area_pl1_sp2","area_pl2_sp2","area_pl3_sp2","area_pl4_sp2", "area_pl5_sp2")]

### should we also look at individual fertilizer categories, or at least differentiate between organic and inorganic fertilizer?
### dap/npk == 1
dta$fert_dap_sp1 <- rowSums( dta[c("grp1a301","grp2b301","grp3c301","grp4d301","grp5e301")],na.rm=T  ) > 0
#only for observations we have data for the question if they use
dta$fert_dap_sp1[is.na(dta$grp1a29) & is.na(dta$grp2b29) & is.na(dta$grp3c29) & is.na(dta$grp4d29) & is.na(dta$grp5e29)] <- NA 
dta$fert_dap_sp2 <- rowSums( dta[c("spouse2grp_sp1f301","spouse2grp_sp2g301","spouse2grp_sp3h301","spouse2group_sp4j301","spouse2grp5_sp5k301")],na.rm=T) > 0
dta$fert_dap_sp2[is.na(dta$spouse2grp_sp1f29) & is.na(dta$spouse2grp_sp2g29) & is.na(dta$spouse2grp_sp3h29) & is.na(dta$spouse2group_sp4j29) & is.na(dta$spouse2grp5_sp5k29)] <- NA
dta$fert_dap <- rowSums(dta[c("fert_dap_sp1","fert_dap_sp2")], na.rm=T) > 0
dta$fert_dap[is.na(dta$fert_dap_sp1) & is.na(dta$fert_dap_sp2)] <- NA

dta$fert_dap_both <- rowSums(dta[c("fert_dap_sp1","fert_dap_sp2")], na.rm=T) ==2
dta$fert_dap_both[is.na(dta$fert_dap_sp1) & is.na(dta$fert_dap_sp2)] <- NA

### should we also look at individual fertilizer categories, or at least differentiate between organic and inorganic fertilizer?
### urea == 2
dta$fert_urea_sp1 <- rowSums(dta[c("grp1a302","grp2b302","grp3c302","grp4d302","grp5e302")], na.rm=T) > 0
dta$fert_urea_sp1[is.na(dta$grp1a29) & is.na(dta$grp2b29) & is.na(dta$grp3c29) & is.na(dta$grp4d29) & is.na(dta$grp5e29)] <- NA 
dta$fert_urea_sp2 <- rowSums(dta[c("spouse2grp_sp1f302","spouse2grp_sp2g302","spouse2grp_sp3h302","spouse2group_sp4j302","spouse2grp5_sp5k302")], na.rm=T  ) > 0
dta$fert_urea_sp2[is.na(dta$spouse2grp_sp1f29) & is.na(dta$spouse2grp_sp2g29) & is.na(dta$spouse2grp_sp3h29) & is.na(dta$spouse2group_sp4j29) & is.na(dta$spouse2grp5_sp5k29)] <- NA

dta$fert_urea <- rowSums(dta[c("fert_urea_sp1","fert_urea_sp2")], na.rm=T) > 0
dta$fert_urea[is.na(dta$fert_urea_sp1) & is.na(dta$fert_urea_sp2)] <- NA

dta$fert_urea_both <- rowSums(dta[c("fert_urea_sp1","fert_urea_sp2")], na.rm=T) ==2
dta$fert_urea_both[is.na(dta$fert_urea_sp1) & is.na(dta$fert_urea_sp2)] <- NA


### organic == 3
dta$fert_org_sp1 <- rowSums(dta[c("grp1a303","grp2b303","grp3c303","grp4d303","grp5e303")],na.rm=T) > 0
dta$fert_org_sp1[is.na(dta$grp1a29) & is.na(dta$grp2b29) & is.na(dta$grp3c29) & is.na(dta$grp4d29) & is.na(dta$grp5e29)] <- NA 
dta$fert_org_sp2 <- rowSums(dta[c("spouse2grp_sp1f303","spouse2grp_sp2g303","spouse2grp_sp3h303","spouse2group_sp4j303","spouse2grp5_sp5k303")], na.rm=T) > 0
dta$fert_org_sp2[is.na(dta$spouse2grp_sp1f29) & is.na(dta$spouse2grp_sp2g29) & is.na(dta$spouse2grp_sp3h29) & is.na(dta$spouse2group_sp4j29) & is.na(dta$spouse2grp5_sp5k29)] <- NA
dta$fert_org <- rowSums(dta[c("fert_org_sp1","fert_org_sp2")], na.rm=T) > 0
dta$fert_org[is.na(dta$fert_org_sp1) & is.na(dta$fert_org_sp2)] <- NA

dta$fert_org_both <- rowSums(dta[c("fert_org_sp1","fert_org_sp2")], na.rm=T) == 2
dta$fert_org_both[is.na(dta$fert_org_sp1) & is.na(dta$fert_org_sp2)] <- NA


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

### aggregate - how do we get a hh level measure of time spent?
### if both men and women are interviewed, we use self reported
for (act in c("prep","plant","weed1","weed2","weed3","spray","harv")) {
dta[paste(paste("time",act,sep="_"),"woman",sep="_")] <- NA
dta[paste(paste("time",act,sep="_"),"man",sep="_")] <- NA

dta[paste(paste("time",act,sep="_"),"woman",sep="_")] <- ifelse(dta$person_interviewed=="man", ifelse(dta$interview_status=="couple interviewed",
rowSums(dta[c(paste(paste("time", act,sep="_"),"pl1_sp2_self", sep="_"),paste(paste("time", act,sep="_"),"pl2_sp2_self", sep="_"),paste(paste("time", act,sep="_"),"pl3_sp2_self", sep="_"),paste(paste("time", act,sep="_"),"pl4_sp2_self", sep="_"), paste(paste("time", act,sep="_"),"pl5_sp2_self", sep="_")) ], na.rm=T)*
ifelse(rowSums(is.na(dta[c(paste(paste("time", act,sep="_"),"pl1_sp2_self", sep="_"),paste(paste("time", act,sep="_"),"pl2_sp2_self", sep="_"),paste(paste("time", act,sep="_"),"pl3_sp2_self", sep="_"),paste(paste("time", act,sep="_"),"pl4_sp2_self", sep="_"), paste(paste("time", act,sep="_"),"pl5_sp2_self", sep="_")) ])) == ncol(dta[c(paste(paste("time", act,sep="_"),"pl1_sp2_self", sep="_"),paste(paste("time", act,sep="_"),"pl2_sp2_self", sep="_"),paste(paste("time", act,sep="_"),"pl3_sp2_self", sep="_"),paste(paste("time", act,sep="_"),"pl4_sp2_self", sep="_"), paste(paste("time", act,sep="_"),"pl5_sp2_self", sep="_")) ]), NA, 1)
,
rowSums(dta[c(paste(paste("time", act,sep="_"),"pl1_sp2_other", sep="_"),paste(paste("time", act,sep="_"),"pl2_sp2_other", sep="_"),paste(paste("time", act,sep="_"),"pl3_sp2_other", sep="_"),paste(paste("time", act,sep="_"),"pl4_sp2_other", sep="_"), paste(paste("time", act,sep="_"),"pl5_sp2_other", sep="_")) ], na.rm=T)*
ifelse(rowSums(is.na(dta[c(paste(paste("time", act,sep="_"),"pl1_sp2_other", sep="_"),paste(paste("time", act,sep="_"),"pl2_sp2_other", sep="_"),paste(paste("time", act,sep="_"),"pl3_sp2_other", sep="_"),paste(paste("time", act,sep="_"),"pl4_sp2_other", sep="_"), paste(paste("time", act,sep="_"),"pl5_sp2_other", sep="_")) ])) == ncol(dta[c(paste(paste("time", act,sep="_"),"pl1_sp2_other", sep="_"),paste(paste("time", act,sep="_"),"pl2_sp2_other", sep="_"),paste(paste("time", act,sep="_"),"pl3_sp2_other", sep="_"),paste(paste("time", act,sep="_"),"pl4_sp2_other", sep="_"), paste(paste("time", act,sep="_"),"pl5_sp2_other", sep="_")) ]), NA, 1))
, 
rowSums(dta[c(paste(paste("time", act,sep="_"),"pl1_sp1_self", sep="_"),paste(paste("time", act,sep="_"),"pl2_sp1_self", sep="_"),paste(paste("time", act,sep="_"),"pl3_sp1_self", sep="_"),paste(paste("time", act,sep="_"),"pl4_sp1_self", sep="_"), paste(paste("time", act,sep="_"),"pl5_sp1_self", sep="_")) ], na.rm=T)*
ifelse(rowSums(is.na(dta[c(paste(paste("time", act,sep="_"),"pl1_sp1_self", sep="_"),paste(paste("time", act,sep="_"),"pl2_sp1_self", sep="_"),paste(paste("time", act,sep="_"),"pl3_sp1_self", sep="_"),paste(paste("time", act,sep="_"),"pl4_sp1_self", sep="_"), paste(paste("time", act,sep="_"),"pl5_sp1_self", sep="_")) ])) == ncol(dta[c(paste(paste("time", act,sep="_"),"pl1_sp1_self", sep="_"),paste(paste("time", act,sep="_"),"pl2_sp1_self", sep="_"),paste(paste("time", act,sep="_"),"pl3_sp1_self", sep="_"),paste(paste("time", act,sep="_"),"pl4_sp1_self", sep="_"), paste(paste("time", act,sep="_"),"pl5_sp1_self", sep="_")) ]), NA, 1))

dta[paste(paste("time",act,sep="_"),"man",sep="_")] <- ifelse(dta$person_interviewed=="woman", ifelse(dta$interview_status=="couple interviewed",
rowSums(dta[c(paste(paste("time", act,sep="_"),"pl1_sp2_self", sep="_"),paste(paste("time", act,sep="_"),"pl2_sp2_self", sep="_"),paste(paste("time", act,sep="_"),"pl3_sp2_self", sep="_"),paste(paste("time", act,sep="_"),"pl4_sp2_self", sep="_"), paste(paste("time", act,sep="_"),"pl5_sp2_self", sep="_")) ], na.rm=T)*
ifelse(rowSums(is.na(dta[c(paste(paste("time", act,sep="_"),"pl1_sp2_self", sep="_"),paste(paste("time", act,sep="_"),"pl2_sp2_self", sep="_"),paste(paste("time", act,sep="_"),"pl3_sp2_self", sep="_"),paste(paste("time", act,sep="_"),"pl4_sp2_self", sep="_"), paste(paste("time", act,sep="_"),"pl5_sp2_self", sep="_")) ])) == ncol(dta[c(paste(paste("time", act,sep="_"),"pl1_sp2_self", sep="_"),paste(paste("time", act,sep="_"),"pl2_sp2_self", sep="_"),paste(paste("time", act,sep="_"),"pl3_sp2_self", sep="_"),paste(paste("time", act,sep="_"),"pl4_sp2_self", sep="_"), paste(paste("time", act,sep="_"),"pl5_sp2_self", sep="_")) ]), NA, 1)
,
rowSums(dta[c(paste(paste("time", act,sep="_"),"pl1_sp2_other", sep="_"),paste(paste("time", act,sep="_"),"pl2_sp2_other", sep="_"),paste(paste("time", act,sep="_"),"pl3_sp2_other", sep="_"),paste(paste("time", act,sep="_"),"pl4_sp2_other", sep="_"), paste(paste("time", act,sep="_"),"pl5_sp2_other", sep="_")) ], na.rm=T)*
ifelse(rowSums(is.na(dta[c(paste(paste("time", act,sep="_"),"pl1_sp2_other", sep="_"),paste(paste("time", act,sep="_"),"pl2_sp2_other", sep="_"),paste(paste("time", act,sep="_"),"pl3_sp2_other", sep="_"),paste(paste("time", act,sep="_"),"pl4_sp2_other", sep="_"), paste(paste("time", act,sep="_"),"pl5_sp2_other", sep="_")) ])) == ncol(dta[c(paste(paste("time", act,sep="_"),"pl1_sp2_other", sep="_"),paste(paste("time", act,sep="_"),"pl2_sp2_other", sep="_"),paste(paste("time", act,sep="_"),"pl3_sp2_other", sep="_"),paste(paste("time", act,sep="_"),"pl4_sp2_other", sep="_"), paste(paste("time", act,sep="_"),"pl5_sp2_other", sep="_")) ]), NA, 1))
, 
rowSums(dta[c(paste(paste("time", act,sep="_"),"pl1_sp1_self", sep="_"),paste(paste("time", act,sep="_"),"pl2_sp1_self", sep="_"),paste(paste("time", act,sep="_"),"pl3_sp1_self", sep="_"),paste(paste("time", act,sep="_"),"pl4_sp1_self", sep="_"), paste(paste("time", act,sep="_"),"pl5_sp1_self", sep="_")) ], na.rm=T)*
ifelse(rowSums(is.na(dta[c(paste(paste("time", act,sep="_"),"pl1_sp1_self", sep="_"),paste(paste("time", act,sep="_"),"pl2_sp1_self", sep="_"),paste(paste("time", act,sep="_"),"pl3_sp1_self", sep="_"),paste(paste("time", act,sep="_"),"pl4_sp1_self", sep="_"), paste(paste("time", act,sep="_"),"pl5_sp1_self", sep="_")) ])) == ncol(dta[c(paste(paste("time", act,sep="_"),"pl1_sp1_self", sep="_"),paste(paste("time", act,sep="_"),"pl2_sp1_self", sep="_"),paste(paste("time", act,sep="_"),"pl3_sp1_self", sep="_"),paste(paste("time", act,sep="_"),"pl4_sp1_self", sep="_"), paste(paste("time", act,sep="_"),"pl5_sp1_self", sep="_")) ]), NA, 1))
}

dta$tot_time_man <-  rowSums(dta[c("time_prep_man","time_plant_man","time_weed1_man","time_weed2_man","time_weed3_man","time_spray_man","time_harv_man")], na.rm=T)* ifelse(rowSums(is.na(dta[c("time_prep_man","time_plant_man","time_weed1_man","time_weed2_man","time_weed3_man","time_spray_man","time_harv_man")])) == ncol(dta[c("time_prep_man","time_plant_man","time_weed1_man","time_weed2_man","time_weed3_man","time_spray_man","time_harv_man")]), NA, 1)
dta$tot_time_woman <-  rowSums(dta[c("time_prep_woman","time_plant_woman","time_weed1_woman","time_weed2_woman","time_weed3_woman","time_spray_woman","time_harv_woman")], na.rm=T)* ifelse(rowSums(is.na(dta[c("time_prep_woman","time_plant_woman","time_weed1_woman","time_weed2_woman","time_weed3_woman","time_spray_woman","time_harv_woman")])) == ncol(dta[c("time_prep_woman","time_plant_woman","time_weed1_woman","time_weed2_woman","time_weed3_woman","time_spray_woman","time_harv_woman")]), NA, 1)
dta$tot_own_time_hh <- rowSums(dta[c("tot_time_man", "tot_time_woman")], na.rm=T)* ifelse(rowSums(is.na(dta[c("tot_time_man", "tot_time_woman")])) == ncol(dta[c("tot_time_man", "tot_time_woman")]), NA, 1)

dta$timedif_prep <- dta$time_prep_man - dta$time_plant_woman
dta$timedif_plant <- dta$time_plant_man - dta$time_plant_woman
dta$timedif_weed_man  <- dta$time_weed1_man + dta$time_weed2_man + dta$time_weed3_man - (dta$time_weed1_woman + dta$time_weed2_woman + dta$time_weed3_woman)
dta$timedif_spray_man <- dta$time_spray_man - dta$time_spray_woman
dta$timedif_harv_man <- dta$time_harv_man -dta$time_harv_woman
dta$totdif_time <- dta$tot_time_man - dta$tot_time_woman
## hired in labour
dta[c("grp1a152", "grp2b152", "grp3c152", "grp4d152", "grp5e152")] <- lapply(dta[c("grp1a152", "grp2b152", "grp3c152", "grp4d152", "grp5e152")], function(x) replace(x, x == 999, NA) )
dta[c("spouse2grp_sp1f152", "spouse2grp_sp2g152", "spouse2grp_sp3h152", "spouse2group_sp4j152", "spouse2grp5_sp5k152")] <- lapply(dta[c("spouse2grp_sp1f152", "spouse2grp_sp2g152", "spouse2grp_sp3h152", "spouse2group_sp4j152", "spouse2grp5_sp5k152")], function(x) replace(x, x == 999, NA) )

dta$time_hired_sp1 <- rowSums(dta[c("grp1a152", "grp2b152", "grp3c152", "grp4d152", "grp5e152")],na.rm=T)
dta$time_hired_sp2 <- rowSums(dta[c("spouse2grp_sp1f152", "spouse2grp_sp2g152", "spouse2grp_sp3h152", "spouse2group_sp4j152", "spouse2grp5_sp5k152")], na.rm=T)

dta$time_hired_hh <- ifelse(dta$interview_status=="couple interviewed", (dta$time_hired_sp1 + dta$time_hired_sp2)/2,dta$time_hired_sp1 )
dta$tot_time_hh <-  rowSums(dta[c("time_hired_hh", "tot_own_time_hh")], na.rm=T) 

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

### calculated consumption expenditure
dta[c("maize_value", "sorghum_value", "millet_value", "rice_value", "cassava_value", "sweetpotatoes_value", "beans_value", "gnuts_value", "fruits_value", "veg_value", "sugar_value", "cooking_oil_value", "soap_value", "airtime_value")] <- 
lapply(dta[c("maize_value", "sorghum_value", "millet_value", "rice_value", "cassava_value", "sweetpotatoes_value", "beans_value", "gnuts_value", "fruits_value", "veg_value", "sugar_value", "cooking_oil_value", "soap_value", "airtime_value")], function(x) replace(x, x == 999, NA) )

dta$cons_sp1 <- rowSums(dta[c("maize_value", "sorghum_value", "millet_value", "rice_value", "cassava_value", "sweetpotatoes_value", "beans_value", "gnuts_value", "fruits_value", "veg_value", "sugar_value", "cooking_oil_value", "soap_value", "airtime_value")], na.rm=T)

dta[c("spouse2maize_value_sp", "spouse2sorghum_value_sp", "spouse2millet_value_sp", "spouse2rice_value_sp", "spouse2cassava_value_sp", "spouse2sweetpotatoes_value_sp", "spouse2beans_value_sp", "spouse2gnuts_value_sp", "spouse2fruits_value_sp", "spouse2veg_value_sp", "spouse2sugar_value_sp", "spouse2cooking_oil_value_sp", "spouse2soap_value_sp", "spouse2airtime_value_sp")] <- 
lapply(dta[c("spouse2maize_value_sp", "spouse2sorghum_value_sp", "spouse2millet_value_sp", "spouse2rice_value_sp", "spouse2cassava_value_sp", "spouse2sweetpotatoes_value_sp", "spouse2beans_value_sp", "spouse2gnuts_value_sp", "spouse2fruits_value_sp", "spouse2veg_value_sp", "spouse2sugar_value_sp", "spouse2cooking_oil_value_sp", "spouse2soap_value_sp", "spouse2airtime_value_sp")], function(x) replace(x, x == 999, NA) )

dta$cons_sp2 <- rowSums(dta[c("spouse2maize_value_sp", "spouse2sorghum_value_sp", "spouse2millet_value_sp", "spouse2rice_value_sp", "spouse2cassava_value_sp", "spouse2sweetpotatoes_value_sp", "spouse2beans_value_sp", "spouse2gnuts_value_sp", "spouse2fruits_value_sp", "spouse2veg_value_sp", "spouse2sugar_value_sp", "spouse2cooking_oil_value_sp", "spouse2soap_value_sp", "spouse2airtime_value_sp")], na.rm=T)

dta$cons <- dta$cons_sp1+dta$cons_sp2
dta$cons[dta$cons == 0] <- NA

dta$cons_maize_yes <- rowSums(cbind(dta$maize_cons=="Yes", dta$spouse2maize_sp=="Yes"), na.rm=T) >0
dta$cons_maize_yes[is.na(dta$maize_cons) & is.na(dta$spouse2maize_sp) ] <- NA

dta$cons_maize_val <- rowSums(dta[c("maize_value","spouse2maize_value_sp")], na.rm=T)
dta$cons_maize_val[is.na(dta$maize_cons) & is.na(dta$spouse2maize_sp) ] <- NA

#############sales
## has sold 
dta$sold_woman <- ifelse(dta$gender1=="woman", dta$q72>0,dta$spouse2r72>0)
dta$sold_man_woman <- ifelse(dta$gender1=="woman", dta$q75>0,dta$spouse2r75>0)
dta$sold_both_woman <- ifelse(dta$gender1=="woman", dta$q78>0,dta$spouse2r78>0)


#sold by you alone q72 and R72 - this is in bags of about 100 kgs

##as reported by woman
dta$nr_bags_sold_woman <- ifelse(dta$gender1=="woman", dta$q72,dta$spouse2r72)
dta$nr_bags_sold_woman[dta$nr_bags_sold_woman > 97] <- NA
dta$nr_bags_sold_man_woman <- ifelse(dta$gender1=="woman", dta$q75,dta$spouse2r75)
dta$nr_bags_sold_man_woman[dta$nr_bags_sold_man_woman > 97] <- NA
dta$nr_bags_sold_both_woman <- ifelse(dta$gender1=="woman", dta$q78,dta$spouse2r78)
dta$nr_bags_sold_both_woman[dta$nr_bags_sold_both_woman >97] <- NA
dta$nr_bags_sold_other_woman <- ifelse(dta$gender1=="woman", dta$q81,as.numeric(as.character(dta$spouse2r81)))
dta$nr_bags_sold_other_woman[dta$nr_bags_sold_other_woman >97] <- NA

dta$share_sold_woman_woman <- dta$nr_bags_sold_woman / rowSums(dta[c("nr_bags_sold_woman","nr_bags_sold_man_woman","nr_bags_sold_both_woman","nr_bags_sold_other_woman")])
dta$share_sold_man_woman <- dta$nr_bags_sold_man_woman / rowSums(dta[c("nr_bags_sold_woman","nr_bags_sold_man_woman","nr_bags_sold_both_woman","nr_bags_sold_other_woman")])
dta$share_sold_both_woman <- dta$nr_bags_sold_both_woman / rowSums(dta[c("nr_bags_sold_woman","nr_bags_sold_man_woman","nr_bags_sold_both_woman","nr_bags_sold_other_woman")])
#as reported by man
dta$nr_bags_sold_man <- ifelse(dta$gender1=="man", dta$q72,dta$spouse2r72)
dta$nr_bags_sold_man[dta$nr_bags_sold_man > 97] <- NA
dta$nr_bags_sold_woman_man <- ifelse(dta$gender1=="man", dta$q75,dta$spouse2r75)
dta$nr_bags_sold_woman_man[dta$nr_bags_sold_woman_man > 97] <- NA
dta$nr_bags_sold_both_man <- ifelse(dta$gender1=="man", dta$q78,dta$spouse2r78)
dta$nr_bags_sold_both_man[dta$nr_bags_sold_both_man >97] <- NA
dta$nr_bags_sold_other_man <- ifelse(dta$gender1=="man", dta$q81,as.numeric(as.character(dta$spouse2r81)))
dta$nr_bags_sold_other_man[dta$nr_bags_sold_other_man >97] <- NA

dta$share_sold_man_man <- dta$nr_bags_sold_man / rowSums(dta[c("nr_bags_sold_man","nr_bags_sold_woman_man","nr_bags_sold_both_man","nr_bags_sold_other_man")])
dta$share_sold_woman_man <- dta$nr_bags_sold_woman_man / rowSums(dta[c("nr_bags_sold_man","nr_bags_sold_woman_man","nr_bags_sold_both_man","nr_bags_sold_other_man")])
dta$share_sold_both_man <- dta$nr_bags_sold_both_man / rowSums(dta[c("nr_bags_sold_man","nr_bags_sold_woman_man","nr_bags_sold_both_man","nr_bags_sold_other_man")])


#average shares

dta$share_sold_man_both <- ((dta$nr_bags_sold_man + dta$nr_bags_sold_man_woman)/2) / ((rowSums(dta[c("nr_bags_sold_man","nr_bags_sold_woman_man","nr_bags_sold_both_man","nr_bags_sold_other_man")]) + rowSums(dta[c("nr_bags_sold_woman","nr_bags_sold_man_woman","nr_bags_sold_both_woman","nr_bags_sold_other_woman")]))/2)

dta$share_sold_woman_both <-  ((dta$nr_bags_sold_woman_man + dta$nr_bags_sold_woman)/2) / ((rowSums(dta[c("nr_bags_sold_man","nr_bags_sold_woman_man","nr_bags_sold_both_man","nr_bags_sold_other_man")]) + rowSums(dta[c("nr_bags_sold_woman","nr_bags_sold_man_woman","nr_bags_sold_both_woman","nr_bags_sold_other_woman")]))/2)

dta$share_sold_both_both <- ((dta$nr_bags_sold_both_man + dta$nr_bags_sold_both_woman)/2) / ((rowSums(dta[c("nr_bags_sold_man","nr_bags_sold_woman_man","nr_bags_sold_both_man","nr_bags_sold_other_man")]) + rowSums(dta[c("nr_bags_sold_woman","nr_bags_sold_man_woman","nr_bags_sold_both_woman","nr_bags_sold_other_woman")]))/2)
### price sold

dta$price_sold_woman <- ifelse(dta$gender1=="woman", dta$q73/dta$q72,dta$spouse2r73/dta$spouse2r72)
dta$price_sold_woman[dta$price_sold_woman < 10000] <- NA
dta$price_sold_man_woman <- ifelse(dta$gender1=="woman", dta$q76/dta$q75,dta$spouse2r76/dta$spouse2r75)
dta$price_sold_man_woman[dta$price_sold_woman < 10000] <- NA
dta$price_sold_both_woman <- ifelse(dta$gender1=="woman", dta$q79/dta$q78,dta$spouse2r79/dta$spouse2r78)
dta$price_sold_both_woman[dta$price_sold_both_woman <10000] <- NA

##as reported by woman
dta$income_woman <- ifelse(dta$gender1=="woman", dta$q73,dta$spouse2r73)
dta$income_woman[is.na(dta$income_woman)] <- 0
dta$income_woman[dta$income_woman ==  999] <- NA
dta$income_man_woman <- ifelse(dta$gender1=="woman", dta$q76,dta$spouse2r76)
dta$income_man_woman[is.na(dta$income_man_woman)] <- 0
dta$income_man_woman[dta$income_man_woman ==  999] <- NA
dta$income_both_woman <- ifelse(dta$gender1=="woman", dta$q79,dta$spouse2r79)
dta$income_both_woman[is.na(dta$income_both_woman)] <- 0
dta$income_both_woman[dta$income_both_woman == 999] <- NA



dta$income_man <- ifelse(dta$gender1=="man", dta$q73,dta$spouse2r73)
dta$income_man[is.na(dta$income_man)] <- 0
dta$income_man[dta$income_man ==  999] <- NA
dta$income_woman_man <- ifelse(dta$gender1=="man", dta$q76,dta$spouse2r76)
dta$income_woman_man[is.na(dta$income_woman_man)] <- 0
dta$income_woman_man[dta$income_woman_man ==  999] <- NA
dta$income_both_man <- ifelse(dta$gender1=="man", dta$q79,dta$spouse2r79)
dta$income_both_man[is.na(dta$income_both_man)] <- 0
dta$income_both_man[dta$income_both_man == 999] <- NA



##sold both as reported by man
dta$price_sold_both_man <- ifelse(dta$gender1=="man", dta$q79/dta$q78,dta$spouse2r79/dta$spouse2r78)
dta$price_sold_both_man[dta$price_sold_both_man <10000] <- NA

dta$price_sold_both_man[dta$price_sold_both_man > 100000] <- NA
### sold to whom?
dta$sold_to_trader_woman <- ifelse(dta$gender1=="woman", dta$q742,dta$spouse2r742)
dta$sold_to_middleman_woman <- ifelse(dta$gender1=="woman", dta$q743,dta$spouse2r743)
dta$sold_to_processor_woman <- ifelse(dta$gender1=="woman", dta$q744,dta$spouse2r744)

### sold to whom?
dta$sold_to_trader_man_woman <- ifelse(dta$gender1=="woman", dta$q772,dta$spouse2r802)
dta$sold_to_middleman_man_woman <- ifelse(dta$gender1=="woman", dta$q773,dta$spouse2r803)
dta$sold_to_processor_man_woman <- ifelse(dta$gender1=="woman", dta$q774,dta$spouse2r804)

### sold to whom?
dta$sold_to_trader_both_woman <- ifelse(dta$gender1=="woman", dta$q802,dta$spouse2r802)
dta$sold_to_middleman_both_woman <- ifelse(dta$gender1=="woman", dta$q803,dta$spouse2r803)
dta$sold_to_processor_both_woman <- ifelse(dta$gender1=="woman", dta$q804,dta$spouse2r804)

### of all the women who remember being shown a video
 prop.table(table(ifelse(dta$gender1=="woman" & dta$recipient == "female", dta$q91,dta$spouse2r91)))
 prop.table(table(ifelse(dta$gender1=="man" & dta$recipient == "male", dta$q91,dta$spouse2r91)))

dta$women_discuss <- ifelse(dta$gender1=="woman" & dta$recipient == "female", dta$q91,dta$spouse2r91)==2
dta$male_discuss <- ifelse(dta$gender1=="man" & dta$recipient == "male", dta$q91,dta$spouse2r91)==2
##do a chi-square test
dta$group <- "women"
df <- cbind(dta$women_discuss,dta$group)

dta$group <- "men"
df <- data.frame(rbind(df,cbind(dta$male_discuss,dta$group)))
names(df) <- c("discuss","group")
prop.table(table(df[,1:2]),2)
chisq.test(table(df[,1:2]),simulate.p.value=TRUE)
fisher.test(table(df[,1:2]))


#### redo timing
### hh level time spent on preparing 

### as reported by man


dta$time_prep_man_man <- NA
dta$time_prep_man_man[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_prep_pl1_sp1_self","time_prep_pl2_sp1_self","time_prep_pl3_sp1_self","time_prep_pl4_sp1_self","time_prep_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_prep_man_man[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_prep_pl1_sp1_self","time_prep_pl2_sp1_self","time_prep_pl3_sp1_self","time_prep_pl4_sp1_self","time_prep_pl5_sp1_self")]))==5)]  <- NA
dta$time_prep_man_man[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_prep_pl1_sp2_self","time_prep_pl2_sp2_self","time_prep_pl3_sp2_self","time_prep_pl4_sp2_self","time_prep_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_prep_man_man[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_prep_pl1_sp2_self","time_prep_pl2_sp2_self","time_prep_pl3_sp2_self","time_prep_pl4_sp2_self","time_prep_pl5_sp2_self")]))==5)]  <- NA

dta$time_prep_man_woman <- NA
dta$time_prep_man_woman[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_prep_pl1_sp2_other","time_prep_pl2_sp2_other","time_prep_pl3_sp2_other","time_prep_pl4_sp2_other","time_prep_pl5_sp2_other")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_prep_man_woman[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_prep_pl1_sp2_other","time_prep_pl2_sp2_other","time_prep_pl3_sp2_other","time_prep_pl4_sp2_other","time_prep_pl5_sp2_other")]))==5)]  <- NA
dta$time_prep_man_woman[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_prep_pl1_sp1_other","time_prep_pl2_sp1_other","time_prep_pl3_sp1_other","time_prep_pl4_sp1_other","time_prep_pl5_sp1_other")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_prep_man_woman[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_prep_pl1_sp1_other","time_prep_pl2_sp1_other","time_prep_pl3_sp1_other","time_prep_pl4_sp1_other","time_prep_pl5_sp1_other")]))==5)]  <- NA

dta$time_prep_woman_woman <- NA
dta$time_prep_woman_woman[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_prep_pl1_sp1_self","time_prep_pl2_sp1_self","time_prep_pl3_sp1_self","time_prep_pl4_sp1_self","time_prep_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_prep_woman_woman[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_prep_pl1_sp1_self","time_prep_pl2_sp1_self","time_prep_pl3_sp1_self","time_prep_pl4_sp1_self","time_prep_pl5_sp1_self")]))==5)]  <- NA
dta$time_prep_woman_woman[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_prep_pl1_sp2_self","time_prep_pl2_sp2_self","time_prep_pl3_sp2_self","time_prep_pl4_sp2_self","time_prep_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_prep_woman_woman[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_prep_pl1_sp2_self","time_prep_pl2_sp2_self","time_prep_pl3_sp2_self","time_prep_pl4_sp2_self","time_prep_pl5_sp2_self")]))==5)]  <- NA

dta$time_prep_woman_man <- NA
dta$time_prep_woman_man[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_prep_pl1_sp2_other","time_prep_pl2_sp2_other","time_prep_pl3_sp2_other","time_prep_pl4_sp2_other","time_prep_pl5_sp2_other")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_prep_woman_man[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_prep_pl1_sp2_other","time_prep_pl2_sp2_other","time_prep_pl3_sp2_other","time_prep_pl4_sp2_other","time_prep_pl5_sp2_other")]))==5)]  <- NA
dta$time_prep_woman_man[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_prep_pl1_sp1_other","time_prep_pl2_sp1_other","time_prep_pl3_sp1_other","time_prep_pl4_sp1_other","time_prep_pl5_sp1_other")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_prep_woman_man[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_prep_pl1_sp1_other","time_prep_pl2_sp1_other","time_prep_pl3_sp1_other","time_prep_pl4_sp1_other","time_prep_pl5_sp1_other")]))==5)]  <- NA
##### time plant

## as reported by man


dta$time_plant_man_man <- NA
dta$time_plant_man_man[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_plant_pl1_sp1_self","time_plant_pl2_sp1_self","time_plant_pl3_sp1_self","time_plant_pl4_sp1_self","time_plant_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_plant_man_man[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_plant_pl1_sp1_self","time_plant_pl2_sp1_self","time_plant_pl3_sp1_self","time_plant_pl4_sp1_self","time_plant_pl5_sp1_self")]))==5)]  <- NA
dta$time_plant_man_man[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_plant_pl1_sp2_self","time_plant_pl2_sp2_self","time_plant_pl3_sp2_self","time_plant_pl4_sp2_self","time_plant_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_plant_man_man[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_plant_pl1_sp2_self","time_plant_pl2_sp2_self","time_plant_pl3_sp2_self","time_plant_pl4_sp2_self","time_plant_pl5_sp2_self")]))==5)]  <- NA

dta$time_plant_man_woman <- NA
dta$time_plant_man_woman[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_plant_pl1_sp2_other","time_plant_pl2_sp2_other","time_plant_pl3_sp2_other","time_plant_pl4_sp2_other","time_plant_pl5_sp2_other")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_plant_man_woman[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_plant_pl1_sp2_other","time_plant_pl2_sp2_other","time_plant_pl3_sp2_other","time_plant_pl4_sp2_other","time_plant_pl5_sp2_other")]))==5)]  <- NA
dta$time_plant_man_woman[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_plant_pl1_sp1_other","time_plant_pl2_sp1_other","time_plant_pl3_sp1_other","time_plant_pl4_sp1_other","time_plant_pl5_sp1_other")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_plant_man_woman[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_plant_pl1_sp1_other","time_plant_pl2_sp1_other","time_plant_pl3_sp1_other","time_plant_pl4_sp1_other","time_plant_pl5_sp1_other")]))==5)]  <- NA

dta$time_plant_woman_woman <- NA
dta$time_plant_woman_woman[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_plant_pl1_sp1_self","time_plant_pl2_sp1_self","time_plant_pl3_sp1_self","time_plant_pl4_sp1_self","time_plant_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_plant_woman_woman[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_plant_pl1_sp1_self","time_plant_pl2_sp1_self","time_plant_pl3_sp1_self","time_plant_pl4_sp1_self","time_plant_pl5_sp1_self")]))==5)]  <- NA
dta$time_plant_woman_woman[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_plant_pl1_sp2_self","time_plant_pl2_sp2_self","time_plant_pl3_sp2_self","time_plant_pl4_sp2_self","time_plant_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_plant_woman_woman[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_plant_pl1_sp2_self","time_plant_pl2_sp2_self","time_plant_pl3_sp2_self","time_plant_pl4_sp2_self","time_plant_pl5_sp2_self")]))==5)]  <- NA

dta$time_plant_woman_man <- NA
dta$time_plant_woman_man[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_plant_pl1_sp2_other","time_plant_pl2_sp2_other","time_plant_pl3_sp2_other","time_plant_pl4_sp2_other","time_plant_pl5_sp2_other")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_plant_woman_man[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_plant_pl1_sp2_other","time_plant_pl2_sp2_other","time_plant_pl3_sp2_other","time_plant_pl4_sp2_other","time_plant_pl5_sp2_other")]))==5)]  <- NA
dta$time_plant_woman_man[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_plant_pl1_sp1_other","time_plant_pl2_sp1_other","time_plant_pl3_sp1_other","time_plant_pl4_sp1_other","time_plant_pl5_sp1_other")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_plant_woman_man[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_plant_pl1_sp1_other","time_plant_pl2_sp1_other","time_plant_pl3_sp1_other","time_plant_pl4_sp1_other","time_plant_pl5_sp1_other")]))==5)]  <- NA


########
### time weed1

## as reported by man


dta$time_weed1_man_man <- NA
dta$time_weed1_man_man[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_weed1_pl1_sp1_self","time_weed1_pl2_sp1_self","time_weed1_pl3_sp1_self","time_weed1_pl4_sp1_self","time_weed1_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_weed1_man_man[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_weed1_pl1_sp1_self","time_weed1_pl2_sp1_self","time_weed1_pl3_sp1_self","time_weed1_pl4_sp1_self","time_weed1_pl5_sp1_self")]))==5)]  <- NA
dta$time_weed1_man_man[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_weed1_pl1_sp2_self","time_weed1_pl2_sp2_self","time_weed1_pl3_sp2_self","time_weed1_pl4_sp2_self","time_weed1_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_weed1_man_man[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_weed1_pl1_sp2_self","time_weed1_pl2_sp2_self","time_weed1_pl3_sp2_self","time_weed1_pl4_sp2_self","time_weed1_pl5_sp2_self")]))==5)]  <- NA

dta$time_weed1_man_woman <- NA
dta$time_weed1_man_woman[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_weed1_pl1_sp2_other","time_weed1_pl2_sp2_other","time_weed1_pl3_sp2_other","time_weed1_pl4_sp2_other","time_weed1_pl5_sp2_other")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_weed1_man_woman[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_weed1_pl1_sp2_other","time_weed1_pl2_sp2_other","time_weed1_pl3_sp2_other","time_weed1_pl4_sp2_other","time_weed1_pl5_sp2_other")]))==5)]  <- NA
dta$time_weed1_man_woman[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_weed1_pl1_sp1_other","time_weed1_pl2_sp1_other","time_weed1_pl3_sp1_other","time_weed1_pl4_sp1_other","time_weed1_pl5_sp1_other")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_weed1_man_woman[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_weed1_pl1_sp1_other","time_weed1_pl2_sp1_other","time_weed1_pl3_sp1_other","time_weed1_pl4_sp1_other","time_weed1_pl5_sp1_other")]))==5)]  <- NA

dta$time_weed1_woman_woman <- NA
dta$time_weed1_woman_woman[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_weed1_pl1_sp1_self","time_weed1_pl2_sp1_self","time_weed1_pl3_sp1_self","time_weed1_pl4_sp1_self","time_weed1_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_weed1_woman_woman[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_weed1_pl1_sp1_self","time_weed1_pl2_sp1_self","time_weed1_pl3_sp1_self","time_weed1_pl4_sp1_self","time_weed1_pl5_sp1_self")]))==5)]  <- NA
dta$time_weed1_woman_woman[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_weed1_pl1_sp2_self","time_weed1_pl2_sp2_self","time_weed1_pl3_sp2_self","time_weed1_pl4_sp2_self","time_weed1_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_weed1_woman_woman[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_weed1_pl1_sp2_self","time_weed1_pl2_sp2_self","time_weed1_pl3_sp2_self","time_weed1_pl4_sp2_self","time_weed1_pl5_sp2_self")]))==5)]  <- NA

dta$time_weed1_woman_man <- NA
dta$time_weed1_woman_man[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_weed1_pl1_sp2_other","time_weed1_pl2_sp2_other","time_weed1_pl3_sp2_other","time_weed1_pl4_sp2_other","time_weed1_pl5_sp2_other")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_weed1_woman_man[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_weed1_pl1_sp2_other","time_weed1_pl2_sp2_other","time_weed1_pl3_sp2_other","time_weed1_pl4_sp2_other","time_weed1_pl5_sp2_other")]))==5)]  <- NA
dta$time_weed1_woman_man[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_weed1_pl1_sp1_other","time_weed1_pl2_sp1_other","time_weed1_pl3_sp1_other","time_weed1_pl4_sp1_other","time_weed1_pl5_sp1_other")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_weed1_woman_man[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_weed1_pl1_sp1_other","time_weed1_pl2_sp1_other","time_weed1_pl3_sp1_other","time_weed1_pl4_sp1_other","time_weed1_pl5_sp1_other")]))==5)]  <- NA

## time weed2

## as reported by man


dta$time_weed2_man_man <- NA
dta$time_weed2_man_man[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_weed2_pl1_sp1_self","time_weed2_pl2_sp1_self","time_weed2_pl3_sp1_self","time_weed2_pl4_sp1_self","time_weed2_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_weed2_man_man[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_weed2_pl1_sp1_self","time_weed2_pl2_sp1_self","time_weed2_pl3_sp1_self","time_weed2_pl4_sp1_self","time_weed2_pl5_sp1_self")]))==5)]  <- NA
dta$time_weed2_man_man[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_weed2_pl1_sp2_self","time_weed2_pl2_sp2_self","time_weed2_pl3_sp2_self","time_weed2_pl4_sp2_self","time_weed2_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_weed2_man_man[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_weed2_pl1_sp2_self","time_weed2_pl2_sp2_self","time_weed2_pl3_sp2_self","time_weed2_pl4_sp2_self","time_weed2_pl5_sp2_self")]))==5)]  <- NA

dta$time_weed2_man_woman <- NA
dta$time_weed2_man_woman[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_weed2_pl1_sp2_other","time_weed2_pl2_sp2_other","time_weed2_pl3_sp2_other","time_weed2_pl4_sp2_other","time_weed2_pl5_sp2_other")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_weed2_man_woman[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_weed2_pl1_sp2_other","time_weed2_pl2_sp2_other","time_weed2_pl3_sp2_other","time_weed2_pl4_sp2_other","time_weed2_pl5_sp2_other")]))==5)]  <- NA
dta$time_weed2_man_woman[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_weed2_pl1_sp1_other","time_weed2_pl2_sp1_other","time_weed2_pl3_sp1_other","time_weed2_pl4_sp1_other","time_weed2_pl5_sp1_other")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_weed2_man_woman[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_weed2_pl1_sp1_other","time_weed2_pl2_sp1_other","time_weed2_pl3_sp1_other","time_weed2_pl4_sp1_other","time_weed2_pl5_sp1_other")]))==5)]  <- NA

dta$time_weed2_woman_woman <- NA
dta$time_weed2_woman_woman[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_weed2_pl1_sp1_self","time_weed2_pl2_sp1_self","time_weed2_pl3_sp1_self","time_weed2_pl4_sp1_self","time_weed2_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_weed2_woman_woman[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_weed2_pl1_sp1_self","time_weed2_pl2_sp1_self","time_weed2_pl3_sp1_self","time_weed2_pl4_sp1_self","time_weed2_pl5_sp1_self")]))==5)]  <- NA
dta$time_weed2_woman_woman[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_weed2_pl1_sp2_self","time_weed2_pl2_sp2_self","time_weed2_pl3_sp2_self","time_weed2_pl4_sp2_self","time_weed2_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_weed2_woman_woman[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_weed2_pl1_sp2_self","time_weed2_pl2_sp2_self","time_weed2_pl3_sp2_self","time_weed2_pl4_sp2_self","time_weed2_pl5_sp2_self")]))==5)]  <- NA

dta$time_weed2_woman_man <- NA
dta$time_weed2_woman_man[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_weed2_pl1_sp2_other","time_weed2_pl2_sp2_other","time_weed2_pl3_sp2_other","time_weed2_pl4_sp2_other","time_weed2_pl5_sp2_other")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_weed2_woman_man[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_weed2_pl1_sp2_other","time_weed2_pl2_sp2_other","time_weed2_pl3_sp2_other","time_weed2_pl4_sp2_other","time_weed2_pl5_sp2_other")]))==5)]  <- NA
dta$time_weed2_woman_man[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_weed2_pl1_sp1_other","time_weed2_pl2_sp1_other","time_weed2_pl3_sp1_other","time_weed2_pl4_sp1_other","time_weed2_pl5_sp1_other")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_weed2_woman_man[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_weed2_pl1_sp1_other","time_weed2_pl2_sp1_other","time_weed2_pl3_sp1_other","time_weed2_pl4_sp1_other","time_weed2_pl5_sp1_other")]))==5)]  <- NA

# time weed

## as reported by man


dta$time_weed3_man_man <- NA
dta$time_weed3_man_man[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_weed3_pl1_sp1_self","time_weed3_pl2_sp1_self","time_weed3_pl3_sp1_self","time_weed3_pl4_sp1_self","time_weed3_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_weed3_man_man[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_weed3_pl1_sp1_self","time_weed3_pl2_sp1_self","time_weed3_pl3_sp1_self","time_weed3_pl4_sp1_self","time_weed3_pl5_sp1_self")]))==5)]  <- NA
dta$time_weed3_man_man[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_weed3_pl1_sp2_self","time_weed3_pl2_sp2_self","time_weed3_pl3_sp2_self","time_weed3_pl4_sp2_self","time_weed3_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_weed3_man_man[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_weed3_pl1_sp2_self","time_weed3_pl2_sp2_self","time_weed3_pl3_sp2_self","time_weed3_pl4_sp2_self","time_weed3_pl5_sp2_self")]))==5)]  <- NA

dta$time_weed3_man_woman <- NA
dta$time_weed3_man_woman[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_weed3_pl1_sp2_other","time_weed3_pl2_sp2_other","time_weed3_pl3_sp2_other","time_weed3_pl4_sp2_other","time_weed3_pl5_sp2_other")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_weed3_man_woman[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_weed3_pl1_sp2_other","time_weed3_pl2_sp2_other","time_weed3_pl3_sp2_other","time_weed3_pl4_sp2_other","time_weed3_pl5_sp2_other")]))==5)]  <- NA
dta$time_weed3_man_woman[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_weed3_pl1_sp1_other","time_weed3_pl2_sp1_other","time_weed3_pl3_sp1_other","time_weed3_pl4_sp1_other","time_weed3_pl5_sp1_other")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_weed3_man_woman[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_weed3_pl1_sp1_other","time_weed3_pl2_sp1_other","time_weed3_pl3_sp1_other","time_weed3_pl4_sp1_other","time_weed3_pl5_sp1_other")]))==5)]  <- NA

dta$time_weed3_woman_woman <- NA
dta$time_weed3_woman_woman[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_weed3_pl1_sp1_self","time_weed3_pl2_sp1_self","time_weed3_pl3_sp1_self","time_weed3_pl4_sp1_self","time_weed3_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_weed3_woman_woman[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_weed3_pl1_sp1_self","time_weed3_pl2_sp1_self","time_weed3_pl3_sp1_self","time_weed3_pl4_sp1_self","time_weed3_pl5_sp1_self")]))==5)]  <- NA
dta$time_weed3_woman_woman[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_weed3_pl1_sp2_self","time_weed3_pl2_sp2_self","time_weed3_pl3_sp2_self","time_weed3_pl4_sp2_self","time_weed3_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_weed3_woman_woman[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_weed3_pl1_sp2_self","time_weed3_pl2_sp2_self","time_weed3_pl3_sp2_self","time_weed3_pl4_sp2_self","time_weed3_pl5_sp2_self")]))==5)]  <- NA

dta$time_weed3_woman_man <- NA
dta$time_weed3_woman_man[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_weed3_pl1_sp2_other","time_weed3_pl2_sp2_other","time_weed3_pl3_sp2_other","time_weed3_pl4_sp2_other","time_weed3_pl5_sp2_other")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_weed3_woman_man[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_weed3_pl1_sp2_other","time_weed3_pl2_sp2_other","time_weed3_pl3_sp2_other","time_weed3_pl4_sp2_other","time_weed3_pl5_sp2_other")]))==5)]  <- NA
dta$time_weed3_woman_man[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_weed3_pl1_sp1_other","time_weed3_pl2_sp1_other","time_weed3_pl3_sp1_other","time_weed3_pl4_sp1_other","time_weed3_pl5_sp1_other")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_weed3_woman_man[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_weed3_pl1_sp1_other","time_weed3_pl2_sp1_other","time_weed3_pl3_sp1_other","time_weed3_pl4_sp1_other","time_weed3_pl5_sp1_other")]))==5)]  <- NA

###
# time sparying 

## as reported by man


dta$time_spray_man_man <- NA
dta$time_spray_man_man[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_spray_pl1_sp1_self","time_spray_pl2_sp1_self","time_spray_pl3_sp1_self","time_spray_pl4_sp1_self","time_spray_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_spray_man_man[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_spray_pl1_sp1_self","time_spray_pl2_sp1_self","time_spray_pl3_sp1_self","time_spray_pl4_sp1_self","time_spray_pl5_sp1_self")]))==5)]  <- NA
dta$time_spray_man_man[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_spray_pl1_sp2_self","time_spray_pl2_sp2_self","time_spray_pl3_sp2_self","time_spray_pl4_sp2_self","time_spray_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_spray_man_man[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_spray_pl1_sp2_self","time_spray_pl2_sp2_self","time_spray_pl3_sp2_self","time_spray_pl4_sp2_self","time_spray_pl5_sp2_self")]))==5)]  <- NA

dta$time_spray_man_woman <- NA
dta$time_spray_man_woman[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_spray_pl1_sp2_other","time_spray_pl2_sp2_other","time_spray_pl3_sp2_other","time_spray_pl4_sp2_other","time_spray_pl5_sp2_other")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_spray_man_woman[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_spray_pl1_sp2_other","time_spray_pl2_sp2_other","time_spray_pl3_sp2_other","time_spray_pl4_sp2_other","time_spray_pl5_sp2_other")]))==5)]  <- NA
dta$time_spray_man_woman[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_spray_pl1_sp1_other","time_spray_pl2_sp1_other","time_spray_pl3_sp1_other","time_spray_pl4_sp1_other","time_spray_pl5_sp1_other")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_spray_man_woman[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_spray_pl1_sp1_other","time_spray_pl2_sp1_other","time_spray_pl3_sp1_other","time_spray_pl4_sp1_other","time_spray_pl5_sp1_other")]))==5)]  <- NA

dta$time_spray_woman_woman <- NA
dta$time_spray_woman_woman[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_spray_pl1_sp1_self","time_spray_pl2_sp1_self","time_spray_pl3_sp1_self","time_spray_pl4_sp1_self","time_spray_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_spray_woman_woman[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_spray_pl1_sp1_self","time_spray_pl2_sp1_self","time_spray_pl3_sp1_self","time_spray_pl4_sp1_self","time_spray_pl5_sp1_self")]))==5)]  <- NA
dta$time_spray_woman_woman[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_spray_pl1_sp2_self","time_spray_pl2_sp2_self","time_spray_pl3_sp2_self","time_spray_pl4_sp2_self","time_spray_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_spray_woman_woman[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_spray_pl1_sp2_self","time_spray_pl2_sp2_self","time_spray_pl3_sp2_self","time_spray_pl4_sp2_self","time_spray_pl5_sp2_self")]))==5)]  <- NA

dta$time_spray_woman_man <- NA
dta$time_spray_woman_man[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_spray_pl1_sp2_other","time_spray_pl2_sp2_other","time_spray_pl3_sp2_other","time_spray_pl4_sp2_other","time_spray_pl5_sp2_other")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_spray_woman_man[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_spray_pl1_sp2_other","time_spray_pl2_sp2_other","time_spray_pl3_sp2_other","time_spray_pl4_sp2_other","time_spray_pl5_sp2_other")]))==5)]  <- NA
dta$time_spray_woman_man[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_spray_pl1_sp1_other","time_spray_pl2_sp1_other","time_spray_pl3_sp1_other","time_spray_pl4_sp1_other","time_spray_pl5_sp1_other")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_spray_woman_man[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_spray_pl1_sp1_other","time_spray_pl2_sp1_other","time_spray_pl3_sp1_other","time_spray_pl4_sp1_other","time_spray_pl5_sp1_other")]))==5)]  <- NA

####
# time harvesting

## as reported by man


dta$time_harv_man_man <- NA
dta$time_harv_man_man[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_harv_pl1_sp1_self","time_harv_pl2_sp1_self","time_harv_pl3_sp1_self","time_harv_pl4_sp1_self","time_harv_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_harv_man_man[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_harv_pl1_sp1_self","time_harv_pl2_sp1_self","time_harv_pl3_sp1_self","time_harv_pl4_sp1_self","time_harv_pl5_sp1_self")]))==5)]  <- NA
dta$time_harv_man_man[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_harv_pl1_sp2_self","time_harv_pl2_sp2_self","time_harv_pl3_sp2_self","time_harv_pl4_sp2_self","time_harv_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_harv_man_man[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_harv_pl1_sp2_self","time_harv_pl2_sp2_self","time_harv_pl3_sp2_self","time_harv_pl4_sp2_self","time_harv_pl5_sp2_self")]))==5)]  <- NA

dta$time_harv_man_woman <- NA
dta$time_harv_man_woman[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_harv_pl1_sp2_other","time_harv_pl2_sp2_other","time_harv_pl3_sp2_other","time_harv_pl4_sp2_other","time_harv_pl5_sp2_other")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_harv_man_woman[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_harv_pl1_sp2_other","time_harv_pl2_sp2_other","time_harv_pl3_sp2_other","time_harv_pl4_sp2_other","time_harv_pl5_sp2_other")]))==5)]  <- NA
dta$time_harv_man_woman[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_harv_pl1_sp1_other","time_harv_pl2_sp1_other","time_harv_pl3_sp1_other","time_harv_pl4_sp1_other","time_harv_pl5_sp1_other")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_harv_man_woman[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_harv_pl1_sp1_other","time_harv_pl2_sp1_other","time_harv_pl3_sp1_other","time_harv_pl4_sp1_other","time_harv_pl5_sp1_other")]))==5)]  <- NA

dta$time_harv_woman_woman <- NA
dta$time_harv_woman_woman[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_harv_pl1_sp1_self","time_harv_pl2_sp1_self","time_harv_pl3_sp1_self","time_harv_pl4_sp1_self","time_harv_pl5_sp1_self")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_harv_woman_woman[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_harv_pl1_sp1_self","time_harv_pl2_sp1_self","time_harv_pl3_sp1_self","time_harv_pl4_sp1_self","time_harv_pl5_sp1_self")]))==5)]  <- NA
dta$time_harv_woman_woman[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_harv_pl1_sp2_self","time_harv_pl2_sp2_self","time_harv_pl3_sp2_self","time_harv_pl4_sp2_self","time_harv_pl5_sp2_self")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_harv_woman_woman[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_harv_pl1_sp2_self","time_harv_pl2_sp2_self","time_harv_pl3_sp2_self","time_harv_pl4_sp2_self","time_harv_pl5_sp2_self")]))==5)]  <- NA

dta$time_harv_woman_man <- NA
dta$time_harv_woman_man[dta$person_interviewed=="woman"]  <- rowSums(dta[c("time_harv_pl1_sp2_other","time_harv_pl2_sp2_other","time_harv_pl3_sp2_other","time_harv_pl4_sp2_other","time_harv_pl5_sp2_other")], na.rm=T)[dta$person_interviewed=="woman"] 
dta$time_harv_woman_man[dta$person_interviewed=="woman" & (rowSums(is.na(dta[c("time_harv_pl1_sp2_other","time_harv_pl2_sp2_other","time_harv_pl3_sp2_other","time_harv_pl4_sp2_other","time_harv_pl5_sp2_other")]))==5)]  <- NA
dta$time_harv_woman_man[dta$person_interviewed=="man"]  <- rowSums(dta[c("time_harv_pl1_sp1_other","time_harv_pl2_sp1_other","time_harv_pl3_sp1_other","time_harv_pl4_sp1_other","time_harv_pl5_sp1_other")], na.rm=T)[dta$person_interviewed=="man"] 
dta$time_harv_woman_man[dta$person_interviewed=="man" & (rowSums(is.na(dta[c("time_harv_pl1_sp1_other","time_harv_pl2_sp1_other","time_harv_pl3_sp1_other","time_harv_pl4_sp1_other","time_harv_pl5_sp1_other")]))==5)]  <- NA

dta$time_weed_man_man <- rowSums(dta[c("time_weed1_man_man","time_weed2_man_man","time_weed3_man_man")], na.rm=T)
dta$time_weed_man_woman <- rowSums(dta[c("time_weed1_man_woman","time_weed2_man_woman","time_weed3_man_woman")], na.rm=T)
dta$time_weed_woman_woman <- rowSums(dta[c("time_weed1_woman_woman","time_weed2_woman_woman","time_weed3_woman_woman")], na.rm=T)
dta$time_weed_woman_man <- rowSums(dta[c("time_weed1_woman_man","time_weed2_woman_man","time_weed3_woman_man")], na.rm=T)

dta$time_tot_man_man <- rowSums(dta[c("time_prep_man_man","time_plant_man_man","time_weed_man_man","time_spray_man_man","time_harv_man_man")], na.rm=T)
dta$time_tot_man_woman <- rowSums(dta[c("time_prep_man_woman","time_plant_man_woman","time_weed_man_woman","time_spray_man_woman","time_harv_man_woman")], na.rm=T)
dta$time_tot_woman_man <- rowSums(dta[c("time_prep_woman_man","time_plant_woman_man","time_weed_woman_man","time_spray_woman_man","time_harv_woman_man")], na.rm=T)
dta$time_tot_woman_woman <- rowSums(dta[c("time_prep_woman_woman","time_plant_woman_woman","time_weed_woman_woman","time_spray_woman_woman","time_harv_woman_woman")], na.rm=T)

#how to deal with missings
dta$tot_time_hh <- rowMeans(dta[c("time_tot_man_man","time_tot_man_woman")]) + rowMeans(dta[c("time_tot_woman_man","time_tot_woman_woman")]) 
### keep only those where both have been interviewed
dta$tot_time_hh[dta$interview_status!="couple interviewed"] <- NA

### agree that they sold
dta$disagree_sold <- dta$q71 != dta$spouse2r71 
dta$agree_sold <- dta$q71 == dta$spouse2r71 
dta$agree_both <- dta$q71 == "Yes" & dta$spouse2r71 == "Yes" 
dta$agree_both[is.na(dta$disagree_sold)] <- NA

dta$nr_bags_sold_both_man[is.na(dta$nr_bags_sold_both_man)] <- 0
dta$nr_bags_sold_both_woman[is.na(dta$nr_bags_sold_both_woman)] <- 0


#sold by you alone q72 and R72 - this is in bags of about 100 kgs


dta$nr_bags_sold_woman[is.na(dta$nr_bags_sold_woman)] <- 0
dta$nr_bags_sold_man_woman[is.na(dta$nr_bags_sold_man_woman)] <- 0
dta$nr_bags_sold_both_woman[is.na(dta$nr_bags_sold_both_woman)] <- 0
dta$nr_bags_sold_other_woman[is.na(dta$nr_bags_sold_other_woman)] <- 0

dta$nr_bags_sold_man[is.na(dta$nr_bags_sold_man)] <- 0
dta$nr_bags_sold_woman_man[is.na(dta$nr_bags_sold_woman_man)] <- 0
dta$nr_bags_sold_both_man[is.na(dta$nr_bags_sold_both_man)] <- 0
dta$nr_bags_sold_other_man[is.na(dta$nr_bags_sold_other_man)] <- 0



dta$share_sold_both_both <- ((dta$nr_bags_sold_both_man + dta$nr_bags_sold_both_woman)/2) / ((rowSums(dta[c("nr_bags_sold_man","nr_bags_sold_woman_man","nr_bags_sold_both_man","nr_bags_sold_other_man")]) + rowSums(dta[c("nr_bags_sold_woman","nr_bags_sold_man_woman","nr_bags_sold_both_woman","nr_bags_sold_other_woman")]))/2)
dta$share_sold_both_both[is.na(dta$disagree_sold)] <- NA
dta$share_sold_both_both[dta$q71 == "No" & dta$spouse2r71 == "No"] <- NA


dta$agree_spend_edu <- (dta$spend_list1 & dta$spouse2spend_list_sp1) | (dta$spend_list2 & dta$spouse2spend_list_sp2)
dta$agree_spend_edu[is.na(dta$agree_spend_edu)] <- FALSE
dta$agree_spend_edu[is.na(dta$agree_both)] <- NA
dta$agree_spend_edu[dta$q71 == "No" & dta$spouse2r71 == "No"] <- NA


dta$agree_spend_ag <- (dta$spend_list3 & dta$spouse2spend_list_sp3) |  (dta$spend_list4 & dta$spouse2spend_list_sp4)
dta$agree_spend_ag[is.na(dta$agree_spend_ag)] <- FALSE
dta$agree_spend_ag[is.na(dta$agree_both)] <- NA
dta$agree_spend_ag[dta$q71 == "No" & dta$spouse2r71 == "No"] <- NA


dta$agree_spend_inv <- dta$spend_list5 & dta$spouse2spend_list_sp5
dta$agree_spend_inv[is.na(dta$agree_spend_inv)] <- FALSE
dta$agree_spend_inv[is.na(dta$agree_both)] <- NA
dta$agree_spend_inv[dta$q71 == "No" & dta$spouse2r71 == "No"] <- NA

dta$agree_spend_biz <- dta$spend_list6 & dta$spouse2spend_list_sp6
dta$agree_spend_biz[is.na(dta$agree_spend_biz)] <- FALSE
dta$agree_spend_biz[is.na(dta$agree_both)] <- NA
dta$agree_spend_biz[dta$q71 == "No" & dta$spouse2r71 == "No"] <- NA

dta$agree_spend_cons <- dta$spend_list7 & dta$spouse2spend_list_sp7
dta$agree_spend_cons[is.na(dta$agree_spend_cons)] <- FALSE
dta$agree_spend_cons[is.na(dta$agree_both)] <- NA
dta$agree_spend_con[dta$q71 == "No" & dta$spouse2r71 == "No"] <- NA

dta$agree_spend_sav <- dta$spend_list8 & dta$spouse2spend_list_sp8
dta$agree_spend_sav[is.na(dta$agree_spend_sav)] <- FALSE
dta$agree_spend_sav[is.na(dta$agree_both)] <- NA
dta$agree_spend_sav[dta$q71 == "No" & dta$spouse2r71 == "No"] <- NA

dta$agree_spend_alc <- dta$spend_list9 & dta$spouse2spend_list_sp9
dta$agree_spend_alc[is.na(dta$agree_spend_alc)] <- FALSE
dta$agree_spend_alc[is.na(dta$agree_both)] <- NA
dta$agree_spend_alc[dta$q71 == "No" & dta$spouse2r71 == "No"] <- NA




## agreement planting days after rain
dta$disagree_plant_share <- rowSums((dta[c("grp1days1","grp2days2","grp3days3","grp4days4", "grp5days5")] != dta[c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5")]), na.rm=T)/ 
rowSums( !is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))

### disagreement on recommended spacing

dta$disagree_spacing_share <- rowSums(dta[c("grp1a201","grp2b201","grp3c201","grp4d201","grp5e201")] != dta[c("spouse2grp_sp1f201","spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201","spouse2grp5_sp5k201")],na.rm=T)/ 
rowSums( !is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))

### disagreement on recommended striga

dta$disagree_striga_share <- rowSums(dta[c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241")] != 
dta[c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241")],na.rm=T)/ 
rowSums( !is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))

## disagreement on weeding

dta$disagree_weeding_share <- rowSums((dta[c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26")] == 3) != 
(dta[c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26")] == 3),na.rm=T)/ 
rowSums( !is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))

##  disagreement on fertilizer

dta$disagree_fert_share <- rowSums((dta[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")] == "Yes") !=
(dta[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29")] == "Yes"),na.rm=T)/ 
rowSums( !is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))

## disagreement on seed
dta$disagree_seed_share <- rowSums((dta[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")] == "Yes") != 
(dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42")] == "Yes"),na.rm=T)/ 
rowSums( !is.na(dta[c("mgt_both_pl1","mgt_both_pl2","mgt_both_pl3","mgt_both_pl4","mgt_both_pl5")]))

dta$disagree_prod <- abs(dta$prod_tot_sp1- dta$prod_tot_sp2) 

dta$disagree_area <- abs(dta$area_tot_sp1- dta$area_tot_sp2) 
dta$disagree_yield <- abs(dta$prod_tot_sp1/dta$area_tot_sp1- dta$prod_tot_sp2/dta$area_tot_sp2) 

dta$disagree_man_time <- dta$time_tot_man_man - dta$time_tot_man_woman
dta$disagree_woman_time <-  dta$time_tot_woman_woman - dta$time_tot_woman_man 

dta$disagree_sold_woman <- dta$nr_bags_sold_woman - dta$nr_bags_sold_woman_man
dta$disagree_sold_man <- dta$nr_bags_sold_man - dta$nr_bags_sold_man_woman
dta$disagree_sold_both <- abs(dta$nr_bags_sold_both_man - dta$nr_bags_sold_both_woman)


source("/home/bjvca/data/projects/digital green/papers/DP_disagreement/analysis/code_chunks/time_harv.R")
source("/home/bjvca/data/projects/digital green/papers/DP_disagreement/analysis/code_chunks/time_preparation.R")
source("/home/bjvca/data/projects/digital green/papers/DP_disagreement/analysis/code_chunks/time_plant.R")
source("/home/bjvca/data/projects/digital green/papers/DP_disagreement/analysis/code_chunks/time_weed1.R")
source("/home/bjvca/data/projects/digital green/papers/DP_disagreement/analysis/code_chunks/time_weed2.R")
source("/home/bjvca/data/projects/digital green/papers/DP_disagreement/analysis/code_chunks/time_weed3.R")
source("/home/bjvca/data/projects/digital green/papers/DP_disagreement/analysis/code_chunks/time_spray.R")
source("/home/bjvca/data/projects/digital green/papers/DP_disagreement/analysis/code_chunks/time_harv.R")

write.csv(dta,"/home/bjvca/data/projects/digital green/papers/DP_disagreement/endline_dta.csv")


