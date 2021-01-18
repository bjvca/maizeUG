library(clubSandwich)
dta <- read.csv("/home/bjvca/data/projects/digital green/papers/DP_cooperation/endline_dta.csv")

dta <- subset(dta, recipient == "couple")
dta <- subset(dta, interview_status == "couple interviewed")
dta <- subset(dta, messenger != "ctrl")


### decision making
dec <- c("mgt","dectime", "decspace","decstriga", "decweed")
df_ols <- array(NA,dim=c(6,3,length(dec)))
for (i in 1:length(dec)) {
d <- dta[c("hhid","messenger",paste(paste(dec[i],"both_pl",sep="_") ,1:5,sep=""))]

dta_long <- reshape(d, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"both",sep="_") , idvar = "hhid")

ols <- lm( as.formula(paste(paste(dec[i],"both",sep="_"),"(messenger=='couple')", sep ="~")),data=dta_long)
vcov_cluster <- vcovCR(ols, cluster = dta_long$hhid, type = "CR0")
res <- coef_test(ols, vcov_cluster)
conf <- conf_int(ols, vcov_cluster)
df_ols[1,1,i] <- mean(dta_long[,4][dta_long$messenger != "couple"], na.rm=T)
df_ols[2,1,i] <- sd(dta_long[,4][dta_long$messenger != "couple"], na.rm=T)
df_ols[3,1,i] <- res[2,1]
df_ols[4,1,i] <- res[2,2]
df_ols[5,1,i] <- res[2,5]
df_ols[6,1,i] <- nobs(ols)



d <- dta[c("hhid","messenger",paste(paste(dec[i],"man_pl",sep="_") ,1:5,sep=""))]

dta_long <- reshape(d, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"man",sep="_") , idvar = "hhid")
ols <- lm( as.formula(paste(paste(dec[i],"man",sep="_"),"(messenger=='couple')", sep ="~")),data=dta_long)
vcov_cluster <- vcovCR(ols, cluster = dta_long$hhid, type = "CR0")
res <- coef_test(ols, vcov_cluster)
conf <- conf_int(ols, vcov_cluster)

df_ols[1,2,i] <- mean(dta_long[,4][dta_long$messenger != "couple"], na.rm=T)
df_ols[2,2,i] <- sd(dta_long[,4][dta_long$messenger != "couple"], na.rm=T)
df_ols[3,2,i] <- res[2,1]
df_ols[4,2,i] <- res[2,2]
df_ols[5,2,i] <- res[2,5]
df_ols[6,2,i] <- nobs(ols)

d <- dta[c("hhid","messenger",paste(paste(dec[i],"woman_pl",sep="_") ,1:5,sep=""))]

dta_long <- reshape(d, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"woman",sep="_") , idvar = "hhid")
ols <- lm( as.formula(paste(paste(dec[i],"woman",sep="_"),"(messenger=='couple')", sep ="~")),data=dta_long)
vcov_cluster <- vcovCR(ols, cluster = dta_long$hhid, type = "CR0")
res <- coef_test(ols, vcov_cluster)
conf <- conf_int(ols, vcov_cluster)

df_ols[1,3,i] <- mean(dta_long[,4][dta_long$messenger != "couple"], na.rm=T)
df_ols[2,3,i] <- sd(dta_long[,4][dta_long$messenger != "couple"], na.rm=T)
df_ols[3,3,i] <- res[2,1]
df_ols[4,3,i] <- res[2,2]
df_ols[5,3,i] <- res[2,5]
df_ols[6,3,i] <- nobs(ols)
}


dta$time_weed_man_man_pl1 <- dta$time_weed1_man_man_pl1 + dta$time_weed3_man_man_pl1 + dta$time_weed3_man_man_pl1
dta$time_weed_woman_woman_pl1 <- dta$time_weed1_woman_woman_pl1 + dta$time_weed3_woman_woman_pl1 + dta$time_weed3_woman_woman_pl1
dta$time_weed_man_man_pl2 <- dta$time_weed1_man_man_pl2 + dta$time_weed3_man_man_pl2 + dta$time_weed3_man_man_pl2
dta$time_weed_woman_woman_pl2 <- dta$time_weed1_woman_woman_pl2 + dta$time_weed3_woman_woman_pl2 + dta$time_weed3_woman_woman_pl2
dta$time_weed_man_man_pl3 <- dta$time_weed1_man_man_pl3 + dta$time_weed3_man_man_pl3 + dta$time_weed3_man_man_pl3
dta$time_weed_woman_woman_pl3 <- dta$time_weed1_woman_woman_pl3 + dta$time_weed3_woman_woman_pl3 + dta$time_weed3_woman_woman_pl3
dta$time_weed_man_man_pl4 <- dta$time_weed1_man_man_pl4 + dta$time_weed3_man_man_pl4 + dta$time_weed3_man_man_pl4
dta$time_weed_woman_woman_pl4 <- dta$time_weed1_woman_woman_pl4 + dta$time_weed3_woman_woman_pl4 + dta$time_weed3_woman_woman_pl4
dta$time_weed_man_man_pl5 <- dta$time_weed1_man_man_pl5 + dta$time_weed3_man_man_pl5 + dta$time_weed3_man_man_pl5
dta$time_weed_woman_woman_pl5 <- dta$time_weed1_woman_woman_pl5 + dta$time_weed3_woman_woman_pl5 + dta$time_weed3_woman_woman_pl5


dta$time_weed_man_woman_pl1 <- dta$time_weed1_man_woman_pl1 + dta$time_weed3_man_woman_pl1 + dta$time_weed3_man_woman_pl1
dta$time_weed_woman_man_pl1 <- dta$time_weed1_woman_man_pl1 + dta$time_weed3_woman_man_pl1 + dta$time_weed3_woman_man_pl1
dta$time_weed_man_woman_pl2 <- dta$time_weed1_man_woman_pl2 + dta$time_weed3_man_woman_pl2 + dta$time_weed3_man_woman_pl2
dta$time_weed_woman_man_pl2 <- dta$time_weed1_woman_man_pl2 + dta$time_weed3_woman_man_pl2 + dta$time_weed3_woman_man_pl2
dta$time_weed_man_woman_pl3 <- dta$time_weed1_man_woman_pl3 + dta$time_weed3_man_woman_pl3 + dta$time_weed3_man_woman_pl3
dta$time_weed_woman_man_pl3 <- dta$time_weed1_woman_man_pl3 + dta$time_weed3_woman_man_pl3 + dta$time_weed3_woman_man_pl3
dta$time_weed_man_woman_pl4 <- dta$time_weed1_man_woman_pl4 + dta$time_weed3_man_woman_pl4 + dta$time_weed3_man_woman_pl4
dta$time_weed_woman_man_pl4 <- dta$time_weed1_woman_man_pl4 + dta$time_weed3_woman_man_pl4 + dta$time_weed3_woman_man_pl4
dta$time_weed_man_woman_pl5 <- dta$time_weed1_man_woman_pl5 + dta$time_weed3_man_woman_pl5 + dta$time_weed3_man_woman_pl5
dta$time_weed_woman_man_pl5 <- dta$time_weed1_woman_man_pl5 + dta$time_weed3_woman_man_pl5 + dta$time_weed3_woman_man_pl5

dec <- c("time_prep","time_plant","time_weed","time_spray","time_harv")
df_ols_time <- array(NA,dim=c(6,3,length(dec)))
for (i in 1:length(dec)) {
dm <- dta[c("hhid","messenger",paste(paste(dec[i],"man_man_pl",sep="_") ,1:5,sep=""))]
dw <- dta[c("hhid","messenger",paste(paste(dec[i],"woman_woman_pl",sep="_") ,1:5,sep=""))]

dta_long_m <- reshape(dm, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"man_man",sep="_") , idvar = "hhid")
dta_long_w <- reshape(dw, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"woman_woman",sep="_") , idvar = "hhid")
dta_long <- merge(dta_long_m, dta_long_w)
dta_long$differ <- NA
dta_long$differ <- unlist(dta_long[paste(dec[i],"woman_woman",sep="_")] - dta_long[paste(dec[i],"man_man",sep="_")])

ols <- lm( "differ~(messenger=='couple')",data=dta_long)
vcov_cluster <- vcovCR(ols, cluster = dta_long$hhid, type = "CR0")
res <- coef_test(ols, vcov_cluster)
conf <- conf_int(ols, vcov_cluster)

df_ols_time[1,1,i] <- mean(dta_long$differ[dta_long$messenger != "couple"], na.rm=T)
df_ols_time[2,1,i] <- sd(dta_long[,4][dta_long$messenger != "couple"], na.rm=T)
df_ols_time[3,1,i] <- res[2,1]
df_ols_time[4,1,i] <- res[2,2]
df_ols_time[5,1,i] <- res[2,5]
df_ols_time[6,1,i] <- nobs(ols)

dm <- dta[c("hhid","messenger",paste(paste(dec[i],"man_man_pl",sep="_") ,1:5,sep=""))]
dw <- dta[c("hhid","messenger",paste(paste(dec[i],"woman_man_pl",sep="_") ,1:5,sep=""))]

dta_long_m <- reshape(dm, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"man_man",sep="_") , idvar = "hhid")
dta_long_w <- reshape(dw, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"woman_man",sep="_") , idvar = "hhid")
dta_long <- merge(dta_long_m, dta_long_w)
dta_long$differ <- NA
dta_long$differ <- unlist( dta_long[paste(dec[i],"man_man",sep="_")])

ols <- lm( "differ~(messenger=='couple')",data=dta_long)
vcov_cluster <- vcovCR(ols, cluster = dta_long$hhid, type = "CR0")
res <- coef_test(ols, vcov_cluster)
conf <- conf_int(ols, vcov_cluster)

df_ols_time[1,2,i] <- mean(dta_long$differ[dta_long$messenger != "couple"], na.rm=T)
df_ols_time[2,2,i] <- sd(dta_long[,4][dta_long$messenger != "couple"], na.rm=T)
df_ols_time[3,2,i] <- res[2,1]
df_ols_time[4,2,i] <- res[2,2]
df_ols_time[5,2,i] <- res[2,5]
df_ols_time[6,2,i] <- nobs(ols)

dm <- dta[c("hhid","messenger",paste(paste(dec[i],"man_woman_pl",sep="_") ,1:5,sep=""))]
dw <- dta[c("hhid","messenger",paste(paste(dec[i],"woman_woman_pl",sep="_") ,1:5,sep=""))]

dta_long_m <- reshape(dm, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"man_woman",sep="_") , idvar = "hhid")
dta_long_w <- reshape(dw, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"woman_woman",sep="_") , idvar = "hhid")
dta_long <- merge(dta_long_m, dta_long_w)
dta_long$differ <- NA
dta_long$differ <- unlist(dta_long[paste(dec[i],"woman_woman",sep="_")] )

ols <- lm( "differ~(messenger=='couple')",data=dta_long)
vcov_cluster <- vcovCR(ols, cluster = dta_long$hhid, type = "CR0")
res <- coef_test(ols, vcov_cluster)
conf <- conf_int(ols, vcov_cluster)

df_ols_time[1,3,i] <- mean(dta_long$differ[dta_long$messenger != "couple"], na.rm=T)
df_ols_time[2,3,i] <- sd(dta_long[,4][dta_long$messenger != "couple"], na.rm=T)
df_ols_time[3,3,i] <- res[2,1]
df_ols_time[4,3,i] <- res[2,2]
df_ols_time[5,3,i] <- res[2,5]
df_ols_time[6,3,i] <- nobs(ols)
}


###conflict resolution
i <- 1
df_ols_confl <-  array(NA,dim=c(6,3,2))
ols <- lm( "man_tells_wife~(messenger=='couple')",data=dta)
df_ols_confl[1,1,i] <- mean(dta$man_tells_wife[dta$messenger != "couple"], na.rm=T)
df_ols_confl[2,1,i] <- sd(dta$man_tells_wife[dta$messenger != "couple"], na.rm=T)
df_ols_confl[3,1,i] <- summary(ols)$coefficients[2, 1]
df_ols_confl[4,1,i] <- summary(ols)$coefficients[2, 2]
df_ols_confl[5,1,i] <- summary(ols)$coefficients[2, 4]
df_ols_confl[6,1,i] <- nobs(ols)

ols <- lm( "wife_tells_man~(messenger=='couple')",data=dta)
df_ols_confl[1,2,i] <- mean(dta$wife_tells_man[dta$messenger != "couple"], na.rm=T)
df_ols_confl[2,2,i] <- sd(dta$wife_tells_man[dta$messenger != "couple"], na.rm=T)
df_ols_confl[3,2,i] <- summary(ols)$coefficients[2, 1]
df_ols_confl[4,2,i] <- summary(ols)$coefficients[2, 2]
df_ols_confl[5,2,i] <- summary(ols)$coefficients[2, 4]
df_ols_confl[6,2,i] <- nobs(ols)

ols <- lm( "both_tell~(messenger=='couple')",data=dta)
df_ols_confl[1,3,i] <- mean(dta$both_tell[dta$messenger != "couple"], na.rm=T)
df_ols_confl[2,3,i] <- sd(dta$both_tell[dta$messenger != "couple"], na.rm=T)
df_ols_confl[3,3,i] <- summary(ols)$coefficients[2, 1]
df_ols_confl[4,3,i] <- summary(ols)$coefficients[2, 2]
df_ols_confl[5,3,i] <- summary(ols)$coefficients[2, 4]
df_ols_confl[6,3,i] <- nobs(ols)
i <- 2
ols <- lm( "man_listens~(messenger=='couple')",data=dta)
df_ols_confl[1,1,i] <- mean(dta$wife_listens[dta$messenger != "couple"], na.rm=T)
df_ols_confl[2,1,i] <- sd(dta$wife_listens[dta$messenger != "couple"], na.rm=T)
df_ols_confl[3,1,i] <- summary(ols)$coefficients[2, 1]
df_ols_confl[4,1,i] <- summary(ols)$coefficients[2, 2]
df_ols_confl[5,1,i] <- summary(ols)$coefficients[2, 4]
df_ols_confl[6,1,i] <- nobs(ols)

ols <- lm( "wife_listens~(messenger=='couple')",data=dta)
df_ols_confl[1,2,i] <- mean(dta$man_listens[dta$messenger != "couple"], na.rm=T)
df_ols_confl[2,2,i] <- sd(dta$man_listens[dta$messenger != "couple"], na.rm=T)
df_ols_confl[3,2,i] <- summary(ols)$coefficients[2, 1]
df_ols_confl[4,2,i] <- summary(ols)$coefficients[2, 2]
df_ols_confl[5,2,i] <- summary(ols)$coefficients[2, 4]
df_ols_confl[6,2,i] <- nobs(ols)

ols <- lm( "spouses_listen~(messenger=='couple')",data=dta)
df_ols_confl[1,3,i] <- mean(dta$spouses_listen[dta$messenger != "couple"], na.rm=T)
df_ols_confl[2,3,i] <- sd(dta$spouses_listen[dta$messenger != "couple"], na.rm=T)
df_ols_confl[3,3,i] <- summary(ols)$coefficients[2, 1]
df_ols_confl[4,3,i] <- summary(ols)$coefficients[2, 2]
df_ols_confl[5,3,i] <- summary(ols)$coefficients[2, 4]
df_ols_confl[6,3,i] <- nobs(ols)

i <- 1
df_ols_inc <-  array(NA,dim=c(6,3,2))
ols <- lm( "((nr_bags_sold_both_man + nr_bags_sold_both_woman)/2)~(messenger=='couple')",data=dta)
df_ols_inc[1,1,i] <- mean(dta$nr_bags_sold_both_man[dta$messenger != "couple"], na.rm=T)
df_ols_inc[2,1,i] <- sd(dta$nr_bags_sold_both_man[dta$messenger != "couple"], na.rm=T)
df_ols_inc[3,1,i] <- summary(ols)$coefficients[2, 1]
df_ols_inc[4,1,i] <- summary(ols)$coefficients[2, 2]
df_ols_inc[5,1,i] <- summary(ols)$coefficients[2, 4]
df_ols_inc[6,1,i] <- nobs(ols)

ols <- lm( "nr_bags_sold_man~(messenger=='couple')",data=dta)
df_ols_inc[1,2,i] <- mean(dta$nr_bags_sold_both_woman[dta$messenger != "couple"], na.rm=T)
df_ols_inc[2,2,i] <- sd(dta$nr_bags_sold_both_woman[dta$messenger != "couple"], na.rm=T)
df_ols_inc[3,2,i] <- summary(ols)$coefficients[2, 1]
df_ols_inc[4,2,i] <- summary(ols)$coefficients[2, 2]
df_ols_inc[5,2,i] <- summary(ols)$coefficients[2, 4]
df_ols_inc[6,2,i] <- nobs(ols)

ols <- lm( "nr_bags_sold_woman~(messenger=='couple')",data=dta)
df_ols_inc[1,3,i] <- mean(dta$nr_bags_sold_both_woman[dta$messenger != "couple"], na.rm=T)
df_ols_inc[2,3,i] <- sd(dta$nr_bags_sold_both_woman[dta$messenger != "couple"], na.rm=T)
df_ols_inc[3,3,i] <- summary(ols)$coefficients[2, 1]
df_ols_inc[4,3,i] <- summary(ols)$coefficients[2, 2]
df_ols_inc[5,3,i] <- summary(ols)$coefficients[2, 4]
df_ols_inc[6,3,i] <- nobs(ols)


### income hiding
dta$man_hiding <- (dta$nr_bags_sold_man )  - ( dta$nr_bags_sold_man_woman )
dta$woman_hiding <- (dta$nr_bags_sold_woman )  - ( dta$nr_bags_sold_woman_man )
i <- 2

ols <- lm( "man_hiding~(messenger=='couple')",data=dta)
df_ols_inc[1,1,i] <- mean(dta$man_hiding[dta$messenger != "couple"], na.rm=T)
df_ols_inc[2,1,i] <- sd(dta$man_hiding[dta$messenger != "couple"], na.rm=T)
df_ols_inc[3,1,i] <- summary(ols)$coefficients[2, 1]
df_ols_inc[4,1,i] <- summary(ols)$coefficients[2, 2]
df_ols_inc[5,1,i] <- summary(ols)$coefficients[2, 4]
df_ols_inc[6,1,i] <- nobs(ols)

ols <- lm( "woman_hiding~(messenger=='couple')",data=dta)
df_ols_inc[1,2,i] <- mean(dta$woman_hiding[dta$messenger != "couple"], na.rm=T)
df_ols_inc[2,2,i] <- sd(dta$woman_hiding[dta$messenger != "couple"], na.rm=T)
df_ols_inc[3,2,i] <- summary(ols)$coefficients[2, 1]
df_ols_inc[4,2,i] <- summary(ols)$coefficients[2, 2]
df_ols_inc[5,2,i] <- summary(ols)$coefficients[2, 4]
df_ols_inc[6,2,i] <- nobs(ols)


