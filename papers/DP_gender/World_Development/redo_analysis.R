rm(list=ls())

source("/home/bjvca/data/projects/digital green/endline/data/init_gender_WE.R")

library(ggplot2)
library(doParallel)
library(data.table)
library(dplyr)

library(car)

set.seed(07032018)

plot_res <- array(NA, c(12,5,6))
colnames(plot_res) <-  c("x","y","ylo","yhi","grp")


FW_index <- function(treat, indexer, data) {
### function to make family wise index using covariance as weights (following http://cyrussamii.com/?p=2656)
### FW_index("messenger != 'ctrl' ", c("know_space", "know_combine", "know_weed"),dta)
data <- data[complete.cases(data[indexer]),]
x <- data[indexer]

				for(j in 1:ncol(x)){
					x[,j] <- (x[,j] - mean(x[,j]))/sd(x[,j])
				}

					i.vec <- as.matrix(rep(1,ncol(x)))
					Sx <- cov(x)
					
					data$index <- t(solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)%*%t(x))
mod <- lm(as.formula(paste("index",treat,sep="~")) , data=data)
return(list(mod, data))
}

### wrapper function to make summary forest plots
credplot.gg <- function(d,units, hypo, axlabs, lim){
 # d is a data frame with 4 columns
 # d$x gives variable names
 # d$y gives center point
 # d$ylo gives lower limits
 # d$yhi gives upper limits
 require(ggplot2)
 p <- ggplot(d, aes(x=x, y=y, ymin=ylo, ymax=yhi, colour=as.factor(grp)))+
 geom_pointrange(position=position_dodge(-.4), size=.2)+
 geom_hline(yintercept = 0, linetype=1)+
 coord_flip(ylim = c(-lim,lim))+
 xlab('') + ylab(units)+ labs(title=hypo)  + theme_minimal()+ theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),legend.text=element_text(size=12), plot.title = element_text(size=14,hjust = 0.5), legend.title=element_blank())+
    geom_errorbar(aes(ymin=ylo, ymax=yhi),position=position_dodge(-.4),width=0,cex=1) + scale_colour_manual(values = c("#CCCCCC", "#6E8DAB", "#104E8B", "#000000")) + scale_x_discrete(labels=axlabs)
 return(p)
}



#########################################################################################
## drop the control
dta <- subset(dta, messenger != "ctrl")
dta_glob <- dta

man <- "mgt_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_mgt_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_mgt_woman) <- c("hhid","time","mgt_w")
man <- "dectime_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_dectime_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_dectime_woman) <- c("hhid","time","dectime_w")
man <- "decspace_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decspace_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decspace_woman) <-  c("hhid","time","decspace_w")
man <- "decstriga_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decstriga_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decstriga_woman) <-  c("hhid","time","decstriga_w")
man <- "decweed_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decweed_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decweed_woman) <-  c("hhid","time","decweed_w")



all_ind <- merge(merge(merge(merge(dta_ind_mgt_woman,dta_ind_dectime_woman),dta_ind_decspace_woman),dta_ind_decstriga_woman), dta_ind_decweed_woman )
all_hh <- aggregate(all_ind[c( "mgt_w"  ,  "dectime_w", "decspace_w", "decstriga_w","decweed_w")],list(all_ind$hhid),mean,na.rm=T)
names(all_hh)[names(all_hh) == 'Group.1'] <- 'hhid'

dta <- merge(dta_glob,all_hh,by="hhid")
dta$mgt_w[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$dectime_w[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$decspace_w[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$decstriga_w[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$decweed_w[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA


man <- "mgt_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_mgt_both_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_mgt_both_woman) <- c("hhid","time","mgt_j")
man <- "dectime_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_dectime_both_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_dectime_both_woman) <- c("hhid","time","dectime_j")
man <- "decspace_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decspace_both_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decspace_both_woman) <-  c("hhid","time","decspace_j")
man <- "decstriga_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decstriga_both_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decstriga_both_woman) <-  c("hhid","time","decstriga_j")
man <- "decweed_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decweed_both_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decweed_both_woman) <-  c("hhid","time","decweed_j")


all_ind <- merge(merge(merge(merge(dta_ind_mgt_both_woman,dta_ind_dectime_both_woman),dta_ind_decspace_both_woman),dta_ind_decstriga_both_woman), dta_ind_decweed_both_woman )
all_hh <- aggregate(all_ind[c( "mgt_j"  ,  "dectime_j", "decspace_j", "decstriga_j","decweed_j")],list(all_ind$hhid),mean,na.rm=T)
names(all_hh)[names(all_hh) == 'Group.1'] <- 'hhid'


dta <- merge(dta,all_hh,by="hhid")
dta$mgt_j[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$dectime_j[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$decspace_j[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$decstriga_j[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$decweed_j[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA


man <- "mgt_man"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_mgt_man <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_mgt_man) <- c("hhid","time","mgt_m")
man <- "dectime_man"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_dectime_man <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_dectime_man) <- c("hhid","time","dectime_m")
man <- "decspace_man"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decspace_man <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decspace_man) <-  c("hhid","time","decspace_m")
man <- "decstriga_man"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decstriga_man <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decstriga_man) <-  c("hhid","time","decstriga_m")
man <- "decweed_man"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decweed_man <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decweed_man) <-  c("hhid","time","decweed_m")


all_ind <- merge(merge(merge(merge(dta_ind_mgt_man,dta_ind_dectime_man),dta_ind_decspace_man),dta_ind_decstriga_man), dta_ind_decweed_man )
all_hh <- aggregate(all_ind[c( "mgt_m"  ,  "dectime_m", "decspace_m", "decstriga_m","decweed_m")],list(all_ind$hhid),mean,na.rm=T)
names(all_hh)[names(all_hh) == 'Group.1'] <- 'hhid'


dta <- merge(dta,all_hh,by="hhid")
dta$mgt_m[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$dectime_m[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$decspace_m[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$decstriga_m[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$decweed_m[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA

#################################################################################ADOPTION ##################################################################

man <- "dectime_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

out_sp1 =c("grp1days1","grp2days2","grp3days3","grp4days4", "grp5days5")
out_sp2 =c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5")
dta[out_sp1] <-  (dta[out_sp1] == 1)
dta[out_sp2] <-  (dta[out_sp2] == 1)

dta_ind_time <- merge(merge(reshape(dta[c("hhid","gender1", dec_vars)], varying = dec_vars,v.names="decide", idvar="hhid", direction="long"),reshape(dta[c(out_sp1,"hhid")], varying = out_sp1,v.names="outcome_sp1", idvar="hhid", direction="long"), by=c("hhid","time")),reshape(dta[c(out_sp2,"hhid")], varying = out_sp2,v.names="outcome_sp2", idvar="hhid", direction="long"), by=c("hhid","time"))

dta_ind_time$outcome <- ifelse(dta_ind_time$gender1=="woman",dta_ind_time$outcome_sp1, dta_ind_time$outcome_sp2)
dta_ind_time$adopt_time_w <- dta_ind_time$decide*dta_ind_time$outcome

man <- "decspace_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

out_sp1 = c("grp1a201","grp2b201","grp3c201","grp4d201","grp5e201")
out_sp2 = c("spouse2grp_sp1f201","spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201","spouse2grp5_sp5k201")


dta_ind_space <- merge(merge(reshape(dta[c("hhid","gender1", dec_vars)], varying = dec_vars,v.names="decide", idvar="hhid", direction="long"),reshape(dta[c(out_sp1,"hhid")], varying = out_sp1,v.names="outcome_sp1", idvar="hhid", direction="long"), by=c("hhid","time")),reshape(dta[c(out_sp2,"hhid")], varying = out_sp2,v.names="outcome_sp2", idvar="hhid", direction="long"), by=c("hhid","time"))

dta_ind_space$outcome <- ifelse(dta_ind_space$gender1=="woman",dta_ind_space$outcome_sp1, dta_ind_space$outcome_sp2)
dta_ind_space$adopt_space_w <- dta_ind_space$decide*dta_ind_space$outcome

man <- "decstriga_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

out_sp1 = c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241")
out_sp2 = c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241")

dta_ind_striga <- merge(merge(reshape(dta[c("hhid","gender1", dec_vars)], varying = dec_vars,v.names="decide", idvar="hhid", direction="long"),reshape(dta[c(out_sp1,"hhid")], varying = out_sp1,v.names="outcome_sp1", idvar="hhid", direction="long"), by=c("hhid","time")),reshape(dta[c(out_sp2,"hhid")], varying = out_sp2,v.names="outcome_sp2", idvar="hhid", direction="long"), by=c("hhid","time"))

dta_ind_striga$outcome <- ifelse(dta_ind_striga$gender1=="woman",dta_ind_striga$outcome_sp1, dta_ind_striga$outcome_sp2)
dta_ind_striga$adopt_striga_w <- dta_ind_striga$decide*dta_ind_striga$outcome

man <- "decweed_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

out_sp1 = c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26")
out_sp2 = c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26")
dta[out_sp1] <-  (dta[out_sp1] <= 3)
dta[out_sp2] <-  (dta[out_sp2] <= 3)

dta_ind_weed <- merge(merge(reshape(dta[c("hhid","gender1", dec_vars)], varying = dec_vars,v.names="decide", idvar="hhid", direction="long"),reshape(dta[c(out_sp1,"hhid")], varying = out_sp1,v.names="outcome_sp1", idvar="hhid", direction="long"), by=c("hhid","time")),reshape(dta[c(out_sp2,"hhid")], varying = out_sp2,v.names="outcome_sp2", idvar="hhid", direction="long"), by=c("hhid","time"))

dta_ind_weed$outcome <- ifelse(dta_ind_weed$gender1=="woman",dta_ind_weed$outcome_sp1, dta_ind_weed$outcome_sp2)
dta_ind_weed$adopt_weed_w <- dta_ind_weed$decide*dta_ind_weed$outcome



all_ind <- merge(merge(merge(dta_ind_time,dta_ind_space, by=c("hhid","time")),dta_ind_striga, by=c("hhid","time")),dta_ind_weed, by=c("hhid","time"))
all_hh <- aggregate(all_ind[c(  "adopt_time_w", "adopt_space_w", "adopt_striga_w","adopt_weed_w")],list(all_ind$hhid),mean,na.rm=T)
names(all_hh)[names(all_hh) == 'Group.1'] <- 'hhid'

dta <- merge(dta,all_hh,by="hhid")
dta$adopt_time_w[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$adopt_space_w[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$adopt_striga_w[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$adopt_weed_w[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA


man <- "dectime_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

out_sp1 =c("grp1days1","grp2days2","grp3days3","grp4days4", "grp5days5")
out_sp2 =c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5")

dta_ind_time <- merge(merge(reshape(dta[c("hhid","gender1", dec_vars)], varying = dec_vars,v.names="decide", idvar="hhid", direction="long"),reshape(dta[c(out_sp1,"hhid")], varying = out_sp1,v.names="outcome_sp1", idvar="hhid", direction="long"), by=c("hhid","time")),reshape(dta[c(out_sp2,"hhid")], varying = out_sp2,v.names="outcome_sp2", idvar="hhid", direction="long"), by=c("hhid","time"))

dta_ind_time$outcome <- ifelse(dta_ind_time$gender1=="woman",dta_ind_time$outcome_sp1, dta_ind_time$outcome_sp2)
dta_ind_time$adopt_time_j <- dta_ind_time$decide*dta_ind_time$outcome


man <- "decspace_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

out_sp1 = c("grp1a201","grp2b201","grp3c201","grp4d201","grp5e201")
out_sp2 = c("spouse2grp_sp1f201","spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201","spouse2grp5_sp5k201")


dta_ind_space <- merge(merge(reshape(dta[c("hhid","gender1", dec_vars)], varying = dec_vars,v.names="decide", idvar="hhid", direction="long"),reshape(dta[c(out_sp1,"hhid")], varying = out_sp1,v.names="outcome_sp1", idvar="hhid", direction="long"), by=c("hhid","time")),reshape(dta[c(out_sp2,"hhid")], varying = out_sp2,v.names="outcome_sp2", idvar="hhid", direction="long"), by=c("hhid","time"))

dta_ind_space$outcome <- ifelse(dta_ind_space$gender1=="woman",dta_ind_space$outcome_sp1, dta_ind_space$outcome_sp2)
dta_ind_space$adopt_space_j <- dta_ind_space$decide*dta_ind_space$outcome


man <- "decstriga_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

out_sp1 = c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241")
out_sp2 = c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241")

dta_ind_striga <- merge(merge(reshape(dta[c("hhid","gender1", dec_vars)], varying = dec_vars,v.names="decide", idvar="hhid", direction="long"),reshape(dta[c(out_sp1,"hhid")], varying = out_sp1,v.names="outcome_sp1", idvar="hhid", direction="long"), by=c("hhid","time")),reshape(dta[c(out_sp2,"hhid")], varying = out_sp2,v.names="outcome_sp2", idvar="hhid", direction="long"), by=c("hhid","time"))

dta_ind_striga$outcome <- ifelse(dta_ind_striga$gender1=="woman",dta_ind_striga$outcome_sp1, dta_ind_striga$outcome_sp2)
dta_ind_striga$adopt_striga_j <- dta_ind_striga$decide*dta_ind_striga$outcome


man <- "decweed_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

out_sp1 = c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26")
out_sp2 = c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26")


dta_ind_weed <- merge(merge(reshape(dta[c("hhid","gender1", dec_vars)], varying = dec_vars,v.names="decide", idvar="hhid", direction="long"),reshape(dta[c(out_sp1,"hhid")], varying = out_sp1,v.names="outcome_sp1", idvar="hhid", direction="long"), by=c("hhid","time")),reshape(dta[c(out_sp2,"hhid")], varying = out_sp2,v.names="outcome_sp2", idvar="hhid", direction="long"), by=c("hhid","time"))

dta_ind_weed$outcome <- ifelse(dta_ind_weed$gender1=="woman",dta_ind_weed$outcome_sp1, dta_ind_weed$outcome_sp2)
dta_ind_weed$adopt_weed_j <- dta_ind_weed$decide*dta_ind_weed$outcome



all_ind <- merge(merge(merge(dta_ind_time,dta_ind_space, by=c("hhid","time")),dta_ind_striga, by=c("hhid","time")),dta_ind_weed, by=c("hhid","time"))
all_hh <- aggregate(all_ind[c(  "adopt_time_j", "adopt_space_j", "adopt_striga_j","adopt_weed_j")],list(all_ind$hhid),mean,na.rm=T)
names(all_hh)[names(all_hh) == 'Group.1'] <- 'hhid'

dta <- merge(dta,all_hh,by="hhid")

dta$adopt_time_j[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$adopt_space_j[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$adopt_striga_j[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$adopt_weed_j[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA



man <- "dectime_man_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

out_sp1 =c("grp1days1","grp2days2","grp3days3","grp4days4", "grp5days5")
out_sp2 =c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5")


dta_ind_time <- merge(merge(reshape(dta[c("hhid","gender1", dec_vars)], varying = dec_vars,v.names="decide", idvar="hhid", direction="long"),reshape(dta[c(out_sp1,"hhid")], varying = out_sp1,v.names="outcome_sp1", idvar="hhid", direction="long"), by=c("hhid","time")),reshape(dta[c(out_sp2,"hhid")], varying = out_sp2,v.names="outcome_sp2", idvar="hhid", direction="long"), by=c("hhid","time"))

dta_ind_time$outcome <- ifelse(dta_ind_time$gender1=="woman",dta_ind_time$outcome_sp1, dta_ind_time$outcome_sp2)
dta_ind_time$adopt_time_m <- dta_ind_time$decide*dta_ind_time$outcome


man <- "decspace_man_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

out_sp1 = c("grp1a201","grp2b201","grp3c201","grp4d201","grp5e201")
out_sp2 = c("spouse2grp_sp1f201","spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201","spouse2grp5_sp5k201")


dta_ind_space <- merge(merge(reshape(dta[c("hhid","gender1", dec_vars)], varying = dec_vars,v.names="decide", idvar="hhid", direction="long"),reshape(dta[c(out_sp1,"hhid")], varying = out_sp1,v.names="outcome_sp1", idvar="hhid", direction="long"), by=c("hhid","time")),reshape(dta[c(out_sp2,"hhid")], varying = out_sp2,v.names="outcome_sp2", idvar="hhid", direction="long"), by=c("hhid","time"))

dta_ind_space$outcome <- ifelse(dta_ind_space$gender1=="woman",dta_ind_space$outcome_sp1, dta_ind_space$outcome_sp2)
dta_ind_space$adopt_space_m <- dta_ind_space$decide*dta_ind_space$outcome


man <- "decstriga_man_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

out_sp1 = c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241")
out_sp2 = c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241")

dta_ind_striga <- merge(merge(reshape(dta[c("hhid","gender1", dec_vars)], varying = dec_vars,v.names="decide", idvar="hhid", direction="long"),reshape(dta[c(out_sp1,"hhid")], varying = out_sp1,v.names="outcome_sp1", idvar="hhid", direction="long"), by=c("hhid","time")),reshape(dta[c(out_sp2,"hhid")], varying = out_sp2,v.names="outcome_sp2", idvar="hhid", direction="long"), by=c("hhid","time"))

dta_ind_striga$outcome <- ifelse(dta_ind_striga$gender1=="woman",dta_ind_striga$outcome_sp1, dta_ind_striga$outcome_sp2)
dta_ind_striga$adopt_striga_m <- dta_ind_striga$decide*dta_ind_striga$outcome


man <- "decweed_man_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")

out_sp1 = c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26")
out_sp2 = c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26")


dta_ind_weed <- merge(merge(reshape(dta[c("hhid","gender1", dec_vars)], varying = dec_vars,v.names="decide", idvar="hhid", direction="long"),reshape(dta[c(out_sp1,"hhid")], varying = out_sp1,v.names="outcome_sp1", idvar="hhid", direction="long"), by=c("hhid","time")),reshape(dta[c(out_sp2,"hhid")], varying = out_sp2,v.names="outcome_sp2", idvar="hhid", direction="long"), by=c("hhid","time"))

dta_ind_weed$outcome <- ifelse(dta_ind_weed$gender1=="woman",dta_ind_weed$outcome_sp1, dta_ind_weed$outcome_sp2)
dta_ind_weed$adopt_weed_m <- dta_ind_weed$decide*dta_ind_weed$outcome



all_ind <- merge(merge(merge(dta_ind_time,dta_ind_space, by=c("hhid","time")),dta_ind_striga, by=c("hhid","time")),dta_ind_weed, by=c("hhid","time"))
all_hh <- aggregate(all_ind[c(  "adopt_time_m", "adopt_space_m", "adopt_striga_m","adopt_weed_m")],list(all_ind$hhid),mean,na.rm=T)
names(all_hh)[names(all_hh) == 'Group.1'] <- 'hhid'

dta <- merge(dta,all_hh,by="hhid")

dta$adopt_time_m[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$adopt_space_m[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$adopt_striga_m[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$adopt_weed_m[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA


#########################
man <- "decDAP_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decDAP <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decDAP) <- c("hhid","time","decDAP_w")
man <- "decUrea_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decUREA <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decUREA) <- c("hhid","time","decUREA_w")
man <- "decOrg_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decOrg <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decOrg) <-  c("hhid","time","decOrg_w")
man <- "decHybrid_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decHybrid <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decHybrid) <-  c("hhid","time","decHybrid_w")
man <- "decOPV_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decOPV <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decOPV) <-  c("hhid","time","decOPV_w")



all_ind <- merge(merge(merge(merge(dta_ind_decDAP,dta_ind_decUREA),dta_ind_decOrg),dta_ind_decHybrid), dta_ind_decOPV )
all_hh <- aggregate(all_ind[c( "decDAP_w"  ,  "decUREA_w", "decOrg_w", "decHybrid_w","decOPV_w")],list(all_ind$hhid),mean,na.rm=T)
names(all_hh)[names(all_hh) == 'Group.1'] <- 'hhid'

dta <- merge(dta,all_hh,by="hhid")
dta$decDAP_w[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$decUREA_w[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$decOrg_w[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$decHybrid_w[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$decOPV_w[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA

man <- "decDAP_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decDAP <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decDAP) <- c("hhid","time","decDAP_j")
man <- "decUrea_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decUREA <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decUREA) <- c("hhid","time","decUREA_j")
man <- "decOrg_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decOrg <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decOrg) <-  c("hhid","time","decOrg_j")
man <- "decHybrid_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decHybrid <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decHybrid) <-  c("hhid","time","decHybrid_j")
man <- "decOPV_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decOPV <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decOPV) <-  c("hhid","time","decOPV_j")



all_ind <- merge(merge(merge(merge(dta_ind_decDAP,dta_ind_decUREA),dta_ind_decOrg),dta_ind_decHybrid), dta_ind_decOPV )
all_hh <- aggregate(all_ind[c( "decDAP_j"  ,  "decUREA_j", "decOrg_j", "decHybrid_j","decOPV_j")],list(all_ind$hhid),mean,na.rm=T)
names(all_hh)[names(all_hh) == 'Group.1'] <- 'hhid'

dta <- merge(dta,all_hh,by="hhid")

dta$decDAP_j[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$decUREA_j[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$decOrg_j[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$decHybrid_j[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$decOPV_j[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA

man <- "decDAP_man_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decDAP <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decDAP) <- c("hhid","time","decDAP_m")
man <- "decUrea_man_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decUREA <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decUREA) <- c("hhid","time","decUREA_m")
man <- "decOrg_man_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decOrg <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decOrg) <-  c("hhid","time","decOrg_m")
man <- "decHybrid_man_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decHybrid <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decHybrid) <-  c("hhid","time","decHybrid_m")
man <- "decOPV_man_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decOPV <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decOPV) <-  c("hhid","time","decOPV_m")



all_ind <- merge(merge(merge(merge(dta_ind_decDAP,dta_ind_decUREA),dta_ind_decOrg),dta_ind_decHybrid), dta_ind_decOPV)
all_hh <- aggregate(all_ind[c( "decDAP_m"  ,  "decUREA_m", "decOrg_m", "decHybrid_m","decOPV_m")],list(all_ind$hhid),mean,na.rm=T)
names(all_hh)[names(all_hh) == 'Group.1'] <- 'hhid'

dta <- merge(dta,all_hh,by="hhid")

dta$decDAP_m[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$decUREA_m[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$decOrg_m[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$decHybrid_m[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$decOPV_m[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA

################################################################# PRODUCTION OUTCOMES 
## we first need to construct, at the plot level, an indicator of management

man <- "mgt_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_mgt_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_mgt_woman) <- c("hhid","time","mgt_woman")
man <- "dectime_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_dectime_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_dectime_woman) <- c("hhid","time","dectime_woman")
man <- "decspace_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decspace_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decspace_woman) <-  c("hhid","time","decspace_woman")
man <- "decstriga_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decstriga_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decstriga_woman) <-  c("hhid","time","decstriga_woman")
man <- "decweed_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decweed_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decweed_woman) <-  c("hhid","time","decweed_woman")

all_ind <- merge(merge(merge(merge(dta_ind_mgt_woman,dta_ind_dectime_woman),dta_ind_decspace_woman),dta_ind_decstriga_woman), dta_ind_decweed_woman )
### arbitrary threshold allert!!! women managed plots are defined as plots where 3 or more out of 5 decision are made by the woman alone as reported by the woman
all_ind$women_decisions <- rowSums(all_ind[,3:7]) >= 3

### production
dec_vars <- paste(paste("prod",paste("_pl",1:5, sep=""), sep=""), "sp1",sep="_")
dta_ind_prod_sp1 <- reshape(cbind(dta_glob[c("hhid","person_interviewed", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_prod_sp1) <- c("hhid","person_interviewed","time","prod_sp1")
dec_vars <- paste(paste("prod",paste("_pl",1:5, sep=""), sep=""), "sp2",sep="_")
dta_ind_prod_sp2 <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_prod_sp2) <- c("hhid","time","prod_sp2")
prod_ind <- merge(dta_ind_prod_sp1,dta_ind_prod_sp2, by=c("hhid","time"))
prod_ind$prod <- ifelse(prod_ind$person_interviewed=="woman",prod_ind$prod_sp1,prod_ind$prod_sp2)

### area
dec_vars <- paste(paste("area",paste("_pl",1:5, sep=""), sep=""), "sp1",sep="_")
dta_ind_area_sp1 <- reshape(cbind(dta_glob[c("hhid","person_interviewed", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_area_sp1) <- c("hhid","person_interviewed","time","area_sp1")
dec_vars <- paste(paste("area",paste("_pl",1:5, sep=""), sep=""), "sp2",sep="_")
dta_ind_area_sp2 <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_area_sp2) <- c("hhid","time","area_sp2")
area_ind <- merge(dta_ind_area_sp1,dta_ind_area_sp2, by=c("hhid","time"))
area_ind$area <- ifelse(area_ind$person_interviewed=="woman",area_ind$area_sp1,area_ind$area_sp2)

### yield_better
dec_vars <- paste("yield_better_sp1",paste("_pl",1:5, sep=""), sep="")
dta_ind_yield_better_sp1 <- reshape(cbind(dta_glob[c("hhid","person_interviewed", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_yield_better_sp1) <- c("hhid","person_interviewed","time","yield_better_sp1")
dec_vars <- paste("yield_better_sp2",paste("_pl",1:5, sep=""), sep="")
dta_ind_yield_better_sp2 <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_yield_better_sp2) <- c("hhid","time","yield_better_sp2")
yield_better_ind <- merge(dta_ind_yield_better_sp1,dta_ind_yield_better_sp2, by=c("hhid","time"))
yield_better_ind$yield_better <- ifelse(yield_better_ind$person_interviewed=="woman",yield_better_ind$yield_better_sp1,yield_better_ind$yield_better_sp2)

prod_ind <- merge(all_ind,merge(prod_ind,merge(area_ind,yield_better_ind, by=c("hhid","time")), by=c("hhid","time")), by=c("hhid","time"))[c("hhid","women_decisions","prod","area","yield_better")]

prod_hh <- aggregate(cbind(prod_ind$prod,prod_ind$women_decisions*prod_ind$prod), list(prod_ind$hhid), sum, na.rm=T)
names(prod_hh) <- c("hhid", "prod_tot","prod")
prod_hh$prod_share <- prod_hh$prod/prod_hh$prod_tot
prod_hh$prod_tot <- NULL
area_hh <- aggregate(cbind(prod_ind$area,prod_ind$women_decisions*prod_ind$area), list(prod_ind$hhid), sum, na.rm=T)
names(area_hh) <- c("hhid", "area_tot","area")
area_hh$area_share <- area_hh$area/area_hh$area_tot
area_hh$area_tot <- NULL
yield_better_hh <- aggregate(prod_ind$women_decisions*prod_ind$yield_better, list(prod_ind$hhid), mean, na.rm=T)
names(yield_better_hh) <- c("hhid", "yield_better")
yield_hh <- aggregate(cbind(prod_ind$women_decisions*prod_ind$prod,prod_ind$women_decisions*prod_ind$area), list(prod_ind$hhid), sum, na.rm=T)
names(yield_hh) <- c("hhid", "prod","area")
yield_hh$yield <- yield_hh$prod/yield_hh$area
yield_hh$yield[yield_hh$prod==0] <- 0 
yield_hh$yield[yield_hh$area==0] <- 0 
yield_hh$prod <- NULL
yield_hh$area <- NULL

dta <- merge(dta,merge(merge(merge(prod_hh,area_hh, by="hhid"),yield_better_hh, by="hhid"),yield_hh, by="hhid"),by="hhid")
names(dta)[names(dta) == "prod"] <- "prod_w"
names(dta)[names(dta) == "prod_share"] <- "prod_share_w"
names(dta)[names(dta) == "area"]  <- "area_w"
names(dta)[names(dta) == "area_share"]  <- "area_share_w"
names(dta)[names(dta) == "yield_better"]  <- "yield_better_w"
names(dta)[names(dta) == "yield"] <- "yield_w"

dta$prod_w[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$prod_share_w[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$area_w[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$area_share_w[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$yield_better_w[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$yield_w[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA

man <- "mgt_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_mgt_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_mgt_woman) <- c("hhid","time","mgt_woman")
man <- "dectime_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_dectime_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_dectime_woman) <- c("hhid","time","dectime_woman")
man <- "decspace_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decspace_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decspace_woman) <-  c("hhid","time","decspace_woman")
man <- "decstriga_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decstriga_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decstriga_woman) <-  c("hhid","time","decstriga_woman")
man <- "decweed_both_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decweed_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decweed_woman) <-  c("hhid","time","decweed_woman")

all_ind <- merge(merge(merge(merge(dta_ind_mgt_woman,dta_ind_dectime_woman),dta_ind_decspace_woman),dta_ind_decstriga_woman), dta_ind_decweed_woman )
### arbitrary threshold allert!!! women managed plots are defined as plots where 3 or more out of 5 decision are made by the woman alone as reported by the woman
all_ind$women_decisions <- rowSums(all_ind[,3:7]) >= 3

### production
dec_vars <- paste(paste("prod",paste("_pl",1:5, sep=""), sep=""), "sp1",sep="_")
dta_ind_prod_sp1 <- reshape(cbind(dta_glob[c("hhid","person_interviewed", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_prod_sp1) <- c("hhid","person_interviewed","time","prod_sp1")
dec_vars <- paste(paste("prod",paste("_pl",1:5, sep=""), sep=""), "sp2",sep="_")
dta_ind_prod_sp2 <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_prod_sp2) <- c("hhid","time","prod_sp2")
prod_ind <- merge(dta_ind_prod_sp1,dta_ind_prod_sp2, by=c("hhid","time"))
prod_ind$prod <- ifelse(prod_ind$person_interviewed=="woman",prod_ind$prod_sp1,prod_ind$prod_sp2)

### area
dec_vars <- paste(paste("area",paste("_pl",1:5, sep=""), sep=""), "sp1",sep="_")
dta_ind_area_sp1 <- reshape(cbind(dta_glob[c("hhid","person_interviewed", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_area_sp1) <- c("hhid","person_interviewed","time","area_sp1")
dec_vars <- paste(paste("area",paste("_pl",1:5, sep=""), sep=""), "sp2",sep="_")
dta_ind_area_sp2 <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_area_sp2) <- c("hhid","time","area_sp2")
area_ind <- merge(dta_ind_area_sp1,dta_ind_area_sp2, by=c("hhid","time"))
area_ind$area <- ifelse(area_ind$person_interviewed=="woman",area_ind$area_sp1,area_ind$area_sp2)

### yield_better
dec_vars <- paste("yield_better_sp1",paste("_pl",1:5, sep=""), sep="")
dta_ind_yield_better_sp1 <- reshape(cbind(dta_glob[c("hhid","person_interviewed", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_yield_better_sp1) <- c("hhid","person_interviewed","time","yield_better_sp1")
dec_vars <- paste("yield_better_sp2",paste("_pl",1:5, sep=""), sep="")
dta_ind_yield_better_sp2 <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_yield_better_sp2) <- c("hhid","time","yield_better_sp2")
yield_better_ind <- merge(dta_ind_yield_better_sp1,dta_ind_yield_better_sp2, by=c("hhid","time"))
yield_better_ind$yield_better <- ifelse(yield_better_ind$person_interviewed=="woman",yield_better_ind$yield_better_sp1,yield_better_ind$yield_better_sp2)

prod_ind <- merge(all_ind,merge(prod_ind,merge(area_ind,yield_better_ind, by=c("hhid","time")), by=c("hhid","time")), by=c("hhid","time"))[c("hhid","women_decisions","prod","area","yield_better")]

prod_hh <- aggregate(cbind(prod_ind$prod,prod_ind$women_decisions*prod_ind$prod), list(prod_ind$hhid), sum, na.rm=T)
names(prod_hh) <- c("hhid", "prod_tot","prod")
prod_hh$prod_share <- prod_hh$prod/prod_hh$prod_tot
prod_hh$prod_tot <- NULL
area_hh <- aggregate(cbind(prod_ind$area,prod_ind$women_decisions*prod_ind$area), list(prod_ind$hhid), sum, na.rm=T)
names(area_hh) <- c("hhid", "area_tot","area")
area_hh$area_share <- area_hh$area/area_hh$area_tot
area_hh$area_tot <- NULL
yield_better_hh <- aggregate(prod_ind$women_decisions*prod_ind$yield_better, list(prod_ind$hhid), mean, na.rm=T)
names(yield_better_hh) <- c("hhid", "yield_better")
yield_hh <- aggregate(cbind(prod_ind$women_decisions*prod_ind$prod,prod_ind$women_decisions*prod_ind$area), list(prod_ind$hhid), sum, na.rm=T)
names(yield_hh) <- c("hhid", "prod","area")
yield_hh$yield <- yield_hh$prod/yield_hh$area
yield_hh$yield[yield_hh$prod==0] <- 0 
yield_hh$yield[yield_hh$area==0] <- 0 
yield_hh$prod <- NULL
yield_hh$area <- NULL

dta <- merge(dta,merge(merge(merge(prod_hh,area_hh, by="hhid"),yield_better_hh, by="hhid"),yield_hh, by="hhid"),by="hhid")
names(dta)[names(dta) == "prod"] <- "prod_j"
names(dta)[names(dta) == "prod_share"] <- "prod_share_j"
names(dta)[names(dta) == "area"]  <- "area_j"
names(dta)[names(dta) == "area_share"]  <- "area_share_j"
names(dta)[names(dta) == "yield_better"]  <- "yield_better_j"
names(dta)[names(dta) == "yield"] <- "yield_j"

dta$prod_j[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$prod_share_j[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$area_j[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$area_share_j[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$yield_better_j[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$yield_j[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA

man <- "mgt_man_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_mgt_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_mgt_woman) <- c("hhid","time","mgt_woman")
man <- "dectime_man_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_dectime_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_dectime_woman) <- c("hhid","time","dectime_woman")
man <- "decspace_man_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decspace_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decspace_woman) <-  c("hhid","time","decspace_woman")
man <- "decstriga_man_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decstriga_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decstriga_woman) <-  c("hhid","time","decstriga_woman")
man <- "decweed_man_woman"
dec_vars <- paste(man,paste("_pl",1:5, sep=""), sep="")
dta_ind_decweed_woman <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_decweed_woman) <-  c("hhid","time","decweed_woman")

all_ind <- merge(merge(merge(merge(dta_ind_mgt_woman,dta_ind_dectime_woman),dta_ind_decspace_woman),dta_ind_decstriga_woman), dta_ind_decweed_woman )
### arbitrary threshold allert!!! women managed plots are defined as plots where 3 or more out of 5 decision are made by the woman alone as reported by the woman
all_ind$women_decisions <- rowSums(all_ind[,3:7]) >= 3

### production
dec_vars <- paste(paste("prod",paste("_pl",1:5, sep=""), sep=""), "sp1",sep="_")
dta_ind_prod_sp1 <- reshape(cbind(dta_glob[c("hhid","person_interviewed", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_prod_sp1) <- c("hhid","person_interviewed","time","prod_sp1")
dec_vars <- paste(paste("prod",paste("_pl",1:5, sep=""), sep=""), "sp2",sep="_")
dta_ind_prod_sp2 <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_prod_sp2) <- c("hhid","time","prod_sp2")
prod_ind <- merge(dta_ind_prod_sp1,dta_ind_prod_sp2, by=c("hhid","time"))
prod_ind$prod <- ifelse(prod_ind$person_interviewed=="woman",prod_ind$prod_sp1,prod_ind$prod_sp2)

### area
dec_vars <- paste(paste("area",paste("_pl",1:5, sep=""), sep=""), "sp1",sep="_")
dta_ind_area_sp1 <- reshape(cbind(dta_glob[c("hhid","person_interviewed", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_area_sp1) <- c("hhid","person_interviewed","time","area_sp1")
dec_vars <- paste(paste("area",paste("_pl",1:5, sep=""), sep=""), "sp2",sep="_")
dta_ind_area_sp2 <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_area_sp2) <- c("hhid","time","area_sp2")
area_ind <- merge(dta_ind_area_sp1,dta_ind_area_sp2, by=c("hhid","time"))
area_ind$area <- ifelse(area_ind$person_interviewed=="woman",area_ind$area_sp1,area_ind$area_sp2)

### yield_better
dec_vars <- paste("yield_better_sp1",paste("_pl",1:5, sep=""), sep="")
dta_ind_yield_better_sp1 <- reshape(cbind(dta_glob[c("hhid","person_interviewed", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_yield_better_sp1) <- c("hhid","person_interviewed","time","yield_better_sp1")
dec_vars <- paste("yield_better_sp2",paste("_pl",1:5, sep=""), sep="")
dta_ind_yield_better_sp2 <- reshape(cbind(dta_glob[c("hhid", dec_vars)]), varying = dec_vars,v.names="decide", idvar="hhid", direction="long")
names(dta_ind_yield_better_sp2) <- c("hhid","time","yield_better_sp2")
yield_better_ind <- merge(dta_ind_yield_better_sp1,dta_ind_yield_better_sp2, by=c("hhid","time"))
yield_better_ind$yield_better <- ifelse(yield_better_ind$person_interviewed=="woman",yield_better_ind$yield_better_sp1,yield_better_ind$yield_better_sp2)

prod_ind <- merge(all_ind,merge(prod_ind,merge(area_ind,yield_better_ind, by=c("hhid","time")), by=c("hhid","time")), by=c("hhid","time"))[c("hhid","women_decisions","prod","area","yield_better")]

prod_hh <- aggregate(cbind(prod_ind$prod,prod_ind$women_decisions*prod_ind$prod), list(prod_ind$hhid), sum, na.rm=T)
names(prod_hh) <- c("hhid", "prod_tot","prod")
prod_hh$prod_share <- prod_hh$prod/prod_hh$prod_tot
prod_hh$prod_tot <- NULL
area_hh <- aggregate(cbind(prod_ind$area,prod_ind$women_decisions*prod_ind$area), list(prod_ind$hhid), sum, na.rm=T)
names(area_hh) <- c("hhid", "area_tot","area")
area_hh$area_share <- area_hh$area/area_hh$area_tot
area_hh$area_tot <- NULL
yield_better_hh <- aggregate(prod_ind$women_decisions*prod_ind$yield_better, list(prod_ind$hhid), mean, na.rm=T)
names(yield_better_hh) <- c("hhid", "yield_better")
yield_hh <- aggregate(cbind(prod_ind$women_decisions*prod_ind$prod,prod_ind$women_decisions*prod_ind$area), list(prod_ind$hhid), sum, na.rm=T)
names(yield_hh) <- c("hhid", "prod","area")
yield_hh$yield <- yield_hh$prod/yield_hh$area
yield_hh$yield[yield_hh$prod==0] <- 0 
yield_hh$yield[yield_hh$area==0] <- 0 
yield_hh$prod <- NULL
yield_hh$area <- NULL

dta <- merge(dta,merge(merge(merge(prod_hh,area_hh, by="hhid"),yield_better_hh, by="hhid"),yield_hh, by="hhid"),by="hhid")

names(dta)[names(dta) == "prod"] <- "prod_m"
names(dta)[names(dta) == "prod_share"] <- "prod_share_m"
names(dta)[names(dta) == "area"]  <- "area_m"
names(dta)[names(dta) == "area_share"]  <- "area_share_m"
names(dta)[names(dta) == "yield_better"]  <- "yield_better_m"
names(dta)[names(dta) == "yield"] <- "yield_m"


dta$prod_m[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$prod_share_m[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$area_m[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$area_share_m[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$yield_better_m[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA
dta$yield_m[dta$interview_status=='one individual interviewed' & dta$gender1 == 'man'] <- NA



names(dta)[names(dta) == "sold_woman"] <- "sold_w"
names(dta)[names(dta) == "nr_bags_sold_woman"] <- "nr_bags_sold_w"
names(dta)[names(dta) == "sold_both_woman"]  <- "sold_j"
names(dta)[names(dta) == "nr_bags_sold_both_woman"]  <- "nr_bags_sold_j"
names(dta)[names(dta) == "sold_man_woman"]  <- "sold_m"
names(dta)[names(dta) == "nr_bags_sold_man_woman"] <- "nr_bags_sold_m"


#################################################################################ANALYSIS##########################################
outcomes_ind <- c("know_space","know_combine","know_weed","know_armyworm","mgt", "dectime", "decspace", "decstriga","decweed","adopt_time", "adopt_space", "adopt_striga","adopt_weed","decDAP",  "decUREA", "decOrg", "decHybrid","decOPV","prod", "prod_share", "area", "area_share", "yield_better", "yield", "sold", "nr_bags_sold")
out_w <- array(NA,c(length(outcomes_ind),13,2))
out_m <- array(NA,c(length(outcomes_ind),13,2))
out_j <- array(NA,c(length(outcomes_ind),13,2))
#relevel treatments to make sure male is control group
dta$recipient <- as.factor(dta$recipient)
dta$messenger <- as.factor(dta$messenger)
treatment <- "recipient*messenger"
dta <- within(dta, recipient <- relevel(recipient, ref = "male"))
dta <- within(dta, messenger <- relevel(messenger, ref = "male"))
i <- 1


for (outcome in outcomes_ind) {

out_w[i,1,1] <- mean(unlist(dta[dta$recipient=="male",][paste(outcome,"w",sep="_")]), na.rm=T)
out_m[i,1,1] <- mean(unlist(dta[dta$recipient=="male",][paste(outcome,"m",sep="_")]), na.rm=T)
out_j[i,1,1] <- mean(unlist(dta[dta$recipient=="male",][paste(outcome,"j",sep="_")]), na.rm=T)

out_w[i,2,1] <- sd(unlist(dta[dta$recipient=="male",][paste(outcome,"w",sep="_")]), na.rm=T)
out_m[i,2,1] <- sd(unlist(dta[dta$recipient=="male",][paste(outcome,"m",sep="_")]), na.rm=T)
out_j[i,2,1] <- sd(unlist(dta[dta$recipient=="male",][paste(outcome,"j",sep="_")]), na.rm=T)

out_w[i,1,2] <- mean(unlist(dta[dta$messenger=="male",][paste(outcome,"w",sep="_")]), na.rm=T)
out_m[i,1,2] <- mean(unlist(dta[dta$messenger=="male",][paste(outcome,"m",sep="_")]), na.rm=T)
out_j[i,1,2] <- mean(unlist(dta[dta$messenger=="male",][paste(outcome,"j",sep="_")]), na.rm=T)

out_w[i,2,2] <- sd(unlist(dta[dta$messenger=="male",][paste(outcome,"w",sep="_")]), na.rm=T)
out_m[i,2,2] <- sd(unlist(dta[dta$messenger=="male",][paste(outcome,"m",sep="_")]), na.rm=T)
out_j[i,2,2] <- sd(unlist(dta[dta$messenger=="male",][paste(outcome,"j",sep="_")]), na.rm=T)

mod_w <- lm(as.formula(paste(paste(outcome,"w",sep="_"),treatment, sep="~")),data=dta)
mod_m <- lm(as.formula(paste(paste(outcome,"m",sep="_"),treatment, sep="~")),data=dta)
mod_j <- lm(as.formula(paste(paste(outcome,"j",sep="_"),treatment, sep="~")),data=dta)
#out[,,1] collects results for T1
#couple treatment
#coef estimate, se and p-value
out_w[i,3:5,1] <- summary(mod_w)$coefficients[2,c(1:2,4)]
out_m[i,3:5,1] <- summary(mod_m)$coefficients[2,c(1:2,4)]
out_j[i,3:5,1] <- summary(mod_j)$coefficients[2,c(1:2,4)]

#lower and upper conf limits
out_w[i,6:7,1] <- confint(mod_w)[2,]
out_m[i,6:7,1] <- confint(mod_m)[2,]
out_j[i,6:7,1] <- confint(mod_j)[2,]

#woman treatment
#coef estimate, se and p-value
out_w[i,8:10,1] <- summary(mod_w)$coefficients[3,c(1:2,4)]
out_m[i,8:10,1] <- summary(mod_m)$coefficients[3,c(1:2,4)]
out_j[i,8:10,1] <- summary(mod_j)$coefficients[3,c(1:2,4)]
#lower and upper conf limits
out_w[i,11:12,1] <- confint(mod_w)[3,]
out_m[i,11:12,1] <- confint(mod_m)[3,]
out_j[i,11:12,1] <- confint(mod_j)[3,]
#t-test
out_w[i,13,1] <- linearHypothesis(mod_w,"recipientcouple = recipientfemale")[2,6]
out_m[i,13,1] <- linearHypothesis(mod_m,"recipientcouple = recipientfemale")[2,6]
out_j[i,13,1] <- linearHypothesis(mod_j,"recipientcouple = recipientfemale")[2,6]

#out[,,2] collects results for T2
#couple treatment
#coef estimate, se and p-value
out_w[i,3:5,2] <- summary(mod_w)$coefficients[4,c(1:2,4)]
out_m[i,3:5,2] <- summary(mod_m)$coefficients[4,c(1:2,4)]
out_j[i,3:5,2] <- summary(mod_j)$coefficients[4,c(1:2,4)]

#lower and upper conf limits
out_w[i,6:7,2] <- confint(mod_w)[4,]
out_m[i,6:7,2] <- confint(mod_m)[4,]
out_j[i,6:7,2] <- confint(mod_j)[4,]

#woman treatment
#coef estimate, se and p-value
out_w[i,8:10,2] <- summary(mod_w)$coefficients[5,c(1:2,4)]
out_m[i,8:10,2] <- summary(mod_m)$coefficients[5,c(1:2,4)]
out_j[i,8:10,2] <- summary(mod_j)$coefficients[5,c(1:2,4)]
#lower and upper conf limits
out_w[i,11:12,2] <- confint(mod_w)[5,]
out_m[i,11:12,2] <- confint(mod_m)[5,]
out_j[i,11:12,2] <- confint(mod_j)[5,]
#t-test
out_w[i,13,2] <- linearHypothesis(mod_w,"messengercouple = messengerfemale")[2,6]
out_m[i,13,2] <- linearHypothesis(mod_m,"messengercouple = messengerfemale")[2,6]
out_j[i,13,2] <- linearHypothesis(mod_j,"messengercouple = messengerfemale")[2,6]


i <- i + 1
}
rownames(out_w) <- outcomes_ind 
rownames(out_j) <- outcomes_ind 
rownames(out_m) <- outcomes_ind 

colnames(out_w) <- c("ctrl_mean","ctrl_sd","Tcouple_mean","Tcouple_se","Tcouple_p","Tcouple_conf_L","Tcouple_conf_U","Twoman_mean","Twoman_se","Twoman_p","Twoman_conf_L","Twoman_conf_U","p_comp_Ts")
colnames(out_j) <- c("ctrl_mean","ctrl_sd","Tcouple_mean","Tcouple_se","Tcouple_p","Tcouple_conf_L","Tcouple_conf_U","Twoman_mean","Twoman_se","Twoman_p","Twoman_conf_L","Twoman_conf_U","p_comp_Ts")
colnames(out_m) <- c("ctrl_mean","ctrl_sd","Tcouple_mean","Tcouple_se","Tcouple_p","Tcouple_conf_L","Tcouple_conf_U","Twoman_mean","Twoman_se","Twoman_p","Twoman_conf_L","Twoman_conf_U","p_comp_Ts")

### now the indices; we have 4: knowledge, decision making, adoption, input use, adoption, sales
know_ind_res_w <- FW_index(treatment, paste(c("know_space", "know_combine", "know_weed"),"w",sep="_"),dta)
know_ind_res_j <- FW_index(treatment, paste(c("know_space", "know_combine", "know_weed"),"j",sep="_"),dta)
know_ind_res_m <- FW_index(treatment, paste(c("know_space", "know_combine", "know_weed"),"m",sep="_"),dta)

dec_ind_res_w <- FW_index(treatment, paste(c("mgt", "dectime", "decspace", "decstriga","decweed"),"w",sep="_"),dta)
dec_ind_res_j <- FW_index(treatment, paste(c("mgt", "dectime", "decspace", "decstriga","decweed"),"j",sep="_"),dta)
dec_ind_res_m <- FW_index(treatment, paste(c("mgt", "dectime", "decspace", "decstriga","decweed"),"m",sep="_"),dta)

adopt_ind_res_w <- FW_index(treatment, paste(c("adopt_time", "adopt_space", "adopt_striga","adopt_weed"),"w",sep="_"),dta)
adopt_ind_res_j <- FW_index(treatment, paste(c("adopt_time", "adopt_space", "adopt_striga","adopt_weed"),"j",sep="_"),dta)
adopt_ind_res_m <- FW_index(treatment, paste(c("adopt_time", "adopt_space", "adopt_striga","adopt_weed"),"m",sep="_"),dta)

input_ind_res_w <- FW_index(treatment, paste(c("decDAP",  "decUREA", "decOrg", "decHybrid","decOPV"),"w",sep="_"),dta)
input_ind_res_j <- FW_index(treatment, paste(c("decDAP",  "decUREA", "decOrg", "decHybrid","decOPV"),"j",sep="_"),dta)
input_ind_res_m <- FW_index(treatment, paste(c("decDAP",  "decUREA", "decOrg", "decHybrid","decOPV"),"m",sep="_"),dta)


outcome_ind_res_w <- FW_index(treatment, paste(c("yield_better", "yield", "sold", "nr_bags_sold"),"w",sep="_"),dta)
outcome_ind_res_j <- FW_index(treatment, paste(c("yield_better", "yield", "sold", "nr_bags_sold"),"j",sep="_"),dta)
outcome_ind_res_m <- FW_index(treatment, paste(c("yield_better", "yield", "sold", "nr_bags_sold"),"m",sep="_"),dta)


all_ind_res_w <- FW_index(treatment, paste(c("know_space", "know_combine", "know_weed","mgt", "dectime", "decspace", "decstriga","decweed","adopt_time", "adopt_space", "adopt_striga","adopt_weed","decDAP",  "decUREA", "decOrg", "decHybrid","decOPV", "yield_better", "yield", "sold", "nr_bags_sold" ),"w",sep="_"),dta)
all_ind_res_j <- FW_index(treatment, paste(c("know_space", "know_combine", "know_weed","mgt", "dectime", "decspace", "decstriga","decweed","adopt_time", "adopt_space", "adopt_striga","adopt_weed","decDAP",  "decUREA", "decOrg", "decHybrid","decOPV", "yield_better", "yield", "sold", "nr_bags_sold" ),"j",sep="_"),dta)
################## dec_weed and adopt_weed
all_ind_res_m <- FW_index(treatment, paste(c("know_space", "know_combine", "know_weed","mgt", "dectime", "decspace", "decstriga","decweed","adopt_time", "adopt_space", "adopt_striga","adopt_weed","decDAP",  "decUREA", "decOrg", "decHybrid","decOPV", "yield_better", "yield", "sold", "nr_bags_sold" ),"m",sep="_"),dta)

### put this also all in matrices
ind_out_w <- array(NA,c(6,13,2))
ind_out_m <- array(NA,c(6,13,2))
ind_out_j <- array(NA,c(6,13,2))

### overall index
ind_out_w[1,1,1] <- mean(all_ind_res_w[[2]]$index[all_ind_res_w[[2]]$recipient=="male"])
ind_out_j[1,1,1] <- mean(all_ind_res_j[[2]]$index[all_ind_res_j[[2]]$recipient=="male"])
ind_out_m[1,1,1] <- mean(all_ind_res_m[[2]]$index[all_ind_res_m[[2]]$recipient=="male"])

ind_out_w[1,2,1] <- sd(all_ind_res_w[[2]]$index[all_ind_res_w[[2]]$recipient=="male"])
ind_out_j[1,2,1] <- sd(all_ind_res_j[[2]]$index[all_ind_res_j[[2]]$recipient=="male"])
ind_out_m[1,2,1] <- sd(all_ind_res_m[[2]]$index[all_ind_res_m[[2]]$recipient=="male"])

ind_out_w[1,1,2] <- mean(all_ind_res_w[[2]]$index[all_ind_res_w[[2]]$messenger=="male"])
ind_out_j[1,1,2] <- mean(all_ind_res_j[[2]]$index[all_ind_res_j[[2]]$messenger=="male"])
ind_out_m[1,1,2] <- mean(all_ind_res_m[[2]]$index[all_ind_res_m[[2]]$messenger=="male"])

ind_out_w[1,2,2] <- sd(all_ind_res_w[[2]]$index[all_ind_res_w[[2]]$messenger=="male"])
ind_out_j[1,2,2] <- sd(all_ind_res_j[[2]]$index[all_ind_res_j[[2]]$messenger=="male"])
ind_out_m[1,2,2] <- sd(all_ind_res_m[[2]]$index[all_ind_res_m[[2]]$messenger=="male"])

ind_out_w[1,3:5,1] <-  summary(all_ind_res_w[[1]])$coefficients[2,c(1:2,4)]
ind_out_j[1,3:5,1] <-  summary(all_ind_res_j[[1]])$coefficients[2,c(1:2,4)]
ind_out_m[1,3:5,1] <-  summary(all_ind_res_m[[1]])$coefficients[2,c(1:2,4)]

ind_out_w[1,6:7,1] <- confint(all_ind_res_w[[1]])[2,]
ind_out_j[1,6:7,1] <- confint(all_ind_res_j[[1]])[2,]
ind_out_m[1,6:7,1] <- confint(all_ind_res_m[[1]])[2,]

#woman treatment
#coef estimate, se and p-value
ind_out_w[1,8:10,1] <- summary(all_ind_res_w[[1]])$coefficients[3,c(1:2,4)]
ind_out_j[1,8:10,1] <- summary(all_ind_res_j[[1]])$coefficients[3,c(1:2,4)]
ind_out_m[1,8:10,1] <- summary(all_ind_res_m[[1]])$coefficients[3,c(1:2,4)]

ind_out_w[1,11:12,1] <- confint(all_ind_res_w[[1]])[3,]
ind_out_j[1,11:12,1] <- confint(all_ind_res_j[[1]])[3,]
ind_out_m[1,11:12,1] <- confint(all_ind_res_m[[1]])[3,]

ind_out_w[1,13,1] <- linearHypothesis(all_ind_res_w[[1]],"recipientcouple = recipientfemale")[2,6]
ind_out_j[1,13,1] <- linearHypothesis(all_ind_res_j[[1]],"recipientcouple = recipientfemale")[2,6]
ind_out_m[1,13,1] <- linearHypothesis(all_ind_res_m[[1]],"recipientcouple = recipientfemale")[2,6]


ind_out_w[1,3:5,2] <-  summary(all_ind_res_w[[1]])$coefficients[4,c(1:2,4)]
ind_out_j[1,3:5,2] <-  summary(all_ind_res_j[[1]])$coefficients[4,c(1:2,4)]
ind_out_m[1,3:5,2] <-  summary(all_ind_res_m[[1]])$coefficients[4,c(1:2,4)]

ind_out_w[1,6:7,2] <- confint(all_ind_res_w[[1]])[4,]
ind_out_j[1,6:7,2] <- confint(all_ind_res_j[[1]])[4,]
ind_out_m[1,6:7,2] <- confint(all_ind_res_m[[1]])[4,]

#woman treatment
#coef estimate, se and p-value
ind_out_w[1,8:10,2] <- summary(all_ind_res_w[[1]])$coefficients[5,c(1:2,4)]
ind_out_j[1,8:10,2] <- summary(all_ind_res_j[[1]])$coefficients[5,c(1:2,4)]
ind_out_m[1,8:10,2] <- summary(all_ind_res_m[[1]])$coefficients[5,c(1:2,4)]

ind_out_w[1,11:12,2] <- confint(all_ind_res_w[[1]])[5,]
ind_out_j[1,11:12,2] <- confint(all_ind_res_j[[1]])[5,]
ind_out_m[1,11:12,2] <- confint(all_ind_res_m[[1]])[5,]

ind_out_w[1,13,2] <- linearHypothesis(all_ind_res_w[[1]],"messengercouple = messengerfemale")[2,6]
ind_out_j[1,13,2] <- linearHypothesis(all_ind_res_j[[1]],"messengercouple = messengerfemale")[2,6]
ind_out_m[1,13,2] <- linearHypothesis(all_ind_res_m[[1]],"messengercouple = messengerfemale")[2,6]

### knowledge index
i <- 2
ind_out_w[i,1,1] <- mean(know_ind_res_w[[2]]$index[know_ind_res_w[[2]]$recipient=="male"])
ind_out_j[i,1,1] <- mean(know_ind_res_j[[2]]$index[know_ind_res_j[[2]]$recipient=="male"])
ind_out_m[i,1,1] <- mean(know_ind_res_m[[2]]$index[know_ind_res_m[[2]]$recipient=="male"])

ind_out_w[i,2,1] <- sd(know_ind_res_w[[2]]$index[know_ind_res_w[[2]]$recipient=="male"])
ind_out_j[i,2,1] <- sd(know_ind_res_j[[2]]$index[know_ind_res_j[[2]]$recipient=="male"])
ind_out_m[i,2,1] <- sd(know_ind_res_m[[2]]$index[know_ind_res_m[[2]]$recipient=="male"])

ind_out_w[i,1,2] <- mean(know_ind_res_w[[2]]$index[know_ind_res_w[[2]]$messenger=="male"])
ind_out_j[i,1,2] <- mean(know_ind_res_j[[2]]$index[know_ind_res_j[[2]]$messenger=="male"])
ind_out_m[i,1,2] <- mean(know_ind_res_m[[2]]$index[know_ind_res_m[[2]]$messenger=="male"])

ind_out_w[i,2,2] <- sd(know_ind_res_w[[2]]$index[know_ind_res_w[[2]]$messenger=="male"])
ind_out_j[i,2,2] <- sd(know_ind_res_j[[2]]$index[know_ind_res_j[[2]]$messenger=="male"])
ind_out_m[i,2,2] <- sd(know_ind_res_m[[2]]$index[know_ind_res_m[[2]]$messenger=="male"])

ind_out_w[i,3:5,1] <-  summary(know_ind_res_w[[1]])$coefficients[2,c(1:2,4)]
ind_out_j[i,3:5,1] <-  summary(know_ind_res_j[[1]])$coefficients[2,c(1:2,4)]
ind_out_m[i,3:5,1] <-  summary(know_ind_res_m[[1]])$coefficients[2,c(1:2,4)]

ind_out_w[i,6:7,1] <- confint(know_ind_res_w[[1]])[2,]
ind_out_j[i,6:7,1] <- confint(know_ind_res_j[[1]])[2,]
ind_out_m[i,6:7,1] <- confint(know_ind_res_m[[1]])[2,]

#woman treatment
#coef estimate, se and p-value
ind_out_w[i,8:10,1] <- summary(know_ind_res_w[[1]])$coefficients[3,c(1:2,4)]
ind_out_j[i,8:10,1] <- summary(know_ind_res_j[[1]])$coefficients[3,c(1:2,4)]
ind_out_m[i,8:10,1] <- summary(know_ind_res_m[[1]])$coefficients[3,c(1:2,4)]

ind_out_w[i,11:12,1] <- confint(know_ind_res_w[[1]])[3,]
ind_out_j[i,11:12,1] <- confint(know_ind_res_j[[1]])[3,]
ind_out_m[i,11:12,1] <- confint(know_ind_res_m[[1]])[3,]

ind_out_w[i,13,1] <- linearHypothesis(know_ind_res_w[[1]],"recipientcouple = recipientfemale")[2,6]
ind_out_j[i,13,1] <- linearHypothesis(know_ind_res_j[[1]],"recipientcouple = recipientfemale")[2,6]
ind_out_m[i,13,1] <- linearHypothesis(know_ind_res_m[[1]],"recipientcouple = recipientfemale")[2,6]


ind_out_w[i,3:5,2] <-  summary(know_ind_res_w[[1]])$coefficients[4,c(1:2,4)]
ind_out_j[i,3:5,2] <-  summary(know_ind_res_j[[1]])$coefficients[4,c(1:2,4)]
ind_out_m[i,3:5,2] <-  summary(know_ind_res_m[[1]])$coefficients[4,c(1:2,4)]

ind_out_w[i,6:7,2] <- confint(know_ind_res_w[[1]])[4,]
ind_out_j[i,6:7,2] <- confint(know_ind_res_j[[1]])[4,]
ind_out_m[i,6:7,2] <- confint(know_ind_res_m[[1]])[4,]

#woman treatment
#coef estimate, se and p-value
ind_out_w[i,8:10,2] <- summary(know_ind_res_w[[1]])$coefficients[5,c(1:2,4)]
ind_out_j[i,8:10,2] <- summary(know_ind_res_j[[1]])$coefficients[5,c(1:2,4)]
ind_out_m[i,8:10,2] <- summary(know_ind_res_m[[1]])$coefficients[5,c(1:2,4)]

ind_out_w[i,11:12,2] <- confint(know_ind_res_w[[1]])[5,]
ind_out_j[i,11:12,2] <- confint(know_ind_res_j[[1]])[5,]
ind_out_m[i,11:12,2] <- confint(know_ind_res_m[[1]])[5,]

ind_out_w[i,13,2] <- linearHypothesis(know_ind_res_w[[1]],"messengercouple = messengerfemale")[2,6]
ind_out_j[i,13,2] <- linearHypothesis(know_ind_res_j[[1]],"messengercouple = messengerfemale")[2,6]
ind_out_m[i,13,2] <- linearHypothesis(know_ind_res_m[[1]],"messengercouple = messengerfemale")[2,6]

### decision index
i <- 3
ind_out_w[i,1,1] <- mean(dec_ind_res_w[[2]]$index[dec_ind_res_w[[2]]$recipient=="male"])
ind_out_j[i,1,1] <- mean(dec_ind_res_j[[2]]$index[dec_ind_res_j[[2]]$recipient=="male"])
ind_out_m[i,1,1] <- mean(dec_ind_res_m[[2]]$index[dec_ind_res_m[[2]]$recipient=="male"])

ind_out_w[i,2,1] <- sd(dec_ind_res_w[[2]]$index[dec_ind_res_w[[2]]$recipient=="male"])
ind_out_j[i,2,1] <- sd(dec_ind_res_j[[2]]$index[dec_ind_res_j[[2]]$recipient=="male"])
ind_out_m[i,2,1] <- sd(dec_ind_res_m[[2]]$index[dec_ind_res_m[[2]]$recipient=="male"])

ind_out_w[i,1,2] <- mean(dec_ind_res_w[[2]]$index[dec_ind_res_w[[2]]$messenger=="male"])
ind_out_j[i,1,2] <- mean(dec_ind_res_j[[2]]$index[dec_ind_res_j[[2]]$messenger=="male"])
ind_out_m[i,1,2] <- mean(dec_ind_res_m[[2]]$index[dec_ind_res_m[[2]]$messenger=="male"])

ind_out_w[i,2,2] <- sd(dec_ind_res_w[[2]]$index[dec_ind_res_w[[2]]$messenger=="male"])
ind_out_j[i,2,2] <- sd(dec_ind_res_j[[2]]$index[dec_ind_res_j[[2]]$messenger=="male"])
ind_out_m[i,2,2] <- sd(dec_ind_res_m[[2]]$index[dec_ind_res_m[[2]]$messenger=="male"])

ind_out_w[i,3:5,1] <-  summary(dec_ind_res_w[[1]])$coefficients[2,c(1:2,4)]
ind_out_j[i,3:5,1] <-  summary(dec_ind_res_j[[1]])$coefficients[2,c(1:2,4)]
ind_out_m[i,3:5,1] <-  summary(dec_ind_res_m[[1]])$coefficients[2,c(1:2,4)]

ind_out_w[i,6:7,1] <- confint(dec_ind_res_w[[1]])[2,]
ind_out_j[i,6:7,1] <- confint(dec_ind_res_j[[1]])[2,]
ind_out_m[i,6:7,1] <- confint(dec_ind_res_m[[1]])[2,]

#woman treatment
#coef estimate, se and p-value
ind_out_w[i,8:10,1] <- summary(dec_ind_res_w[[1]])$coefficients[3,c(1:2,4)]
ind_out_j[i,8:10,1] <- summary(dec_ind_res_j[[1]])$coefficients[3,c(1:2,4)]
ind_out_m[i,8:10,1] <- summary(dec_ind_res_m[[1]])$coefficients[3,c(1:2,4)]

ind_out_w[i,11:12,1] <- confint(dec_ind_res_w[[1]])[3,]
ind_out_j[i,11:12,1] <- confint(dec_ind_res_j[[1]])[3,]
ind_out_m[i,11:12,1] <- confint(dec_ind_res_m[[1]])[3,]

ind_out_w[i,13,1] <- linearHypothesis(dec_ind_res_w[[1]],"recipientcouple = recipientfemale")[2,6]
ind_out_j[i,13,1] <- linearHypothesis(dec_ind_res_j[[1]],"recipientcouple = recipientfemale")[2,6]
ind_out_m[i,13,1] <- linearHypothesis(dec_ind_res_m[[1]],"recipientcouple = recipientfemale")[2,6]


ind_out_w[i,3:5,2] <-  summary(dec_ind_res_w[[1]])$coefficients[4,c(1:2,4)]
ind_out_j[i,3:5,2] <-  summary(dec_ind_res_j[[1]])$coefficients[4,c(1:2,4)]
ind_out_m[i,3:5,2] <-  summary(dec_ind_res_m[[1]])$coefficients[4,c(1:2,4)]

ind_out_w[i,6:7,2] <- confint(dec_ind_res_w[[1]])[4,]
ind_out_j[i,6:7,2] <- confint(dec_ind_res_j[[1]])[4,]
ind_out_m[i,6:7,2] <- confint(dec_ind_res_m[[1]])[4,]

#woman treatment
#coef estimate, se and p-value
ind_out_w[i,8:10,2] <- summary(dec_ind_res_w[[1]])$coefficients[5,c(1:2,4)]
ind_out_j[i,8:10,2] <- summary(dec_ind_res_j[[1]])$coefficients[5,c(1:2,4)]
ind_out_m[i,8:10,2] <- summary(dec_ind_res_m[[1]])$coefficients[5,c(1:2,4)]

ind_out_w[i,11:12,2] <- confint(dec_ind_res_w[[1]])[5,]
ind_out_j[i,11:12,2] <- confint(dec_ind_res_j[[1]])[5,]
ind_out_m[i,11:12,2] <- confint(dec_ind_res_m[[1]])[5,]

ind_out_w[i,13,2] <- linearHypothesis(dec_ind_res_w[[1]],"messengercouple = messengerfemale")[2,6]
ind_out_j[i,13,2] <- linearHypothesis(dec_ind_res_j[[1]],"messengercouple = messengerfemale")[2,6]
ind_out_m[i,13,2] <- linearHypothesis(dec_ind_res_m[[1]],"messengercouple = messengerfemale")[2,6]

### adoption index
i <- 4
ind_out_w[i,1,1] <- mean(adopt_ind_res_w[[2]]$index[adopt_ind_res_w[[2]]$recipient=="male"])
ind_out_j[i,1,1] <- mean(adopt_ind_res_j[[2]]$index[adopt_ind_res_j[[2]]$recipient=="male"])
ind_out_m[i,1,1] <- mean(adopt_ind_res_m[[2]]$index[adopt_ind_res_m[[2]]$recipient=="male"])

ind_out_w[i,2,1] <- sd(adopt_ind_res_w[[2]]$index[adopt_ind_res_w[[2]]$recipient=="male"])
ind_out_j[i,2,1] <- sd(adopt_ind_res_j[[2]]$index[adopt_ind_res_j[[2]]$recipient=="male"])
ind_out_m[i,2,1] <- sd(adopt_ind_res_m[[2]]$index[adopt_ind_res_m[[2]]$recipient=="male"])

ind_out_w[i,1,2] <- mean(adopt_ind_res_w[[2]]$index[adopt_ind_res_w[[2]]$messenger=="male"])
ind_out_j[i,1,2] <- mean(adopt_ind_res_j[[2]]$index[adopt_ind_res_j[[2]]$messenger=="male"])
ind_out_m[i,1,2] <- mean(adopt_ind_res_m[[2]]$index[adopt_ind_res_m[[2]]$messenger=="male"])

ind_out_w[i,2,2] <- sd(adopt_ind_res_w[[2]]$index[adopt_ind_res_w[[2]]$messenger=="male"])
ind_out_j[i,2,2] <- sd(adopt_ind_res_j[[2]]$index[adopt_ind_res_j[[2]]$messenger=="male"])
ind_out_m[i,2,2] <- sd(adopt_ind_res_m[[2]]$index[adopt_ind_res_m[[2]]$messenger=="male"])

ind_out_w[i,3:5,1] <-  summary(adopt_ind_res_w[[1]])$coefficients[2,c(1:2,4)]
ind_out_j[i,3:5,1] <-  summary(adopt_ind_res_j[[1]])$coefficients[2,c(1:2,4)]
ind_out_m[i,3:5,1] <-  summary(adopt_ind_res_m[[1]])$coefficients[2,c(1:2,4)]

ind_out_w[i,6:7,1] <- confint(adopt_ind_res_w[[1]])[2,]
ind_out_j[i,6:7,1] <- confint(adopt_ind_res_j[[1]])[2,]
ind_out_m[i,6:7,1] <- confint(adopt_ind_res_m[[1]])[2,]

#woman treatment
#coef estimate, se and p-value
ind_out_w[i,8:10,1] <- summary(adopt_ind_res_w[[1]])$coefficients[3,c(1:2,4)]
ind_out_j[i,8:10,1] <- summary(adopt_ind_res_j[[1]])$coefficients[3,c(1:2,4)]
ind_out_m[i,8:10,1] <- summary(adopt_ind_res_m[[1]])$coefficients[3,c(1:2,4)]

ind_out_w[i,11:12,1] <- confint(adopt_ind_res_w[[1]])[3,]
ind_out_j[i,11:12,1] <- confint(adopt_ind_res_j[[1]])[3,]
ind_out_m[i,11:12,1] <- confint(adopt_ind_res_m[[1]])[3,]

ind_out_w[i,13,1] <- linearHypothesis(adopt_ind_res_w[[1]],"recipientcouple = recipientfemale")[2,6]
ind_out_j[i,13,1] <- linearHypothesis(adopt_ind_res_j[[1]],"recipientcouple = recipientfemale")[2,6]
ind_out_m[i,13,1] <- linearHypothesis(adopt_ind_res_m[[1]],"recipientcouple = recipientfemale")[2,6]


ind_out_w[i,3:5,2] <-  summary(adopt_ind_res_w[[1]])$coefficients[4,c(1:2,4)]
ind_out_j[i,3:5,2] <-  summary(adopt_ind_res_j[[1]])$coefficients[4,c(1:2,4)]
ind_out_m[i,3:5,2] <-  summary(adopt_ind_res_m[[1]])$coefficients[4,c(1:2,4)]

ind_out_w[i,6:7,2] <- confint(adopt_ind_res_w[[1]])[4,]
ind_out_j[i,6:7,2] <- confint(adopt_ind_res_j[[1]])[4,]
ind_out_m[i,6:7,2] <- confint(adopt_ind_res_m[[1]])[4,]

#woman treatment
#coef estimate, se and p-value
ind_out_w[i,8:10,2] <- summary(adopt_ind_res_w[[1]])$coefficients[5,c(1:2,4)]
ind_out_j[i,8:10,2] <- summary(adopt_ind_res_j[[1]])$coefficients[5,c(1:2,4)]
ind_out_m[i,8:10,2] <- summary(adopt_ind_res_m[[1]])$coefficients[5,c(1:2,4)]

ind_out_w[i,11:12,2] <- confint(adopt_ind_res_w[[1]])[5,]
ind_out_j[i,11:12,2] <- confint(adopt_ind_res_j[[1]])[5,]
ind_out_m[i,11:12,2] <- confint(adopt_ind_res_m[[1]])[5,]

ind_out_w[i,13,2] <- linearHypothesis(adopt_ind_res_w[[1]],"messengercouple = messengerfemale")[2,6]
ind_out_j[i,13,2] <- linearHypothesis(adopt_ind_res_j[[1]],"messengercouple = messengerfemale")[2,6]
ind_out_m[i,13,2] <- linearHypothesis(adopt_ind_res_m[[1]],"messengercouple = messengerfemale")[2,6]

### input index
i <- 5
ind_out_w[i,1,1] <- mean(input_ind_res_w[[2]]$index[input_ind_res_w[[2]]$recipient=="male"])
ind_out_j[i,1,1] <- mean(input_ind_res_j[[2]]$index[input_ind_res_j[[2]]$recipient=="male"])
ind_out_m[i,1,1] <- mean(input_ind_res_m[[2]]$index[input_ind_res_m[[2]]$recipient=="male"])

ind_out_w[i,2,1] <- sd(input_ind_res_w[[2]]$index[input_ind_res_w[[2]]$recipient=="male"])
ind_out_j[i,2,1] <- sd(input_ind_res_j[[2]]$index[input_ind_res_j[[2]]$recipient=="male"])
ind_out_m[i,2,1] <- sd(input_ind_res_m[[2]]$index[input_ind_res_m[[2]]$recipient=="male"])

ind_out_w[i,1,2] <- mean(input_ind_res_w[[2]]$index[input_ind_res_w[[2]]$messenger=="male"])
ind_out_j[i,1,2] <- mean(input_ind_res_j[[2]]$index[input_ind_res_j[[2]]$messenger=="male"])
ind_out_m[i,1,2] <- mean(input_ind_res_m[[2]]$index[input_ind_res_m[[2]]$messenger=="male"])

ind_out_w[i,2,2] <- sd(input_ind_res_w[[2]]$index[input_ind_res_w[[2]]$messenger=="male"])
ind_out_j[i,2,2] <- sd(input_ind_res_j[[2]]$index[input_ind_res_j[[2]]$messenger=="male"])
ind_out_m[i,2,2] <- sd(input_ind_res_m[[2]]$index[input_ind_res_m[[2]]$messenger=="male"])

ind_out_w[i,3:5,1] <-  summary(input_ind_res_w[[1]])$coefficients[2,c(1:2,4)]
ind_out_j[i,3:5,1] <-  summary(input_ind_res_j[[1]])$coefficients[2,c(1:2,4)]
ind_out_m[i,3:5,1] <-  summary(input_ind_res_m[[1]])$coefficients[2,c(1:2,4)]

ind_out_w[i,6:7,1] <- confint(input_ind_res_w[[1]])[2,]
ind_out_j[i,6:7,1] <- confint(input_ind_res_j[[1]])[2,]
ind_out_m[i,6:7,1] <- confint(input_ind_res_m[[1]])[2,]

#woman treatment
#coef estimate, se and p-value
ind_out_w[i,8:10,1] <- summary(input_ind_res_w[[1]])$coefficients[3,c(1:2,4)]
ind_out_j[i,8:10,1] <- summary(input_ind_res_j[[1]])$coefficients[3,c(1:2,4)]
ind_out_m[i,8:10,1] <- summary(input_ind_res_m[[1]])$coefficients[3,c(1:2,4)]

ind_out_w[i,11:12,1] <- confint(input_ind_res_w[[1]])[3,]
ind_out_j[i,11:12,1] <- confint(input_ind_res_j[[1]])[3,]
ind_out_m[i,11:12,1] <- confint(input_ind_res_m[[1]])[3,]

ind_out_w[i,13,1] <- linearHypothesis(input_ind_res_w[[1]],"recipientcouple = recipientfemale")[2,6]
ind_out_j[i,13,1] <- linearHypothesis(input_ind_res_j[[1]],"recipientcouple = recipientfemale")[2,6]
ind_out_m[i,13,1] <- linearHypothesis(input_ind_res_m[[1]],"recipientcouple = recipientfemale")[2,6]


ind_out_w[i,3:5,2] <-  summary(input_ind_res_w[[1]])$coefficients[4,c(1:2,4)]
ind_out_j[i,3:5,2] <-  summary(input_ind_res_j[[1]])$coefficients[4,c(1:2,4)]
ind_out_m[i,3:5,2] <-  summary(input_ind_res_m[[1]])$coefficients[4,c(1:2,4)]

ind_out_w[i,6:7,2] <- confint(input_ind_res_w[[1]])[4,]
ind_out_j[i,6:7,2] <- confint(input_ind_res_j[[1]])[4,]
ind_out_m[i,6:7,2] <- confint(input_ind_res_m[[1]])[4,]

#woman treatment
#coef estimate, se and p-value
ind_out_w[i,8:10,2] <- summary(input_ind_res_w[[1]])$coefficients[5,c(1:2,4)]
ind_out_j[i,8:10,2] <- summary(input_ind_res_j[[1]])$coefficients[5,c(1:2,4)]
ind_out_m[i,8:10,2] <- summary(input_ind_res_m[[1]])$coefficients[5,c(1:2,4)]

ind_out_w[i,11:12,2] <- confint(input_ind_res_w[[1]])[5,]
ind_out_j[i,11:12,2] <- confint(input_ind_res_j[[1]])[5,]
ind_out_m[i,11:12,2] <- confint(input_ind_res_m[[1]])[5,]

ind_out_w[i,13,2] <- linearHypothesis(input_ind_res_w[[1]],"messengercouple = messengerfemale")[2,6]
ind_out_j[i,13,2] <- linearHypothesis(input_ind_res_j[[1]],"messengercouple = messengerfemale")[2,6]
ind_out_m[i,13,2] <- linearHypothesis(input_ind_res_m[[1]],"messengercouple = messengerfemale")[2,6]

### outcome index
i <- 6
ind_out_w[i,1,1] <- mean(outcome_ind_res_w[[2]]$index[outcome_ind_res_w[[2]]$recipient=="male"])
ind_out_j[i,1,1] <- mean(outcome_ind_res_j[[2]]$index[outcome_ind_res_j[[2]]$recipient=="male"])
ind_out_m[i,1,1] <- mean(outcome_ind_res_m[[2]]$index[outcome_ind_res_m[[2]]$recipient=="male"])

ind_out_w[i,2,1] <- sd(outcome_ind_res_w[[2]]$index[outcome_ind_res_w[[2]]$recipient=="male"])
ind_out_j[i,2,1] <- sd(outcome_ind_res_j[[2]]$index[outcome_ind_res_j[[2]]$recipient=="male"])
ind_out_m[i,2,1] <- sd(outcome_ind_res_m[[2]]$index[outcome_ind_res_m[[2]]$recipient=="male"])

ind_out_w[i,1,2] <- mean(outcome_ind_res_w[[2]]$index[outcome_ind_res_w[[2]]$messenger=="male"])
ind_out_j[i,1,2] <- mean(outcome_ind_res_j[[2]]$index[outcome_ind_res_j[[2]]$messenger=="male"])
ind_out_m[i,1,2] <- mean(outcome_ind_res_m[[2]]$index[outcome_ind_res_m[[2]]$messenger=="male"])

ind_out_w[i,2,2] <- sd(outcome_ind_res_w[[2]]$index[outcome_ind_res_w[[2]]$messenger=="male"])
ind_out_j[i,2,2] <- sd(outcome_ind_res_j[[2]]$index[outcome_ind_res_j[[2]]$messenger=="male"])
ind_out_m[i,2,2] <- sd(outcome_ind_res_m[[2]]$index[outcome_ind_res_m[[2]]$messenger=="male"])

ind_out_w[i,3:5,1] <-  summary(outcome_ind_res_w[[1]])$coefficients[2,c(1:2,4)]
ind_out_j[i,3:5,1] <-  summary(outcome_ind_res_j[[1]])$coefficients[2,c(1:2,4)]
ind_out_m[i,3:5,1] <-  summary(outcome_ind_res_m[[1]])$coefficients[2,c(1:2,4)]

ind_out_w[i,6:7,1] <- confint(outcome_ind_res_w[[1]])[2,]
ind_out_j[i,6:7,1] <- confint(outcome_ind_res_j[[1]])[2,]
ind_out_m[i,6:7,1] <- confint(outcome_ind_res_m[[1]])[2,]

#woman treatment
#coef estimate, se and p-value
ind_out_w[i,8:10,1] <- summary(outcome_ind_res_w[[1]])$coefficients[3,c(1:2,4)]
ind_out_j[i,8:10,1] <- summary(outcome_ind_res_j[[1]])$coefficients[3,c(1:2,4)]
ind_out_m[i,8:10,1] <- summary(outcome_ind_res_m[[1]])$coefficients[3,c(1:2,4)]

ind_out_w[i,11:12,1] <- confint(outcome_ind_res_w[[1]])[3,]
ind_out_j[i,11:12,1] <- confint(outcome_ind_res_j[[1]])[3,]
ind_out_m[i,11:12,1] <- confint(outcome_ind_res_m[[1]])[3,]

ind_out_w[i,13,1] <- linearHypothesis(outcome_ind_res_w[[1]],"recipientcouple = recipientfemale")[2,6]
ind_out_j[i,13,1] <- linearHypothesis(outcome_ind_res_j[[1]],"recipientcouple = recipientfemale")[2,6]
ind_out_m[i,13,1] <- linearHypothesis(outcome_ind_res_m[[1]],"recipientcouple = recipientfemale")[2,6]


ind_out_w[i,3:5,2] <-  summary(outcome_ind_res_w[[1]])$coefficients[4,c(1:2,4)]
ind_out_j[i,3:5,2] <-  summary(outcome_ind_res_j[[1]])$coefficients[4,c(1:2,4)]
ind_out_m[i,3:5,2] <-  summary(outcome_ind_res_m[[1]])$coefficients[4,c(1:2,4)]

ind_out_w[i,6:7,2] <- confint(outcome_ind_res_w[[1]])[4,]
ind_out_j[i,6:7,2] <- confint(outcome_ind_res_j[[1]])[4,]
ind_out_m[i,6:7,2] <- confint(outcome_ind_res_m[[1]])[4,]

#woman treatment
#coef estimate, se and p-value
ind_out_w[i,8:10,2] <- summary(outcome_ind_res_w[[1]])$coefficients[5,c(1:2,4)]
ind_out_j[i,8:10,2] <- summary(outcome_ind_res_j[[1]])$coefficients[5,c(1:2,4)]
ind_out_m[i,8:10,2] <- summary(outcome_ind_res_m[[1]])$coefficients[5,c(1:2,4)]

ind_out_w[i,11:12,2] <- confint(outcome_ind_res_w[[1]])[5,]
ind_out_j[i,11:12,2] <- confint(outcome_ind_res_j[[1]])[5,]
ind_out_m[i,11:12,2] <- confint(outcome_ind_res_m[[1]])[5,]

ind_out_w[i,13,2] <- linearHypothesis(outcome_ind_res_w[[1]],"messengercouple = messengerfemale")[2,6]
ind_out_j[i,13,2] <- linearHypothesis(outcome_ind_res_j[[1]],"messengercouple = messengerfemale")[2,6]
ind_out_m[i,13,2] <- linearHypothesis(outcome_ind_res_m[[1]],"messengercouple = messengerfemale")[2,6]

### create graphs


library(plyr)
require(gridExtra)
library(cowplot)
library(grid)
library(ggpubr)

### summary for the recipient treatment
lims <- .42 
## create data.frame to plot - make sure you get correct i's for the indices; last one is overall index
d_plot <- data.frame(rbind(ind_out_w[1,c(8,11,12),1],ind_out_j[1,c(8,11,12),1],ind_out_m[1,c(8,11,12),1]))
d_plot <- rbind(d_plot, data.frame(rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA))))
d_plot <- rbind(d_plot,data.frame(rbind(ind_out_w[2,c(8,11,12),1],ind_out_j[2,c(8,11,12),1],ind_out_m[2,c(8,11,12),1])))
d_plot <- rbind(d_plot,data.frame(rbind(ind_out_w[3,c(8,11,12),1],ind_out_j[3,c(8,11,12),1],ind_out_m[3,c(8,11,12),1])))
d_plot <- rbind(d_plot,data.frame(rbind(ind_out_w[4,c(8,11,12),1],ind_out_j[4,c(8,11,12),1],ind_out_m[4,c(8,11,12),1])))
d_plot <- rbind(d_plot,data.frame(rbind(ind_out_w[5,c(8,11,12),1],ind_out_j[5,c(8,11,12),1],ind_out_m[5,c(8,11,12),1])))
d_plot <- rbind(d_plot,data.frame(rbind(ind_out_w[6,c(8,11,12),1],ind_out_j[6,c(8,11,12),1],ind_out_m[6,c(8,11,12),1])))

names(d_plot) <- c("y","ylo","yhi")

d_plot$x <- rep(c("index","","knowledge","decision making","adoption","input use","outcomes"), each=3)
d_plot$grp <- rep(c("woman outcome","joint outcome","man outcome"), times=7)
d_plot$grp <-  factor(d_plot$grp , levels=c("woman outcome","joint outcome","man outcome"))
d_plot$x <-  factor(d_plot$x, levels=rev((c("index","","knowledge","decision making","adoption","input use","outcomes"))))

plot1 <- credplot.gg(d_plot,'SDs','target == women',levels(d_plot$x),lims) + theme(legend.position = "none")

d_plot <- data.frame(rbind(ind_out_w[1,c(3,6,7),1],ind_out_j[1,c(3,6,7),1],ind_out_m[1,c(3,6,7),1]))
d_plot <- rbind(d_plot, data.frame(rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA))))
d_plot <- rbind(d_plot,data.frame(rbind(ind_out_w[2,c(3,6,7),1],ind_out_j[2,c(3,6,7),1],ind_out_m[2,c(3,6,7),1])))
d_plot <- rbind(d_plot,data.frame(rbind(ind_out_w[3,c(3,6,7),1],ind_out_j[3,c(3,6,7),1],ind_out_m[3,c(3,6,7),1])))
d_plot <- rbind(d_plot,data.frame(rbind(ind_out_w[4,c(3,6,7),1],ind_out_j[4,c(3,6,7),1],ind_out_m[4,c(3,6,7),1])))
d_plot <- rbind(d_plot,data.frame(rbind(ind_out_w[5,c(3,6,7),1],ind_out_j[5,c(3,6,7),1],ind_out_m[5,c(3,6,7),1])))
d_plot <- rbind(d_plot,data.frame(rbind(ind_out_w[6,c(3,6,7),1],ind_out_j[6,c(3,6,7),1],ind_out_m[6,c(3,6,7),1])))

names(d_plot) <- c("y","ylo","yhi")

d_plot$x <- rep(c("index","","knowledge","decision making","adoption","input use","outcomes"), each=3)
d_plot$grp <- rep(c("woman outcome","joint outcome","man outcome"), times=7)
d_plot$grp <-  factor(d_plot$grp , levels=c("woman outcome","joint outcome","man outcome"))
d_plot$x <-  factor(d_plot$x, levels=rev((c("index","","knowledge","decision making","adoption","input use","outcomes"))))


plot2 <- credplot.gg(d_plot,'SDs','target == couple',levels(d_plot$x),lims) + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())  + theme(legend.position = "right")


png("/home/bjvca/data/projects/digital green/papers/DP_gender/World_Development/fig1.png", units="px", height=3200, width= 6400, res=600)
ggarrange(plot1,plot2, common.legend = TRUE, legend = "bottom", widths=c(.5,.41))
dev.off()
### summary for the messenger treatment
lims <- .42 
## create data.frame to plot - make sure you get correct i's for the indices; last one is overall index
d_plot <- data.frame(rbind(ind_out_w[1,c(8,11,12),2],ind_out_j[1,c(8,11,12),2],ind_out_m[1,c(8,11,12),2]))
d_plot <- rbind(d_plot, data.frame(rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA))))
d_plot <- rbind(d_plot,data.frame(rbind(ind_out_w[2,c(8,11,12),2],ind_out_j[2,c(8,11,12),2],ind_out_m[2,c(8,11,12),2])))
d_plot <- rbind(d_plot,data.frame(rbind(ind_out_w[3,c(8,11,12),2],ind_out_j[3,c(8,11,12),2],ind_out_m[3,c(8,11,12),2])))
d_plot <- rbind(d_plot,data.frame(rbind(ind_out_w[4,c(8,11,12),2],ind_out_j[4,c(8,11,12),2],ind_out_m[4,c(8,11,12),2])))
d_plot <- rbind(d_plot,data.frame(rbind(ind_out_w[5,c(8,11,12),2],ind_out_j[5,c(8,11,12),2],ind_out_m[5,c(8,11,12),2])))
d_plot <- rbind(d_plot,data.frame(rbind(ind_out_w[6,c(8,11,12),2],ind_out_j[6,c(8,11,12),2],ind_out_m[6,c(8,11,12),2])))

names(d_plot) <- c("y","ylo","yhi")

d_plot$x <- rep(c("index","","knowledge","decision making","adoption","input use","outcomes"), each=3)
d_plot$grp <- rep(c("woman outcome","joint outcome","man outcome"), times=7)
d_plot$grp <-  factor(d_plot$grp , levels=c("woman outcome","joint outcome","man outcome"))
d_plot$x <-  factor(d_plot$x, levels=rev((c("index","","knowledge","decision making","adoption","input use","outcomes"))))

plot1 <- credplot.gg(d_plot,'SDs','messenger == women',levels(d_plot$x),lims) + theme(legend.position = "none")

d_plot <- data.frame(rbind(ind_out_w[1,c(3,6,7),2],ind_out_j[1,c(3,6,7),2],ind_out_m[1,c(3,6,7),2]))
d_plot <- rbind(d_plot, data.frame(rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA))))
d_plot <- rbind(d_plot,data.frame(rbind(ind_out_w[2,c(3,6,7),2],ind_out_j[2,c(3,6,7),2],ind_out_m[2,c(3,6,7),2])))
d_plot <- rbind(d_plot,data.frame(rbind(ind_out_w[3,c(3,6,7),2],ind_out_j[3,c(3,6,7),2],ind_out_m[3,c(3,6,7),2])))
d_plot <- rbind(d_plot,data.frame(rbind(ind_out_w[4,c(3,6,7),2],ind_out_j[4,c(3,6,7),2],ind_out_m[4,c(3,6,7),2])))
d_plot <- rbind(d_plot,data.frame(rbind(ind_out_w[5,c(3,6,7),2],ind_out_j[5,c(3,6,7),2],ind_out_m[5,c(3,6,7),2])))
d_plot <- rbind(d_plot,data.frame(rbind(ind_out_w[6,c(3,6,7),2],ind_out_j[6,c(3,6,7),2],ind_out_m[6,c(3,6,7),2])))

names(d_plot) <- c("y","ylo","yhi")

d_plot$x <- rep(c("index","","knowledge","decision making","adoption","input use","outcomes"), each=3)
d_plot$grp <- rep(c("woman outcome","joint outcome","man outcome"), times=7)
d_plot$grp <-  factor(d_plot$grp , levels=c("woman outcome","joint outcome","man outcome"))
d_plot$x <-  factor(d_plot$x, levels=rev((c("index","","knowledge","decision making","adoption","input use","outcomes"))))


plot2 <- credplot.gg(d_plot,'SDs','messenger == couple',levels(d_plot$x),lims) + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())  + theme(legend.position = "right")


png("/home/bjvca/data/projects/digital green/papers/DP_gender/World_Development/fig2.png", units="px", height=3200, width= 6400, res=600)
ggarrange(plot1,plot2, common.legend = TRUE, legend = "bottom", widths=c(.5,.41))
dev.off()

### individual outcomes - recipient treatment
lims <- .5
## create data.frame to plot - make sure you get correct i's for the indices; last one is overall index
d_plot <- rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA))
d_plot <-  rbind(d_plot,rbind(ind_out_w[1,c(8,11,12),1],ind_out_j[1,c(8,11,12),1],ind_out_m[1,c(8,11,12),1]))
d_plot <- rbind(d_plot,rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA)))
d_plot <-  rbind(d_plot,rbind(ind_out_w[2,c(8,11,12),1],ind_out_j[2,c(8,11,12),1],ind_out_m[2,c(8,11,12),1]))

for (i in 1:4) {
#d_plot <- rbind(d_plot, data.frame(rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA))))
d_plot <- rbind(d_plot,rbind(out_w[i,c(8,11,12),1]/out_w[i,2,1],out_j[i,c(8,11,12),1]/out_j[i,2,1],out_m[i,c(8,11,12),1]/out_m[i,2,1]))
}
d_plot <- rbind(d_plot,rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA)))
d_plot <- rbind(d_plot,rbind(ind_out_w[3,c(8,11,12),1],ind_out_j[3,c(8,11,12),1],ind_out_m[3,c(8,11,12),1]))

for (i in 5:9) {

d_plot <- rbind(d_plot,rbind(out_w[i,c(8,11,12),1]/out_w[i,2,1],out_j[i,c(8,11,12),1]/out_j[i,2,1],out_m[i,c(8,11,12),1]/out_m[i,2,1]))
}
d_plot <- rbind(d_plot, rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA)))
d_plot <- rbind(d_plot,rbind(ind_out_w[4,c(8,11,12),1],ind_out_j[4,c(8,11,12),1],ind_out_m[4,c(8,11,12),1]))

for (i in 10:13) {
d_plot <- rbind(d_plot,rbind(out_w[i,c(8,11,12),1]/out_w[i,2,1],out_j[i,c(8,11,12),1]/out_j[i,2,1],out_m[i,c(8,11,12),1]/out_m[i,2,1]))
}
d_plot <- rbind(d_plot, rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA)))
d_plot <- rbind(d_plot,rbind(ind_out_w[5,c(8,11,12),1],ind_out_j[5,c(8,11,12),1],ind_out_m[5,c(8,11,12),1]))

for (i in 14:18) {

d_plot <- rbind(d_plot,rbind(out_w[i,c(8,11,12),1]/out_w[i,2,1],out_j[i,c(8,11,12),1]/out_j[i,2,1],out_m[i,c(8,11,12),1]/out_m[i,2,1]))
}
d_plot <- rbind(d_plot,rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA)))
d_plot <-  rbind(d_plot,rbind(ind_out_w[6,c(8,11,12),1],ind_out_j[6,c(8,11,12),1],ind_out_m[6,c(8,11,12),1]))
for (i in 23:26) {

d_plot <- rbind(d_plot,rbind(out_w[i,c(8,11,12),1]/out_w[i,2,1],out_j[i,c(8,11,12),1]/out_j[i,2,1],out_m[i,c(8,11,12),1]/out_m[i,2,1]))
}
d_plot <- rbind(d_plot,rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA)))

d_plot <- data.frame(d_plot) 
names(d_plot) <- c("y","ylo","yhi")

d_plot$x <- rep(c("","overall index","      ","knowledge index","knows about spacing", "knows about combining","knows about weeding","knows about FAW", " ","decision index","decision to plant maze","decision on timing of planting","decision on spacing & seed rate","decision on how to fight striga","decision on weeding","  ","adoption index","timely planting","optimal spacing & seed rate","recommended striga weeding","recommended timing of weeding", "   ","input use index","DAP","UREA","Organic","Hybrid seed", "OPV","    ", "outcome index","yield improved","yield","sold","quantity sold","     "), each=3)
d_plot$grp <- rep(c("woman outcome","joint outcome","man outcome"), times=35)
d_plot$grp <-  factor(d_plot$grp , levels=c("woman outcome","joint outcome","man outcome"))
d_plot$x <-  factor(d_plot$x, levels=rev(c("","overall index","      ","knowledge index","knows about spacing", "knows about combining","knows about weeding","knows about FAW", " ","decision index","decision to plant maze","decision on timing of planting","decision on spacing & seed rate","decision on how to fight striga","decision on weeding","  ","adoption index","timely planting","optimal spacing & seed rate","recommended striga weeding","recommended timing of weeding","   ", "input use index","DAP","UREA","Organic","Hybrid seed", "OPV","    ", "outcome index","yield improved","yield","sold","quantity sold","     ")))


bold.labels <- ifelse(levels(d_plot$x) %in% c("overall index","knowledge index","decision index","adoption index","input use index","outcome index"), yes = "bold", no = "plain")


plot1 <- credplot.gg(d_plot,'SDs','woman receives information',levels(d_plot$x), lims) + theme(legend.position = "none") +
  theme(axis.text.y = element_text(face = bold.labels))


## create data.frame to plot - make sure you get correct i's for the indices; last one is overall index
d_plot <- rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA))
d_plot <-  rbind(d_plot,rbind(ind_out_w[1,c(3,6,7),1],ind_out_j[1,c(3,6,7),1],ind_out_m[1,c(3,6,7),1]))
d_plot <- rbind(d_plot,rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA)))
d_plot <-  rbind(d_plot,rbind(ind_out_w[2,c(3,6,7),1],ind_out_j[2,c(3,6,7),1],ind_out_m[2,c(3,6,7),1]))

for (i in 1:4) {
#d_plot <- rbind(d_plot, data.frame(rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA))))
d_plot <- rbind(d_plot,rbind(out_w[i,c(3,6,7),1]/out_w[i,2,1],out_j[i,c(3,6,7),1]/out_j[i,2,1],out_m[i,c(3,6,7),1]/out_m[i,2,1]))
}
d_plot <- rbind(d_plot,rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA)))
d_plot <- rbind(d_plot,rbind(ind_out_w[3,c(3,6,7),1],ind_out_j[3,c(3,6,7),1],ind_out_m[3,c(3,6,7),1]))

for (i in 5:9) {

d_plot <- rbind(d_plot,rbind(out_w[i,c(3,6,7),1]/out_w[i,2,1],out_j[i,c(3,6,7),1]/out_j[i,2,1],out_m[i,c(3,6,7),1]/out_m[i,2,1]))
}
d_plot <- rbind(d_plot,rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA)))
d_plot <- rbind(d_plot,rbind(ind_out_w[4,c(3,6,7),1],ind_out_j[4,c(3,6,7),1],ind_out_m[4,c(3,6,7),1]))

for (i in 10:13) {
d_plot <- rbind(d_plot,rbind(out_w[i,c(3,6,7),1]/out_w[i,2,1],out_j[i,c(3,6,7),1]/out_j[i,2,1],out_m[i,c(3,6,7),1]/out_m[i,2,1]))
}
d_plot <- rbind(d_plot,rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA)))
d_plot <- rbind(d_plot,rbind(ind_out_w[5,c(3,6,7),1],ind_out_j[5,c(3,6,7),1],ind_out_m[5,c(3,6,7),1]))

for (i in 14:18) {

d_plot <- rbind(d_plot,rbind(out_w[i,c(3,6,7),1]/out_w[i,2,1],out_j[i,c(3,6,7),1]/out_j[i,2,1],out_m[i,c(3,6,7),1]/out_m[i,2,1]))
}
d_plot <- rbind(d_plot,rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA)))
d_plot <-  rbind(d_plot,rbind(ind_out_w[6,c(3,6,7),1],ind_out_j[6,c(3,6,7),1],ind_out_m[6,c(3,6,7),1]))
for (i in 23:26) {

d_plot <- rbind(d_plot,rbind(out_w[i,c(3,6,7),1]/out_w[i,2,1],out_j[i,c(3,6,7),1]/out_j[i,2,1],out_m[i,c(3,6,7),1]/out_m[i,2,1]))
}
d_plot <- rbind(d_plot,rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA)))

d_plot <- data.frame(d_plot) 
names(d_plot) <- c("y","ylo","yhi")

d_plot$x <- rep(c("","overall index","      ","knowledge index","knows about spacing", "knows about combining","knows about weeding","knows about FAW", " ","decision index","decision to plant maze","decision on timing of planting","decision on spacing & seed rate","decision on how to fight striga","decision on weeding","  ","adoption index","timely planting","optimal spacing & seed rate","recommended striga weeding","recommended timing of weeding", "   ","input use index","DAP","UREA","Organic","Hybrid seed", "OPV","    ", "outcome index","yield improved","yield","sold","quantity sold","     "), each=3)
d_plot$grp <- rep(c("woman outcome","joint outcome","man outcome"), times=35)
d_plot$grp <-  factor(d_plot$grp , levels=c("woman outcome","joint outcome","man outcome"))
d_plot$x <-  factor(d_plot$x, levels=rev(c("","overall index","      ","knowledge index","knows about spacing", "knows about combining","knows about weeding","knows about FAW", " ","decision index","decision to plant maze","decision on timing of planting","decision on spacing & seed rate","decision on how to fight striga","decision on weeding","  ","adoption index","timely planting","optimal spacing & seed rate","recommended striga weeding","recommended timing of weeding","   ", "input use index","DAP","UREA","Organic","Hybrid seed", "OPV","    ", "outcome index","yield improved","yield","sold","quantity sold","     ")))


bold.labels <- ifelse(levels(d_plot$x) %in% c("overall index","knowledge index","decision index","adoption index","input use index","outcome index"), yes = "bold", no = "plain")

plot2 <- credplot.gg(d_plot,'SDs','couple receives information',levels(d_plot$x),lims) + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())  + theme(legend.position = "right")


png("/home/bjvca/data/projects/digital green/papers/DP_gender/World_Development/fig3.png", units="px", height=6400, width= 6400, res=600)
ggarrange(plot1,plot2, common.legend = TRUE, legend = "bottom", widths=c(.5,.3))
dev.off()


## individual outcomes - messenger treatment
lims <- .5
## create data.frame to plot - make sure you get correct i's for the indices; last one is overall index
d_plot <- rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA))
d_plot <-  rbind(d_plot,rbind(ind_out_w[1,c(8,11,12),2],ind_out_j[1,c(8,11,12),2],ind_out_m[1,c(8,11,12),2]))
d_plot <- rbind(d_plot,rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA)))
d_plot <-  rbind(d_plot,rbind(ind_out_w[2,c(8,11,12),2],ind_out_j[2,c(8,11,12),2],ind_out_m[2,c(8,11,12),2]))

for (i in 1:4) {
#d_plot <- rbind(d_plot, data.frame(rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA))))
d_plot <- rbind(d_plot,rbind(out_w[i,c(8,11,12),2]/out_w[i,2,2],out_j[i,c(8,11,12),2]/out_j[i,2,2],out_m[i,c(8,11,12),2]/out_m[i,2,2]))
}
d_plot <- rbind(d_plot,rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA)))
d_plot <- rbind(d_plot,rbind(ind_out_w[3,c(8,11,12),2],ind_out_j[3,c(8,11,12),2],ind_out_m[3,c(8,11,12),2]))

for (i in 5:9) {

d_plot <- rbind(d_plot,rbind(out_w[i,c(8,11,12),2]/out_w[i,2,2],out_j[i,c(8,11,12),2]/out_j[i,2,2],out_m[i,c(8,11,12),2]/out_m[i,2,2]))
}
d_plot <- rbind(d_plot, rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA)))
d_plot <- rbind(d_plot,rbind(ind_out_w[4,c(8,11,12),2],ind_out_j[4,c(8,11,12),2],ind_out_m[4,c(8,11,12),2]))

for (i in 10:13) {
d_plot <- rbind(d_plot,rbind(out_w[i,c(8,11,12),2]/out_w[i,2,2],out_j[i,c(8,11,12),2]/out_j[i,2,2],out_m[i,c(8,11,12),2]/out_m[i,2,2]))
}
d_plot <- rbind(d_plot, rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA)))
d_plot <- rbind(d_plot,rbind(ind_out_w[5,c(8,11,12),2],ind_out_j[5,c(8,11,12),2],ind_out_m[5,c(8,11,12),2]))

for (i in 14:18) {

d_plot <- rbind(d_plot,rbind(out_w[i,c(8,11,12),2]/out_w[i,2,2],out_j[i,c(8,11,12),2]/out_j[i,2,2],out_m[i,c(8,11,12),2]/out_m[i,2,2]))
}
d_plot <- rbind(d_plot,rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA)))
d_plot <-  rbind(d_plot,rbind(ind_out_w[6,c(8,11,12),2],ind_out_j[6,c(8,11,12),2],ind_out_m[6,c(8,11,12),2]))
for (i in 23:26) {

d_plot <- rbind(d_plot,rbind(out_w[i,c(8,11,12),2]/out_w[i,2,2],out_j[i,c(8,11,12),2]/out_j[i,2,2],out_m[i,c(8,11,12),2]/out_m[i,2,2]))
}
d_plot <- rbind(d_plot,rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA)))

d_plot <- data.frame(d_plot) 
names(d_plot) <- c("y","ylo","yhi")

d_plot$x <- rep(c("","overall index","      ","knowledge index","knows about spacing", "knows about combining","knows about weeding","knows about FAW", " ","decision index","decision to plant maze","decision on timing of planting","decision on spacing & seed rate","decision on how to fight striga","decision on weeding","  ","adoption index","timely planting","optimal spacing & seed rate","recommended striga weeding","recommended timing of weeding", "   ","input use index","DAP","UREA","Organic","Hybrid seed", "OPV","    ", "outcome index","yield improved","yield","sold","quantity sold","     "), each=3)
d_plot$grp <- rep(c("woman outcome","joint outcome","man outcome"), times=35)
d_plot$grp <-  factor(d_plot$grp , levels=c("woman outcome","joint outcome","man outcome"))
d_plot$x <-  factor(d_plot$x, levels=rev(c("","overall index","      ","knowledge index","knows about spacing", "knows about combining","knows about weeding","knows about FAW", " ","decision index","decision to plant maze","decision on timing of planting","decision on spacing & seed rate","decision on how to fight striga","decision on weeding","  ","adoption index","timely planting","optimal spacing & seed rate","recommended striga weeding","recommended timing of weeding","   ", "input use index","DAP","UREA","Organic","Hybrid seed", "OPV","    ", "outcome index","yield improved","yield","sold","quantity sold","     ")))


bold.labels <- ifelse(levels(d_plot$x) %in% c("overall index","knowledge index","decision index","adoption index","input use index","outcome index"), yes = "bold", no = "plain")


plot1 <- credplot.gg(d_plot,'SDs','woman gives information',levels(d_plot$x), lims) + theme(legend.position = "none") +
  theme(axis.text.y = element_text(face = bold.labels))


## create data.frame to plot - make sure you get correct i's for the indices; last one is overall index
d_plot <- rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA))
d_plot <-  rbind(d_plot,rbind(ind_out_w[1,c(3,6,7),2],ind_out_j[1,c(3,6,7),2],ind_out_m[1,c(3,6,7),2]))
d_plot <- rbind(d_plot,rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA)))
d_plot <-  rbind(d_plot,rbind(ind_out_w[2,c(3,6,7),2],ind_out_j[2,c(3,6,7),2],ind_out_m[2,c(3,6,7),2]))

for (i in 1:4) {
#d_plot <- rbind(d_plot, data.frame(rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA))))
d_plot <- rbind(d_plot,rbind(out_w[i,c(3,6,7),2]/out_w[i,2,2],out_j[i,c(3,6,7),2]/out_j[i,2,2],out_m[i,c(3,6,7),2]/out_m[i,2,2]))
}
d_plot <- rbind(d_plot,rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA)))
d_plot <- rbind(d_plot,rbind(ind_out_w[3,c(3,6,7),2],ind_out_j[3,c(3,6,7),2],ind_out_m[3,c(3,6,7),2]))

for (i in 5:9) {

d_plot <- rbind(d_plot,rbind(out_w[i,c(3,6,7),2]/out_w[i,2,2],out_j[i,c(3,6,7),2]/out_j[i,2,2],out_m[i,c(3,6,7),2]/out_m[i,2,2]))
}
d_plot <- rbind(d_plot,rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA)))
d_plot <- rbind(d_plot,rbind(ind_out_w[4,c(3,6,7),2],ind_out_j[4,c(3,6,7),2],ind_out_m[4,c(3,6,7),2]))

for (i in 10:13) {
d_plot <- rbind(d_plot,rbind(out_w[i,c(3,6,7),2]/out_w[i,2,2],out_j[i,c(3,6,7),2]/out_j[i,2,2],out_m[i,c(3,6,7),2]/out_m[i,2,2]))
}
d_plot <- rbind(d_plot,rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA)))
d_plot <- rbind(d_plot,rbind(ind_out_w[5,c(3,6,7),2],ind_out_j[5,c(3,6,7),2],ind_out_m[5,c(3,6,7),2]))

for (i in 14:18) {

d_plot <- rbind(d_plot,rbind(out_w[i,c(3,6,7),2]/out_w[i,2,2],out_j[i,c(3,6,7),2]/out_j[i,2,2],out_m[i,c(3,6,7),2]/out_m[i,2,2]))
}
d_plot <- rbind(d_plot,rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA)))
d_plot <-  rbind(d_plot,rbind(ind_out_w[6,c(3,6,7),2],ind_out_j[6,c(3,6,7),2],ind_out_m[6,c(3,6,7),2]))
for (i in 23:26) {

d_plot <- rbind(d_plot,rbind(out_w[i,c(3,6,7),2]/out_w[i,2,2],out_j[i,c(3,6,7),2]/out_j[i,2,2],out_m[i,c(3,6,7),2]/out_m[i,2,2]))
}
d_plot <- rbind(d_plot,rbind(c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA)))

d_plot <- data.frame(d_plot) 
names(d_plot) <- c("y","ylo","yhi")

d_plot$x <- rep(c("","overall index","      ","knowledge index","knows about spacing", "knows about combining","knows about weeding","knows about FAW", " ","decision index","decision to plant maze","decision on timing of planting","decision on spacing & seed rate","decision on how to fight striga","decision on weeding","  ","adoption index","timely planting","optimal spacing & seed rate","recommended striga weeding","recommended timing of weeding", "   ","input use index","DAP","UREA","Organic","Hybrid seed", "OPV","    ", "outcome index","yield improved","yield","sold","quantity sold","     "), each=3)
d_plot$grp <- rep(c("woman outcome","joint outcome","man outcome"), times=35)
d_plot$grp <-  factor(d_plot$grp , levels=c("woman outcome","joint outcome","man outcome"))
d_plot$x <-  factor(d_plot$x, levels=rev(c("","overall index","      ","knowledge index","knows about spacing", "knows about combining","knows about weeding","knows about FAW", " ","decision index","decision to plant maze","decision on timing of planting","decision on spacing & seed rate","decision on how to fight striga","decision on weeding","  ","adoption index","timely planting","optimal spacing & seed rate","recommended striga weeding","recommended timing of weeding","   ", "input use index","DAP","UREA","Organic","Hybrid seed", "OPV","    ", "outcome index","yield improved","yield","sold","quantity sold","     ")))


bold.labels <- ifelse(levels(d_plot$x) %in% c("overall index","knowledge index","decision index","adoption index","input use index","outcome index"), yes = "bold", no = "plain")

plot2 <- credplot.gg(d_plot,'SDs','couple gives information',levels(d_plot$x),lims) + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())  + theme(legend.position = "right")


png("/home/bjvca/data/projects/digital green/papers/DP_gender/World_Development/fig4.png", units="px", height=6400, width= 6400, res=600)
ggarrange(plot1,plot2, common.legend = TRUE, legend = "bottom", widths=c(.5,.3))
dev.off()



