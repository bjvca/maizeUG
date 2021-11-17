rm(list=ls())
library(clubSandwich)
library(ggpubr)
library(stringr)
library(multiwayvcov)

library(clubSandwich)
library(car)
dta <- read.csv("/home/bjvca/data/projects/digital green/papers/DP_disagreement/data/endline_dta.csv",stringsAsFactors =FALSE)
experts <- read.csv("/home/bjvca/data/projects/digital green/papers/DP_disagreement/data/expert_rankings.csv",stringsAsFactors =FALSE)
norms <- experts[,c(10:20,29)] 


norms[1,12] <- 80 ### this is charles fogeting one answer
norms[,12] <- as.numeric(as.character(norms[,12])) 
#we calcuate averages weighted by how well experts know culture
#norms <- lapply(norms[ , -ncol(norms)], weighted.mean,  w = norms[,12])
#without weigning
norms <- data.frame(t(colMeans(norms[,1:11])))

names(norms) <- c("mgt","dectime", "decspace","decstriga", "decweed", "sold","time_prep","time_plant", "time_weed","time_spray", "time_harv")
norms <- data.frame(t(data.frame(norms)))
names(norms) <- "male_task"
norms$male_task <- 100 - norms$male_task
norms$dec <- rownames(norms)

experts_hiding <- experts[,21:29]

experts_hiding[1,9] <- 80 
experts_hiding[,9] <- as.numeric(as.character(experts_hiding[,9])) 

#we could actually do weighted averages 

experts_hiding <- lapply(experts_hiding[ , -ncol(experts_hiding)], weighted.mean,  w = experts_hiding[,9])

names(experts_hiding) <-  c("time_prep","time_plant", "time_weed","time_spray", "time_harv","sold", "sold_q","sold_p")

experts_hiding <- data.frame(t(data.frame(experts_hiding)))
names(experts_hiding) <- "easy_to_monitor"
experts_hiding$dec <- rownames(experts_hiding)
experts_hiding$dec[experts_hiding$dec=="sold_q"] <- "sold_quant"
experts_hiding$dec[experts_hiding$dec=="sold_p"] <- "income"

dta <- subset(dta, interview_status == "couple interviewed")

#double loop -  this needs to be stacked for plots and for decisions

#first create variables
 dec <- c("mgt","dectime", "decspace","decstriga", "decweed")
 tmp <- c()
for (i in (1:length(dec))) {

tmp <- c(tmp,paste(paste(dec[i],"cat_pl",sep="_"), 1:5, sep="_"))

}

new <- data.frame(matrix(NA,2548,25))
names(new) <- tmp
dta <- cbind(dta,new)

for (i in (1:5)) {
for (plot in (1:5)) {

### category 2: man says man decides or man says both decide and woman says both decide or woman says man dcides 

dta[paste(paste(dec[i],"cat_pl",sep="_") ,plot,sep="_")][(dta[paste(paste(dec[i],"man_pl",sep="_") ,plot,sep="")]==1 | dta[paste(paste(dec[i],"both_man_pl",sep="_") ,plot,sep="")]==1 ) &  (dta[paste(paste(dec[i],"both_woman_pl",sep="_") ,plot,sep="")]==1 | dta[paste(paste(dec[i],"man_woman_pl",sep="_") ,plot,sep="")]==1 )] <- "cat2"

#category 3: man says she decides, woman says she does not

dta[paste(paste(dec[i],"cat_pl",sep="_") ,plot,sep="_")][(dta[paste(paste(dec[i],"man_woman_pl",sep="_") ,plot,sep="")]==0 & dta[paste(paste(dec[i],"both_woman_pl",sep="_") ,plot,sep="")]==0 ) &  (dta[paste(paste(dec[i],"both_man_pl",sep="_") ,plot,sep="")]==1 | dta[paste(paste(dec[i],"man_pl",sep="_") ,plot,sep="")]==1 )] <- "cat3"

#category 4: woman says he decides, man says she does not

dta[paste(paste(dec[i],"cat_pl",sep="_") ,plot,sep="_")][(dta[paste(paste(dec[i],"man_woman_pl",sep="_") ,plot,sep="")]==1 | dta[paste(paste(dec[i],"both_woman_pl",sep="_") ,plot,sep="")]==1 ) &  (dta[paste(paste(dec[i],"both_man_pl",sep="_") ,plot,sep="")]==0 & dta[paste(paste(dec[i],"man_pl",sep="_") ,plot,sep="")]==0 )] <- "cat4"
### category 2: man says man does not  decides and woman says man does not decide 
dta[paste(paste(dec[i],"cat_pl",sep="_") ,plot,sep="_")][(dta[paste(paste(dec[i],"man_woman_pl",sep="_") ,plot,sep="")]==0 & dta[paste(paste(dec[i],"both_woman_pl",sep="_") ,plot,sep="")]==0 ) &  (dta[paste(paste(dec[i],"both_man_pl",sep="_") ,plot,sep="")]==0 & dta[paste(paste(dec[i],"man_pl",sep="_") ,plot,sep="")]==0 )] <- "cat1"
}
}


dta_part <- dta[c("hhid",tmp)]
### how can we reshape this
### step one - make long at plot level

dta_reshape <- reshape(dta_part, direction ="long", varying=tmp,v.names="new", times = tmp,  idvar="hhid")

dta_reshape$plot <-  as.numeric(str_sub(dta_reshape$time, nchar(dta_reshape$time),nchar(dta_reshape$time) ))


dta_reshape$dec <- str_sub(dta_reshape$time, 0,nchar(dta_reshape$time)-9 )
dta_reshape$time <- NULL
rownames(dta_reshape) <- NULL 
names(dta_reshape)[2] <- "var"

dta_reshape$disagree <- NA
dta_reshape$disagree[dta_reshape$var %in% c("cat3", "cat4")] <- 1
dta_reshape$disagree[dta_reshape$var %in% c("cat1", "cat2")] <- 0

#Wald test following https://cran.r-project.org/web/packages/clubSandwich/vignettes/Wald-tests-in-clubSandwich.html
#This tests the hypothesis that disagreement is the same 

#method 1 -  joint null hypothesis that disagreement is equal across decision categories

#mod <- lm(disagree~as.factor(dec),data=dta_reshape)
#vcov_cluster <- vcovCR(mod, cluster=dta_reshape$hhid, type = "CR2")
#coef_test(mod, vcov = vcov_cluster)
#Wald_test(mod, constraints = constrain_zero(2:5), vcov = vcov_cluster)

##method 2:

#mod <- lm(disagree~0+as.factor(dec),data=dta_reshape)

#vcov_cluster <- vcovCR(mod, cluster=dta_reshape$hhid, type = "CR2")
#coef_test(mod, vcov = vcov_cluster)

#Wald_test(mod, constraints = constrain_equal(1:5, coefs = coef(mod)), vcov = vcov_cluster)



### for sales

dta$sold[(dta$nr_bags_sold_man  > 0 | dta$nr_bags_sold_both_man >0)  & (dta$nr_bags_sold_man_woman > 0  | dta$nr_bags_sold_both_woman >0) ] <- "cat2"
dta$sold[(dta$nr_bags_sold_man  == 0 & dta$nr_bags_sold_both_man ==0 & dta$nr_bags_sold_man_woman == 0  & dta$nr_bags_sold_both_woman ==0)] <- "cat1"

dta$sold[((dta$nr_bags_sold_man  > 0| dta$nr_bags_sold_both_man >0) & dta$nr_bags_sold_man_woman == 0 & dta$nr_bags_sold_both_woman ==0)] <- "cat3"
dta$sold[(dta$nr_bags_sold_man  == 0  & dta$nr_bags_sold_both_man ==0  & (dta$nr_bags_sold_man_woman > 0 | dta$nr_bags_sold_both_woman >0) )] <- "cat4"
dta_part <- dta[c("hhid","sold")]

dta_reshape_2 <- reshape(dta_part, direction ="long", varying=c("sold"),v.names="new", times = c("sold"),  idvar="hhid")

dta_reshape_2$dec <- dta_reshape_2$time

dta_reshape_2$time <- NULL
rownames(dta_reshape_2) <- NULL 
names(dta_reshape_2)[2] <- "var"

dta_reshape_2$plot <- NA


dta_reshape_2$disagree <- NA
dta_reshape_2$disagree[dta_reshape_2$var %in% c("cat3", "cat4")] <- 1
dta_reshape_2$disagree[dta_reshape_2$var %in% c("cat1", "cat2")] <- 0

dta_reshape_2 <- dta_reshape_2[names(dta_reshape)]

dta_reshape <- data.frame(rbind(dta_reshape,dta_reshape_2))

################################################### labour time

#### aggregate weeding time

dta$time_weed_man_man_pl1 <- dta$time_weed1_man_man_pl1 + dta$time_weed2_man_man_pl1 + dta$time_weed3_man_man_pl1 
dta$time_weed_man_man_pl2 <- dta$time_weed1_man_man_pl2 + dta$time_weed2_man_man_pl2 + dta$time_weed3_man_man_pl2 
dta$time_weed_man_man_pl3 <- dta$time_weed1_man_man_pl3 + dta$time_weed2_man_man_pl3 + dta$time_weed3_man_man_pl3 
dta$time_weed_man_man_pl4 <- dta$time_weed1_man_man_pl4 + dta$time_weed2_man_man_pl4 + dta$time_weed3_man_man_pl4 
dta$time_weed_man_man_pl5 <- dta$time_weed1_man_man_pl5 + dta$time_weed2_man_man_pl5 + dta$time_weed3_man_man_pl5

dta$time_weed_woman_man_pl1 <- dta$time_weed1_woman_man_pl1 + dta$time_weed2_woman_man_pl1 + dta$time_weed3_woman_man_pl1 
dta$time_weed_woman_man_pl2 <- dta$time_weed1_woman_man_pl2 + dta$time_weed2_woman_man_pl2 + dta$time_weed3_woman_man_pl2 
dta$time_weed_woman_man_pl3 <- dta$time_weed1_woman_man_pl3 + dta$time_weed2_woman_man_pl3 + dta$time_weed3_woman_man_pl3 
dta$time_weed_woman_man_pl4 <- dta$time_weed1_woman_man_pl4 + dta$time_weed2_woman_man_pl4 + dta$time_weed3_woman_man_pl4 
dta$time_weed_woman_man_pl5 <- dta$time_weed1_woman_man_pl5 + dta$time_weed2_woman_man_pl5 + dta$time_weed3_woman_man_pl5 



dta$time_weed_man_woman_pl1 <- dta$time_weed1_man_woman_pl1 + dta$time_weed2_man_woman_pl1 + dta$time_weed3_man_woman_pl1 
dta$time_weed_man_woman_pl2 <- dta$time_weed1_man_woman_pl2 + dta$time_weed2_man_woman_pl2 + dta$time_weed3_man_woman_pl2 
dta$time_weed_man_woman_pl3 <- dta$time_weed1_man_woman_pl3 + dta$time_weed2_man_woman_pl3 + dta$time_weed3_man_woman_pl3 
dta$time_weed_man_woman_pl4 <- dta$time_weed1_man_woman_pl4 + dta$time_weed2_man_woman_pl4 + dta$time_weed3_man_woman_pl4 
dta$time_weed_man_woman_pl5 <- dta$time_weed1_man_woman_pl5 + dta$time_weed2_man_woman_pl5 + dta$time_weed3_man_woman_pl5 

dta$time_weed_woman_woman_pl1 <- dta$time_weed1_woman_woman_pl1 + dta$time_weed2_woman_woman_pl1 + dta$time_weed3_woman_woman_pl1 
dta$time_weed_woman_woman_pl2 <- dta$time_weed1_woman_woman_pl2 + dta$time_weed2_woman_woman_pl2 + dta$time_weed3_woman_woman_pl2 
dta$time_weed_woman_woman_pl3 <- dta$time_weed1_woman_woman_pl3 + dta$time_weed2_woman_woman_pl3 + dta$time_weed3_woman_woman_pl3 
dta$time_weed_woman_woman_pl4 <- dta$time_weed1_woman_woman_pl4 + dta$time_weed2_woman_woman_pl4 + dta$time_weed3_woman_woman_pl4 
dta$time_weed_woman_woman_pl5 <- dta$time_weed1_woman_woman_pl5 + dta$time_weed2_woman_woman_pl5 + dta$time_weed3_woman_woman_pl5 



##double loop -  this needs to be stacked for plots and for decisions

##first create variables
 dec <- c("time_prep","time_plant", "time_weed","time_spray", "time_harv")
 tmp <- c()
for (i in (1:length(dec))) {

tmp <- c(tmp,paste(paste(dec[i],"cat_pl",sep="_"), 1:5, sep="_"))

}

new <- data.frame(matrix(NA,2548,25))
names(new) <- tmp
dta <- cbind(dta,new)

for (i in (1:5)) {
for (plot in (1:5)) {


### category 2: man says woman decides or man says both decide and woman says both decide or woman says woman dcides 
lims <- sd(unlist( dta[paste(paste(dec[i],"man_woman_pl",sep="_") ,plot,sep="")] +  dta[paste(paste(dec[i],"man_man_pl",sep="_") ,plot,sep="")])/2,na.rm=T)


dta[paste(paste(dec[i],"cat_pl",sep="_") ,plot,sep="_")][abs(dta[paste(paste(dec[i],"man_woman_pl",sep="_") ,plot,sep="")] - dta[paste(paste(dec[i],"man_man_pl",sep="_") ,plot,sep="")]) < lims] <- "cat2"

#category 3: wife says she decides

dta[paste(paste(dec[i],"cat_pl",sep="_") ,plot,sep="_")][dta[paste(paste(dec[i],"man_man_pl",sep="_") ,plot,sep="")] - dta[paste(paste(dec[i],"man_woman_pl",sep="_") ,plot,sep="")]  >= lims] <- "cat3"

dta[paste(paste(dec[i],"cat_pl",sep="_") ,plot,sep="_")][dta[paste(paste(dec[i],"man_man_pl",sep="_") ,plot,sep="")] - dta[paste(paste(dec[i],"man_woman_pl",sep="_") ,plot,sep="")]  <= -lims] <- "cat4"

}
}


dta_part <- dta[c("hhid",tmp)]
### how can we reshape this
### step one - make long at plot level

dta_reshape_3 <- reshape(dta_part, direction ="long", varying=tmp,v.names="new", times = tmp,  idvar="hhid")

dta_reshape_3$plot <-  as.numeric(str_sub(dta_reshape_3$time, nchar(dta_reshape_3$time),nchar(dta_reshape_3$time) ))


dta_reshape_3$dec <- str_sub(dta_reshape_3$time, 0,nchar(dta_reshape_3$time)-9 )
dta_reshape_3$time <- NULL
rownames(dta_reshape_3) <- NULL 
names(dta_reshape_3)[2] <- "var"

dta_reshape_3$disagree <- NA
dta_reshape_3$disagree[dta_reshape_3$var %in% c("cat3", "cat4")] <- 1
dta_reshape_3$disagree[dta_reshape_3$var %in% c("cat1", "cat2")] <- 0


dta_reshape_3 <- dta_reshape_3[names(dta_reshape)]

dta_reshape <- data.frame(rbind(dta_reshape,dta_reshape_3))



##################################################### sales quantities and value


##double loop -  this needs to be stacked for plots and for decisions

##first create variables

#take absolute value of difference between what men and women report 

lims <- sd(unlist( (dta$nr_bags_sold_man + dta$nr_bags_sold_both_man) + (dta$nr_bags_sold_man_woman  + dta$nr_bags_sold_both_woman))/2,na.rm=T)



dta$sold_quant[abs((dta$nr_bags_sold_man + dta$nr_bags_sold_both_man) - (dta$nr_bags_sold_man_woman  + dta$nr_bags_sold_both_woman))<lims] <- "cat2"
dta$sold_quant[((dta$nr_bags_sold_man + dta$nr_bags_sold_both_man) - (dta$nr_bags_sold_man_woman  + dta$nr_bags_sold_both_woman))>=lims] <- "cat3"
dta$sold_quant[((dta$nr_bags_sold_man + dta$nr_bags_sold_both_man) - (dta$nr_bags_sold_man_woman  + dta$nr_bags_sold_both_woman))<=-lims] <- "cat4"



lims <- sd(unlist( (dta$income_man + dta$income_both_man) + (dta$income_man_woman+ dta$income_both_woman))/2,na.rm=T)
dta$income[abs((dta$income_man + dta$income_both_man) - (dta$income_man_woman+ dta$income_both_woman))<lims] <- "cat2"
dta$income[((dta$income_man + dta$income_both_man) - (dta$income_man_woman+ dta$income_both_woman)) >= lims] <- "cat3"
dta$income[((dta$income_man + dta$income_both_man) - (dta$income_man_woman+ dta$income_both_woman)) <= -lims] <- "cat4"

dta_part <- dta[c("hhid","sold_quant","income")]



dta_reshape_4 <- reshape(dta_part, direction ="long", varying=c("sold_quant","income"),v.names="new", times = c("sold_quant","income"),  idvar="hhid")

dta_reshape_4$dec <- dta_reshape_4$time

dta_reshape_4$time <- NULL
rownames(dta_reshape_4) <- NULL 
names(dta_reshape_4)[2] <- "var"

dta_reshape_4$plot <- NA


dta_reshape_4$disagree <- NA
dta_reshape_4$disagree[dta_reshape_4$var %in% c("cat3", "cat4")] <- 1
dta_reshape_4$disagree[dta_reshape_4$var %in% c("cat1", "cat2")] <- 0

dta_reshape_4 <- dta_reshape_4[names(dta_reshape)]


dta_reshape <- data.frame(rbind(dta_reshape,dta_reshape_4))
desc <- vector(mode="character", length=23)
desc[1] <- format(round(mean(dta$time_spray_woman, na.rm=T) , digits=1),nsmall=1)
desc[2] <- format(round(mean(dta$time_weed_woman, na.rm=T) , digits=1),nsmall=1)
desc[3] <- format(round(mean(dta$time_spray_woman==dta$time_spray_woman_man, na.rm=T)*100 , digits=0),nsmall=0)
desc[4] <-format(round(mean(dta$time_weed_woman==dta$time_weed_woman_man, na.rm=T)*100 , digits=0),nsmall=0)
desc[5:15] <- format(round(norms[,1] , digits=0),nsmall=0)
desc[16:23] <- format(round(experts_hiding[,1] , digits=0),nsmall=0)


#################################################### analysis
dec <- c("mgt","dectime", "decspace","decstriga", "decweed","sold")
mat_tab_m_1 <- matrix(NA, 7,13)
### table 1:
for (i in 1:6) {
mat_tab_m_1[1:4,i] <- prop.table(table(dta_reshape[dta_reshape$dec==dec[i],]$var))
mat_tab_m_1[5,i]  <- mat_tab_m_1[1,i] + mat_tab_m_1[2,i]
mat_tab_m_1[6,i] <- sum(table(dta_reshape[dta_reshape$dec==dec[i],]$var))
ptest <- prop.test(x= c(table(dta_reshape[dta_reshape$dec==dec[i],]$var)[c("cat3","cat4")]), n= c(sum(table(dta_reshape[dta_reshape$dec==dec[i],]$var)),sum(table(dta_reshape[dta_reshape$dec==dec[i],]$var))))

mat_tab_m_1[7,i] <-  ptest$p.value
}


dec <- c("time_prep","time_plant", "time_weed","time_spray", "time_harv","sold_quant","income")

### table 1:
for (i in 1:7) {
mat_tab_m_1[2:4,i+6] <- prop.table(table(dta_reshape[dta_reshape$dec==dec[i],]$var))
mat_tab_m_1[5,i+6]  <-  mat_tab_m_1[2,i+6]
mat_tab_m_1[6,i+6] <- sum(table(dta_reshape[dta_reshape$dec==dec[i],]$var))
ptest <- prop.test(x= c(table(dta_reshape[dta_reshape$dec==dec[i],]$var)[c("cat3","cat4")]), n= c(sum(table(dta_reshape[dta_reshape$dec==dec[i],]$var)),sum(table(dta_reshape[dta_reshape$dec==dec[i],]$var))))

mat_tab_m_1[7,i+6] <-  ptest$p.value
}
mat_tab_m_1[7,]<- format(round(mat_tab_m_1[7,],digits=3),nsmall=3)
mat_tab_m_1[6,]<- format(round(as.numeric(mat_tab_m_1[6,]),digits=0),nsmall=0, big.mark=",")
mat_tab_m_1[1:5,]<- format(round(as.numeric(mat_tab_m_1[1:5,]),digits=2),nsmall=0)

#for sales
prop.table(table(dta_reshape[dta_reshape$dec=="sold",]$var))[1] + prop.table(table(dta_reshape[dta_reshape$dec=="sold",]$var))[2]
prop.table(table(dta_reshape[dta_reshape$dec=="sold_quantity",]$var))

### collecting F-test
#ftests_m <- vector(mode="character", length=10)

mod <- lm(disagree~0+as.factor(dec),data=dta_reshape[dta_reshape$dec %in% c("mgt","dectime", "decspace","decstriga", "decweed","sold"),])
vcov_cluster <- vcovCR(mod, cluster=dta_reshape[dta_reshape$dec %in% c("mgt","dectime", "decspace","decstriga", "decweed","sold"),]$hhid, type = "CR2")
coef_test(mod, vcov = vcov_cluster)

##save this because this takes a lot of time to run
#ftests_m[1] <- format(round(Wald_test(mod, constraints = constrain_equal(1:5, coefs = coef(mod)), vcov = vcov_cluster)$p_val,digits=3),nsmall=3)

mod <- lm(disagree~0+as.factor(dec),data=dta_reshape[dta_reshape$dec %in% c("time_prep","time_plant", "time_weed","time_spray", "time_harv"),])
vcov_cluster <- vcovCR(mod, cluster=dta_reshape[dta_reshape$dec %in% c("time_prep","time_plant", "time_weed","time_spray", "time_harv"),]$hhid, type = "CR2")
coef_test(mod, vcov = vcov_cluster)

##save this because this takes a lot of time to run
#ftests_m[2] <- format(round(Wald_test(mod, constraints = constrain_equal(1:5, coefs = coef(mod)), vcov = vcov_cluster)$p_val,digits=3),nsmall=3)

mod <- lm(disagree~0+as.factor(dec),data=dta_reshape[dta_reshape$dec %in% c("sold","sold_quant","income"),])
vcov_cluster <- vcovCR(mod, cluster=dta_reshape[dta_reshape$dec %in% c("sold","sold_quant","income"),]$hhid, type = "CR2")
coef_test(mod, vcov = vcov_cluster)

##save this because this takes a lot of time to run
#ftests_m[3] <- format(round(Wald_test(mod, constraints = constrain_equal(1:3, coefs = coef(mod)), vcov = vcov_cluster)$p_val,digits=3),nsmall=3)



#save(ftests_m, file = "/home/bjvca/data/projects/digital green/papers/DP_disagreement/ftest_m.Rd")
save(mat_tab_m_1, file = "/home/bjvca/data/projects/digital green/papers/DP_disagreement/mat_tab_m_1.Rd")
save(desc, file = "/home/bjvca/data/projects/digital green/papers/DP_disagreement/desc.Rd")


# ### read merge in treatments

dta_reshape <- merge(dta_reshape,dta[c("hhid","messenger","recipient")], by.x="hhid", by.y="hhid" ) 
#mere in nroms and hiding indicators
dta_reshape <-  merge(dta_reshape, norms, by.x="dec", by.y="dec", all.x=T)
dta_reshape <-  merge(dta_reshape, experts_hiding, by.x="dec", by.y="dec", all.x=T)


### Q: does challenging role conguence reduce disagreement?
mat_tab_m_2 <- matrix(NA, 12,3)

##keep only observations where both spouses were exposed to the video
dta_copy <- subset(dta_reshape, recipient == "couple")
### compare couple messenger to the default (male only messenger) - for robustness, we could also try comparing male only to (couple + female)
dta_copy <- subset(dta_copy, messenger != "ctrl")
dta_copy <- subset(dta_copy, messenger != "female")

dta_copy <- subset(dta_copy, dec %in% c("mgt","dectime", "decspace","decstriga", "decweed","sold"))

mod <- lm((var=="cat3")~as.factor(messenger=="couple"),data=dta_copy)
vcov_cluster <- vcovCR(mod, cluster=dta_copy$hhid, type = "CR2")
coef_test(mod, vcov = vcov_cluster)
mat_tab_m_2[1,1] <- coef_test(mod, vcov = vcov_cluster)[1,1] ##const
mat_tab_m_2[2,1] <- coef_test(mod, vcov = vcov_cluster)[1,2] ##SE

mat_tab_m_2[3,1] <- coef_test(mod, vcov = vcov_cluster)[2,1] ##const
mat_tab_m_2[4,1] <- coef_test(mod, vcov = vcov_cluster)[2,2] ##SE
mat_tab_m_2[5,1] <- coef_test(mod, vcov = vcov_cluster)[2,5] ##p-val
mat_tab_m_2[12,1] <- nobs(mod) ##nobs

mod <- lm((var=="cat3")~(male_task >55  | male_task < 45),data=dta_copy)
vcov_cluster <- vcovCR(mod, cluster=dta_copy$hhid, type = "CR2")
coef_test(mod, vcov = vcov_cluster)
mat_tab_m_2[1,2] <- coef_test(mod, vcov = vcov_cluster)[1,1] ##const
mat_tab_m_2[2,2] <- coef_test(mod, vcov = vcov_cluster)[1,2] ##SE
mat_tab_m_2[6,2] <- coef_test(mod, vcov = vcov_cluster)[2,1] ##const
mat_tab_m_2[7,2] <- coef_test(mod, vcov = vcov_cluster)[2,2] ##SE
mat_tab_m_2[8,2] <- coef_test(mod, vcov = vcov_cluster)[2,5] ##p-val
mat_tab_m_2[12,2] <- nobs(mod) ##nobs

mod <- lm((var=="cat3")~as.factor(messenger=="couple")*( male_task >55 | male_task < 45),data=dta_copy)
vcov_cluster <- vcovCR(mod, cluster=dta_copy$hhid, type = "CR2")
coef_test(mod, vcov = vcov_cluster)

#mod <- lm(disagree~as.factor(dec),data=dta_reshape)
#vcov_cluster <- vcovCR(mod, cluster=dta_reshape$hhid, type = "CR2")
#coef_test(mod, vcov = vcov_cluster)
#Wald_test(mod, constraints = constrain_zero(7:10), vcov = vcov_cluster)


mat_tab_m_2[1,3] <- coef_test(mod, vcov = vcov_cluster)[1,1] ##const
mat_tab_m_2[2,3] <- coef_test(mod, vcov = vcov_cluster)[1,2] ##SE
mat_tab_m_2[3,3] <- coef_test(mod, vcov = vcov_cluster)[2,1] ##const
mat_tab_m_2[4,3] <- coef_test(mod, vcov = vcov_cluster)[2,2] ##SE
mat_tab_m_2[5,3] <- coef_test(mod, vcov = vcov_cluster)[2,5] ##p-val

mat_tab_m_2[6,3] <- coef_test(mod, vcov = vcov_cluster)[3,1] ##const
mat_tab_m_2[7,3] <- coef_test(mod, vcov = vcov_cluster)[3,2] ##SE
mat_tab_m_2[8,3] <- coef_test(mod, vcov = vcov_cluster)[3,5] ##p-val

mat_tab_m_2[9,3] <- coef_test(mod, vcov = vcov_cluster)[4,1] ##const
mat_tab_m_2[10,3] <- coef_test(mod, vcov = vcov_cluster)[4,2] ##SE
mat_tab_m_2[11,3] <- coef_test(mod, vcov = vcov_cluster)[4,5] ##p-val
mat_tab_m_2[12,3] <- nobs(mod) ##nobs
save(mat_tab_m_2, file = "/home/bjvca/data/projects/digital green/papers/DP_disagreement/mat_tab_m_2.Rd")

##now for labour
mat_tab_m_3 <- matrix(NA, 12,3)
dta_copy <- subset(dta_reshape, recipient == "couple")

dta_copy <- subset(dta_copy, messenger != "ctrl")
dta_copy <- subset(dta_copy, messenger != "female")

dta_copy <- subset(dta_copy, dec %in%  c("time_prep","time_plant", "time_weed","time_spray", "time_harv"))

mod <- lm((var=="cat4")~as.factor(messenger=="couple"),data=dta_copy)
vcov_cluster <- vcovCR(mod, cluster=dta_copy$hhid, type = "CR2")
coef_test(mod, vcov = vcov_cluster)
mat_tab_m_3[1,1] <- coef_test(mod, vcov = vcov_cluster)[1,1] ##const
mat_tab_m_3[2,1] <- coef_test(mod, vcov = vcov_cluster)[1,2] ##SE

mat_tab_m_3[3,1] <- coef_test(mod, vcov = vcov_cluster)[2,1] ##const
mat_tab_m_3[4,1] <- coef_test(mod, vcov = vcov_cluster)[2,2] ##SE
mat_tab_m_3[5,1] <- coef_test(mod, vcov = vcov_cluster)[2,5] ##p-val
mat_tab_m_3[12,1] <- nobs(mod) ##nobs

mod <- lm((var=="cat4")~( male_task >55 | male_task < 45),data=dta_copy)
vcov_cluster <- vcovCR(mod, cluster=dta_copy$hhid, type = "CR2")
coef_test(mod, vcov = vcov_cluster)
mat_tab_m_3[1,2] <- coef_test(mod, vcov = vcov_cluster)[1,1] ##const
mat_tab_m_3[2,2] <- coef_test(mod, vcov = vcov_cluster)[1,2] ##SE
mat_tab_m_3[6,2] <- coef_test(mod, vcov = vcov_cluster)[2,1] ##const
mat_tab_m_3[7,2] <- coef_test(mod, vcov = vcov_cluster)[2,2] ##SE
mat_tab_m_3[8,2] <- coef_test(mod, vcov = vcov_cluster)[2,5] ##p-val
mat_tab_m_3[12,2] <- nobs(mod) ##nobs

mod <- lm((var=="cat4")~as.factor(messenger=="couple")*(male_task >55 | male_task < 45),data=dta_copy)
vcov_cluster <- vcovCR(mod, cluster=dta_copy$hhid, type = "CR2")
coef_test(mod, vcov = vcov_cluster)

mat_tab_m_3[1,3] <- coef_test(mod, vcov = vcov_cluster)[1,1] ##const
mat_tab_m_3[2,3] <- coef_test(mod, vcov = vcov_cluster)[1,2] ##SE
mat_tab_m_3[3,3] <- coef_test(mod, vcov = vcov_cluster)[2,1] ##const
mat_tab_m_3[4,3] <- coef_test(mod, vcov = vcov_cluster)[2,2] ##SE
mat_tab_m_3[5,3] <- coef_test(mod, vcov = vcov_cluster)[2,5] ##p-val

mat_tab_m_3[6,3] <- coef_test(mod, vcov = vcov_cluster)[3,1] ##const
mat_tab_m_3[7,3] <- coef_test(mod, vcov = vcov_cluster)[3,2] ##SE
mat_tab_m_3[8,3] <- coef_test(mod, vcov = vcov_cluster)[3,5] ##p-val

mat_tab_m_3[9,3] <- coef_test(mod, vcov = vcov_cluster)[4,1] ##const
mat_tab_m_3[10,3] <- coef_test(mod, vcov = vcov_cluster)[4,2] ##SE
mat_tab_m_3[11,3] <- coef_test(mod, vcov = vcov_cluster)[4,5] ##p-val
mat_tab_m_3[12,3] <- nobs(mod) ##nobs
save(mat_tab_m_3, file = "/home/bjvca/data/projects/digital green/papers/DP_disagreement/mat_tab_m_3.Rd")

#now for marketing
mat_tab_m_6 <- matrix(NA, 12,3)
dta_copy <- subset(dta_reshape, recipient == "couple")

dta_copy <- subset(dta_copy, messenger != "ctrl")
dta_copy <- subset(dta_copy, messenger != "female")

dta_copy <- subset(dta_copy, dec %in%  c("sold","sold_quant","income"))

mod <- lm((var=="cat3")~as.factor(messenger=="couple"),data=dta_copy)
vcov_cluster <- vcovCR(mod, cluster=dta_copy$hhid, type = "CR2")
coef_test(mod, vcov = vcov_cluster)
mat_tab_m_6[1,1] <- coef_test(mod, vcov = vcov_cluster)[1,1] ##const
mat_tab_m_6[2,1] <- coef_test(mod, vcov = vcov_cluster)[1,2] ##SE

mat_tab_m_6[3,1] <- coef_test(mod, vcov = vcov_cluster)[2,1] ##const
mat_tab_m_6[4,1] <- coef_test(mod, vcov = vcov_cluster)[2,2] ##SE
mat_tab_m_6[5,1] <- coef_test(mod, vcov = vcov_cluster)[2,5] ##p-val
mat_tab_m_6[12,1] <- nobs(mod) ##nobs

mod <- lm((var=="cat3")~( male_task >55 | male_task < 45),data=dta_copy)
vcov_cluster <- vcovCR(mod, cluster=dta_copy$hhid, type = "CR2")
coef_test(mod, vcov = vcov_cluster)
mat_tab_m_6[1,2] <- coef_test(mod, vcov = vcov_cluster)[1,1] ##const
mat_tab_m_6[2,2] <- coef_test(mod, vcov = vcov_cluster)[1,2] ##SE
mat_tab_m_6[6,2] <- coef_test(mod, vcov = vcov_cluster)[2,1] ##const
mat_tab_m_6[7,2] <- coef_test(mod, vcov = vcov_cluster)[2,2] ##SE
mat_tab_m_6[8,2] <- coef_test(mod, vcov = vcov_cluster)[2,5] ##p-val
mat_tab_m_6[12,2] <- nobs(mod) ##nobs

mod <- lm((var=="cat3")~as.factor(messenger=="couple")*( male_task >55 | male_task < 45),data=dta_copy)
vcov_cluster <- vcovCR(mod, cluster=dta_copy$hhid, type = "CR2")
coef_test(mod, vcov = vcov_cluster)

mat_tab_m_6[1,3] <- coef_test(mod, vcov = vcov_cluster)[1,1] ##const
mat_tab_m_6[2,3] <- coef_test(mod, vcov = vcov_cluster)[1,2] ##SE
mat_tab_m_6[3,3] <- coef_test(mod, vcov = vcov_cluster)[2,1] ##const
mat_tab_m_6[4,3] <- coef_test(mod, vcov = vcov_cluster)[2,2] ##SE
mat_tab_m_6[5,3] <- coef_test(mod, vcov = vcov_cluster)[2,5] ##p-val

mat_tab_m_6[6,3] <- coef_test(mod, vcov = vcov_cluster)[3,1] ##const
mat_tab_m_6[7,3] <- coef_test(mod, vcov = vcov_cluster)[3,2] ##SE
mat_tab_m_6[8,3] <- coef_test(mod, vcov = vcov_cluster)[3,5] ##p-val

mat_tab_m_6[9,3] <- coef_test(mod, vcov = vcov_cluster)[4,1] ##const
mat_tab_m_6[10,3] <- coef_test(mod, vcov = vcov_cluster)[4,2] ##SE
mat_tab_m_6[11,3] <- coef_test(mod, vcov = vcov_cluster)[4,5] ##p-val
mat_tab_m_6[12,3] <- nobs(mod) ##nobs
save(mat_tab_m_6, file = "/home/bjvca/data/projects/digital green/papers/DP_disagreement/mat_tab_m_6.Rd")



#### Q: does reducing scope for hiding, by giving spouses the same information, reduce disagreement?
#keep only farmers that were exposed to the video featuring a couple
dta_copy <- subset(dta_reshape, messenger == "couple")

dta_copy <- subset(dta_copy, dec %in% c("mgt","dectime", "decspace","decstriga", "decweed","sold"))
mat_tab_m_4 <- matrix(NA, 12,3)

mod <- lm((var=="cat3")~as.factor(recipient=="couple"),data=dta_copy)
vcov_cluster <- vcovCR(mod, cluster=dta_copy$hhid, type = "CR2")
coef_test(mod, vcov = vcov_cluster)

mat_tab_m_4[1,1] <- coef_test(mod, vcov = vcov_cluster)[1,1] ##const
mat_tab_m_4[2,1] <- coef_test(mod, vcov = vcov_cluster)[1,2] ##SE

mat_tab_m_4[3,1] <- coef_test(mod, vcov = vcov_cluster)[2,1] ##const
mat_tab_m_4[4,1] <- coef_test(mod, vcov = vcov_cluster)[2,2] ##SE
mat_tab_m_4[5,1] <- coef_test(mod, vcov = vcov_cluster)[2,5] ##p-val
mat_tab_m_4[12,1] <- nobs(mod) ##nobs

mod <- lm((var=="cat3")~scale(male_task>55 | male_task < 45),data=dta_copy)
vcov_cluster <- vcovCR(mod, cluster=dta_copy$hhid, type = "CR2")
coef_test(mod, vcov = vcov_cluster)

mod <- lm((var=="cat3")~as.factor(recipient=="couple")*scale(male_task>55 | male_task < 45),data=dta_copy)
vcov_cluster <- vcovCR(mod, cluster=dta_copy$hhid, type = "CR2")
coef_test(mod, vcov = vcov_cluster)

save(mat_tab_m_4, file = "/home/bjvca/data/projects/digital green/papers/DP_disagreement/mat_tab_m_4.Rd")

### now for labour

dta_copy <- subset(dta_reshape, messenger == "couple")

dta_copy <- subset(dta_copy, dec %in% c("time_prep","time_plant", "time_weed","time_spray", "time_harv"))
mat_tab_m_5 <- matrix(NA, 12,3)

mod <- lm((var=="cat4")~as.factor(recipient=="couple"),data=dta_copy)
vcov_cluster <- vcovCR(mod, cluster=dta_copy$hhid, type = "CR2")
coef_test(mod, vcov = vcov_cluster)

mat_tab_m_5[1,1] <- coef_test(mod, vcov = vcov_cluster)[1,1] ##const
mat_tab_m_5[2,1] <- coef_test(mod, vcov = vcov_cluster)[1,2] ##SE

mat_tab_m_5[3,1] <- coef_test(mod, vcov = vcov_cluster)[2,1] ##const
mat_tab_m_5[4,1] <- coef_test(mod, vcov = vcov_cluster)[2,2] ##SE
mat_tab_m_5[5,1] <- coef_test(mod, vcov = vcov_cluster)[2,5] ##p-val
mat_tab_m_5[12,1] <- nobs(mod) ##nobs


mod <- lm((var=="cat4")~((100-easy_to_monitor)>40),data=dta_copy)
vcov_cluster <- vcovCR(mod, cluster=dta_copy$hhid, type = "CR2")
coef_test(mod, vcov = vcov_cluster)
mat_tab_m_5[1,2] <- coef_test(mod, vcov = vcov_cluster)[1,1] ##const
mat_tab_m_5[2,2] <- coef_test(mod, vcov = vcov_cluster)[1,2] ##SE
mat_tab_m_5[6,2] <- coef_test(mod, vcov = vcov_cluster)[2,1] ##const
mat_tab_m_5[7,2] <- coef_test(mod, vcov = vcov_cluster)[2,2] ##SE
mat_tab_m_5[8,2] <- coef_test(mod, vcov = vcov_cluster)[2,5] ##p-val
mat_tab_m_5[12,2] <- nobs(mod) ##nobs

mod <- lm((var=="cat4")~as.factor(recipient=="couple")*((100-easy_to_monitor)>40),data=dta_copy)
vcov_cluster <- vcovCR(mod, cluster=dta_copy$hhid, type = "CR2")
coef_test(mod, vcov = vcov_cluster)

mat_tab_m_5[1,3] <- coef_test(mod, vcov = vcov_cluster)[1,1] ##const
mat_tab_m_5[2,3] <- coef_test(mod, vcov = vcov_cluster)[1,2] ##SE
mat_tab_m_5[3,3] <- coef_test(mod, vcov = vcov_cluster)[2,1] ##const
mat_tab_m_5[4,3] <- coef_test(mod, vcov = vcov_cluster)[2,2] ##SE
mat_tab_m_5[5,3] <- coef_test(mod, vcov = vcov_cluster)[2,5] ##p-val

mat_tab_m_5[6,3] <- coef_test(mod, vcov = vcov_cluster)[3,1] ##const
mat_tab_m_5[7,3] <- coef_test(mod, vcov = vcov_cluster)[3,2] ##SE
mat_tab_m_5[8,3] <- coef_test(mod, vcov = vcov_cluster)[3,5] ##p-val

mat_tab_m_5[9,3] <- coef_test(mod, vcov = vcov_cluster)[4,1] ##const
mat_tab_m_5[10,3] <- coef_test(mod, vcov = vcov_cluster)[4,2] ##SE
mat_tab_m_5[11,3] <- coef_test(mod, vcov = vcov_cluster)[4,5] ##p-val
mat_tab_m_5[12,3] <- nobs(mod) ##nobs
save(mat_tab_m_5, file = "/home/bjvca/data/projects/digital green/papers/DP_disagreement/mat_tab_m_5.Rd")

#now for marketing

dta_copy <- subset(dta_reshape, messenger == "couple")

dta_copy <- subset(dta_copy, dec %in% c("sold"))
mat_tab_m_7 <- matrix(NA, 12,3)

mod <- lm((var=="cat3")~as.factor(recipient=="couple"),data=dta_copy)
vcov_cluster <- vcovCR(mod, cluster=dta_copy$hhid, type = "CR2")
coef_test(mod, vcov = vcov_cluster)

mat_tab_m_7[1,1] <- coef_test(mod, vcov = vcov_cluster)[1,1] ##const
mat_tab_m_7[2,1] <- coef_test(mod, vcov = vcov_cluster)[1,2] ##SE

mat_tab_m_7[3,1] <- coef_test(mod, vcov = vcov_cluster)[2,1] ##const
mat_tab_m_7[4,1] <- coef_test(mod, vcov = vcov_cluster)[2,2] ##SE
mat_tab_m_7[5,1] <- coef_test(mod, vcov = vcov_cluster)[2,5] ##p-val
mat_tab_m_7[12,1] <- nobs(mod) ##nobs

dta_copy <- subset(dta_reshape, messenger == "couple")
dta_copy <- subset(dta_copy, dec %in% c("sold_quant"))
mod <- lm((var=="cat3")~as.factor(recipient=="couple"),data=dta_copy)
vcov_cluster <- vcovCR(mod, cluster=dta_copy$hhid, type = "CR2")
coef_test(mod, vcov = vcov_cluster)

mat_tab_m_7[1,2] <- coef_test(mod, vcov = vcov_cluster)[1,1] ##const
mat_tab_m_7[2,2] <- coef_test(mod, vcov = vcov_cluster)[1,2] ##SE
mat_tab_m_7[3,2] <- coef_test(mod, vcov = vcov_cluster)[2,1] ##const
mat_tab_m_7[4,2] <- coef_test(mod, vcov = vcov_cluster)[2,2] ##SE
mat_tab_m_7[5,2] <- coef_test(mod, vcov = vcov_cluster)[2,5] ##p-val
mat_tab_m_7[12,2] <- nobs(mod) ##nobs
dta_copy <- subset(dta_reshape, messenger == "couple")
dta_copy <- subset(dta_copy, dec %in% c("income"))
mod <- lm((var=="cat3")~as.factor(recipient=="couple"),data=dta_copy)
vcov_cluster <- vcovCR(mod, cluster=dta_copy$hhid, type = "CR2")
coef_test(mod, vcov = vcov_cluster)

mat_tab_m_7[1,3] <- coef_test(mod, vcov = vcov_cluster)[1,1] ##const
mat_tab_m_7[2,3] <- coef_test(mod, vcov = vcov_cluster)[1,2] ##SE
mat_tab_m_7[3,3] <- coef_test(mod, vcov = vcov_cluster)[2,1] ##const
mat_tab_m_7[4,3] <- coef_test(mod, vcov = vcov_cluster)[2,2] ##SE
mat_tab_m_7[5,3] <- coef_test(mod, vcov = vcov_cluster)[2,5] ##p-val
mat_tab_m_7[12,3] <- nobs(mod) ##nobs


save(mat_tab_m_7, file = "/home/bjvca/data/projects/digital green/papers/DP_disagreement/mat_tab_m_7.Rd")

