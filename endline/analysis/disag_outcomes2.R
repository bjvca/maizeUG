### redo analysis on practices at the individual level. This time unit of analysis is plot level decision making for each practice seperately


rm(list=ls())
source("/home/bjvca/data/projects/digital green/endline/data/init.R")
#source("functions.R")
#dta <- read.csv("AWS.csv")

#set totrep to zero if you do not want simulation based inferecne
repl <- 10000
#set this to true if you want to run WYFSR
wyfs_stat <- FALSE

set.seed(07032018)
dta <- subset(dta, !is.na(interview_status))
dta$messenger <- as.character(dta$messenger)

### indexing results arrays

res_pract_mm <- array(NA, c(10,4,4))
rownames(res_pract_mm) <- c("space","striga","weed", "use_fert","seed","combiner","bought_seed","chem","labour","immediate")
res_pract_wm <- array(NA, c(10,4,4))
rownames(res_pract_wm) <- c("space","striga","weed", "use_fert","seed","combiner","bought_seed","chem","labour","immediate")
res_pract_bm <- array(NA, c(10,4,4))
rownames(res_pract_bm) <- c("space","striga","weed", "use_fert","seed","combiner","bought_seed","chem","labour","immediate")


res_prod_fm <- array(NA, c(5,4,4))
rownames(res_prod_fm) <- c("prod","area","yield","yield_better","prod_index")
res_prod_fi <- array(NA, c(5,4,4))
rownames(res_prod_fi) <- c("prod","area","yield","yield_better","prod_index")
res_prod_mm <- array(NA, c(5,4,4))
rownames(res_prod_mm) <- c("prod","area","yield","yield_better","prod_index")
res_prod_bm <- array(NA, c(5,4,4))
rownames(res_prod_bm) <- c("prod","area","yield","yield_better","prod_index")
res_prod_wm_share1 <- array(NA, c(5,4,4))
rownames(res_prod_wm_share1) <- c("prod","area","yield","yield_better","prod_index")
res_prod_wm_share2 <- array(NA, c(5,4,4))
rownames(res_prod_wm_share2) <- c("prod","area","yield","yield_better","prod_index")


res_h0_wel <-  array(NA, c(6,4,4))
rownames(res_h0_wel) <- c("better_av","better_6m","eatpref","eatenough","log_cons","welfare_index")
res_h0_disp <-  array(NA, c(4,4,4))
rownames(res_h0_disp) <- c("cons_maize","sold_maize","saved_seed","disp_index")
res_decision <- array(NA, c(6,4,4))
rownames(res_decision) <- c("dec_male","decide_female","female_involved","decide_joint", "both_tell","spouses_listen")

dta[c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26")] <- dta[c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26")]==3
dta[c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26")] <- dta[c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26")] == 3

dta[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")] <- dta[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")] == "Yes"
dta[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29")] <- dta[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29")] == "Yes"

dta[ paste("dec_man",paste("_pl",1:5, sep=""), sep="")] <- dta[ paste("dec_man",paste("_pl",1:5, sep=""), sep="")] >0
dta[ paste("dec_woman",paste("_pl",1:5, sep=""), sep="")] <- dta[ paste("dec_man",paste("_pl",1:5, sep=""), sep="")] >0
dta[ paste("dec_both",paste("_pl",1:5, sep=""), sep="")] <- dta[ paste("dec_man",paste("_pl",1:5, sep=""), sep="")] >0

dta[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")] <- dta[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")] == "Yes"
dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42")] <- dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42")] == "Yes"

dta[c("grp1a55a" ,"grp2b55b", "grp3c55b","grp4d55b","grp5e55b")]  <- dta[c("grp1a55a" ,"grp2b55b", "grp3c55b","grp4d55b","grp5e55b")] == "Yes"
dta[ c("spouse2grp_sp1f55a","spouse2grp_sp2g55b","spouse2grp_sp3h55b","spouse2group_sp4j55b","spouse2grp5_sp5k55b")] <- dta[ c("spouse2grp_sp1f55a","spouse2grp_sp2g55b","spouse2grp_sp3h55b","spouse2group_sp4j55b","spouse2grp5_sp5k55b")]=="Yes"

dta[c("grp1a151","grp2b55b","grp3c55b","grp4d55b","grp5e55b")] <- dta[c("grp1a151","grp2b55b","grp3c55b","grp4d55b","grp5e55b")] =="Yes"
dta[c("spouse2grp_sp1f151","spouse2grp_sp2g151","spouse2grp_sp3h151","spouse2group_sp4j151","spouse2grp5_sp5k151")] <- dta[c("spouse2grp_sp1f151","spouse2grp_sp2g151","spouse2grp_sp3h151","spouse2group_sp4j151","spouse2grp5_sp5k151")] =="Yes"

dta[c("grp1seed_purchase1","grp2seed_purchase2","grp3seed_purchase3","grp4seed_purchase4","grp5seed_purchase5")] <- dta[c("grp1seed_purchase1","grp2seed_purchase2","grp3seed_purchase3","grp4seed_purchase4","grp5seed_purchase5")]=="Yes"
dta[c("spouse2grp_sp1seed_purchasesp1", "spouse2grp_sp2seed_purchase_sp2","spouse2grp_sp3seed_purchasesp3","spouse2group_sp4seed_purchasesp4","spouse2grp5_sp5seed_purchasesp5")] <- dta[c("spouse2grp_sp1seed_purchasesp1","spouse2grp_sp2seed_purchase_sp2","spouse2grp_sp3seed_purchasesp3","spouse2group_sp4seed_purchasesp4", "spouse2grp5_sp5seed_purchasesp5")]=="Yes"

c("spouse2grp_sp1f55a","spouse2grp_sp2g55b","spouse2grp_sp3h55b","spouse2group_sp4j55b","spouse2grp5_sp5k55b")

dta$combiner_sp1_pl1 <- NA
dta$combiner_sp1_pl2  <- NA
dta$combiner_sp1_pl3  <- NA
dta$combiner_sp1_pl4  <- NA
dta$combiner_sp1_pl5  <- NA
dta[c("combiner_sp1_pl1","combiner_sp1_pl2","combiner_sp1_pl3","combiner_sp1_pl4","combiner_sp1_pl5")]<- dta[c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42")] *  dta[c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29")]

dta$combiner_sp2_pl1 <- NA
dta$combiner_sp2_pl2  <- NA
dta$combiner_sp2_pl3  <- NA
dta$combiner_sp2_pl4  <- NA
dta$combiner_sp2_pl5  <- NA
dta[c("combiner_sp2_pl1","combiner_sp2_pl2","combiner_sp2_pl3","combiner_sp2_pl4","combiner_sp2_pl5")]<- dta[c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42")] *  dta[c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29")]

dta[c("grp1days1","grp2days2","grp3days3","grp4days4", "grp5days5")] <- dta[c("grp1days1","grp2days2","grp3days3","grp4days4", "grp5days5")] ==1
dta[c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5")] <- dta[c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5")] ==1

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
#s_h1 <- min(table(dta$messenger[dta$messenger != "ctrl"], dta$recipient[dta$messenger != "ctrl"]))
#dta_bal <- rbind(dta[dta$messenger=="ctrl",],
# dta[dta$messenger=="couple" & dta$recipient == "couple",][sample( nrow(dta[dta$messenger=="couple" & dta$recipient == "couple",]),s_h1),],
# dta[dta$messenger=="male" & dta$recipient == "couple",][sample( nrow(dta[dta$messenger=="male" & dta$recipient == "couple",]),s_h1),],
# dta[dta$messenger=="female" & dta$recipient == "couple",][sample( nrow(dta[dta$messenger=="female" & dta$recipient == "couple",]),s_h1),],
# dta[dta$messenger=="female" & dta$recipient == "male",][sample( nrow(dta[dta$messenger=="female" & dta$recipient == "male",]),s_h1),],
# dta[dta$messenger=="female" & dta$recipient == "female",][sample( nrow(dta[dta$messenger=="female" & dta$recipient == "female",]),s_h1),],
# dta[dta$messenger=="male" & dta$recipient == "male",][sample( nrow(dta[dta$messenger=="male" & dta$recipient == "male",]),s_h1),],
# dta[dta$messenger=="male" & dta$recipient == "female",][sample( nrow(dta[dta$messenger=="male" & dta$recipient == "female",]),s_h1),],
# dta[dta$messenger=="couple" & dta$recipient == "male",][sample( nrow(dta[dta$messenger=="couple" & dta$recipient == "male",]),s_h1),],
# dta[dta$messenger=="couple" & dta$recipient == "female",][sample( nrow(dta[dta$messenger=="couple" & dta$recipient == "female",]),s_h1),])

treatment <- "(messenger != 'ctrl') +ivr+sms+as.factor(recipient)+ as.factor(messenger) + called + (totsms >0)" 
} else if (h==2) {
############################################# reducing information asymmetries ##############################################
## drop the control
dta <- subset(dta, messenger != "ctrl")
## sample size for balance H0
#s_h0 <- min(table(dta$messenger, dta$recipient)[,1])
### sample size for balance H1
#s_h1 <- min(table(dta$messenger, dta$recipient)[,-1])

#dta_bal <- rbind( dta[dta$messenger=="couple" & dta$recipient == "couple",][sample( nrow(dta[dta$messenger=="couple" & dta$recipient == "couple",]),s_h0),],
# dta[dta$messenger=="male" & dta$recipient == "couple",][sample( nrow(dta[dta$messenger=="male" & dta$recipient == "couple",]),s_h0),],
# dta[dta$messenger=="female" & dta$recipient == "couple",][sample( nrow(dta[dta$messenger=="female" & dta$recipient == "couple",]),s_h0),],

# dta[dta$messenger=="female" & dta$recipient == "male",][sample( nrow(dta[dta$messenger=="female" & dta$recipient == "male",]),s_h1),],
# dta[dta$messenger=="female" & dta$recipient == "female",][sample( nrow(dta[dta$messenger=="female" & dta$recipient == "female",]),s_h1),],
# dta[dta$messenger=="male" & dta$recipient == "male",][sample( nrow(dta[dta$messenger=="male" & dta$recipient == "male",]),s_h1),],
# dta[dta$messenger=="male" & dta$recipient == "female",][sample( nrow(dta[dta$messenger=="male" & dta$recipient == "female",]),s_h1),],
# dta[dta$messenger=="couple" & dta$recipient == "male",][sample( nrow(dta[dta$messenger=="couple" & dta$recipient == "male",]),s_h1),],
# dta[dta$messenger=="couple" & dta$recipient == "female",][sample( nrow(dta[dta$messenger=="couple" & dta$recipient == "female",]),s_h1),])

treatment <- "(recipient == 'couple') +ivr+sms+as.factor(messenger) + called + (totsms >0)"
} else if (h==3) {
################################################## Projecting cooperative approach ###################################################
## sample size for balance H0 -  basically table(dta$recipient[dta$messenger == "couple"])
dta <- subset(dta, messenger != "ctrl")

#s_h0 <- min(table(dta$messenger, dta$recipient)[1,])

### sample size for balance H1
#s_h1 <- min(table(dta$messenger, dta$recipient)[-1,])

#dta_bal <- rbind( dta[dta$messenger=="couple" & dta$recipient == "couple",][sample( nrow(dta[dta$messenger=="couple" & dta$recipient == "couple",]),s_h0),],
# dta[dta$messenger=="male" & dta$recipient == "couple",][sample( nrow(dta[dta$messenger=="male" & dta$recipient == "couple",]),s_h1),],
# dta[dta$messenger=="female" & dta$recipient == "couple",][sample( nrow(dta[dta$messenger=="female" & dta$recipient == "couple",]),s_h1),],

# dta[dta$messenger=="female" & dta$recipient == "male",][sample( nrow(dta[dta$messenger=="female" & dta$recipient == "male",]),s_h1),],
# dta[dta$messenger=="female" & dta$recipient == "female",][sample( nrow(dta[dta$messenger=="female" & dta$recipient == "female",]),s_h1),],
# dta[dta$messenger=="male" & dta$recipient == "male",][sample( nrow(dta[dta$messenger=="male" & dta$recipient == "male",]),s_h1),],
# dta[dta$messenger=="male" & dta$recipient == "female",][sample( nrow(dta[dta$messenger=="male" & dta$recipient == "female",]),s_h1),],
# dta[dta$messenger=="couple" & dta$recipient == "male",][sample( nrow(dta[dta$messenger=="couple" & dta$recipient == "male",]),s_h0),],
# dta[dta$messenger=="couple" & dta$recipient == "female",][sample( nrow(dta[dta$messenger=="couple" & dta$recipient == "female",]),s_h0),])
treatment <- "(messenger == 'couple')+ivr+sms+as.factor(recipient) + called + (totsms >0)" 

} else if (h==4) {
############################################################## gender matching ###############################################################
dta <- subset(dta, messenger != "ctrl")
dta$recipient <- as.character(dta$recipient)

dta <- subset(dta, recipient != "couple" & messenger != "couple")
#s_h0 <- min(table(dta$recipient, dta$messenger))

#dta_bal <- rbind(
# dta[dta$messenger=="female" & dta$recipient == "male",][sample( nrow(dta[dta$messenger=="female" & dta$recipient == "male",]),s_h0),],
# dta[dta$messenger=="female" & dta$recipient == "female",][sample( nrow(dta[dta$messenger=="female" & dta$recipient == "female",]),s_h0),],
# dta[dta$messenger=="male" & dta$recipient == "male",][sample( nrow(dta[dta$messenger=="male" & dta$recipient == "male",]),s_h0),],
# dta[dta$messenger=="male" & dta$recipient == "female",][sample( nrow(dta[dta$messenger=="male" & dta$recipient == "female",]),s_h0),])

treatment <- "(messenger == recipient) +ivr+sms + called + (totsms >0)"
}

############################### knowledge  ############################
dta_bal <- dta


############################### practices #############################
### used recommended spacing use on at lease one plot as reported by at least one spouse

results <-  plot_RI(dta, man = "decspace_man", out_sp1 = c("grp1a201","grp2b201","grp3c201","grp4d201","grp5e201"),out_sp2 = c("spouse2grp_sp1f201","spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201","spouse2grp5_sp5k201"),treatment , repl)

res_pract_mm[1,1,h]  <- results[[1]]
res_pract_mm[1,2,h]  <- results[[2]]
res_pract_mm[1,3,h]  <- results[[3]]
res_pract_mm[1,4,h]  <- results[[4]]


results <- plot_RI(dta, man = "decspace_woman", out_sp1 = c("grp1a201","grp2b201","grp3c201","grp4d201","grp5e201"),out_sp2 = c("spouse2grp_sp1f201","spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201","spouse2grp5_sp5k201"),treatment , repl)

res_pract_wm[1,1,h]  <- results[[1]]
res_pract_wm[1,2,h]  <- results[[2]]
res_pract_wm[1,3,h]  <- results[[3]]
res_pract_wm[1,4,h]  <- results[[4]]

results <-  plot_RI(dta, man = "decspace_both", out_sp1 = c("grp1a201","grp2b201","grp3c201","grp4d201","grp5e201"),out_sp2 = c("spouse2grp_sp1f201","spouse2grp_sp2g201","spouse2grp_sp3h201","spouse2group_sp4j201","spouse2grp5_sp5k201"),treatment , repl)

res_pract_bm[1,1,h]  <- results[[1]]
res_pract_bm[1,2,h]  <- results[[2]]
res_pract_bm[1,3,h]  <- results[[3]]
res_pract_bm[1,4,h]  <- results[[4]]


## used recommended way to fight striga - this should be changed to include info of all plots 

results <- plot_RI(dta, man = "decstriga_man", out_sp1 = c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241"),out_sp2 = c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241"),treatment , repl)


res_pract_mm[2,1,h]  <- results[[1]]
res_pract_mm[2,2,h]  <- results[[2]]
res_pract_mm[2,3,h]  <- results[[3]]
res_pract_mm[2,4,h]  <- results[[4]]

results <- plot_RI(dta, man = "decstriga_woman", out_sp1 = c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241"),out_sp2 = c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241"),treatment , repl)


res_pract_wm[2,1,h]  <- results[[1]]
res_pract_wm[2,2,h]  <- results[[2]]
res_pract_wm[2,3,h]  <- results[[3]]
res_pract_wm[2,4,h]  <- results[[4]]

results <- plot_RI(dta, man = "decstriga_both", out_sp1 = c("grp1a241","grp2b241","grp3c241","grp4d241", "grp5e241"),out_sp2 = c("spouse2grp_sp1f241","spouse2grp_sp2g241","spouse2grp_sp3h241","spouse2group_sp4j241", "spouse2grp5_sp5k241"),treatment , repl)

res_pract_bm[2,1,h]  <- results[[1]]
res_pract_bm[2,2,h]  <- results[[2]]
res_pract_bm[2,3,h]  <- results[[3]]
res_pract_bm[2,4,h]  <- results[[4]]

## weeded on recommended timing? - this should be changed to include info of all plots 

results <- plot_RI(dta, man = "decweed_man", out_sp1 = c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26"),out_sp2 = c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26"),treatment , repl)


res_pract_mm[3,1,h]  <- results[[1]]
res_pract_mm[3,2,h]  <- results[[2]]
res_pract_mm[3,3,h]  <- results[[3]]
res_pract_mm[3,4,h]  <- results[[4]]

results <- plot_RI(dta, man = "decweed_woman", out_sp1 = c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26"),out_sp2 = c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26"),treatment , repl)


res_pract_wm[3,1,h]  <- results[[1]]
res_pract_wm[3,2,h]  <- results[[2]]
res_pract_wm[3,3,h]  <- results[[3]]
res_pract_wm[3,4,h]  <- results[[4]]


results <- plot_RI(dta, man = "decweed_both", out_sp1 = c("grp1a26","grp2b26", "grp3c26", "grp4d26", "grp5e26"),out_sp2 = c("spouse2grp_sp1f26","spouse2grp_sp2g26","spouse2grp_sp3h26","spouse2group_sp4j26", "spouse2grp5_sp5k26"),treatment , repl)

res_pract_bm[3,1,h]  <- results[[1]]
res_pract_bm[3,2,h]  <- results[[2]]
res_pract_bm[3,3,h]  <- results[[3]]
res_pract_bm[3,4,h]  <- results[[4]]

### fertilizer use
### any fertlizer used on a plot?
### but how to define who decided/managed?
results <- plot_RI(dta, man = "dec_man", out_sp1 = c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29") ,out_sp2 = c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29"),treatment , repl)
res_pract_mm[4,1,h]  <- results[[1]]
res_pract_mm[4,2,h]  <- results[[2]]
res_pract_mm[4,3,h]  <- results[[3]]
res_pract_mm[4,4,h]  <- results[[4]]

results <- plot_RI(dta, man = "dec_woman", out_sp1 = c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29"),out_sp2 = c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29"),treatment , repl)


res_pract_wm[4,1,h]  <- results[[1]]
res_pract_wm[4,2,h]  <- results[[2]]
res_pract_wm[4,3,h]  <- results[[3]]
res_pract_wm[4,4,h]  <- results[[4]]


results <- plot_RI(dta, man = "dec_both", out_sp1 = c("grp1a29","grp2b29","grp3c29","grp4d29","grp5e29"),out_sp2 = c("spouse2grp_sp1f29","spouse2grp_sp2g29","spouse2grp_sp3h29","spouse2group_sp4j29","spouse2grp5_sp5k29"),treatment , repl)

res_pract_bm[4,1,h]  <- results[[1]]
res_pract_bm[4,2,h]  <- results[[2]]
res_pract_bm[4,3,h]  <- results[[3]]
res_pract_bm[4,4,h]  <- results[[4]]

###improved seed  

results <- plot_RI(dta, man = "dec_man", out_sp1 = c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42") ,out_sp2 =c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42"),treatment , repl)
res_pract_mm[5,1,h]  <- results[[1]]
res_pract_mm[5,2,h]  <- results[[2]]
res_pract_mm[5,3,h]  <- results[[3]]
res_pract_mm[5,4,h]  <- results[[4]]

results <- plot_RI(dta, man = "dec_woman", out_sp1 = c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42"),out_sp2 = c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42"),treatment , repl)


res_pract_wm[5,1,h]  <- results[[1]]
res_pract_wm[5,2,h]  <- results[[2]]
res_pract_wm[5,3,h]  <- results[[3]]
res_pract_wm[5,4,h]  <- results[[4]]


results <- plot_RI(dta, man = "dec_both", out_sp1 = c("grp1a42","grp2b42","grp3c42","grp4d42","grp5e42"),out_sp2 = c("spouse2grp_sp1f42","spouse2grp_sp2g42","spouse2grp_sp3h42","spouse2group_sp4j42","spouse2grp5_sp5k42"),treatment , repl)

res_pract_bm[5,1,h]  <- results[[1]]
res_pract_bm[5,2,h]  <- results[[2]]
res_pract_bm[5,3,h]  <- results[[3]]
res_pract_bm[5,4,h]  <- results[[4]]

###combiner
results <- plot_RI(dta, man = "dec_man", out_sp1 = c("combiner_sp1_pl1","combiner_sp1_pl2","combiner_sp1_pl3","combiner_sp1_pl4","combiner_sp1_pl5") ,out_sp2 =c("combiner_sp2_pl1","combiner_sp2_pl2","combiner_sp2_pl3","combiner_sp2_pl4","combiner_sp2_pl5"),treatment , repl)
res_pract_mm[6,1,h]  <- results[[1]]
res_pract_mm[6,2,h]  <- results[[2]]
res_pract_mm[6,3,h]  <- results[[3]]
res_pract_mm[6,4,h]  <- results[[4]]

results <- plot_RI(dta, man = "dec_woman", out_sp1 =  c("combiner_sp1_pl1","combiner_sp1_pl2","combiner_sp1_pl3","combiner_sp1_pl4","combiner_sp1_pl5"),out_sp2 = c("combiner_sp2_pl1","combiner_sp2_pl2","combiner_sp2_pl3","combiner_sp2_pl4","combiner_sp2_pl5"),treatment , repl)


res_pract_wm[6,1,h]  <- results[[1]]
res_pract_wm[6,2,h]  <- results[[2]]
res_pract_wm[6,3,h]  <- results[[3]]
res_pract_wm[6,4,h]  <- results[[4]]


results <- plot_RI(dta, man = "dec_both", out_sp1 = c("combiner_sp1_pl1","combiner_sp1_pl2","combiner_sp1_pl3","combiner_sp1_pl4","combiner_sp1_pl5"),out_sp2 = c("combiner_sp2_pl1","combiner_sp2_pl2","combiner_sp2_pl3","combiner_sp2_pl4","combiner_sp2_pl5"),treatment , repl)

res_pract_bm[6,1,h]  <- results[[1]]
res_pract_bm[6,2,h]  <- results[[2]]
res_pract_bm[6,3,h]  <- results[[3]]
res_pract_bm[6,4,h]  <- results[[4]]



#### bought seed

results <- plot_RI(dta, man = "dec_man", out_sp1 =c("grp1seed_purchase1","grp2seed_purchase2","grp3seed_purchase3","grp4seed_purchase4","grp5seed_purchase5"),out_sp2 =c("spouse2grp_sp1seed_purchasesp1", "spouse2grp_sp2seed_purchase_sp2","spouse2grp_sp3seed_purchasesp3","spouse2group_sp4seed_purchasesp4","spouse2grp5_sp5seed_purchasesp5"),treatment , repl)
res_pract_mm[7,1,h]  <- results[[1]]
res_pract_mm[7,2,h]  <- results[[2]]
res_pract_mm[7,3,h]  <- results[[3]]
res_pract_mm[7,4,h]  <- results[[4]]

results <- plot_RI(dta, man = "dec_woman", out_sp1 =c("grp1seed_purchase1","grp2seed_purchase2","grp3seed_purchase3","grp4seed_purchase4","grp5seed_purchase5"),out_sp2 =c("spouse2grp_sp1seed_purchasesp1", "spouse2grp_sp2seed_purchase_sp2","spouse2grp_sp3seed_purchasesp3","spouse2group_sp4seed_purchasesp4","spouse2grp5_sp5seed_purchasesp5"),treatment , repl)


res_pract_wm[7,1,h]  <- results[[1]]
res_pract_wm[7,2,h]  <- results[[2]]
res_pract_wm[7,3,h]  <- results[[3]]
res_pract_wm[7,4,h]  <- results[[4]]


results <- plot_RI(dta, man = "dec_both", out_sp1 =c("grp1seed_purchase1","grp2seed_purchase2","grp3seed_purchase3","grp4seed_purchase4","grp5seed_purchase5"),out_sp2 =c("spouse2grp_sp1seed_purchasesp1", "spouse2grp_sp2seed_purchase_sp2","spouse2grp_sp3seed_purchasesp3","spouse2group_sp4seed_purchasesp4","spouse2grp5_sp5seed_purchasesp5"),treatment , repl)

res_pract_bm[7,1,h]  <- results[[1]]
res_pract_bm[7,2,h]  <- results[[2]]
res_pract_bm[7,3,h]  <- results[[3]]
res_pract_bm[7,4,h]  <- results[[4]]

##### used chemicals

results <- plot_RI(dta, man = "dec_man", out_sp1 =c("grp1a55a" ,"grp2b55b", "grp3c55b","grp4d55b","grp5e55b"),out_sp2 =c("spouse2grp_sp1f55a","spouse2grp_sp2g55b","spouse2grp_sp3h55b","spouse2group_sp4j55b","spouse2grp5_sp5k55b"),treatment , repl)
res_pract_mm[8,1,h]  <- results[[1]]
res_pract_mm[8,2,h]  <- results[[2]]
res_pract_mm[8,3,h]  <- results[[3]]
res_pract_mm[8,4,h]  <- results[[4]]

results <- plot_RI(dta, man = "dec_woman", out_sp1 =c("grp1a55a" ,"grp2b55b", "grp3c55b","grp4d55b","grp5e55b"),out_sp2 =c("spouse2grp_sp1f55a","spouse2grp_sp2g55b","spouse2grp_sp3h55b","spouse2group_sp4j55b","spouse2grp5_sp5k55b"),treatment , repl)


res_pract_wm[8,1,h]  <- results[[1]]
res_pract_wm[8,2,h]  <- results[[2]]
res_pract_wm[8,3,h]  <- results[[3]]
res_pract_wm[8,4,h]  <- results[[4]]


results <- plot_RI(dta, man = "dec_both", out_sp1 =c("grp1a55a" ,"grp2b55b", "grp3c55b","grp4d55b","grp5e55b"),out_sp2 =c("spouse2grp_sp1f55a","spouse2grp_sp2g55b","spouse2grp_sp3h55b","spouse2group_sp4j55b","spouse2grp5_sp5k55b"),treatment , repl)

res_pract_bm[8,1,h]  <- results[[1]]
res_pract_bm[8,2,h]  <- results[[2]]
res_pract_bm[8,3,h]  <- results[[3]]
res_pract_bm[8,4,h]  <- results[[4]]


####hired labour

results <- plot_RI(dta, man = "dec_man", out_sp1 =c("grp1a151","grp2b55b","grp3c55b","grp4d55b","grp5e55b"),out_sp2 =c("spouse2grp_sp1f151","spouse2grp_sp2g151","spouse2grp_sp3h151","spouse2group_sp4j151","spouse2grp5_sp5k151"),treatment , repl)
res_pract_mm[9,1,h]  <- results[[1]]
res_pract_mm[9,2,h]  <- results[[2]]
res_pract_mm[9,3,h]  <- results[[3]]
res_pract_mm[9,4,h]  <- results[[4]]

results <- plot_RI(dta, man = "dec_woman", out_sp1 =c("grp1a151","grp2b55b","grp3c55b","grp4d55b","grp5e55b"),out_sp2 =c("spouse2grp_sp1f151","spouse2grp_sp2g151","spouse2grp_sp3h151","spouse2group_sp4j151","spouse2grp5_sp5k151"),treatment , repl)


res_pract_wm[9,1,h]  <- results[[1]]
res_pract_wm[9,2,h]  <- results[[2]]
res_pract_wm[9,3,h]  <- results[[3]]
res_pract_wm[9,4,h]  <- results[[4]]


results <- plot_RI(dta, man = "dec_both", out_sp1 =c("grp1a151","grp2b55b","grp3c55b","grp4d55b","grp5e55b"),out_sp2 =c("spouse2grp_sp1f151","spouse2grp_sp2g151","spouse2grp_sp3h151","spouse2group_sp4j151","spouse2grp5_sp5k151"),treatment , repl)

res_pract_bm[9,1,h]  <- results[[1]]
res_pract_bm[9,2,h]  <- results[[2]]
res_pract_bm[9,3,h]  <- results[[3]]
res_pract_bm[9,4,h]  <- results[[4]]


####plant immediately after rain

results <- plot_RI(dta, man = "dectime_man", out_sp1 =c("grp1days1","grp2days2","grp3days3","grp4days4", "grp5days5"),out_sp2 =c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5"),treatment , repl)
res_pract_mm[10,1,h]  <- results[[1]]
res_pract_mm[10,2,h]  <- results[[2]]
res_pract_mm[10,3,h]  <- results[[3]]
res_pract_mm[10,4,h]  <- results[[4]]

results <- plot_RI(dta, man = "dectime_woman", out_sp1 =c("grp1days1","grp2days2","grp3days3","grp4days4", "grp5days5"),out_sp2 =c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5"),treatment , repl)


res_pract_wm[10,1,h]  <- results[[1]]
res_pract_wm[10,2,h]  <- results[[2]]
res_pract_wm[10,3,h]  <- results[[3]]
res_pract_wm[10,4,h]  <- results[[4]]


results <- plot_RI(dta, man = "dectime_both", out_sp1 =c("grp1days1","grp2days2","grp3days3","grp4days4", "grp5days5"),out_sp2 =c("spouse2grp_sp1days1","spouse2grp_sp2days_sp2","spouse2grp_sp3days_sp3","spouse2group_sp4dayssp4", "spouse2grp5_sp5dayssp5"),treatment , repl)

res_pract_bm[10,1,h]  <- results[[1]]
res_pract_bm[10,2,h]  <- results[[2]]
res_pract_bm[10,3,h]  <- results[[3]]
res_pract_bm[10,4,h]  <- results[[4]]


################################ production ###########################
#### does the video increases production related outcomes?

##trimming is done on end result
#dta_bal2 <- subset(dta_bal, prod_tot_fm>0)
#dta_bal2$log_prod_tot_fm <- log(dta_bal2$prod_tot_fm)
#dta_trim <- trim("log_prod_tot_fm", dta_bal2, .05)

#### production
#res_prod_fm[1,1,h] <- summary(lm(as.formula(paste("log_prod_tot_fm",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
#res_prod_fm[1,2,h] <- summary(lm(as.formula(paste("log_prod_tot_fm",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
#res_prod_fm[1,3,h] <- ifelse(totrep >0, RI("log_prod_tot_fm",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_prod_tot_fm",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

#### area

#dta_bal2 <- subset(dta_bal, area_tot_fm>0)
#dta_bal2$log_area_tot_fm <- log(dta_bal2$area_tot_fm)

#dta_trim <- trim("log_area_tot_fm", dta_bal2, .05)

#res_prod_fm[2,1,h] <- summary(lm(as.formula(paste("log_area_tot_fm",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
#res_prod_fm[2,2,h] <- summary(lm(as.formula(paste("log_area_tot_fm",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
#res_prod_fm[2,3,h] <- ifelse(totrep >0, RI("log_area_tot_fm",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_area_tot_fm",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

####yield

#dta_bal2 <- subset(dta_bal, yield_av_fm >0)
#dta_bal2$log_yield_av_fm <- log(dta_bal2$yield_av_fm)
#dta_trim <- trim("log_yield_av_fm", dta_bal2, .05)

#res_prod_fm[3,1,h] <- summary(lm(as.formula(paste("log_yield_av_fm",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
#res_prod_fm[3,2,h] <- summary(lm(as.formula(paste("log_yield_av_fm",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
#res_prod_fm[3,3,h] <- ifelse(totrep >0, RI("log_yield_av_fm",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_yield_av_fm",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

#### was yield better compared to normal year?
#summary(lm(yield_better_fm~messenger != "ctrl", data=dta_bal))

#res_prod_fm[4,1,h] <- summary(lm(as.formula(paste("yield_better_fm",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
#res_prod_fm[4,2,h] <- summary(lm(as.formula(paste("yield_better_fm",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
#res_prod_fm[4,3,h] <- ifelse(totrep >0, RI("yield_better_fm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("yield_better_fm",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

#dta_bal2 <- subset(dta_bal, area_tot_fm >0 & prod_tot_fm>0 & yield_av_fm >0)
#dta_bal2$log_prod_tot_fm <- log(dta_bal2$prod_tot_fm)
#dta_bal2$log_area_tot_fm <- log(dta_bal2$area_tot_fm)
#dta_bal2$log_yield_av_fm <- log(dta_bal2$yield_av_fm)

##dta_bal2 <- trim("log_prod_tot", dta_bal2, .1)
##dta_bal2 <- trim("log_area_tot", dta_bal2, .1)
#dta_bal2 <- trim("log_yield_av_fm", dta_bal2, .05)


##res_prod_fm[1:4,4,h] <- FSR_OLS( c("log_prod_tot", "log_area_tot", "log_yield_av","yield_better") ,treatment,dta_bal, nr_repl = totrep)[[4]]

#dta_bal2$log_area_tot_fm <- -dta_bal2$log_area_tot_fm

#	indexer <- FW_index(treatment, c("log_prod_tot_fm", "log_area_tot_fm","yield_better_fm"),dta_bal2, nr_repl=totrep)
#	res_prod_fm[5,1,h] <-  indexer[[1]]$coefficients[1,1]
#	res_prod_fm[5,2,h] <-  indexer[[1]]$coefficients[2,1]
#	res_prod_fm[5,3,h] <-  indexer[[2]]


########################### wi#################################
##trimming is done on end result
#dta_bal2 <- subset(dta_bal, prod_tot_fi>0)
#dta_bal2$log_prod_tot_fi <- log(dta_bal2$prod_tot_fi)
#dta_trim <- trim("log_prod_tot_fi", dta_bal2, .05)

#### production
#res_prod_fi[1,1,h] <- summary(lm(as.formula(paste("log_prod_tot_fi",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
#res_prod_fi[1,2,h] <- summary(lm(as.formula(paste("log_prod_tot_fi",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
#res_prod_fi[1,3,h] <- ifelse(totrep >0, RI("log_prod_tot_fi",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_prod_tot_fi",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

#### area

#dta_bal2 <- subset(dta_bal, area_tot_fi>0)
#dta_bal2$log_area_tot_fi <- log(dta_bal2$area_tot_fi)

#dta_trim <- trim("log_area_tot_fi", dta_bal2, .05)

#res_prod_fi[2,1,h] <- summary(lm(as.formula(paste("log_area_tot_fi",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
#res_prod_fi[2,2,h] <- summary(lm(as.formula(paste("log_area_tot_fi",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
#res_prod_fi[2,3,h] <- ifelse(totrep >0, RI("log_area_tot_fi",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_area_tot_fi",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

####yield

#dta_bal2 <- subset(dta_bal, yield_av_fi >0)
#dta_bal2$log_yield_av_fi <- log(dta_bal2$yield_av_fi)
#dta_trim <- trim("log_yield_av_fi", dta_bal2, .05)

#res_prod_fi[3,1,h] <- summary(lm(as.formula(paste("log_yield_av_fi",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
#res_prod_fi[3,2,h] <- summary(lm(as.formula(paste("log_yield_av_fi",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
#res_prod_fi[3,3,h] <- ifelse(totrep >0, RI("log_yield_av_fi",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_yield_av_fi",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

#### was yield better compared to normal year?
#summary(lm(yield_better_fi~messenger != "ctrl", data=dta_bal))

#res_prod_fi[4,1,h] <- summary(lm(as.formula(paste("yield_better_fi",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
#res_prod_fi[4,2,h] <- summary(lm(as.formula(paste("yield_better_fi",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
#res_prod_fi[4,3,h] <- ifelse(totrep >0, RI("yield_better_fi",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("yield_better_fi",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

#dta_bal2 <- subset(dta_bal, area_tot_fi >0 & prod_tot_fi>0 & yield_av_fi >0)
#dta_bal2$log_prod_tot_fi <- log(dta_bal2$prod_tot_fi)
#dta_bal2$log_area_tot_fi <- log(dta_bal2$area_tot_fi)
#dta_bal2$log_yield_av_fi <- log(dta_bal2$yield_av_fi)

##dta_bal2 <- trim("log_prod_tot", dta_bal2, .1)
##dta_bal2 <- trim("log_area_tot", dta_bal2, .1)
#dta_bal2 <- trim("log_yield_av_fi", dta_bal2, .05)


##res_prod_fi[1:4,4,h] <- FSR_OLS( c("log_prod_tot", "log_area_tot", "log_yield_av","yield_better") ,treatment,dta_bal, nr_repl = totrep)[[4]]

#dta_bal2$log_area_tot_fi <- -dta_bal2$log_area_tot_fi

#	indexer <- FW_index(treatment, c("log_prod_tot_fi", "log_area_tot_fi","yield_better_fi"),dta_bal2, nr_repl=totrep)
#	res_prod_fi[5,1,h] <-  indexer[[1]]$coefficients[1,1]
#	res_prod_fi[5,2,h] <-  indexer[[1]]$coefficients[2,1]
#	res_prod_fi[5,3,h] <-  indexer[[2]]


######################### mm#################################
##trimming is done on end result
#dta_bal2 <- subset(dta_bal, prod_tot_mm>0)
#dta_bal2$log_prod_tot_mm <- log(dta_bal2$prod_tot_mm)
#dta_trim <- trim("log_prod_tot_mm", dta_bal2, .05)

#### production
#res_prod_mm[1,1,h] <- summary(lm(as.formula(paste("log_prod_tot_mm",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
#res_prod_mm[1,2,h] <- summary(lm(as.formula(paste("log_prod_tot_mm",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
#res_prod_mm[1,3,h] <- ifelse(totrep >0, RI("log_prod_tot_mm",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_prod_tot_mm",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

#### area

#dta_bal2 <- subset(dta_bal, area_tot_mm>0)
#dta_bal2$log_area_tot_mm <- log(dta_bal2$area_tot_mm)

#dta_trim <- trim("log_area_tot_mm", dta_bal2, .05)

#res_prod_mm[2,1,h] <- summary(lm(as.formula(paste("log_area_tot_mm",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
#res_prod_mm[2,2,h] <- summary(lm(as.formula(paste("log_area_tot_mm",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
#res_prod_mm[2,3,h] <- ifelse(totrep >0, RI("log_area_tot_mm",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_area_tot_mm",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

####yield

#dta_bal2 <- subset(dta_bal, yield_av_mm >0)
#dta_bal2$log_yield_av_mm <- log(dta_bal2$yield_av_mm)
#dta_trim <- trim("log_yield_av_mm", dta_bal2, .05)

#res_prod_mm[3,1,h] <- summary(lm(as.formula(paste("log_yield_av_mm",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
#res_prod_mm[3,2,h] <- summary(lm(as.formula(paste("log_yield_av_mm",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
#res_prod_mm[3,3,h] <- ifelse(totrep >0, RI("log_yield_av_mm",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_yield_av_mm",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

#### was yield better compared to normal year?
#summary(lm(yield_better_mm~messenger != "ctrl", data=dta_bal))

#res_prod_mm[4,1,h] <- summary(lm(as.formula(paste("yield_better_mm",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
#res_prod_mm[4,2,h] <- summary(lm(as.formula(paste("yield_better_mm",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
#res_prod_mm[4,3,h] <- ifelse(totrep >0, RI("yield_better_mm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("yield_better_mm",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

#dta_bal2 <- subset(dta_bal, area_tot_mm >0 & prod_tot_mm>0 & yield_av_mm >0)
#dta_bal2$log_prod_tot_mm <- log(dta_bal2$prod_tot_mm)
#dta_bal2$log_area_tot_mm <- log(dta_bal2$area_tot_mm)
#dta_bal2$log_yield_av_mm <- log(dta_bal2$yield_av_mm)

##dta_bal2 <- trim("log_prod_tot", dta_bal2, .1)
##dta_bal2 <- trim("log_area_tot", dta_bal2, .1)
#dta_bal2 <- trim("log_yield_av_mm", dta_bal2, .05)


##res_prod_mm[1:4,4,h] <- FSR_OLS( c("log_prod_tot", "log_area_tot", "log_yield_av","yield_better") ,treatment,dta_bal, nr_repl = totrep)[[4]]

#dta_bal2$log_area_tot_mm <- -dta_bal2$log_area_tot_mm

#	indexer <- FW_index(treatment, c("log_prod_tot_mm", "log_area_tot_mm", "yield_better_mm"),dta_bal2, nr_repl=totrep)
#	res_prod_mm[5,1,h] <-  indexer[[1]]$coefficients[1,1]
#	res_prod_mm[5,2,h] <-  indexer[[1]]$coefficients[2,1]
#	res_prod_mm[5,3,h] <-  indexer[[2]]


###################### bm#################################
##trimming is done on end result
#dta_bal2 <- subset(dta_bal, prod_tot_bm>0)
#dta_bal2$log_prod_tot_bm <- log(dta_bal2$prod_tot_bm)
#dta_trim <- trim("log_prod_tot_bm", dta_bal2, .05)

#### production
#res_prod_bm[1,1,h] <- summary(lm(as.formula(paste("log_prod_tot_bm",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
#res_prod_bm[1,2,h] <- summary(lm(as.formula(paste("log_prod_tot_bm",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
#res_prod_bm[1,3,h] <- ifelse(totrep >0, RI("log_prod_tot_bm",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_prod_tot_bm",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

#### area

#dta_bal2 <- subset(dta_bal, area_tot_bm>0)
#dta_bal2$log_area_tot_bm <- log(dta_bal2$area_tot_bm)

#dta_trim <- trim("log_area_tot_bm", dta_bal2, .05)

#res_prod_bm[2,1,h] <- summary(lm(as.formula(paste("log_area_tot_bm",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
#res_prod_bm[2,2,h] <- summary(lm(as.formula(paste("log_area_tot_bm",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
#res_prod_bm[2,3,h] <- ifelse(totrep >0, RI("log_area_tot_bm",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_area_tot_bm",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

####yield

#dta_bal2 <- subset(dta_bal, yield_av_bm >0)
#dta_bal2$log_yield_av_bm <- log(dta_bal2$yield_av_bm)
#dta_trim <- trim("log_yield_av_bm", dta_bal2, .05)

#res_prod_bm[3,1,h] <- summary(lm(as.formula(paste("log_yield_av_bm",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
#res_prod_bm[3,2,h] <- summary(lm(as.formula(paste("log_yield_av_bm",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
#res_prod_bm[3,3,h] <- ifelse(totrep >0, RI("log_yield_av_bm",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_yield_av_bm",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

#### was yield better compared to normal year?
#summary(lm(yield_better_bm~messenger != "ctrl", data=dta_bal))

#res_prod_bm[4,1,h] <- summary(lm(as.formula(paste("yield_better_bm",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
#res_prod_bm[4,2,h] <- summary(lm(as.formula(paste("yield_better_bm",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
#res_prod_bm[4,3,h] <- ifelse(totrep >0, RI("yield_better_bm",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("yield_better_bm",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

#dta_bal2 <- subset(dta_bal, area_tot_bm >0 & prod_tot_bm>0 & yield_av_bm >0)
#dta_bal2$log_prod_tot_bm <- log(dta_bal2$prod_tot_bm)
#dta_bal2$log_area_tot_bm <- log(dta_bal2$area_tot_bm)
#dta_bal2$log_yield_av_bm <- log(dta_bal2$yield_av_bm)

##dta_bal2 <- trim("log_prod_tot", dta_bal2, .1)
##dta_bal2 <- trim("log_area_tot", dta_bal2, .1)
#dta_bal2 <- trim("log_yield_av_bm", dta_bal2, .05)


##res_prod_bm[1:4,4,h] <- FSR_OLS( c("log_prod_tot", "log_area_tot", "log_yield_av","yield_better") ,treatment,dta_bal, nr_repl = totrep)[[4]]

#dta_bal2$log_area_tot_bm <- -dta_bal2$log_area_tot_bm

#	indexer <- FW_index(treatment, c("log_prod_tot_bm", "log_area_tot_bm","yield_better_bm"),dta_bal2, nr_repl=totrep)
#	res_prod_bm[5,1,h] <-  indexer[[1]]$coefficients[1,1]
#	res_prod_bm[5,2,h] <-  indexer[[1]]$coefficients[2,1]
#	res_prod_bm[5,3,h] <-  indexer[[2]]


########################
################### wmshare1#################################
##trimming is done on end result
#dta_bal2 <- subset(dta_bal, prod_tot_wm_share1>0)
#dta_bal2$log_prod_tot_wm_share1 <- log(dta_bal2$prod_tot_wm_share1)
#dta_trim <- trim("log_prod_tot_wm_share1", dta_bal2, .05)

#### production
#res_prod_wm_share1[1,1,h] <- summary(lm(as.formula(paste("log_prod_tot_wm_share1",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
#res_prod_wm_share1[1,2,h] <- summary(lm(as.formula(paste("log_prod_tot_wm_share1",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
#res_prod_wm_share1[1,3,h] <- ifelse(totrep >0, RI("log_prod_tot_wm_share1",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_prod_tot_wm_share1",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

#### area

#dta_bal2 <- subset(dta_bal, area_tot_wm_share1>0)
#dta_bal2$log_area_tot_wm_share1 <- log(dta_bal2$area_tot_wm_share1)

#dta_trim <- trim("log_area_tot_wm_share1", dta_bal2, .05)

#res_prod_wm_share1[2,1,h] <- summary(lm(as.formula(paste("log_area_tot_wm_share1",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
#res_prod_wm_share1[2,2,h] <- summary(lm(as.formula(paste("log_area_tot_wm_share1",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
#res_prod_wm_share1[2,3,h] <- ifelse(totrep >0, RI("log_area_tot_wm_share1",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_area_tot_wm_share1",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

####yield

#dta_bal2 <- subset(dta_bal, yield_av_wm_share1 >0)
#dta_bal2$log_yield_av_wm_share1 <- log(dta_bal2$yield_av_wm_share1)
#dta_trim <- trim("log_yield_av_wm_share1", dta_bal2, .05)

#res_prod_wm_share1[3,1,h] <- summary(lm(as.formula(paste("log_yield_av_wm_share1",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
#res_prod_wm_share1[3,2,h] <- summary(lm(as.formula(paste("log_yield_av_wm_share1",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
#res_prod_wm_share1[3,3,h] <- ifelse(totrep >0, RI("log_yield_av_wm_share1",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_yield_av_wm_share1",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

#### was yield better compared to normal year?
#summary(lm(yield_better_wm_share1~messenger != "ctrl", data=dta_bal))

#res_prod_wm_share1[4,1,h] <- summary(lm(as.formula(paste("yield_better_wm_share1",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
#res_prod_wm_share1[4,2,h] <- summary(lm(as.formula(paste("yield_better_wm_share1",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
#res_prod_wm_share1[4,3,h] <- ifelse(totrep >0, RI("yield_better_wm_share1",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("yield_better_wm_share1",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

#dta_bal2 <- subset(dta_bal, area_tot_wm_share1 >0 & prod_tot_wm_share1>0 & yield_av_wm_share1 >0)
#dta_bal2$log_prod_tot_wm_share1 <- log(dta_bal2$prod_tot_wm_share1)
#dta_bal2$log_area_tot_wm_share1 <- log(dta_bal2$area_tot_wm_share1)
#dta_bal2$log_yield_av_wm_share1 <- log(dta_bal2$yield_av_wm_share1)

##dta_bal2 <- trim("log_prod_tot", dta_bal2, .1)
##dta_bal2 <- trim("log_area_tot", dta_bal2, .1)
#dta_bal2 <- trim("log_yield_av_wm_share1", dta_bal2, .05)


##res_prod_wm_share1[1:4,4,h] <- FSR_OLS( c("log_prod_tot", "log_area_tot", "log_yield_av","yield_better") ,treatment,dta_bal, nr_repl = totrep)[[4]]

#dta_bal2$log_area_tot_wm_share1 <- -dta_bal2$log_area_tot_wm_share1

#	indexer <- FW_index(treatment, c("log_prod_tot_wm_share1", "log_area_tot_wm_share1","yield_better_wm_share1"),dta_bal2, nr_repl=totrep)
#	res_prod_wm_share1[5,1,h] <-  indexer[[1]]$coefficients[1,1]
#	res_prod_wm_share1[5,2,h] <-  indexer[[1]]$coefficients[2,1]
#	res_prod_wm_share1[5,3,h] <-  indexer[[2]]


################ wmshare2#################################
##trimming is done on end result
#dta_bal2 <- subset(dta_bal, prod_tot_wm_share2>0)
#dta_bal2$log_prod_tot_wm_share2 <- log(dta_bal2$prod_tot_wm_share2)
#dta_trim <- trim("log_prod_tot_wm_share2", dta_bal2, .05)

#### production
#res_prod_wm_share2[1,1,h] <- summary(lm(as.formula(paste("log_prod_tot_wm_share2",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
#res_prod_wm_share2[1,2,h] <- summary(lm(as.formula(paste("log_prod_tot_wm_share2",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
#res_prod_wm_share2[1,3,h] <- ifelse(totrep >0, RI("log_prod_tot_wm_share2",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_prod_tot_wm_share2",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

#### area

#dta_bal2 <- subset(dta_bal, area_tot_wm_share2>0)
#dta_bal2$log_area_tot_wm_share2 <- log(dta_bal2$area_tot_wm_share2)

#dta_trim <- trim("log_area_tot_wm_share2", dta_bal2, .05)

#res_prod_wm_share2[2,1,h] <- summary(lm(as.formula(paste("log_area_tot_wm_share2",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
#res_prod_wm_share2[2,2,h] <- summary(lm(as.formula(paste("log_area_tot_wm_share2",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
#res_prod_wm_share2[2,3,h] <- ifelse(totrep >0, RI("log_area_tot_wm_share2",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_area_tot_wm_share2",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

####yield

#dta_bal2 <- subset(dta_bal, yield_av_wm_share2 >0)
#dta_bal2$log_yield_av_wm_share2 <- log(dta_bal2$yield_av_wm_share2)
#dta_trim <- trim("log_yield_av_wm_share2", dta_bal2, .05)

#res_prod_wm_share2[3,1,h] <- summary(lm(as.formula(paste("log_yield_av_wm_share2",treatment,sep = "~")), data=dta_trim))$coefficients[1,1]
#res_prod_wm_share2[3,2,h] <- summary(lm(as.formula(paste("log_yield_av_wm_share2",treatment,sep = "~")), data=dta_trim))$coefficients[2,1]
#res_prod_wm_share2[3,3,h] <- ifelse(totrep >0, RI("log_yield_av_wm_share2",treatment , dta_trim, nr_repl = totrep), summary(lm(as.formula(paste("log_yield_av_wm_share2",treatment,sep = "~")), data=dta_trim))$coefficients[2,4])

#### was yield better compared to normal year?
#summary(lm(yield_better_wm_share2~messenger != "ctrl", data=dta_bal))

#res_prod_wm_share2[4,1,h] <- summary(lm(as.formula(paste("yield_better_wm_share2",treatment,sep = "~")), data=dta_bal))$coefficients[1,1]
#res_prod_wm_share2[4,2,h] <- summary(lm(as.formula(paste("yield_better_wm_share2",treatment,sep = "~")), data=dta_bal))$coefficients[2,1]
#res_prod_wm_share2[4,3,h] <- ifelse(totrep >0, RI("yield_better_wm_share2",treatment , dta_bal, nr_repl = totrep), summary(lm(as.formula(paste("yield_better_wm_share2",treatment,sep = "~")), data=dta_bal))$coefficients[2,4])

#dta_bal2 <- subset(dta_bal, area_tot_wm_share2 >0 & prod_tot_wm_share2>0 & yield_av_wm_share2 >0)
#dta_bal2$log_prod_tot_wm_share2 <- log(dta_bal2$prod_tot_wm_share2)
#dta_bal2$log_area_tot_wm_share2 <- log(dta_bal2$area_tot_wm_share2)
#dta_bal2$log_yield_av_wm_share2 <- log(dta_bal2$yield_av_wm_share2)

#dta_bal2 <- trim("log_yield_av_wm_share2", dta_bal2, .05)


##res_prod_wm_share2[1:4,4,h] <- FSR_OLS( c("log_prod_tot", "log_area_tot", "log_yield_av","yield_better") ,treatment,dta_bal, nr_repl = totrep)[[4]]


#dta_bal2$log_area_tot_wm_share2 <- -dta_bal2$log_area_tot_wm_share2

#	indexer <- FW_index(treatment, c("log_prod_tot_wm_share2", "log_area_tot_wm_share2","yield_better_wm_share2"),dta_bal2, nr_repl=totrep)
#	res_prod_wm_share2[5,1,h] <-  indexer[[1]]$coefficients[1,1]
#	res_prod_wm_share2[5,2,h] <-  indexer[[1]]$coefficients[2,1]
#	res_prod_wm_share2[5,3,h] <-  indexer[[2]]




}




