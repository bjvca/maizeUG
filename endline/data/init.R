### Some initial explorations when first batch of data came in
dta <- read.csv("/home/bjvca/data/projects/digital green/endline/data/endline.csv")
dta <- subset(dta, femalehead == 0)



 

# the variable video_shown does not have info so let's just merge in from the sampling list
treats <- read.csv("/home/bjvca/data/projects/digital green/midline/list_sec.csv")
dta <- merge(treats, dta, by="hhid", all.y=T)

### who recieved the video?
dta$recipient.y <- NULL
names(dta)[names(dta) == 'recipient.x'] <- 'recipient'

library(dplyr)
dta <- dta %>% 
    mutate(treat = group_indices_(dta, .dots=c("recipient", "messenger"))) 
dta <- dta %>% 
    mutate(uniqID = group_indices_(dta, .dots=c("distID", "subID","vilID"))) 
dta$perm <- unlist(sapply(names(table(dta$uniqID)), function(x) resample(dta$treat[dta$uniq==x])))
		dta$messenger_p[dta$perm == 4  |dta$perm == 8  |dta$perm == 12  ] <- "male"
		dta$messenger_p[dta$perm == 3  |dta$perm == 7  |dta$perm == 11  ] <- "female"
		dta$messenger_p[dta$perm == 1  |dta$perm == 5  |dta$perm == 9  ] <- "female"
		dta$messenger_p[dta$perm == 2  |dta$perm == 6  |dta$perm == 10  ] <- "ctrl"
		dta$recipient_p[dta$perm == 9  |dta$perm == 10  |dta$perm == 11 |dta$perm == 12  ] <- "male"
		dta$recipient_p[dta$perm == 5  |dta$perm == 6  |dta$perm == 7 |dta$perm == 8  ] <- "female"
		dta$recipient_p[dta$perm == 1  |dta$perm == 2  |dta$perm == 3 |dta$perm == 4  ] <- "couple"

library(doParallel)

cl <- makeCluster(4)
registerDoParallel(cl)

RI <- function(dep, indep, dta , nr_repl = 1000) {
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
			}
			)
	
}
print(sum(oper)/nr_repl)
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


## effect on production on first plot:
t.test(dta_bal$grp1a16*dta_bal$grp1a17~dta_bal$messenger == "ctrl")

## production compared to previous year (below 3 is better)
t.test(dta_bal$grp1a18<3~dta_bal$messenger == "ctrl")
## used recommended spacing
t.test(dta_bal$grp1a201==1~dta_bal$messenger == "ctrl")
RI("grp1a201==1","messenger == 'ctrl'" , dta_bal)

## used recommended way to fight striga
t.test(dta_bal$grp1a241==1~dta_bal$messenger == "ctrl")
RI("grp1a241==1","messenger == 'ctrl'" , dta_bal)

## weeded on recommended timing?
t.test(dta_bal$grp1a26==1~dta_bal$messenger == "ctrl")
## used fertilizer 
t.test(dta_bal$grp1a29=="Yes"~dta_bal$messenger == "ctrl")
prop.test(t(table(dta_bal$grp1a29=="Yes"~dta_bal$messenger == "ctrl")))
system.time(RI("grp1a29=='Yes' ","messenger == 'ctrl'" , dta_bal))
##improved seed
t.test(dta_bal$grp1a42=="Yes"~dta_bal$messenger == "ctrl")
prop.test(t(table(dta_bal$grp1a29=="Yes"~dta_bal$messenger == "ctrl")))
## chemicals
t.test(dta_bal$grp1a55a=="Yes"~dta_bal$messenger == "ctrl")

## better off than others
t.test(dta_bal$q409==1~dta_bal$messenger == "ctrl")
prop.test(t(table(dta_bal$q409==1,dta_bal$messenger == "ctrl")))

## better off than 6 months ago
t.test(dta_bal$q110==1~dta_bal$messenger == "ctrl")
prop.test(t(table(dta_bal$q110==1,dta_bal$messenger == "ctrl")))



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



### more effective if targetted to both male and female?
t.test(dta_bal$grp1a16*dta_bal$grp1a17 ~ dta_bal$recipient == "couple")
### decided together to plant maize on this plot 
t.test((dta_bal$grp1a10==3 & dta_bal$spouse2grp_sp1f10==3) ~dta_bal$recipient == "couple")
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


