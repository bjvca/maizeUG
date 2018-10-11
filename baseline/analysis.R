library(foreign)
dta <- read.dta("/home/bjvca/data/projects/digital green/baseline/DLEC.dta")
dta$know_space <- dta$maizeoptimal_spacing == "a"
dta$know_small <- dta$maizeq22 == "c"
dta$know_weed <- dta$maizeq23 == "b"
siglev <-  1.96
set.seed(12345)
### drop the femheaded
dta <- subset(dta, recipient != "n/a")

### does it work?
#t.test(dta$know_space~dta$maizevideo_shown!="ctrl")
 prop.test(t(table(dta$know_space, dta$maizevideo_shown!="ctrl")))

#t.test(dta$know_small~dta$maizevideo_shown!="ctrl")
 prop.test(t(table(dta$know_small, dta$maizevideo_shown!="ctrl")))
#t.test(dta$know_weed~dta$maizevideo_shown!="ctrl")
 prop.test(t(table(dta$know_weed, dta$maizevideo_shown!="ctrl")))

### this is all cool, but we also need to balance accross treatment cells before running these tests

## sample size for balance H0 - no effect
##just leave all controls in there


## sample size for balance H1
s_h1 <- min(table(dta$maizevideo_shown[dta$maizevideo_shown != "ctrl"], dta$recipient[dta$maizevideo_shown != "ctrl"]))

dta_bal <- rbind(dta[dta$maizevideo_shown=="ctrl",],
 dta[dta$maizevideo_shown=="couple" & dta$recipient == "couple",][sample( nrow(dta[dta$maizevideo_shown=="couple" & dta$recipient == "couple",]),s_h1),],
 dta[dta$maizevideo_shown=="male" & dta$recipient == "couple",][sample( nrow(dta[dta$maizevideo_shown=="male" & dta$recipient == "couple",]),s_h1),],
 dta[dta$maizevideo_shown=="female" & dta$recipient == "couple",][sample( nrow(dta[dta$maizevideo_shown=="female" & dta$recipient == "couple",]),s_h1),],
 dta[dta$maizevideo_shown=="female" & dta$recipient == "male",][sample( nrow(dta[dta$maizevideo_shown=="female" & dta$recipient == "male",]),s_h1),],
 dta[dta$maizevideo_shown=="female" & dta$recipient == "female",][sample( nrow(dta[dta$maizevideo_shown=="female" & dta$recipient == "female",]),s_h1),],
 dta[dta$maizevideo_shown=="male" & dta$recipient == "male",][sample( nrow(dta[dta$maizevideo_shown=="male" & dta$recipient == "male",]),s_h1),],
 dta[dta$maizevideo_shown=="male" & dta$recipient == "female",][sample( nrow(dta[dta$maizevideo_shown=="male" & dta$recipient == "female",]),s_h1),],
 dta[dta$maizevideo_shown=="couple" & dta$recipient == "male",][sample( nrow(dta[dta$maizevideo_shown=="couple" & dta$recipient == "male",]),s_h1),],
 dta[dta$maizevideo_shown=="couple" & dta$recipient == "female",][sample( nrow(dta[dta$maizevideo_shown=="couple" & dta$recipient == "female",]),s_h1),])

prop.test(t(table(dta_bal$know_space, dta_bal$maizevideo_shown!="ctrl")))
prop.test(t(table(dta_bal$know_small, dta_bal$maizevideo_shown!="ctrl")))
prop.test(t(table(dta_bal$know_weed, dta_bal$maizevideo_shown!="ctrl")))



pdf("/home/bjvca/data/projects/digital green/baseline/videoworks.pdf")
par(mfrow=c(1,3))
siglev <-  1.96
means <- tapply(dta_bal$know_space,dta_bal$maizevideo_shown!="ctrl", FUN=mean)
barCenters <- barplot(tapply(dta_bal$know_space,dta_bal$maizevideo_shown!="ctrl", FUN=mean), ylim=c(0,1), xpd = FALSE, main="Spacing",xlab="Watched Video", names.arg=c("CTRL","TREAT"), ylab="proportion of correct answers", cex.lab=1.5, cex.axis=2, cex.main = 2)

segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
         means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5)

arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
       means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

means <- tapply(dta_bal$know_small,dta_bal$maizevideo_shown!="ctrl", FUN=mean)
barCenters <- barplot(tapply(dta_bal$know_small,dta_bal$maizevideo_shown!="ctrl", FUN=mean), ylim=c(0,1), xpd = FALSE, main="Combine",xlab="Watched Video", names.arg=c("CTRL","TREAT"), ylab="", cex.lab=1.5, cex.axis=2, cex.main = 2)

segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
         means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5)

arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
       means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

means <- tapply(dta_bal$know_weed,dta_bal$maizevideo_shown!="ctrl", FUN=mean)
barCenters <- barplot(tapply(dta_bal$know_weed,dta_bal$maizevideo_shown!="ctrl", FUN=mean), ylim=c(0,1), xpd = FALSE, main="Weeding",xlab="Watched Video", names.arg=c("CTRL","TREAT"), ylab="", cex.lab=1.5, cex.axis=2, cex.main = 2)

segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
         means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5)

arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
       means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
dev.off()




### drop control here, we are now only interested in comparing treatments
dta <- subset(dta, maizevideo_shown != "ctrl")

#### make sure to balance before doing tests
## sample size for balance H0 -  basically table(dta$maizevideo_shown[dta$recipient == "couple"])
s_h0 <- min(table(dta$maizevideo_shown, dta$recipient)[,1])

## sample size for balance H1
s_h1 <- min(table(dta$maizevideo_shown, dta$recipient)[,-1])

dta_bal <- rbind( dta[dta$maizevideo_shown=="couple" & dta$recipient == "couple",][sample( nrow(dta[dta$maizevideo_shown=="couple" & dta$recipient == "couple",]),s_h0),],
 dta[dta$maizevideo_shown=="male" & dta$recipient == "couple",][sample( nrow(dta[dta$maizevideo_shown=="male" & dta$recipient == "couple",]),s_h0),],
 dta[dta$maizevideo_shown=="female" & dta$recipient == "couple",][sample( nrow(dta[dta$maizevideo_shown=="female" & dta$recipient == "couple",]),s_h0),],

 dta[dta$maizevideo_shown=="female" & dta$recipient == "male",][sample( nrow(dta[dta$maizevideo_shown=="female" & dta$recipient == "male",]),s_h1),],
 dta[dta$maizevideo_shown=="female" & dta$recipient == "female",][sample( nrow(dta[dta$maizevideo_shown=="female" & dta$recipient == "female",]),s_h1),],
 dta[dta$maizevideo_shown=="male" & dta$recipient == "male",][sample( nrow(dta[dta$maizevideo_shown=="male" & dta$recipient == "male",]),s_h1),],
 dta[dta$maizevideo_shown=="male" & dta$recipient == "female",][sample( nrow(dta[dta$maizevideo_shown=="male" & dta$recipient == "female",]),s_h1),],
 dta[dta$maizevideo_shown=="couple" & dta$recipient == "male",][sample( nrow(dta[dta$maizevideo_shown=="couple" & dta$recipient == "male",]),s_h1),],
 dta[dta$maizevideo_shown=="couple" & dta$recipient == "female",][sample( nrow(dta[dta$maizevideo_shown=="couple" & dta$recipient == "female",]),s_h1),])




### info assymmetry effect
#t.test(dta$know_space~dta$recipient=="couple")
#t.test(dta$know_small~dta$recipient=="couple")
#t.test(dta$know_weed~dta$recipient=="couple")

prop.test(t(table(dta$know_space, dta$recipient=="couple")))
prop.test(t(table(dta$know_small, dta$recipient=="couple")))
prop.test(t(table(dta$know_weed, dta$recipient=="couple")))

prop.test(t(table(dta_bal$know_space, dta_bal$recipient=="couple")))
prop.test(t(table(dta_bal$know_small, dta_bal$recipient=="couple")))
prop.test(t(table(dta_bal$know_weed, dta_bal$recipient=="couple")))

pdf("/home/bjvca/data/projects/digital green/baseline/recipient.pdf")
par(mfrow=c(1,3))
means <- tapply(dta_bal$know_space,dta_bal$recipient=="couple", FUN=mean)
barCenters <- barplot(tapply(dta_bal$know_space,dta_bal$recipient=="couple", FUN=mean), ylim=c(0,1), xpd = FALSE, main="Spacing",xlab="Audience", names.arg=c("INDIV","COUPLE"), ylab="proportion of correct answers", cex.lab=1.5, cex.axis=2, cex.main = 2)

segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
         means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5)

arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
       means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

means <- tapply(dta_bal$know_small,dta_bal$recipient=="couple", FUN=mean)
barCenters <- barplot(tapply(dta_bal$know_small,dta_bal$recipient=="couple", FUN=mean), ylim=c(0,1), xpd = FALSE, main="Combine",xlab="Audience", names.arg=c("INDIV","COUPLE"), ylab="", cex.lab=1.5, cex.axis=2, cex.main = 2)

segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
         means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5)

arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
       means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
### but this one is sig at 10 percent siglev <-  1.64
means <- tapply(dta_bal$know_weed,dta_bal$recipient=="couple", FUN=mean)
barCenters <- barplot(tapply(dta_bal$know_weed,dta_bal$recipient=="couple", FUN=mean), ylim=c(0,1), xpd = FALSE, main="Weeding",xlab="Audience", names.arg=c("INDIV","COUPLE"), ylab="", cex.lab=1.5, cex.axis=2, cex.main = 2)

segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
         means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5)

arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
       means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
dev.off()
### projecting a household approach
#t.test(dta$know_space~dta$maizevideo_shown=="couple")
#t.test(dta$know_small~dta$maizevideo_shown=="couple")
#t.test(dta$know_weed~dta$maizevideo_shown=="couple")

#### make sure to balance before doing tests
## sample size for balance H0 -  basically table(dta$recipient[dta$maizevideo_shown == "couple"])
s_h0 <- min(table(dta$maizevideo_shown, dta$recipient)[1,])

## sample size for balance H1
s_h1 <- min(table(dta$maizevideo_shown, dta$recipient)[-1,])

dta_bal <- rbind( dta[dta$maizevideo_shown=="couple" & dta$recipient == "couple",][sample( nrow(dta[dta$maizevideo_shown=="couple" & dta$recipient == "couple",]),s_h0),],
 dta[dta$maizevideo_shown=="male" & dta$recipient == "couple",][sample( nrow(dta[dta$maizevideo_shown=="male" & dta$recipient == "couple",]),s_h1),],
 dta[dta$maizevideo_shown=="female" & dta$recipient == "couple",][sample( nrow(dta[dta$maizevideo_shown=="female" & dta$recipient == "couple",]),s_h1),],

 dta[dta$maizevideo_shown=="female" & dta$recipient == "male",][sample( nrow(dta[dta$maizevideo_shown=="female" & dta$recipient == "male",]),s_h1),],
 dta[dta$maizevideo_shown=="female" & dta$recipient == "female",][sample( nrow(dta[dta$maizevideo_shown=="female" & dta$recipient == "female",]),s_h1),],
 dta[dta$maizevideo_shown=="male" & dta$recipient == "male",][sample( nrow(dta[dta$maizevideo_shown=="male" & dta$recipient == "male",]),s_h1),],
 dta[dta$maizevideo_shown=="male" & dta$recipient == "female",][sample( nrow(dta[dta$maizevideo_shown=="male" & dta$recipient == "female",]),s_h1),],
 dta[dta$maizevideo_shown=="couple" & dta$recipient == "male",][sample( nrow(dta[dta$maizevideo_shown=="couple" & dta$recipient == "male",]),s_h0),],
 dta[dta$maizevideo_shown=="couple" & dta$recipient == "female",][sample( nrow(dta[dta$maizevideo_shown=="couple" & dta$recipient == "female",]),s_h0),])




prop.test(t(table(dta$know_space, dta$maizevideo_shown=="couple")))
prop.test(t(table(dta_bal$know_space, dta_bal$maizevideo_shown=="couple")))

prop.test(t(table(dta$know_small, dta$maizevideo_shown=="couple")))
prop.test(t(table(dta_bal$know_small, dta_bal$maizevideo_shown=="couple")))

prop.test(t(table(dta$know_weed, dta$maizevideo_shown=="couple")))
prop.test(t(table(dta_bal$know_weed, dta_bal$maizevideo_shown=="couple")))




pdf("/home/bjvca/data/projects/digital green/baseline/messenger.pdf")
par(mfrow=c(1,3))
means <- tapply(dta_bal$know_space,dta_bal$maizevideo_shown=="couple", FUN=mean)
barCenters <- barplot(tapply(dta_bal$know_space,dta_bal$maizevideo_shown=="couple", FUN=mean), ylim=c(0,1), xpd = FALSE, main="Spacing",xlab="Messenger", names.arg=c("INDIV","COUPLE"), ylab="proportion of correct answers", cex.lab=1.5, cex.axis=2, cex.main = 2)

segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
         means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5)

arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
       means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

means <- tapply(dta_bal$know_small,dta_bal$maizevideo_shown=="couple", FUN=mean)
barCenters <- barplot(tapply(dta_bal$know_small,dta_bal$maizevideo_shown=="couple", FUN=mean), ylim=c(0,1), xpd = FALSE, main="Combine",xlab="Messenger", names.arg=c("INDIV","COUPLE"), ylab="", cex.lab=1.5, cex.axis=2, cex.main = 2)

segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
         means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5)

arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
       means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
### but this one is sig at 10 percent siglev <-  1.64
means <- tapply(dta_bal$know_weed,dta_bal$maizevideo_shown=="couple", FUN=mean)
barCenters <- barplot(tapply(dta_bal$know_weed,dta_bal$maizevideo_shown=="couple", FUN=mean), ylim=c(0,1), xpd = FALSE, main="Weeding",xlab="Messenger", names.arg=c("INDIV","COUPLE"), ylab="", cex.lab=1.5, cex.axis=2, cex.main = 2)

segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
         means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5)

arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
       means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

dev.off()

### gender matching

dta <- subset(dta, recipient != "couple" & maizevideo_shown != "couple")
s_h0 <- min(table(dta$recipient, dta$maizevideo_shown))

dta_bal <- rbind(
 dta[dta$maizevideo_shown=="female" & dta$recipient == "male",][sample( nrow(dta[dta$maizevideo_shown=="female" & dta$recipient == "male",]),s_h0),],
 dta[dta$maizevideo_shown=="female" & dta$recipient == "female",][sample( nrow(dta[dta$maizevideo_shown=="female" & dta$recipient == "female",]),s_h0),],
 dta[dta$maizevideo_shown=="male" & dta$recipient == "male",][sample( nrow(dta[dta$maizevideo_shown=="male" & dta$recipient == "male",]),s_h0),],
 dta[dta$maizevideo_shown=="male" & dta$recipient == "female",][sample( nrow(dta[dta$maizevideo_shown=="male" & dta$recipient == "female",]),s_h0),])


dta$matched <- NA
dta$matched[dta$recipient == "male" & dta$maizevideo_shown=="male" | dta$recipient == "female" & dta$maizevideo_shown=="female"] <- TRUE
dta$matched[dta$recipient == "male" & dta$maizevideo_shown=="female" | dta$recipient == "female" & dta$maizevideo_shown=="male"] <- FALSE

t.test(dta$know_space~dta$matched)
t.test(dta$know_small~dta$matched)
t.test(dta$know_weed~dta$matched)


dta_bal$matched <- NA
dta_bal$matched[dta_bal$recipient == "male" & dta_bal$maizevideo_shown=="male" | dta_bal$recipient == "female" & dta_bal$maizevideo_shown=="female"] <- TRUE
dta_bal$matched[dta_bal$recipient == "male" & dta_bal$maizevideo_shown=="female" | dta_bal$recipient == "female" & dta_bal$maizevideo_shown=="male"] <- FALSE



prop.test(t(table(dta$know_space, dta$matched)))
prop.test(t(table(dta_bal$know_space, dta_bal$matched)))

prop.test(t(table(dta$know_small, dta$matched)))
prop.test(t(table(dta_bal$know_small, dta_bal$matched)))

prop.test(t(table(dta$know_weed, dta$matched)))
prop.test(t(table(dta_bal$know_weed, dta_bal$matched)))


pdf("/home/bjvca/data/projects/digital green/baseline/matched.pdf")
par(mfrow=c(1,3))
means <- tapply(dta$know_space,  dta$matched, FUN=mean)
barCenters <- barplot(tapply(dta$know_space, dta$matched, FUN=mean), ylim=c(0,1), xpd = FALSE, main="Spacing",xlab="gender-matching", names.arg=c("MIXED","MATCHED"), ylab="proportion of correct answers", cex.lab=1.5, cex.axis=2, cex.main = 2)

segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta)[1]), barCenters,
         means +  siglev*sqrt(means*(1-means)/dim(dta)[1]), lwd = 1.5)

arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta)[1]), barCenters,
       means +  siglev*sqrt(means*(1-means)/dim(dta)[1]), lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

means <- tapply(dta$know_small, dta$matched, FUN=mean)
barCenters <- barplot(tapply(dta$know_small, dta$matched, FUN=mean), ylim=c(0,1), xpd = FALSE, main="Combine",xlab="gender-matching", names.arg=c("MIXED","MATCHED"), ylab="", cex.lab=1.5, cex.axis=2, cex.main = 2)

segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta)[1]), barCenters,
         means +  siglev*sqrt(means*(1-means)/dim(dta)[1]), lwd = 1.5)

arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta)[1]), barCenters,
       means +  siglev*sqrt(means*(1-means)/dim(dta)[1]), lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
### but this one is sig at 10 percent siglev <-  1.64
means <- tapply(dta$know_weed,dta$matched, FUN=mean)
barCenters <- barplot(tapply(dta$know_weed,dta$matched, FUN=mean), ylim=c(0,1), xpd = FALSE, main="Weeding",xlab="gender-matching", names.arg=c("MIXED","MATCHED"), ylab="", cex.lab=1.5, cex.axis=2, cex.main = 2)

segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta)[1]), barCenters,
         means +  siglev*sqrt(means*(1-means)/dim(dta)[1]), lwd = 1.5)

arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta)[1]), barCenters,
       means +  siglev*sqrt(means*(1-means)/dim(dta)[1]), lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
dev.off()



### look at some balancing variables

library(foreign)
dta <- read.dta("/home/bjvca/data/projects/digital green/baseline/DLEC.dta")


###################  function for balancing over treatment cells
balancr <- function(h_ind = h, dta_in = dta_bal, bal_var="know_space") {
#bal_var <- "dectime_man_pl1"
#h <- 1
#dta_in <- dta

if (h_ind==1) {
s_h1 <- min(table(factor(dta$recipient[dta$recipient!="couple"  & !is.na(dta_in[bal_var])])))
dta_out <- rbind(dta_in[dta_in$recipient=="couple",]
, dta_in[dta_in$recipient=="female"  & !is.na(dta_in[bal_var]),][sample( nrow(dta_in[dta_in$recipient=="female"  & !is.na(dta_in[bal_var]),]),s_h1),], dta_in[dta_in$recipient=="male"   & !is.na(dta_in[bal_var]),][sample( nrow(dta_in[dta_in$recipient=="male" & !is.na(dta_in[bal_var]),]),s_h1),])
} else if (h_ind==2) {
s_h1 <- min(table(factor(dta$recipient[dta$recipient!="male"  & !is.na(dta_in[bal_var])])))
dta_out <- rbind(dta_in[dta_in$recipient=="male",]
, dta_in[dta_in$recipient=="female"  & !is.na(dta_in[bal_var]),][sample( nrow(dta_in[dta_in$recipient=="female"  & !is.na(dta_in[bal_var]),]),s_h1),], dta_in[dta_in$recipient=="couple"   & !is.na(dta_in[bal_var]),][sample( nrow(dta_in[dta_in$recipient=="couple" & !is.na(dta_in[bal_var]),]),s_h1),])
} else if (h_ind==5) {
s_h1 <- min(table(factor(dta$messenger[dta$messenger!="couple"  & !is.na(dta_in[bal_var])])))
dta_out <- rbind(dta_in[dta_in$messenger=="couple",]
, dta_in[dta_in$messenger=="female"  & !is.na(dta_in[bal_var]),][sample( nrow(dta_in[dta_in$messenger=="female"  & !is.na(dta_in[bal_var]),]),s_h1),], dta_in[dta_in$messenger=="male"   & !is.na(dta_in[bal_var]),][sample( nrow(dta_in[dta_in$messenger=="male" & !is.na(dta_in[bal_var]),]),s_h1),])
}  else if (h_ind==6) {
s_h1 <- min(table(factor(dta$messenger[dta$messenger!="male"  & !is.na(dta_in[bal_var])])))
dta_out <- rbind(dta_in[dta_in$messenger=="male",]
, dta_in[dta_in$messenger=="female"  & !is.na(dta_in[bal_var]),][sample( nrow(dta_in[dta_in$messenger=="female"  & !is.na(dta_in[bal_var]),]),s_h1),], dta_in[dta_in$messenger=="couple"   & !is.na(dta_in[bal_var]),][sample( nrow(dta_in[dta_in$messenger=="couple" & !is.na(dta_in[bal_var]),]),s_h1),])
}  else if (h_ind==7) {
s_h_match <- min(diag(table(factor(dta_in$messenger[ !is.na(dta_in[bal_var])]),factor(dta_in$recipient[!is.na(dta_in[bal_var])])) ))
s_h_nonmatch <- min(table(factor(dta_in$messenger[ !is.na(dta_in[bal_var])]),factor(dta_in$recipient[!is.na(dta_in[bal_var])]))[1,2],table(factor(dta_in$messenger[ !is.na(dta_in[bal_var])]),factor(dta_in$recipient[!is.na(dta_in[bal_var])]))[2,1] )
dta_out <- rbind(dta_in[dta_in$messenger=="female"  & dta_in$recipient=="female" &  !is.na(dta_in[bal_var]),][sample( nrow(dta_in[dta_in$messenger=="female"  & dta_in$recipient=="female" & !is.na(dta_in[bal_var]),]),s_h_match),],
dta_in[dta_in$messenger=="male"  & dta_in$recipient=="male" &  !is.na(dta_in[bal_var]),][sample( nrow(dta_in[dta_in$messenger=="male"  & dta_in$recipient=="male" & !is.na(dta_in[bal_var]),]),s_h_match),],
dta_in[dta_in$messenger=="female"  & dta_in$recipient=="male" &  !is.na(dta_in[bal_var]),][sample( nrow(dta_in[dta_in$messenger=="female"  & dta_in$recipient=="male" & !is.na(dta_in[bal_var]),]),s_h_nonmatch),],
dta_in[dta_in$messenger=="male"  & dta_in$recipient=="female" &  !is.na(dta_in[bal_var]),][sample( nrow(dta_in[dta_in$messenger=="male"  & dta_in$recipient=="female" & !is.na(dta_in[bal_var]),]),s_h_nonmatch),])
} else {
dta_out <- dta_in
}
 
return(dta_out)
}

###################

dta$know_space <- dta$maizeoptimal_spacing == "a"
dta$know_small <- dta$maizeq22 == "c"
dta$know_weed <- dta$maizeq23 == "b"
dta$maizeage[dta$maizeage == 999] <- NA
siglev <-  1.96
set.seed(54321)
### drop the femheaded
dta <- subset(dta, recipient != "n/a")
dta$messenger <- dta$maizevideo_shown

## merge in treatments
treats <- read.csv("/home/bjvca/data/projects/digital green/sampling/sampling_list_ID.csv")[c("HHID","IVR","sms")]
names(treats) <- c("hhid","ivr","sms")
dta <- merge(treats, dta, by="hhid", all=F)

summary(dta$maizebags_harv*100/dta$maizearea_cultivation)
summary(dta$maizeage)
summary(as.numeric(dta$maizeeduc>2))
summary(dta$maizehh_no)
summary(dta$maizeprrooms) 
summary(as.numeric(dta$maizeprinfo_receiv=="Yes"))
#this is actually fertilizer use
summary(as.numeric(dta$maizeprinfo_receiv_spouse=="Yes"))
#this is seed use
summary(as.numeric(dta$maizeprinput_use=="Yes"))
summary(dta$maizedist_shop)
dta$yield <- dta$maizebags_harv*100/dta$maizearea_cultivation
dta_all <- dta
dta <- subset(dta_all, messenger!='ctrl')

summary(dta$maizebags_harv*100/dta$maizearea_cultivation)
summary(dta$maizeage)
summary(as.numeric(dta$maizeeduc>2))
summary(dta$maizehh_no)
summary(dta$maizeprrooms) 
summary(as.numeric(dta$maizeprinfo_receiv=="Yes"))
#this is actually fertilizer use
summary(as.numeric(dta$maizeprinfo_receiv_spouse=="Yes"))
#this is seed use
summary(as.numeric(dta$maizeprinput_use=="Yes"))
summary(dta$maizedist_shop)

dta_copy <- dta_all

balance <- array(NA,c(18, 7))
jointF <-  array(NA,c(3, 8))
write.csv(dta_all,"/home/bjvca/data/projects/digital green/baseline/base_merge.cvs")

for (h in 1:7) {
if (h==1) {
############################################ H1: info asymmetry: rec=individual vs rec=couple #########################################################
dta <- dta_copy

treat <- "(recipient == 'couple') +ivr+sms+as.factor(messenger)" 
} else if (h==2) {
############################################ H2: empower: rec=male vs rec=couple or woman #########################################################
dta <- dta_copy
treat <- "(recipient != 'male') +ivr+sms+ as.factor(messenger)" 
} else if (h==3) {
############################################ H3: empower a1: rec=male vs rec=female ###################################################
dta <- subset(dta_copy, recipient == "male" | recipient == "female")
treat <- "(recipient == 'female') +ivr+sms+as.factor(messenger)" 
} else if (h==4) {
############################################ H4: empower a2: rec=male vs rec=couple ###################################################
dta <- subset(dta_copy, recipient == "male" | recipient == "couple")
treat <- "(recipient == 'couple') +ivr+sms+ as.factor(messenger)"

##is this equal to just comparing female to couple???

} else if (h==5) {
############################################ H5: promote collective approach ###################################################
dta <- dta_copy
treat <- "(messenger == 'couple') +ivr+sms+as.factor(recipient)"
} else if (h==6) {
############################################ H6: challenge gender stereotype ###################################################
dta <- dta_copy
treat <- "(messenger != 'male') +ivr+sms+as.factor(recipient)"
} else if (h==7) {
############################################ H7: homophily ###################################################
dta <- subset(dta_copy, recipient != "couple" & messenger != "couple")
treat <- "(messenger == recipient) +ivr+sms"
}
print(h)


dta2 <- balancr( h_ind = h, dta_in = dta, bal_var="yield")
balance[1,h] <-  summary(lm(as.formula(paste("yield",treat, sep="~")), data= dta2))$coefficients[2,1]
balance[2,h] <-  summary(lm(as.formula(paste("yield",treat, sep="~")), data= dta2))$coefficients[2,4]

dta2 <- balancr( h_ind = h, dta_in = dta, bal_var="maizeage")
balance[3,h] <- summary(lm(as.formula(paste("maizeage",treat, sep="~")), data= dta2))$coefficients[2,1]
balance[4,h] <- summary(lm(as.formula(paste("maizeage",treat, sep="~")), data= dta2))$coefficients[2,4]

dta2 <- balancr( h_ind = h, dta_in = dta, bal_var="maizeeduc")
balance[5,h] <- summary(lm(as.formula(paste("(maizeeduc > 2)",treat, sep="~")), data= dta2))$coefficients[2,1]
balance[6,h] <- summary(lm(as.formula(paste("(maizeeduc >2)",treat, sep="~")), data= dta2))$coefficients[2,4]

dta2 <- balancr( h_ind = h, dta_in = dta, bal_var="maizehh_no")
balance[7,h] <- summary(lm(as.formula(paste("maizehh_no",treat, sep="~")), data= dta2))$coefficients[2,1]
balance[8,h] <- summary(lm(as.formula(paste("maizehh_no",treat, sep="~")), data= dta2))$coefficients[2,4]

dta2 <- balancr( h_ind = h, dta_in = dta, bal_var="maizeprrooms")
balance[9,h] <-summary(lm(as.formula(paste("maizeprrooms",treat, sep="~")), data= dta2))$coefficients[2,1]
balance[10,h] <-summary(lm(as.formula(paste("maizeprrooms",treat, sep="~")), data= dta2))$coefficients[2,4]

dta2 <- balancr( h_ind = h, dta_in = dta, bal_var="maizeprinfo_receiv")
balance[11,h] <-summary(lm(as.formula(paste("(maizeprinfo_receiv=='Yes')",treat, sep="~")), data= dta2))$coefficients[2,1]
balance[12,h] <-summary(lm(as.formula(paste("(maizeprinfo_receiv=='Yes')",treat, sep="~")), data= dta2))$coefficients[2,4]
### labeling mistake here: maizeprinfo_receiv_spouse should be fertilizer_use 

dta2 <- balancr( h_ind = h, dta_in = dta, bal_var="maizeprinfo_receiv_spouse")
balance[13,h] <-summary(lm(as.formula(paste("maizeprinfo_receiv_spouse=='Yes'",treat, sep="~")), data= dta2))$coefficients[2,1]
balance[14,h] <-summary(lm(as.formula(paste("maizeprinfo_receiv_spouse=='Yes'",treat, sep="~")), data= dta2))$coefficients[2,4]
### labeling mistake here: maizeprinfo_receiv_spouse should be improvedseed_use
dta2 <- balancr( h_ind = h, dta_in = dta, bal_var="maizeprinput_use")
balance[15,h] <-summary(lm(as.formula(paste("maizeprinput_use=='Yes'",treat, sep="~")), data= dta2))$coefficients[2,1]
balance[16,h] <-summary(lm(as.formula(paste("maizeprinput_use=='Yes'",treat, sep="~")), data= dta2))$coefficients[2,4]
dta2 <- balancr( h_ind = h, dta_in = dta, bal_var="maizedist_shop")
balance[17,h] <-summary(lm(as.formula(paste("maizedist_shop",treat, sep="~")), data= dta2))$coefficients[2,1]
balance[18,h] <-summary(lm(as.formula(paste("maizedist_shop",treat, sep="~")), data= dta2))$coefficients[2,4]

}

dta <- dta_all
## joint tests and Nobs
covars <- c("yield + maizeage + (maizeeduc > 2) + maizehh_no+ maizeprrooms + (maizeprinfo_receiv=='Yes') + (maizeprinfo_receiv_spouse=='Yes') + (maizeprinput_use=='Yes') + maizedist_shop")
x <- summary(lm(as.formula(paste("(messenger != 'ctrl')",covars, sep="~")), data= dta))
jointF[1,1] <- x$fstatistic[1]
jointF[2,1] <- pf(x$fstatistic[1],x$fstatistic[2],x$fstatistic[3],lower.tail=FALSE)
jointF[3,1] <- x$df[1] + x$df[2]

dta <- subset(dta_all, messenger!='ctrl')
x <- summary(lm(as.formula(paste("(maizeivr=='yes')",covars, sep="~")), data= dta))
jointF[1,2] <- x$fstatistic[1]
jointF[2,2] <- pf(x$fstatistic[1],x$fstatistic[2],x$fstatistic[3],lower.tail=FALSE)
jointF[3,2] <- x$df[1] + x$df[2]

dta <- subset(dta_all, maizeivr=='yes' )
x <- summary(lm(as.formula(paste("(sms=='yes')",covars, sep="~")), data= dta))
jointF[1,3] <- x$fstatistic[1]
jointF[2,3] <- pf(x$fstatistic[1],x$fstatistic[2],x$fstatistic[3],lower.tail=FALSE)
jointF[3,3] <- x$df[1] + x$df[2]

#H1:

dta <- subset(dta_all, messenger!='ctrl')
h <- 1
x <- summary(lm(as.formula(paste("(recipient=='couple')",covars, sep="~")), data= balancr( h_ind = h, dta_in = dta, bal_var="yield")))
jointF[1,4] <- x$fstatistic[1]
jointF[2,4] <- pf(x$fstatistic[1],x$fstatistic[2],x$fstatistic[3],lower.tail=FALSE)
jointF[3,4] <- x$df[1] + x$df[2]


#H2
h <- 2
x <- summary(lm(as.formula(paste("(recipient=='male')",covars, sep="~")), data= balancr( h_ind = h, dta_in = dta, bal_var="yield")))
jointF[1,5] <- x$fstatistic[1]
jointF[2,5] <- pf(x$fstatistic[1],x$fstatistic[2],x$fstatistic[3],lower.tail=FALSE)
jointF[3,5] <- x$df[1] + x$df[2]


#H3
h <- 5
x <- summary(lm(as.formula(paste("(messenger=='couple')",covars, sep="~")), data= balancr( h_ind = h, dta_in = dta, bal_var="yield")))
jointF[1,6] <- x$fstatistic[1]
jointF[2,6] <- pf(x$fstatistic[1],x$fstatistic[2],x$fstatistic[3],lower.tail=FALSE)
jointF[3,6] <- x$df[1] + x$df[2]

#H4
h <- 6
x <- summary(lm(as.formula(paste("(messenger=='male')",covars, sep="~")), data= balancr( h_ind = h, dta_in = dta, bal_var="yield")))
jointF[1,7] <- x$fstatistic[1]
jointF[2,7] <- pf(x$fstatistic[1],x$fstatistic[2],x$fstatistic[3],lower.tail=FALSE)
jointF[3,7] <- x$df[1] + x$df[2]

h <- 7
dta <- subset(dta, (messenger!='couple' & recipient!='couple') )
x <- summary(lm(as.formula(paste("(recipient==messenger)",covars, sep="~")), data= balancr( h_ind = h, dta_in = dta, bal_var="yield")))
jointF[1,8] <- x$fstatistic[1]
jointF[2,8] <- pf(x$fstatistic[1],x$fstatistic[2],x$fstatistic[3],lower.tail=FALSE)
jointF[3,8] <- x$df[1] + x$df[2]


library(foreign)
dta <- read.dta("/home/bjvca/data/projects/digital green/baseline/DLEC.dta")

dta$know_space <- dta$maizeoptimal_spacing == "a"
dta$know_small <- dta$maizeq22 == "c"
dta$know_weed <- dta$maizeq23 == "b"
dta$maizeage[dta$maizeage == 999] <- NA
siglev <-  1.96
set.seed(54321)

### here we do not drop the femheaded, in femheaded HH, the video was shown to the female
dta$recipient[dta$recipient == "n/a"] <- "female"
dta$messenger <- dta$maizevideo_shown
dta$video <- TRUE
dta$video[dta$messenger=="ctrl"] <- FALSE

## merge in treatments
treats <- read.csv("/home/bjvca/data/projects/digital green/sampling/sampling_list_ID.csv")[c("HHID","IVR","sms")]
names(treats) <- c("hhid","ivr","sms")
treats$femhead <- FALSE
###merge in treatments for FHs - did they receive sms messages?
treats_FH <- read.csv("/home/bjvca/data/projects/digital green/sampling/femhead_list_ID.csv")[c("HHID","IVR")]
names(treats_FH) <- c("hhid","ivr")
treats_FH$sms <- "no"
treats_FH$femhead <- TRUE

treats <- rbind(treats_FH, treats)
treats$sms <- as.factor(treats$sms)
dta <- merge(treats, dta, by="hhid", all=F)

dta$yield <- dta$maizebags_harv*100/dta$maizearea_cultivation
dta$eduhead <- as.numeric(dta$maizeeduc>2)
dta$maizeprinfo_receiv <- as.numeric(dta$maizeprinfo_receiv=="Yes")
dta$fert <- as.numeric(dta$maizeprinfo_receiv_spouse=="Yes")
dta$seed <- as.numeric(dta$maizeprinput_use=="Yes")#### redo balance tests for DP_ICT paper
dta$maizemobile <- dta$maizemobile == "Yes"
dta$maizemobile_access <- dta$maizemobile_access == "Yes"
dta$maizemobile_access[dta$maizemobile == TRUE] <- TRUE 

outmat <- array(NA,c(22, 8))

outcome <- c("yield","","maizeage","","eduhead","","maizehh_no","","maizeprrooms","","maizeprinfo_receiv","", "fert","","seed","","maizedist_shop","","maizemobile","","maizemobile_access","")
for (i in seq(1,22,2)) {
print(i)
outmat[i,1] <-  mean(unlist(dta[ dta$video==FALSE,][outcome[i]]), na.rm=T)
outmat[i+1,1] <-  sd(unlist(dta[ dta$video==FALSE,][outcome[i]]), na.rm=T)
outmat[i,2] <- summary(lm(as.formula(paste(outcome[i],"video+ivr+sms+messenger+recipient+femhead",sep="~")) ,data=dta))$coef[2,1]
outmat[i+1,2] <- summary(lm(as.formula(paste(outcome[i],"video+ivr+sms+messenger+recipient+femhead",sep="~")) ,data=dta))$coef[2,2]
outmat[i,3]  <- summary(lm(as.formula(paste(outcome[i],"video+ivr+sms+messenger+recipient+femhead",sep="~")) ,data=dta))$coef[2,4]
outmat[i,4] <- summary(lm(as.formula(paste(outcome[i],"video+ivr+sms+messenger+recipient+femhead",sep="~")) ,data=dta))$coef[3,1]
outmat[i+1,4] <- summary(lm(as.formula(paste(outcome[i],"video+ivr+sms+messenger+recipient+femhead",sep="~")) ,data=dta))$coef[3,2]
outmat[i,5]  <- summary(lm(as.formula(paste(outcome[i],"video+ivr+sms+messenger+recipient+femhead",sep="~")) ,data=dta))$coef[3,4]
outmat[i,6] <- summary(lm(as.formula(paste(outcome[i],"video+ivr+sms+messenger+recipient+femhead",sep="~")) ,data=dta))$coef[4,1]
outmat[i+1,6] <- summary(lm(as.formula(paste(outcome[i],"video+ivr+sms+messenger+recipient+femhead",sep="~")) ,data=dta))$coef[4,2]
outmat[i,7] <-  summary(lm(as.formula(paste(outcome[i],"video+ivr+sms+messenger+recipient+femhead",sep="~")) ,data=dta))$coef[4,4]
outmat[i,8] <-  nobs(lm(as.formula(paste(outcome[i],"video+ivr+sms+messenger+recipient+femhead",sep="~")) ,data=dta))
}

## F-tests - these are partial F-test as we also control for design effects (messenger, recipient and femhead)

dta_cpy <- dta
write.csv(dta,"/home/bjvca/data/projects/digital green/endline/data/raw/baseline.csv")

### an extremely inelegant way to get rid of missings...
fm <- lm(video ~yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop + maizemobile + maizemobile_access + (messenger == "male") + (messenger == "female") + recipient +femhead+ivr+sms, data=dta,na.action = na.exclude)
dta$resid <- resid(fm)
dta <- subset(dta, !is.na(resid))

reduced <- lm(video ~ (messenger == "male") + (messenger == "female")+recipient +femhead+ivr+sms, data=dta)
full <- lm(video ~yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop + maizemobile + maizemobile_access + (messenger == "male") + (messenger == "female") + recipient +femhead+ivr+sms, data=dta)
anova(reduced,full)

dta <- dta_cpy
dta$num_ivr[dta$ivr == "yes"] <- 1
dta$num_ivr[dta$ivr == "no"] <- 0
### an extremely inelegant way to get rid of missings...
fm <- lm(as.numeric(num_ivr) ~yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop + maizemobile + maizemobile_access  + messenger+recipient +femhead+video+sms, data=dta,na.action = na.exclude)
dta$resid <- resid(fm)
dta <- subset(dta, !is.na(resid))

reduced <- lm(num_ivr ~ messenger+recipient +femhead+video+sms, data=dta)
full <- lm(num_ivr ~yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop + maizemobile + maizemobile_access  + messenger+recipient +femhead+video+sms, data=dta)
anova(reduced,full)

dta <- dta_cpy
dta$num_sms[dta$sms == "yes"] <- 1
dta$num_sms[dta$sms == "no"] <- 0
### an extremely inelegant way to get rid of missings...
fm <- lm(as.numeric(num_sms) ~yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop  + maizemobile + maizemobile_access + messenger+recipient +femhead+ video+ivr, data=dta,na.action = na.exclude)
dta$resid <- resid(fm)
dta <- subset(dta, !is.na(resid))

reduced <- lm(as.numeric(num_sms) ~ messenger+recipient +femhead+video+ivr, data=dta)
full <- lm(as.numeric(num_sms) ~yield+maizeage+eduhead+maizehh_no+maizeprrooms+maizeprinfo_receiv+fert+seed+maizedist_shop  + maizemobile + maizemobile_access + messenger+recipient +femhead+video+ivr, data=dta)
anova(reduced,full)


