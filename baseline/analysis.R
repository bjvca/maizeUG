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
dta$know_space <- dta$maizeoptimal_spacing == "a"
dta$know_small <- dta$maizeq22 == "c"
dta$know_weed <- dta$maizeq23 == "b"
dta$maizeage[dta$maizeage == 999] <- NA
siglev <-  1.96
set.seed(54321)
### drop the femheaded
dta <- subset(dta, recipient != "n/a")

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


summary(lm((dta_bal$maizebags_harv*100/dta_bal$maizearea_cultivation~dta_bal$maizevideo_shown!="ctrl")))
summary(lm(dta_bal$maizeage~dta_bal$maizevideo_shown!="ctrl"))
summary(lm(as.numeric(dta_bal$maizeeduc>2)~dta_bal$maizevideo_shown!="ctrl"))
summary(lm(dta_bal$maizehh_no~dta_bal$maizevideo_shown!="ctrl"))
summary(lm(dta_bal$maizeprrooms~dta_bal$maizevideo_shown!="ctrl"))
summary(lm(as.numeric(dta_bal$maizeprinfo_receiv=="Yes")~dta_bal$maizevideo_shown!="ctrl"))
## labeling mistake here: maizeprinfo_receiv_spouse should be fertilizer_use 
summary(lm(as.numeric(dta_bal$maizeprinfo_receiv_spouse=="Yes")~dta_bal$maizevideo_shown!="ctrl"))
## labeling mistake here: maizeprinfo_receiv_spouse should be improvedseed_use
summary(lm(as.numeric(dta_bal$maizeprinput_use=="Yes")~dta_bal$maizevideo_shown!="ctrl"))
summary(lm(dta_bal$maizedist_shop~dta_bal$maizevideo_shown!="ctrl"))
dta_bal$yield <- dta_bal$maizebags_harv*100/dta_bal$maizearea_cultivation

#also do some joint tests
summary(lm((maizevideo_shown!="ctrl")~yield + maizeage + as.numeric(maizeeduc>2) + maizehh_no + maizeprrooms + as.numeric(maizeprinfo_receiv=="Yes") + as.numeric(maizeprinfo_receiv_spouse=="Yes") + as.numeric(maizeprinput_use=="Yes") + maizedist_shop, data=dta_bal))

dta <- subset(dta, maizevideo_shown != "ctrl")

#### make sure to balance before doing tests
## sample size for balance H0
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




summary(lm(dta_bal$maizebags_harv*100/dta_bal$maizearea_cultivation~dta_bal$recipient=="couple"))
summary(lm(dta_bal$maizeage~dta_bal$recipient=="couple"))
summary(lm(as.numeric(dta_bal$maizeeduc>2)~dta_bal$recipient=="couple"))
summary(lm(dta_bal$maizehh_no~dta_bal$recipient=="couple"))
summary(lm(dta_bal$maizeprrooms~dta_bal$recipient=="couple"))
summary(lm(as.numeric(dta_bal$maizeprinfo_receiv=="Yes")~dta_bal$recipient=="couple"))
summary(lm(as.numeric(dta_bal$maizeprinfo_receiv_spouse=="Yes")~dta_bal$recipient=="couple"))
summary(lm(as.numeric(dta_bal$maizeprinput_use=="Yes")~dta_bal$recipient=="couple"))
summary(lm(dta_bal$maizedist_shop~dta_bal$recipient=="couple"))

dta_bal$yield <- dta_bal$maizebags_harv*100/dta_bal$maizearea_cultivation
summary(lm((dta_bal$recipient=="couple")~yield + maizeage + as.numeric(maizeeduc>2) + maizehh_no + maizeprrooms + as.numeric(maizeprinfo_receiv=="Yes") + as.numeric(maizeprinfo_receiv_spouse=="Yes") + as.numeric(maizeprinput_use=="Yes") + maizedist_shop, data=dta_bal))


#### make sure to balance before doing tests
## sample size for balance H0 -  basically table(dta$recipient[dta$maizevideo_shown == "couple"])
s_h0 <- min(table(dta$maizevideo_shown, dta$recipient)[1,])


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






summary(lm(dta_bal$maizebags_harv*100/dta_bal$maizearea_cultivation~dta_bal$maizevideo_shown=="couple"))
summary(lm(dta_bal$maizeage~dta_bal$maizevideo_shown=="couple"))
summary(lm(as.numeric(dta_bal$maizeeduc>2)~dta_bal$maizevideo_shown=="couple"))
summary(lm(dta_bal$maizehh_no~dta_bal$maizevideo_shown=="couple"))
summary(lm(dta_bal$maizeprrooms~dta_bal$maizevideo_shown=="couple"))
summary(lm(as.numeric(dta_bal$maizeprinfo_receiv=="Yes")~dta_bal$maizevideo_shown=="couple"))
summary(lm(as.numeric(dta_bal$maizeprinfo_receiv_spouse=="Yes")~dta_bal$maizevideo_shown=="couple"))
summary(lm(as.numeric(dta_bal$maizeprinput_use=="Yes")~dta_bal$maizevideo_shown=="couple"))
summary(lm(dta_bal$maizedist_shop~dta_bal$maizevideo_shown=="couple"))

dta_bal$yield <- dta_bal$maizebags_harv*100/dta_bal$maizearea_cultivation
summary(lm((maizevideo_shown=="couple")~yield + maizeage + as.numeric(maizeeduc>2) + maizehh_no + maizeprrooms + as.numeric(maizeprinfo_receiv=="Yes") + as.numeric(maizeprinfo_receiv_spouse=="Yes") + as.numeric(maizeprinput_use=="Yes") + maizedist_shop, data=dta_bal))


### balance for gender matching hypothesis (H4)

dta <- subset(dta, recipient != "couple" & maizevideo_shown != "couple")
s_h0 <- min(table(dta$recipient, dta$maizevideo_shown))

dta_bal <- rbind(
 dta[dta$maizevideo_shown=="female" & dta$recipient == "male",][sample( nrow(dta[dta$maizevideo_shown=="female" & dta$recipient == "male",]),s_h0),],
 dta[dta$maizevideo_shown=="female" & dta$recipient == "female",][sample( nrow(dta[dta$maizevideo_shown=="female" & dta$recipient == "female",]),s_h0),],
 dta[dta$maizevideo_shown=="male" & dta$recipient == "male",][sample( nrow(dta[dta$maizevideo_shown=="male" & dta$recipient == "male",]),s_h0),],
 dta[dta$maizevideo_shown=="male" & dta$recipient == "female",][sample( nrow(dta[dta$maizevideo_shown=="male" & dta$recipient == "female",]),s_h0),])


dta_bal$matched <- NA
dta_bal$matched[dta_bal$recipient == "male" & dta_bal$maizevideo_shown=="male" | dta_bal$recipient == "female" & dta_bal$maizevideo_shown=="female"] <- TRUE
dta_bal$matched[dta_bal$recipient == "male" & dta_bal$maizevideo_shown=="female" | dta_bal$recipient == "female" & dta_bal$maizevideo_shown=="male"] <- FALSE


summary(lm(dta_bal$maizebags_harv*100/dta_bal$maizearea_cultivation~dta_bal$matched))
summary(lm(dta_bal$maizeage~dta_bal$matched))
summary(lm(as.numeric(dta_bal$maizeeduc>2)~dta_bal$matched))
summary(lm(dta_bal$maizehh_no~dta_bal$matched))
summary(lm(dta_bal$maizeprrooms~dta_bal$matched))
summary(lm(as.numeric(dta_bal$maizeprinfo_receiv=="Yes")~dta_bal$matched))
summary(lm(as.numeric(dta_bal$maizeprinfo_receiv_spouse=="Yes")~dta_bal$matched))
summary(lm(as.numeric(dta_bal$maizeprinput_use=="Yes")~dta_bal$matched))
summary(lm(dta_bal$maizedist_shop~dta_bal$matched))

dta_bal$yield <- dta_bal$maizebags_harv*100/dta_bal$maizearea_cultivation
summary(lm(matched~yield + maizeage + as.numeric(maizeeduc>2) + maizehh_no + maizeprrooms + as.numeric(maizeprinfo_receiv=="Yes") + as.numeric(maizeprinfo_receiv_spouse=="Yes") + as.numeric(maizeprinput_use=="Yes") + maizedist_shop, data=dta_bal))


