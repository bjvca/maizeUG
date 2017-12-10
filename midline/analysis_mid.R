library(foreign)
dta <- read.dta("/home/bjvca/data/projects/digital green/midline/DLECvisit2v1.dta")
dta$know_space <- dta$hhvideomaizeoptimal_spacing == "a"
dta$know_small <- dta$hhvideomaizeq22 == "c"
dta$know_weed <- dta$hhvideomaizeq23 == "b"
siglev <-  1.96
### drop the femheaded
dta <- subset(dta, recipient != "")


### does it work?
#t.test(dta$know_space~dta$messenger!="ctrl")
 prop.test(t(table(dta$know_space, dta$messenger!="ctrl")))

#t.test(dta$know_small~dta$messenger!="ctrl")
 prop.test(t(table(dta$know_small, dta$messenger!="ctrl")))
#t.test(dta$know_weed~dta$messenger!="ctrl")
 prop.test(t(table(dta$know_weed, dta$messenger!="ctrl")))
## sample size for balance H0 - no effect
##just leave all controls in there


## sample size for balance H1
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

prop.test(t(table(dta_bal$know_space, dta_bal$messenger!="ctrl")))
prop.test(t(table(dta_bal$know_small, dta_bal$messenger!="ctrl")))
prop.test(t(table(dta_bal$know_weed, dta_bal$messenger!="ctrl")))




pdf("/home/bjvca/data/projects/digital green/midline/videoworks_mid.pdf")
par(mfrow=c(1,3))
siglev <-  1.96
means <- tapply(dta_bal$know_space,dta_bal$messenger!="ctrl", FUN=mean)
barCenters <- barplot(tapply(dta_bal$know_space,dta_bal$messenger!="ctrl", FUN=mean), ylim=c(0,1), xpd = FALSE, main="Spacing",xlab="Watched Video", names.arg=c("CTRL","TREAT"), ylab="proportion of correct answers")

segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
         means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5)

arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
       means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

means <- tapply(dta_bal$know_small,dta_bal$messenger!="ctrl", FUN=mean)
barCenters <- barplot(tapply(dta_bal$know_small,dta_bal$messenger!="ctrl", FUN=mean), ylim=c(0,1), xpd = FALSE, main="Combine",xlab="Watched Video", names.arg=c("CTRL","TREAT"), ylab="proportion of correct answers")

segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
         means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5)

arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
       means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

means <- tapply(dta_bal$know_weed,dta_bal$messenger!="ctrl", FUN=mean)
barCenters <- barplot(tapply(dta_bal$know_weed,dta_bal$messenger!="ctrl", FUN=mean), ylim=c(0,1), xpd = FALSE, main="Weeding",xlab="Watched Video", names.arg=c("CTRL","TREAT"), ylab="proportion of correct answers")

segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
         means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5)

arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
       means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
dev.off()

## remove the control, we are now comparing betweeen treatments

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

### info assymmetry effect

prop.test(t(table(dta$know_space, dta$recipient=="couple")))
prop.test(t(table(dta$know_small, dta$recipient=="couple")))
prop.test(t(table(dta$know_weed, dta$recipient=="couple")))

prop.test(t(table(dta_bal$know_space, dta_bal$recipient=="couple")))
prop.test(t(table(dta_bal$know_small, dta_bal$recipient=="couple")))
prop.test(t(table(dta_bal$know_weed, dta_bal$recipient=="couple")))

pdf("/home/bjvca/data/projects/digital green/midline/recipient_mid.pdf")
par(mfrow=c(1,3))
means <- tapply(dta_bal$know_space,dta_bal$recipient=="couple", FUN=mean)
barCenters <- barplot(tapply(dta_bal$know_space,dta_bal$recipient=="couple", FUN=mean), ylim=c(0,1), xpd = FALSE, main="Spacing",xlab="Audience", names.arg=c("INDIV","COUPLE"), ylab="proportion of correct answers")

segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
         means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5)

arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
       means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

means <- tapply(dta_bal$know_small,dta_bal$recipient=="couple", FUN=mean)
barCenters <- barplot(tapply(dta_bal$know_small,dta_bal$recipient=="couple", FUN=mean), ylim=c(0,1), xpd = FALSE, main="Combine",xlab="Audience", names.arg=c("INDIV","COUPLE"), ylab="proportion of correct answers")

segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
         means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5)

arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
       means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
### but this one is sig at 10 percent siglev <-  1.64
means <- tapply(dta_bal$know_weed,dta_bal$recipient=="couple", FUN=mean)
barCenters <- barplot(tapply(dta_bal$know_weed,dta_bal$recipient=="couple", FUN=mean), ylim=c(0,1), xpd = FALSE, main="Weeding",xlab="Audience", names.arg=c("INDIV","COUPLE"), ylab="proportion of correct answers")

segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
         means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5)

arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
       means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
dev.off()

### projecting a household approach
#t.test(dta$know_space~dta$messenger=="couple")
#t.test(dta$know_small~dta$messenger=="couple")
#t.test(dta$know_weed~dta$messenger=="couple")

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

prop.test(t(table(dta$know_space, dta$messenger=="couple")))
prop.test(t(table(dta_bal$know_space, dta_bal$messenger=="couple")))
prop.test(t(table(dta$know_small, dta$messenger=="couple")))
prop.test(t(table(dta_bal$know_small, dta_bal$messenger=="couple")))
prop.test(t(table(dta$know_weed, dta$messenger=="couple")))
prop.test(t(table(dta_bal$know_weed, dta_bal$messenger=="couple")))


pdf("/home/bjvca/data/projects/digital green/midline/audience_mid.pdf")
par(mfrow=c(1,3))
means <- tapply(dta_bal$know_space,dta_bal$messenger=="couple", FUN=mean)
barCenters <- barplot(tapply(dta_bal$know_space,dta_bal$messenger=="couple", FUN=mean), ylim=c(0,1), xpd = FALSE, main="Spacing",xlab="Messenger", names.arg=c("INDIV","COUPLE"), ylab="proportion of correct answers")

segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
         means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5)

arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
       means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

means <- tapply(dta_bal$know_small,dta_bal$messenger=="couple", FUN=mean)
barCenters <- barplot(tapply(dta_bal$know_small,dta_bal$messenger=="couple", FUN=mean), ylim=c(0,1), xpd = FALSE, main="Combine",xlab="Messenger", names.arg=c("INDIV","COUPLE"), ylab="proportion of correct answers")

segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
         means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5)

arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
       means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
### but this one is sig at 10 percent siglev <-  1.64
means <- tapply(dta_bal$know_weed,dta_bal$messenger=="couple", FUN=mean)
barCenters <- barplot(tapply(dta_bal$know_weed,dta_bal$messenger=="couple", FUN=mean), ylim=c(0,1), xpd = FALSE, main="Weeding",xlab="Messenger", names.arg=c("INDIV","COUPLE"), ylab="proportion of correct answers")

segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
         means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5)

arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
       means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

dev.off()


### gender matching

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



prop.test(t(table(dta$know_space, dta$matched)))
prop.test(t(table(dta_bal$know_space, dta_bal$matched)))

prop.test(t(table(dta$know_small, dta$matched)))
prop.test(t(table(dta_bal$know_small, dta_bal$matched)))

prop.test(t(table(dta$know_weed, dta$matched)))
prop.test(t(table(dta_bal$know_weed, dta_bal$matched)))

pdf("/home/bjvca/data/projects/digital green/midline/matched_mid.pdf")
par(mfrow=c(1,3))
means <- tapply(dta_bal$know_space,  dta_bal$matched, FUN=mean)
barCenters <- barplot(tapply(dta_bal$know_space, dta_bal$matched, FUN=mean), ylim=c(0,1), xpd = FALSE, main="Spacing",xlab="gender-matching", names.arg=c("MIXED","MATCHED"), ylab="proportion of correct answers")

segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
         means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5)

arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
       means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

means <- tapply(dta_bal$know_small, dta_bal$matched, FUN=mean)
barCenters <- barplot(tapply(dta_bal$know_small, dta_bal$matched, FUN=mean), ylim=c(0,1), xpd = FALSE, main="Combine",xlab="gender-matching", names.arg=c("MIXED","MATCHED"), ylab="proportion of correct answers")

segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
         means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5)

arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
       means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
### but this one is sig at 10 percent siglev <-  1.64
means <- tapply(dta_bal$know_weed,dta_bal$matched, FUN=mean)
barCenters <- barplot(tapply(dta_bal$know_weed,dta_bal$matched, FUN=mean), ylim=c(0,1), xpd = FALSE, main="Weeding",xlab="gender-matching", names.arg=c("MIXED","MATCHED"), ylab="proportion of correct answers")

segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
         means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5)

arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), barCenters,
       means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1]), lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
dev.off()





