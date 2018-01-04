library(foreign)
library(ggplot2)
dta <- read.dta("/home/bjvca/data/projects/digital green/baseline/DLEC.dta")
dta$know_space <- dta$maizeoptimal_spacing == "a"
dta$know_small <- dta$maizeq22 == "c"
dta$know_weed <- dta$maizeq23 == "b"
siglev <-  1.96
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

###  baseline

means <- tapply(dta_bal$know_space,dta_bal$maizevideo_shown!="ctrl", FUN=mean)
time1 <- data.frame(tapply(dta_bal$know_space,dta_bal$maizevideo_shown!="ctrl", FUN=mean))
names(time1) <- "mean"
time1$group <- rownames(time1)
time1$up <- means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
time1$down <- means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
time1$time <- 1

means <- tapply(dta_bal$know_small,dta_bal$maizevideo_shown!="ctrl", FUN=mean)
small1 <- data.frame(tapply(dta_bal$know_small,dta_bal$maizevideo_shown!="ctrl", FUN=mean))
names(small1) <- "mean"
small1$group <- rownames(small1)
small1$up <- means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
small1$down <- means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
small1$time <- 1

means <- tapply(dta_bal$know_weed,dta_bal$maizevideo_shown!="ctrl", FUN=mean)
weed1 <- data.frame(tapply(dta_bal$know_weed,dta_bal$maizevideo_shown!="ctrl", FUN=mean))
names(weed1) <- "mean"
weed1$group <- rownames(small1)
weed1$up <- means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
weed1$down <- means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
weed1$time <- 1


### now for midline
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



###midline

means <- tapply(dta_bal$know_space,dta_bal$messenger!="ctrl", FUN=mean)

time2 <- data.frame(tapply(dta_bal$know_space,dta_bal$messenger!="ctrl", FUN=mean))
names(time2) <- "mean"
time2$group <- rownames(time2)
time2$up <- means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
time2$down <- means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
time2$time <- 2

means <- tapply(dta_bal$know_small,dta_bal$messenger!="ctrl", FUN=mean)
small2 <- data.frame(tapply(dta_bal$know_small,dta_bal$messenger!="ctrl", FUN=mean))
names(small2) <- "mean"
small2$group <- rownames(small2)
small2$up <- means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
small2$down <- means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
small2$time <- 2

means <- tapply(dta_bal$know_weed,dta_bal$messenger!="ctrl", FUN=mean)
weed2 <- data.frame(tapply(dta_bal$know_weed,dta_bal$messenger!="ctrl", FUN=mean))
names(weed2) <- "mean"
weed2$group <- rownames(weed2)
weed2$up <- means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
weed2$down <- means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
weed2$time <- 2

all <- rbind(time1, time2)
small <- rbind(small1,small2)
weed <- rbind(weed1,weed2)

all$time <- as.factor(all$time)
small$time <- as.factor(all$time)
weed$time <- as.factor(weed$time)

all$group <- as.factor(all$group) 
levels(all$group) <- c("Ctrl","Treat")
small$group <- as.factor(small$group) 
levels(small$group) <- c("Ctrl","Treat")

weed$group <- as.factor(weed$group) 
levels(weed$group) <- c("Ctrl","Treat")


p1 <- ggplot(all, aes(x=time, y=mean, group=group, color=group)) + 
  geom_line() +
  geom_point()+
  geom_pointrange(aes(ymin=up, ymax=down))+ ylab("proportion of correct answers") + ggtitle("planting", subtitle = NULL)+ theme(plot.title = element_text(hjust = 0.5))


p2 <- ggplot(small, aes(x=time, y=mean, group=group, color=group)) + 
  geom_line() +
  geom_point()+
  geom_pointrange(aes(ymin=up, ymax=down))+ ylab("proportion of correct answers")+ ggtitle("combine", subtitle = NULL)+ theme(plot.title = element_text(hjust = 0.5))


p3 <- ggplot(weed, aes(x=time, y=mean, group=group, color=group)) + 
  geom_line() +
  geom_point()+
  geom_pointrange(aes(ymin=up, ymax=down))+ ylab("proportion of correct answers")+ ggtitle("weeding", subtitle = NULL) + theme(plot.title = element_text(hjust = 0.5))

require(gridExtra)
pdf("/home/bjvca/data/projects/digital green/papers/ICAE/h0_all.pdf")
grid.arrange(p1, p2, p3, ncol=3)
dev.off()
########################### H1 #####################################
rm(list=ls())
library(foreign)
dta <- read.dta("/home/bjvca/data/projects/digital green/baseline/DLEC.dta")
dta$know_space <- dta$maizeoptimal_spacing == "a"
dta$know_small <- dta$maizeq22 == "c"
dta$know_weed <- dta$maizeq23 == "b"
siglev <-  1.96
### drop the femheaded
dta <- subset(dta, recipient != "n/a")

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


means <- tapply(dta_bal$know_space,dta_bal$recipient=="couple", FUN=mean)

###  baseline

means <-  tapply(dta_bal$know_space,dta_bal$recipient=="couple", FUN=mean)
time1 <- data.frame( tapply(dta_bal$know_space,dta_bal$recipient=="couple", FUN=mean))
names(time1) <- "mean"
time1$group <- rownames(time1)
time1$up <- means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
time1$down <- means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
time1$time <- 1

means <- tapply(dta_bal$know_small,dta_bal$recipient=="couple", FUN=mean)
small1 <- data.frame(tapply(dta_bal$know_small,dta_bal$recipient=="couple", FUN=mean))
names(small1) <- "mean"
small1$group <- rownames(small1)
small1$up <- means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
small1$down <- means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
small1$time <- 1

means <- tapply(dta_bal$know_weed,dta_bal$recipient=="couple", FUN=mean)
weed1 <- data.frame(tapply(dta_bal$know_weed,dta_bal$recipient=="couple", FUN=mean))
names(weed1) <- "mean"
weed1$group <- rownames(small1)
weed1$up <- means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
weed1$down <- means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
weed1$time <- 1

library(foreign)
dta <- read.dta("/home/bjvca/data/projects/digital green/midline/DLECvisit2v1.dta")
dta$know_space <- dta$hhvideomaizeoptimal_spacing == "a"
dta$know_small <- dta$hhvideomaizeq22 == "c"
dta$know_weed <- dta$hhvideomaizeq23 == "b"
siglev <-  1.96
### drop the femheaded
dta <- subset(dta, recipient != "")


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


means <- tapply(dta_bal$know_space,dta_bal$recipient=="couple", FUN=mean)

means <- tapply(dta_bal$know_space,dta_bal$recipient=="couple", FUN=mean)

time2 <- data.frame(tapply(dta_bal$know_space,dta_bal$recipient=="couple", FUN=mean))
names(time2) <- "mean"
time2$group <- rownames(time2)
time2$up <- means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
time2$down <- means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
time2$time <- 2

means <- tapply(dta_bal$know_small,dta_bal$recipient=="couple", FUN=mean)
small2 <- data.frame(tapply(dta_bal$know_small,dta_bal$recipient=="couple", FUN=mean))
names(small2) <- "mean"
small2$group <- rownames(small2)
small2$up <- means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
small2$down <- means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
small2$time <- 2

means <- tapply(dta_bal$know_weed,dta_bal$recipient=="couple", FUN=mean)
weed2 <- data.frame(tapply(dta_bal$know_weed,dta_bal$recipient=="couple", FUN=mean))
names(weed2) <- "mean"
weed2$group <- rownames(weed2)
weed2$up <- means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
weed2$down <- means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
weed2$time <- 2

all <- rbind(time1, time2)
small <- rbind(small1,small2)
weed <- rbind(weed1,weed2)

all$time <- as.factor(all$time)
small$time <- as.factor(all$time)
weed$time <- as.factor(weed$time)

all$group <- as.factor(all$group) 
levels(all$group) <- c("Ctrl","Treat")
small$group <- as.factor(small$group) 
levels(small$group) <- c("Ctrl","Treat")

weed$group <- as.factor(weed$group) 
levels(weed$group) <- c("Ctrl","Treat")


p1 <- ggplot(all, aes(x=time, y=mean, group=group, color=group)) + 
  geom_line() +
  geom_point()+
  geom_pointrange(aes(ymin=up, ymax=down))+ ylab("proportion of correct answers") + ggtitle("planting", subtitle = NULL)+ theme(plot.title = element_text(hjust = 0.5))


p2 <- ggplot(small, aes(x=time, y=mean, group=group, color=group)) + 
  geom_line() +
  geom_point()+
  geom_pointrange(aes(ymin=up, ymax=down))+ ylab("proportion of correct answers")+ ggtitle("combine", subtitle = NULL)+ theme(plot.title = element_text(hjust = 0.5))


p3 <- ggplot(weed, aes(x=time, y=mean, group=group, color=group)) + 
  geom_line() +
  geom_point()+
  geom_pointrange(aes(ymin=up, ymax=down))+ ylab("proportion of correct answers")+ ggtitle("weeding", subtitle = NULL) + theme(plot.title = element_text(hjust = 0.5))

require(gridExtra)
pdf("/home/bjvca/data/projects/digital green/papers/ICAE/h1_all.pdf")
grid.arrange(p1, p2, p3, ncol=3)
dev.off()

########################### H2 #####################################

rm(list=ls())
library(foreign)
dta <- read.dta("/home/bjvca/data/projects/digital green/baseline/DLEC.dta")
dta$know_space <- dta$maizeoptimal_spacing == "a"
dta$know_small <- dta$maizeq22 == "c"
dta$know_weed <- dta$maizeq23 == "b"
siglev <-  1.96
### drop the femheaded
dta <- subset(dta, recipient != "n/a")

### drop control here, we are now only interested in comparing treatments
dta <- subset(dta, maizevideo_shown != "ctrl")


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




means <- tapply(dta_bal$know_space,dta_bal$maizevideo_shown=="couple", FUN=mean)

###  baseline

means <-  tapply(dta_bal$know_space,dta_bal$maizevideo_shown=="couple", FUN=mean)
time1 <- data.frame( tapply(dta_bal$know_space,dta_bal$maizevideo_shown=="couple", FUN=mean))
names(time1) <- "mean"
time1$group <- rownames(time1)
time1$up <- means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
time1$down <- means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
time1$time <- 1

means <- tapply(dta_bal$know_small,dta_bal$maizevideo_shown=="couple", FUN=mean)
small1 <- data.frame(tapply(dta_bal$know_small,dta_bal$maizevideo_shown=="couple", FUN=mean))
names(small1) <- "mean"
small1$group <- rownames(small1)
small1$up <- means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
small1$down <- means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
small1$time <- 1

means <- tapply(dta_bal$know_weed,dta_bal$maizevideo_shown=="couple", FUN=mean)
weed1 <- data.frame(tapply(dta_bal$know_weed,dta_bal$maizevideo_shown=="couple", FUN=mean))
names(weed1) <- "mean"
weed1$group <- rownames(small1)
weed1$up <- means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
weed1$down <- means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
weed1$time <- 1


###
dta <- read.dta("/home/bjvca/data/projects/digital green/midline/DLECvisit2v1.dta")
dta$know_space <- dta$hhvideomaizeoptimal_spacing == "a"
dta$know_small <- dta$hhvideomaizeq22 == "c"
dta$know_weed <- dta$hhvideomaizeq23 == "b"
siglev <-  1.96
### drop the femheaded
dta <- subset(dta, recipient != "")

dta <- subset(dta, messenger != "ctrl")

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




means <- tapply(dta_bal$know_space,dta_bal$messenger=="couple", FUN=mean)

time2 <- data.frame(tapply(dta_bal$know_space,dta_bal$messenger=="couple", FUN=mean))
names(time2) <- "mean"
time2$group <- rownames(time2)
time2$up <- means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
time2$down <- means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
time2$time <- 2

means <- tapply(dta_bal$know_small,dta_bal$messenger=="couple", FUN=mean)
small2 <- data.frame(tapply(dta_bal$know_small,dta_bal$messenger=="couple", FUN=mean))
names(small2) <- "mean"
small2$group <- rownames(small2)
small2$up <- means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
small2$down <- means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
small2$time <- 2

means <- tapply(dta_bal$know_weed,dta_bal$messenger=="couple", FUN=mean)
weed2 <- data.frame(tapply(dta_bal$know_weed,dta_bal$messenger=="couple", FUN=mean))
names(weed2) <- "mean"
weed2$group <- rownames(weed2)
weed2$up <- means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
weed2$down <- means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
weed2$time <- 2

all <- rbind(time1, time2)
small <- rbind(small1,small2)
weed <- rbind(weed1,weed2)

all$time <- as.factor(all$time)
small$time <- as.factor(all$time)
weed$time <- as.factor(weed$time)

all$group <- as.factor(all$group) 
levels(all$group) <- c("Ctrl","Treat")
small$group <- as.factor(small$group) 
levels(small$group) <- c("Ctrl","Treat")

weed$group <- as.factor(weed$group) 
levels(weed$group) <- c("Ctrl","Treat")


p1 <- ggplot(all, aes(x=time, y=mean, group=group, color=group)) + 
  geom_line() +
  geom_point()+
  geom_pointrange(aes(ymin=up, ymax=down))+ ylab("proportion of correct answers") + ggtitle("planting", subtitle = NULL)+ theme(plot.title = element_text(hjust = 0.5))


p2 <- ggplot(small, aes(x=time, y=mean, group=group, color=group)) + 
  geom_line() +
  geom_point()+
  geom_pointrange(aes(ymin=up, ymax=down))+ ylab("proportion of correct answers")+ ggtitle("combine", subtitle = NULL)+ theme(plot.title = element_text(hjust = 0.5))


p3 <- ggplot(weed, aes(x=time, y=mean, group=group, color=group)) + 
  geom_line() +
  geom_point()+
  geom_pointrange(aes(ymin=up, ymax=down))+ ylab("proportion of correct answers")+ ggtitle("weeding", subtitle = NULL) + theme(plot.title = element_text(hjust = 0.5))

require(gridExtra)
pdf("/home/bjvca/data/projects/digital green/papers/ICAE/h2_all.pdf")
grid.arrange(p1, p2, p3, ncol=3)
dev.off()

########################### H3 #####################################

rm(list=ls())
library(foreign)
dta <- read.dta("/home/bjvca/data/projects/digital green/baseline/DLEC.dta")
dta$know_space <- dta$maizeoptimal_spacing == "a"
dta$know_small <- dta$maizeq22 == "c"
dta$know_weed <- dta$maizeq23 == "b"
siglev <-  1.96
### drop the femheaded
dta <- subset(dta, recipient != "n/a")

### drop control here, we are now only interested in comparing treatments
dta <- subset(dta, maizevideo_shown != "ctrl")

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



means <- tapply(dta$know_space,  dta$matched, FUN=mean)

means <- tapply(dta$know_space,  dta$matched, FUN=mean)

###  baseline

means <-  tapply(dta$know_space,  dta$matched, FUN=mean)
time1 <- data.frame(tapply(dta$know_space,  dta$matched, FUN=mean))
names(time1) <- "mean"
time1$group <- rownames(time1)
time1$up <- means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
time1$down <- means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
time1$time <- 1

means <-  tapply(dta$know_small,  dta$matched, FUN=mean)
small1 <- data.frame( tapply(dta$know_small,  dta$matched, FUN=mean))
names(small1) <- "mean"
small1$group <- rownames(small1)
small1$up <- means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
small1$down <- means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
small1$time <- 1

means <-  tapply(dta$know_weed,  dta$matched, FUN=mean)
weed1 <- data.frame( tapply(dta$know_weed,  dta$matched, FUN=mean))
names(weed1) <- "mean"
weed1$group <- rownames(small1)
weed1$up <- means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
weed1$down <- means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
weed1$time <- 1



###
dta <- read.dta("/home/bjvca/data/projects/digital green/midline/DLECvisit2v1.dta")
dta$know_space <- dta$hhvideomaizeoptimal_spacing == "a"
dta$know_small <- dta$hhvideomaizeq22 == "c"
dta$know_weed <- dta$hhvideomaizeq23 == "b"
siglev <-  1.96
### drop the femheaded
dta <- subset(dta, recipient != "")

dta <- subset(dta, messenger != "ctrl")

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


means <- tapply(dta_bal$know_space,  dta_bal$matched, FUN=mean)


time2 <- data.frame(tapply(dta_bal$know_space,dta_bal$matched, FUN=mean))
names(time2) <- "mean"
time2$group <- rownames(time2)
time2$up <- means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
time2$down <- means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
time2$time <- 2

means <- tapply(dta_bal$know_small,dta_bal$matched, FUN=mean)
small2 <- data.frame(tapply(dta_bal$know_small,dta_bal$matched, FUN=mean))
names(small2) <- "mean"
small2$group <- rownames(small2)
small2$up <- means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
small2$down <- means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
small2$time <- 2

means <- tapply(dta_bal$know_weed,dta_bal$matched, FUN=mean)
weed2 <- data.frame(tapply(dta_bal$know_weed,dta_bal$matched, FUN=mean))
names(weed2) <- "mean"
weed2$group <- rownames(weed2)
weed2$up <- means +  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
weed2$down <- means -  siglev*sqrt(means*(1-means)/dim(dta_bal)[1])
weed2$time <- 2

all <- rbind(time1, time2)
small <- rbind(small1,small2)
weed <- rbind(weed1,weed2)

all$time <- as.factor(all$time)
small$time <- as.factor(all$time)
weed$time <- as.factor(weed$time)

all$group <- as.factor(all$group) 
levels(all$group) <- c("Ctrl","Treat")
small$group <- as.factor(small$group) 
levels(small$group) <- c("Ctrl","Treat")

weed$group <- as.factor(weed$group) 
levels(weed$group) <- c("Ctrl","Treat")


p1 <- ggplot(all, aes(x=time, y=mean, group=group, color=group)) + 
  geom_line() +
  geom_point()+
  geom_pointrange(aes(ymin=up, ymax=down))+ ylab("proportion of correct answers") + ggtitle("planting", subtitle = NULL)+ theme(plot.title = element_text(hjust = 0.5))


p2 <- ggplot(small, aes(x=time, y=mean, group=group, color=group)) + 
  geom_line() +
  geom_point()+
  geom_pointrange(aes(ymin=up, ymax=down))+ ylab("proportion of correct answers")+ ggtitle("combine", subtitle = NULL)+ theme(plot.title = element_text(hjust = 0.5))


p3 <- ggplot(weed, aes(x=time, y=mean, group=group, color=group)) + 
  geom_line() +
  geom_point()+
  geom_pointrange(aes(ymin=up, ymax=down))+ ylab("proportion of correct answers")+ ggtitle("weeding", subtitle = NULL) + theme(plot.title = element_text(hjust = 0.5))

require(gridExtra)
pdf("/home/bjvca/data/projects/digital green/papers/ICAE/h3_all.pdf")
grid.arrange(p1, p2, p3, ncol=3)
dev.off()

