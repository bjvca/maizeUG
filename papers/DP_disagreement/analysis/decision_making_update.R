library(clubSandwich)
library(ggpubr)
dta <- read.csv("/home/bjvca/data/projects/digital green/papers/DP_disagreement/endline_dta.csv")
dta <- subset(dta, interview_status == "couple interviewed")

### decision making graphs 

df_b <- data.frame(NA)
dec <- c("mgt","dectime", "decspace","decstriga", "decweed")
for (i in 1:length(dec)) {
d <- dta[c("hhid","messenger",paste(paste(dec[i],"both_woman_pl",sep="_") ,1:5,sep=""))]
dta_long <- reshape(d, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"both",sep="_") , idvar = "hhid")
df_b <- data.frame(rbind(df_b,mean(dta_long[,4], na.rm=T)))
}


df_m <- data.frame(NA)
dec <- c("mgt","dectime", "decspace","decstriga", "decweed")
for (i in 1:length(dec)) {
d <- dta[c("hhid","messenger",paste(paste(dec[i],"man_woman_pl",sep="_") ,1:5,sep=""))]
dta_long <- reshape(d, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"man_woman",sep="_") , idvar = "hhid")
df_m <- data.frame(rbind(df_m,mean(dta_long[,4], na.rm=T)))
}

df_w <- data.frame(NA)
dec <- c("mgt","dectime", "decspace","decstriga", "decweed")
for (i in 1:length(dec)) {
d <- dta[c("hhid","messenger",paste(paste(dec[i],"woman_pl",sep="_") ,1:5,sep=""))]
dta_long <- reshape(d, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"woman",sep="_") , idvar = "hhid")
df_w <- data.frame(rbind(df_w,mean(dta_long[,4], na.rm=T)))
}



df <-cbind(df_b,df_m,df_w)[2:6,]
rownames(df) <- c("plant maize","timing","spacing","striga","weeding")

names(df) <- c("activity","share","decsion_maker")
df$share <- as.numeric(as.character(df$share))
colours <- c("#CCCCCC", "#6E8DAB", "#104E8B")

png("/home/bjvca/data/projects/digital green/papers/DP_disagreement/results/decsion_activities_update_woman.png", units="px", height=3200, width= 4200, res=600)

barplot(as.matrix(t(df)), main="", ylab = "share of plots", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours, ylim = c(0,.8))
legend("topleft", c("spouses thogether","husband alone","wife alone"), cex=1.3, bty="n", fill=colours)
dev.off()

df_b <- data.frame(NA)
dec <- c("mgt","dectime", "decspace","decstriga", "decweed")
for (i in 1:length(dec)) {
d <- dta[c("hhid","messenger",paste(paste(dec[i],"both_man_pl",sep="_") ,1:5,sep=""))]
dta_long <- reshape(d, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"both",sep="_") , idvar = "hhid")
df_b <- data.frame(rbind(df_b,mean(dta_long[,4], na.rm=T)))
}


df_m <- data.frame(NA)
dec <- c("mgt","dectime", "decspace","decstriga", "decweed")
for (i in 1:length(dec)) {
d <- dta[c("hhid","messenger",paste(paste(dec[i],"man_pl",sep="_") ,1:5,sep=""))]
dta_long <- reshape(d, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"both",sep="_") , idvar = "hhid")
df_m <- data.frame(rbind(df_m,mean(dta_long[,4], na.rm=T)))
}

df_w <- data.frame(NA)
dec <- c("mgt","dectime", "decspace","decstriga", "decweed")
for (i in 1:length(dec)) {
d <- dta[c("hhid","messenger",paste(paste(dec[i],"woman_man_pl",sep="_") ,1:5,sep=""))]
dta_long <- reshape(d, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"both",sep="_") , idvar = "hhid")
df_w <- data.frame(rbind(df_w,mean(dta_long[,4], na.rm=T)))
}



df <-cbind(df_b,df_m,df_w)[2:6,]
rownames(df) <- c("plant maize","timing","spacing","striga","weeding")

names(df) <- c("activity","share","decsion_maker")
df$share <- as.numeric(as.character(df$share))
colours <- c("#CCCCCC", "#6E8DAB", "#104E8B")

png("/home/bjvca/data/projects/digital green/papers/DP_disagreement/results/decsion_activities_update_man.png", units="px", height=3200, width= 4200, res=600)

barplot(as.matrix(t(df)), main="", ylab = "share of plots", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours, ylim = c(0,.8))
legend("topleft", c("spouses thogether","husband alone","wife alone"), cex=1.3, bty="n", fill=colours)
dev.off()

### graphs for joint decsion making and secrecy in decsion making
df_b <- data.frame(NA)
df_w <- data.frame(NA)
df_m <- data.frame(NA)
dec <- c("mgt","dectime", "decspace","decstriga", "decweed")
df_ols_behind_back <- array(NA,dim=c(6,3,length(dec)))
for (i in 1:length(dec)) {
dm <- dta[c("hhid","messenger",paste(paste(dec[i],"both_man_pl",sep="_") ,1:5,sep=""))]
dw <- dta[c("hhid","messenger",paste(paste(dec[i],"both_woman_pl",sep="_") ,1:5,sep=""))]

dta_long_m <- reshape(dm, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"both_man",sep="_") , idvar = "hhid")
dta_long_w <- reshape(dw, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"both_woman",sep="_") , idvar = "hhid")
dta_long <- merge(dta_long_m, dta_long_w)
dta_long$differ <- NA
dta_long$differ <- unlist((dta_long[paste(dec[i],"both_man",sep="_")] ==1 & dta_long[paste(dec[i],"both_woman",sep="_")]==1))
df_b <- data.frame(rbind(df_b,mean(dta_long$differ, na.rm=T)))


dm <- dta[c("hhid","messenger",paste(paste(dec[i],"man_pl",sep="_") ,1:5,sep=""))]
db <- dta[c("hhid","messenger",paste(paste(dec[i],"both_man_pl",sep="_") ,1:5,sep=""))]
dw <- dta[c("hhid","messenger",paste(paste(dec[i],"woman_pl",sep="_") ,1:5,sep=""))]

dta_long_m <- reshape(dm, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"man",sep="_") , idvar = "hhid")
dta_long_b <- reshape(db, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"both_man",sep="_") , idvar = "hhid")
dta_long_w <- reshape(dw, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"woman",sep="_") , idvar = "hhid")
dta_long <- merge(merge(dta_long_m, dta_long_w), dta_long_b)
dta_long$differ <- NA
dta_long$differ <- unlist(((dta_long[paste(dec[i],"man",sep="_")] ==1 |  dta_long[paste(dec[i],"both_man",sep="_")] ==1 ) & dta_long[paste(dec[i],"woman",sep="_")]==1))

df_m <- data.frame(rbind(df_m,mean(dta_long$differ, na.rm=T)))

dm <- dta[c("hhid","messenger",paste(paste(dec[i],"man_pl",sep="_") ,1:5,sep=""))]
db <- dta[c("hhid","messenger",paste(paste(dec[i],"both_woman_pl",sep="_") ,1:5,sep=""))]
dw <- dta[c("hhid","messenger",paste(paste(dec[i],"woman_pl",sep="_") ,1:5,sep=""))]

dta_long_m <- reshape(dm, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"man",sep="_") , idvar = "hhid")
dta_long_b <- reshape(db, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"both_woman",sep="_") , idvar = "hhid")
dta_long_w <- reshape(dw, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"woman",sep="_") , idvar = "hhid")
dta_long <- merge(merge(dta_long_m, dta_long_w), dta_long_b)
dta_long$differ <- NA
dta_long$differ <- unlist(((dta_long[paste(dec[i],"woman",sep="_")] ==1 |  dta_long[paste(dec[i],"both_woman",sep="_")] ==1 ) & dta_long[paste(dec[i],"man",sep="_")]==1))

df_w <- data.frame(rbind(df_w,mean(dta_long$differ, na.rm=T)))
}



df <-cbind(df_m,df_w)[2:6,]
rownames(df) <- c("plant maize","timing","spacing","striga","weeding")

names(df) <- c("female disagreement","male disagreement")
colours <- c("#E5F5E0", "#A1D99B")

png("/home/bjvca/data/projects/digital green/papers/DP_disagreement/results/decsion_activities_joint_deception.png", units="px", height=3200, width= 4200, res=600)

barplot(as.matrix(t(df)), main="", ylab = "share of plots", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours, ylim = c(0,.25))
legend("topleft", c("woman disagrees man was involved","man disagrees women was involved"), cex=1.3, bty="n", fill=colours)
dev.off()

### labour time graph

### aggregate weeding time

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

### cleaning time data - judged by looking at man_man_pl1
dec <- c("time_weed")
for (i in 1:length(dec)) {
dta[paste(paste(dec[i],"man_man_pl",sep="_") ,1:5,sep="")] <- lapply(dta[paste(paste(dec[i],"man_man_pl",sep="_") ,1:5,sep="")], function(x) replace(x, x > 60,NA) )
dta[paste(paste(dec[i],"man_woman_pl",sep="_") ,1:5,sep="")] <- lapply(dta[paste(paste(dec[i],"man_woman_pl",sep="_") ,1:5,sep="")], function(x) replace(x, x > 60,NA) )
dta[paste(paste(dec[i],"woman_man_pl",sep="_") ,1:5,sep="")] <- lapply(dta[paste(paste(dec[i],"woman_man_pl",sep="_") ,1:5,sep="")], function(x) replace(x, x > 60,NA) )
dta[paste(paste(dec[i],"woman_woman_pl",sep="_") ,1:5,sep="")] <- lapply(dta[paste(paste(dec[i],"woman_woman_pl",sep="_") ,1:5,sep="")], function(x) replace(x, x > 60,NA) )
}

dec <- c("time_plant", "time_spray")
for (i in 1:length(dec)) {
dta[paste(paste(dec[i],"man_man_pl",sep="_") ,1:5,sep="")] <- lapply(dta[paste(paste(dec[i],"man_man_pl",sep="_") ,1:5,sep="")], function(x) replace(x, x > 10,NA) )
dta[paste(paste(dec[i],"man_woman_pl",sep="_") ,1:5,sep="")] <- lapply(dta[paste(paste(dec[i],"man_woman_pl",sep="_") ,1:5,sep="")], function(x) replace(x, x > 10,NA) )
dta[paste(paste(dec[i],"woman_man_pl",sep="_") ,1:5,sep="")] <- lapply(dta[paste(paste(dec[i],"woman_man_pl",sep="_") ,1:5,sep="")], function(x) replace(x, x > 10,NA) )
dta[paste(paste(dec[i],"woman_woman_pl",sep="_") ,1:5,sep="")] <- lapply(dta[paste(paste(dec[i],"woman_woman_pl",sep="_") ,1:5,sep="")], function(x) replace(x, x > 10,NA) )
}

dec <- c("time_harv")
for (i in 1:length(dec)) {
dta[paste(paste(dec[i],"man_man_pl",sep="_") ,1:5,sep="")] <- lapply(dta[paste(paste(dec[i],"man_man_pl",sep="_") ,1:5,sep="")], function(x) replace(x, x > 15,NA) )
dta[paste(paste(dec[i],"man_woman_pl",sep="_") ,1:5,sep="")] <- lapply(dta[paste(paste(dec[i],"man_woman_pl",sep="_") ,1:5,sep="")], function(x) replace(x, x > 15,NA) )
dta[paste(paste(dec[i],"woman_man_pl",sep="_") ,1:5,sep="")] <- lapply(dta[paste(paste(dec[i],"woman_man_pl",sep="_") ,1:5,sep="")], function(x) replace(x, x > 15,NA) )
dta[paste(paste(dec[i],"woman_woman_pl",sep="_") ,1:5,sep="")] <- lapply(dta[paste(paste(dec[i],"woman_woman_pl",sep="_") ,1:5,sep="")], function(x) replace(x, x > 15,NA) )
}



df_m <- data.frame(NA)
dec <- c("time_prep","time_plant", "time_weed","time_spray", "time_harv")
for (i in 1:length(dec)) {
d <- dta[c("hhid","messenger",paste(paste(dec[i],"man_man_pl",sep="_") ,1:5,sep=""))]
dta_long <- reshape(d, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"man_man",sep="_") , idvar = "hhid")
df_m <- data.frame(rbind(df_m,mean(dta_long[,4], na.rm=T)))
}


df_w <-  data.frame(NA)
dec <- c("time_prep","time_plant", "time_weed","time_spray", "time_harv")
for (i in 1:length(dec)) {
d <- dta[c("hhid","messenger",paste(paste(dec[i],"woman_man_pl",sep="_") ,1:5,sep=""))]
dta_long <- reshape(d, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"woman_man",sep="_") , idvar = "hhid")
df_w <- data.frame(rbind(df_w,mean(dta_long[,4], na.rm=T)))
}


df <-cbind(df_m,df_w)[2:6,]
rownames(df) <- c("prepare land","planting","weeding","spraying","harvesting")

names(df) <- c("man","woman")

colours <- c("#CCCCCC", "#6E8DAB")

png("/home/bjvca/data/projects/digital green/papers/DP_disagreement/results/labour_time_man.png", units="px", height=3200, width= 4200, res=600)

barplot(as.matrix(t(df)), main="", ylab = "days of work", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours)
legend("topleft", c("husband","wife"), cex=1.3, bty="n", fill=colours)
dev.off()


df_m <- data.frame(NA)
dec <- c("time_prep","time_plant", "time_weed","time_spray", "time_harv")
for (i in 1:length(dec)) {
d <- dta[c("hhid","messenger",paste(paste(dec[i],"man_woman_pl",sep="_") ,1:5,sep=""))]
dta_long <- reshape(d, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"man_woman",sep="_") , idvar = "hhid")
df_m <- data.frame(rbind(df_m,mean(dta_long[,4], na.rm=T)))
}


df_w <-  data.frame(NA)
dec <- c("time_prep","time_plant", "time_weed","time_spray", "time_harv")
for (i in 1:length(dec)) {
d <- dta[c("hhid","messenger",paste(paste(dec[i],"woman_woman_pl",sep="_") ,1:5,sep=""))]
dta_long <- reshape(d, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"woman_woman",sep="_") , idvar = "hhid")
df_w <- data.frame(rbind(df_w,mean(dta_long[,4], na.rm=T)))
}


df <-cbind(df_m,df_w)[2:6,]
rownames(df) <- c("prepare land","planting","weeding","spraying","harvesting")

names(df) <- c("man","woman")

colours <- c("#CCCCCC", "#6E8DAB")

png("/home/bjvca/data/projects/digital green/papers/DP_disagreement/results/labour_time_woman.png", units="px", height=3200, width= 4200, res=600)

barplot(as.matrix(t(df)), main="", ylab = "days of work", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours)
legend("topleft", c("husband","wife"), cex=1.3, bty="n", fill=colours)
dev.off()

### graphs for shirking
df_w <- data.frame(NA)
df_m <- data.frame(NA)
dec <- c("time_prep","time_plant", "time_weed","time_spray", "time_harv")

for (i in 1:length(dec)) {


dm <- dta[c("hhid","messenger",paste(paste(dec[i],"man_man_pl",sep="_") ,1:5,sep=""))]
dw <- dta[c("hhid","messenger",paste(paste(dec[i],"man_woman_pl",sep="_") ,1:5,sep=""))]

dta_long_m <- reshape(dm, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"man_man",sep="_") , idvar = "hhid")
dta_long_w <- reshape(dw, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"man_woman",sep="_") , idvar = "hhid")
dta_long <- merge(dta_long_m, dta_long_w)
dta_long$differ <- NA
dta_long$differ <- (unlist( dta_long[paste(dec[i],"man_man",sep="_")] < dta_long[paste(dec[i],"man_woman",sep="_")]) )

df_m <- data.frame(rbind(df_m,mean(dta_long$differ, na.rm=T)))

dm <- dta[c("hhid","messenger",paste(paste(dec[i],"woman_woman_pl",sep="_") ,1:5,sep=""))]
dw <- dta[c("hhid","messenger",paste(paste(dec[i],"woman_man_pl",sep="_") ,1:5,sep=""))]

dta_long_m <- reshape(dm, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"woman_woman",sep="_") , idvar = "hhid")
dta_long_w <- reshape(dw, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"woman_man",sep="_") , idvar = "hhid")
dta_long <- merge(dta_long_m, dta_long_w)
dta_long$differ <- NA
dta_long$differ <- (unlist(dta_long[paste(dec[i],"woman_woman",sep="_")] < dta_long[paste(dec[i],"woman_man",sep="_")]) )

df_w <- data.frame(rbind(df_w,mean(dta_long$differ, na.rm=T)))
}



df <-cbind(df_m,df_w)[2:6,]
rownames(df) <- c("prepare land","planting","weeding","spraying","harvesting")

names(df) <- c("man","woman")

colours <- c( "#A1D99B", "#31A354")

png("/home/bjvca/data/projects/digital green/papers/DP_disagreement/results/shirking.png", units="px", height=3200, width= 4200, res=600)

barplot(as.matrix(t(df)), main="", ylab = "shirking", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours)
legend("topright", c("man","woman"), cex=1.3, bty="n", fill=colours)
dev.off()

## graph for difference in time 
df_m <- data.frame(NA)
dec <- c("time_prep","time_plant", "time_weed","time_spray", "time_harv")
for (i in 1:length(dec)) {
dm <- dta[c("hhid","messenger",paste(paste(dec[i],"woman_woman_pl",sep="_") ,1:5,sep=""))]
dw <- dta[c("hhid","messenger",paste(paste(dec[i],"man_man_pl",sep="_") ,1:5,sep=""))]

dta_long_m <- reshape(dm, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"woman_woman",sep="_") , idvar = "hhid")
dta_long_w <- reshape(dw, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"man_man",sep="_") , idvar = "hhid")
dta_long <- merge(dta_long_m, dta_long_w)
dta_long$differ <- NA
dta_long$differ <- (unlist(dta_long[paste(dec[i],"woman_woman",sep="_")] - dta_long[paste(dec[i],"man_man",sep="_")]) )

df_m <- data.frame(rbind(df_m,mean(dta_long$differ, na.rm=T)))


}


df <-cbind(df_m)[2:6,]
names(df) <- c("prepare land","planting","weeding","spraying","harvesting")
df <- data.frame(df)
names(df) <- "hours"
df$task <- rownames(df)
png("/home/bjvca/data/projects/digital green/papers/DP_disagreement/results/equity.png", units="px", height=1500, width= 4200, res=600)



ggdotchart(df, x = "task", y = "hours",
     
            color = "grey",
           sorting = "descending",                       # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           rotate = TRUE,                                # Rotate vertically
                                 # Order by groups
           dot.size = 10, ylab = "difference in time worked (hours) between female co-head and male co-head",                                # Large dot size
           label = round(df$hours,2),                        # Add mpg values as dot labels
           font.label = list(color = "black", size = 9, 
                             vjust = 0.5),               # Adjust label parameters
           ggtheme = theme_pubr()                        # ggplot2 theme
           )

dev.off()

### graphs for maize sales
sel <- c("nr_bags_sold_both_man","nr_bags_sold_man","nr_bags_sold_woman_man", "nr_bags_sold_both_woman","nr_bags_sold_man_woman", "nr_bags_sold_woman")

dta[sel] <- lapply(dta[sel], function(x) replace(x, x > quantile(x,.99),NA) )

df <- cbind(data.frame(colMeans(dta[sel[1:3]], na.rm=T)),  data.frame(colMeans(dta[sel[4:6]], na.rm=T)))
names(df) <- c("man reports","woman reports")

colours <- c( "#6E8DAB", "#104E8B")


png("/home/bjvca/data/projects/digital green/papers/DP_disagreement/results/sales.png", units="px", height=3200, width= 4200, res=600)
par(mfrow=c(1,2)) 
barplot(as.matrix(df[2:3,]), main="quantity sold", ylab = "bags", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours)



### now similar graph for revenue
sel <- c("income_both_man","income_man","income_woman_man", "income_both_woman","income_man_woman", "income_woman")

dta[sel] <- lapply(dta[sel], function(x) replace(x, x > quantile(x,.99, na.rm=T),NA) )

df <- cbind(data.frame(colMeans(dta[sel[1:3]], na.rm=T)),  data.frame(colMeans(dta[sel[4:6]], na.rm=T)))
df <- df/3600
names(df) <- c("man reports","woman reports")

barplot(as.matrix(df[2:3,]), main="revenue from sales", ylab = "USD", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours)
legend("topright", c("man sold","woman sold"), cex=1.3, bty="n", fill=colours)
dev.off()

dta$man_hiding <- (dta$nr_bags_sold_man ) >  ( dta$nr_bags_sold_man_woman )
dta$woman_hiding <- (dta$nr_bags_sold_woman )  > ( dta$nr_bags_sold_woman_man )

dta$joint_quant <- (dta$nr_bags_sold_both_man + dta$nr_bags_sold_both_woman)/2 
dta$man_hiding_quant <- (dta$nr_bags_sold_man ) -  ( dta$nr_bags_sold_man_woman )
dta$woman_hiding_quant <- (dta$nr_bags_sold_woman )  - ( dta$nr_bags_sold_woman_man )

dta$joint_income <- (dta$income_both_man + dta$income_both_woman)/2
dta$man_hiding_income <- (dta$income_man - dta$income_man_woman)/3600
dta$woman_hiding_income <- (dta$income_woman - dta$income_woman_man)/3600

df <- cbind(colMeans(dta[c("man_hiding_quant","woman_hiding_quant")], na.rm=T), colMeans(dta[c("man_hiding_income","woman_hiding_income")], na.rm=T))
names(df) <- c("quantities","revenue")

colours <- c("#CCCCCC", "#6E8DAB")

#barplot(as.matrix(df), main="", ylab = "share", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours)
#legend("topright", c("man hiding","woman hiding"), cex=1.3, bty="n", fill=colours)

### likert scales


#dta$tell_each_other_binned <- cut(dta$tell_each_other, c(1,2.5,3.5,4.5,5))
levels(dta$man_tells_wife_scale) <- c("Never","Rarely","Sometimes","Mostly","Always")
png("/home/bjvca/data/projects/digital green/papers/DP_disagreement/results/likert_plot_tell_each_other.png", units="px", height=3200, width= 4200, res=600)
par(mfrow=c(1,2), xpd=NA) 
colfunc<-colorRampPalette(c("#104E8B", "#6E8DAB","#CCCCCC"))
df <- data.frame(prop.table(table(dta$man_tells_wife_scale)), prop.table(table(dta$wife_tells_man_scale)))[,c(2,4)]
barplot(as.matrix(df), col=colfunc(5), main="Talk to \npartner", names.arg=c("man","woman"), cex.main=1.5,cex.axis=1.5, cex.names=1.5)
legend("right", legend=rev(c("Never","Rarely","Sometimes","Mostly","Always")), cex=1.5, bty="n", fill=rev(colfunc(4)), inset=c(-1.3,0))
dev.off()

dta_bkp <- dta



################# analysis
#only use subset where both spouses saw the video
dta <- subset(dta, recipient == "couple")

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

dm <- dta[c("hhid","messenger",paste(paste(dec[i],"man_pl",sep="_") ,1:5,sep=""))]
db <- dta[c("hhid","messenger",paste(paste(dec[i],"both_man_pl",sep="_") ,1:5,sep=""))]
dw <- dta[c("hhid","messenger",paste(paste(dec[i],"woman_pl",sep="_") ,1:5,sep=""))]

dta_long_m <- reshape(dm, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"man",sep="_") , idvar = "hhid")
dta_long_b <- reshape(db, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"both_man",sep="_") , idvar = "hhid")
dta_long_w <- reshape(dw, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"woman",sep="_") , idvar = "hhid")
dta_long <- merge(merge(dta_long_m, dta_long_w), dta_long_b)
dta_long$differ <- NA
dta_long$differ <- unlist(((dta_long[paste(dec[i],"man",sep="_")] ==1 |  dta_long[paste(dec[i],"both_man",sep="_")] ==1 ) & dta_long[paste(dec[i],"woman",sep="_")]==1))

ols <- lm( as.formula(paste("differ","(messenger=='couple')", sep ="~")),data=dta_long)
vcov_cluster <- vcovCR(ols, cluster = dta_long$hhid, type = "CR0")
res <- coef_test(ols, vcov_cluster)
conf <- conf_int(ols, vcov_cluster)

df_ols[1,2,i] <- mean(dta_long$differ[dta_long$messenger != "couple"], na.rm=T)
df_ols[2,2,i] <- sd(dta_long$differ[dta_long$messenger != "couple"], na.rm=T)
df_ols[3,2,i] <- res[2,1]
df_ols[4,2,i] <- res[2,2]
df_ols[5,2,i] <- res[2,5]
df_ols[6,2,i] <- nobs(ols)

dm <- dta[c("hhid","messenger",paste(paste(dec[i],"man_pl",sep="_") ,1:5,sep=""))]
db <- dta[c("hhid","messenger",paste(paste(dec[i],"both_woman_pl",sep="_") ,1:5,sep=""))]
dw <- dta[c("hhid","messenger",paste(paste(dec[i],"woman_pl",sep="_") ,1:5,sep=""))]

dta_long_m <- reshape(dm, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"man",sep="_") , idvar = "hhid")
dta_long_b <- reshape(db, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"both_woman",sep="_") , idvar = "hhid")
dta_long_w <- reshape(dw, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"woman",sep="_") , idvar = "hhid")
dta_long <- merge(merge(dta_long_m, dta_long_w), dta_long_b)
dta_long$differ <- NA
dta_long$differ <- unlist(((dta_long[paste(dec[i],"woman",sep="_")] ==1 |  dta_long[paste(dec[i],"both_woman",sep="_")] ==1 ) & dta_long[paste(dec[i],"man",sep="_")]==1))

ols <- lm( as.formula(paste("differ","(messenger=='couple')", sep ="~")),data=dta_long)
vcov_cluster <- vcovCR(ols, cluster = dta_long$hhid, type = "CR0")
res <- coef_test(ols, vcov_cluster)
conf <- conf_int(ols, vcov_cluster)

df_ols[1,3,i] <- mean(dta_long$differ[dta_long$messenger != "couple"], na.rm=T)
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
dta_long$differ <- (unlist(dta_long[paste(dec[i],"woman_woman",sep="_")] - dta_long[paste(dec[i],"man_man",sep="_")]))

ols <- lm( "(differ)~(messenger=='couple')",data=dta_long)
vcov_cluster <- vcovCR(ols, cluster = dta_long$hhid, type = "CR0")
res <- coef_test(ols, vcov_cluster)
conf <- conf_int(ols, vcov_cluster)

df_ols_time[1,1,i] <- mean((dta_long$differ[dta_long$messenger != "couple"]), na.rm=T)
df_ols_time[2,1,i] <- sd((dta_long[,4][dta_long$messenger != "couple"]), na.rm=T)
df_ols_time[3,1,i] <- res[2,1]
df_ols_time[4,1,i] <- res[2,2]
df_ols_time[5,1,i] <- res[2,5]
df_ols_time[6,1,i] <- nobs(ols)

dm <- dta[c("hhid","messenger",paste(paste(dec[i],"man_man_pl",sep="_") ,1:5,sep=""))]
dw <- dta[c("hhid","messenger",paste(paste(dec[i],"man_woman_pl",sep="_") ,1:5,sep=""))]

dta_long_m <- reshape(dm, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"man_man",sep="_") , idvar = "hhid")
dta_long_w <- reshape(dw, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"man_woman",sep="_") , idvar = "hhid")
dta_long <- merge(dta_long_m, dta_long_w)
dta_long$differ <- NA
dta_long$differ <- (unlist( dta_long[paste(dec[i],"man_man",sep="_")] < dta_long[paste(dec[i],"man_woman",sep="_")]) )

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

dm <- dta[c("hhid","messenger",paste(paste(dec[i],"woman_woman_pl",sep="_") ,1:5,sep=""))]
dw <- dta[c("hhid","messenger",paste(paste(dec[i],"woman_man_pl",sep="_") ,1:5,sep=""))]

dta_long_m <- reshape(dm, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"woman_woman",sep="_") , idvar = "hhid")
dta_long_w <- reshape(dw, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"woman_man",sep="_") , idvar = "hhid")
dta_long <- merge(dta_long_m, dta_long_w)
dta_long$differ <- NA
dta_long$differ <- (unlist(dta_long[paste(dec[i],"woman_woman",sep="_")] < dta_long[paste(dec[i],"woman_man",sep="_")]) )

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

df_ols_inc <-  array(NA,dim=c(6,3,3))
i <- 1

ols <- lm( "joint_quant~(messenger=='couple')",data=dta)
df_ols_inc[1,1,i] <- mean(dta$joint_quant[dta$messenger != "couple"], na.rm=T)
df_ols_inc[2,1,i] <- sd(dta$joint_quant[dta$messenger != "couple"], na.rm=T)
df_ols_inc[3,1,i] <- summary(ols)$coefficients[2, 1]
df_ols_inc[4,1,i] <- summary(ols)$coefficients[2, 2]
df_ols_inc[5,1,i] <- summary(ols)$coefficients[2, 4]
df_ols_inc[6,1,i] <- nobs(ols)

ols <- lm( "man_hiding~(messenger=='couple')",data=dta)
df_ols_inc[1,2,i] <- mean(dta$man_hiding[dta$messenger != "couple"], na.rm=T)
df_ols_inc[2,2,i] <- sd(dta$man_hiding[dta$messenger != "couple"], na.rm=T)
df_ols_inc[3,2,i] <- summary(ols)$coefficients[2, 1]
df_ols_inc[4,2,i] <- summary(ols)$coefficients[2, 2]
df_ols_inc[5,2,i] <- summary(ols)$coefficients[2, 4]
df_ols_inc[6,2,i] <- nobs(ols)

ols <- lm( "woman_hiding~(messenger=='couple')",data=dta)
df_ols_inc[1,3,i] <- mean(dta$woman_hiding[dta$messenger != "couple"], na.rm=T)
df_ols_inc[2,3,i] <- sd(dta$woman_hiding[dta$messenger != "couple"], na.rm=T)
df_ols_inc[3,3,i] <- summary(ols)$coefficients[2, 1]
df_ols_inc[4,3,i] <- summary(ols)$coefficients[2, 2]
df_ols_inc[5,3,i] <- summary(ols)$coefficients[2, 4]
df_ols_inc[6,3,i] <- nobs(ols)


i <- 2

ols <- lm( "joint_quant~(messenger=='couple')",data=dta)
df_ols_inc[1,1,i] <- mean(dta$joint_quant[dta$messenger != "couple"], na.rm=T)
df_ols_inc[2,1,i] <- sd(dta$joint_quant[dta$messenger != "couple"], na.rm=T)
df_ols_inc[3,1,i] <- summary(ols)$coefficients[2, 1]
df_ols_inc[4,1,i] <- summary(ols)$coefficients[2, 2]
df_ols_inc[5,1,i] <- summary(ols)$coefficients[2, 4]
df_ols_inc[6,1,i] <- nobs(ols)

ols <- lm( "man_hiding_quant~(messenger=='couple')",data=dta)
df_ols_inc[1,2,i] <- mean(dta$man_hiding_quant[dta$messenger != "couple"], na.rm=T)
df_ols_inc[2,2,i] <- sd(dta$man_hiding_quant[dta$messenger != "couple"], na.rm=T)
df_ols_inc[3,2,i] <- summary(ols)$coefficients[2, 1]
df_ols_inc[4,2,i] <- summary(ols)$coefficients[2, 2]
df_ols_inc[5,2,i] <- summary(ols)$coefficients[2, 4]
df_ols_inc[6,2,i] <- nobs(ols)

ols <- lm( "woman_hiding_quant~(messenger=='couple')",data=dta)
df_ols_inc[1,3,i] <- mean(dta$woman_hiding_quant[dta$messenger != "couple"], na.rm=T)
df_ols_inc[2,3,i] <- sd(dta$woman_hiding_quant[dta$messenger != "couple"], na.rm=T)
df_ols_inc[3,3,i] <- summary(ols)$coefficients[2, 1]
df_ols_inc[4,3,i] <- summary(ols)$coefficients[2, 2]
df_ols_inc[5,3,i] <- summary(ols)$coefficients[2, 4]
df_ols_inc[6,3,i] <- nobs(ols)


### income hiding
i <- 3

ols <- lm( "joint_income~(messenger=='couple')",data=dta)
df_ols_inc[1,1,i] <- mean(dta$joint_income[dta$messenger != "couple"], na.rm=T)
df_ols_inc[2,1,i] <- sd(dta$joint_income[dta$messenger != "couple"], na.rm=T)
df_ols_inc[3,1,i] <- summary(ols)$coefficients[2, 1]
df_ols_inc[4,1,i] <- summary(ols)$coefficients[2, 2]
df_ols_inc[5,1,i] <- summary(ols)$coefficients[2, 4]
df_ols_inc[6,1,i] <- nobs(ols)

ols <- lm( "man_hiding_income~(messenger=='couple')",data=dta)
df_ols_inc[1,2,i] <- mean(dta$man_hiding_income[dta$messenger != "couple"], na.rm=T)
df_ols_inc[2,2,i] <- sd(dta$man_hiding_income[dta$messenger != "couple"], na.rm=T)
df_ols_inc[3,2,i] <- summary(ols)$coefficients[2, 1]
df_ols_inc[4,2,i] <- summary(ols)$coefficients[2, 2]
df_ols_inc[5,2,i] <- summary(ols)$coefficients[2, 4]
df_ols_inc[6,2,i] <- nobs(ols)

ols <- lm( "woman_hiding_income~(messenger=='couple')",data=dta)
df_ols_inc[1,3,i] <- mean(dta$woman_hiding_income[dta$messenger != "couple"], na.rm=T)
df_ols_inc[2,3,i] <- sd(dta$woman_hiding_income[dta$messenger != "couple"], na.rm=T)
df_ols_inc[3,3,i] <- summary(ols)$coefficients[2, 1]
df_ols_inc[4,3,i] <- summary(ols)$coefficients[2, 2]
df_ols_inc[5,3,i] <- summary(ols)$coefficients[2, 4]
df_ols_inc[6,3,i] <- nobs(ols)

################ analysis 2
#only use subset where both spouses saw the video
dta <- subset(dta_bkp, messenger == "couple")

### decision making
dec <- c("mgt","dectime", "decspace","decstriga", "decweed")
df_ols_2 <- array(NA,dim=c(6,3,length(dec)))
for (i in 1:length(dec)) {
d <- dta[c("hhid","recipient",paste(paste(dec[i],"both_pl",sep="_") ,1:5,sep=""))]

dta_long <- reshape(d, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"both",sep="_") , idvar = "hhid")

ols <- lm( as.formula(paste(paste(dec[i],"both",sep="_"),"(recipient=='couple')", sep ="~")),data=dta_long)
vcov_cluster <- vcovCR(ols, cluster = dta_long$hhid, type = "CR0")
res <- coef_test(ols, vcov_cluster)
conf <- conf_int(ols, vcov_cluster)
df_ols_2[1,1,i] <- mean(dta_long[,4][dta_long$recipient != "couple"], na.rm=T)
df_ols_2[2,1,i] <- sd(dta_long[,4][dta_long$recipient != "couple"], na.rm=T)
df_ols_2[3,1,i] <- res[2,1]
df_ols_2[4,1,i] <- res[2,2]
df_ols_2[5,1,i] <- res[2,5]
df_ols_2[6,1,i] <- nobs(ols)

dm <- dta[c("hhid","recipient",paste(paste(dec[i],"man_pl",sep="_") ,1:5,sep=""))]
db <- dta[c("hhid","recipient",paste(paste(dec[i],"both_man_pl",sep="_") ,1:5,sep=""))]
dw <- dta[c("hhid","recipient",paste(paste(dec[i],"woman_pl",sep="_") ,1:5,sep=""))]

dta_long_m <- reshape(dm, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"man",sep="_") , idvar = "hhid")
dta_long_b <- reshape(db, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"both_man",sep="_") , idvar = "hhid")
dta_long_w <- reshape(dw, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"woman",sep="_") , idvar = "hhid")
dta_long <- merge(merge(dta_long_m, dta_long_w), dta_long_b)
dta_long$differ <- NA
dta_long$differ <- unlist(((dta_long[paste(dec[i],"man",sep="_")] ==1 |  dta_long[paste(dec[i],"both_man",sep="_")] ==1 ) & dta_long[paste(dec[i],"woman",sep="_")]==1))

ols <- lm( as.formula(paste("differ","(recipient=='couple')", sep ="~")),data=dta_long)
vcov_cluster <- vcovCR(ols, cluster = dta_long$hhid, type = "CR0")
res <- coef_test(ols, vcov_cluster)
conf <- conf_int(ols, vcov_cluster)

df_ols_2[1,2,i] <- mean(dta_long$differ[dta_long$recipient != "couple"], na.rm=T)
df_ols_2[2,2,i] <- sd(dta_long$differ[dta_long$recipient != "couple"], na.rm=T)
df_ols_2[3,2,i] <- res[2,1]
df_ols_2[4,2,i] <- res[2,2]
df_ols_2[5,2,i] <- res[2,5]
df_ols_2[6,2,i] <- nobs(ols)

dm <- dta[c("hhid","recipient",paste(paste(dec[i],"man_pl",sep="_") ,1:5,sep=""))]
db <- dta[c("hhid","recipient",paste(paste(dec[i],"both_woman_pl",sep="_") ,1:5,sep=""))]
dw <- dta[c("hhid","recipient",paste(paste(dec[i],"woman_pl",sep="_") ,1:5,sep=""))]

dta_long_m <- reshape(dm, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"man",sep="_") , idvar = "hhid")
dta_long_b <- reshape(db, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"both_woman",sep="_") , idvar = "hhid")
dta_long_w <- reshape(dw, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"woman",sep="_") , idvar = "hhid")
dta_long <- merge(merge(dta_long_m, dta_long_w), dta_long_b)
dta_long$differ <- NA
dta_long$differ <- unlist(((dta_long[paste(dec[i],"woman",sep="_")] ==1 |  dta_long[paste(dec[i],"both_woman",sep="_")] ==1 ) & dta_long[paste(dec[i],"man",sep="_")]==1))

ols <- lm( as.formula(paste("differ","(recipient=='couple')", sep ="~")),data=dta_long)
vcov_cluster <- vcovCR(ols, cluster = dta_long$hhid, type = "CR0")
res <- coef_test(ols, vcov_cluster)
conf <- conf_int(ols, vcov_cluster)

df_ols_2[1,3,i] <- mean(dta_long$differ[dta_long$recipient != "couple"], na.rm=T)
df_ols_2[2,3,i] <- sd(dta_long[,4][dta_long$recipient != "couple"], na.rm=T)
df_ols_2[3,3,i] <- res[2,1]
df_ols_2[4,3,i] <- res[2,2]
df_ols_2[5,3,i] <- res[2,5]
df_ols_2[6,3,i] <- nobs(ols)
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
df_ols_2_time <- array(NA,dim=c(6,3,length(dec)))
for (i in 1:length(dec)) {
dm <- dta[c("hhid","recipient",paste(paste(dec[i],"man_man_pl",sep="_") ,1:5,sep=""))]
dw <- dta[c("hhid","recipient",paste(paste(dec[i],"woman_woman_pl",sep="_") ,1:5,sep=""))]

dta_long_m <- reshape(dm, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"man_man",sep="_") , idvar = "hhid")
dta_long_w <- reshape(dw, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"woman_woman",sep="_") , idvar = "hhid")
dta_long <- merge(dta_long_m, dta_long_w)
dta_long$differ <- NA
dta_long$differ <- (unlist(dta_long[paste(dec[i],"woman_woman",sep="_")] - dta_long[paste(dec[i],"man_man",sep="_")]))

ols <- lm( "(differ)~(recipient=='couple')",data=dta_long)
vcov_cluster <- vcovCR(ols, cluster = dta_long$hhid, type = "CR0")
res <- coef_test(ols, vcov_cluster)
conf <- conf_int(ols, vcov_cluster)

df_ols_2_time[1,1,i] <- mean((dta_long$differ[dta_long$recipient != "couple"]), na.rm=T)
df_ols_2_time[2,1,i] <- sd((dta_long[,4][dta_long$recipient != "couple"]), na.rm=T)
df_ols_2_time[3,1,i] <- res[2,1]
df_ols_2_time[4,1,i] <- res[2,2]
df_ols_2_time[5,1,i] <- res[2,5]
df_ols_2_time[6,1,i] <- nobs(ols)

dm <- dta[c("hhid","recipient",paste(paste(dec[i],"man_man_pl",sep="_") ,1:5,sep=""))]
dw <- dta[c("hhid","recipient",paste(paste(dec[i],"man_woman_pl",sep="_") ,1:5,sep=""))]

dta_long_m <- reshape(dm, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"man_man",sep="_") , idvar = "hhid")
dta_long_w <- reshape(dw, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"man_woman",sep="_") , idvar = "hhid")
dta_long <- merge(dta_long_m, dta_long_w)
dta_long$differ <- NA
dta_long$differ <- (unlist( dta_long[paste(dec[i],"man_man",sep="_")] < dta_long[paste(dec[i],"man_woman",sep="_")]) )

ols <- lm( "differ~(recipient=='couple')",data=dta_long)
vcov_cluster <- vcovCR(ols, cluster = dta_long$hhid, type = "CR0")
res <- coef_test(ols, vcov_cluster)
conf <- conf_int(ols, vcov_cluster)

df_ols_2_time[1,2,i] <- mean(dta_long$differ[dta_long$recipient != "couple"], na.rm=T)
df_ols_2_time[2,2,i] <- sd(dta_long[,4][dta_long$recipient != "couple"], na.rm=T)
df_ols_2_time[3,2,i] <- res[2,1]
df_ols_2_time[4,2,i] <- res[2,2]
df_ols_2_time[5,2,i] <- res[2,5]
df_ols_2_time[6,2,i] <- nobs(ols)

dm <- dta[c("hhid","recipient",paste(paste(dec[i],"woman_woman_pl",sep="_") ,1:5,sep=""))]
dw <- dta[c("hhid","recipient",paste(paste(dec[i],"woman_man_pl",sep="_") ,1:5,sep=""))]

dta_long_m <- reshape(dm, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"woman_woman",sep="_") , idvar = "hhid")
dta_long_w <- reshape(dw, direction = "long",varying = 3:7 ,v.names=paste(dec[i],"woman_man",sep="_") , idvar = "hhid")
dta_long <- merge(dta_long_m, dta_long_w)
dta_long$differ <- NA
dta_long$differ <- (unlist(dta_long[paste(dec[i],"woman_woman",sep="_")] < dta_long[paste(dec[i],"woman_man",sep="_")]) )

ols <- lm( "differ~(recipient=='couple')",data=dta_long)
vcov_cluster <- vcovCR(ols, cluster = dta_long$hhid, type = "CR0")
res <- coef_test(ols, vcov_cluster)
conf <- conf_int(ols, vcov_cluster)

df_ols_2_time[1,3,i] <- mean(dta_long$differ[dta_long$recipient != "couple"], na.rm=T)
df_ols_2_time[2,3,i] <- sd(dta_long[,4][dta_long$recipient != "couple"], na.rm=T)
df_ols_2_time[3,3,i] <- res[2,1]
df_ols_2_time[4,3,i] <- res[2,2]
df_ols_2_time[5,3,i] <- res[2,5]
df_ols_2_time[6,3,i] <- nobs(ols)
}


###conflict resolution
i <- 1
df_ols_2_confl <-  array(NA,dim=c(6,3,2))
ols <- lm( "man_tells_wife~(recipient=='couple')",data=dta)
df_ols_2_confl[1,1,i] <- mean(dta$man_tells_wife[dta$recipient != "couple"], na.rm=T)
df_ols_2_confl[2,1,i] <- sd(dta$man_tells_wife[dta$recipient != "couple"], na.rm=T)
df_ols_2_confl[3,1,i] <- summary(ols)$coefficients[2, 1]
df_ols_2_confl[4,1,i] <- summary(ols)$coefficients[2, 2]
df_ols_2_confl[5,1,i] <- summary(ols)$coefficients[2, 4]
df_ols_2_confl[6,1,i] <- nobs(ols)

ols <- lm( "wife_tells_man~(recipient=='couple')",data=dta)
df_ols_2_confl[1,2,i] <- mean(dta$wife_tells_man[dta$recipient != "couple"], na.rm=T)
df_ols_2_confl[2,2,i] <- sd(dta$wife_tells_man[dta$recipient != "couple"], na.rm=T)
df_ols_2_confl[3,2,i] <- summary(ols)$coefficients[2, 1]
df_ols_2_confl[4,2,i] <- summary(ols)$coefficients[2, 2]
df_ols_2_confl[5,2,i] <- summary(ols)$coefficients[2, 4]
df_ols_2_confl[6,2,i] <- nobs(ols)

ols <- lm( "both_tell~(recipient=='couple')",data=dta)
df_ols_2_confl[1,3,i] <- mean(dta$both_tell[dta$recipient != "couple"], na.rm=T)
df_ols_2_confl[2,3,i] <- sd(dta$both_tell[dta$recipient != "couple"], na.rm=T)
df_ols_2_confl[3,3,i] <- summary(ols)$coefficients[2, 1]
df_ols_2_confl[4,3,i] <- summary(ols)$coefficients[2, 2]
df_ols_2_confl[5,3,i] <- summary(ols)$coefficients[2, 4]
df_ols_2_confl[6,3,i] <- nobs(ols)
i <- 2
ols <- lm( "man_listens~(recipient=='couple')",data=dta)
df_ols_2_confl[1,1,i] <- mean(dta$wife_listens[dta$recipient != "couple"], na.rm=T)
df_ols_2_confl[2,1,i] <- sd(dta$wife_listens[dta$recipient != "couple"], na.rm=T)
df_ols_2_confl[3,1,i] <- summary(ols)$coefficients[2, 1]
df_ols_2_confl[4,1,i] <- summary(ols)$coefficients[2, 2]
df_ols_2_confl[5,1,i] <- summary(ols)$coefficients[2, 4]
df_ols_2_confl[6,1,i] <- nobs(ols)

ols <- lm( "wife_listens~(recipient=='couple')",data=dta)
df_ols_2_confl[1,2,i] <- mean(dta$man_listens[dta$recipient != "couple"], na.rm=T)
df_ols_2_confl[2,2,i] <- sd(dta$man_listens[dta$recipient != "couple"], na.rm=T)
df_ols_2_confl[3,2,i] <- summary(ols)$coefficients[2, 1]
df_ols_2_confl[4,2,i] <- summary(ols)$coefficients[2, 2]
df_ols_2_confl[5,2,i] <- summary(ols)$coefficients[2, 4]
df_ols_2_confl[6,2,i] <- nobs(ols)

ols <- lm( "spouses_listen~(recipient=='couple')",data=dta)
df_ols_2_confl[1,3,i] <- mean(dta$spouses_listen[dta$recipient != "couple"], na.rm=T)
df_ols_2_confl[2,3,i] <- sd(dta$spouses_listen[dta$recipient != "couple"], na.rm=T)
df_ols_2_confl[3,3,i] <- summary(ols)$coefficients[2, 1]
df_ols_2_confl[4,3,i] <- summary(ols)$coefficients[2, 2]
df_ols_2_confl[5,3,i] <- summary(ols)$coefficients[2, 4]
df_ols_2_confl[6,3,i] <- nobs(ols)

df_ols_2_inc <-  array(NA,dim=c(6,3,3))
i <- 1

ols <- lm( "joint_quant~(recipient=='couple')",data=dta)
df_ols_2_inc[1,1,i] <- mean(dta$joint_quant[dta$recipient != "couple"], na.rm=T)
df_ols_2_inc[2,1,i] <- sd(dta$joint_quant[dta$recipient != "couple"], na.rm=T)
df_ols_2_inc[3,1,i] <- summary(ols)$coefficients[2, 1]
df_ols_2_inc[4,1,i] <- summary(ols)$coefficients[2, 2]
df_ols_2_inc[5,1,i] <- summary(ols)$coefficients[2, 4]
df_ols_2_inc[6,1,i] <- nobs(ols)

ols <- lm( "man_hiding~(recipient=='couple')",data=dta)
df_ols_2_inc[1,2,i] <- mean(dta$man_hiding[dta$recipient != "couple"], na.rm=T)
df_ols_2_inc[2,2,i] <- sd(dta$man_hiding[dta$recipient != "couple"], na.rm=T)
df_ols_2_inc[3,2,i] <- summary(ols)$coefficients[2, 1]
df_ols_2_inc[4,2,i] <- summary(ols)$coefficients[2, 2]
df_ols_2_inc[5,2,i] <- summary(ols)$coefficients[2, 4]
df_ols_2_inc[6,2,i] <- nobs(ols)

ols <- lm( "woman_hiding~(recipient=='couple')",data=dta)
df_ols_2_inc[1,3,i] <- mean(dta$woman_hiding[dta$recipient != "couple"], na.rm=T)
df_ols_2_inc[2,3,i] <- sd(dta$woman_hiding[dta$recipient != "couple"], na.rm=T)
df_ols_2_inc[3,3,i] <- summary(ols)$coefficients[2, 1]
df_ols_2_inc[4,3,i] <- summary(ols)$coefficients[2, 2]
df_ols_2_inc[5,3,i] <- summary(ols)$coefficients[2, 4]
df_ols_2_inc[6,3,i] <- nobs(ols)

i <- 2

ols <- lm( "joint_quant~(recipient=='couple')",data=dta)
df_ols_2_inc[1,1,i] <- mean(dta$joint_quant[dta$recipient != "couple"], na.rm=T)
df_ols_2_inc[2,1,i] <- sd(dta$joint_quant[dta$recipient != "couple"], na.rm=T)
df_ols_2_inc[3,1,i] <- summary(ols)$coefficients[2, 1]
df_ols_2_inc[4,1,i] <- summary(ols)$coefficients[2, 2]
df_ols_2_inc[5,1,i] <- summary(ols)$coefficients[2, 4]
df_ols_2_inc[6,1,i] <- nobs(ols)

ols <- lm( "man_hiding_quant~(recipient=='couple')",data=dta)
df_ols_2_inc[1,2,i] <- mean(dta$man_hiding_quant[dta$recipient != "couple"], na.rm=T)
df_ols_2_inc[2,2,i] <- sd(dta$man_hiding_quant[dta$recipient != "couple"], na.rm=T)
df_ols_2_inc[3,2,i] <- summary(ols)$coefficients[2, 1]
df_ols_2_inc[4,2,i] <- summary(ols)$coefficients[2, 2]
df_ols_2_inc[5,2,i] <- summary(ols)$coefficients[2, 4]
df_ols_2_inc[6,2,i] <- nobs(ols)

ols <- lm( "woman_hiding_quant~(recipient=='couple')",data=dta)
df_ols_2_inc[1,3,i] <- mean(dta$woman_hiding_quant[dta$recipient != "couple"], na.rm=T)
df_ols_2_inc[2,3,i] <- sd(dta$woman_hiding_quant[dta$recipient != "couple"], na.rm=T)
df_ols_2_inc[3,3,i] <- summary(ols)$coefficients[2, 1]
df_ols_2_inc[4,3,i] <- summary(ols)$coefficients[2, 2]
df_ols_2_inc[5,3,i] <- summary(ols)$coefficients[2, 4]
df_ols_2_inc[6,3,i] <- nobs(ols)


### income hiding

i <- 3

ols <- lm( "joint_income~(recipient=='couple')",data=dta)
df_ols_2_inc[1,1,i] <- mean(dta$joint_income[dta$recipient != "couple"], na.rm=T)
df_ols_2_inc[2,1,i] <- sd(dta$joint_income[dta$recipient != "couple"], na.rm=T)
df_ols_2_inc[3,1,i] <- summary(ols)$coefficients[2, 1]
df_ols_2_inc[4,1,i] <- summary(ols)$coefficients[2, 2]
df_ols_2_inc[5,1,i] <- summary(ols)$coefficients[2, 4]
df_ols_2_inc[6,1,i] <- nobs(ols)

ols <- lm( "man_hiding_income~(recipient=='couple')",data=dta)
df_ols_2_inc[1,2,i] <- mean(dta$man_hiding_income[dta$recipient != "couple"], na.rm=T)
df_ols_2_inc[2,2,i] <- sd(dta$man_hiding_income[dta$recipient != "couple"], na.rm=T)
df_ols_2_inc[3,2,i] <- summary(ols)$coefficients[2, 1]
df_ols_2_inc[4,2,i] <- summary(ols)$coefficients[2, 2]
df_ols_2_inc[5,2,i] <- summary(ols)$coefficients[2, 4]
df_ols_2_inc[6,2,i] <- nobs(ols)

ols <- lm( "woman_hiding_income~(recipient=='couple')",data=dta)
df_ols_2_inc[1,3,i] <- mean(dta$woman_hiding_income[dta$recipient != "couple"], na.rm=T)
df_ols_2_inc[2,3,i] <- sd(dta$woman_hiding_income[dta$recipient != "couple"], na.rm=T)
df_ols_2_inc[3,3,i] <- summary(ols)$coefficients[2, 1]
df_ols_2_inc[4,3,i] <- summary(ols)$coefficients[2, 2]
df_ols_2_inc[5,3,i] <- summary(ols)$coefficients[2, 4]
df_ols_2_inc[6,3,i] <- nobs(ols)








