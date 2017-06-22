

all <- rbind(read.csv("/home/bjvca/data/projects/digital green/sampling/iganga_admin.csv", header=F),
read.csv("/home/bjvca/data/projects/digital green/sampling/bugiri_admin.csv", header=F),
read.csv("/home/bjvca/data/projects/digital green/sampling/namayingo_admin.csv", header=F),
read.csv("/home/bjvca/data/projects/digital green/sampling/mayuge_admin.csv", header=F),
read.csv("/home/bjvca/data/projects/digital green/sampling/namutumba_admin.csv", header=F))

names(all) <- c("district","county","sc","parish","village")


all$district <- substring(all$district, 4)
all$county <- substring(all$county, 5)
all$sc <- substring(all$sc, 4)
all$parish <- substring(all$parish, 4)
all$village <- substring(all$village, 4)

all$county <- NULL

for (i in 1:dim(all)[1]) {
if (all$district[i] == "") {
all$district[i] <- all$district[i-1]
}
if (all$sc[i] == "") {
all$sc[i] <- all$sc[i-1]
}
if (all$parish[i] == "") {
all$parish[i] <- all$parish[i-1]
}
}


all$sc <- paste(all$district,all$sc, sep = "-")
all$parish <- paste(all$sc,all$parish, sep = "-")
all$village <- paste(all$parish,all$village, sep = "-")

for (j in names(table(all$district))) {
print(j)
print(length(table(all$sc[all$district == j])))
 print(length(table(all$parish[all$district == j])))
print( length(table(all$village[all$district == j])))
}

all <- subset(all, all$sc!="BUGIRI-BUGIRI TOWN COUNCIL")
all <- subset(all, all$sc!="IGANGA-BUSEMBATIA TOWN COUNCIL")
all <- subset(all, all$sc!="IGANGA-CENTRAL DIVISION")
all <- subset(all, all$sc!="MAYUGE-MAYUGE TOWN COUNCIL")
all <- subset(all, all$sc!="MAYUGE-JAGUZI")
all <- subset(all, all$sc!="MAYUGE-MALONGO")
all <- subset(all, all$sc!="MAYUGE-MAYUGE TOWN COUNCIL")
all <- subset(all, all$sc!="NAMAYINGO-NAMAYINGO TOWN COUNCIL")
all <- subset(all, all$sc!="NAMAYINGO-SIGULU ISLANDS") 
all <- subset(all, all$sc!="NAMUTUMBA-NAMUTUMBA TOWN COUNCIL")
### this is rice growing only
all <- subset(all, all$parish !="BUGIRI-BULESA-BUWUNI TOWN BOARD")

set.seed(12345)

sample_set <- all[all$parish %in% sample(all$parish,51),]
### this is the sample
### create the sample
messenger <- c(rep("male",3),rep("female",3),rep("couple",3),"ctrl") 
recipient <- c(rep(c("male","female","couple"),3),"ctrl") 
frame <- data.frame(messenger, recipient)   
###merge sample and frame for the first random 257 combimatins

first_part <- merge(sample_set[sample(nrow(sample_set), 257), ],frame)

### remove the obs that are in first_part from sample set
sample_set <- subset(sample_set, !(village %in% names(table(first_part$village))))

messenger <- c(rep("male",3),rep("female",3),rep("couple",3)) 
recipient <- c(rep(c("male","female","couple"),3)) 
frame <- data.frame(messenger, recipient)  

second_part <- merge(sample_set[sample(nrow(sample_set), 85), ],frame)
first_part <- rbind(first_part, second_part)

### and again, get remaining

sample_set <- subset(sample_set, !(village %in% names(table(first_part$village))))

messenger <- c(rep("male",2),rep("female",2),rep("couple",3)) 
recipient <- c(rep(c("male","female"),2),c("male","female","couple")) 
frame <- data.frame(messenger, recipient)  

second_part <- merge(sample_set[sample(nrow(sample_set), 27), ],frame)
first_part <- rbind(first_part, second_part)

### and again, get remaining

sample_set <- subset(sample_set, !(village %in% names(table(first_part$village))))

messenger <- c(rep("male",2),rep("female",2)) 
recipient <- c(rep(c("male","female"),2)) 
frame <- data.frame(messenger, recipient)  

second_part <- merge(sample_set,frame)
final_sample <- rbind(first_part, second_part)

### now mix in the IVR treatment
final_sample$IVR <- "no"

final_sample[final_sample$recipient == "male" & final_sample$messenger == "male",]$IVR[sample(length(final_sample[final_sample$recipient == "male" & final_sample$messenger == "male",]$IVR), floor(length(final_sample[final_sample$recipient == "male" & final_sample$messenger == "male",]$IVR)/2)) ] <- "yes"


final_sample[final_sample$recipient == "male" & final_sample$messenger == "female",]$IVR[sample(length(final_sample[final_sample$recipient == "male" & final_sample$messenger == "female",]$IVR), floor(length(final_sample[final_sample$recipient == "male" & final_sample$messenger == "female",]$IVR)/2)) ] <- "yes"

final_sample[final_sample$recipient == "male" & final_sample$messenger == "couple",]$IVR[sample(length(final_sample[final_sample$recipient == "male" & final_sample$messenger == "couple",]$IVR), floor(length(final_sample[final_sample$recipient == "male" & final_sample$messenger == "couple",]$IVR)/2)) ] <- "yes"

final_sample[final_sample$recipient == "female" & final_sample$messenger == "male",]$IVR[sample(length(final_sample[final_sample$recipient == "female" & final_sample$messenger == "male",]$IVR), floor(length(final_sample[final_sample$recipient == "female" & final_sample$messenger == "male",]$IVR)/2)) ] <- "yes"


final_sample[final_sample$recipient == "female" & final_sample$messenger == "female",]$IVR[sample(length(final_sample[final_sample$recipient == "female" & final_sample$messenger == "female",]$IVR), floor(length(final_sample[final_sample$recipient == "female" & final_sample$messenger == "female",]$IVR)/2)) ] <- "yes"

final_sample[final_sample$recipient == "female" & final_sample$messenger == "couple",]$IVR[sample(length(final_sample[final_sample$recipient == "female" & final_sample$messenger == "couple",]$IVR), floor(length(final_sample[final_sample$recipient == "female" & final_sample$messenger == "couple",]$IVR)/2)) ] <- "yes"

final_sample[final_sample$recipient == "couple" & final_sample$messenger == "male",]$IVR[sample(length(final_sample[final_sample$recipient == "couple" & final_sample$messenger == "male",]$IVR), floor(length(final_sample[final_sample$recipient == "couple" & final_sample$messenger == "male",]$IVR)/2)) ] <- "yes"


final_sample[final_sample$recipient == "couple" & final_sample$messenger == "female",]$IVR[sample(length(final_sample[final_sample$recipient == "couple" & final_sample$messenger == "female",]$IVR), floor(length(final_sample[final_sample$recipient == "couple" & final_sample$messenger == "female",]$IVR)/2)) ] <- "yes"

final_sample[final_sample$recipient == "couple" & final_sample$messenger == "couple",]$IVR[sample(length(final_sample[final_sample$recipient == "couple" & final_sample$messenger == "couple",]$IVR), floor(length(final_sample[final_sample$recipient == "couple" & final_sample$messenger == "couple",]$IVR)/2)) ] <- "yes"

### for ctrl treatment, also balance over who gets to see the video

final_sample[final_sample$messenger == "ctrl",]$recipient[sample(length(final_sample[final_sample$messenger == "ctrl",]$recipient), floor(length(final_sample[final_sample$messenger == "ctrl",]$recipient)/3))] <- "female"

final_sample[final_sample$messenger == "ctrl" & final_sample$recipient != "female",]$recipient[sample(length(final_sample[final_sample$messenger == "ctrl" & final_sample$recipient != "female",]$recipient), floor(length(final_sample[final_sample$messenger == "ctrl" & final_sample$recipient != "female",]$recipient)/2))] <- "male"

final_sample[final_sample$messenger == "ctrl" & final_sample$recipient == "ctrl",]$recipient <- "couple"

final_sample$sc <- (sapply(strsplit(final_sample$sc,"-"),'[',2))

final_sample$parish <- (sapply(strsplit(final_sample$parish,"-"),'[',3))

final_sample$village <- (sapply(strsplit(final_sample$village,"-"),'[',4))

write.csv(final_sample, "/home/bjvca/data/projects/digital green/sampling/sampling_list.csv")



