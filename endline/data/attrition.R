### investigating attrition
rm(list=ls())
library(readstata13)

dta <- read.csv("/home/bjvca/data/projects/digital green/endline/data/endline.csv")
dta <- subset(dta, femalehead == 0)
 

# the variable video_shown does not have info so let's just merge in from the sampling list
treats <- read.csv("/home/bjvca/data/projects/digital green/midline/list_sec.csv")
dta <- merge(treats, dta, by="hhid", all=T)

### who recieved the video?
dta$recipient.y <- NULL
names(dta)[names(dta) == 'recipient.x'] <- 'recipient'
dta$messenger.y <- NULL
names(dta)[names(dta) == 'messenger.x'] <- 'messenger'


# general attrition about 8 percent
summary(dta$interview_status)/dim(dta)[1]
# and unrelated to treatment
summary(lm(is.na(dta$interview_status)~dta$messenger=="ctrl"))
summary(lm(is.na(dta$interview_status)~dta$recipient=="couple"))
summary(lm(is.na(dta$interview_status)~dta$messenger=="couple"))

#dta <- subset(dta, dta$messenger != "ctrl")
#simulate corrections
#dta$interview_status[dta$recipient == "male" & dta$interview_status=="one individual interviewed" & !is.na(dta$interview_status)][sample(1:length(dta$interview_status[dta$recipient == "male" & dta$interview_status=="one individual interviewed" & !is.na(dta$interview_status)]),87)] <- "couple interviewed"
#dta$interview_status[dta$recipient == "female" & dta$interview_status=="one individual interviewed" & !is.na(dta$interview_status)][sample(1:length(dta$interview_status[dta$recipient == "female" & dta$interview_status=="one individual interviewed" & !is.na(dta$interview_status)]),148)] <- "couple interviewed"

#dta$interview_status[dta$recipient == "couple" & dta$interview_status=="one individual interviewed" & !is.na(dta$interview_status)][sample(1:length(dta$interview_status[dta$recipient == "couple" & dta$interview_status=="one individual interviewed" & !is.na(dta$interview_status)]),31)] <- "couple interviewed"
prop.table(table(dta$interview_status, dta$recipient),2)


#but in 31 percent of households that were interviewed, only one person in the household was interviewed instead of two.
summary(dta$interview_status=="one individual interviewed")
#and here, there is a correlation with the treatment - in households where the video was shown to the couple, it is more likely that both spouses where interviewed
summary(lm((interview_status=="one individual interviewed")~recipient=="couple", data=dta))
RI("recipient=='couple'","interview_status=='one individual interviewed'", dta,1000)

summary(lm((interview_status=="one individual interviewed")~messenger=="couple", data=dta))

## is gender of the person who was not interviewed correlated to the treatment?
summary(lm((person_interviewed=="man")~recipient=="couple", data=dta[dta$messenger!="ctrl" & dta$interview_status=="one individual interviewed",]))
## if the video was shown to the man, it is more likely that the man only was interviewed
## if the video was shown to the woman, it is more likely that the woman only was interviewed
summary(lm((person_interviewed=="man")~recipient, data=dta[dta$messenger!="ctrl" & dta$interview_status=="one individual interviewed",]))

###this is the problem
prop.table(table(dta$interview_status, dta$recipient),2)

ss <- subset(dta, interview_status=="one individual interviewed" & (recipient == "couple") & (reason_for_one_individual == "one is away" | reason_for_one_individual == "other reason"))
ss <- ss[c("hhid","recipient",  "person_interviewed")]
library(foreign)
gps <- read.dta("/home/bjvca/data/projects/digital green/baseline/DLEC.dta")[c("maize_gps_latitude", "maizephone_number", "maize_gps_longitude","district",             "sub","village",  "hhid" ,"hh_name", "name")  ]
m_ss <- merge(gps,ss, by="hhid")
###merge in names of gardens
m_ss <- merge(m_ss, read.dta13("/home/bjvca/data/projects/digital green/endline/data/raw/ALLDATA3.dta")[c("hhid","plot_no","garden1","garden2","garden3","garden4","garden5")], by="hhid")
write.csv(m_ss,"/home/bjvca/data/projects/digital green/endline/revisits/revisit_list4.csv")




#coords <- m_ss[c( "maize_gps_latitude" , "maize_gps_longitude","recipient") ]
#names(coords) <- c("lat","long","treat")
#coords$lat <- as.numeric(as.character(coords$lat))
#coords$long <- as.numeric(as.character(coords$long))
#coords$treat <- as.character(coords$treat)
#coords <- coords[complete.cases(coords),]
#write.table(coords,sep=",",row.names=FALSE, col.names=FALSE,"/home/bjvca/data/projects/digital green/endline/revisits/coords.csv")
