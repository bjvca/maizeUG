### prepare public data for ivr callers
dta <- read.dta("/home/bjvca/data/projects/digital green/endline/data/raw/call_log_merge.dta")
dta$called <- "yes"
dta$phonenumber <- NULL
write.csv(dta, "/home/bjvca/data/projects/digital green/endline/data/call_log_pub.csv")

### prepare public data for sms
### read in sms log
sms_log <- read.csv("/home/bjvca/data/projects/digital green/endline/data/raw/sms_log.csv")[c("Subscriber.Phone", "Scheduled.Date", "Status")]
      
## only keep the ones that were delivered
sms_log <- subset(sms_log, Status == "Finished (complete)")

## get IDS of phone numbers for merging log to dataset
IDs <- read.csv("/home/bjvca/data/projects/digital green/baseline/tel.csv")[c("HHID","tel")]
IDs$tel <- paste("256",IDs$tel, sep="")
sms_log <- merge(IDs, sms_log, by.x = "tel", by.y="Subscriber.Phone")
sms_log <- reshape(sms_log,v.names = "Scheduled.Date", idvar = "HHID",timevar="Scheduled.Date", direction = "wide")

sms_log$rec_weed_third_8 <- !is.na(sms_log$"Scheduled.Date.2017-10-17") | !is.na(sms_log$"Scheduled.Date.2017-10-25")
sms_log$rec_urea_7 <- !is.na(sms_log$"Scheduled.Date.2017-09-26") 
sms_log$rec_weed_second_6 <- !is.na(sms_log$"Scheduled.Date.2017-09-19") 
sms_log$rec_striga_5 <- !is.na(sms_log$"Scheduled.Date.2017-09-14") 
sms_log$rec_weed_first_4 <- !is.na(sms_log$"Scheduled.Date.2017-09-12")  | !is.na(sms_log$"Scheduled.Date.2017-09-13") ### the sms guys made a big mistake here, about 600 households got sms 3 twice and did not get 4
sms_log$rec_seed_1 <- !is.na(sms_log$"Scheduled.Date.2017-08-29") | !is.na(sms_log$"Scheduled.Date.2017-08-31") 
sms_log$rec_spacing_2 <- !is.na(sms_log$"Scheduled.Date.2017-09-05") |  !is.na(sms_log$"Scheduled.Date.2017-09-06")
sms_log$rec_gapfill_3 <- !is.na(sms_log$"Scheduled.Date.2017-09-07") | !is.na(sms_log$"Scheduled.Date.2017-09-08") | !is.na(sms_log$"Scheduled.Date.2017-09-11")

sms_log <- sms_log[c("HHID","rec_seed_1", "rec_spacing_2" ,"rec_gapfill_3", "rec_weed_first_4", "rec_striga_5", "rec_weed_second_6", "rec_urea_7", "rec_weed_third_8")] 
sms_log$totsms <- rowSums(sms_log[2:9])
write.csv(sms_log, "/home/bjvca/data/projects/digital green/endline/data/sms_log_pub.csv")
