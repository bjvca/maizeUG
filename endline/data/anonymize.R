### takes the raw data and puts it in CSV that can go on github
rm(list=ls())

library(readstata13)
#dta <- read.dta13("/home/bjvca/data/projects/digital green/endline/data/raw/ALLDATA.dta")
#dta <- read.dta13("/home/bjvca/data/projects/digital green/endline/data/raw/ALLDATA2.dta")
dta <- read.dta13("/home/bjvca/data/projects/digital green/endline/data/raw/ALLDATA3.dta")

### prepare for public release - make anonymous
dta$enumerator <- as.numeric(as.factor(dta$enumerator))
dta$start <- NULL
dta$end <- NULL
dta$deviceid  <- NULL
dta$subscriberid   <- NULL       
dta$simserial  <- NULL
dta$phonenumber <- NULL
dta$hh_name <- NULL

dta$hh_name2 <- NULL
dta$maizephone_number <- NULL

### use numeric codes for the districts, subcounties and villages
###in each district, apply unique number to a subcounty

i_dist <- 1
dta$distID <- NULL
dta$subID <- NULL
dta$vilID <- NULL

dta[dta == 8888] <- NA
dta[dta == "n/a"] <- NA
dta[dta == ""] <- NA

for (dist in names(table(dta$district))) {
	print(dist)
	i_sub <- 1
	for (sub in names(table(dta$sub[dta$district==dist]))) {
		print(sub)
			i_village <- 1
			for (village in names(table(dta$village[dta$district == dist & dta$sub == sub]))) {
				print(village)
				dta$vilID[dta$district == dist & dta$sub == sub & dta$village == village] <- i_village
				i_village <- i_village + 1
			}
		dta$subID[dta$district == dist & dta$sub == sub] <- i_sub
		i_sub <- i_sub + 1
	}
dta$distID[dta$district==dist] <- i_dist
i_dist <- i_dist + 1
}

dta$distID <- as.numeric(dta$distID)
dta$subID <- as.numeric(dta$subID)
dta$vilID <- as.numeric(dta$vilID)

dta$district <- NULL
dta$dist <- NULL
dta$sub <- NULL
dta$sc <- NULL
dta$parish <- NULL
dta$village <- NULL

## remove names of the gardens
dta$garden1 <- NULL
dta$garden2 <- NULL
dta$garden3 <- NULL
dta$garden4 <- NULL
dta$garden5 <- NULL

###GPS location - but does not seem to be measured 
dta$gps <- NULL           
dta$gps_latitude  <- NULL   
dta$gps_longitude  <- NULL  
dta$gps_altitude  <- NULL  
dta$gps_precision   <- NULL 
dta$sp_name <- NULL
dta$number_village_hhs <- NULL
dta$Expnumber_village_hhs <- NULL

dta$team <- NULL

write.csv(dta,"/home/bjvca/data/projects/digital green/endline/data/endline.csv")

