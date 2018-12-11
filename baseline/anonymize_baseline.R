library(foreign)
dta <- read.dta("/home/bjvca/data/projects/digital green/baseline/DLEC.dta")
### prepare for public release - make anonymous



dta$enumerator <- as.numeric(as.factor(dta$enumerator))
  
 
dta$hh_name <- NULL
dta$spouse_name <- NULL
dta$name <- NULL
dta$hh_name2 <- NULL
dta$maizephone_number <- NULL
dta$maizeq26b <- NULL

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
dta$sub <- NULL
dta$village <- NULL

###GPS location - but does not seem to be measured 
dta$maizegps <- NULL           
dta$maize_gps_latitude  <- NULL   
dta$maize_gps_longitude  <- NULL  
dta$maize_gps_altitude  <- NULL  
dta$maize_gps_precision   <- NULL 

write.csv(dta,"/home/bjvca/data/projects/digital green/baseline/baseline.csv")

