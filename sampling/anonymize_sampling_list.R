##original sampling list is later used to merge in treatment - so make it anonymous to push on github
dta <- read.csv("/home/bjvca/data/projects/digital green/sampling/sampling_list_ID.csv")
dta$head_name <- NULL 
dta$tel_contact <- NULL

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
	for (sub in names(table(dta$sc[dta$district==dist]))) {
		print(sub)
			i_village <- 1
			for (village in names(table(dta$village[dta$district == dist & dta$sub == sub]))) {
				print(village)
				dta$vilID[dta$district == dist & dta$sc == sub & dta$village == village] <- i_village
				i_village <- i_village + 1
			}
		dta$subID[dta$district == dist & dta$sc == sub] <- i_sub
		i_sub <- i_sub + 1
	}
dta$distID[dta$district==dist] <- i_dist
i_dist <- i_dist + 1
}

dta$distID <- as.numeric(dta$distID)
dta$subID <- as.numeric(dta$subID)
dta$vilID <- as.numeric(dta$vilID)

dta$district <- NULL
dta$sc <- NULL
dta$village <- NULL
dta$parish <- NULL
write.csv(dta,"/home/bjvca/data/projects/digital green/sampling/sampling_list_pub.csv")

dta <- read.csv("/home/bjvca/data/projects/digital green/sampling/femhead_list_ID.csv")
dta$head_name <- NULL 
dta$tel_contact <- NULL

## use numeric codes for the districts, subcounties and villages
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
	for (sub in names(table(dta$sc[dta$district==dist]))) {
		print(sub)
			i_village <- 1
			for (village in names(table(dta$village[dta$district == dist & dta$sub == sub]))) {
				print(village)
				dta$vilID[dta$district == dist & dta$sc == sub & dta$village == village] <- i_village
				i_village <- i_village + 1
			}
		dta$subID[dta$district == dist & dta$sc == sub] <- i_sub
		i_sub <- i_sub + 1
	}
dta$distID[dta$district==dist] <- i_dist
i_dist <- i_dist + 1
}

dta$distID <- as.numeric(dta$distID)
dta$subID <- as.numeric(dta$subID)
dta$vilID <- as.numeric(dta$vilID)

dta$district <- NULL
dta$sc <- NULL
dta$village <- NULL
dta$parish <- NULL
write.csv(dta,"/home/bjvca/data/projects/digital green/sampling/femhead_list_pub.csv")
