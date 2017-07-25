###get raw listing data as generated from the ODK listing app
dta <- read.csv("/home/bjvca/data/projects/digital green/sampling/raw_listing/Maize_sampling_final.csv")

dta_1 <- dta[c("district","sub","village","HH.1..hhh_name", "HH.1..phone")]
names(dta_1) <- c("district","sc","village","head_name", "tel_contact")

dta_2 <- dta[c("district","sub","village","HH.2..hhh_name", "HH.2..phone")]
names(dta_2) <- c("district","sc","village","head_name", "tel_contact")

dta_3 <- dta[c("district","sub","village","HH.3..hhh_name", "HH.3..phone")]
names(dta_3) <- c("district","sc","village","head_name", "tel_contact")

dta_4 <- dta[c("district","sub","village","HH.4..hhh_name", "HH.4..phone")]
names(dta_4) <- c("district","sc","village","head_name", "tel_contact")

dta_5 <- dta[c("district","sub","village","HH.5..hhh_name", "HH.5..phone")]
names(dta_5) <- c("district","sc","village","head_name", "tel_contact")

dta_6 <- dta[c("district","sub","village","HH.6..hhh_name", "HH.6..phone")]
names(dta_6) <- c("district","sc","village","head_name", "tel_contact")

dta_7 <- dta[c("district","sub","village","HH.7..hhh_name", "HH.7..phone")]
names(dta_7) <- c("district","sc","village","head_name", "tel_contact")

dta_8 <- dta[c("district","sub","village","HH.8..hhh_name", "HH.8..phone")]
names(dta_8) <- c("district","sc","village","head_name", "tel_contact")

dta_9 <- dta[c("district","sub","village","HH.9..hhh_name", "HH.9..phone")]
names(dta_9) <- c("district","sc","village","head_name", "tel_contact")

dta_10 <- dta[c("district","sub","village","HH.10..hhh_name", "HH.10..phone")]
names(dta_10) <- c("district","sc","village","head_name", "tel_contact")

dta_11 <- dta[c("district","sub","village","HH.11..hhh_name", "HH.11..phone")]
names(dta_11) <- c("district","sc","village","head_name", "tel_contact")

dta_long <- rbind(dta_1, dta_2, dta_3, dta_4, dta_5, dta_6, dta_7, dta_8, dta_9, dta_10, dta_11)

sampling_list <- read.csv("/home/bjvca/data/projects/digital green/sampling/sampling_list_ID.csv")
#are all villages uniquely identified by district, sub-county, village?
sum(table(paste(paste(sampling_list$district,sampling_list$sc, sep="-"),sampling_list$village, sep="-"))> 10)

sampling_list$head_name <- as.character(sampling_list$head_name)
#yes

reserve_list <- as.data.frame(matrix(ncol=5, nrow=1))
names(reserve_list) <- c("district", "sc", "village", "head_name","tel_contact")

for (district in levels(droplevels(sampling_list$district))) {
for (sc in levels(droplevels(sampling_list$sc[sampling_list$district == district]))) {
for (village in levels(droplevels(sampling_list$village[sampling_list$district == district & sampling_list$sc == sc]))) {
print(district)
print(sc)
print(village)
if (is.na(sampling_list$head_name[sampling_list$sc == sc & sampling_list$village == village & sampling_list$district == district])[1]) {
sampling_list$head_name[sampling_list$sc == sc & sampling_list$village == village & sampling_list$district == district] <-  as.character(dta_long$head_name[dta_long$sc == sc & dta_long$village == village & dta_long$district == district])[1:length(sampling_list$head_name[sampling_list$sc == sc & sampling_list$village == village & sampling_list$district == district])]

sampling_list$tel_contact[sampling_list$sc == sc & sampling_list$village == village & sampling_list$district == district] <-  as.character(dta_long$tel_contact[dta_long$sc == sc & dta_long$village == village & dta_long$district == district])[1:length(sampling_list$tel_contact[sampling_list$sc == sc & sampling_list$village == village & sampling_list$district == district])] 

reserve_list <- rbind(reserve_list, tail(dta_long[dta_long$sc == sc & dta_long$village == village & dta_long$district == district,],11- length(sampling_list$tel_contact[sampling_list$sc == sc & sampling_list$village == village & sampling_list$district == district])))
}
}
}
}
res_nam <- read.csv( "/home/bjvca/data/projects/digital green/sampling/reserve_list_namayingo.csv")
res_nam$X <- NULL
names(res_nam) <- c("head_name","parish","village", "district")
res_nam$sc <- NA
res_nam$tel_contact <- NA
res_nam <- res_nam[c("district", "sc","parish", "village","head_name","tel_contact")]
res_nam <- res_nam[2:length(res_nam$district),]

reserve_list$parish <- NA
reserve_list <- reserve_list[2:length(reserve_list$district),]
reserve_list <- reserve_list[c("district", "sc","parish", "village","head_name","tel_contact")]

reserve_list <- rbind(reserve_list, res_nam)

reserve_list <- reserve_list[order(reserve_list$district,reserve_list$sc,reserve_list$parish, reserve_list$village, reserve_list$head_name),]
sampling_list <- sampling_list[order(sampling_list$district,sampling_list$sc,sampling_list$parish, sampling_list$village, sampling_list$head_name),]
## remove trailing and leading spaces and put in CAPS
sampling_list$head_name <- toupper(trimws(sampling_list$head_name))
## remove also repeated <space> in between first and second name
sampling_list$head_name <- gsub('([ ])\\1+', '\\1', sampling_list$head_name)

## and some final changes


sampling_list$X.1 <- NULL
sampling_list$X <- paste("HH",sampling_list$X, sep="")
names(sampling_list)[1] <- "HHID"
sampling_list$tel_contact[is.na(sampling_list$tel_contact)] <- 999
# two more missing, taken from reserve list
sampling_list$head_name[sampling_list$HHID == "HH2275"] <- "WANDERA STEPHEN"
sampling_list$head_name[sampling_list$HHID == "HH2392"] <- "MATENDE GODFREY"


sampling_list <- sampling_list[order(sampling_list$district,sampling_list$sc,sampling_list$parish,sampling_list$village),]
write.csv(sampling_list, "/home/bjvca/data/projects/digital green/sampling/sampling_list_ID.csv",  row.names = FALSE)
## remove trailing and leading spaces and put in CAPS
reserve_list$head_name <- toupper(trimws(reserve_list$head_name))
## remove also repeated <space> in between first and second name
reserve_list$head_name <- gsub('([ ])\\1+', '\\1', reserve_list$head_name)

reserve_list <- reserve_list[order(reserve_list$district,reserve_list$sc,reserve_list$village),]

write.csv(reserve_list, "/home/bjvca/data/projects/digital green/sampling/reserve_list_ID.csv",  row.names = FALSE)
###now prepare sampling list for femheaded households
femhead_list <- dta[c("district","sub","village", "fem_hh", "fem_hh_phone")]
names(femhead_list)  <- c("district","sc","village","head_name", "tel_contact")
##merge in parsihes from sampling list
femhead_list <- merge(femhead_list, sampling_list[c("district","sc","parish","village")], by=c("district","sc","village"))

femhead_list <- femhead_list[!duplicated(femhead_list), ]
femhead_list <- femhead_list[c("district","sc","parish","village", "head_name", "tel_contact") ]
femhead_list <- rbind(femhead_list, read.csv("/home/bjvca/data/projects/digital green/sampling/femhead_list_namayingo.csv")[c("district","sc","parish","village", "head_name", "tel_contact") ])

## remove trailing and leading spaces and put in CAPS
femhead_list$head_name <- toupper(trimws(femhead_list$head_name))
## remove also repeated <space> in between first and second name
femhead_list$head_name <- gsub('([ ])\\1+', '\\1', femhead_list$head_name)

## shuffle
femhead_list <- femhead_list[sample(nrow(femhead_list),nrow(femhead_list), replace=F),]
# first half of sampel get to see male video, others the female video
femhead_list$messenger <- "female"
femhead_list$messenger[1:(nrow(femhead_list)/2)] <- "male"
## mix in IVR
femhead_list$IVR <- "no"
femhead_list$IVR[femhead_list$messenger == "female"][1:(floor(length(femhead_list$IVR[femhead_list$messenger == "female"])/2))] <- "yes"
femhead_list$IVR[femhead_list$messenger == "male"][1:(floor(length(femhead_list$IVR[femhead_list$messenger == "male"])/2))] <- "yes"
femhead_list$HHID <- paste("FH",row.names(femhead_list),sep="")
femhead_list <- femhead_list[c("HHID", "district", "sc", "parish", "village", "head_name", "tel_contact", "messenger", "IVR")]
femhead_list$tel_contact[is.na(femhead_list$tel_contact)] <- 999

femhead_list <- femhead_list[order(femhead_list$district,femhead_list$sc,femhead_list$parish,femhead_list$village),]
write.csv(femhead_list, "/home/bjvca/data/projects/digital green/sampling/femhead_list_ID.csv",  row.names = FALSE)










