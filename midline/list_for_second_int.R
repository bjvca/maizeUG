library(foreign)
dta <- read.dta("/home/bjvca/data/projects/digital green/baseline/DLECv2.dta")
### drop the femheaded
dta <- subset(dta, femalehead != 1)

dta$hh_name2 <- toupper(dta$hh_name2)

dta$hh_name2  <- toupper(trimws(dta$hh_name2))
## remove also repeated <space> in between first and second name
dta$hh_name2 <- gsub('([ ])\\1+', '\\1', dta$hh_name2 )

dta$hh_name2 [dta$hh_name2 == "N/A"] <- NA

dta$head_name[!is.na(dta$hh_name2)] <- dta$hh_name2[!is.na(dta$hh_name2)]
 
dta$spouse_name[dta$spouse_name == "n/a"] <- NA
dta$name[dta$name == "n/a"] <- NA


dta$spouse_name  <- toupper(trimws(dta$spouse_name))
## remove also repeated <space> in between first and second name
dta$spouse_name <- gsub('([ ])\\1+', '\\1', dta$spouse_name )

dta$name  <- toupper(trimws(dta$name))
## remove also repeated <space> in between first and second name
dta$name <- gsub('([ ])\\1+', '\\1', dta$name )

dta$spouse_name[!is.na(dta$name)] <- dta$name[!is.na(dta$name)]
all <- dta[c(  "dist","sc", "parish", "village","hhid", "head_name", "spouse_name","messenger","recipient", "team","tel_contact","maizephone_number","maizeq26b" )]
all$maizephone_number <- as.numeric(as.character(all$maizephone_number))
all$maizeq26b <- as.numeric(as.character(all$maizeq26b))

all[all == "n/a"] <- NA
all[all == "999"] <- NA
names(all) <- c(  "dist","sc", "parish", "village","hhid", "head_name", "spouse_name","messenger","recipient","team", "tel_contact1","tel_contact2","tel_contact3" )

write.csv(all,"/home/bjvca/data/projects/digital green/midline/list_sec.csv",row.names=F)

#### now for females
library(foreign)
dta <- read.dta("/home/bjvca/data/projects/digital green/baseline/DLECv2.dta")
### drop the femheaded
dta <- subset(dta, femalehead == 1)

dta$hh_name2 <- toupper(dta$hh_name2)

dta$hh_name2  <- toupper(trimws(dta$hh_name2))
## remove also repeated <space> in between first and second name
dta$hh_name2 <- gsub('([ ])\\1+', '\\1', dta$hh_name2 )

dta$hh_name2 [dta$hh_name2 == "N/A"] <- NA

dta$head_name[!is.na(dta$hh_name2)] <- dta$hh_name2[!is.na(dta$hh_name2)]
all <- dta[c(  "dist","sc", "parish", "village","hhid", "head_name","messenger", "team","tel_contact","maizephone_number","maizeq26b" )]
all$maizephone_number <- as.numeric(as.character(all$maizephone_number))
all$maizeq26b <- as.numeric(as.character(all$maizeq26b))

all[all == "n/a"] <- NA
all[all == "999"] <- NA
names(all) <- c(  "dist","sc", "parish", "village","hhid", "head_name","messenger","team", "tel_contact1","tel_contact2","tel_contact3" )

write.csv(all,"/home/bjvca/data/projects/digital green/midline/list_sec_femhead.csv",row.names=F)


