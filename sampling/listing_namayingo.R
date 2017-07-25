dta <- read.csv("/home/bjvca/data/projects/digital green/sampling/namayingo_vht_listing.csv")
set.seed(12345)
dta$namehousehold <- toupper(trimws(dta$namehousehold))
dta$parishes <- toupper(dta$parishes)
dta$villages <- toupper(dta$villages)
dta <- subset(dta, !is.na(parishes))



### split masodi in two
dta$villages[dta$parishes == "NANSUMA" & dta$villages == "MASODI"] <- "MASODI B"
dta$villages[dta$parishes == "NANSUMA" & dta$villages == "MASODI B"][1:117] <- "MASODI A"
### split bumoli in two
dta$villages[dta$parishes == "NANSUMA" & dta$villages == "BUMOLI"] <- "BUMOLI B"
dta$villages[dta$parishes == "NANSUMA" & dta$villages == "BUMOLI B"][1:139] <- "BUMOLI A"

### get selected parishes
sampling_list <- read.csv("/home/bjvca/data/projects/digital green/sampling/sampling_list.csv")

list <- subset(sampling_list, district == "NAMAYINGO")
reserve_list = as.data.frame(matrix(ncol=3, nrow=1))
names(reserve_list) = c("namehousehold", "parishes", "villages")

for (parish in levels(droplevels(list$parish))) {

for (village in levels(droplevels(list$village[list$parish == parish]))) {
print(parish)
print(village)
if (dim(dta[dta$parishes == parish & dta$villages == village & trimws(dta$gender) == "m",])[1] > 0) {
list_constr <-  dta[dta$parishes == parish & dta$villages == village & trimws(dta$gender) == "m",]
### remove duplicates in terms of names of this list
list_constr <- list_constr[!duplicated(list_constr$namehousehold),]
list_constr <- list_constr[sample(nrow(list_constr),15),][c("namehousehold","parishes","villages")]
### merge first X to sampling_list.csv
 sampling_list$head_name[sampling_list$parish == parish & sampling_list$village == village & sampling_list$district == "NAMAYINGO"] <-  list_constr$namehousehold[1:length( sampling_list$head_name[sampling_list$parish == parish & sampling_list$village == village & sampling_list$district == "NAMAYINGO"])]
reserve_list <- rbind(reserve_list, tail(list_constr,4))
}
}
}
reserve_list$district <- "NAMAYINGO"

write.csv(sampling_list, "/home/bjvca/data/projects/digital green/sampling/sampling_list_ID.csv")
write.csv(reserve_list, "/home/bjvca/data/projects/digital green/sampling/reserve_list_namayingo.csv")

### now construct female headed list
sampling_list <- read.csv("/home/bjvca/data/projects/digital green/sampling/sampling_list.csv")

list <- subset(sampling_list, district == "NAMAYINGO")
for (parish in levels(droplevels(list$parish))) {

for (village in levels(droplevels(list$village[list$parish == parish]))) {
print(parish)
print(village)
if (dim(dta[dta$parishes == parish & dta$villages == village & trimws(dta$gender) == "f",])[1] > 0) {
list_constr <-  dta[dta$parishes == parish & dta$villages == village & trimws(dta$gender) == "f",]
### remove duplicates in terms of names of this list
list_constr <- list_constr[!duplicated(list_constr$namehousehold),]
list_constr <- list_constr[sample(nrow(list_constr),1),][c("namehousehold","parishes","villages")]
### merge first X to sampling_list.csv
 sampling_list$head_name[sampling_list$parish == parish & sampling_list$village == village & sampling_list$district == "NAMAYINGO"] <-  list_constr$namehousehold[1:length( sampling_list$head_name[sampling_list$parish == parish & sampling_list$village == village & sampling_list$district == "NAMAYINGO"])]
}
}
}

femhead_list <- sampling_list[!is.na(sampling_list$head_name),]
femhead_list <- femhead_list[c("district","sc","parish","village","head_name","tel_contact")]
write.csv(femhead_list, "/home/bjvca/data/projects/digital green/sampling/femhead_list_namayingo.csv")



