#make time preparation

dta$time_prep_man_man_pl1  <- 0
dta$time_prep_man_man_pl1[dta$person_interviewed == "man"] <- dta$time_prep_pl1_sp1_self[dta$person_interviewed == "man"]
dta$time_prep_man_man_pl1[dta$person_interviewed == "woman"] <- dta$time_prep_pl1_sp2_self[dta$person_interviewed == "woman"]

dta$time_prep_woman_man_pl1 <- 0
dta$time_prep_woman_man_pl1[dta$person_interviewed == "man"] <- dta$time_prep_pl1_sp1_other[dta$person_interviewed == "man"]
dta$time_prep_woman_man_pl1[dta$person_interviewed == "woman"] <- dta$time_prep_pl1_sp2_other[dta$person_interviewed == "woman"]

dta$time_prep_man_woman_pl1 <- 0
dta$time_prep_man_woman_pl1[dta$person_interviewed == "man"] <- dta$time_prep_pl1_sp2_other[dta$person_interviewed == "man"]
dta$time_prep_man_woman_pl1[dta$person_interviewed == "woman"] <- dta$time_prep_pl1_sp1_other[dta$person_interviewed == "woman"]

dta$time_prep_woman_woman_pl1 <- 0
dta$time_prep_woman_woman_pl1[dta$person_interviewed == "man"] <- dta$time_prep_pl1_sp2_self[dta$person_interviewed == "man"]
dta$time_prep_woman_woman_pl1[dta$person_interviewed == "woman"] <- dta$time_prep_pl1_sp1_self[dta$person_interviewed == "woman"]

#polt2
dta$time_prep_man_man_pl2  <- 0
dta$time_prep_man_man_pl2[dta$person_interviewed == "man"] <- dta$time_prep_pl2_sp1_self[dta$person_interviewed == "man"]
dta$time_prep_man_man_pl2[dta$person_interviewed == "woman"] <- dta$time_prep_pl2_sp2_self[dta$person_interviewed == "woman"]

dta$time_prep_woman_man_pl2 <- 0
dta$time_prep_woman_man_pl2[dta$person_interviewed == "man"] <- dta$time_prep_pl2_sp1_other[dta$person_interviewed == "man"]
dta$time_prep_woman_man_pl2[dta$person_interviewed == "woman"] <- dta$time_prep_pl2_sp2_other[dta$person_interviewed == "woman"]

dta$time_prep_man_woman_pl2 <- 0
dta$time_prep_man_woman_pl2[dta$person_interviewed == "man"] <- dta$time_prep_pl2_sp2_other[dta$person_interviewed == "man"]
dta$time_prep_man_woman_pl2[dta$person_interviewed == "woman"] <- dta$time_prep_pl2_sp1_other[dta$person_interviewed == "woman"]

dta$time_prep_woman_woman_pl2 <- 0
dta$time_prep_woman_woman_pl2[dta$person_interviewed == "man"] <- dta$time_prep_pl2_sp2_self[dta$person_interviewed == "man"]
dta$time_prep_woman_woman_pl2[dta$person_interviewed == "woman"] <- dta$time_prep_pl2_sp1_self[dta$person_interviewed == "woman"]

#polt3
dta$time_prep_man_man_pl3  <- 0
dta$time_prep_man_man_pl3[dta$person_interviewed == "man"] <- dta$time_prep_pl3_sp1_self[dta$person_interviewed == "man"]
dta$time_prep_man_man_pl3[dta$person_interviewed == "woman"] <- dta$time_prep_pl3_sp2_self[dta$person_interviewed == "woman"]

dta$time_prep_woman_man_pl3 <- 0
dta$time_prep_woman_man_pl3[dta$person_interviewed == "man"] <- dta$time_prep_pl3_sp1_other[dta$person_interviewed == "man"]
dta$time_prep_woman_man_pl3[dta$person_interviewed == "woman"] <- dta$time_prep_pl3_sp2_other[dta$person_interviewed == "woman"]

dta$time_prep_man_woman_pl3 <- 0
dta$time_prep_man_woman_pl3[dta$person_interviewed == "man"] <- dta$time_prep_pl3_sp2_other[dta$person_interviewed == "man"]
dta$time_prep_man_woman_pl3[dta$person_interviewed == "woman"] <- dta$time_prep_pl3_sp1_other[dta$person_interviewed == "woman"]

dta$time_prep_woman_woman_pl3 <- 0
dta$time_prep_woman_woman_pl3[dta$person_interviewed == "man"] <- dta$time_prep_pl3_sp2_self[dta$person_interviewed == "man"]
dta$time_prep_woman_woman_pl3[dta$person_interviewed == "woman"] <- dta$time_prep_pl3_sp1_self[dta$person_interviewed == "woman"]

#polt3
dta$time_prep_man_man_pl4  <- 0
dta$time_prep_man_man_pl4[dta$person_interviewed == "man"] <- dta$time_prep_pl4_sp1_self[dta$person_interviewed == "man"]
dta$time_prep_man_man_pl4[dta$person_interviewed == "woman"] <- dta$time_prep_pl4_sp2_self[dta$person_interviewed == "woman"]

dta$time_prep_woman_man_pl4 <- 0
dta$time_prep_woman_man_pl4[dta$person_interviewed == "man"] <- dta$time_prep_pl4_sp1_other[dta$person_interviewed == "man"]
dta$time_prep_woman_man_pl4[dta$person_interviewed == "woman"] <- dta$time_prep_pl4_sp2_other[dta$person_interviewed == "woman"]

dta$time_prep_man_woman_pl4 <- 0
dta$time_prep_man_woman_pl4[dta$person_interviewed == "man"] <- dta$time_prep_pl4_sp2_other[dta$person_interviewed == "man"]
dta$time_prep_man_woman_pl4[dta$person_interviewed == "woman"] <- dta$time_prep_pl4_sp1_other[dta$person_interviewed == "woman"]

dta$time_prep_woman_woman_pl4 <- 0
dta$time_prep_woman_woman_pl4[dta$person_interviewed == "man"] <- dta$time_prep_pl4_sp2_self[dta$person_interviewed == "man"]
dta$time_prep_woman_woman_pl4[dta$person_interviewed == "woman"] <- dta$time_prep_pl4_sp1_self[dta$person_interviewed == "woman"]

#polt3
dta$time_prep_man_man_pl5  <- 0
dta$time_prep_man_man_pl5[dta$person_interviewed == "man"] <- dta$time_prep_pl5_sp1_self[dta$person_interviewed == "man"]
dta$time_prep_man_man_pl5[dta$person_interviewed == "woman"] <- dta$time_prep_pl5_sp2_self[dta$person_interviewed == "woman"]

dta$time_prep_woman_man_pl5 <- 0
dta$time_prep_woman_man_pl5[dta$person_interviewed == "man"] <- dta$time_prep_pl5_sp1_other[dta$person_interviewed == "man"]
dta$time_prep_woman_man_pl5[dta$person_interviewed == "woman"] <- dta$time_prep_pl5_sp2_other[dta$person_interviewed == "woman"]

dta$time_prep_man_woman_pl5 <- 0
dta$time_prep_man_woman_pl5[dta$person_interviewed == "man"] <- dta$time_prep_pl5_sp2_other[dta$person_interviewed == "man"]
dta$time_prep_man_woman_pl5[dta$person_interviewed == "woman"] <- dta$time_prep_pl5_sp1_other[dta$person_interviewed == "woman"]

dta$time_prep_woman_woman_pl5 <- 0
dta$time_prep_woman_woman_pl5[dta$person_interviewed == "man"] <- dta$time_prep_pl5_sp2_self[dta$person_interviewed == "man"]
dta$time_prep_woman_woman_pl5[dta$person_interviewed == "woman"] <- dta$time_prep_pl5_sp1_self[dta$person_interviewed == "woman"]
