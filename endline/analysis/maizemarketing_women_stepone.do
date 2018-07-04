**Uganda RCT

clear
eststo clear


**Generate outcome variables for hh level data set


*gl dir  "C:\Users\u0107600\" 
gl dir  C:\Users\Liz\

*Load Data
		use "$dir\Dropbox\Uganda-ICT\maizeUG-master\endline\analysis\endline_indiv_destring_kgmaize.dta", clear
		
		
cd "$dir\Dropbox\Uganda-ICT\maizeUG-master\endline\analysis\tables"


	/*
	INDIVIDUAL
	
	FIRST SAMPLE--> ALL: WHETHER OR NOT THEY SOLD ANYTHING
	If nothing sold then 0 
	
	
	
	
	GENERATE ALL VARS AS EQUAL TO ZERO THEN REPLACE WITH AMOUNTS
	
	*/
	
	
				*Rename variables--these are number of BAGS sold
			
		rename q70 seed_sved
		rename q72 amt_sold_solo
		rename q81 amt_sold_else
		*rename spouse2r72 spouse_amt_sold_solo
		*rename spouse2r81 spouse_amt_sold_else	
		rename q75 amt_sold_spouse
		*rename spouse2r75 spouse_amt_sold_spouse
		rename q78 amt_sold_joint
		*rename spouse2r78 spouse_amt_sold_joint
			
		foreach v of varlist amt_sold_solo amt_sold_else amt_sold_spouse amt_sold_joint grp1a17 grp2b17 grp3c17 grp4d17 grp5e17 	grp1f17 grp2g17 grp3h17 group_sp4j17 grp5_sp5k17{
		replace `v'=. if `v'==999
		}

		**Change missing amounts to 0 for analysis for ALL
		foreach v of varlist amt_sold_solo amt_sold_else amt_sold_spouse amt_sold_joint {
		replace `v'=0 if `v'==. 
		*replace `v'=0 if `v'==. & q71[_n-1]=="Yes" &hhid==hhid[_n-1]
		*replace `v'=0 if `v'==. & q71[_n+1]=="Yes" &hhid==hhid[_n+1]
		}
*	


		
		*Need to calc KGs per bag
				*  Q16 : How many bags		Q17 Kgs in a bag

		foreach v of varlist grp1a17 grp2b17 grp3c17 grp4d17 grp5e17 	grp1f17 grp2g17 grp3h17 group_sp4j17 grp5_sp5k17 grp1a16 grp2b16 grp3c16 grp4d16 grp5e16 grp1f16 grp2g16 grp3h16 group_sp4j16 grp5_sp5k16{
		replace `v'=0 if `v'==.
		}
		
		replace plot_no="." if plot_no=="NA"
		destring plot_no, replace
		sort hhid
		replace plot_no=plot_no[_n+1] if hhid==hhid[_n+1] & plot_no==.
			replace plot_no=0 if plot_no==.
	

	
	
		gen bag_avg=0
replace bag_avg= (grp1a17+grp2b17+grp3c17+grp4d17+grp5e17)/plot_no if v1!=. & plot_no!=0
replace bag_avg= (grp1f17 +grp2g17+ grp3h17 +group_sp4j17+ grp5_sp5k17)/plot_no if v1==. & plot_no!=0


			**generate amount sold in KG
			gen amt_sold_solo_kg= amt_sold_solo*bag_avg
			gen amt_sold_else_kg= amt_sold_else*bag_avg
			gen amt_sold_spouse_kg=amt_sold_spouse*bag_avg
			gen amt_sold_joint_kg= amt_sold_joint*bag_avg
		

**generate indicator for when 1 spouse reports selling maize and the other doesnt
gen soldyes_maize=.
replace soldyes_maize=0 
replace soldyes_maize=1 if q71=="Yes"

*if q71=="No"
tab soldyes_maize

sort hhid gender1
gen maize_disagree=.
replace maize_disagree=0 if soldyes_maize==soldyes_maize[_n-1] & hhid==hhid[_n-1] & soldyes_maize!=.
replace maize_disagree=0 if soldyes_maize==soldyes_maize[_n+1] & hhid==hhid[_n+1] & soldyes_maize!=.
replace maize_disagree=1 if soldyes_maize!=soldyes_maize[_n-1] & hhid==hhid[_n-1] & soldyes_maize!=.
replace maize_disagree=1 if soldyes_maize!=soldyes_maize[_n+1] & hhid==hhid[_n+1] & soldyes_maize!=.

tab maize_disagree

	*create female var
gen female=.
replace female=1 if gender1==2
replace female=0 if gender1==1

	
	


sort hhid female

	
			generate totalmaizesold=.
		replace totalmaizesold= amt_sold_solo+ amt_sold_spouse+amt_sold_joint+amt_sold_else
		
					generate totalmaizesoldkg=totalmaizesold*bag_avg

			
			gen prop_maizesold=0
			replace prop_maizesold=totalmaizesoldkg/maize_total_harv_kg if maize_total_harv_kg!=0
			
		**Prices
			gen price_sold_solo=0
		replace price_sold_solo=q73/(amt_sold_solo_kg)
		tab price_sold_solo
		
		gen price_sold_joint=0
		replace price_sold_joint=q79/(amt_sold_joint_kg)
		tab price_sold_joint
		
		gen price_sold_spouse=0
		replace price_sold_spouse=q76/(amt_sold_spouse_kg)
		tab price_sold_spouse

	
	*a) increase women’s involvement in maize sales?
			/*	Indicators
				-Women’s involvement in receiving income from selling maize 
				(amount wife sells personally + amount jointly sold)/ total amount of maize sales 
				   (you can base this either on wife reported or husband reported data) */
				   
				   
				**amount sold jointly should be the PROPORTION sold jointly of HH AMT sold
				gen amt_sold_joint_kg_prop=0
				replace amt_sold_joint_kg_prop= amt_sold_joint_kg/totalmaizesoldkg
				
				label variable amt_sold_joint_kg_prop "Amt Sold Jointly Prop."
				tab amt_sold_joint_kg_prop
				   
				  *maize sales participation by women
				   
						*generate women level maize sales indicator (reported by either
				*/	
				gen maize_sold_woman=0
				replace maize_sold_woman= amt_sold_solo_kg if female==1
				replace maize_sold_woman= amt_sold_spouse_kg if female==0

				gen maizesale_partici_woman=0
				replace maizesale_partici_woman= (maize_sold_woman+amt_sold_joint_kg)/totalmaizesoldkg
		   tab maizesale_partici_woman
				   
	*c) likelihood to sell through higher price channels? 
			/*	Indicators
			- to whom one is selling to (in principle: women may be less connected to higher bidders, or choose marketing 
			  channels that typically offer lower prices like small itinerant trader)
		*/

		*Can we quantify/ order the channels and make a continuous variable?

		sum q80*
		
		*sld to traders		    
	gen sold_traders=0
	replace sold_traders=1 if q801==1|q802==1
	gen sold_middle=0
	replace sold_middle=1 if q803==1

	
	
	*spending preferences
	
			*Create smaller spending categories
		
		gen pref_aginput=0
		replace pref_aginput=1 if spend_list4==1|spend_list3==1
		
	gen pref_publicgood=0
	replace pref_publicgood=1 if spend_list1==1|spend_list2==1
	
	gen pref_invest=0
	replace pref_invest=1 if spend_list5==1|spend_list6==1|spend_list8==1
		
	gen pref_total=pref_aginput+pref_publicgood+pref_invest


	
	
	/*
	*AGREEMENT
		gen pref_aginput_agree=0
		replace pref_aginput_agree=1 if pref_aginput==1&spouse_pref_aginput==1
		
	gen pref_publicgood_agree=0
	replace pref_publicgood_agree=1 if pref_publicgood==1&spouse_pref_publicgood==1
	
	gen pref_invest_agree=0
	replace pref_invest_agree=1 if pref_invest==1&spouse_pref_invest==1
	
	gen pref_agree= pref_aginput_agree + pref_publicgood_agree + pref_invest_agree
*/
								
				label variable amt_sold_solo_kg "Amt Sold Alone KG"
				label variable amt_sold_spouse_kg "Amt Sold Spouse KG"
				label variable amt_sold_joint_kg "Amt Sold Joint KG"
				label variable amt_sold_else_kg "Amt Sold Else KG"			
				label variable maizesale_partici "Maize Partic HH"	
						
				label variable price_sold_solo "Price Sold Alone"
				label variable price_sold_spouse "Price Sold Spouse"
				label variable price_sold_joint "Price Sold Joint"
				label variable q741 "Sold Solo Kenya Tdrs"
				label variable q742 "Sold Solo Uganda Tdrs"
				label variable q743 "Sold Solo Middle"
				label variable q744 "Sold Solo Mill"
				label variable q745 "Sold Solo Neigh"
				label variable q746 "Sold Solo Govt"
				label variable q771 "Spouse Sold Kenya Tdrs"
				label variable q772 "Spouse Sold Uganda Tdrs"
				label variable q773 "Spouse Sold Middle"
				label variable q774 "Spouse Sold Mill"
				label variable q775 "Spouse Sold Neigh"
				label variable q776 "Spouse Sold Govt"
				label variable q801 "Sold Joint Kenya Tdrs"
				label variable q802 "Sold Joint Uganda Tdrs"
				label variable q803 "Sold Joint Middle"
				label variable q804 "Sold Joint Mill"
				label variable q805 "Sold Joint Neigh"
				label variable q806 "Sold Joint Govt"
				label variable maize_total_harv_kg "Total Maize Harvested"
				label variable totalmaizesoldkg "Total Maize Sold (kg)"
	
	
			****************
			*  Regressions
			
			
			*fix  ids and create real village id
foreach v of varlist vilid subid distid{
replace `v'="." if `v'=="NA"
destring `v', replace
replace `v'=`v'[_n-1] if hhid==hhid[_n-1]
}

gen subid_add=subid*1000
gen distid_add=distid*100
gen vilid_final= subid_add+distid_add+vilid




order hhid gender1

*Make HHID non string
	gen hhid_byte=substr(hhid,3, 5)
	order hhid_byte, a(hhid)
	destring hhid_byte, replace
 
*drop control group
drop if messenger==0

*destring other factors 
gen ivr2=.
replace ivr2=0 if ivr=="no"
replace ivr2=1 if ivr=="yes"


gen sms2=.
replace sms2=0 if sms=="no"
replace sms2=1 if sms=="yes"


sort hhid female
replace ivr2=ivr2[_n-1] if ivr2==. & hhid==hhid[_n-1] 
replace ivr2=ivr2[_n+1] if ivr2==. & hhid==hhid[_n+1] 
replace sms2=sms2[_n-1] if sms2==. & hhid==hhid[_n-1] 
replace sms2=sms2[_n+1] if sms2==. & hhid==hhid[_n+1] 



	
***label variables
label variable female "Female"

label variable messenger " Messenger" 
label variable ivr2 "IVR"
label variable sms2 "SMS"

		
		gen woman_informed=.
		replace woman_informed=0 if recipient ==1
		replace woman_informed=1 if recipient==2|recipient==3
		label variable woman_informed "Woman Reci"
		
		gen couple_informed=.
		replace couple_informed=0 if recipient ==1|recipient==2
		replace couple_informed=1 if recipient==3
		label variable couple_informed "Couple Reci"	
		
		gen woman_messeng=.
		replace woman_messeng=0 if messenger==1
		replace woman_messeng=1 if messenger==2|messenger==3
		label variable woman_messeng "Woman Messeng"


		gen couple_messeng=.
		replace couple_messeng=0 if messenger==2|messenger==1
		replace couple_messeng=1 if messenger==3
		label variable couple_messeng "Couple Messeng"


	xtset vilid_final

	eststo clear
	
set seed 4562
	**equalize sample sizes within the woman_informed variable
preserve

sample 2118 if recipient==2, c

		foreach v of varlist  maize_total_harv_kg  totalmaizesoldkg prop_maizesold amt_sold_joint_kg_prop maizesale_partici_woman   price*    {
		xtreg `v' i.woman_informed i.messenger ivr2 sms2 if female==1, vce(cluster vilid_final)  fe
		eststo
}


		foreach v of varlist   sold_*   {
		xtlogit `v' i.woman_informed i.messenger ivr2 sms2 if female==1,   fe
		eststo
}


esttab using woman_informed_step1_female,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)



	
	eststo clear

		foreach v of varlist  maize_total_harv_kg totalmaizesoldkg prop_maizesold  amt_sold_joint_kg_prop maizesale_partici_woman   price*    {
		xtreg `v' i.woman_informed i.messenger ivr2 sms2 if female==0, vce(cluster vilid_final)  fe
		eststo
}


		foreach v of varlist   sold_*   {
		xtlogit `v' i.woman_informed i.messenger ivr2 sms2 if female==0,   fe
		eststo
}


esttab using woman_informed_step1_male,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)
	
	eststo clear
restore
exit
preserve

sample 2284 if recipient==2, c
	
	
	
	eststo clear


		foreach v of varlist  maize_total_harv_kg  totalmaizesoldkg prop_maizesold amt_sold_joint_kg_prop maizesale_partici_woman   price*    {
		xtreg `v' i.couple_informed i.messenger ivr2 sms2 if female==1, vce(cluster vilid_final)  fe
		eststo
}


		foreach v of varlist   sold_*   {
		xtlogit `v' i.couple_informed i.messenger ivr2 sms2 if female==1,   fe
		eststo
}


esttab using couple_informed_step1_female,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)


	eststo clear


		foreach v of varlist  maize_total_harv_kg  totalmaizesoldkg prop_maizesold amt_sold_joint_kg_prop maizesale_partici_woman   price*    {
		xtreg `v' i.couple_informed i.messenger ivr2 sms2 if female==0, vce(cluster vilid_final)  fe
		eststo
}


		foreach v of varlist   sold_*   {
		xtlogit `v' i.couple_informed i.messenger ivr2 sms2 if female==0,   fe
		eststo
}


esttab using couple_informed_step1_male,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)

restore

preserve


sample 2034 if messenger==2, c
		
	eststo clear


		foreach v of varlist  maize_total_harv_kg  totalmaizesoldkg prop_maizesold amt_sold_joint_kg_prop maizesale_partici_woman   price*    {
		xtreg `v' i.woman_messeng i.recipient ivr2 sms2 if female==1, vce(cluster vilid_final)  fe
		eststo
}


		foreach v of varlist   sold_*   {
		xtlogit `v' i.woman_messeng i.recipient ivr2 sms2 if female==1,   fe
		eststo
}

esttab using woman_messeng_step1_female,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)
	
	eststo clear

		foreach v of varlist  maize_total_harv_kg  totalmaizesoldkg prop_maizesold amt_sold_joint_kg_prop maizesale_partici_woman   price*    {
		xtreg `v' i.woman_messeng i.recipient ivr2 sms2 if female==0, vce(cluster vilid_final)  fe
		eststo
}

		foreach v of varlist   sold_*   {
		xtlogit `v' i.woman_messeng i.recipient ivr2 sms2 if female==0,   fe
		eststo
}

esttab using woman_messeng_step1_male,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)
	
restore	
	
	
preserve	
	
sample 2006 if messenger==2, c
	
		eststo clear


		foreach v of varlist  maize_total_harv_kg totalmaizesoldkg prop_maizesold  amt_sold_joint_kg_prop maizesale_partici_woman   price*   {
		xtreg `v' i.couple_messeng i.recipient ivr2 sms2 if female==1, vce(cluster vilid_final)  fe
		eststo
}
		foreach v of varlist   sold_*   {

		xtlogit `v' i.couple_messeng i.recipient ivr2 sms2 if female==1,   fe
		eststo
}
esttab using couple_messeng_step1_female,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)
	
	eststo clear

		foreach v of varlist  maize_total_harv_kg totalmaizesoldkg prop_maizesold  amt_sold_joint_kg_prop maizesale_partici_woman   price*    {
		xtreg `v' i.couple_messeng i.recipient ivr2 sms2 if female==0, vce(cluster vilid_final)  fe
		eststo
}

		foreach v of varlist   sold_*   {
		xtlogit `v' i.couple_messeng i.recipient ivr2 sms2 if female==0,   fe
		eststo
}

esttab using couple_messeng_step1_male,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)
	
	
restore	
	
	eststo clear
	
	
	
	***Spending Preferences
	
	
		**Agrement on spending Pref
	eststo clear

	
			foreach v of varlist  pref_aginput pref_publicgood pref_invest  {
			preserve
			sample 2118 if recipient==2, c

		xtlogit `v' ivr2 sms2 i.messenger woman_informed  if female==1,  fe
		eststo
		restore
		preserve
		sample 2284 if recipient==2, c

		xtlogit `v' couple_informed i.messenger ivr2 sms2 if female==1,  fe
		eststo
		restore
		preserve
		sample 2034 if messenger==2, c

		xtlogit `v' i.recipient woman_messeng  ivr2 sms2 if female==1,   fe
		eststo
		
		restore
		preserve
		sample 2006 if messenger==2, c

		xtlogit `v' couple_messeng i.recipient ivr2 sms2 if female==1,   fe
		eststo		
		restore	
		
}		
	esttab using pref_publicgood_female,  replace  tex fragment  order(woman_informed couple_informed woman_messeng couple_messeng)   obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)
	
	
	
	eststo clear
			foreach v of varlist  pref_aginput pref_publicgood pref_invest  {
				preserve
			sample 2118 if recipient==2, c
		
		xtlogit `v' ivr2 sms2 i.messenger woman_informed  if female==0,  fe
		eststo
		restore
		preserve
		sample 2284 if recipient==2, c
		
		xtlogit `v' couple_informed i.messenger ivr2 sms2 if female==0,  fe
		eststo
		restore
		preserve
		sample 2034 if messenger==2, c
		
		xtlogit `v'  i.recipient ivr2 sms2 woman_messeng if female==0,   fe
		eststo
		restore
		preserve
		sample 2006 if messenger==2, c
		xtlogit `v' couple_messeng i.recipient ivr2 sms2 if female==0,   fe
		eststo		
			restore
		
}		
	esttab using pref_publicgood_male,  replace  tex fragment  obslast nonotes order(woman_informed couple_informed woman_messeng couple_messeng) nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)
	eststo clear			
		
			foreach v of varlist   pref_total{

		xtreg `v' i.woman_informed i.recipient ivr2 sms2 , vce(cluster vilid_final)  fe
		eststo
		xtreg `v' i.couple_informed i.recipient ivr2 sms2 , vce(cluster vilid_final)  fe
		eststo
		xtreg `v' i.woman_messeng i.recipient ivr2 sms2 , vce(cluster vilid_final)  fe
		eststo
		xtreg `v' i.couple_messeng i.recipient ivr2 sms2 , vce(cluster vilid_final)  fe
		eststo
}
esttab using pref_agreeind,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)

	
	
	
	
	
	exit
		/*
	HOUSEHOLD
	
	FIRST SAMPLE--> ALL: WHETHER OR NOT THEY SOLD ANYTHING
	If nothing sold then 0 
	
	
	
	
	GENERATE ALL VARS AS EQUAL TO ZERO THEN REPLACE WITH AMOUNTS
	
	*/

	
	
	*Load Data
		use "$dir\Dropbox\Uganda-ICT\maizeUG-master\endline\analysis\endline_hh_destring.dta", clear
		
		
		*Only keep obs with both couples for household analysis
drop if both_avail!="yes"
	

	
	
		*Rename variables -- these are NUMBER of bags sold
		rename q70 seed_sved
		rename q72 amt_sold_solo
		rename q81 amt_sold_else
		rename spouse2r72 spouse_amt_sold_solo
		rename spouse2r81 spouse_amt_sold_else	
		rename q75 amt_sold_spouse
		rename spouse2r75 spouse_amt_sold_spouse
		rename q78 amt_sold_joint
		rename spouse2r78 spouse_amt_sold_joint
		
		
*Change 999 to missing for I dont know responses
		foreach v of varlist q76 q73 spouse2r73 q76 spouse2r76 q79 spouse2r79 amt_sold_solo amt_sold_else amt_sold_spouse amt_sold_joint spouse_amt_sold_solo spouse_amt_sold_else spouse_amt_sold_spouse spouse_amt_sold_joint{
replace `v'=. if `v'==999

}

		**Change missing amounts to 0 for analysis, includes those from households who disagree on maize selling-- HH who both said No to selling maize are not included
		foreach v of varlist amt_sold_solo amt_sold_else amt_sold_spouse amt_sold_joint spouse_amt_sold_solo spouse_amt_sold_else spouse_amt_sold_spouse spouse_amt_sold_joint{
		replace `v'=0 if `v'==. 
		*& q71=="Yes" | `v'==. & maize_disagree==1
		}
		
				*Need to calc KGs per bag
		foreach v of varlist grp1a17 grp2b17 grp3c17 grp4d17 grp5e17 		spouse2grp_sp1f17 spouse2grp_sp2g17 spouse2grp_sp3h17 spouse2group_sp4j17 spouse2grp5_sp5k17{
		replace `v'=0 if `v'==.
		}
		
		replace plot_no="." if plot_no=="NA"
		destring plot_no, replace
		sort hhid
		replace plot_no=plot_no[_n+1] if hhid==hhid[_n+1] & plot_no==.
		
		gen bag_avg=.
replace bag_avg= (grp1a17+grp2b17+grp3c17+grp4d17+grp5e17)/plot_no 
gen bag_avg_spouse=.
replace bag_avg_spouse= (	spouse2grp_sp1f17 +spouse2grp_sp2g17+ spouse2grp_sp3h17 +spouse2group_sp4j17+ spouse2grp5_sp5k17)/plot_no 


			**generate amount sold in KG
			gen amt_sold_solo_kg= amt_sold_solo*bag_avg
			gen amt_sold_else_kg= amt_sold_else*bag_avg
			gen amt_sold_spouse_kg=amt_sold_spouse*bag_avg
			gen amt_sold_joint_kg= amt_sold_joint*bag_avg

			gen spouse_amt_sold_solo_kg= spouse_amt_sold_solo*bag_avg_spouse
			gen spouse_amt_sold_else_kg= spouse_amt_sold_else*bag_avg_spouse
			gen spouse_amt_sold_spouse_kg=spouse_amt_sold_spouse*bag_avg_spouse
			gen spouse_amt_sold_joint_kg= spouse_amt_sold_joint*bag_avg_spouse
		
		
		
**generate indicator for when 1 spouse reports selling maize and the other doesnt
gen soldyes_maize=.
replace soldyes_maize=1 if q71=="Yes"
replace  soldyes_maize=0 if q71=="No"

gen soldyes_maize_spouse=.
replace soldyes_maize_spouse=1 if spouse2r71=="Yes"
replace soldyes_maize_spouse=0 if spouse2r71=="No"


sort hhid gender1
gen maize_disagree=.
replace maize_disagree=0 if soldyes_maize==soldyes_maize_spouse

replace maize_disagree=1 if soldyes_maize!=soldyes_maize_spouse

tab maize_disagree



		
	*create female var
gen female=.
replace female=1 if gender1==2
replace female=0 if gender1==1


		generate totalmaizesold=.
		replace totalmaizesold= amt_sold_solo+ amt_sold_spouse+amt_sold_joint+amt_sold_else
		
							generate totalmaizesoldkg=totalmaizesold*bag_avg

		
		
		generate totalmaizesold_spouse=.
		replace totalmaizesold_spouse= spouse_amt_sold_spouse+ spouse_amt_sold_solo+ spouse_amt_sold_joint +spouse_amt_sold_else

							generate totalmaizesoldkg_spouse=totalmaizesold_spouse*bag_avg_spouse

		
		**Prices
			gen price_sold_solo=.
		replace price_sold_solo=q73/amt_sold_solo_kg
		tab price_sold_solo
		
		gen price_sold_joint=.
		replace price_sold_joint=q79/amt_sold_joint_kg
		tab price_sold_joint
		
		gen price_sold_spouse=.
		replace price_sold_spouse=q76/amt_sold_spouse_kg
		tab price_sold_spouse


				gen price_sold_solo_spouse=.
		replace price_sold_solo_spouse=spouse2r73/(spouse_amt_sold_solo*1000)
		tab price_sold_solo_spouse
		
		gen price_sold_joint_spouse=.
		replace price_sold_joint_spouse=q79/(spouse_amt_sold_joint*1000)
		tab price_sold_joint_spouse
		
		gen price_sold_spouse_spouse=.
		replace price_sold_spouse_spouse=spouse2r76/(spouse_amt_sold_spouse*1000)
		tab price_sold_spouse_spouse	
		

	/* Outcomes to analyze

	*Generate all gaps between spouses 
	*/
		*Change al missing to zero in step 1
	foreach v of varlist q73 q76 q79 spouse2r73 spouse2r76 spouse2r79 q82 spouse2r82{
	gen `v'_nomis=`v'
	replace `v'_nomis=0 if `v'==. 
	*& q71=="Yes" |`v'==. & maize_disagree==1
	}
	
	*Amtounts Difference
					
					*difference in reported what is sold jointly (AMOUNTS)

			sort hhid female 
		gen sold_joint_gap=.
		replace sold_joint_gap= abs(amt_sold_joint_kg-spouse_amt_sold_joint_kg)
	label variable sold_joint_gap "Diff in Amt Sold Jointly" 	   
	
	**Price Differences
			gen price_diff_joint=.
		replace price_diff_joint=abs(price_sold_joint-price_sold_spouse_spouse)
		label variable price_diff_joint "Price Diff Selling Joint"
			
			gen price_diff_solo=.
		replace price_diff_solo=abs(price_sold_solo-price_sold_solo_spouse)
		label variable price_diff_solo "Price Diff Selling Alone"
			

			
	
	
	**Total Maize Inc Ab diff
	gen maizeinc_total=.
	replace maizeinc_total= q73_nomis+ q76_nomis +q79_nomis+q82_nomis
		gen maizeinc_total_spouse=.
	replace maizeinc_total_spouse= spouse2r76_nomis+ spouse2r79_nomis +spouse2r73_nomis +spouse2r82_nomis 
	*if q71=="Yes" | spouse2r71=="Yes" 
	
	gen maizeinc_diff=.
	replace maizeinc_diff=abs(maizeinc_total-maizeinc_total_spouse)/100000

	
	**DIfference in income reported from joint selling
	
	gen maizeinc_joint_diff=.
	replace maizeinc_joint_diff= abs(q79_nomis-spouse2r79_nomis) 
	*if amt_sold_joint!=0 | spouse_amt_sold_joint!=0
	
	
	*Difference in reporting on what spouses sold individually (INC)
	gen maize_husband_diff=.
	replace maize_husband_diff= abs(q73- spouse2r76) if gender1==1 
	replace maize_husband_diff= abs(q76- spouse2r73) if gender1==2
	
	gen maize_wife_diff=.
	replace maize_wife_diff= abs(q73- spouse2r76) if gender1==2 
	replace maize_wife_diff= abs(q76- spouse2r73) if gender1==1
	
		*Difference in reporting on what spouses sold individually (amts)
	gen maize_husband_diffamt=.
	replace maize_husband_diffamt= abs(amt_sold_solo- spouse_amt_sold_spouse) if gender1==1 
	replace maize_husband_diffamt= abs(amt_sold_spouse- spouse_amt_sold_solo) if gender1==2
	
	gen maize_wife_diffamt=.
	replace maize_wife_diffamt= abs(amt_sold_solo- spouse_amt_sold_spouse) if gender1==2 
	replace maize_wife_diffamt= abs(amt_sold_spouse- spouse_amt_sold_solo) if gender1==1

	
	
	
					   
				*Generate transparency gap variable
			*What i reported alone minus what my spouse reported i sold alone
			sort hhid female
			gen trans_gap_joint=.
			replace trans_gap_joint= abs(q79_nomis-spouse2r79_nomis)/100000
			sum trans_gap_joint 
			   
			label variable trans_gap_joint "Total Inc Diff from Selling Jointly" 	
		
		
		
			**Agreement on Maize sold jointly 
			
				*Agree that they sold SOME amount tonight (don't have to agree on the amount)
				gen sold_joint_agree=0
				replace sold_joint_agree=1 if amt_sold_joint_kg>0 & spouse_amt_sold_joint_kg>0
		
		
		
		
		***Spend preferences
		
		label variable spend_list1 "school fees"

. label variable spend_list2 "medical"

. label variable spend_list3 "ag input"

. label variable spend_list4 "hiring ag lab"

. label variable spend_list5 "hh inves"

. label variable spend_list6 "sm busi"

. label variable spend_list7 "consump"

. label variable spend_list8 "savings"

. label variable spend_list9 "personal"

. label variable spend_list10 "other"

	foreach v of varlist spend_list1 spend_list2 spend_list3 spend_list4 spend_list5 spend_list6 spend_list7 spend_list8 spend_list9 spend_list10 {
	replace `v'="." if `v'=="NA"
	destring `v', replace
	}
		
		*Create smaller spending categories
		
		gen pref_aginput=0
		replace pref_aginput=1 if spend_list4==1|spend_list3==1
		
	gen pref_publicgood=0
	replace pref_publicgood=1 if spend_list1==1|spend_list2==1
	
	gen pref_invest=0
	replace pref_invest=1 if spend_list5==1|spend_list6==1|spend_list8==1
		
		gen spouse_pref_aginput=0
		replace spouse_pref_aginput=1 if spouse2spend_list_sp4==1|spouse2spend_list_sp3==1
		
	gen spouse_pref_publicgood=0
	replace spouse_pref_publicgood=1 if spouse2spend_list_sp1==1|spouse2spend_list_sp2==1
	
	gen spouse_pref_invest=0
	replace spouse_pref_invest=1 if spouse2spend_list_sp5==1|spouse2spend_list_sp6==1|spouse2spend_list_sp8==1
	
	
	*AGREEMENT
		gen pref_aginput_agree=0
		replace pref_aginput_agree=1 if pref_aginput==1&spouse_pref_aginput==1
		
	gen pref_publicgood_agree=0
	replace pref_publicgood_agree=1 if pref_publicgood==1&spouse_pref_publicgood==1
	
	gen pref_invest_agree=0
	replace pref_invest_agree=1 if pref_invest==1&spouse_pref_invest==1
	
	gen pref_agree= pref_aginput_agree + pref_publicgood_agree + pref_invest_agree

	gen pref_agreeone=0
	replace pref_agreeone= 1 if pref_agree>=1
					
					*transparency gap on what husband sold (AMOUNTS) 
			/*
			sort hhid female
			gen trans_gap_spouse=.
			replace trans_gap_spouse= abs(amt_sold_solo-spouse_amt_sold_spouse)
*/

	
		***********Regressions
		
		
		
					*fix  ids and create real village id
foreach v of varlist vilid subid distid{
replace `v'="." if `v'=="NA"
destring `v', replace
replace `v'=`v'[_n-1] if hhid==hhid[_n-1]
}

gen subid_add=subid*1000
gen distid_add=distid*100
gen vilid_final= subid_add+distid_add+vilid


order hhid gender1

*Make HHID non string
	gen hhid_byte=substr(hhid,3, 5)
	order hhid_byte, a(hhid)
	destring hhid_byte, replace
 
*drop control group
drop if messenger==0

*destring other factors 
gen ivr2=.
replace ivr2=0 if ivr=="no"
replace ivr2=1 if ivr=="yes"


gen sms2=.
replace sms2=0 if sms=="no"
replace sms2=1 if sms=="yes"



**reverse maize disagree variable
gen maize_agree=0  if maize_disagree==1
replace maize_agree=1 if maize_disagree==0

	
***label variables
label variable female "Female"

label variable messenger " Messenger" 
label variable ivr2 "IVR"
label variable sms2 "SMS"
			
			sum maizeinc_diff trans_gap_joint sold_joint_gap  maize_husband_diffamt maize_wife_diffamt

		*Q: . Does being informed as a woman (either by seeing the video alone or as a couple – 
		*as opposed to seeing the video as a man alone) improve outcomes?
		
		gen woman_informed=.
		replace woman_informed=0 if recipient ==1
		replace woman_informed=1 if recipient==2|recipient==3
		label variable woman_informed "Woman Reci"

	**do we need village fixed effects?
	
	xtset vilid_final
	eststo clear

	preserve
	sample 672 if recipient==3, c
	

		foreach v of varlist maizeinc_diff trans_gap_joint sold_joint_gap  maize_husband_diffamt maize_wife_diffamt {
		xtreg `v' i.woman_informed i.messenger ivr2 sms2 , vce(cluster vilid_final)  fe
		eststo
}

		foreach v of varlist  maize_agree sold_joint_agree {
		xtlogit `v' i.woman_informed i.messenger ivr2 sms2 ,  fe
		eststo
}
esttab using woman_informedhh,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)

restore
/*
	eststo clear

	xi: craggit soldyes_maize i.woman_informed i.messenger ivr2 sms2, sec(maize_wife_diffamt i.woman_informed i.messenger ivr2 sms2) vce(cluster vilid_final)
	
		foreach v of varlist  maizeinc_diff  sold_joint_gap  maize_husband_diffamt maize_wife_diffamt  {

		 xi: craggit soldyes_maize  i.woman_informed i.messenger ivr2 sms2 , sec(`v' i.woman_informed i.messenger ivr2 sms2 )  vce(cluster vilid_final)  
	}
	

	eststo clear
		foreach v of varlist  maizeinc_diff  sold_joint_gap  maize_husband_diffamt maize_wife_diffamt  {
	
	heckman `v' i.woman_informed i.messenger ivr2 sms2, select(soldyes_maize  i.woman_informed i.messenger ivr2 sms2) vce(cluster vilid_final)
eststo
}


*/

		*Q: .Does portraying a couple being involved in maize farming/intensification improve outcomes?


		gen couple_informed=.
		replace couple_informed=0 if recipient ==1|recipient==2
		replace couple_informed=1 if recipient==3
		label variable couple_informed "Couple Reci"	

	eststo clear
	preserve
	sample 672 if recipient==1, c
	

		foreach v of varlist maizeinc_diff trans_gap_joint sold_joint_gap  maize_husband_diffamt maize_wife_diffamt {
		xtreg `v' i.couple_informed i.messenger ivr2 sms2 , vce(cluster vilid_final)  fe
		eststo
}

		foreach v of varlist  maize_agree sold_joint_agree{
		xtlogit `v' i.couple_informed i.messenger ivr2 sms2 ,  fe
		eststo
}

esttab using couple_informedhh,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)

restore


		*Q: .Does portraying a woman being involved in maize farming/intensification (either a 
			*couple or woman as messenger – as opposed to man alone as messenger): improve outcomes?

		gen woman_messeng=.
		replace woman_messeng=0 if messenger==1
		replace woman_messeng=1 if messenger==2|messenger==3
		label variable woman_messeng "Woman Messeng"

	xtset vilid_final
	eststo clear
	
			foreach v of varlist maizeinc_diff trans_gap_joint  sold_joint_gap  maize_husband_diffamt maize_wife_diffamt {
		xtreg `v' i.woman_messeng i.recipient ivr2 sms2 , vce(cluster vilid_final)  fe
		eststo
}
			foreach v of varlist  maize_agree sold_joint_agree {
		xtlogit `v' i.woman_messeng i.recipient ivr2 sms2 ,   fe
		eststo
}
esttab using woman_messenghh,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)

	eststo clear
	
	
	

		*Q: Does portraying the image of a household cooperating in maize farming/intensification
			*(couple as the messenger – as opposed to man or woman alone as messenger) improve outcomes?

		gen couple_messeng=.
		replace couple_messeng=0 if messenger==2|messenger==1
		replace couple_messeng=1 if messenger==3
		label variable couple_messeng "Couple Messeng"




	xtset vilid_final
	eststo clear

	
	
	
	
	
			foreach v of varlist maizeinc_diff trans_gap_joint  sold_joint_gap  maize_husband_diffamt maize_wife_diffamt {
		xtreg `v' i.couple_messeng i.recipient ivr2 sms2 , vce(cluster vilid_final)  fe
		eststo
}
			foreach v of varlist  maize_agree sold_joint_agree{
		xtlogit `v' i.couple_messeng i.recipient ivr2 sms2 ,   fe
		eststo
}


esttab using couple_messenghh,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)

	eststo clear

	
	

	**Agrement on spending Pref
	eststo clear

	
			foreach v of varlist  pref_publicgood_agree pref_aginput_agree  {
			preserve
	sample 672 if recipient==3, c

		xtlogit `v' woman_informed i.messenger ivr2 sms2 ,  fe
		eststo
		restore
		preserve
	sample 672 if recipient==1, c
	
		xtlogit `v' couple_informed i.messenger ivr2 sms2 ,  fe
		eststo
		restore
		xtlogit `v' woman_messeng i.recipient ivr2 sms2 ,   fe
		eststo
		xtlogit `v' couple_messeng i.recipient ivr2 sms2 ,   fe
		eststo		
			
		
}		
	esttab using pref_cat_agree,  replace  tex fragment order(woman_informed couple_informed woman_messeng couple_messeng) obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)
	eststo clear
			
		/*
			foreach v of varlist   pref_agree {

		xtreg `v' woman_informed i.recipient ivr2 sms2 , vce(cluster vilid_final)  fe
		eststo
		xtreg `v' couple_informed i.recipient ivr2 sms2 , vce(cluster vilid_final)  fe
		eststo
		xtreg `v' woman_messeng i.recipient ivr2 sms2 , vce(cluster vilid_final)  fe
		eststo
		xtreg `v' couple_messeng i.recipient ivr2 sms2 , vce(cluster vilid_final)  fe
		eststo
}
*/
			foreach v of varlist   pref_agreeone  {
	preserve
	sample 672 if recipient==3, c

		xtlogit `v' woman_informed i.recipient ivr2 sms2 ,   fe
		eststo
		restore
			preserve
	sample 672 if recipient==1, c

		xtlogit `v' couple_informed i.recipient ivr2 sms2 ,   fe
		eststo
		restore
		xtlogit `v' woman_messeng i.recipient ivr2 sms2 ,   fe
		eststo
		xtlogit `v' couple_messeng i.recipient ivr2 sms2 ,  fe
		eststo
}

esttab using pref_agree,  replace  tex fragment  obslast nonotes nomtitles order(woman_informed couple_informed woman_messeng couple_messeng) compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)
