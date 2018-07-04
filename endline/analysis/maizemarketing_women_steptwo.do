**Uganda RCT-- STEP TWO SELECTION MODEL

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
	
	FIRST SAMPLE--> ADD MISSINGS: WHETHER OR NOT THEY SOLD ANYTHING
	If nothing sold then .
	
	
	
	
	
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




		
		*Need to calc KGs per bag
		*  Q16 : How many bags		Q17 Kgs in a bag
		
		*replace with 0 only to help with adding variables
		foreach v of varlist grp1a17 grp2b17 grp3c17 grp4d17 grp5e17 	grp1f17 grp2g17 grp3h17 group_sp4j17 grp5_sp5k17 grp1a16 grp2b16 grp3c16 grp4d16 grp5e16 grp1f16 grp2g16 grp3h16 group_sp4j16 grp5_sp5k16{
		replace `v'=0 if `v'==.
		}
		
		
		
		replace plot_no="." if plot_no=="NA"
		destring plot_no, replace
		sort hhid
		replace plot_no=plot_no[_n+1] if hhid==hhid[_n+1] & plot_no==.
	

	
	
		gen bag_avg=.
replace bag_avg= (grp1a17+grp2b17+grp3c17+grp4d17+grp5e17)/plot_no if v1!=. & plot_no>0
replace bag_avg= (grp1f17 +grp2g17+ grp3h17 +group_sp4j17+ grp5_sp5k17)/plot_no if v1==. & plot_no>0


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
		**Change missing amounts to 0 for HH in which one spouse reported selling maize (and the other did not)
		foreach v of varlist amt_sold_solo amt_sold_else amt_sold_spouse amt_sold_joint amt_sold_solo_kg amt_sold_else_kg amt_sold_spouse_kg amt_sold_joint_kg{
		replace `v'=0 if `v'==. & q71[_n-1]=="Yes" &hhid==hhid[_n-1]
		replace `v'=0 if `v'==. & q71[_n+1]=="Yes" &hhid==hhid[_n+1]
		}
*	
	
			generate totalmaizesold=.
		replace totalmaizesold= amt_sold_solo+ amt_sold_spouse+amt_sold_joint+amt_sold_else
		
					generate totalmaizesoldkg=totalmaizesold*bag_avg

			
			gen prop_maizesold=.
			replace prop_maizesold=totalmaizesoldkg/maize_total_harv_kg if maize_total_harv_kg!=0
			
		**Prices
			gen price_sold_solo=.
		replace price_sold_solo=q73/(amt_sold_solo_kg)
		tab price_sold_solo
		
		gen price_sold_joint=.
		replace price_sold_joint=q79/(amt_sold_joint_kg)
		tab price_sold_joint
		
		gen price_sold_spouse=.
		replace price_sold_spouse=q76/(amt_sold_spouse_kg)
		tab price_sold_spouse

	
	*a) increase women’s involvement in maize sales?
			/*	Indicators
				-Women’s involvement in receiving income from selling maize 
				(amount wife sells personally + amount jointly sold)/ total amount of maize sales 
				   (you can base this either on wife reported or husband reported data) */
				   
				   
				**amount sold jointly should be the PROPORTION sold jointly of HH AMT sold
				gen amt_sold_joint_kg_prop=.
				replace amt_sold_joint_kg_prop= amt_sold_joint_kg/totalmaizesoldkg
				
				label variable amt_sold_joint_kg_prop "Amt Sold Jointly Prop."
				tab amt_sold_joint_kg_prop
				   
				  *maize sales participation by women
				   
						*generate women level maize sales indicator (reported by either
				*/	
				gen maize_sold_woman=.
				replace maize_sold_woman= amt_sold_solo_kg if female==1
				replace maize_sold_woman= amt_sold_spouse_kg if female==0

				gen maizesale_partici_woman=.
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
	gen sold_traders=.
	replace sold_traders=0 if 	q801==0 & q802==0
	replace sold_traders=1 if q801==1|q802==1
	gen sold_middle=.
	replace sold_middle=0 if  q803==0
	replace sold_middle=1 if q803==1

	
	
	
	*spending preferences
	
			*Create smaller spending categories
		
		gen pref_aginput=.
		replace pref_aginput=0 if	spend_list4==0 & spend_list3==0
		replace pref_aginput=1 if spend_list4==1|spend_list3==1
		
	gen pref_publicgood=.
		replace pref_publicgood=0 if spend_list1==0 & spend_list2==0
	replace pref_publicgood=1 if spend_list1==1|spend_list2==1
	
	gen pref_invest=.
	replace pref_invest=0 if spend_list5==0 & spend_list6==0 & spend_list8==0
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
				label variable soldyes_maize "Sold Maize" 
	
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
	
set seed 6732


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

esttab using woman_informed_step2_female,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)



	eststo clear

*amt_sold_joint_kg_prop doesn't work (87% of sample sold 0 or 1 prop) and same for maizesale_partici_woman

		foreach v of varlist    totalmaizesoldkg prop_maizesold    price_sold_joint   {

		heckman `v' i.woman_informed i.messenger ivr2 sms2 if female==1, select(soldyes_maize  i.woman_informed i.messenger ivr2 sms2) vce(cluster vilid_final)

		eststo
}


		foreach v of varlist sold_traders  sold_middle   {
			heckprobit `v' i.woman_informed i.messenger ivr2 sms2 if female==1, select(soldyes_maize=  i.woman_informed i.messenger ivr2 sms2) vce(cluster vilid_final)

		eststo
}

*/
esttab using woman_informed_step2_female_heckm,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)




	eststo clear

		foreach v of varlist  maize_total_harv_kg totalmaizesoldkg prop_maizesold  amt_sold_joint_kg_prop maizesale_partici_woman   price*    {
		xtreg `v' i.woman_informed i.messenger ivr2 sms2 if female==0, vce(cluster vilid_final)  fe
		eststo
}


		foreach v of varlist   sold_*   {
		xtlogit `v' i.woman_informed i.messenger ivr2 sms2 if female==0,   fe
		eststo
}


esttab using woman_informed_step2_male,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)
	
	eststo clear
	
	

		foreach v of varlist   totalmaizesoldkg prop_maizesold   price_sold_joint       {
		heckman `v' i.woman_informed i.messenger ivr2 sms2 if female==0, select(soldyes_maize  i.woman_informed i.messenger ivr2 sms2) vce(cluster vilid_final)

		eststo
}

/*
		foreach v of varlist sold_traders  sold_middle   {
			heckprobit `v' i.woman_informed i.messenger ivr2 sms2 if female==0, select(soldyes_maize=  i.woman_informed i.messenger ivr2 sms2) vce(cluster vilid_final)

		eststo
}
*/

esttab using woman_informed_step2_maleheckm,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)
	
	
	
	
restore


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


esttab using couple_informed_step2_female,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)


	eststo clear

*price_sold_joint
		foreach v of varlist    totalmaizesoldkg prop_maizesold       {
				heckman `v' i.couple_informed i.messenger ivr2 sms2 if female==1, select(soldyes_maize  i.couple_informed i.messenger ivr2 sms2) vce(cluster vilid_final)

		eststo
}
*sold_middle
		foreach v of varlist sold_traders     {
			heckprobit `v' i.couple_informed i.messenger ivr2 sms2 if female==1, select(soldyes_maize=  i.couple_informed i.messenger ivr2 sms2) vce(cluster vilid_final)

		eststo
}



esttab using couple_informed_step2_female_heckm,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)



	eststo clear


		foreach v of varlist  maize_total_harv_kg  totalmaizesoldkg prop_maizesold amt_sold_joint_kg_prop maizesale_partici_woman   price*    {
		xtreg `v' i.couple_informed i.messenger ivr2 sms2 if female==0, vce(cluster vilid_final)  fe
		eststo
}


		foreach v of varlist   sold_*   {
		xtlogit `v' i.couple_informed i.messenger ivr2 sms2 if female==0,   fe
		eststo
}


esttab using couple_informed_step2_male,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)


	eststo clear


		foreach v of varlist    totalmaizesoldkg prop_maizesold     price_sold_joint  {
				heckman `v' i.couple_informed i.messenger ivr2 sms2 if female==0, select(soldyes_maize  i.couple_informed i.messenger ivr2 sms2) vce(cluster vilid_final)

		eststo
}

/*
*sold_middle

		foreach v of varlist sold_traders     {
			heckprobit `v' i.couple_informed i.messenger ivr2 sms2 if female==0, select(soldyes_maize=  i.couple_informed i.messenger ivr2 sms2) vce(cluster vilid_final)

		eststo
}

*/
esttab using couple_informed_step2_male_heckm,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)

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

esttab using woman_messeng_step2_female,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)


	eststo clear


		foreach v of varlist    totalmaizesoldkg prop_maizesold    price_sold_joint   {
		heckman `v' i.woman_messeng i.recipient ivr2 sms2 if female==1, select(soldyes_maize  i.woman_messeng i.recipient ivr2 sms2) vce(cluster vilid_final)
		
		eststo
}

		foreach v of varlist sold_traders  sold_middle   {
			heckprobit `v' i.woman_messeng i.recipient ivr2 sms2 if female==1, select(soldyes_maize=  i.woman_messeng i.recipient ivr2 sms2) vce(cluster vilid_final)

		eststo
}


esttab using woman_messeng_step2_female_heckm,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)


	eststo clear


		foreach v of varlist  maize_total_harv_kg  totalmaizesoldkg prop_maizesold amt_sold_joint_kg_prop maizesale_partici_woman   price*    {
		xtreg `v' i.woman_messeng i.recipient ivr2 sms2 if female==0, vce(cluster vilid_final)  fe
		eststo
}


		foreach v of varlist   sold_*   {
		xtlogit `v' i.woman_messeng i.recipient ivr2 sms2 if female==0,   fe
		eststo
}

esttab using woman_messeng_step2_male,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)


	eststo clear


		foreach v of varlist    totalmaizesoldkg prop_maizesold    price_sold_joint   {
		heckman `v' i.woman_messeng i.recipient ivr2 sms2 if female==0, select(soldyes_maize  i.woman_messeng i.recipient ivr2 sms2) vce(cluster vilid_final)
		
		eststo
}
*sold_traders
		foreach v of varlist   sold_middle   {
			heckprobit `v' i.woman_messeng i.recipient ivr2 sms2 if female==0, select(soldyes_maize=  i.woman_messeng i.recipient ivr2 sms2) vce(cluster vilid_final)

		eststo
}



esttab using woman_messeng_step2_male_heckm,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)


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
esttab using couple_messeng_step2_female,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)
	
	eststo clear

	
		foreach v of varlist   totalmaizesoldkg prop_maizesold  price_sold_joint   {
		heckman `v' i.couple_messeng i.recipient ivr2 sms2 if female==1, select(soldyes_maize  i.couple_messeng i.recipient ivr2 sms2) vce(cluster vilid_final)

		eststo
}

		foreach v of varlist sold_traders  sold_middle   {
			heckprobit `v' i.couple_messeng i.recipient ivr2 sms2 if female==1, select(soldyes_maize=  i.couple_messeng i.recipient ivr2 sms2) vce(cluster vilid_final)

		eststo
}


esttab using couple_messeng_step2_femaleheckm,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)
	
	eststo clear
	
	

		foreach v of varlist  maize_total_harv_kg totalmaizesoldkg prop_maizesold  amt_sold_joint_kg_prop maizesale_partici_woman   price*   {
		xtreg `v' i.couple_messeng i.recipient ivr2 sms2 if female==1, vce(cluster vilid_final)  fe
		eststo
}
		foreach v of varlist   sold_*   {

		xtlogit `v' i.couple_messeng i.recipient ivr2 sms2 if female==0,   fe
		eststo
}
esttab using couple_messeng_step2_male,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)
	
	eststo clear

	
		foreach v of varlist   totalmaizesoldkg prop_maizesold  price_sold_joint   {
		heckman `v' i.couple_messeng i.recipient ivr2 sms2 if female==0, select(soldyes_maize  i.couple_messeng i.recipient ivr2 sms2) vce(cluster vilid_final)

		eststo
		}
		
		/*
		*sold_middle
				foreach v of varlist sold_traders     {
			heckprobit `v' i.couple_messeng i.recipient ivr2 sms2 if female==0, select(soldyes_maize=  i.couple_messeng i.recipient ivr2 sms2) vce(cluster vilid_final)

		eststo
}

*/

esttab using couple_messeng_step2_maleheckm,  replace  tex fragment  obslast nonotes nomtitles compress nogaps  label star( * 0.10  ** 0.05 *** 0.01) se  r2 noomitted   nobaselevels eqlabels(none)  b(%6.3f)
	
	eststo clear
