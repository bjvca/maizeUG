***** UGANDA ICT RCT
**Liz Ignowski
** May 8, 2018
clear
cd "C:\Users\u0107600\Dropbox\Uganda-ICT\maizeUG-master\endline\analysis"

*Import Data
		*import delimited C:\Users\u0107600\Dropbox\Uganda\endline\data\endline.csv, clear
		*import delimited C:\Users\Liz\Dropbox\Uganda\endline\data\endline.csv, clear
		*3613 variables
		use endline, clear
		
				**Focus is on HH who have sold maize, drop HH who haven't? And what is criteria? 
		*(both spouses agree or one of them says they have sold it)
		*drop HH who don't know if they have sold any maize (both spouses don't know)
		drop if q71=="NA" & spouse2r71=="NA"
		drop if q71=="98" & spouse2r71=="NA"		
		drop if q71=="98" & spouse2r71=="98"		

		
		keep if both_avail=="yes"
		
	keep hhid  gender1 spouse* 
	
		rename spouse2* *
		drop spend_list1 spend_list2 spend_list3 spend_list4 spend_list5 spend_list6 spend_list7 spend_list8 spend_list9 spend_list10 spend_list98
		rename r* q*
		rename grp_sp* grp*
		rename spend_list_sp* spend_list*
		drop amountl1 amountl2 amountl7 amountl8 amountl3 amountl5 amountl4 amountl6
		foreach num of numlist 1 2 3 4 5 6 7 8 {
		rename amountl`num'_sp amountl`num'
		}
		
	save endline_secondspouse, replace
	
			*import delimited C:\Users\Liz\Dropbox\Uganda\endline\data\endline.csv, clear
			use endline, clear
					keep if both_avail=="yes"
		**Focus is on HH who have sold maize, drop HH who haven't? And what is criteria? 
		*(both spouses agree or one of them says they have sold it)
		*drop HH who don't know if they have sold any maize (both spouses don't know)
		drop if q71=="." & spouse2r71=="."
		drop if q71=="98" & spouse2r71=="."		

	drop spouse*

	save endline_firstspouse, replace
	
	use endline_firstspouse, clear
	
	*append using "C:\Users\Liz\Dropbox\Uganda-ICT\maizeUG-master\endline\analysis\endline_secondspouse.dta"
	append using "C:\Users\u0107600\Dropbox\Uganda-ICT\maizeUG-master\endline\analysis\endline_secondspouse.dta"

	save endline_indiv, replace

	*correct gender for individual 
	


	gen gender12=gender1
	order gender12, a(gender1)
	replace gender12="." if v1==.
		sort hhid gender12
	replace gender12="woman" if gender1[_n+1]=="man" & gender12=="."
	replace gender12="man" if gender1[_n+1]=="woman" & gender12=="."	
	
	drop gender1
	rename gender12 gender1

	*do same for recipeitn and messenger
	
	
	
	foreach v of varlist messenger recipient{
		gen `v'1=`v'
	order `v'1, a(`v')
	replace `v'1="." if v1==.
		sort hhid `v'1
		replace `v'1=`v'[_n+1] if `v'1=="."
		

	drop `v'
	rename `v'1 `v'
	}
	save endline_indiv, replace
