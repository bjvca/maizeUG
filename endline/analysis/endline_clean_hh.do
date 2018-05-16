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

		*Destring variables
		
		foreach v of varlist grp*{
		replace `v'="." if `v'=="NA"
		}
			destring grp*, replace
			
				foreach v of varlist q*{
		replace `v'="." if `v'=="NA"
		}
			destring q*, replace
			
				foreach v of varlist spouse*{
		replace `v'="." if `v'=="NA"
		}
			destring spouse*, replace
			
			
		foreach v of varlist gender1 recipient messenger {
		replace `v'="." if `v'=="NA"
		replace `v'="1" if `v'=="man"
		replace `v'="1" if `v'=="male"
		replace `v'="2" if `v'=="woman"
		replace `v'="2" if `v'=="female"	
		replace `v'="3" if `v'=="couple"
		replace `v'="4" if `v'=="ctrl"
		destring `v', replace
		}
		
			save endline_hh_destring, replace
		
