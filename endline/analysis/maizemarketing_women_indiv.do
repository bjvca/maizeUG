***** UGANDA ICT RCT
**Liz Ignowski
** May 8, 2018
clear
cd "C:\Users\u0107600\Dropbox\Uganda-ICT\maizeUG-master\endline\analysis"
*cd "C:\Users\Liz\Dropbox\Uganda-ICT\maizeUG-master\endline\analysis"

/*
*Import Data
		import delimited C:\Users\u0107600\Dropbox\Uganda\endline\data\endline.csv, clear
		*import delimited C:\Users\Liz\Dropbox\Uganda\endline\data\endline.csv, clear

		*save "C:\Users\Liz\Dropbox\Uganda-ICT\maizeUG-master\endline\data\endline_liz.dta", replace
		save "C:\Users\u0107600\Dropbox\Uganda-ICT\maizeUG-master\endline\data\endline_liz.dta", replace

*/

*Load Data
		use "C:\Users\u0107600\Dropbox\Uganda-ICT\maizeUG-master\endline\analysis\endline_indiv.dta", clear
		*use "C:\Users\Liz\Dropbox\Uganda-ICT\maizeUG-master\endline\analysis\endline_indiv.dta", clear
/*
		*Destring variables
		
		foreach v of varlist grp*{
		replace `v'="." if `v'=="NA"
		}
			destring grp*, replace
			
				foreach v of varlist q*{
		replace `v'="." if `v'=="NA"
		}
			destring q*, replace
			
			*	foreach v of varlist spouse*{
		*replace `v'="." if `v'=="NA"
		*}
			*destring spouse*, replace
			
			
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
		
			save endline_indiv_destring, replace
*/
		use endline_indiv_destring, clear

		*Only keep obs with both couples
		egen dup_find= group(hhid )
		duplicates tag dup_find, gen(dup)
		tab dup
		drop if dup==0

		
		
		
		*gen spouse_gender=.
		*replace spouse_gender= 1 if gender1==2
		*replace spouse_gender=2 if gender1==1
		*order spouse_gender, a(spouse2consent)
		

		
		
		
		
			*Rename variables
		rename q70 seed_sved
		rename q72 amt_sold_solo
		rename q81 amt_sold_else
		*rename spouse2r72 spouse_amt_sold_solo
		*rename spouse2r81 spouse_amt_sold_else	
		rename q75 amt_sold_spouse
		*rename spouse2r75 spouse_amt_sold_spouse
		rename q78 amt_sold_joint
		*rename spouse2r78 spouse_amt_sold_joint
		

		**Generate total maize sold and price variables
		
		generate totalmaizesold=.
		replace totalmaizesold= amt_sold_solo+ amt_sold_spouse+amt_sold_joint+amt_sold_else
		
		gen price_sold_solo=.
		replace price_sold_solo=q73/amt_sold_solo
		tab price_sold_solo
		
		gen price_sold_joint=.
		replace price_sold_joint=q79/amt_sold_joint
		tab price_sold_joint
		
		gen price_sold_spouse=.
		replace price_sold_spouse=q76/amt_sold_spouse
		tab price_sold_spouse
		

		
			save endline_indiv_clean, replace
			
*Identify Necessary variables
use endline_indiv_clean, clear

	/* Outcomes to analyze
	a) increase women’s involvement in maize sales? 
	b) increase the price fetched for selling maize? 
	c) likelihood to sell through higher price channels? 
	d) increase transparency over income from selling maize?  
	e) reduce hiding of income (by wife or husband) ? 
	f) make spending preferences by wife and husband more similar (~increased control of woman over spending income)? 
	e) higher preference for farm or household investments? */
	
	
	
	*a) increase women’s involvement in maize sales?
			/*	Indicators
				-Women’s involvement in receiving income from selling maize 
				(amount wife sells personally + amount jointly sold)/ total amount of maize sales 
				   (you can base this either on wife reported or husband reported data) */

	*Variables of Interest
	
		*amount wife sells personally
		
				*reported by her
				sum amt_sold_solo if gender1==2 
				tab amt_sold_solo if gender1==2 
				sum amt_sold_solo if gender1==2 & amt_sold_solo!=0
				*sum spouse_amt_sold_solo if gender1==1 & spouse_amt_sold_solo<100
			*	tab spouse_amt_sold_solo if gender1==1 
			*	sum spouse_amt_sold_solo if gender1==1 & spouse_amt_sold_solo<100 & spouse_amt_sold_solo!=0
	
				*reported by her spouse
				sum amt_sold_spouse if gender1==1 
				tab amt_sold_spouse if gender1==1 
				sum amt_sold_spouse if gender1==1 & amt_sold_spouse!=0
			*	sum spouse_amt_sold_spouse if gender1==2 
			*	tab spouse_amt_sold_spouse if gender1==2 
			*	sum spouse_amt_sold_spouse if gender1==2 & amt_sold_spouse!=0	
				
		*amount wife sells jointly
		
				*reported by her
				sum amt_sold_joint if gender1==2 
				tab amt_sold_joint if gender1==2 
				sum amt_sold_joint if gender1==2 & amt_sold_joint!=0
			*	sum spouse_amt_sold_joint if gender1==1 &
			*	tab spouse_amt_sold_joint if gender1==1 
			*	sum spouse_amt_sold_joint if gender1==1  & spouse_amt_sold_joint!=0
	
				*reported by her spouse
				sum amt_sold_joint if gender1==1 
				tab amt_sold_joint if gender1==1 
				sum amt_sold_joint if gender1==1 & amt_sold_joint!=0
			*	sum spouse_amt_sold_joint if gender1==2
			*	tab spouse_amt_sold_joint if gender1==2
			*	sum spouse_amt_sold_joint if gender1==2 & spouse_amt_sold_joint!=0
				
				
				
		*generate indicator
		*(amount wife sells personally + amount jointly sold)/ total amount of maize sales
		
				*reported by her
				gen women_maizesales_solo=.
				replace women_maizesales_solo= amt_sold_solo if gender1==2 
				gen women_maizesales_joint=.
				replace women_maizesales_joint= amt_sold_joint if gender1==2 	
				
				gen women_maizesale_parti=(women_maizesales_solo+women_maizesales_joint)/totalmaizesold
	
				*reported by husband
				gen women_maizesales_solo2=.
				replace women_maizesales_solo2= amt_sold_spouse if gender1==1 
				gen women_maizesales_joint2=.
				replace women_maizesales_joint2= amt_sold_joint if gender1==1 	
				
				gen women_maizesale_parti2=(women_maizesales_solo2+women_maizesales_joint2)/totalmaizesold
				tab women_maizesale_parti2
	
	
	
	*b) increase the price fetched for selling maize? 
			/*	Indicators
				- price fetched for maize sold 
				- price differences: compare price per bag by wife selling alone with sales in which the husband is involved
				   (husband alone, husband jointly)*/
				   
				 *price fetched by woman  reported by her
				 sum price_sold_solo if gender1==2
				 sum price_sold_joint if gender1==2
				 sum price_sold_spouse if gender1==2
				   
				 *price fetched by man  reported by husband
				 sum price_sold_solo if gender1==1
				 sum price_sold_joint if gender1==1
				 sum price_sold_spouse if gender1==1
				   
				   
				   ttest price_sold_solo, by(gender1)
				   ttest price_sold_joint, by(gender1)
				   ttest price_sold_solo==price_sold_spouse
				   ttest price_sold_solo==price_sold_spouse if gender1==1
				   ttest price_sold_solo==price_sold_spouse if gender1==2


				   
				   
	*Variables of Interest		   
				   
	*c) likelihood to sell through higher price channels? 
			/*	Indicators
			- to whom one is selling to (in principle: women may be less connected to higher bidders, or choose marketing 
			  channels that typically offer lower prices like small itinerant trader)
		*/
		
		*SELLING INDIV
		sum q74*
		
		ttest q741, by(gender1)
		
		ttest q742, by(gender1)
		ttest q743, by(gender1)
		
		ttest q744, by(gender1)
		
		ttest q745, by(gender1)
		
		ttest q746, by(gender1)
		
		ttest q747, by(gender1)
			
			*No sig results between gender and who they sold to individually
				*Men sold slighty more (sig 9%) to processors than women
				
		*SELLING BY SPOUSE ALONE
		sum q77*
		
		ttest q771, by(gender1)
		
		ttest q772, by(gender1)
		ttest q773, by(gender1)
		
		ttest q774, by(gender1)
		
		ttest q775, by(gender1)
		
		ttest q776, by(gender1)
		
		ttest q777, by(gender1)

			*No sig results between gender and who their spouse sold to individually

			
		*SELLING JOINTLY
		sum q80*
		
		ttest q801, by(gender1)
		
		ttest q802, by(gender1)
		ttest q803, by(gender1)
		
		ttest q804, by(gender1)
		
		ttest q805, by(gender1)
		
		ttest q806, by(gender1)
		
		ttest q807, by(gender1)

			*No sig results between gender and who their spouse sold to jointly
					*Women report selling to neighbors slighlty less (5%) but the difference is 2% vs 1%
					
			
		
			
	
	*d) increase transparency over income from selling maize?  
			/*	Indicators
			- Transparency gap= (just the difference regardless of whether husband or wife reports higher income)
			*/
			foreach v of varlist q73 q76 q79{
			replace `v'=. if `v'==999
			}
			
			*SELLING ALONE 
			
			*q73: what I reported selling alone
			*q73_spouse: what my spouse reported selling alone
			*q76: What I reported my spouse sold alone
			*q76_spouse: What my spouse reported I sold alone
				
				
				sort hhid q76
				gen q76_spouse=q76[_n-1] if hhid==hhid[_n-1]
				replace q76_spouse=q76[_n+1] if hhid==hhid[_n+1]
				
				sort hhid q73
				gen q73_spouse=q73[_n-1] if hhid==hhid[_n-1]
				replace q73_spouse=q73[_n+1] if hhid==hhid[_n+1]
	
	
				gen q73_nomissing=q73
				replace q73_nomissing=0 if q73==.
				gen q73_spousenomissing=q73_spouse
				replace q73_spousenomissing=0 if q73_spouse==.
				gen q76_spousenomissing=q76_spouse
				replace q76_spousenomissing=0 if q76_spouse==.
				gen q76_nomissing=q76
				replace q76_nomissing=0 if q76==.
			
			
			
			*Generate transparency gap variable
			*What i reported alone minus what my spouse reported i sold alone
			gen trans_gap=.
			replace trans_gap= q73_nomissing-q76_spousenomissing
			replace trans_gap=. if q73_nomissing==0 & q76_spousenomissing==0
			
			*what i reported my spouse selling alone minus what my spouse reported selling alone
			gen trans_gap2=.
			replace trans_gap2= q76_nomissing-q73_spousenomissing
			replace trans_gap2=. if q76_nomissing==0 & q73_spousenomissing==0
			
			sum trans_gap trans_gap2

			*SELLING JOINTLY 
			
			*q79: what I reported selling jointly
			*q79_spouse: what my spouse reported selling jointly
				
				
				sort hhid q79
				gen q79_spouse=q79[_n-1] if hhid==hhid[_n-1]
				replace q79_spouse=q79[_n+1] if hhid==hhid[_n+1]
	
	
				gen q79_nomissing=q79
				replace q79_nomissing=0 if q79==.
				gen q79_spousenomissing=q79_spouse
				replace q79_spousenomissing=0 if q79_spouse==.
		
			
			
			*Generate transparency gap variable
			*What i reported alone minus what my spouse reported i sold alone
			gen trans_gap_joint=.
			replace trans_gap_joint= q79_nomissing-q79_spousenomissing
			replace trans_gap_joint=. if q79_nomissing==0 & q79_spousenomissing==0

			sum trans_gap_joint 
		


			
	
	*e) reduce hiding of income (by wife or husband) ? 
			/*	Indicators
			- hiding income = (here it matters whether husband or wife reports more than the other spouse)
				|Income from selling maize reported by husband - Income from selling maize reported by wife|/ Income from selling maize reported by husband  
	
	*f) make spending preferences by wife and husband more similar (~increased control of woman over spending income)? 
			/*	Indicators
			- compare top 3 of wife’s and husband’s wish list of spending money 
			
	*e) higher preference for farm or household investments? 
			/*	Indicators
			- likelihood of household or farm investments in the top 3 list of spending preferences
	
