clear
clear matrix
clear mata
capture log close
set maxvar 15000
set more off
numlabel, add

*******************************************************************************
*
*  FILENAME:	NGR5-FQ-AbortionCleaning-v#-$date.do
*  PURPOSE:		Clean female abortion data
*  CREATED:		2018-04-10 by Suzanne Bell
*  DATA IN:		NGR5_WealthWeightAll_$date.csv
*  DATA OUT:	NGR5-FQ-AbortionData-$todaysdate.dta
*  UPDATES:		26 Aug 2018 by Mridula Shankar (retained v#)
*					Added code for abortion safety categorization that does not include
*				the issues variable. 
*					Recoded -88 to 0 for self_abt_issues, self_reg_issues
*				20 Aug 2018 by Suzanne (retained v#)
*					Recoded person-time calculation so self_abt_yn!=. instead of !=1
*				06 Sep 2018 by Suzanne (v2)
*					Recoded safety3 variables to code other pills/pill type
*						unknown from pharmacy/chemists as least safe, not less safe
*					Recoded several mifeo/miso pharmacy/chemist combinations with 
*						other safe methods as less safe, and included other
*						non-facilities along with pharmacy/chemist
* 				26 Oct 2018 by Suzanne (v3)
*					Generated respondent abortion/sharing variable for friend 2
*					and respondent incident abortion/sharing variables for both friends
*				05 Nov 2018 by Mridula (v4)
*					Created separate incidence estimates for sensitivity analyses 
*				09 Nov 2018 by Suzanne (v5)
*					Create variable for sensitivity analysis excluding EC use for 
*						period regulation only
* 				10 Jan 2019 by Mridula (v7)
*					Create variables for determining percent of "don't know" for 
*						confidante abortion experience
*					Create confidante incidence estimates removing don't know responses 	
*				25 Jan 2019 by Suzanne (v8)
*					Create variables for whether respondent shared pregnancy removal
*					from last year; created variables for safety methods paper
*				26 Jan 2019 by Mridula (v9)
*					Create incidence variables for confidantes excluding both uncertain
*					preg removals/likely abortions if method unknown AND EC use
*					with no reported complications 
*				27 Jan 2019 by Suzanne (v10)
*					Finished creating combined abortion source variables; made
*					first and last method facility-based surgery safe
*				31 Jan 2019 by Suzanne (v11)
*					Created safe variables using only first method; created dichotomous
*					variable related to whether provider likely trained
*				6 Feb 2019 by Suzanne (v12)
*					Created provider training variables using only first method, only last
*				14 Feb 2019 by Suzanne (v13)
*					Created dichotomous high risk variables using only first, only last,
*					and all information and re-coded dichomotous provider trainined variables
*					using information from existing/new method4 and source4 variables
*				21 Feb 2019 by Suzanne (v14)
*					Corrected _first_source4 code
*				25 Feb 2019 by Suzanne (v15)
*					Created 4-category ethnicity variable with Yoruba as category
*				14 Mar 2019 by Suzanne (v16)
*					Created risk/untrained variables for sensitivity analysis
*				14 Mar 2019 by Suzanne (v17)
*					Created friend 1/respondent combined age and school variables for
*					analyses involving "missing" confidantes
*				19 Mar 2019 by Suzanne (v18)
*					Updated source provider training variable to exclude fieldworkers; put
*					fieldworkers in "other"; created friend1 variables to generate friend1
*					risk/untrained variables
*					
*******************************************************************************

*******************************************************************************
* SET MACROS
*******************************************************************************

* Set country/round macros
local CCRX NGR5

* Set own directory 
global datadir "~/Dropbox (Gates Institute)/Abortion-Nigeria/Data/HHQFQ"
cd "$datadir"

* Set macro for dataset date
local datadate 18Sep2018

* Set macro for date
local today=c(current_date)
local c_today= "`today'"
global date=subinstr("`c_today'", " ", "",.)

di %td_CY-N-D  date("$S_DATE", "DMY")
local newdate: di %td_CY-N-D date("$S_DATE","DMY")
di "`newdate'"
di trim("`newdate'")
global todaysdate: di trim("`newdate'")

* Create log
*capture log using "`CCRX'-FQ-AbortionCleaning.log", replace

*******************************************************************************
* READ IN DATA
*******************************************************************************

* Read in female data
use "$datadir/`CCRX'_WealthWeightAll_`datadate'.dta", clear

*******************************************************************************
* GENERATE AND RENAME/RECODE VARIABLES
*******************************************************************************

* Rename wealth quintile
rename wealth_ wealth

* Recode -88 and -99 as .
foreach var in friend_count friend1_age friend1_school friend2_age friend2_school self_abt_yn  {
replace `var'=. if `var'==-88 | `var'==-99
}

* Recode -88 to 0 
foreach var in self_abt_issues self_reg_issues {
recode `var' (-88=0 "No/Don't know") (1=1 "Yes"), gen(`var'_v2)
}

* Encode state
lab def state_list 1 "anambra" 2 "kaduna" 3 "kano" 4 "lagos" 5 "nasarawa" 6 "rivers" 7 "taraba"
encode level1, gen(state) lab(state_list)

* Generate combined respondent and friend1 age variable for analyses with "missing" confidantes
gen f1r_age=friend1_age
replace f1r_age=FQ_age if f1r_age==.
lab var f1r_age "Combined friend 1 and respondent age variable for analyses with 'missing' confidantes"

* Generate 5-year age groups
lab def age5_list 0 "15-19" 1 "20-24" 2 "25-29" 3 "30-34" 4 "35-39" 5 "40-44" 6 "45-49"
foreach var in FQ_age friend1_age friend2_age f1r_age {
egen `var'5=cut(`var'), at(15(5)50) icodes
lab val `var'5 age5_list
}
rename FQ_age5 age5

* Generate 10-year age groups, include 15-19
foreach var in FQ_age friend1_age friend2_age f1r_age {
egen `var'10=cut(`var'), at(15,20,30,40,50) icodes
}
rename FQ_age10 age10
lab def age10_list 0 "15-19" 1 "20-29" 2 "30-39" 3 "40-49"
lab val age10 friend1_age10 friend2_age10 age10_list

* Generate combine marital status variable
recode FQmarital_status 1 2=1 3 4=2 5=3 -99 -88=., gen(FQmarital_statusv2)
lab def FQmarital_statusv2_list 1 "Currently married/cohabiting" 2 "Divorced/widowed" 3 "Never married"
lab val FQmarital_statusv2 FQmarital_statusv2_list

* Generate fewer category education variable
recode school (1=0 "Never") (2=1 "Primary") (3=2 "Secondary") (4=3 "Higher"), gen(edu4)
lab var edu4 "Highest level of school attended (4 category)"

* Generate combined respondent and friend1 school variable for analyses with "missing" confidantes
gen f1r_school=friend1_school
replace f1r_school=school if f1r_school==.
lab var f1r_school "Combined friend s1 and respondent school variable for analyses with 'missing' confidantes"
lab val f1r_school school_list

* Generate four category religion variable
recode religion (1=1 "Catholic") (2=2 "Other Christian") (3=3 "Islam") (-77 4 96=4 "Other"), gen(religion4)
lab var religion4 "Religion of household head (4 category)"

* Generate three category ethnicity variable
recode ethnicity (5=1 "Hausa") (6=2 "Igbo") (-99=.) (else=3 "Other"), gen(ethnicity3)
lab var ethnicity3 "Ethnicity of household head (3 category)"

* Generate four category ethnicity variable
recode ethnicity (5=1 "Hausa") (6=2 "Igbo") (15=3 "Yoruba") (-99=.) (else=4 "Other"), gen(ethnicity4)
lab var ethnicity4 "Ethnicity of household head (4 category)"

* Generate four category parity variable
replace birth_events_rw=0 if ever_birth==0
recode birth_events_rw -99=. 0=0 1/2=1 3/4=2 5/max=3, gen(parity)
lab def parity_list 0 "0" 1 "1-2" 2 "3-4" 3 "5+"
lab val parity parity_list
lab var parity "Number of live births"
	
* Generate dichotomous acquaintance variable
rename FQaquainted FQacquainted
recode FQacquainted (1/2=1 "Acquainted") (3/4=0 "Not acquainted"), gen(acquainted2)
lab var acquainted2 "Acquainted with respondent (dichotomous)"

* Generate 0/1 rural/urban variable
capture drop urban
gen urban=ur
lab val urban urbanrural
lab var urban "Residence"

* Combine versions of period regulation question
foreach var in self_reg_yn friend1_reg_yn friend2_reg_yn {
gen `var'=`var'_1
replace `var'=`var'_2 if `var'==.
replace `var'=. if `var'==-99
lab val `var' yes_no_list
}

* Recode confidante "yes" and "likely" categories
foreach var in friend1_abt_yn friend1_reg_yn friend1_reg_yn_1 friend1_reg_yn_2	///
friend2_abt_yn friend2_reg_yn friend2_reg_yn_1 friend2_reg_yn_2 friend1_abt_mult_yn	///
friend1_reg_mult_yn friend2_abt_mult_yn friend2_reg_mult_yn friend1_abt_issues	///
friend1_reg_issues friend2_abt_issues friend2_reg_issues {

	* INCLUDING UNCERTAIN CONFIDANTE ABORTIONS
	recode `var' (1 2=1 "yes") (-88 0=0 "no/don't know/no response") (-99=.), gen(`var'_v2)
	
	* EXCLUDING UNCERTAIN CONFIDANTE ABORTIONS
	recode `var' (1=1 "yes") (-88 0 2=0 "no/don't know/no response") (-99=.), gen(`var'_v3)
}

* EXCLUDING UNCERTAIN CONFIDANTE ABORTIONS ONLY IF METHOD UNKNOWN
foreach var in friend1_abt friend1_reg friend2_abt friend2_reg  {
gen `var'_yn_v4=`var'_yn_v2
replace `var'_yn_v4=0 if `var'_yn_v4==1 & (`var'_only==-88 | `var'_first==-88 | `var'_last==-88)   
}

* v7: Recode confidante data to calculate percent "don't know" 
foreach var in friend1_abt_yn friend1_reg_yn friend2_abt_yn friend2_reg_yn {
recode `var' (0 1 2=0 "no/yes/likely") (-88=1 "don't know") (-99=.), gen(`var'_v5)
}

foreach var in friend1 friend2 {
gen `var'_comb_dk =1 if `var'_abt_yn_v5==1
replace `var'_comb_dk=1 if `var'_reg_yn_v5==1
replace `var'_comb_dk=0 if `var'_abt_yn_v5==0 & `var'_reg_yn_v5==0
}

* Generate pregnancy removal in last year (2017/2018)
gen self_abt_1718=0 if self_abt_yearSIF!=.
replace self_abt_1718=1 if self_abt_yearSIF==2017 | self_abt_yearSIF==2018

/*
* Generate period regulation in last year (2017/2018)
gen self_reg_1718=0 if self_reg_yearSIF!=.
replace self_reg_1718=1 if self_reg_yearSIF==2017 | self_reg_yearSIF==2018
*/

* Generate indicator variable for whether ever had pregnancy removal or period regulation
	* Respondent
	gen self_combined=0 if self_abt_yn!=.
	replace self_combined=1 if self_abt_yn==1
	replace self_combined=1 if self_reg_yn==1
	gen self_rec_yn=1 if self_combined==1

	* Friends
	foreach var in friend1 friend2 {
	gen `var'_combined=0 if `var'_abt_yn_v2!=.
	replace `var'_combined=1 if `var'_abt_yn_v2==1
	replace `var'_combined=1 if `var'_reg_yn_v2==1
	gen `var'_rec_yn=1 if `var'_combined==1
	}
	
	* Friends (excluding uncertain confidante abortions)
	foreach var in friend1 friend2 {
	gen `var'_combined_v2=0 if `var'_abt_yn_v3!=.
	replace `var'_combined_v2=1 if `var'_abt_yn_v3==1
	replace `var'_combined_v2=1 if `var'_reg_yn_v3==1
	gen `var'_rec_yn_v2=1 if `var'_combined_v2==1
	}
	
	* Friends (excluding uncertain confidante abortions if method unknown)
	foreach var in friend1 friend2 {
	gen `var'_combined_v3=0 if `var'_abt_yn_v4!=.
	replace `var'_combined_v3=1 if `var'_abt_yn_v4==1
	replace `var'_combined_v3=1 if `var'_reg_yn_v4==1
	gen `var'_rec_yn_v3=1 if `var'_combined_v3==1
	}
	
	* v7: Friends (excluding don't know if BOTH abt & reg are -88) 
	foreach var in friend1 friend2 {
	gen `var'_combined_v4=0 if `var'_abt_yn==0 & `var'_reg_yn==0
	replace `var'_combined_v4=1 if `var'_abt_yn==1 | `var'_abt_yn==2
	replace `var'_combined_v4=1 if `var'_reg_yn==1 | `var'_reg_yn==2
	}
	
* Incidence calculation for 2017 and 2018
* Generate pregnancy removal and period regulation incidence variables (among abortions)
foreach var in self_abt self_reg friend1_abt friend1_reg friend2_abt friend2_reg {
gen `var'_inc=0 if `var'_yearSIF!=.
replace `var'_inc=1 if `var'_yearSIF==2017 | `var'_yearSIF==2018
}
	* EXCLUDING UNCERTAIN CONFIDANTE ABORTIONS
	
	foreach var in friend1_abt friend1_reg friend2_abt friend2_reg {
	gen `var'_inc_v2=`var'_inc
	replace `var'_inc_v2=0 if `var'_yn_v3==0 & (`var'_yearSIF==2017 | `var'_yearSIF==2018)
	}
	
	
	* EXCLUDING UNCERTAIN CONFIDANTE ABORTIONS IF METHOD UNKNOWN 
	
	foreach var in friend1_abt friend1_reg friend2_abt friend2_reg {
	gen `var'_inc_v3 = `var'_inc
	replace `var'_inc_v3=0 if `var'_yn_v4==0 & (`var'_yearSIF==2017 | `var'_yearSIF==2018)
	}
	

* Generate pregnancy removal and period regulation incidence variables (among all respondents)
foreach var in self_abt self_reg {
gen `var'_inc_all=0 if `var'_yn!=.
replace `var'_inc_all=1 if `var'_yearSIF==2017 | `var'_yearSIF==2018
}

foreach var in friend1_abt friend1_reg friend2_abt friend2_reg {
gen `var'_inc_all=0 if `var'_yn_v2!=.
replace `var'_inc_all=1 if `var'_yearSIF==2017 | `var'_yearSIF==2018
}

	* EXCLUDING UNCERTAIN CONFIDANTE ABORTIONS
	
	foreach var in friend1_abt friend1_reg friend2_abt friend2_reg {
	gen `var'_inc_all_v2 = `var'_inc_all
	replace `var'_inc_all_v2=0 if `var'_yn_v3==0 & (`var'_yearSIF==2017 | `var'_yearSIF==2018)
	}
	
	
	* EXCLUDING UNCERTAIN CONFIDANTE ABORTIONS IF METHOD NOT KNOWN
	
	foreach var in friend1_abt friend1_reg friend2_abt friend2_reg {
	gen `var'_inc_all_v3 = `var'_inc_all
	replace `var'_inc_all_v3=0 if `var'_yn_v4==0 & (`var'_yearSIF==2017 | `var'_yearSIF==2018)
	}
	
* v7: Generate pregnancy removal and period regulation incidence variables (among all respondents EXCLUDING don't know)
foreach var in friend1_abt friend1_reg friend2_abt friend2_reg {
gen `var'_inc_all_v5=0 if `var'_yn_v5!=. & `var'_yn_v5!=-88
replace `var'_inc_all_v5=1 if `var'_yearSIF==2017 | `var'_yearSIF==2018
}
* Generate combined incidence variables (among abortions)
foreach var in self friend1 friend2 {
gen `var'_combined_inc=0 if `var'_abt_yearSIF!=2020 | `var'_reg_yearSIF!=2020
replace `var'_combined_inc=1 if `var'_abt_yearSIF==2017 | `var'_abt_yearSIF==2018
replace `var'_combined_inc=1 if `var'_reg_yearSIF==2017 | `var'_reg_yearSIF==2018
}

* Generate pregnancy removal and period regulation incidence variables (among all respondents)
gen self_comb_inc_all=0 if self_combined!=.
replace self_comb_inc_all=1 if self_abt_yearSIF==2017 | self_abt_yearSIF==2018
replace self_comb_inc_all=1 if self_reg_yearSIF==2017 | self_reg_yearSIF==2018

foreach var in friend1 friend2 {
gen `var'_comb_inc_all=0 if `var'_combined!=.
	
	* INCLUDING UNCERTAIN CONFIDANTE ABORTIONS
	replace `var'_comb_inc_all=1 if `var'_abt_yearSIF==2017 | `var'_abt_yearSIF==2018
	replace `var'_comb_inc_all=1 if `var'_reg_yearSIF==2017 | `var'_reg_yearSIF==2018
}

foreach var in friend1 friend2 {
gen `var'_comb_inc_all_v2=0 if `var'_combined_v2!=.
	
	* EXCLUDING UNCERTAIN CONFIDANTE ABORTIONS
	replace `var'_comb_inc_all_v2=1 if (`var'_abt_yearSIF==2017 | `var'_abt_yearSIF==2018) & `var'_abt_yn_v3==1
	replace `var'_comb_inc_all_v2=1 if (`var'_reg_yearSIF==2017 | `var'_reg_yearSIF==2018) & `var'_reg_yn_v3==1
}

foreach var in friend1 friend2 {
gen `var'_comb_inc_all_v3=0 if `var'_combined_v3!=.
	
	* EXCLUDING UNCERTAIN CONFIDANTE ABORTIONS IF METHOD UNKNOWN 
	replace `var'_comb_inc_all_v3=1 if (`var'_abt_yearSIF==2017 | `var'_abt_yearSIF==2018) & `var'_abt_yn_v4==1
	replace `var'_comb_inc_all_v3=1 if (`var'_reg_yearSIF==2017 | `var'_reg_yearSIF==2018) & `var'_reg_yn_v4==1
}

* v7: Generate combined incidence variables (among all respondents EXCLUDING don't know)
foreach var in friend1 friend2 {
gen `var'_comb_inc_all_v6=0 if `var'_combined_v4==0
replace `var'_comb_inc_all_v6=1 if `var'_combined_v4==1 & (`var'_abt_yearSIF==2017 | `var'_abt_yearSIF==2018)
replace `var'_comb_inc_all_v6=1 if `var'_combined_v4==1 & (`var'_reg_yearSIF==2017 | `var'_reg_yearSIF==2018)
replace `var'_comb_inc_all_v6=0 if `var'_combined_v4==1 & `var'_comb_inc_all_v6==.
}

/*
* Incidence calculation for 2017 only
* Generate pregnancy removal and period regulation incidence variables (among abortions)
foreach var in self_abt self_reg friend1_abt friend1_reg friend2_abt friend2_reg {
gen `var'_inc17=0 if `var'_yearSIF!=.
replace `var'_inc17=1 if `var'_yearSIF==2017 
}

* Generate pregnancy removal and period regulation incidence variables (among all respondents)
foreach var in self_abt self_reg {
gen `var'_inc_all17=0 if `var'_yn!=.
replace `var'_inc_all17=1 if `var'_yearSIF==2017 
}

foreach var in friend1_abt friend1_reg friend2_abt friend2_reg {
gen `var'_inc_all17=0 if `var'_yn_v2!=.
replace `var'_inc_all17=1 if `var'_yearSIF==2017 
}

* Generate combined incidence variables (among abortions)
foreach var in self friend1 friend2 {
gen `var'_combined_inc17=0 if `var'_abt_yearSIF!=2020 | `var'_reg_yearSIF!=2020
replace `var'_combined_inc17=1 if `var'_abt_yearSIF==2017 
replace `var'_combined_inc17=1 if `var'_reg_yearSIF==2017 
}

* Generate pregnancy removal and period regulation incidence variables (among all respondents)
foreach var in self friend1 friend2 {
gen `var'_comb_inc_all17=0 if `var'_combined!=.
replace `var'_comb_inc_all17=1 if `var'_abt_yearSIF==2017 
replace `var'_comb_inc_all17=1 if `var'_reg_yearSIF==2017 
}
*/

* Generate variable indicating whether pregnancy removal or period regulation 
* happened more recently if had both
lab def most_recent_list 1 "Removal" 2 "Regulation"
foreach var in self friend1 friend2 {
gen `var'_most_recent=1 if `var'_abt_yearSIF>=`var'_reg_yearSIF & `var'_abt_yearSIF!=2020 & `var'_reg_yearSIF!=2020 & `var'_abt_yearSIF!=.
replace `var'_most_recent=2 if `var'_reg_yearSIF>`var'_abt_yearSIF & `var'_abt_yearSIF!=2020 & `var'_reg_yearSIF!=2020 & `var'_reg_yearSIF!=.
replace `var'_most_recent=1 if `var'_abt_yearSIF>`var'_reg_yearSIF & `var'_abt_yearSIF!=2020 & `var'_reg_yearSIF!=2020 & `var'_abt_yearSIF!=.
replace `var'_most_recent=1 if `var'_reg_yearSIF==`var'_abt_yearSIF & `var'_abt_yearSIF==2020 & `var'_reg_yearSIF==2020 & `var'_abt_yearSIF!=.
replace `var'_most_recent=1 if `var'_abt_yearSIF!=. & `var'_abt_yearSIF!=2020 & `var'_reg_yearSIF==2020 & `var'_abt_yearSIF!=.
replace `var'_most_recent=2 if `var'_reg_yearSIF!=. & `var'_reg_yearSIF!=2020 & `var'_abt_yearSIF==2020 & `var'_reg_yearSIF!=.
lab val `var'_most_recent most_recent_list
}

* Generate variable indicating whether pregnancy removal or period regulation 
* happened more recently if had any
foreach var in self friend1 friend2 {
gen `var'_most_recent_all=1 if `var'_abt_yearSIF>=`var'_reg_yearSIF & `var'_abt_yearSIF!=2020 & `var'_reg_yearSIF!=2020 & `var'_abt_yearSIF!=.
replace `var'_most_recent_all=2 if `var'_reg_yearSIF>`var'_abt_yearSIF & `var'_abt_yearSIF!=2020 & `var'_reg_yearSIF!=2020 & `var'_reg_yearSIF!=.
replace `var'_most_recent_all=1 if `var'_abt_yearSIF>`var'_reg_yearSIF & `var'_abt_yearSIF!=2020 & `var'_reg_yearSIF!=2020 & `var'_abt_yearSIF!=.
replace `var'_most_recent_all=1 if `var'_reg_yearSIF==`var'_abt_yearSIF & `var'_abt_yearSIF==2020 & `var'_reg_yearSIF==2020 & `var'_abt_yearSIF!=.
replace `var'_most_recent_all=1 if `var'_abt_yearSIF!=. & `var'_abt_yearSIF!=2020 & `var'_reg_yearSIF==2020 & `var'_abt_yearSIF!=.
replace `var'_most_recent_all=2 if `var'_reg_yearSIF!=. & `var'_reg_yearSIF!=2020 & `var'_abt_yearSIF==2020 & `var'_reg_yearSIF!=.
replace `var'_most_recent_all=1 if `var'_abt_yearSIF!=. & `var'_reg_yearSIF==.
replace `var'_most_recent_all=2 if `var'_reg_yearSIF!=. & `var'_abt_yearSIF==.
lab val `var'_most_recent_all most_recent_list
}

* Generate variable for whether did multiple things with most recent abortion
gen self_rec_mult_yn=0 if self_combined==1
replace self_rec_mult_yn=1 if self_abt_mult_yn==1 & (self_most_recent==1 | self_most_recent==.)
replace self_rec_mult_yn=1 if self_reg_mult_yn==1 & (self_most_recent==2 | self_most_recent==.)
lab val self_rec_mult yes_no_dnk_nr_list

foreach var in friend1 friend2 {
gen `var'_rec_mult_yn=0 if `var'_combined==1
replace `var'_rec_mult_yn=1 if `var'_abt_mult_yn_v2==1 & (`var'_most_recent==1 | `var'_most_recent==.)
replace `var'_rec_mult_yn=1 if `var'_reg_mult_yn_v2==1 & (`var'_most_recent==2 | `var'_most_recent==.)
lab val `var'_rec_mult_yn yes_no_dnk_nr_list
}

* Generate recent ultimate method variable regardless of whether did multiple things
foreach var in self friend1 friend2 {
gen `var'_rec_ult_meth=`var'_abt_only if `var'_most_recent_all==1
replace `var'_rec_ult_meth=`var'_abt_last if `var'_most_recent_all==1 & `var'_rec_ult_meth==.
replace `var'_rec_ult_meth=`var'_reg_only if `var'_most_recent_all==2 & `var'_rec_ult_meth==.
replace `var'_rec_ult_meth=`var'_reg_last if `var'_most_recent_all==2 & `var'_rec_ult_meth==.
lab val `var'_rec_ult_meth abortion_ways_list
}

* Generate variable that is final method, whether only or last
foreach var in self friend1 friend2 {
foreach x in abt reg {
gen `var'_`x'_ult_meth=`var'_`x'_only
replace `var'_`x'_ult_meth=`var'_`x'_last if `var'_`x'_ult_meth==.
lab val `var'_`x'_ult_meth abortion_ways_list
}
}

* Generate recent first method variable regardless of whether did multiple things
foreach var in self friend1 {
gen `var'_rec_first_meth=`var'_abt_only if `var'_most_recent_all==1
replace `var'_rec_first_meth=`var'_abt_first if `var'_most_recent_all==1 & `var'_rec_first_meth==.
replace `var'_rec_first_meth=`var'_reg_only if `var'_most_recent_all==2 & `var'_rec_first_meth==.
replace `var'_rec_first_meth=`var'_reg_first if `var'_most_recent_all==2 & `var'_rec_first_meth==.
lab val `var'_rec_first_meth abortion_ways_list
}

* Generate variable that is first method regardless of whether did multiple things
foreach var in self friend1 {
foreach x in abt {
gen `var'_`x'_first_meth=`var'_`x'_only
replace `var'_`x'_first_meth=`var'_`x'_first if `var'_`x'_first_meth==.
lab val `var'_`x'_first_meth abortion_ways_list
}
}

* Generate three category abortion method variables
foreach var in self_abt_first self_abt_only self_abt_last self_reg_first self_reg_only self_reg_last	///
friend1_abt_first friend1_abt_only friend1_abt_last friend1_reg_first friend1_reg_only friend1_reg_last	///
friend2_abt_first friend2_abt_only friend2_abt_last friend2_reg_first friend2_reg_only friend2_reg_last	///
self_rec_ult_meth self_abt_ult_meth self_reg_ult_meth friend1_rec_ult_meth friend1_abt_ult_meth	///
friend1_reg_ult_meth friend2_rec_ult_meth friend2_abt_ult_meth friend2_reg_ult_meth {
recode `var' (1=1 "Surgery") (2 3 4 5=2 "Pills") (6/13 -88=3 "Other") (-99=.), gen(`var'3)
}

* Generate four category abortion method variables
foreach var in self_abt_first self_abt_only self_abt_last self_reg_first self_reg_only self_reg_last	///
friend1_abt_first friend1_abt_only friend1_abt_last friend1_reg_first friend1_reg_only friend1_reg_last	///
friend2_abt_first friend2_abt_only friend2_abt_last friend2_reg_first friend2_reg_only friend2_reg_last ///
self_rec_ult_meth self_abt_ult_meth self_reg_ult_meth friend1_rec_ult_meth friend1_abt_ult_meth	///
friend1_reg_ult_meth friend2_rec_ult_meth friend2_abt_ult_meth friend2_reg_ult_meth self_rec_first_meth	///
self_abt_first_meth friend1_rec_first_meth friend1_abt_first_meth {
recode `var' (1=1 "Surgery") (2=2 "Mife/miso") (3 4 5=3 "Other pills/pill type unknown")(6/13 -88=4 "Traditional/other methods") (-99=.), gen(`var'4)
}

* Generate five category abortion method variables
foreach var in self_abt_first self_abt_only self_abt_last self_reg_first self_reg_only self_reg_last	///
friend1_abt_first friend1_abt_only friend1_abt_last friend1_reg_first friend1_reg_only friend1_reg_last	///
friend2_abt_first friend2_abt_only friend2_abt_last friend2_reg_first friend2_reg_only friend2_reg_last ///
self_rec_ult_meth self_abt_ult_meth self_reg_ult_meth friend1_rec_ult_meth friend1_abt_ult_meth	///
friend1_reg_ult_meth friend2_rec_ult_meth friend2_abt_ult_meth friend2_reg_ult_meth {
recode `var' (1=1 "Surgery") (2=2 "Mife/miso") (3 4 5=3 "Other pills/pill type unknown") (6=4 "Injection")(7/13 -88=5 "Traditional/other methods") (-99=.), gen(`var'5)
}

* Generate methods and source of most recent abortion variables
foreach x in self friend1 friend2 {
gen `x'_rec_only=`x'_abt_only if `x'_most_recent_all==1 & `x'_abt_only!=.
replace `x'_rec_only=`x'_reg_only if `x'_most_recent_all==2 & `x'_abt_only!=.

gen `x'_rec_first=`x'_abt_first if `x'_most_recent_all==1 & `x'_abt_first!=.
replace `x'_rec_first=`x'_reg_first if `x'_most_recent_all==2 & `x'_abt_first!=.

gen `x'_rec_last=`x'_abt_last if `x'_most_recent_all==1 & `x'_abt_last!=.
replace `x'_rec_last=`x'_reg_last if `x'_most_recent_all==2 & `x'_abt_last!=.

gen `x'_rec_where=`x'_abt_where if `x'_most_recent_all==1 & `x'_abt_where!=.
replace `x'_rec_where=`x'_reg_where if `x'_most_recent_all==2 & `x'_abt_where!=.

gen `x'_rec_meds=`x'_abt_meds if `x'_most_recent_all==1 & `x'_abt_meds!=.
replace `x'_rec_meds=`x'_reg_meds if `x'_most_recent_all==2 & `x'_abt_meds!=.

gen `x'_rec_last_where=`x'_abt_last_where if `x'_most_recent_all==1 & `x'_abt_last_where!=.
replace `x'_rec_last_where=`x'_reg_last_where if `x'_most_recent_all==2 & `x'_abt_last_where!=.

gen `x'_rec_last_meds=`x'_abt_last_meds if `x'_most_recent_all==1 & `x'_abt_last_meds!=.
replace `x'_rec_last_meds=`x'_reg_last_meds if `x'_most_recent_all==2 & `x'_abt_last_meds!=.

lab val `x'_rec_only `x'_rec_first `x'_rec_last abortion_ways_list
lab val `x'_rec_where `x'_rec_meds `x'_rec_last_where `x'_rec_last_meds providers_list
}

* Generate one three category variable related to method of most recent abortion
lab def method3_list 1 "Surgery" 2 "Pills" 3 "Other"
foreach x in self friend1 friend2 {
foreach y in only first last {
gen `x'_rec_`y'3=0 if (`x'_most_recent_all==1 & `x'_abt_`y'!=.) | (`x'_most_recent_all==2 & `x'_reg_`y'!=.)
replace `x'_rec_`y'3=1 if (`x'_abt_`y'3==1 & `x'_abt_`y'!=.) | (`x'_reg_`y'3==1 & `x'_reg_`y'!=.)
replace `x'_rec_`y'3=2 if (`x'_abt_`y'3==2 & `x'_abt_`y'!=.) | (`x'_reg_`y'3==2 & `x'_reg_`y'!=.)
replace `x'_rec_`y'3=3 if (`x'_abt_`y'3==3 & `x'_abt_`y'!=.) | (`x'_reg_`y'3==3 & `x'_reg_`y'!=.)
replace `x'_rec_`y'3=. if `x'_rec_`y'3==0
lab val `x'_rec_`y'3 method3_list
}
}

* Generate one four category variable related to method of most recent abortion
lab def method4_list 1 "Surgery" 2 "Mife/miso" 3 "Other pills/pill type unknown" 4 "Traditional/other methods"
foreach x in self friend1 friend2 {
foreach y in only first last {
gen `x'_rec_`y'4=0 if (`x'_most_recent_all==1 & `x'_abt_`y'!=.) | (`x'_most_recent_all==2 & `x'_reg_`y'!=.)
replace `x'_rec_`y'4=1 if (`x'_abt_`y'4==1 & `x'_abt_`y'!=.) | (`x'_reg_`y'4==1 & `x'_reg_`y'!=.)
replace `x'_rec_`y'4=2 if (`x'_abt_`y'4==2 & `x'_abt_`y'!=.) | (`x'_reg_`y'4==2 & `x'_reg_`y'!=.)
replace `x'_rec_`y'4=3 if (`x'_abt_`y'4==3 & `x'_abt_`y'!=.) | (`x'_reg_`y'4==3 & `x'_reg_`y'!=.)
replace `x'_rec_`y'4=4 if (`x'_abt_`y'4==4 & `x'_abt_`y'!=.) | (`x'_reg_`y'4==4 & `x'_reg_`y'!=.)
replace `x'_rec_`y'4=. if `x'_rec_`y'4==0
lab val `x'_rec_`y'4 method4_list
}
}

* Generate one five category variable related to method of most recent abortion
lab def method5_list 1 "Surgery" 2 "Mife/miso" 3 "Other pills/pill type unknown" 4 "Injection" 5 "Traditional/other methods"
foreach x in self friend1 friend2 {
foreach y in only first last {
gen `x'_rec_`y'5=0 if (`x'_most_recent_all==1 & `x'_abt_`y'!=.) | (`x'_most_recent_all==2 & `x'_reg_`y'!=.)
replace `x'_rec_`y'5=1 if (`x'_abt_`y'5==1 & `x'_abt_`y'!=.) | (`x'_reg_`y'5==1 & `x'_reg_`y'!=.)
replace `x'_rec_`y'5=2 if (`x'_abt_`y'5==2 & `x'_abt_`y'!=.) | (`x'_reg_`y'5==2 & `x'_reg_`y'!=.)
replace `x'_rec_`y'5=3 if (`x'_abt_`y'5==3 & `x'_abt_`y'!=.) | (`x'_reg_`y'5==3 & `x'_reg_`y'!=.)
replace `x'_rec_`y'5=4 if (`x'_abt_`y'5==4 & `x'_abt_`y'!=.) | (`x'_reg_`y'5==4 & `x'_reg_`y'!=.)
replace `x'_rec_`y'5=5 if (`x'_abt_`y'5==5 & `x'_abt_`y'!=.) | (`x'_reg_`y'5==5 & `x'_reg_`y'!=.)
replace `x'_rec_`y'5=. if `x'_rec_`y'5==0
lab val `x'_rec_`y'5 method5_list
}
}

* Sought care in a health facility for complications
gen self_rec_issues=0 if self_most_recent_all!=.
replace self_rec_issues=1 if self_most_recent_all==1 & self_abt_issues==1
replace self_rec_issues=1 if self_most_recent_all==2 & self_reg_issues==1

foreach var in friend1 friend2 {
gen `var'_rec_issues=0 if `var'_most_recent_all!=.
replace `var'_rec_issues=1 if `var'_most_recent_all==1 & `var'_abt_issues_v2==1
replace `var'_rec_issues=1 if `var'_most_recent_all==2 & `var'_reg_issues_v2==1
label val `var'_rec_issues abortion_ways_list
}

* Generate four category abortion source variables
foreach x in self friend1 {
foreach y in abt reg rec {
foreach z in where meds last_where last_meds {
recode `x'_`y'_`z' (11 12 13 14=1 "Public facility") (21 24 25 34=2 "Private facility") (22 23=3 "Pharmacy/chemist") (15 26 31 32 33 35 96 97 -88=4 "Other"), gen(`x'_`y'_`z'4)
}
}
}

* Combine surgery and pill ultimate source variables
lab def source4_list 1 "Public facility" 2 "Private facility" 3 "Pharmacy/chemist" 4 "Other"
foreach x in self friend1 {
foreach y in abt reg rec {
gen `x'_`y'_ult_source4=`x'_`y'_where4 if `x'_`y'_only==1 
replace `x'_`y'_ult_source4=`x'_`y'_last_where4 if `x'_`y'_last==1 & `x'_`y'_ult_source4==.
replace `x'_`y'_ult_source4=`x'_`y'_meds4 if `x'_`y'_ult_source4==. & (`x'_`y'_only==2 | `x'_`y'_only==3 | `x'_`y'_only==4 | `x'_`y'_only==5)
replace `x'_`y'_ult_source4=`x'_`y'_last_meds4 if `x'_`y'_ult_source4==. & (`x'_`y'_last==2 | `x'_`y'_last==3 | `x'_`y'_last==4 | `x'_`y'_last==5)

* Assume injections come from private provider (or pharmacy/chemist?) - ASSUMPTION
replace `x'_`y'_ult_source4=2 if (`x'_`y'_only==6 | `x'_`y'_last==6) & `x'_`y'_ult_source4==.

* Assume all non-surgery/pills come from "other" location
replace `x'_`y'_ult_source4=4 if `x'_`y'_ult_source4==. & (`x'_`y'_yn==1 | `x'_`y'_yn==2) 
lab val `x'_`y'_ult_source4 source4_list
}
}

* Combine surgery and pill first source variables
foreach x in self friend1 {
foreach y in abt reg rec {
gen `x'_`y'_first_source4=`x'_`y'_where4 
replace `x'_`y'_first_source4=`x'_`y'_meds4 if `x'_`y'_first_source4==. 

* Assume injections come from private provider (or pharmacy/chemist?)
replace `x'_`y'_first_source4=2 if (`x'_`y'_first==6 | `x'_`y'_only==6) & `x'_`y'_first_source4==.

* Assume all non-surgery/pills come from "other" location
replace `x'_`y'_first_source4=4 if `x'_`y'_first_source4==. & (`x'_`y'_yn==1 | `x'_`y'_yn==2) 

* Create variable that is only first for women who do multiple
gen `x'_`y'_onlyfirst_source4=`x'_`y'_first_source4 if `x'_`y'_mult_yn==1 | `x'_`y'_mult_yn==2

lab val `x'_`y'_first_source4 `x'_`y'_onlyfirst_source4 source4_list
}
}

* EXCLUDE ABORTIONS/REGULATIONS WHERE ONLY EMERGENCY CONTRACEPTION USED
foreach var in self friend1 friend2 {
gen `var'_abt_inc_all_v4 =`var'_abt_inc_all
replace `var'_abt_inc_all_v4 =0  if `var'_abt_only==4 & `var'_abt_issues==0
gen `var'_reg_inc_all_v4 =`var'_reg_inc_all
replace `var'_reg_inc_all_v4 =0  if `var'_reg_only==4 & `var'_reg_issues==0
gen `var'_comb_inc_all_v4 =`var'_comb_inc_all
replace `var'_comb_inc_all_v4 =0  if `var'_rec_only==4 & `var'_rec_issues==0
}

* Exclude regulations where only emergency contraception used
foreach var in self friend1 friend2 {
gen `var'_comb_inc_all_v5 =`var'_comb_inc_all
replace `var'_comb_inc_all_v5 =0  if `var'_reg_only==4 & `var'_reg_issues==0
}

/* v9: Generate pregnancy removal and period regulation incidence variables for confidantes 
excluding uncertain removals and regulations if method unknown AND excluding EC use with
no reported complications
*/
foreach var in friend1_abt friend1_reg friend1_comb friend2_abt friend2_reg friend2_comb {
	gen `var'_inc_all_v7 = `var'_inc_all_v3
	replace `var'_inc_all_v7 = 0 if `var'_inc_all_v4==0 
	}
	
* Generate dichotomous abortion safety variable (safe, unsafe)
lab def safety2_list 0 "Safe" 1 "Unsafe"
lab def safety3_list 0 "Safe" 1 "Less safe" 2 "Least safe"
lab def safe2_list 0 "Unsafe" 1 "Safe"
lab def safe2v3_list 0 "Safe/less safe" 1 "Least safe"

foreach x in self friend1 friend2 {
foreach y in abt reg rec {
gen `x'_`y'_safety2=1 if `x'_`y'_yn==1 | `x'_`y'_yn==2

* Generate dichotmous abortion safety variable
	* Safe
		* Only surgery in a government or private facility, no issues reported
		replace `x'_`y'_safety2=0 if `x'_`y'_only==1 & (`x'_`y'_where==11 | `x'_`y'_where==12 | `x'_`y'_where==13 | `x'_`y'_where==14 | `x'_`y'_where==21 | `x'_`y'_where==24 | `x'_`y'_where==25 | `x'_`y'_where==34) & `x'_`y'_issues==0

		* Only mife/miso from government or private facility, no issues reported
		replace `x'_`y'_safety2=0 if `x'_`y'_only==2 & (`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34) & `x'_`y'_issues==0

* Generate categorical abortion safety variable (safe, less safe, least safe)
gen `x'_`y'_safety3=2 if `x'_`y'_yn==1 | `x'_`y'_yn==2

	* Safe
		* Only surgery in a government or private facility, no issues reported
		replace `x'_`y'_safety3=0 if `x'_`y'_only==1 & (`x'_`y'_where==11 | `x'_`y'_where==12 | `x'_`y'_where==13 | `x'_`y'_where==14 | `x'_`y'_where==21 | `x'_`y'_where==24 | `x'_`y'_where==25 | `x'_`y'_where==34) & `x'_`y'_issues==0

		* Only mife/miso from government or private facility, no issues reported
		replace `x'_`y'_safety3=0 if `x'_`y'_only==2 & (`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34) & `x'_`y'_issues==0
		
	* Less safe
		* Only surgery in a government or private facility, issues reported
		replace `x'_`y'_safety3=1 if `x'_`y'_only==1 & (`x'_`y'_where==11 | `x'_`y'_where==12 | `x'_`y'_where==13 | `x'_`y'_where==14 | `x'_`y'_where==21 | `x'_`y'_where==24 | `x'_`y'_where==25 | `x'_`y'_where==34) & `x'_`y'_issues==1

		* Only mife/miso from government or private facility, issues reported
		replace `x'_`y'_safety3=1 if `x'_`y'_only==2 & (`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34) & `x'_`y'_issues==1
		
		* Only mife/miso from pharmacy/chemist , no issues were reported
		replace `x'_`y'_safety3=1 if `x'_`y'_only==2 & (`x'_`y'_meds==22 | `x'_`y'_meds==23) & `x'_`y'_issues==0

		* Only "other pills/pill type unkown" from government or private facility, no issues reported
		replace `x'_`y'_safety3=1 if `x'_`y'_only==5 & (`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34) & `x'_`y'_issues==0

		* IF EXCLUDING "OTHER PILLS/PILL TYPE UNKNOWN" FROM PHARMACY/CHEMISTS FROM LESS SAFE, COMMENT OUT BELOW CODE
		* Only "other pills/pill type unkown" from pharmacy/chemist, no issues reported
		* replace `x'_`y'_safety3=1 if `x'_`y'_only==5 & (`x'_`y'_meds==22 | `x'_`y'_meds==23) & `x'_`y'_issues==0

		* First surgery in government or private facility or mife/miso from government or private facility or chemist/pharmacist, last surgery in a government or private facility, no issues reported
		replace `x'_`y'_safety3=1 if ((`x'_`y'_first==1 & (`x'_`y'_where==11 | `x'_`y'_where==12 | `x'_`y'_where==13 | `x'_`y'_where==14 | `x'_`y'_where==21 | `x'_`y'_where==24 | `x'_`y'_where==25 | `x'_`y'_where==34)) | (`x'_`y'_first==2 & ((`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34 | `x'_`y'_meds==22 | `x'_`y'_meds==23))) & (`x'_`y'_last==1 & (`x'_`y'_last_where==11 | `x'_`y'_last_where==12 | `x'_`y'_last_where==13 | `x'_`y'_last_where==14 | `x'_`y'_last_where==21 | `x'_`y'_last_where==24 | `x'_`y'_last_where==25 | `x'_`y'_last_where==34))) & `x'_`y'_issues==0
		
		* First surgery in government or private facility or mife/miso from government or private facility or chemist/pharmacist, last surgery in a government or private facility, issues reported
		replace `x'_`y'_safety3=1 if ((`x'_`y'_first==1 & (`x'_`y'_where==11 | `x'_`y'_where==12 | `x'_`y'_where==13 | `x'_`y'_where==14 | `x'_`y'_where==21 | `x'_`y'_where==24 | `x'_`y'_where==25 | `x'_`y'_where==34)) | (`x'_`y'_first==2 & ((`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34 | `x'_`y'_meds==22 | `x'_`y'_meds==23))) & (`x'_`y'_last==1 & (`x'_`y'_last_where==11 | `x'_`y'_last_where==12 | `x'_`y'_last_where==13 | `x'_`y'_last_where==14 | `x'_`y'_last_where==21 | `x'_`y'_last_where==24 | `x'_`y'_last_where==25 | `x'_`y'_last_where==34))) & `x'_`y'_issues==1

		* First surgery in government or private facility or mife/miso from government or private facility or chemist/pharmacist, last mife/miso in a government or private facility or pharmacy/chemist, no issues reported
		replace `x'_`y'_safety3=1 if ((`x'_`y'_first==1 & (`x'_`y'_where==11 | `x'_`y'_where==12 | `x'_`y'_where==13 | `x'_`y'_where==14 | `x'_`y'_where==21 | `x'_`y'_where==24 | `x'_`y'_where==25 | `x'_`y'_where==34)) | (`x'_`y'_first==2 & ((`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34 | `x'_`y'_meds==22 | `x'_`y'_meds==23))) & (`x'_`y'_last==2 & (`x'_`y'_last_meds==11 | `x'_`y'_last_meds==12 | `x'_`y'_last_meds==13 | `x'_`y'_last_meds==14 | `x'_`y'_last_meds==21 | `x'_`y'_last_meds==24 | `x'_`y'_last_meds==25 | `x'_`y'_last_meds==34 | `x'_`y'_last_meds==22 | `x'_`y'_last_meds==23))) & `x'_`y'_issues==0
		
		* First surgery in government or private facility or mife/miso from government or private facility or chemist/pharmacist, last mife/miso in a government or private facility or pharmacy/chemist, issues reported
		replace `x'_`y'_safety3=1 if ((`x'_`y'_first==1 & (`x'_`y'_where==11 | `x'_`y'_where==12 | `x'_`y'_where==13 | `x'_`y'_where==14 | `x'_`y'_where==21 | `x'_`y'_where==24 | `x'_`y'_where==25 | `x'_`y'_where==34)) | (`x'_`y'_first==2 & ((`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34 | `x'_`y'_meds==22 | `x'_`y'_meds==23))) & (`x'_`y'_last==2 & (`x'_`y'_last_meds==11 | `x'_`y'_last_meds==12 | `x'_`y'_last_meds==13 | `x'_`y'_last_meds==14 | `x'_`y'_last_meds==21 | `x'_`y'_last_meds==24 | `x'_`y'_last_meds==25 | `x'_`y'_last_meds==34 | `x'_`y'_last_meds==22 | `x'_`y'_last_meds==23))) & `x'_`y'_issues==1

}
}

* Generate safety categorization without including the issues variable
foreach x in self friend1 friend2 {
foreach y in abt reg rec {
gen `x'_`y'_safety2v2=1 if `x'_`y'_yn==1 | `x'_`y'_yn==2

* Generate dichotmous abortion safety variable
	* Safe
		* Only surgery in a government or private facility  
		replace `x'_`y'_safety2v2=0 if `x'_`y'_only==1 & (`x'_`y'_where==11 | `x'_`y'_where==12 | `x'_`y'_where==13 | `x'_`y'_where==14 | `x'_`y'_where==21 | `x'_`y'_where==24 | `x'_`y'_where==25 | `x'_`y'_where==34) 

		* Only mife/miso from government or private facility 
		replace `x'_`y'_safety2v2=0 if `x'_`y'_only==2 & (`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34) 

* Generate categorical abortion safety variable (safe, less safe, least safe)
gen `x'_`y'_safety3v2=2 if `x'_`y'_yn==1 | `x'_`y'_yn==2

	* Safe
		* Only surgery in a government or private facility  
		replace `x'_`y'_safety3v2=0 if `x'_`y'_only==1 & (`x'_`y'_where==11 | `x'_`y'_where==12 | `x'_`y'_where==13 | `x'_`y'_where==14 | `x'_`y'_where==21 | `x'_`y'_where==24 | `x'_`y'_where==25 | `x'_`y'_where==34) 

		* Only mife/miso from government or private facility 
		replace `x'_`y'_safety3v2=0 if `x'_`y'_only==2 & (`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34) 
		
	* Less safe
		* Only mife/miso from pharmacy/chemist
		replace `x'_`y'_safety3v2=1 if `x'_`y'_only==2 & (`x'_`y'_meds==22 | `x'_`y'_meds==23) 

		* Only "other pills/pill type unkown" from government or private facility
		replace `x'_`y'_safety3v2=1 if `x'_`y'_only==5 & (`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34) 

		* IF EXCLUDING "OTHER PILLS/PILL TYPE UNKNOWN" FROM PHARMACY/CHEMISTS FROM LESS SAFE, COMMENT OUT BELOW CODE
		* Only "other pills/pill type unkown" from pharmacy/chemist
		*replace `x'_`y'_safety3v2=1 if `x'_`y'_only==5 & (`x'_`y'_meds==22 | `x'_`y'_meds==23) 
		
		* First surgery in government or private facility or mife/miso from government or private facility or chemist/pharmacist, last surgery in a government or private facility
		replace `x'_`y'_safety3v2=1 if ((`x'_`y'_first==1 & (`x'_`y'_where==11 | `x'_`y'_where==12 | `x'_`y'_where==13 | `x'_`y'_where==14 | `x'_`y'_where==21 | `x'_`y'_where==24 | `x'_`y'_where==25 | `x'_`y'_where==34)) | (`x'_`y'_first==2 & ((`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34 | `x'_`y'_meds==22 | `x'_`y'_meds==23))) & (`x'_`y'_last==1 & (`x'_`y'_last_where==11 | `x'_`y'_last_where==12 | `x'_`y'_last_where==13 | `x'_`y'_last_where==14 | `x'_`y'_last_where==21 | `x'_`y'_last_where==24 | `x'_`y'_last_where==25 | `x'_`y'_last_where==34))) 
		
		* First surgery in government or private facility or mife/miso from government or private facility or chemist/pharmacist, last mife/miso in a government or private facility or pharmacy/chemist
		replace `x'_`y'_safety3v2=1 if ((`x'_`y'_first==1 & (`x'_`y'_where==11 | `x'_`y'_where==12 | `x'_`y'_where==13 | `x'_`y'_where==14 | `x'_`y'_where==21 | `x'_`y'_where==24 | `x'_`y'_where==25 | `x'_`y'_where==34)) | (`x'_`y'_first==2 & ((`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34 | `x'_`y'_meds==22 | `x'_`y'_meds==23))) & (`x'_`y'_last==2 & (`x'_`y'_last_meds==11 | `x'_`y'_last_meds==12 | `x'_`y'_last_meds==13 | `x'_`y'_last_meds==14 | `x'_`y'_last_meds==21 | `x'_`y'_last_meds==24 | `x'_`y'_last_meds==25 | `x'_`y'_last_meds==34 | `x'_`y'_last_meds==22 | `x'_`y'_last_meds==23))) 
		
		* Label safety variable response options
		lab val `x'_`y'_safety2v2 safety2_list
		lab val `x'_`y'_safety3v2 safety3_list
	}
	}
	
* Generate safety categorization without including the issues variable and coding multiple safe things as safe
foreach x in self friend1 friend2 {
foreach y in abt reg rec {
gen `x'_`y'_safety2v3=1 if `x'_`y'_yn==1 | `x'_`y'_yn==2

* Generate dichotmous abortion safety variable
	* Safe
		* Only surgery in a government or private facility  
		replace `x'_`y'_safety2v3=0 if `x'_`y'_only==1 & (`x'_`y'_where==11 | `x'_`y'_where==12 | `x'_`y'_where==13 | `x'_`y'_where==14 | `x'_`y'_where==21 | `x'_`y'_where==24 | `x'_`y'_where==25 | `x'_`y'_where==34) 

		* Only mife/miso from government or private facility 
		replace `x'_`y'_safety2v3=0 if `x'_`y'_only==2 & (`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34) 

		* First mife/miso from government or private facility, last surgery in a government or private facility   
		replace `x'_`y'_safety2v3=0 if (`x'_`y'_first==2 & (`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34)) & (`x'_`y'_last==1 & (`x'_`y'_last_where==11 | `x'_`y'_last_where==12 | `x'_`y'_last_where==13 | `x'_`y'_last_where==14 | `x'_`y'_last_where==21 | `x'_`y'_last_where==24 | `x'_`y'_last_where==25 | `x'_`y'_last_where==34)) 
		
		* First mife/miso from government or private facility, last mife/miso in a government or private facility   
		replace `x'_`y'_safety2v3=0 if (`x'_`y'_first==2 & (`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34)) & (`x'_`y'_last==2 & (`x'_`y'_last_meds==11 | `x'_`y'_last_meds==12 | `x'_`y'_last_meds==13 | `x'_`y'_last_meds==14 | `x'_`y'_last_meds==21 | `x'_`y'_last_meds==24 | `x'_`y'_last_meds==25 | `x'_`y'_last_meds==34)) 
		
		* First surgery in government or private facility, last mife/miso in a government or private facility   
		replace `x'_`y'_safety2v3=0 if (`x'_`y'_first==1 & (`x'_`y'_where==11 | `x'_`y'_where==12 | `x'_`y'_where==13 | `x'_`y'_where==14 | `x'_`y'_where==21 | `x'_`y'_where==24 | `x'_`y'_where==25 | `x'_`y'_where==34)) & (`x'_`y'_last==2 & (`x'_`y'_last_meds==11 | `x'_`y'_last_meds==12 | `x'_`y'_last_meds==13 | `x'_`y'_last_meds==14 | `x'_`y'_last_meds==21 | `x'_`y'_last_meds==24 | `x'_`y'_last_meds==25 | `x'_`y'_last_meds==34)) 
		
* Generate categorical abortion safety variable (safe, less safe, least safe)
gen `x'_`y'_safety3v3=2 if `x'_`y'_yn==1 | `x'_`y'_yn==2

	* Safe
		* Only surgery in a government or private facility  
		replace `x'_`y'_safety3v3=0 if `x'_`y'_only==1 & (`x'_`y'_where==11 | `x'_`y'_where==12 | `x'_`y'_where==13 | `x'_`y'_where==14 | `x'_`y'_where==21 | `x'_`y'_where==24 | `x'_`y'_where==25 | `x'_`y'_where==34) 

		* Only mife/miso from government or private facility 
		replace `x'_`y'_safety3v3=0 if `x'_`y'_only==2 & (`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34) 
		
		* First mife/miso from government or private facility, last surgery in a government or private facility   
		replace `x'_`y'_safety3v3=0 if (`x'_`y'_first==2 & (`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34)) & (`x'_`y'_last==1 & (`x'_`y'_last_where==11 | `x'_`y'_last_where==12 | `x'_`y'_last_where==13 | `x'_`y'_last_where==14 | `x'_`y'_last_where==21 | `x'_`y'_last_where==24 | `x'_`y'_last_where==25 | `x'_`y'_last_where==34)) 
		
		* First mife/miso from government or private facility, last mife/miso in a government or private facility   
		replace `x'_`y'_safety3v3=0 if (`x'_`y'_first==2 & (`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34)) & (`x'_`y'_last==2 & (`x'_`y'_last_meds==11 | `x'_`y'_last_meds==12 | `x'_`y'_last_meds==13 | `x'_`y'_last_meds==14 | `x'_`y'_last_meds==21 | `x'_`y'_last_meds==24 | `x'_`y'_last_meds==25 | `x'_`y'_last_meds==34)) 
		
		* First surgery in government or private facility, last mife/miso in a government or private facility   
		replace `x'_`y'_safety3v3=0 if (`x'_`y'_first==1 & (`x'_`y'_where==11 | `x'_`y'_where==12 | `x'_`y'_where==13 | `x'_`y'_where==14 | `x'_`y'_where==21 | `x'_`y'_where==24 | `x'_`y'_where==25 | `x'_`y'_where==34)) & (`x'_`y'_last==2 & (`x'_`y'_last_meds==11 | `x'_`y'_last_meds==12 | `x'_`y'_last_meds==13 | `x'_`y'_last_meds==14 | `x'_`y'_last_meds==21 | `x'_`y'_last_meds==24 | `x'_`y'_last_meds==25 | `x'_`y'_last_meds==34)) 
		
		* First surgery in government or private facility or mife/miso from a pharmacy/chemist/other, last surgery from a government or private facility
		replace `x'_`y'_safety3v3=1 if ((`x'_`y'_first==1 & (`x'_`y'_where==11 | `x'_`y'_where==12 | `x'_`y'_where==13 | `x'_`y'_where==14 | `x'_`y'_where==21 | `x'_`y'_where==24 | `x'_`y'_where==25 | `x'_`y'_where==34)) | (`x'_`y'_first==2 & (`x'_`y'_meds==15 | `x'_`y'_meds==22 | `x'_`y'_meds==23 | `x'_`y'_meds==26 | `x'_`y'_meds==31 | `x'_`y'_meds==32 | `x'_`y'_meds==33 | `x'_`y'_meds==35 | `x'_`y'_meds==96))) & (`x'_`y'_last==1 & (`x'_`y'_last_where==11 | `x'_`y'_last_where==12 | `x'_`y'_last_where==13 | `x'_`y'_last_where==14 | `x'_`y'_last_where==21 | `x'_`y'_last_where==24 | `x'_`y'_last_where==25 | `x'_`y'_last_where==34))

	* Less safe
		* Only mife/miso from pharmacy/chemist/other
		replace `x'_`y'_safety3v3=1 if `x'_`y'_only==2 & (`x'_`y'_meds==15 | `x'_`y'_meds==22 | `x'_`y'_meds==23 | `x'_`y'_meds==26 | `x'_`y'_meds==31 | `x'_`y'_meds==32 | `x'_`y'_meds==33 | `x'_`y'_meds==35 | `x'_`y'_meds==96) 

		* Only "other pills/pill type unkown" from government or private facility
		replace `x'_`y'_safety3v3=1 if `x'_`y'_only==5 & (`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34) 

		* IF EXCLUDING "OTHER PILLS/PILL TYPE UNKNOWN" FROM PHARMACY/CHEMISTS FROM LESS SAFE, COMMENT OUT BELOW CODE
		* Only "other pills/pill type unkown" from pharmacy/chemist
		*replace `x'_`y'_safety3v3=1 if `x'_`y'_only==5 & (`x'_`y'_meds==22 | `x'_`y'_meds==23) 
			
		* First surgery or mife/miso in government or private facility, last mife/miso from a pharmacy/chemist/other
		replace `x'_`y'_safety3v3=1 if ((`x'_`y'_first==1 & (`x'_`y'_where==11 | `x'_`y'_where==12 | `x'_`y'_where==13 | `x'_`y'_where==14 | `x'_`y'_where==21 | `x'_`y'_where==24 | `x'_`y'_where==25 | `x'_`y'_where==34)) | (`x'_`y'_first==2 & (`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34))) & (`x'_`y'_last==2 & (`x'_`y'_last_meds==15 | `x'_`y'_last_meds==22 | `x'_`y'_last_meds==23 | `x'_`y'_last_meds==26 | `x'_`y'_last_meds==31 | `x'_`y'_last_meds==32 | `x'_`y'_last_meds==33 | `x'_`y'_last_meds==35 | `x'_`y'_last_meds==96))

		* First mife/miso from a pharmacist/chemist/other, last mife/miso in a government or private facility 
		replace `x'_`y'_safety3v3=1 if (`x'_`y'_first==2 & (`x'_`y'_meds==15 | `x'_`y'_meds==22 | `x'_`y'_meds==23 | `x'_`y'_meds==26 | `x'_`y'_meds==31 | `x'_`y'_meds==32 | `x'_`y'_meds==33 | `x'_`y'_meds==35 | `x'_`y'_meds==96)) & (`x'_`y'_last==2 & (`x'_`y'_last_meds==11 | `x'_`y'_last_meds==12 | `x'_`y'_last_meds==13 | `x'_`y'_last_meds==14 | `x'_`y'_last_meds==21 | `x'_`y'_last_meds==24 | `x'_`y'_last_meds==25 | `x'_`y'_last_meds==34))

		* First mife/miso from pharmacy/chemist, last mife/miso from pharmacy/chemist/other 
		replace `x'_`y'_safety3v3=1 if (`x'_`y'_first==2 & (`x'_`y'_meds==15 | `x'_`y'_meds==22 | `x'_`y'_meds==23 | `x'_`y'_meds==26 | `x'_`y'_meds==31 | `x'_`y'_meds==32 | `x'_`y'_meds==33 | `x'_`y'_meds==35 | `x'_`y'_meds==96)) & (`x'_`y'_last==2 & (`x'_`y'_last_meds==15 | `x'_`y'_last_meds==22 | `x'_`y'_last_meds==23 | `x'_`y'_last_meds==26 | `x'_`y'_last_meds==31 | `x'_`y'_last_meds==32 | `x'_`y'_last_meds==33 | `x'_`y'_last_meds==35 | `x'_`y'_last_meds==96))
		
		* Generate safe (1)/unsafe (0) variables
		gen `x'_`y'_safe2v3=0 if `x'_`y'_safety3v3==0 | `x'_`y'_safety3v3==1
		replace `x'_`y'_safe2v3=1 if `x'_`y'_safety3v3==2

		* Label safety variable response options
		lab val `x'_`y'_safety2v3 safety2_list
		lab val `x'_`y'_safety3v3 safety3_list
		lab val `x'_`y'_safe2v3 safe2v3_list

	}
	}
	
* Generate safety categorization like v3 but only using the last method
foreach x in self friend1 friend2 {
foreach y in abt reg rec {
gen `x'_`y'_safety2v4=1 if `x'_`y'_yn==1 | `x'_`y'_yn==2

* Generate dichotmous abortion safety variable
	* Safe
		* Only surgery in a government or private facility  
		replace `x'_`y'_safety2v4=0 if `x'_`y'_only==1 & (`x'_`y'_where==11 | `x'_`y'_where==12 | `x'_`y'_where==13 | `x'_`y'_where==14 | `x'_`y'_where==21 | `x'_`y'_where==24 | `x'_`y'_where==25 | `x'_`y'_where==34) 

		* Only mife/miso from government or private facility 
		replace `x'_`y'_safety2v4=0 if `x'_`y'_only==2 & (`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34) 

		* Last surgery in a government or private facility   
		replace `x'_`y'_safety2v4=0 if (`x'_`y'_last==1 & (`x'_`y'_last_where==11 | `x'_`y'_last_where==12 | `x'_`y'_last_where==13 | `x'_`y'_last_where==14 | `x'_`y'_last_where==21 | `x'_`y'_last_where==24 | `x'_`y'_last_where==25 | `x'_`y'_last_where==34)) 
		
		* Last mife/miso in a government or private facility   
		replace `x'_`y'_safety2v4=0 if (`x'_`y'_last==2 & (`x'_`y'_last_meds==11 | `x'_`y'_last_meds==12 | `x'_`y'_last_meds==13 | `x'_`y'_last_meds==14 | `x'_`y'_last_meds==21 | `x'_`y'_last_meds==24 | `x'_`y'_last_meds==25 | `x'_`y'_last_meds==34)) 
		
* Generate categorical abortion safety variable (safe, less safe, least safe)
gen `x'_`y'_safety3v4=2 if `x'_`y'_yn==1 | `x'_`y'_yn==2

	* Safe
		* Only surgery in a government or private facility  
		replace `x'_`y'_safety3v4=0 if `x'_`y'_only==1 & (`x'_`y'_where==11 | `x'_`y'_where==12 | `x'_`y'_where==13 | `x'_`y'_where==14 | `x'_`y'_where==21 | `x'_`y'_where==24 | `x'_`y'_where==25 | `x'_`y'_where==34) 

		* Only mife/miso from government or private facility 
		replace `x'_`y'_safety3v4=0 if `x'_`y'_only==2 & (`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34) 

		* Last surgery in a government or private facility   
		replace `x'_`y'_safety3v4=0 if (`x'_`y'_last==1 & (`x'_`y'_last_where==11 | `x'_`y'_last_where==12 | `x'_`y'_last_where==13 | `x'_`y'_last_where==14 | `x'_`y'_last_where==21 | `x'_`y'_last_where==24 | `x'_`y'_last_where==25 | `x'_`y'_last_where==34)) 
		
		* Last mife/miso in a government or private facility   
		replace `x'_`y'_safety3v4=0 if (`x'_`y'_last==2 & (`x'_`y'_last_meds==11 | `x'_`y'_last_meds==12 | `x'_`y'_last_meds==13 | `x'_`y'_last_meds==14 | `x'_`y'_last_meds==21 | `x'_`y'_last_meds==24 | `x'_`y'_last_meds==25 | `x'_`y'_last_meds==34)) 
		
	* Less safe
		* Only mife/miso from pharmacy/chemist/other
		replace `x'_`y'_safety3v4=1 if `x'_`y'_only==2 & (`x'_`y'_meds==15 | `x'_`y'_meds==22 | `x'_`y'_meds==23 | `x'_`y'_meds==26 | `x'_`y'_meds==31 | `x'_`y'_meds==32 | `x'_`y'_meds==33 | `x'_`y'_meds==35 | `x'_`y'_meds==96) 

		* Only "other pills/pill type unkown" from government or private facility
		replace `x'_`y'_safety3v4=1 if `x'_`y'_only==5 & (`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34) 

		* IF EXCLUDING "OTHER PILLS/PILL TYPE UNKNOWN" FROM PHARMACY/CHEMISTS FROM LESS SAFE, COMMENT OUT BELOW CODE
		* Only "other pills/pill type unkown" from pharmacy/chemist
		*replace `x'_`y'_safety3v3=1 if `x'_`y'_only==5 & (`x'_`y'_meds==22 | `x'_`y'_meds==23) 
			
		* Last mife/miso from a pharmacy/chemist/other
		replace `x'_`y'_safety3v4=1 if (`x'_`y'_last==2 & (`x'_`y'_last_meds==15 | `x'_`y'_last_meds==22 | `x'_`y'_last_meds==23 | `x'_`y'_last_meds==26 | `x'_`y'_last_meds==31 | `x'_`y'_last_meds==32 | `x'_`y'_last_meds==33 | `x'_`y'_last_meds==35 | `x'_`y'_last_meds==96))

		* Last "other pills/pill type unkown" from government or private facility
		replace `x'_`y'_safety3v4=1 if `x'_`y'_last==5 & (`x'_`y'_last_meds==11 | `x'_`y'_last_meds==12 | `x'_`y'_last_meds==13 | `x'_`y'_last_meds==14 | `x'_`y'_last_meds==21 | `x'_`y'_last_meds==24 | `x'_`y'_last_meds==25 | `x'_`y'_last_meds==34) 

		* Label safety variable response options

		* Generate safe (1)/unsafe (0) variables
		gen `x'_`y'_safe2v4=0 if `x'_`y'_safety3v4==0 | `x'_`y'_safety3v4==1
		replace `x'_`y'_safe2v4=1 if `x'_`y'_safety3v4==2

		* Label safety variable response options
		lab val `x'_`y'_safety2v4 safety2_list
		lab val `x'_`y'_safety3v4 safety3_list
		lab val `x'_`y'_safe2v4 safe2v3_list

	}
	}

* Generate safety categorization like v3 but only using the first method
foreach x in self friend1 friend2 {
foreach y in abt reg rec {
gen `x'_`y'_safety2v5=1 if `x'_`y'_yn==1 | `x'_`y'_yn==2

* Generate dichotmous abortion safety variable
	* Safe
		* Only surgery in a government or private facility  
		replace `x'_`y'_safety2v5=0 if `x'_`y'_only==1 & (`x'_`y'_where==11 | `x'_`y'_where==12 | `x'_`y'_where==13 | `x'_`y'_where==14 | `x'_`y'_where==21 | `x'_`y'_where==24 | `x'_`y'_where==25 | `x'_`y'_where==34) 

		* Only mife/miso from government or private facility 
		replace `x'_`y'_safety2v5=0 if `x'_`y'_only==2 & (`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34) 

		* First surgery in a government or private facility   
		replace `x'_`y'_safety2v5=0 if `x'_`y'_first==1 & (`x'_`y'_where==11 | `x'_`y'_where==12 | `x'_`y'_where==13 | `x'_`y'_where==14 | `x'_`y'_where==21 | `x'_`y'_where==24 | `x'_`y'_where==25 | `x'_`y'_where==34) 
		
		* First mife/miso in a government or private facility   
		replace `x'_`y'_safety2v5=0 if `x'_`y'_first==2 & (`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34) 
		
* Generate categorical abortion safety variable (safe, less safe, least safe)
gen `x'_`y'_safety3v5=2 if `x'_`y'_yn==1 | `x'_`y'_yn==2

	* Safe
		* Only surgery in a government or private facility  
		replace `x'_`y'_safety3v5=0 if `x'_`y'_only==1 & (`x'_`y'_where==11 | `x'_`y'_where==12 | `x'_`y'_where==13 | `x'_`y'_where==14 | `x'_`y'_where==21 | `x'_`y'_where==24 | `x'_`y'_where==25 | `x'_`y'_where==34) 

		* Only mife/miso from government or private facility 
		replace `x'_`y'_safety3v5=0 if `x'_`y'_only==2 & (`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34) 

		* First surgery in a government or private facility   
		replace `x'_`y'_safety3v5=0 if `x'_`y'_first==1 & (`x'_`y'_where==11 | `x'_`y'_where==12 | `x'_`y'_where==13 | `x'_`y'_where==14 | `x'_`y'_where==21 | `x'_`y'_where==24 | `x'_`y'_where==25 | `x'_`y'_where==34) 
		
		* First mife/miso in a government or private facility   
		replace `x'_`y'_safety3v5=0 if `x'_`y'_first==2 & (`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34) 
		
	* Less safe
		* Only mife/miso from pharmacy/chemist/other
		replace `x'_`y'_safety3v5=1 if `x'_`y'_only==2 & (`x'_`y'_meds==15 | `x'_`y'_meds==22 | `x'_`y'_meds==23 | `x'_`y'_meds==26 | `x'_`y'_meds==31 | `x'_`y'_meds==32 | `x'_`y'_meds==33 | `x'_`y'_meds==35 | `x'_`y'_meds==96) 

		* Only "other pills/pill type unkown" from government or private facility
		replace `x'_`y'_safety3v5=1 if `x'_`y'_only==5 & (`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34) 

		* IF EXCLUDING "OTHER PILLS/PILL TYPE UNKNOWN" FROM PHARMACY/CHEMISTS FROM LESS SAFE, COMMENT OUT BELOW CODE
		* Only "other pills/pill type unkown" from pharmacy/chemist
		*replace `x'_`y'_safety3v5=1 if `x'_`y'_only==5 & (`x'_`y'_meds==22 | `x'_`y'_meds==23) 
			
		* First mife/miso from a pharmacy/chemist/other
		replace `x'_`y'_safety3v5=1 if (`x'_`y'_first==2 & (`x'_`y'_meds==15 | `x'_`y'_meds==22 | `x'_`y'_meds==23 | `x'_`y'_meds==26 | `x'_`y'_meds==31 | `x'_`y'_meds==32 | `x'_`y'_meds==33 | `x'_`y'_meds==35 | `x'_`y'_meds==96))

		* Last "other pills/pill type unkown" from government or private facility
		replace `x'_`y'_safety3v5=1 if `x'_`y'_first==5 & (`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34) 

		* Label safety variable response options

		* Generate safe (1)/unsafe (0) variables
		gen `x'_`y'_safe2v5=0 if `x'_`y'_safety3v5==0 | `x'_`y'_safety3v5==1
		replace `x'_`y'_safe2v5=1 if `x'_`y'_safety3v5==2

		* Label safety variable response options
		lab val `x'_`y'_safety2v5 safety2_list
		lab val `x'_`y'_safety3v5 safety3_list
		lab val `x'_`y'_safe2v5 safe2v3_list

	}
	}

* Generate dichotomous potentially high risk abortion method variables
lab def high_risk_list 0 "Not potentially high risk" 1 "Potentially high risk"

foreach x in self friend1 {
foreach y in abt rec {

	* Using only last information
	gen `x'_`y'_risk_last=1 if `x'_`y'_ult_meth4!=.
	replace `x'_`y'_risk_last=0 if `x'_`y'_yn==1 & (`x'_`y'_ult_meth4==1 | `x'_`y'_ult_meth4==2) 
	lab val `x'_`y'_risk_last high_risk_list

	* Using only first information
	gen `x'_`y'_risk_first=1 if `x'_`y'_ult_meth4!=.
	replace `x'_`y'_risk_first=0 if `x'_`y'_yn==1 & (`x'_`y'_first_meth4==1 | `x'_`y'_first_meth4==2) 
	lab val `x'_`y'_risk_first high_risk_list
	
	* Using all source information
	gen `x'_`y'_risk_all=1 if `x'_`y'_ult_meth4!=.
	replace `x'_`y'_risk_all=0 if (`x'_`y'_only==1 | `x'_`y'_only==2)
	replace `x'_`y'_risk_all=0 if (`x'_`y'_first==1 | `x'_`y'_first==2) & (`x'_`y'_last==1 | `x'_`y'_last==2)
	lab val `x'_`y'_risk_all high_risk_list
}
}

* Generate dichotomous potentially trained provider variables
lab def untrained_list 0 "Likely trained provider" 1 "Likely not trained provider"

foreach x in self friend1 {
foreach y in abt rec {
/*
* Create macros for facility type/safety groups
local surg_fac_first "`x'_`y'_where==11 | `x'_`y'_where==12 | `x'_`y'_where==13 | `x'_`y'_where==14 | `x'_`y'_where==21 | `x'_`y'_where==24 | `x'_`y'_where==25 | `x'_`y'_where==34"
local surg_fac_last "`x'_`y'_last_where==11 | `x'_`y'_last_where==12 | `x'_`y'_last_where==13 | `x'_`y'_last_where==14 | `x'_`y'_last_where==21 | `x'_`y'_last_where==24 | `x'_`y'_last_where==25 | `x'_`y'_last_where==34"
local miso_fac_first "`x'_`y'_meds==11 | `x'_`y'_meds==12 | `x'_`y'_meds==13 | `x'_`y'_meds==14 | `x'_`y'_meds==21 | `x'_`y'_meds==24 | `x'_`y'_meds==25 | `x'_`y'_meds==34"
local miso_fac_last "`x'_`y'_last_meds==11 | `x'_`y'_last_meds==12 | `x'_`y'_last_meds==13 | `x'_`y'_last_meds==14 | `x'_`y'_last_meds==21 | `x'_`y'_last_meds==24 | `x'_`y'_last_meds==25 | `x'_`y'_last_meds==34"
local miso_pharm_first "`x'_`y'_meds==15 | `x'_`y'_meds==22 | `x'_`y'_meds==23 | `x'_`y'_meds==26 | `x'_`y'_meds==31 | `x'_`y'_meds==32 | `x'_`y'_meds==33 | `x'_`y'_meds==35 | `x'_`y'_meds==96"
local miso_pharm_last "`x'_`y'_last_meds==15 | `x'_`y'_last_meds==22 | `x'_`y'_last_meds==23 | `x'_`y'_last_meds==26 | `x'_`y'_last_meds==31 | `x'_`y'_last_meds==32 | `x'_`y'_last_meds==33 | `x'_`y'_last_meds==35 | `x'_`y'_last_meds==96"
*/
	* Using only last information
	gen `x'_`y'_untrained_last=1 if `x'_`y'_ult_source4!=.
	replace `x'_`y'_untrained_last=0 if `x'_`y'_ult_source4==1 | `x'_`y'_ult_source4==2
	lab val `x'_`y'_untrained_last untrained_list

	* Using only first information
	gen `x'_`y'_untrained_first=1 if `x'_`y'_ult_source4!=.
	replace `x'_`y'_untrained_first=0 if `x'_`y'_first_source4==1 | `x'_`y'_first_source4==2
	lab val `x'_`y'_untrained_first untrained_list
	
	* Using all source information
	gen `x'_`y'_untrained_all=0 if `x'_`y'_ult_source4!=.
	replace `x'_`y'_untrained_all=1 if `x'_`y'_untrained_last==1 | `x'_`y'_untrained_first==1
	lab val `x'_`y'_untrained_all untrained_list
}
}

* Generate four category risk/provider training variable
lab def risk_untrained_list 0 "Low risk, trained provider" 1 "Low risk, untrained provider" 2 "High risk, trained provider" 3 "High risk, untrained provider"

foreach x in self friend1 {
foreach var in abt rec {
gen `x'_`var'_risk_untrained=0 if `x'_`var'_risk_all==0 & `x'_`var'_untrained_all==0
replace `x'_`var'_risk_untrained=1 if `x'_`var'_risk_all==0 & `x'_`var'_untrained_all==1
replace `x'_`var'_risk_untrained=2 if `x'_`var'_risk_all==1 & `x'_`var'_untrained_all==0
replace `x'_`var'_risk_untrained=3 if `x'_`var'_risk_all==1 & `x'_`var'_untrained_all==1
lab val `x'_`var'_risk_untrained risk_untrained_list
}
}

* Generate four category risk/provider training variable but recode surgeries by untrained provider as high risk/untrained
foreach x in self friend1 {
foreach var in abt rec {
gen `x'_`var'_risk_untrainedv2=`x'_`var'_risk_untrained
replace `x'_`var'_risk_untrainedv2=3 if `x'_`var'_ult_meth4==1 & `x'_`var'_untrained_last==1
replace `x'_`var'_risk_untrainedv2=3 if `x'_`var'_risk_first==1 & `x'_`var'_untrained_first==1
lab val `x'_`var'_risk_untrainedv2 risk_untrained_list
}
}

* Generate dichotomous risk/provider training variables for most safe and most unsafe
foreach x in self friend1 {
foreach var in rec {

	* Most unsafe
	gen `x'_`var'_mostunsafe=0 if `x'_`var'_risk_untrained!=.
	replace `x'_`var'_mostunsafe=1 if `x'_`var'_risk_untrained==3

	* Most safe
	gen `x'_`var'_mostsafe=0 if `x'_`var'_risk_untrained!=.
	replace `x'_`var'_mostsafe=1 if `x'_`var'_risk_untrained==0
}
}

* Generate recommended methods variables using methods variables
lab def abt_recommended_list 0 "Used only recommended methods" 1 "Used not recommended methods"
lab def abt_safe_recommended_list 0 "Used only recommended" 1 "Used not recommended method first, ended with recommended" 2 "Used a not recommended method, ended with not recommended"

foreach x in self friend1 friend2 {
foreach y in abt reg rec {

gen `x'_`y'_recommended=1 if `x'_`y'_yn==1 | `x'_`y'_yn==2
replace `x'_`y'_recommended=0 if `x'_`y'_only==1 | `x'_`y'_only==2
replace `x'_`y'_recommended=0 if (`x'_`y'_first==1 | `x'_`y'_first==2) & (`x'_`y'_last==1 | `x'_`y'_last==2)
lab val `x'_`y'_recommended abt_recommended_list
lab var `x'_`y'_recommended "Whether `x' used recommended methods"

gen `x'_`y'_safe_recommend=2 if `x'_`y'_yn==1 | `x'_`y'_yn==2
replace `x'_`y'_safe_recommend=0 if `x'_`y'_only==1 | `x'_`y'_only==2
replace `x'_`y'_safe_recommend=0 if (`x'_`y'_first==1 | `x'_`y'_first==2) & (`x'_`y'_last==1 | `x'_`y'_last==2)
replace `x'_`y'_safe_recommend=1 if (`x'_`y'_first!=1 & `x'_`y'_first!=2) & (`x'_`y'_last==1 | `x'_`y'_last==2)
lab val `x'_`y'_safe_recommend abt_safe_recommended_list
lab var `x'_`y'_safe_recommend "Whether `x' used recommended only and/or ultimately methods"
}
}

* Generate variables for whether abortion/period regulation occurred in last 5 years
foreach var in self friend1 friend2 {
foreach y in abt reg {
gen `var'_`y'_5yr=1 if `var'_`y'_yearSIF>=2013 & `var'_`y'_yearSIF<=2018 
replace `var'_`y'_5yr=0 if `var'_`y'_yearSIF<2013 

gen `var'_`y'_3yr=1 if `var'_`y'_yearSIF>=2015 & `var'_`y'_yearSIF<=2018 
replace `var'_`y'_3yr=0 if `var'_`y'_yearSIF<2015 
}
}

foreach var in self friend1 friend2 {
gen `var'_rec_5yr=0 if `var'_abt_yn!=.
replace `var'_rec_5yr=1 if (`var'_abt_yearSIF>=2013 & `var'_abt_yearSIF<=2018) | (`var'_reg_yearSIF>=2013 & `var'_reg_yearSIF<=2018) 

gen `var'_rec_3yr=0 if `var'_abt_yn!=.
replace `var'_rec_3yr=1 if (`var'_abt_yearSIF>=2015 & `var'_abt_yearSIF<=2018) | (`var'_reg_yearSIF>=2015 & `var'_reg_yearSIF<=2018) 
} 

* Determine person-time for precise incidence calculations
	* Person-days since January 1, 2017
	gen person_days=(FQtodaySIF-1798848000000)/(24*60*60*1000)
	lab var person_days "Number of days from January 1, 2017 to interview date" 

	* Person-years since January 1, 2017
	gen person_years=person_days/365
	
	* Sum denominator for women who respond to direct abortion question
	egen self_abt_person_years=total(person_years) if self_abt_yn!=.
	egen self_abt_avgperson_years=mean(person_years) if self_abt_yn!=.

* Generate categorical friend count variable
recode friend_count (0=0 "0") (1=1 "1") (2/100=2 "2+") (-88 -99=.), gen(friend_count3)

* Generate dichotmous loner variable
gen loner=1 if friend_count==0
replace loner=0 if friend_count!=0 & friend_count!=-88 & friend_count!=-99 & friend_count!=.

* Generate dichotomous at least one friend variable
gen friend_count_1=0 if friend_count==0
replace friend_count_1=1 if friend_count>=1 & friend_count!=.

* Generate dichotomous at least two friend variable
gen friend_count_2=0 if friend_count==0 
replace friend_count_2=1 if friend_count>=2 & friend_count!=.

* Generate dichotomous abortion common variableol8bm
recode abt_common (3 4 -88 -99=0 "Not common") (1 2=1 "Common"), gen(abt_common2)
lab var abt_common2 "How common abortion is in community (dichomotous)"

* Generate count of how many methods of abortion respondent knows
gen abt_ways_count=abt_ways_surgery+abt_ways_pills_abortion+abt_ways_pills_fever+abt_ways_pills_ec+abt_ways_pills_oth+abt_ways_injection+abt_ways_herbs+abt_ways_alcohol+abt_ways_salt+abt_ways_lemon+abt_ways_cough+abt_ways_insert+abt_ways_other
lab var abt_ways_count "Total number of ways reported that women can remove a pregnancy in community"

	* Knows any method of abortion 
	gen abt_ways_1=0 if abt_ways_count==0
	replace abt_ways_1=1 if abt_ways_count>0 & abt_ways_count!=.
	lab var abt_ways_1 "Reported 1 or more ways that women can remove a pregnancy in community"
	
	* Knows two or more methods of abortion 
	gen abt_ways_2=0 if abt_ways_count==0 | abt_ways_count==1
	replace abt_ways_2=1 if abt_ways_count>1 & abt_ways_count!=.
	lab var abt_ways_2 "Reported 2 or more ways that women can remove a pregnancy in community"

* Generate numeric abt_way for women who reported only one method 
label define abortion_ways_list 1 surgery 2 pills_abortion 3 pills_fever 4 pills_ec ///
5 pills_oth 6 injection 7 herbs 8 alcohol 9 salt 10 lemon 11 cough 12 insert 13 other ///
-88 "-88" -99 "-99", replace

gen abt_ways_method1=abt_ways if abt_ways_count==1
encode abt_ways_method1, gen(abt_ways_method1v2) lab(abortion_ways_list)

label define abortion_ways_list 1 "Surgical procedure" 2 "Pills called mifepristone or misoprostol" ///
3 "Pills you take when you have a fever" 4 "Emergency contraception pills" 5 "Other pills" ///
6 "Injection" 7 "Traditional methods, like herbs" 8 "Alcohol" 9 "Salt, potash, maggi, or kanwa" ///
10 "Lemon or lime" 11 "Cough syrup" 12 "Insert materials into the vagina" 13 "Other", replace

* Replace abt_ways_main with abt_ways if reported only knowing one method of abortion
replace abt_ways_main=abt_ways_method1v2 if abt_ways_main==.

* Generate dichotomous variable for whether know likely safe method
gen abt_ways_safe=0 if abt_ways_count==0
replace abt_ways_safe=1 if abt_ways_surgery==1 | abt_ways_pills_abortion==1
lab var abt_ways_safe "Knows at least one safe abortion method in community (surgery or aboriton pills)"

* Generate combined overall telling confidantes variables (removal and regulation)
forval y=1/2 {
gen self_comb_tell_friend`y'=self_abt_tell_friend`y' if self_most_recent_all==1
replace self_comb_tell_friend`y'=self_reg_tell_friend`y' if self_most_recent_all==2 & self_comb_tell_friend`y'==.
}

* Generate combined agreement variables for abortion norms/stigma variables
foreach var in abt_health_risk abt_rape abt_afford abt_unwanted abt_shame abt_silence {
gen `var'_agree=0 if `var'!=.
replace `var'_agree=1 if `var'==1 | `var'==2 
}

* Create categorical variable for whether respondent reported own abortion and shared
lab def resp_comb_share_list 0 "No likely abortion reported" 1 "Likely abortion reported but not shared with friend" 2 "Likely abortion reported and shared with friend"
lab def resp_abt_share_list 0 "No pregnancy removal reported" 1 "Pregnancy removal reported but not shared with friend" 2 "Pregnancy removal reported and shared with friend"

gen resp_comb_share_f1=0 if self_combined==0
replace resp_comb_share_f1=1 if self_combined==1 & self_comb_tell_friend1==0
replace resp_comb_share_f1=2 if self_combined==1 & self_comb_tell_friend1==1
lab val resp_comb_share_f1 resp_comb_share_list

gen resp_comb_share_f2=0 if self_combined==0
replace resp_comb_share_f2=1 if self_combined==1 & self_comb_tell_friend2==0
replace resp_comb_share_f2=2 if self_combined==1 & self_comb_tell_friend2==1
lab val resp_comb_share_f2 resp_comb_share_list

* Create categorical variable for whether respondent reported own incidence abortion and shared
foreach var in comb abt {
foreach x in 1 2 {
gen resp_`var'_inc_share_f`x'=0 if self_`var'_inc_all==0
replace resp_`var'_inc_share_f`x'=1 if self_`var'_inc_all==1 & self_`var'_tell_friend`x'==0
replace resp_`var'_inc_share_f`x'=2 if self_`var'_inc_all==1 & self_`var'_tell_friend`x'==1
lab val resp_`var'_inc_share_f`x' resp_`var'_share_list
}
}

* Encode follow-up willingness variable
capture lab def yes_no_dnk_nr_list 0 "no" 1 "yes" -88 "-88" -99 "-99", replace
capture encode flw_willing, gen(flw_willingv2) lab(yes_no_dnk_nr_list)
replace flw_willing*=0 if flw_willing==-99

* Destring phone number variable
replace flw_number_confirm="" if flw_number_confirm=="."
destring flw_number_confirm, replace

* Create dichotomous variable for whether provided phone number
gen flw_num_provided=0 if flw_willing==1
replace flw_num_provided=1 if flw_number_confirm!=.

* Label yes/no variables
lab def yes_no_dnk_nr_list 0 "No" 1 "Yes" -88 "-88" -99 "-99", replace
lab val self_reg_yn yes_no_dnk_nr_list

* Drop variables
drop _merge

*******************************************************************************
* GENERATE FRIEND WEIGHTS
*******************************************************************************

* Survey set data
svyset ClusterID [pweight=FQweight], strata(strata) singleunit(scaled)

* Generate "self" variables for weight generation
gen self_age5=age5
gen self_school=school

foreach var in self friend1 friend2 {
gen `var'_age_edu=1 if `var'_age5==0 & `var'_school==1
replace `var'_age_edu=2 if `var'_age5==0 & `var'_school==2
replace `var'_age_edu=3 if `var'_age5==0 & `var'_school==3
replace `var'_age_edu=4 if `var'_age5==0 & `var'_school==4

replace `var'_age_edu=5 if `var'_age5==1 & `var'_school==1
replace `var'_age_edu=6 if `var'_age5==1 & `var'_school==2
replace `var'_age_edu=7 if `var'_age5==1 & `var'_school==3
replace `var'_age_edu=8 if `var'_age5==1 & `var'_school==4

replace `var'_age_edu=9 if `var'_age5==2 & `var'_school==1
replace `var'_age_edu=10 if `var'_age5==2 & `var'_school==2
replace `var'_age_edu=11 if `var'_age5==2 & `var'_school==3
replace `var'_age_edu=12 if `var'_age5==2 & `var'_school==4

replace `var'_age_edu=13 if `var'_age5==3 & `var'_school==1
replace `var'_age_edu=14 if `var'_age5==3 & `var'_school==2
replace `var'_age_edu=15 if `var'_age5==3 & `var'_school==3
replace `var'_age_edu=16 if `var'_age5==3 & `var'_school==4

replace `var'_age_edu=17 if `var'_age5==4 & `var'_school==1
replace `var'_age_edu=18 if `var'_age5==4 & `var'_school==2
replace `var'_age_edu=19 if `var'_age5==4 & `var'_school==3
replace `var'_age_edu=20 if `var'_age5==4 & `var'_school==4

replace `var'_age_edu=21 if `var'_age5==5 & `var'_school==1
replace `var'_age_edu=22 if `var'_age5==5 & `var'_school==2
replace `var'_age_edu=23 if `var'_age5==5 & `var'_school==3
replace `var'_age_edu=24 if `var'_age5==5 & `var'_school==4

replace `var'_age_edu=25 if `var'_age5==6 & `var'_school==1
replace `var'_age_edu=26 if `var'_age5==6 & `var'_school==2
replace `var'_age_edu=27 if `var'_age5==6 & `var'_school==3
replace `var'_age_edu=28 if `var'_age5==6 & `var'_school==4
}
/*
* Generate variables that combine age, education, and state categories
foreach var in self friend1 friend2 {

* Anambra
gen `var'_age_edu=1 if `var'_age5==0 & `var'_school==1 & state==1
replace `var'_age_edu=2 if `var'_age5==0 & `var'_school==2 & state==1
replace `var'_age_edu=3 if `var'_age5==0 & `var'_school==3 & state==1
replace `var'_age_edu=4 if `var'_age5==0 & `var'_school==4 & state==1

replace `var'_age_edu=5 if `var'_age5==1 & `var'_school==1 & state==1
replace `var'_age_edu=6 if `var'_age5==1 & `var'_school==2 & state==1
replace `var'_age_edu=7 if `var'_age5==1 & `var'_school==3 & state==1
replace `var'_age_edu=8 if `var'_age5==1 & `var'_school==4 & state==1

replace `var'_age_edu=9 if `var'_age5==2 & `var'_school==1 & state==1
replace `var'_age_edu=10 if `var'_age5==2 & `var'_school==2 & state==1
replace `var'_age_edu=11 if `var'_age5==2 & `var'_school==3 & state==1
replace `var'_age_edu=12 if `var'_age5==2 & `var'_school==4 & state==1

replace `var'_age_edu=13 if `var'_age5==3 & `var'_school==1 & state==1
replace `var'_age_edu=14 if `var'_age5==3 & `var'_school==2 & state==1
replace `var'_age_edu=15 if `var'_age5==3 & `var'_school==3 & state==1
replace `var'_age_edu=16 if `var'_age5==3 & `var'_school==4 & state==1

replace `var'_age_edu=17 if `var'_age5==4 & `var'_school==1 & state==1
replace `var'_age_edu=18 if `var'_age5==4 & `var'_school==2 & state==1
replace `var'_age_edu=19 if `var'_age5==4 & `var'_school==3 & state==1
replace `var'_age_edu=20 if `var'_age5==4 & `var'_school==4 & state==1

replace `var'_age_edu=21 if `var'_age5==5 & `var'_school==1 & state==1
replace `var'_age_edu=22 if `var'_age5==5 & `var'_school==2 & state==1
replace `var'_age_edu=23 if `var'_age5==5 & `var'_school==3 & state==1
replace `var'_age_edu=24 if `var'_age5==5 & `var'_school==4 & state==1

replace `var'_age_edu=25 if `var'_age5==6 & `var'_school==1 & state==1
replace `var'_age_edu=26 if `var'_age5==6 & `var'_school==2 & state==1
replace `var'_age_edu=27 if `var'_age5==6 & `var'_school==3 & state==1
replace `var'_age_edu=28 if `var'_age5==6 & `var'_school==4 & state==1

* Kaduna
replace `var'_age_edu=29 if `var'_age5==0 & `var'_school==1 & state==2
replace `var'_age_edu=30 if `var'_age5==0 & `var'_school==2 & state==2
replace `var'_age_edu=31 if `var'_age5==0 & `var'_school==3 & state==2
replace `var'_age_edu=32 if `var'_age5==0 & `var'_school==4 & state==2

replace `var'_age_edu=33 if `var'_age5==1 & `var'_school==1 & state==2
replace `var'_age_edu=34 if `var'_age5==1 & `var'_school==2 & state==2
replace `var'_age_edu=35 if `var'_age5==1 & `var'_school==3 & state==2
replace `var'_age_edu=36 if `var'_age5==1 & `var'_school==4 & state==2

replace `var'_age_edu=37 if `var'_age5==2 & `var'_school==1 & state==2
replace `var'_age_edu=38 if `var'_age5==2 & `var'_school==2 & state==2
replace `var'_age_edu=39 if `var'_age5==2 & `var'_school==3 & state==2
replace `var'_age_edu=40 if `var'_age5==2 & `var'_school==4 & state==2

replace `var'_age_edu=41 if `var'_age5==3 & `var'_school==1 & state==2
replace `var'_age_edu=42 if `var'_age5==3 & `var'_school==2 & state==2
replace `var'_age_edu=43 if `var'_age5==3 & `var'_school==3 & state==2
replace `var'_age_edu=44 if `var'_age5==3 & `var'_school==4 & state==2

replace `var'_age_edu=45 if `var'_age5==4 & `var'_school==1 & state==2
replace `var'_age_edu=46 if `var'_age5==4 & `var'_school==2 & state==2
replace `var'_age_edu=47 if `var'_age5==4 & `var'_school==3 & state==2
replace `var'_age_edu=48 if `var'_age5==4 & `var'_school==4 & state==2

replace `var'_age_edu=49 if `var'_age5==5 & `var'_school==1 & state==2
replace `var'_age_edu=50 if `var'_age5==5 & `var'_school==2 & state==2
replace `var'_age_edu=51 if `var'_age5==5 & `var'_school==3 & state==2
replace `var'_age_edu=52 if `var'_age5==5 & `var'_school==4 & state==2

replace `var'_age_edu=53 if `var'_age5==6 & `var'_school==1 & state==2
replace `var'_age_edu=54 if `var'_age5==6 & `var'_school==2 & state==2
replace `var'_age_edu=55 if `var'_age5==6 & `var'_school==3 & state==2
replace `var'_age_edu=56 if `var'_age5==6 & `var'_school==4 & state==2

* Kano
replace `var'_age_edu=57 if `var'_age5==0 & `var'_school==1 & state==3
replace `var'_age_edu=58 if `var'_age5==0 & `var'_school==2 & state==3
replace `var'_age_edu=59 if `var'_age5==0 & `var'_school==3 & state==3
replace `var'_age_edu=60 if `var'_age5==0 & `var'_school==4 & state==3

replace `var'_age_edu=61 if `var'_age5==1 & `var'_school==1 & state==3
replace `var'_age_edu=62 if `var'_age5==1 & `var'_school==2 & state==3
replace `var'_age_edu=63 if `var'_age5==1 & `var'_school==3 & state==3
replace `var'_age_edu=64 if `var'_age5==1 & `var'_school==4 & state==3

replace `var'_age_edu=65 if `var'_age5==2 & `var'_school==1 & state==3
replace `var'_age_edu=66 if `var'_age5==2 & `var'_school==2 & state==3
replace `var'_age_edu=67 if `var'_age5==2 & `var'_school==3 & state==3
replace `var'_age_edu=68 if `var'_age5==2 & `var'_school==4 & state==3

replace `var'_age_edu=69 if `var'_age5==3 & `var'_school==1 & state==3
replace `var'_age_edu=70 if `var'_age5==3 & `var'_school==2 & state==3
replace `var'_age_edu=71 if `var'_age5==3 & `var'_school==3 & state==3
replace `var'_age_edu=72 if `var'_age5==3 & `var'_school==4 & state==3

replace `var'_age_edu=73 if `var'_age5==4 & `var'_school==1 & state==3
replace `var'_age_edu=74 if `var'_age5==4 & `var'_school==2 & state==3
replace `var'_age_edu=75 if `var'_age5==4 & `var'_school==3 & state==3
replace `var'_age_edu=76 if `var'_age5==4 & `var'_school==4 & state==3

replace `var'_age_edu=77 if `var'_age5==5 & `var'_school==1 & state==3
replace `var'_age_edu=78 if `var'_age5==5 & `var'_school==2 & state==3
replace `var'_age_edu=79 if `var'_age5==5 & `var'_school==3 & state==3
replace `var'_age_edu=80 if `var'_age5==5 & `var'_school==4 & state==3

replace `var'_age_edu=81 if `var'_age5==6 & `var'_school==1 & state==3
replace `var'_age_edu=82 if `var'_age5==6 & `var'_school==2 & state==3
replace `var'_age_edu=83 if `var'_age5==6 & `var'_school==3 & state==3
replace `var'_age_edu=84 if `var'_age5==6 & `var'_school==4 & state==3

* Lagos
replace `var'_age_edu=85 if `var'_age5==0 & `var'_school==1 & state==4
replace `var'_age_edu=86 if `var'_age5==0 & `var'_school==2 & state==4
replace `var'_age_edu=87 if `var'_age5==0 & `var'_school==3 & state==4
replace `var'_age_edu=88 if `var'_age5==0 & `var'_school==4 & state==4

replace `var'_age_edu=89 if `var'_age5==1 & `var'_school==1 & state==4
replace `var'_age_edu=90 if `var'_age5==1 & `var'_school==2 & state==4
replace `var'_age_edu=91 if `var'_age5==1 & `var'_school==3 & state==4
replace `var'_age_edu=92 if `var'_age5==1 & `var'_school==4 & state==4

replace `var'_age_edu=93 if `var'_age5==2 & `var'_school==1 & state==4
replace `var'_age_edu=94 if `var'_age5==2 & `var'_school==2 & state==4
replace `var'_age_edu=95 if `var'_age5==2 & `var'_school==3 & state==4
replace `var'_age_edu=96 if `var'_age5==2 & `var'_school==4 & state==4

replace `var'_age_edu=97 if `var'_age5==3 & `var'_school==1 & state==4
replace `var'_age_edu=98 if `var'_age5==3 & `var'_school==2 & state==4
replace `var'_age_edu=99 if `var'_age5==3 & `var'_school==3 & state==4
replace `var'_age_edu=100 if `var'_age5==3 & `var'_school==4 & state==4

replace `var'_age_edu=101 if `var'_age5==4 & `var'_school==1 & state==4
replace `var'_age_edu=102 if `var'_age5==4 & `var'_school==2 & state==4
replace `var'_age_edu=103 if `var'_age5==4 & `var'_school==3 & state==4
replace `var'_age_edu=104 if `var'_age5==4 & `var'_school==4 & state==4

replace `var'_age_edu=105 if `var'_age5==5 & `var'_school==1 & state==4
replace `var'_age_edu=106 if `var'_age5==5 & `var'_school==2 & state==4
replace `var'_age_edu=107 if `var'_age5==5 & `var'_school==3 & state==4
replace `var'_age_edu=108 if `var'_age5==5 & `var'_school==4 & state==4

replace `var'_age_edu=109 if `var'_age5==6 & `var'_school==1 & state==4
replace `var'_age_edu=110 if `var'_age5==6 & `var'_school==2 & state==4
replace `var'_age_edu=111 if `var'_age5==6 & `var'_school==3 & state==4
replace `var'_age_edu=112 if `var'_age5==6 & `var'_school==4 & state==4

* Nasarawa
replace `var'_age_edu=113 if `var'_age5==0 & `var'_school==1 & state==5
replace `var'_age_edu=114 if `var'_age5==0 & `var'_school==2 & state==5
replace `var'_age_edu=115 if `var'_age5==0 & `var'_school==3 & state==5
replace `var'_age_edu=116 if `var'_age5==0 & `var'_school==4 & state==5

replace `var'_age_edu=117 if `var'_age5==1 & `var'_school==1 & state==5
replace `var'_age_edu=118 if `var'_age5==1 & `var'_school==2 & state==5
replace `var'_age_edu=119 if `var'_age5==1 & `var'_school==3 & state==5
replace `var'_age_edu=120 if `var'_age5==1 & `var'_school==4 & state==5

replace `var'_age_edu=121 if `var'_age5==2 & `var'_school==1 & state==5
replace `var'_age_edu=122 if `var'_age5==2 & `var'_school==2 & state==5
replace `var'_age_edu=123 if `var'_age5==2 & `var'_school==3 & state==5
replace `var'_age_edu=124 if `var'_age5==2 & `var'_school==4 & state==5

replace `var'_age_edu=125 if `var'_age5==3 & `var'_school==1 & state==5
replace `var'_age_edu=126 if `var'_age5==3 & `var'_school==2 & state==5
replace `var'_age_edu=127 if `var'_age5==3 & `var'_school==3 & state==5
replace `var'_age_edu=128 if `var'_age5==3 & `var'_school==4 & state==5

replace `var'_age_edu=129 if `var'_age5==4 & `var'_school==1 & state==5
replace `var'_age_edu=130 if `var'_age5==4 & `var'_school==2 & state==5
replace `var'_age_edu=131 if `var'_age5==4 & `var'_school==3 & state==5
replace `var'_age_edu=132 if `var'_age5==4 & `var'_school==4 & state==5

replace `var'_age_edu=133 if `var'_age5==5 & `var'_school==1 & state==5
replace `var'_age_edu=134 if `var'_age5==5 & `var'_school==2 & state==5
replace `var'_age_edu=135 if `var'_age5==5 & `var'_school==3 & state==5
replace `var'_age_edu=136 if `var'_age5==5 & `var'_school==4 & state==5

replace `var'_age_edu=137 if `var'_age5==6 & `var'_school==1 & state==5
replace `var'_age_edu=138 if `var'_age5==6 & `var'_school==2 & state==5
replace `var'_age_edu=139 if `var'_age5==6 & `var'_school==3 & state==5
replace `var'_age_edu=140 if `var'_age5==6 & `var'_school==4 & state==5

* Rivers
replace `var'_age_edu=141 if `var'_age5==0 & `var'_school==1 & state==6
replace `var'_age_edu=142 if `var'_age5==0 & `var'_school==2 & state==6
replace `var'_age_edu=143 if `var'_age5==0 & `var'_school==3 & state==6
replace `var'_age_edu=144 if `var'_age5==0 & `var'_school==4 & state==6

replace `var'_age_edu=145 if `var'_age5==1 & `var'_school==1 & state==6
replace `var'_age_edu=146 if `var'_age5==1 & `var'_school==2 & state==6
replace `var'_age_edu=147 if `var'_age5==1 & `var'_school==3 & state==6
replace `var'_age_edu=148 if `var'_age5==1 & `var'_school==4 & state==6

replace `var'_age_edu=149 if `var'_age5==2 & `var'_school==1 & state==6
replace `var'_age_edu=150 if `var'_age5==2 & `var'_school==2 & state==6
replace `var'_age_edu=151 if `var'_age5==2 & `var'_school==3 & state==6
replace `var'_age_edu=152 if `var'_age5==2 & `var'_school==4 & state==6

replace `var'_age_edu=153 if `var'_age5==3 & `var'_school==1 & state==6
replace `var'_age_edu=154 if `var'_age5==3 & `var'_school==2 & state==6
replace `var'_age_edu=155 if `var'_age5==3 & `var'_school==3 & state==6
replace `var'_age_edu=156 if `var'_age5==3 & `var'_school==4 & state==6

replace `var'_age_edu=157 if `var'_age5==4 & `var'_school==1 & state==6
replace `var'_age_edu=158 if `var'_age5==4 & `var'_school==2 & state==6
replace `var'_age_edu=159 if `var'_age5==4 & `var'_school==3 & state==6
replace `var'_age_edu=160 if `var'_age5==4 & `var'_school==4 & state==6

replace `var'_age_edu=161 if `var'_age5==5 & `var'_school==1 & state==6
replace `var'_age_edu=162 if `var'_age5==5 & `var'_school==2 & state==6
replace `var'_age_edu=163 if `var'_age5==5 & `var'_school==3 & state==6
replace `var'_age_edu=164 if `var'_age5==5 & `var'_school==4 & state==6

replace `var'_age_edu=165 if `var'_age5==6 & `var'_school==1 & state==6
replace `var'_age_edu=166 if `var'_age5==6 & `var'_school==2 & state==6
replace `var'_age_edu=167 if `var'_age5==6 & `var'_school==3 & state==6
replace `var'_age_edu=168 if `var'_age5==6 & `var'_school==4 & state==6

* Taraba
replace `var'_age_edu=169 if `var'_age5==0 & `var'_school==1 & state==7
replace `var'_age_edu=170 if `var'_age5==0 & `var'_school==2 & state==7
replace `var'_age_edu=171 if `var'_age5==0 & `var'_school==3 & state==7
replace `var'_age_edu=172 if `var'_age5==0 & `var'_school==4 & state==7

replace `var'_age_edu=173 if `var'_age5==1 & `var'_school==1 & state==7
replace `var'_age_edu=174 if `var'_age5==1 & `var'_school==2 & state==7
replace `var'_age_edu=175 if `var'_age5==1 & `var'_school==3 & state==7
replace `var'_age_edu=176 if `var'_age5==1 & `var'_school==4 & state==7

replace `var'_age_edu=177 if `var'_age5==2 & `var'_school==1 & state==7
replace `var'_age_edu=178 if `var'_age5==2 & `var'_school==2 & state==7
replace `var'_age_edu=179 if `var'_age5==2 & `var'_school==3 & state==7
replace `var'_age_edu=180 if `var'_age5==2 & `var'_school==4 & state==7

replace `var'_age_edu=181 if `var'_age5==3 & `var'_school==1 & state==7
replace `var'_age_edu=182 if `var'_age5==3 & `var'_school==2 & state==7
replace `var'_age_edu=183 if `var'_age5==3 & `var'_school==3 & state==7
replace `var'_age_edu=184 if `var'_age5==3 & `var'_school==4 & state==7

replace `var'_age_edu=185 if `var'_age5==4 & `var'_school==1 & state==7
replace `var'_age_edu=186 if `var'_age5==4 & `var'_school==2 & state==7
replace `var'_age_edu=187 if `var'_age5==4 & `var'_school==3 & state==7
replace `var'_age_edu=188 if `var'_age5==4 & `var'_school==4 & state==7

replace `var'_age_edu=189 if `var'_age5==5 & `var'_school==1 & state==7
replace `var'_age_edu=190 if `var'_age5==5 & `var'_school==2 & state==7
replace `var'_age_edu=191 if `var'_age5==5 & `var'_school==3 & state==7
replace `var'_age_edu=192 if `var'_age5==5 & `var'_school==4 & state==7

replace `var'_age_edu=193 if `var'_age5==6 & `var'_school==1 & state==7
replace `var'_age_edu=194 if `var'_age5==6 & `var'_school==2 & state==7
replace `var'_age_edu=195 if `var'_age5==6 & `var'_school==3 & state==7
replace `var'_age_edu=196 if `var'_age5==6 & `var'_school==4 & state==7
}
*/

* Create weights for confidante age/education to match respondent
svy: tab self_age_edu
matrix resp=e(b)'
svmat resp

forval f=1/2 {
svy: tab friend`f'_age_edu
matrix f`f'=e(b)'
svmat f`f'

gen f`f'_weight=resp1/f`f'1
mkmat f`f'_weight if f`f'_weight!=.
gen friend`f'_weight=.
forval y=1/28 {
replace friend`f'_weight=f`f'_weight[`y',1] if friend`f'_age_edu==`y'
}
gen friend`f'_weight_comb=friend`f'_weight*FQweight
}

* Recode friend age variables to be 1-7 instead of 0-6
foreach x in 1 2 {
recode friend`x'_age5 0=1 1=2 2=3 3=4 4=5 5=6 6=7, gen(friend`x'_age5v2)
}

* Create weights for confidante age to match respondent
svy: tab age5
matrix resp_age=e(b)'
svmat resp_age

forval f=1/2 {
svy: tab friend`f'_age5v2
matrix f`f'_age=e(b)'
svmat f`f'_age

gen f`f'_age_rt=resp_age1/f`f'_age1
mkmat f`f'_age_rt if f`f'_age_rt!=.
gen f`f'_age_wt=.
forval y=1/7 {
replace f`f'_age_wt=f`f'_age_rt[`y',1] if friend`f'_age5v2==`y'
}
gen f`f'_age_wt_comb=f`f'_age_wt*FQweight
}

* Create weights for confidante school to match respondent
svy: tab school
matrix resp_school=e(b)'
svmat resp_school

forval f=1/2 {
svy: tab friend`f'_school
matrix f`f'_school=e(b)'
svmat f`f'_school

gen f`f'_school_rt=resp_school1/f`f'_school1
mkmat f`f'_school_rt if f`f'_school_rt!=.
gen f`f'_school_wt=.
forval y=1/4 {
replace f`f'_school_wt=f`f'_school_rt[`y',1] if friend`f'_school==`y'
}
gen f`f'_school_wt_comb=f`f'_school_wt*FQweight
}

* Generate combined age/education confidante weights
forval f=1/2 {
gen f`f'_as_wt_comb=f`f'_age_wt*f`f'_school_wt*FQweight
}


*******************************************************************************
* SAVE ABORTION DATA
*******************************************************************************

save "$datadir/`CCRX'-FQ-AbortionData.dta", replace
