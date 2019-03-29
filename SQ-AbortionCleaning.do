clear
clear matrix
clear mata
capture log close
set maxvar 15000
set more off
numlabel, add

*******************************************************************************
*
*  FILENAME:	NGR5-SQ-AbortionEstimates-v#-$date.do
*  PURPOSE:		Clean SDP abortion data
*  CREATED:		2018-06-13 by Suzanne Bell
*  DATA IN:		NGR5_SDP_2Page_Analysis_$date.dta
*  DATA OUT:	NGR5_SDP_AbortionEstimates.dta
*  UPDATES:		2018-09-17 by Suzanne (v3)
*					Re-classified one private tertiary facility to secondary
*					Separate primary and secondary hospitals, updated
*					post_abt_daysv2 calculation you denominator all advanced,
*					and generated combined PAC/abortion methods offered and 
*					distribution variables
*
*******************************************************************************

*******************************************************************************
* SET MACROS
*******************************************************************************

* Set own directory 
global datadir "~/Dropbox (Gates Institute)/Abortion-Nigeria/Data/SDP"
global resultsdir "~/Dropbox (Gates Institute)/Abortion-Nigeria/Results"
cd "$datadir"

* Set macro for date
local today=c(current_date)
local c_today= "`today'"
global date=subinstr("`c_today'", " ", "",.)

* Create log
log using "NGR5-SDP-AbortionCleaning.log", replace

*******************************************************************************
* READ IN AND PREPARE DATA
*******************************************************************************

* Read in re-classification of facilities
insheet using "$datadir/NGR5-SDP-Classification_by_Metainstance_v2_28_06_18.csv", names

replace classification="Primary" if classification=="primary"
replace classification="Secondary" if classification=="secondary"

rename metainstanceid metainstanceID
/*
* Recategorize incorrect facilities
replace classification="Tertiary" if metainstanceID=="uuid:ad3465a5-5020-4d11-aeba-673c926b78a5"
replace classification="Tertiary" if metainstanceID=="uuid:31b62083-2a3a-40fb-8dcf-c1e26d4dc6bb"
replace classification="Tertiary" if metainstanceID=="uuid:2156e856-fab1-456e-9a55-7b58bceed03b"
replace classification="Tertiary" if metainstanceID=="uuid:998b1833-9999-4458-a76e-b5b488077fb8"
replace classification="Tertiary" if metainstanceID=="uuid:2156e856-fab1-456e-9a55-7b58bceed03b"
*/
keep classification metainstanceID

merge 1:1 metainstanceID using "$datadir/NGR5_SDP_2Page_Analysis_30Aug2018.dta"

* Read in SDP data
*use "$datadir/NGR5_SDP_2Page_Analysis_13Jul2018.dta", clear

*******************************************************************************
* RECODE FACILITY TYPE AND CLASSIFICATION
*******************************************************************************

* Encode classification variable
lab def classification_list 1 "Primary" 2 "Secondary" 3 "Tertiary"
encode classification, gen(classificationv2) lab(classification_list)
drop classification
rename classificationv2 classification
order classification, before(facility_type)

*replace classification=3 if metainstanceID=="uuid:3551957c-24be-450e-b6c9-04c0b97466ee"
*replace classification=3 if metainstanceID=="uuid:b3a36a99-7bd1-4501-98af-6f9fc8e7877a"

* Recode incorrect classification type
replace classification=1 if metainstanceID=="uuid:86e7b4a9-bf80-4630-81d4-b12c28b75bb6"
replace classification=2 if metainstanceID=="uuid:8f4dc79b-f07a-412c-9cf8-952ec2de84e0"
replace classification=1 if metainstanceID=="uuid:9d866e2d-269a-4ac4-b31d-b1306a506302"
replace classification=1 if metainstanceID=="uuid:c1a8ad3c-cce5-4e8d-936e-6dc062bebf44"
replace classification=1 if metainstanceID=="uuid:6d4879b7-6e26-4dc8-9e22-a4540dd57889"
replace classification=1 if metainstanceID=="uuid:85daf284-9ad8-40cc-b96f-b2d4567a2160"
replace classification=1 if metainstanceID=="uuid:b3adba80-6cfe-4661-ae19-1ae67ffa69c1"
replace classification=1 if metainstanceID=="uuid:af63a057-7f3d-4d56-930e-8f8b3c74c8a9"
replace classification=1 if metainstanceID=="uuid:d6e67512-167b-4f92-b837-5f5b6a7e8b4a"
replace classification=1 if metainstanceID=="uuid:c1a8ad3c-cce5-4e8d-936e-6dc062bebf44"
replace classification=1 if metainstanceID=="uuid:453d60ef-2d01-4fb7-84bb-0e461d95f175"
replace classification=1 if metainstanceID=="uuid:41ce4e03-b337-45e0-bc5b-f21239992203"

replace classification=2 if metainstanceID=="uuid:45d5b540-dd44-4be8-9480-5992596830c4"
replace classification=2 if metainstanceID=="uuid:4ccd2135-f5db-4b3b-acb7-40001a02f757"
replace classification=2 if metainstanceID=="uuid:d2c87e15-3850-4471-b602-61b74cc9f2cd"
*replace classification=2 if metainstanceID=="uuid:aee0f488-33e7-43c0-9a52-95484e148850"

* Create new facility_type variable that distinguishes hospital type
drop facility_type_v2
recode facility_type (0=1 "Tertiary/Teaching Hospital") (1=3 "Primary Hospital") (1=2 "Secondary Hospital") (2=4 "Health Center") (3=5 "Maternity Clinic") (9=6 "Health Post")	////
(7 8=.), gen(facility_typev2)
replace facility_typev2=1 if classification==3 & facility_type==1
replace facility_typev2=2 if classification==2 & facility_type==1

replace classification=1 if classification==. & facility_typev2!=.

* Replace advanced equal to missing if no facility_typev2 categorization (excludes 3 chemists)
replace advanced=0 if facility_typev2==. & advanced!=.

* Generate new facility type variable that establishes the 5 categories by public/private
gen facility_typev3=1 if classification==3 & sector==0
replace facility_typev3=2 if classification==2 & sector==0
replace facility_typev3=3 if classification==1 & sector==0
replace facility_typev3=4 if classification==2 & sector==1
replace facility_typev3=4 if classification==3 & sector==1
replace facility_typev3=5 if classification==1 & sector==1
lab def facility_typev3_list 1 "Public Tertiary" 2 "Public Secondary" 3 "Public Primary" 4 "Private Tertiary/Secondary" 5 "Private Primary"
lab val facility_typev3 facility_typev3_list

*******************************************************************************
* GENERATE AND RENAME/RECODE VARIABLES
*******************************************************************************

* Label variables
lab var staff_24hr_yn "Whether have staff 24 hours, in person or on call"

* Encode variables
capture lab def yes_no_dnk_nr_list 0 "no" 1 "yes" -88 "-88" -99 "-99", replace
encode staff_24hr_yn, gen(staff_24hr_ynv2) lab(yes_no_dnk_nr_list)

* Encode level1 variable
lab def level1_list 1 "anambra" 2 "kaduna" 3 "kano" 4 "lagos" 5 "nasarawa" 6 "rivers" 7 "taraba"
encode level1, gen(state) lab(level1_list)

* Generate 12+ week provision variables with denominnator as all advanced facilities
foreach var in post_abt_12w_yn provide_abt_12w_yn post_abt_trained_all abt_trained {
gen `var'_all=0 if advanced==1
replace `var'_all=1 if `var'==1
}

* Generate dichotomous trained providers variables
gen post_abt_trained2=0 if post_abt_0w_12w_yn==1
replace post_abt_trained2=1 if post_abt_trained>0 & post_abt_trained!=.

gen abt_trained2=0 if provide_abt_0w_12w_yn==1
replace abt_trained2=1 if abt_trained>0 & abt_trained!=.

* Replace -88/-99 with missing
foreach var in post_abt_trained post_abt_days abt_trained abt_days post_abt_0w_12w_yn	///
post_abt_12w_yn provide_abt_0w_12w_yn provide_abt_12w_yn {
replace `var'=. if `var'==-88 | `var'==-99
}

* Combine select multiple PAC/abortion methods variable versions
foreach var in post_abt_care_0w_12w post_abt_care_12w {
gen `var'=`var'_1
replace `var'=`var'_2 if `var'==""
replace `var'=`var'_3 if `var'==""
}
lab var post_abt_care_0w_12w "Uterine evacuation methods offered for pregnancies 0-12 weeks"
lab var post_abt_care_12w "Uterine evacuation methods offered for pregnancies 12+ weeks"

* Combine dichotomous PAC/abortion methods variable versions
drop pac_method_lap_0w_12w pac_method_lap_12w pacabt_method_lap_0w_12w pacabt_method_lap_12w abt_method_lap_0w_12w abt_method_lap_12w
gen method_lap_0w_12w=regexm(post_abt_care_0w_12w, ["laparotomy"])
gen method_lap_12w=regexm(post_abt_care_12w, ["laparotomy"])

foreach x in 0w_12w 12w {
foreach y in miso miso_mife mva eva de dc {
gen method_`y'_`x'=pac_method_`y'_`x'
replace method_`y'_`x'=pacabt_method_`y'_`x' if method_`y'_`x'==.
replace method_`y'_`x'=abt_method_`y'_`x' if method_`y'_`x'==.
}
}
lab var method_miso_0w_12w "Provide misoprostol for PAC and/or abortion at <=12 weeks"
lab var method_miso_mife_0w_12w "Provide mife/miso for PAC and/or abortion at <=12 weeks"
lab var method_mva_0w_12w "Provide MVA for PAC and/or abortion at <=12 weeks"
lab var method_eva_0w_12w "Provide EVA for PAC and/or abortion at <=12 weeks"
lab var method_de_0w_12w "Provide MVA for D&E and/or abortion at <=12 weeks"
lab var method_dc_0w_12w "Provide EVA for D&C and/or abortion at <=12 weeks"
lab var method_lap_0w_12w "Provide laparotomy for PAC and/or abortion at <=12 weeks"

lab var method_miso_12w "Provide misoprostol for PAC and/or abortion at >12 weeks"
lab var method_miso_mife_12w "Provide mife/miso for PAC and/or abortion at >12 weeks"
lab var method_mva_12w "Provide MVA for PAC and/or abortion at >12 weeks"
lab var method_eva_12w "Provide EVA for PAC and/or abortion at >12 weeks"
lab var method_de_12w "Provide MVA for D&E and/or abortion at >12 weeks"
lab var method_dc_12w "Provide EVA for D&C and/or abortion at >12 weeks"
lab var method_lap_12w "Provide laparotomy for PAC and/or abortion at >12 weeks"
	
* Create variables for distribution of PAC methods used that account for facilities that don't offer
foreach var in miso miso_mife mva eva de dc laparotomy dnk {
gen trt_`var'_all=0 if post_abt_0w_12w_yn==1
replace trt_`var'_all=trt_`var' if trt_`var'!=. 
}
replace trt_dnk_all=0 if trt_dnk_all==-88

foreach var in miso miso_mife mva eva de dc dnk {
gen ind_`var'_all=0 if provide_abt_0w_12w_yn==1
replace ind_`var'_all=ind_`var' if ind_`var'!=.
}

* Generate total number of patients treated accounted for in distribution responses (should be 10)
egen trt_total=sum(trt_miso_all+trt_miso_mife_all+trt_mva_all+trt_eva_all+trt_de_all+trt_dc_all+trt_laparotomy_all+trt_dnk_all), by(metainstanceID)

gen ind_total=sum(ind_miso_all+ind_miso_mife_all+ind_mva_all+ind_eva_all+ind_de_all+ind_dc_all+ind_dnk_all)

* Generate sum of patient distributions across all facilties by method
foreach var in miso miso_mife mva eva de dc laparotomy dnk {
egen trt_`var'_total=sum(trt_`var'_all) if trt_total==10
}
egen trt_total_total=sum(trt_total) if trt_total==10

foreach var in miso miso_mife mva eva de dc dnk {
egen ind_`var'_total=sum(ind_`var'_all) if ind_total==10
}
egen ind_total_total=sum(ind_total) if ind_total==10
	
* Replace 24 hour staff string variable
order staff_24hr_ynv2, after(staff_24hr_yn)
drop staff_24hr_yn
rename staff_24hr_ynv2 staff_24hr_yn

* Label yes/no variables
lab def yes_no_dnk_nr_list 0 "No" 1 "Yes" -88 "-88" -99 "-99", replace
lab val staff_24hr_yn yes_no_dnk_nr_list

* Generate dichotomous 24/7 PAC services variable
gen post_abt_daysv2=0 if advanced==1
replace post_abt_daysv2=1 if post_abt_days==8

* Generate dichotomous LARC provision variable
gen larc=0 if SDP_result==1
replace larc=1 if provided_implants==1 | provided_iud==1

* Generate variable for whether has all basic safe abortion care (SAC) components (minus induced abortion)
gen basic_wo_abt=0 if advanced==1
replace basic_wo_abt=1 if post_abt_0w_12w_yn==1 & abt_services_antibiotics==1 & abt_services_oxytocics==1 & abt_services_iv_fluids==1 & fp_offered==1

* Generate variable for whether has all basic safe abortion care (SAC) components (including induced abortion)
gen basic_w_abt=0 if advanced==1
replace basic_w_abt=1 if provide_abt_0w_12w_yn==1 & post_abt_0w_12w_yn==1 & abt_services_antibiotics==1 & abt_services_oxytocics==1 & abt_services_iv_fluids==1 & fp_offered==1

* Generate variable for whether has all comprehensive safe abortion care (SAC) components (minus induced abortion)
gen comp_wo_abt=0 if advanced==1
replace comp_wo_abt=1 if basic_w_abt==1 & post_abt_12w_yn_all==1 & abt_services_transfusion==1 & abt_services_laparotomy==1 & post_abt_daysv2==1 & larc==1

* Generate variable for whether has all comprehensive safe abortion care (SAC) components (including induced abortion)
gen comp_w_abt=0 if advanced==1
replace comp_w_abt=1 if basic_w_abt==1 & provide_abt_12w_yn_all==1 & post_abt_12w_yn_all==1 & abt_services_transfusion==1 & abt_services_laparotomy==1 & post_abt_daysv2==1 & larc==1

* Generate dichotomous variable for whether provided any PAC in last completed month
gen any_pac_last=0 if advanced==1 & facility_type!=.
replace any_pac_last=1 if patient_last_tot>0 & patient_last_tot!=.

* Generate dichotomous variable for whether provided any safe abortion in last completed month
gen any_abt_last=0 if advanced==1 & facility_type!=.
replace any_abt_last=1 if abt_last_month>0 & abt_last_month!=.

* Generate dichotomous variable for whether provided any PAC or safe abortion in last completed month
gen any_pac_abt_last=0 if advanced==1
replace any_pac_abt_last=1 if any_abt_last==1 | any_abt_last==1

*******************************************************************************
* RECODING BASED ON SUSPECT OR INCOMPLETE DATA
*******************************************************************************

* DO THESE OR NOT?

* Recode facility as not providing abortion if methods used reported as "-99" and avg and last month patients are 0
*replace provide_abt_0w_12w_yn=0 if metainstanceID=="uuid:9d6f3d70-62fd-481f-8be6-2054f819ebd0"
*replace provide_abt_0w_12w_yn=0 if metainstanceID=="uuid:85321881-1e77-4ab4-8ed4-87675b6ab29c"

* Recode facility as not providing PAC if methods used reported as "-99" and inpatient/outpatient answer "-88" or "-99"
	* Or impute to caseload of 0 or average caseload for facility type/state?
replace post_abt_0w_12w_yn=0 if metainstanceID=="uuid:150f281b-a0db-4ba2-9bb2-03eb59c35698"
replace post_abt_0w_12w_yn=0 if metainstanceID=="uuid:16e21389-56e6-4d26-9d70-d0ace37eada0"

* Inpatient outpatient=-99 so no caseloads but did indicate method offered (MVA)
	* Code as if don't provide? Or 0 caseload in last month? Or average caseload for facility type/state?
	*replace post_abt_0w_12w_yn=0 if metainstanceID=="uuid:457b7a51-0b2e-486c-a540-4fc967fe20fa"

*** How to handle -88 or -99 answers for caseload questions for last completed month or average month?

* Replace -88/-99 caseload response with corresponding average or last month if available
	* Outpatient 
		* Average month
		replace outpatient_avg=outpatient_last_month if (outpatient_avg==-88 | outpatient_avg==-99) & outpatient_last_month!=. & outpatient_last_month!=-88 & outpatient_last_month!=-99

		* Last month
		replace outpatient_last_month=outpatient_avg if (outpatient_last_month==-88 | outpatient_last_month==-99) & outpatient_avg!=. & outpatient_avg!=-88 & outpatient_avg!=-99

	* Inpatient
		* Average month
		replace inpatient_avg=inpatient_last_month if (inpatient_avg==-88 | inpatient_avg==-99) & inpatient_last_month!=. & inpatient_last_month!=-88 & inpatient_last_month!=-99

		* Last month
		replace inpatient_last_month=inpatient_avg if (inpatient_last_month==-88 | inpatient_last_month==-99) & inpatient_avg!=. & inpatient_avg!=-88 & inpatient_avg!=-99

	* Abortion
		* Average month
		replace abt_avg=abt_last_month if (abt_avg==-88 | abt_avg==-99) & abt_last_month!=. & abt_last_month!=-88 & abt_last_month!=-99

		* Last month
		replace abt_last_month=abt_avg if (abt_last_month==-88 | abt_last_month==-99) & abt_avg!=. & abt_avg!=-88 & abt_avg!=-99

	* Remaining -88/-99 caseload responses - how to handle? Recode as 0, recode as average for facility type/state, or indicate don't provide?
		* PAC outpatient: 7
		* PAC inpatient: 2
		* Abortion: 8
		* For now, recoding as "." to calculate averages
		foreach var in outpatient_avg outpatient_last_month inpatient_avg inpatient_last_month abt_avg_month abt_last_month {
		replace `var'=. if 	`var'==-88 | `var'==-99
		}
		
* Average the average and last completed months
foreach var in outpatient inpatient abt {
gen `var'_combined=(`var'_avg+`var'_last_month)/2
}
/*	
* Calculated average for last completed month and average month
svyset EA
foreach var in outpatient_avg outpatient_last_month inpatient_avg inpatient_last_month abt_avg_month abt_last_month {
svy: mean `var'
}

foreach var in outpatient inpatient abt {
svy: mean `var'_combined
}
*/		
*******************************************************************************
* SAVE ABORTION DATA
*******************************************************************************

save "$datadir/NGR5-SDP-AbortionData.dta", replace


