/*====================================================================
Project      : Female incomes and housheold food expenditure in Nicaragua
Output       : Master database for estimations
Author       : Roxana Gómez-Valle   
Creation Date: 16 Dec 2022 
====================================================================*/

/*====================================================================
                        0: Program set up
====================================================================*/
global pjdatabase "C:\Users\User\Documents\FI and expenditures\Database"
global dofiles   "C:\Users\User\Documents\FI and expenditures\Do-files"

set more off , perm
clear all
version 15.1

/*====================================================================
                        1: External do-files
====================================================================*/
qui {
    do "${dofiles}/relative-incomes.do"
    do "${dofiles}/hh-expenditures.do"
}

/*====================================================================
                        2: Merging databases
====================================================================*/
use "${pjdatabase}/emnv14-income-share.dta", clear
merge 1:1 i00 using "${pjdatabase}/emnv14-total-hh-expenditures.dta", gen (merge)
drop if (merge == 2)
drop merge
numlabel, add

save "${pjdatabase}/emnv14- master-data.dta", replace

/*====================================================================
                        3: Estimations
====================================================================*/
*-----------3.1: Descriptives
sum share_exp_hfood ln_fincome sq_fincome ln_mincome sq_mincome female_earners ///
    male_earners i.area_residence ln_hhsize i.region_residence hh_children ///
	age_female age_male educ_female educ_male i.hh_composition food_price [aw=peso2]
	
ttest share_exp_hfood, by (area_residence)
 
 *-----------3.2: Breusch-Pagan test
reg share_exp_hfood ln_fincome sq_fincome ln_mincome sq_mincome food_price ///
    age_female age_male educ_female educ_male female_earners male_earners  ///
	i.area_residence ln_hhsize i.region_residence hh_children i.hh_composition
hettest
estat imtest, white

*-----------3.3: OLS estimation
reg share_exp_hfood ln_fincome sq_fincome ln_mincome sq_mincome food_price ///
    age_female age_male educ_female educ_male female_earners male_earners  ///
	i.area_residence ln_hhsize i.region_residence hh_children i.hh_composition [aw=peso2] ///
	, vce (r)

*-----------3.4: Leverage (univariate)
*Manual identification of high leverage data points
local variables share_exp_hfood ln_fincome sq_fincome ln_mincome sq_mincome food_price ///
    ln_hhsize hh_children  

foreach variable of local variables {
    graph box `variable' [aw = peso2]
}	
	
clonevar out_share_exp_food = share_exp_hfood
replace  out_share_exp_food = . if ((share_exp_hfood == 0.6376329) ///
    | (share_exp_hfood == 86.71799) | (share_exp_hfood == 88.73306))

clonevar out_food_price = food_price
replace  out_food_price = . if (food_price == 2605.267)

clonevar out_hh_children = hh_children
replace  out_hh_children = . if (hh_children == 10)

clonevar out_ln_fincome = ln_fincome
replace  out_ln_fincome = . if 	(ln_fincome < 4)
	
*-----------3.5: Regression with transformed variables
reg out_share_exp_food out_ln_fincome sq_fincome ln_mincome sq_mincome out_food_price ///
    age_female age_male educ_female educ_male female_earners male_earners  ///
	i.area_residence ln_hhsize i.region_residence out_hh_children          ///
	i.hh_composition [aw=peso2], vce (r)
	
*Leverage and squared residuals
reg share_exp_hfood ln_fincome sq_fincome ln_mincome sq_mincome food_price ///
    age_female age_male educ_female educ_male female_earners male_earners  ///
	i.area_residence ln_hhsize i.region_residence hh_children i.hh_composition [aw=peso2] 
lvr2plot
	
*-----------3.6: Robust regression analysis  
robreg mm share_exp_hfood ln_fincome sq_fincome ln_mincome sq_mincome food_price ///
    age_female age_male educ_female educ_male female_earners male_earners        ///
	i.area_residence ln_hhsize i.region_residence hh_children i.hh_composition   ///
	, vce (r) efficiency (95)
	
exit
*End of do-file


