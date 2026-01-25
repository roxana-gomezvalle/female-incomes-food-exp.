/*====================================================================
Project      : Female incomes and housheold expenditures in Nicaragua
Output       : Total household food expenditures
Auhot        : Roxana Gómez-Valle
Creation Date: 10 Jan 2023
====================================================================*/

/*====================================================================
                        0: Program set up
====================================================================*/
global pjdatabase "C:\Users\User\Documents\FI and expenditures\Database"
global do-files   "C:\Users\User\Documents\FI and expenditures\DO-files"

set more off , perm
clear all
version 15.1

/*====================================================================
                1: Household services
====================================================================*/

*--------------------1.1: House services
use "${pjdatabase}/emnv14_02_datos_de_la_vivienda_y_el_hogar.dta", clear
rename *, lower
numlabel, add

local payments s1p20 s1p30b s1p30d s1p30f s1p14a s1p14b
foreach payment of local payments {
    replace `payment' = . if ((`payment' == 99998) | (`payment' == 99999) ///
	| (`payment' == 9998))
}

local dollars s1p12b s1p13b s1p14b
foreach dollar of local dollars {
    replace `dollar' = `dollar' * 26.3612 
}

egen    exp_hservices = rsum(s1p12a s1p17 s1p20 s1p23 s1p24 s1p27 s1p30a s1p30b ///
                        s1p30c s1p30d s1p30e s1p30f s1p12a s1p12b s1p13a s1p13b ///
						s1p14a s1p14b)
lab var exp_hservices "Household services expenditures"

keep i00 dominio4 i06 exp_hservices
save "${pjdatabase}/emnv14-house-services.dta", replace

/*====================================================================
                       2: Health expenditures
====================================================================*/
use "${pjdatabase}/emnv14_04_poblacion.dta", clear
rename *, lower
numlabel, add

*--------------------2.1: Health expenditures 
replace s3p4b  = . if (s3p4b == 99998) 
replace s3p13b = . if (s3p13b == 99998)

egen health_aux = rsum(s3p4b s3p5b s3p6b s3p7b s3p10b s3p9b s3p13b s3p15b s3p16b)
bys i00: egen exp_hhealth = sum(health_aux) 
lab var       exp_hhealth "Household health expenditures"

/*====================================================================
                       3: Education expenditures
====================================================================*/

egen aux_fees = rsum(s4p6b s4p7b s4p8b s4p8c s4p21b s4p22b s4p23b s4p24b s4p24c)
egen aux_others = rsum(s4p9b s4p9c s4p10b s4p10c s4p10d s4p10e s4p25b s4p25c ///
    s4p26b s4p26c s4p26d s4p28 s4p30)
replace aux_others = aux_others / 12
egen aux_education = rsum(aux_fees aux_others)

bys i00: egen exp_heducation = sum(aux_education)
lab var       exp_heducation "Housheold education expenditures"

bys i00: gen hogar = _n
keep if (hogar == 1)
keep i00 dominio4 i06 peso2 peso3 exp_hhealth exp_heducation

save "${pjdatabase}/emnv14-health-education.dta", replace

/*====================================================================
                        4: Food expenditures
====================================================================*/
use "${pjdatabase}/emnv14_08_parte_a_de_la_seccion_7.dta", clear
rename *, lower
numlabel, add

*--------------------4.1: Monthly housheold food expenditures
replace s7p4 = . if (s7p4 == 9)
replace s7p6 = . if (s7p6 > 99999)

recode s7p4 (1 = 30.4166666667) (2 = 4.2857142857) (3 = 2) (4 = 1) ///
    (5 = 0.3333333333) (6 = 0.1666666667) (7 = 0.0833333333), gen(freq1)
	
recode s7p8 (1 = 30.4166666667) (2 = 4.2857142857) (3 = 2) (4 = 1) ///
    (5 = 0.3333333333) (6 = 0.1666666667) (7 = 0.0833333333), gen(freq2)

gen     food_aux0 = s7p6 * freq1
lab var food_aux0 "Food expenditure"

gen     food_aux1 = s7p10 * freq2 if ((s7p7 == 1) | (s7p7 == 3))
lab var food_aux1 "Food expenditure autoconsumption"

egen    food_aux = rsum(food_aux0 food_aux1) 
lab var food_aux "Auxiliar for food expenditure"

preserve
drop if ((s7prod == 54) | (s7prod == 55) | (s7prod == 57) ///
    | (s7prod == 58.09999847412109375)) // Alcohol, cigarettes and food away from home
	
bys i00: egen exp_hfood = sum(food_aux) 
lab var       exp_hfood "Housheold food expenditures"
replace       exp_hfood = . if ((s7p3 == 1) & (s7p6 == .))

*--------------------4.1: Aggregated price for food
replace s7p5a = . if (s7p5a > 9999)
gen aux_price0 = s7p6 / s7p5a
gen aux_price1 = s7p10 / s7p9a
egen mean_price = rmean(aux_price0 aux_price1)

bys i00: egen food_price = sum(mean_price)
replace       food_price = . if (food_price == 0)
lab var       food_price "Aggregated price of food"

bys i00: gen hogar = _n
keep if (hogar == 1)

keep i00 dominio4 i06 exp_hfood food_price
save "${pjdatabase}/emnv14-food.dta", replace

/*====================================================================
                5: Alcohol and cigarettes
====================================================================*/
restore

*--------------------5.1: Alcohol and cigarettes
keep if ((s7prod == 54) | (s7prod == 55) | (s7prod == 57))

bys i00: egen exp_halcohol = sum(food_aux) 
lab var       exp_halcohol "Alcohol and cigarettes expenditures"
replace       exp_halcohol = . if ((s7p3 == 1) & (s7p6 == .))

bys i00: gen hogar = _n
keep if (hogar == 1)

keep i00 dominio4 i06 exp_halcohol
save "${pjdatabase}/emnv14-alcohol-cigarettes.dta", replace

/*====================================================================
                    6: Transportation expenditures
====================================================================*/

*--------------------6.1: Transportation (including fuel)
use "${pjdatabase}/emnv14_09_parte_b1_de_la_seccion_7.dta", clear
rename *, lower
numlabel, add
preserve
drop if ((s7b1cod == 3) | (s7b1cod == 2)) //Items not related

bys i00: egen exp_transport = sum(s7p18)
replace       exp_transport = exp_transport * 4.2857142857
lab var       exp_transport "Transportation expenditures"

bys i00: gen hogar = _n
keep if (hogar == 1)

keep i00 dominio4 i06 exp_transport
save "${pjdatabase}/emnv14-transportation.dta", replace

*--------------------6.2: Other transport expenses (includes fuel)
use "${pjdatabase}/emnv14_11_parte_b3_de_la_seccion_7.dta", clear
rename *, lower
numlabel, add

keep if  (s7b3cod == 5)
replace s7p22 = . if (s7p22 == 9999998)
replace s7p22 = s7p22 / 6

keep i00 dominio4 i06 s7p22
merge 1:1 i00 using "${pjdatabase}/emnv14-transportation.dta", gen (merge)

egen    exp_htransport = rsum(exp_transport s7p22)
lab var exp_htransport "Housheold transportation expenditures"
drop merge
save "${pjdatabase}/emnv14-transportation.dta", replace

/*====================================================================
                    7: Housheold goods
====================================================================*/
*--------------------7.1: Household goods - montly
use "${pjdatabase}/emnv14_10_parte_b2_de_la_seccion_7.dta", clear
rename *, lower
numlabel, add

keep if ((s7b2cod < 8) | (s7b2cod == 15) | (s7b2cod == 16) | (s7b2cod == 14) ///
    | (s7b2cod == 20) | (s7b2cod == 21) | (s7b2cod == 23))

replace s7p20 = . if (s7p20 == 999998)
bys i00: egen exp_hgoods = sum(s7p20)
lab var       exp_hgoods "Household goods expenditures"

bys i00: gen hogar = _n
keep if (hogar == 1)

keep i00 dominio4 i06 exp_hgoods
save "${pjdatabase}/emnv14-hhgoods.dta", replace

*--------------------7.1: Household goods - semester
use "${pjdatabase}/emnv14_11_parte_b3_de_la_seccion_7.dta", clear
rename *, lower
numlabel, add

keep if ((s7b3cod == 7) | (s7b3cod == 9) | (s7b3cod == 10))
replace s7p22 = . if (s7p22 == 9999998)

bys i00: egen exp2_hgoods = sum(s7p22)
replace       exp2_hgoods = exp2_hgoods / 6
lab var       exp2_hgoods "Household goods expenditures"

bys i00: gen hogar = _n
keep if (hogar == 1)

keep i00 dominio4 i06 exp2_hgoods
merge 1:1 i00 using "${pjdatabase}/emnv14-hhgoods.dta", gen(merge)
egen aux = rsum (exp_hgoods exp2_hgoods)
replace exp_hgoods = aux
drop aux merge

save "${pjdatabase}/emnv14-hhgoods.dta", replace

/*====================================================================
                    8: Clothing and personal goods
====================================================================*/
*--------------------8.1: Clothing and personal goods - monthly
use "${pjdatabase}/emnv14_10_parte_b2_de_la_seccion_7.dta", clear
rename *, lower
numlabel, add

keep if ((s7b2cod == 8) | (s7b2cod == 9) | (s7b2cod == 10) | (s7b2cod == 12) ///
    | (s7b2cod == 13) | (s7b2cod == 17))

replace s7p20 = . if (s7p20 == 999998)
bys i00: egen exp_hpersonalg = sum(s7p20)
lab var       exp_hpersonalg "Personal goods expenditures"

bys i00: gen hogar = _n
keep if (hogar == 1)

keep i00 dominio4 i06 exp_hpersonalg
save "${pjdatabase}/emnv14-personal-goods.dta", replace

*--------------------8.1: Clothing and personal goods - semester
use "${pjdatabase}/emnv14_11_parte_b3_de_la_seccion_7.dta", clear
rename *, lower
numlabel, add

keep if (s7b3cod < 5) 
replace s7p22 = . if (s7p22 == 9999998)

bys i00: egen exp2_hpersonalg = sum(s7p22)
replace       exp2_hpersonalg = exp2_hpersonalg / 6
lab var       exp2_hpersonalg "Household clothing and personal goods expenditures"

bys i00: gen hogar = _n
keep if (hogar == 1)

keep i00 dominio4 i06 exp2_hpersonalg
merge 1:1 i00 using "${pjdatabase}/emnv14-personal-goods.dta", gen(merge)

egen aux = rsum (exp_hpersonalg exp2_hpersonalg)
replace exp_hpersonalg = aux
drop aux merge
save "${pjdatabase}/emnv14-personal-goods.dta", replace

/*====================================================================
                    9: Leisure 
====================================================================*/
*--------------------9: Leisure
use "${pjdatabase}/emnv14_10_parte_b2_de_la_seccion_7.dta", clear
rename *, lower
numlabel, add

keep if (s7b2cod == 19)

rename s7p20 aux_leisure
lab var      aux_leisure "Leisure expenditures"

keep i00 dominio4 i06 aux_leisure
save "${pjdatabase}/emnv14-leisure.dta", replace

use "${pjdatabase}/emnv14_11_parte_b3_de_la_seccion_7.dta", clear
rename *, lower
numlabel, add

keep if (s7b3cod == 12)
clonevar aux1_leisure = s7p22 
replace  aux1_leisure = aux1_leisure / 6
lab var  aux1_leisure "Leisure expenditures"

merge 1:1 i00 using "${pjdatabase}/emnv14-leisure.dta", gen (merge)
drop merge
save "${pjdatabase}/emnv14-leisure.dta", replace

use "${pjdatabase}/emnv14_12_parte_b4_de_la_seccion_7.dta", clear
rename *, lower
numlabel, add

keep if ((s7b4cod == 5) | (s7b4cod == 6) | (s7b4cod == 16))
bys i00: egen aux2_leisure = sum(s7p25)
replace       aux2_leisure = aux2_leisure / 12
lab var       aux2_leisure "Leisure expenditures"

bys i00: gen hogar=_n
keep if (hogar==1)

keep i00 dominio4 i06 aux2_leisure
merge 1:1 i00 using "${pjdatabase}/emnv14-leisure.dta", gen (merge)
drop merge
save "${pjdatabase}/emnv14-leisure.dta", replace

egen    exp_hhleisure = rsum (aux_leisure aux1_leisure aux2_leisure)
lab var exp_hhleisure "Housheold leisure expenditures"

save "${pjdatabase}/emnv14-leisure.dta", replace

/*====================================================================
                    10: Assets
====================================================================*/
*--------------------10.1: Assets
use "${pjdatabase}/emnv14_12_parte_b4_de_la_seccion_7.dta", clear
rename *, lower
numlabel, add

keep if ((s7b4cod == 2) | (s7b4cod == 3) | (s7b4cod == 3.099999904632568359))

bys i00: egen exp_hassets = sum(s7p25)
replace       exp_hassets = exp_hassets / 12
lab var       exp_hassets "Houshehold assets expenditures"

bys i00: gen hogar=_n
keep if (hogar == 1)

keep i00 dominio4 i06 exp_hassets
save "${pjdatabase}/emnv14-assets.dta", replace

/*====================================================================
                    11: Total household expenditures
====================================================================*/
*--------------------11.1: Total housing expenditures
use "${pjdatabase}/emnv14-house-services.dta", clear

foreach file in "${pjdatabase}/emnv14-health-education.dta" "${pjdatabase}/emnv14-food.dta" ///
    "${pjdatabase}/emnv14-alcohol-cigarettes.dta" "${pjdatabase}/emnv14-transportation.dta" ///
	"${pjdatabase}/emnv14-hhgoods.dta" "${pjdatabase}/emnv14-personal-goods.dta"            ///
	"${pjdatabase}/emnv14-leisure.dta" "${pjdatabase}/emnv14-assets.dta"{
        merge 1:1 i00 using "`file'", gen (merge)
        drop merge
}

order peso*, last

egen household_exp = rsum(exp_hservices exp_hhealth exp_heducation exp_hfood ///
    exp_halcohol exp_htransport exp_hgoods exp_hpersonalg exp_hhleisure       ///
	exp_hassets)
replace exp_hfood = . if (exp_hfood == 0)

local shares exp_hservices exp_hhealth exp_heducation exp_hfood exp_halcohol ///
    exp_htransport exp_hgoods exp_hpersonalg exp_hhleisure exp_hassets
foreach share of local shares {
    gen share_`share' = (`share' / household_exp) * 100
}

save "${pjdatabase}/emnv14-total-hh-expenditures.dta", replace

exit
*End of do-file


