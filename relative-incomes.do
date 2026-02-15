/*====================================================================
Project      : Labour incomes - Based on CEPAL(2018). Methodology (see Note 1) 
Output       : Relative incomes
Author       : Roxana Gómez-Valle
Creation Date: 21 January 2023
====================================================================*/

/*====================================================================
                        0: Program set up
====================================================================*/
global pjdatabase "C:\Users\User\OneDrive\FI and expenditures\Database"

set more off , perm
clear all
version 15.1

/*====================================================================
                        1: Indvidual labour income
====================================================================*/
use "${pjdatabase}/emnv14_04_poblacion", clear
rename *, lower
numlabel, add
drop if (miembro == 2)

*---------------------------1.1: Wages and other incomes first job
*---------1.1.1: Wages
replace s5p19b = . if ((s5p19b == 98) | (s5p19b == 99))
recode  s5p19b (1 = 30.4166666667) (2 = 4.2857142857) (3 = 2.1428571429) (4 = 2) ///
    (5 = 1) (6 = 0.33333333333) (7 = 0.16666666666) (8 = 0.08333333333) (else = .) ///
	, gen(freq1)
replace s5p19a = . if ((s5p19a == 9999998) | (s5p19a == 9999999))

gen wages = s5p19a * freq1

*---------1.1.2: Commissions, overtime, tips
gen comissions = s5p20b

*---------1.1.3: Thirteenth month and holidays
replace s5p21b = . if ((s5p21b == 9999998) | (s5p21b == 9999999))
replace s5p21c = . if (s5p21c == 99)

gen holidays = s5p21b / s5p21c

*---------1.1.4: Meals
gen meals = s5p22b

*---------1.1.5: Housing
gen housing = s5p23b

*---------1.1.6: Transportation
gen transport = s5p24b

*---------1.1.7: Clothing
replace s5p25b = . if (s5p25b == 999999) 
gen clothing = (s5p25b * s5p25c) / 12

*---------1.1.8: Total wages and other incomes from the first job
egen    i_wage = rsum (wages comissions holidays meals housing transport clothing)
replace i_wage = . if ((wages == .) & (comissions == .) & (holidays == .) ///
    & (meals == .) & (housing == .) & (transport == .) & (clothing == .)) 

*--------------------------------1.2: Independent employment first job
replace s5p26a = . if ((s5p26a == 9999999) |  (s5p26a == 9999998)) 
replace s5p26b = . if (s5p26b > 97) 
recode  s5p26b (1 = 30.4166666667) (2 = 4.2857142857) (3 = 2.1428571429) (4 = 2) ///
    (5 = 1) (6 = 0.33333333333) (7 = 0.16666666666) (8 = 0.08333333333) (else = .) ///
	, gen(freq2)

gen i_se = s5p26a * freq2

*-------------------------------1.3: Wages and other incomes second job
*---------1.3.1: Wages
recode s5p35b (1 = 30.4166666667) (2 = 4.2857142857) (3 = 2.1428571429) (4 = 2) ///
    (5 = 1) (6 = 0.33333333333) (else = .), gen(freq3)

gen wages2 = s5p35a * freq3

*---------1.3.2: Comissions, overtime, tips
gen comissions2 = s5p36b

*---------1.3.3: Thirtheenth month and holidays
gen holidays2 = s5p37b / s5p37c

*---------1.3.4: Meals
gen meals2 = s5p38b

*---------1.3.5: Housing
gen housing2 = s5p39b

*---------1.3.6: Transportation
gen transport2 = s5p40b

*---------1.3.7: Clothing
gen clothing2 = (s5p41b * s5p41c) / 12

*---------1.3.8: Wages and other incomes from second job
egen    i_wage2 = rsum (wages2 comissions2 holidays2 meals2 housing2 transport2 clothing2)
replace i_wage2 = . if ((wages2 == .) & (comissions2 == .) & (holidays2 == .) ///
    & (meals2 == .) & (housing2 == .) & (transport2 == .) & (clothing2 == .)) 

*----------------------------1.4: Independent employment second job
replace s5p42b = . if ((s5p42b > 97) | (s5p42b == .))
recode  s5p42b (1 = 30.4166666667) (2 = 4.2857142857) (3 = 2.1428571429) (4 = 2) ///
    (5 = 1) (6 = 0.33333333333) (7 = 0.16666666666) (8 = 0.08333333333) (else = .) ///
	, gen(freq4)

gen i_se2 = s5p42a * freq4

*----------------------------1.5: Self-employement last 12 months
replace s5p54a = . if (s5p54a == 9999999)
replace s5p54b = . if (s5p54b == 99)
recode  s5p54b (1 = 30.4166666667) (2 = 4.2857142857) (3 = 2.1428571429) (4 = 2) ///
    (5 = 1) (6 = 0.33333333333) (7 = 0.16666666666) (8 = 0.08333333333) (else = .) ///
	, gen(freq5)

gen i_se3 = s5p54a * freq5

*---------------------------1.6: Total individual labour incomes
egen    i_income = rsum(i_wage i_se i_wage2 i_se2 i_se3)
replace i_income = . if ((i_wage == .) & (i_se == .) & (i_wage2 == .) ///
    & (i_se2 == .) & (i_se3 == .))
lab var i_income "Individual monthly labour income"

/*====================================================================
                        2: Household labor income
====================================================================*/
*-----------2.1: HH labor incomes
bys i00: egen hh_income = sum(i_income)
lab var       hh_income "HH monthly labour income"

/*====================================================================
                        3: Female labour income 
====================================================================*/
*-----------3.1: Female labour income
bys i00: egen female_income = sum(i_income) if (s2p5 == 2)
lab var       female_income "Monthly female labour income - aux"

bys i00: egen fincome = max(female_income)
lab var       fincome "Monthly female labour income"

*-----------3.2: Female labour income - ln
gen     ln_fincome = ln(fincome + 1)
lab var ln_fincome "ln of female income"

gen     sq_fincome = (ln_fincome)^2
lab var sq_fincome "Squared of female labour income"

/*====================================================================
                        4: Male labour income 
====================================================================*/
*-----------4.1: Male labour income
bys i00: egen male_income = sum(i_income) if (s2p5 == 1)
lab var       male_income "Monthly male labour income - aux"

bys i00: egen mincome = max(male_income)
lab var       mincome "Monthly male labour income"

*-----------4.2: Male labour income - ln
gen     ln_mincome = ln(mincome + 1)
lab var ln_mincome "ln of male income"

gen     sq_mincome = (ln_mincome)^2
lab var sq_mincome "Square of male labour income"

/*====================================================================
                        5: Number of household earners
====================================================================*/
*-----------5.1: Household earners
gen aux_earner = (i_income > 0)
bys i00: egen hh_earner = sum(aux_earner)
lab var       hh_earner "Number of household earners"

*-----------5.2: Female earners
bys i00: egen aux_fearners = sum(aux_earner) if (s2p5 == 2)
bys i00: egen female_earners = max(aux_fearners)
lab var       female_earners "Number of female earners"
replace       female_earners = 0 if (female_earners == .)

*-----------5.3: Male earners
bys i00: egen aux_mearners = sum(aux_earner) if (s2p5 == 1)
bys i00: egen male_earners = max(aux_mearners)
lab var       male_earners "Number of male earners"
replace       male_earners = 0 if (male_earners == .)

/*====================================================================
                        6: Household characteristics
====================================================================*/
*-----------6.1: Area of residence
recode i06 (1 = 0) (2 = 1), gen (area_residence)
lab var area_residence "Area of residence"

*-----------6.2: Region of residence
rename dominio4 region_residence
lab var         region_residence "Region of residence"
lab define      region_residence 1 "Managua (capital), ref." 2 "Rest Pacific" ///
                                 3 "Central" 4 "Caribbean", replace
lab value       region_residence region_residence

*-----------6.3: Housheold size
bys i00: gen hogar = _n
bys i00: egen hh_size = max(hogar)
lab var       hh_size "Number of household members"

gen     ln_hhsize = ln(hh_size)
lab var ln_hhsize "Natural log of household size"

*-----------6.4: Number of children
gen aux_children = s2p2a < 14
bys i00: egen hh_children = sum (aux_children)
lab var       hh_children "Number of children in the household"

*-----------6.5: Average age women
bys i00: egen aux_agef = mean(s2p2a) if ((s2p5 == 2) & (i_income > 0))
bys i00: egen age_female = max(aux_agef)
lab var       age_female "Average age of female earners"

*-----------6.6: Average age men
bys i00: egen aux_agem = mean(s2p2a) if ((s2p5 == 1) & (i_income > 0))
bys i00: egen age_male = max(aux_agem)
lab var       age_male "Average age of male earners"

*-----------6.7: Average years of schooling women
*-------6.7.1: Individual years of schooling
replace s4p12b = . if (s4p12b == 8)
recode  s4p12a (0/3 12 = 0) (4/5 7 = 6) (6 = 9) (8/9 = 11) (10/11 = 16) (else = .) ///
    , gen (grade_school)
	
gen     years_schooling = grade_school + s4p12b
lab var years_schooling "Individual years of schooling"
replace years_schooling = 0 if (years_schooling == .)

*-------6.7.2: Average years of schooling women
bys i00: egen aux_eduf = mean(years_schooling) if ((s2p5 == 2) & (i_income > 0))
bys i00: egen educ_female = max(aux_eduf)
lab var       educ_female "Average years of schooling of female earners"

*-------6.7.3: Average years of schooling men
bys i00: egen aux_edum = mean(years_schooling) if ((s2p5 == 1) & (i_income > 0))
bys i00: egen educ_male = max(aux_edum)
lab var       educ_male "Average years of schooling of male earners"

*-----------6.8: Housheold structure
*-------6.8.1: Dual-earner nuclear households with dependent children
gen nuclear_aux = (s2p4 == 2)
replace nuclear_aux = 1 if ((s2p4 == 1) & (s2p7 < 3))
replace nuclear_aux = 1 if ((s2p4 == 3) & (i_income == 0))

bys i00: egen hh_composition = mean(nuclear_aux)
recode hh_composition (1 = 1) (else = 0)

drop nuclear_aux 
label var hh_composition "Dual-earner nuclear household"
label define hh_composition 0 "Dual-earner nuclear household" 1 "Non-nuclear household"

*-------6.8.2: Selecting households
gen female_earner = (s2p5 == 2) & (i_income > 0) & (i_income != .)
gen male_earner = (s2p5 == 1) & (i_income > 0) & (i_income != .)

bys i00: egen f_earners = max(female_earner)
bys i00: egen m_earners = max(male_earner)
gen earners = f_earners + m_earners

keep if (earners == 2)
/*====================================================================
                        7: Final steps
====================================================================*/
keep if (hogar == 1)
keep i00 female_income ln_fincome male_income ln_mincome hh_earner area_residence ///
    hh_size ln_hhsize region_residence female_earners male_earners hh_children    ///
	age_female age_male years_schooling educ_female educ_male hh_composition      ///
	sq_fincome sq_mincome earners

save "${pjdatabase}/emnv14-income-share.dta", replace

exit
/* End of do-file */

><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

Notes:
1. CEPAL (2018) - Medición de la pobreza por ingresos: Actualización metodológica y resultados. Metodologías CEPAL 2.


