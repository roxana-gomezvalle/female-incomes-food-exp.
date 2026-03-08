#-------------------------------------------------------------#
#     Influence of female and male incomes on food expenditures 
#     of Nicaraguan households
#     Last modified: 07 March 2026                  
#-------------------------------------------------------------#

#0. Loading libraries----
library(here)
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(survey)
library(sandwich)
library(lmtest)
library(ggplot2)
library(robustbase)
library(olsrr)
library(whitestrap)

#Relative incomes estimation----
emnv14_income_share <- 
  read_dta(here("emnv14_04_poblacion.dta"))

#Keeping variables of interest
emnv14_income_share <- emnv14_income_share %>% 
  filter(miembro < 2)

#1. Individual labour income----
#1.1. Wages and other incomes from the first job----

#Wages
emnv14_income_share <- emnv14_income_share %>%
  mutate(s5p19a = 
           if_else(s5p19a %in% c(9999998, 9999999), 
                   NA_real_, 
                   s5p19a))

emnv14_income_share <- emnv14_income_share %>%
  mutate(freq1 = 
           case_when(
             s5p19b == 1  ~ 30.4166666667,
             s5p19b == 2  ~ 4.2857142857,
             s5p19b == 3  ~ 2.1428571429,
             s5p19b == 4  ~ 2,
             s5p19b == 5  ~ 1,
             s5p19b == 6  ~ 0.33333333333,
             s5p19b == 7  ~ 0.16666666666,
             s5p19b == 8  ~ 0.08333333333,
             s5p19b %in% c(98, 99) ~ NA_real_
    ),
    wages = s5p19a * freq1
  )

#Commissions, overtime and tips
emnv14_income_share <- emnv14_income_share %>% 
  mutate(comissions = s5p20b)

#Thirteenth month and holidays
emnv14_income_share <- emnv14_income_share %>%
  mutate(s5p21b = 
           if_else(s5p21b %in% c(9999998, 9999999), 
                   NA_real_, 
                   s5p21b),
         s5p21c = 
           if_else(s5p21c %in% c(99), 
                   NA_real_, 
                   s5p21c))

emnv14_income_share <- emnv14_income_share %>% 
  mutate (holidays = s5p21b / s5p21c)

#Meals
emnv14_income_share <- emnv14_income_share %>% 
  mutate(meals = s5p22b)

#Housing
emnv14_income_share <- emnv14_income_share %>% 
  mutate(housing = s5p23b)

#Transportation
emnv14_income_share <- emnv14_income_share %>% 
  mutate(transport = s5p24b)

#Clothing
emnv14_income_share <- emnv14_income_share %>%
  mutate(s5p25b = 
           if_else(s5p25b %in% c(999999), 
                   NA_real_, 
                   s5p25b),
         clothing = ((s5p25b * s5p25c) / 12))

#Total wages and other incomes from the first job
emnv14_income_share <- emnv14_income_share %>%
  mutate(
    i_wage = if_else(
      is.na(wages) & is.na(comissions) & is.na(holidays) & is.na(meals) & 
        is.na(housing) & is.na(transport) & is.na(clothing), 
      NA_real_,
      rowSums(across(c(wages, comissions, holidays, meals, housing, transport, 
                       clothing)), na.rm = TRUE)
    )
  )

#1.2. Independent employment first job----
emnv14_income_share <- emnv14_income_share %>%
  mutate(s5p26a = 
           if_else(s5p26a %in% c(9999998, 9999999), 
                   NA_real_, 
                   s5p26a))

emnv14_income_share <- emnv14_income_share %>%
  mutate(freq2 = 
           case_when(
             s5p26b == 1  ~ 30.4166666667,
             s5p26b == 2  ~ 4.2857142857,
             s5p26b == 3  ~ 2.1428571429,
             s5p26b == 4  ~ 2,
             s5p26b == 5  ~ 1,
             s5p26b == 6  ~ 0.33333333333,
             s5p26b == 7  ~ 0.16666666666,
             s5p26b == 8  ~ 0.08333333333,
             s5p26b %in% c(98, 99) ~ NA_real_
    ),
    i_se = s5p26a * freq2
  )

#1.3. Wages and other incomes second job----
#Wages
emnv14_income_share <- emnv14_income_share %>%
  mutate(freq3 = 
           case_when(
             s5p35b == 1  ~ 30.4166666667,
             s5p35b == 2  ~ 4.2857142857,
             s5p35b == 3  ~ 2.1428571429,
             s5p35b == 4  ~ 2,
             s5p35b == 5  ~ 1,
             s5p35b == 6  ~ 0.33333333333,
             s5p35b %in% c(98, 99) ~ NA_real_
    ),
    wages2 = s5p35a * freq3
  )

#Comissions, overtime and tips
emnv14_income_share <- emnv14_income_share %>% 
  mutate(comissions2 = s5p36b)

#Thirteenth month and holidays
emnv14_income_share <- emnv14_income_share %>% 
  mutate(holidays2 = s5p37b / s5p37c)

#Meals
emnv14_income_share <- emnv14_income_share %>% 
  mutate(meals2 = s5p38b)

#Housing
emnv14_income_share <- emnv14_income_share %>% 
  mutate(housing2 = s5p39b)

#Transportation
emnv14_income_share <- emnv14_income_share %>% 
  mutate(transport2 = s5p40b)

#Clothing
emnv14_income_share <- emnv14_income_share %>% 
  mutate(clothing2 = ((s5p41b * s5p41c) / 12))

#Total wages and other incomes from second job
emnv14_income_share <- emnv14_income_share %>%
  mutate(
    i_wage2 = 
      if_else(
      is.na(wages2) & is.na(comissions2) & is.na(holidays2) & is.na(meals2) & 
        is.na(housing2) & is.na(transport2) & is.na(clothing2),
      NA_real_,
      rowSums(across(c(wages2, comissions2, holidays2, meals2, housing2, transport2, 
                       clothing2)), na.rm = TRUE)
    )
  )

#1.4. Independent employment second job----
emnv14_income_share <- emnv14_income_share %>%
  mutate(freq4 = 
           case_when(
             s5p42b == 1  ~ 30.4166666667,
             s5p42b == 2  ~ 4.2857142857,
             s5p42b == 3  ~ 2.1428571429,
             s5p42b == 4  ~ 2,
             s5p42b == 5  ~ 1,
             s5p42b == 6  ~ 0.33333333333,
             s5p42b == 7  ~ 0.16666666666,
             s5p42b == 8  ~ 0.08333333333,
             s5p42b %in% c(98, 99) ~ NA_real_
    ),
    i_se2 = s5p42a * freq4
  )

#1.5. Self-employment last 12 months----
emnv14_income_share <- emnv14_income_share %>%
  mutate(s5p54a = 
           if_else(s5p54a %in% c(9999999), 
                   NA_real_, 
                   s5p54a))

emnv14_income_share <- emnv14_income_share %>%
  mutate(freq5 = 
           case_when(
             s5p54b == 1  ~ 30.4166666667,
             s5p54b == 2  ~ 4.2857142857,
             s5p54b == 3  ~ 2.1428571429,
             s5p54b == 4  ~ 2,
             s5p54b == 5  ~ 1,
             s5p54b == 6  ~ 0.33333333333,
             s5p54b == 7  ~ 0.16666666666,
             s5p54b == 8  ~ 0.08333333333,
             s5p54b %in% c(99) ~ NA_real_
    ),
    i_se3 = s5p54a * freq5
  )

#1.6. Total individual labour income----
emnv14_income_share <- emnv14_income_share %>%
  mutate(i_income = 
           if_else(is.na(i_wage) & is.na(i_se) & is.na(i_wage2) & is.na(i_se2) & 
        is.na(i_se3),
      NA_real_,
      rowSums(across(c(i_wage, i_se, i_wage2, i_se2, i_se3)), na.rm = TRUE)
    )
  )

#2. Household labor income----
emnv14_income_share <- emnv14_income_share %>%
  group_by(i00) %>%
  mutate(hh_income = sum(i_income, na.rm = TRUE))

#3. Female labour income----
emnv14_income_share <- emnv14_income_share %>%
  group_by(i00) %>%
  mutate(fincome = sum(i_income[s2p5 == 2], na.rm = TRUE))

#3.1. Female labour income - ln
emnv14_income_share <- emnv14_income_share %>% 
  mutate(ln_fincome = 
           log(fincome +1), 
           sq_fincome = ln_fincome ^2)

#4. Male labour income----
emnv14_income_share <- emnv14_income_share %>%
  group_by(i00) %>%
  mutate(mincome = sum(i_income[s2p5 == 1], na.rm = TRUE))

#4.1. Male labour income - ln
emnv14_income_share <- emnv14_income_share %>% mutate(ln_mincome = 
                                                      log(mincome +1), 
                                                      sq_mincome = ln_mincome ^2)

#5. Number of household earners----
#5.1. Household earners----
emnv14_income_share <- emnv14_income_share %>% 
  mutate(aux_earner = 
           if_else(i_income > 0, 1, 0))

emnv14_income_share <- emnv14_income_share %>%
  group_by(i00) %>%
  mutate(hh_earner = sum(aux_earner, na.rm = TRUE))


#5.2. Female earners----
emnv14_income_share <- emnv14_income_share %>%
  group_by(i00) %>%
  mutate(female_earners = sum(aux_earner[s2p5 == 2], na.rm = TRUE))


#5.3. Male earners----
emnv14_income_share <- emnv14_income_share %>%
  group_by(i00) %>%
  mutate(male_earners = sum(aux_earner[s2p5 == 1], na.rm = TRUE))

#6. Household characteristics----
#6.1. Area of residence----
emnv14_income_share <- emnv14_income_share %>% 
  mutate(area_residence = 
           ifelse(i06 == 2, 1, 0),
)

#6.2. Region of residence----
emnv14_income_share <- emnv14_income_share %>% 
  rename(region_residence = dominio4)

#6.3. Household size----
emnv14_income_share <- emnv14_income_share %>%
  group_by(i00) %>%
  mutate(hh_size = max(s2p00),
         ln_hhsize = log(hh_size))

#6.4. Number of children----
emnv14_income_share <- emnv14_income_share %>%
  mutate(aux_children = 
           if_else(s2p2a < 14, 1, 0))

emnv14_income_share <- emnv14_income_share %>% 
  group_by(i00) %>% 
  mutate(hh_children = sum(aux_children))

#6.5. Average age women----
emnv14_income_share <- emnv14_income_share %>% 
  group_by(i00) %>% 
  mutate(age_female = mean(s2p2a[s2p5 == 2 & i_income>0], na.rm = TRUE))

#6.6. Average age men----
emnv14_income_share <- emnv14_income_share %>% 
  group_by(i00) %>% 
  mutate(age_male = mean(s2p2a[s2p5 == 1 & i_income>0], na.rm = TRUE))

#6.7. Average years of schooling----
#Individual years of schooling
emnv14_income_share <- emnv14_income_share %>%
  mutate(s4p12b = 
           if_else(s4p12b %in% c(8), 
                   NA_real_, 
                   s4p12b))

emnv14_income_share <- emnv14_income_share %>% 
  mutate(grade_school = 
           case_match(s4p12a,
                      c (0, 1, 2, 3, 12) ~ 0,
                      c (4, 5, 7) ~ 6,
                      c (6) ~ 9,
                      c (8, 9) ~ 11,
                      c(10, 11) ~ 16,
))

emnv14_income_share <- emnv14_income_share %>% 
  mutate(years_schooling = grade_school + s4p12b)

emnv14_income_share <- emnv14_income_share %>%
  mutate(years_schooling = 
           if_else(years_schooling %in% NA_real_, 0, years_schooling))


#Average years of schooling women
emnv14_income_share <- emnv14_income_share %>% 
  group_by(i00) %>% 
  mutate(educ_female = mean(years_schooling[s2p5 == 2 & i_income > 0], na.rm = TRUE))

#Average years of schooling men
emnv14_income_share <- emnv14_income_share %>% 
  group_by(i00) %>% 
  mutate(educ_male = mean(years_schooling[s2p5 == 1 & i_income > 0], na.rm = TRUE))

#6.8. Household structure----
#Dual-earner nuclear households with dependent children
emnv14_income_share <- emnv14_income_share %>% 
  mutate(nuclear_aux = if_else(s2p4 == 2, 1, 0),
         nuclear_aux = if_else(s2p4 == 1 & s2p7 < 3, 
                               1, 
                               nuclear_aux),
         nuclear_aux = if_else(s2p4 == 3 & (i_income == 0 | is.na(i_income)) , 
                               1, 
                               nuclear_aux))

emnv14_income_share <- emnv14_income_share %>% 
  group_by(i00) %>%
  mutate(hh_composition = mean(nuclear_aux))

emnv14_income_share <- emnv14_income_share %>%
  mutate(hh_composition = 
           if_else(hh_composition < 1, 0, hh_composition), na.rm = TRUE)

#Selecting households
emnv14_income_share <- emnv14_income_share %>% 
  mutate(female_earner = 
           if_else(s2p5 == 2 & i_income > 0 & !is.na(i_income), 
                   1, 
                   0))

emnv14_income_share <- emnv14_income_share %>% 
  mutate(male_earner = if_else(s2p5 == 1 & i_income > 0 & !is.na(i_income), 
                               1,
                               0))

emnv14_income_share <- emnv14_income_share %>% 
  group_by(i00) %>% 
  mutate(f_earners = max(female_earner))

emnv14_income_share <- emnv14_income_share %>% 
  group_by(i00) %>% 
  mutate(m_earners = max(male_earner))

emnv14_income_share <- emnv14_income_share %>% 
  mutate(earners = f_earners + m_earners)

emnv14_income_share <- emnv14_income_share %>% 
  filter(earners == 2)

#7. Final steps, relative incomes----
emnv14_income_share <- emnv14_income_share %>% 
  filter(s2p00 == 1)

emnv14_income_share <- emnv14_income_share %>% 
  select(i00, fincome, ln_fincome, mincome, ln_mincome, hh_earner, area_residence, 
         hh_size, ln_hhsize, region_residence, female_earners, male_earners,
         hh_children, age_female, age_male, years_schooling, educ_female, 
         educ_male, hh_composition, sq_fincome, sq_mincome, earners)

#Household expenditures----
#1. House services----
emnv14_house_services <- 
  read_dta(here("emnv14_02_datos_de_la_vivienda_y_el_hogar.dta"))

payments <- c("s1p20", "s1p30b", "s1p30d", "s1p30f", "s1p14a", "s1p14b")

emnv14_house_services[payments] <- lapply(emnv14_house_services[payments],
                                     function(x) {
                                       if_else(x == 99998 | x == 99999 | x == 9998,
                                                  NA_real_, 
                                               x)
                                     })

dollars <- c("s1p12b", "s1p13b", "s1p14b")

emnv14_house_services <- emnv14_house_services %>%
  mutate(across(all_of(dollars), ~ .x * 26.3612 ))

emnv14_house_services <- emnv14_house_services %>% 
  mutate(exp_hservices = rowSums(across(c(s1p12a, s1p17, s1p20, s1p23, s1p24,
                                          s1p27, s1p30a, s1p30b, s1p30c, s1p30d,
                                          s1p30e, s1p30f, s1p12b, s1p13a,
                                          s1p13b, s1p14a, s1p14b)),
                                na.rm = TRUE))

emnv14_house_services <- emnv14_house_services %>% 
  select(i00, dominio4, i06, exp_hservices)

#2. Health expenditures----
emnv14_health_education <- 
  read_dta(here("emnv14_04_poblacion.dta"))

emnv14_health_education <- emnv14_health_education %>%
  mutate(s3p4b = 
           if_else(s3p4b %in% c(99998), 
                   NA_real_, 
                   s3p4b))

emnv14_health_education <- emnv14_health_education %>%
  mutate(s3p13b = 
           if_else(s3p13b %in% c(99998), 
                   NA_real_, 
                   s3p13b))

emnv14_health_education <- emnv14_health_education %>% 
  mutate(health_aux = 
           rowSums(across(c(s3p4b, s3p5b, s3p6b, s3p7b, s3p10b, s3p9b, s3p13b, 
                            s3p15b, s3p16b)),
                   na.rm = TRUE))

emnv14_health_education <- emnv14_health_education %>% 
  group_by(i00) %>%
  mutate(exp_hhealth = sum(health_aux))

#3. Education expenditures----
emnv14_health_education <- emnv14_health_education %>% 
  mutate(aux_fees = 
           rowSums(across(c(s4p6b, s4p7b, s4p8b, s4p8c, s4p21b, s4p22b, s4p23b, 
                            s4p24b, s4p24c)),
                              na.rm = TRUE))

emnv14_health_education <- emnv14_health_education %>% 
  mutate(aux_others = 
           rowSums(across(c(s4p9b, s4p9c, s4p10b, s4p10c, s4p10d, s4p10e, s4p25b,
                            s4p25c, s4p26b, s4p26c, s4p26d, s4p28, s4p30)),
                            na.rm = TRUE))

emnv14_health_education <- emnv14_health_education %>% 
  mutate(aux_others = aux_others / 12)

emnv14_health_education <- emnv14_health_education %>% 
  mutate(aux_education = 
           rowSums(across(c(aux_fees, aux_others)),
                              na.rm = TRUE))

emnv14_health_education <- emnv14_health_education %>% 
  group_by(i00) %>%
  mutate(exp_heducation = sum(aux_education))

emnv14_health_education <- emnv14_health_education %>% 
  filter(s2p00 == 1)

emnv14_health_education <- emnv14_health_education %>% 
  select(i00, dominio4, i06, peso2, peso3, exp_hhealth, exp_heducation)

#4. Food expenditures----
emnv14_food_aux <- 
  read_dta(here("emnv14_08_parte_a_de_la_seccion_7.dta")) 

#4.1. Monthly household food expenditures----
emnv14_food_aux <- emnv14_food_aux %>%
  mutate(s7p4 = 
           if_else(s7p4 %in% c(9), 
                   NA_real_, 
                   s7p4))

emnv14_food_aux <- emnv14_food_aux %>%
  mutate(s7p6 = 
           if_else(s7p6 > 99999, NA_real_, s7p6))

emnv14_food_aux <- emnv14_food_aux %>% 
  mutate(freq1 = 
           case_match(s7p4,
                      c (1) ~ 30.4166666667,
                      c (2) ~ 4.2857142857,
                      c (3) ~ 2,
                      c (4) ~ 1,
                      c(5) ~ 0.3333333333,
                      c (6) ~ 0.1666666667,
                      c (7) ~ 0.0833333333,
))

emnv14_food_aux <- emnv14_food_aux %>% 
  mutate(freq2 = 
           case_match(s7p8,
                      c (1) ~ 30.4166666667,
                      c (2) ~ 4.2857142857,
                      c (3) ~ 2,
                      c (4) ~ 1,
                      c(5) ~ 0.3333333333,
                      c (6) ~ 0.1666666667,
                      c (7) ~ 0.0833333333,
))

emnv14_food_aux <- emnv14_food_aux %>% 
  mutate(food_aux0 = s7p6 * freq1)

emnv14_food_aux <- emnv14_food_aux %>% 
  mutate(food_aux1 = 
           if_else(s7p7 == 1 | s7p7 == 3,
    s7p10 * freq2,
    NA_real_
  )
)

emnv14_food_aux <- emnv14_food_aux %>% 
  mutate(food_aux = 
           rowSums(across(c(food_aux0, food_aux1)),
                                 na.rm = TRUE))

emnv14_food <- emnv14_food_aux %>%
  filter(!s7prod %in% c(54, 55, 57, 58.09999847412109375))

emnv14_food <- emnv14_food %>% 
  group_by(i00) %>% 
  mutate(exp_hfood = sum(food_aux))

emnv14_food <- emnv14_food %>% 
  mutate(exp_hfood = 
           if_else((s7p3 == 1) & is.na(s7p6), 
                   NA_real_, 
                   exp_hfood))

#4.2. Aggregated price for food----
emnv14_food <- emnv14_food %>% 
  mutate(s7p5a = 
           if_else(s7p5a > 9999, 
                   NA_real_,
                   s7p5a))

emnv14_food <- emnv14_food %>% 
  mutate(aux_price0 = s7p6 / s7p5a,
         aux_price1 = s7p10 / s7p9a)

emnv14_food <- emnv14_food %>% 
  mutate(mean_price = 
           rowMeans(across(c(aux_price0, aux_price1)),
                            na.rm = TRUE))

emnv14_food <- emnv14_food %>% 
  group_by(i00) %>% 
  mutate(food_price = 
           sum(mean_price, na.rm = TRUE))

emnv14_food <- emnv14_food %>% 
  mutate(food_price = 
           if_else(food_price == 0, 
                   NA_real_, 
                   food_price))

emnv14_food <- emnv14_food %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_food <- emnv14_food %>% 
  select(i00, dominio4, i06, exp_hfood, food_price)

#5. Alcohol and cigarettes----
emnv14_alcohol_cigarettes <- emnv14_food_aux %>% 
  filter(s7prod %in% c(54, 55, 57))

emnv14_alcohol_cigarettes <- emnv14_alcohol_cigarettes %>% 
  group_by(i00) %>%
  mutate(exp_halcohol = sum(food_aux))

emnv14_alcohol_cigarettes <- emnv14_alcohol_cigarettes %>% 
  mutate(exp_halcohol = 
           if_else(s7p3 == 1 & is.na(s7p6),
                   NA_real_, 
                   exp_halcohol))

emnv14_alcohol_cigarettes <- emnv14_alcohol_cigarettes %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_alcohol_cigarettes <- emnv14_alcohol_cigarettes %>% 
  select(i00, dominio4, i06, exp_halcohol)

#6. Transportation expenditures----
#6.1. Transportation (including fuel)----
emnv14_transportation <- 
  read_dta(here("emnv14_09_parte_b1_de_la_seccion_7.dta")) 

emnv14_transportation <- emnv14_transportation %>% 
  filter(!s7b1cod %in% c(2,3))

emnv14_transportation <- emnv14_transportation %>% 
  group_by(i00) %>%
  mutate(exp_transport = sum(s7p18, na.rm = TRUE),
         exp_transport = exp_transport * 4.2857142857)

emnv14_transportation <- emnv14_transportation %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_transportation <- emnv14_transportation %>% 
  select(i00, dominio4, i06, exp_transport)

#6.2. Other transport expenses (including fuel)----
emnv14_transportation_aux <- 
  read_dta(here("emnv14_11_parte_b3_de_la_seccion_7.dta"))

emnv14_transportation_aux <- emnv14_transportation_aux %>% 
  filter(s7b3cod %in% c(5))

emnv14_transportation_aux <- emnv14_transportation_aux %>% 
  mutate(s7p22 = 
           if_else(s7p22 == 9999998, NA_real_, s7p22))

emnv14_transportation_aux <- emnv14_transportation_aux %>% 
  mutate(s7p22 = s7p22 / 6)

emnv14_transportation_aux <- emnv14_transportation_aux %>% 
  select(i00, dominio4, i06, s7p22)

emnv14_transportation <- 
  full_join(emnv14_transportation, emnv14_transportation_aux,
            by = c ("i00", "dominio4", "i06"), 
            relationship = "one-to-one")

emnv14_transportation <- emnv14_transportation %>% 
  mutate(exp_htransport = 
           rowSums(across(c(exp_transport, s7p22)),
                   na.rm = TRUE))

#7. Household goods----
#7.1. Household goods - monthly----
emnv14_hhgoods <- 
  read_dta(here("emnv14_10_parte_b2_de_la_seccion_7.dta"))

emnv14_hhgoods <- emnv14_hhgoods %>% 
  filter(s7b2cod %in% c(1, 2, 3, 4, 5, 6, 7, 15, 16, 14, 20, 21, 23))

emnv14_hhgoods <- emnv14_hhgoods %>% 
  mutate(s7p20 = 
           if_else(s7p20 == 999998, NA_real_, s7p20))

emnv14_hhgoods <- emnv14_hhgoods %>% 
  group_by(i00) %>% 
  mutate(exp_hgoods = sum(s7p20, na.rm = TRUE))

emnv14_hhgoods <- emnv14_hhgoods %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_hhgoods <- emnv14_hhgoods %>% 
  select(i00, dominio4, i06, exp_hgoods)

#7.2. Household goods - semester----
emnv14_hhgoods_aux <- 
  read_dta(here("emnv14_11_parte_b3_de_la_seccion_7.dta"))

emnv14_hhgoods_aux <- emnv14_hhgoods_aux %>% 
  filter(s7b3cod %in% c(7, 9, 10))

emnv14_hhgoods_aux <- emnv14_hhgoods_aux %>% 
  mutate(s7p22 = 
           if_else(s7p22 == 9999998, NA_real_, s7p22))

emnv14_hhgoods_aux <- emnv14_hhgoods_aux %>% 
  group_by(i00) %>% 
  mutate(exp2_hgoods = sum(s7p22, na.rm = TRUE),
         exp2_hgoods = exp2_hgoods / 6)

emnv14_hhgoods_aux <- emnv14_hhgoods_aux %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_hhgoods_aux <- emnv14_hhgoods_aux %>% 
  select(i00, dominio4, i06, exp2_hgoods)

emnv14_hhgoods <- 
  full_join(emnv14_hhgoods, emnv14_hhgoods_aux,
            by = c ("i00", "dominio4", "i06"), 
            relationship = "one-to-one")

emnv14_hhgoods <- emnv14_hhgoods %>% 
  mutate(aux = 
           rowSums(across(c(exp_hgoods, exp2_hgoods,)), na.rm = TRUE))

emnv14_hhgoods <- emnv14_hhgoods %>% 
  mutate(exp_hgoods = aux)

emnv14_hhgoods <- emnv14_hhgoods %>% 
  select(i00, dominio4, i06, exp_hgoods, exp2_hgoods)

#8. Clothing and personal goods----
#8.1 Clothing and personal goods - monthly----
emnv14_personal_goods <- 
  read_dta(here("emnv14_10_parte_b2_de_la_seccion_7.dta"))

emnv14_personal_goods <- emnv14_personal_goods %>% 
  filter(s7b2cod %in% c(8, 9, 10, 12, 13, 17))

emnv14_personal_goods <- emnv14_personal_goods %>% 
  mutate(s7p20 = 
           if_else(s7p20 == 999998, NA_real_, s7p20))

emnv14_personal_goods <- emnv14_personal_goods %>% 
  group_by(i00) %>% 
  mutate(
    exp_hpersonalg = sum(s7p20, na.rm = TRUE))

emnv14_personal_goods <- emnv14_personal_goods %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_personal_goods <- emnv14_personal_goods %>% 
  select(i00, dominio4, i06, exp_hpersonalg)

#8.2. Clothing and personal goods - semester----
emnv14_personal_goods_aux <- 
  read_dta(here("emnv14_11_parte_b3_de_la_seccion_7.dta"))

emnv14_personal_goods_aux <- emnv14_personal_goods_aux %>% 
  filter(s7b3cod %in% c(1, 2, 3, 4))

emnv14_personal_goods_aux <- emnv14_personal_goods_aux %>% 
  mutate(s7p22 = 
           if_else(s7p22 == 9999998, NA_real_, s7p22))

emnv14_personal_goods_aux <- emnv14_personal_goods_aux %>% 
  group_by(i00) %>% 
  mutate(exp2_hpersonalg = sum(s7p22, na.rm = TRUE),
         exp2_hpersonalg = exp2_hpersonalg / 6)

emnv14_personal_goods_aux <- emnv14_personal_goods_aux %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_personal_goods_aux <- emnv14_personal_goods_aux %>% 
  select(i00, dominio4, i06, exp2_hpersonalg)

emnv14_personal_goods <- 
  full_join(emnv14_personal_goods, emnv14_personal_goods_aux,
                            by = c ("i00", "dominio4", "i06"), 
                            relationship = "one-to-one")

emnv14_personal_goods <- emnv14_personal_goods %>% 
  mutate(aux = 
           rowSums(across(c(exp_hpersonalg, exp2_hpersonalg,)), na.rm = TRUE))

emnv14_personal_goods <- emnv14_personal_goods %>% 
  mutate(exp_hpersonalg = aux)

emnv14_personal_goods <- emnv14_personal_goods %>% 
  select(i00, dominio4, i06, exp_hpersonalg, exp2_hpersonalg)

#9. Leisure----
emnv14_leisure <- 
  read_dta(here("emnv14_10_parte_b2_de_la_seccion_7.dta"))

emnv14_leisure <- emnv14_leisure %>% 
  filter(s7b2cod == 19)

emnv14_leisure <- emnv14_leisure %>% 
  mutate(aux_leisure = s7p20)

emnv14_leisure <- emnv14_leisure %>% 
  select(i00, dominio4, i06, aux_leisure)

emnv14_leisure_aux <- 
  read_dta(here("emnv14_11_parte_b3_de_la_seccion_7.dta"))

emnv14_leisure_aux <- emnv14_leisure_aux %>% 
  filter(s7b3cod %in% c(12))

emnv14_leisure_aux <- emnv14_leisure_aux %>% 
  mutate(aux1_leisure = s7p22,
         aux1_leisure = aux1_leisure / 6)

emnv14_leisure <- 
  full_join(emnv14_leisure, emnv14_leisure_aux,
            by = c ("i00", "dominio4", "i06"), 
            relationship = "one-to-one")

emnv14_leisure_aux1 <- 
  read_dta(here("emnv14_12_parte_b4_de_la_seccion_7.dta"))
  
emnv14_leisure_aux1 <- emnv14_leisure_aux1 %>% 
  filter(s7b4cod %in% c(5, 6, 16))

emnv14_leisure_aux1 <- emnv14_leisure_aux1 %>% 
  group_by(i00) %>% 
  mutate(aux2_leisure = sum (s7p25, na.rm = TRUE),
         aux2_leisure = aux2_leisure / 12)

emnv14_leisure_aux1 <- emnv14_leisure_aux1 %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_leisure_aux1 <- emnv14_leisure_aux1 %>% 
  select(i00, dominio4, i06, aux2_leisure)  

emnv14_leisure <- 
  full_join(emnv14_leisure, emnv14_leisure_aux1,
            by = c ("i00", "dominio4", "i06"), 
            relationship = "one-to-one")

emnv14_leisure <- emnv14_leisure %>% 
  mutate(exp_hhleisure = 
           rowSums(across(c(aux_leisure, aux1_leisure, aux2_leisure)), na.rm = TRUE))

#10. Assets----
emnv14_assets <- 
  read_dta(here("emnv14_12_parte_b4_de_la_seccion_7.dta"))

emnv14_assets <- emnv14_assets %>% 
  filter(s7b4cod %in% c(2, 3, 3.099999904632568359))

emnv14_assets <- emnv14_assets %>% 
  group_by(i00) %>% 
  mutate(exp_hassets = sum(s7p25, na.rm = TRUE),
         exp_hassets = exp_hassets / 12)

emnv14_assets <- emnv14_assets %>% 
  group_by(i00) %>%
  mutate(hogar = row_number()) %>%
  filter(hogar == 1) %>%
  ungroup()

emnv14_assets <- emnv14_assets %>% 
  select(i00, dominio4, i06, exp_hassets)

#11. Total household expenditures----
databases <- list(
  emnv14_health_education,
  emnv14_food,
  emnv14_alcohol_cigarettes,
  emnv14_transportation,
  emnv14_hhgoods,
  emnv14_personal_goods,
  emnv14_leisure,
  emnv14_assets,
  emnv14_house_services
)

emnv14_total_hh_expenditures <- 
  databases %>%
  reduce(left_join, by = "i00")

emnv14_total_hh_expenditures <- emnv14_total_hh_expenditures %>% 
  mutate(household_exp = 
           rowSums(across(c(exp_hservices, exp_hhealth, exp_heducation,
                            exp_hfood, exp_halcohol, exp_htransport, 
                            exp_hgoods, exp_hpersonalg, exp_hhleisure,
                            exp_hassets)), na.rm = TRUE))

emnv14_total_hh_expenditures <- emnv14_total_hh_expenditures %>% 
  mutate(exp_hfood = 
           if_else(exp_hfood == 0, NA_real_, exp_hfood))

shares <- c("exp_hservices", "exp_hhealth", "exp_heducation", "exp_hfood", "exp_halcohol", 
            "exp_htransport", "exp_hgoods", "exp_hpersonalg", "exp_hhleisure",
            "exp_hassets")

emnv14_total_hh_expenditures <- emnv14_total_hh_expenditures %>%
  mutate(across(all_of(shares), ~ (.x / household_exp) * 100,
                .names = "share_{.col}" ))

#Estimations----
#1. Merging the database----
emnv14_master_data <- 
  full_join(emnv14_income_share, emnv14_total_hh_expenditures,
            by = c ("i00"), 
            relationship = "one-to-one")

emnv14_master_data <- emnv14_master_data %>% 
  filter(!is.na(ln_fincome))

#2. Estimations----
#2.1. Descriptives----
emnv14_master_data$area_residence.f <- 
  factor(emnv14_master_data$area_residence)

emnv14_master_data$region_residence.f <- 
  factor(emnv14_master_data$region_residence)

emnv14_master_data$hh_composition.f <- 
  factor(emnv14_master_data$hh_composition)

weight <- svydesign(
  ids = ~1,
  weights = ~peso2,
  data = emnv14_master_data
)

vars <- ~ share_exp_hfood + ln_fincome + sq_fincome + ln_mincome + sq_mincome +
  female_earners + male_earners + ln_hhsize + hh_children +
  age_female + age_male + educ_female + educ_male + food_price + area_residence.f +
  region_residence.f + hh_composition.f

svymean(vars, design = weight, na.rm = TRUE)

emnv14_master_data %>% 
  { oneway.test(share_exp_hfood ~ area_residence, data = .) }


#2.2. Regresions----
reg1 <- lm(share_exp_hfood ~ ln_fincome + sq_fincome + ln_mincome + sq_mincome +
             food_price + age_female + age_male + educ_female + educ_male + 
             female_earners + male_earners + area_residence.f + ln_hhsize + 
             region_residence.f + hh_children + hh_composition.f,
           data = emnv14_master_data)
summary(reg1)

ols_test_breusch_pagan(reg1)

white_test(reg1)

#2.3. Robust estimates----
reg2 <- lm(share_exp_hfood ~ ln_fincome + sq_fincome + ln_mincome + sq_mincome +
             food_price + age_female + age_male + educ_female + educ_male + 
             female_earners + male_earners + area_residence.f + ln_hhsize + 
             region_residence.f + hh_children + hh_composition.f,
           data = emnv14_master_data,
           weights = peso2)

coeftest(
  reg2,
  vcov = vcovHC(reg2, type = "HC1")
)

#2.4. Leverage (univariate)----
#Manual identification of high leverage data points
variable <- c("share_exp_hfood", "ln_fincome", "sq_fincome", "ln_mincome",
              "sq_mincome", "food_price", "ln_hhsize", "hh_children")

for (var in variable) {
  wboxplot <- ggplot(emnv14_master_data, aes(
    x = "",
    y = .data[[var]],
  )) +
    geom_boxplot()
  print(wboxplot)
}

emnv14_master_data <- emnv14_master_data %>% 
  mutate(out_share_exp_food = share_exp_hfood)

emnv14_master_data <- emnv14_master_data %>% 
  mutate(out_share_exp_food = 
           if_else((share_exp_hfood == 0.6376329 | share_exp_hfood == 86.71799 |
                      share_exp_hfood == 88.73306), 
                   NA_real_, out_share_exp_food))

emnv14_master_data <- emnv14_master_data %>% 
  mutate(out_food_price = food_price)

emnv14_master_data <- emnv14_master_data %>% 
  mutate(out_food_price = 
           if_else((food_price == 2605.267), 
                   NA_real_,
                   out_food_price))

emnv14_master_data <- emnv14_master_data %>% 
  mutate(out_hh_children = hh_children)

emnv14_master_data <- emnv14_master_data %>% 
  mutate(out_hh_children =
           if_else((hh_children == 10), 
                   NA_real_,
                   out_hh_children))

emnv14_master_data <- emnv14_master_data %>% 
  mutate(out_ln_fincome = ln_fincome)

emnv14_master_data <- emnv14_master_data %>% 
  mutate(out_ln_fincome = 
           if_else((ln_fincome < 4), NA_real_, out_ln_fincome))

#2.5. Regression with transformed variables----
reg3 <- lm(out_share_exp_food ~  out_ln_fincome + sq_fincome + ln_mincome + 
             sq_mincome + out_food_price + age_female + age_male + educ_female + 
             educ_male + female_earners + male_earners + area_residence.f + 
             ln_hhsize + region_residence.f + out_hh_children + hh_composition.f,
           data = emnv14_master_data,
           weights = peso2)

coeftest(
  reg3,
  vcov = vcovHC(reg3, type = "HC1")
)
  
#2.6. Robust regression analysis----
reg4 <- lmrob(share_exp_hfood ~ ln_fincome + sq_fincome + ln_mincome + 
                sq_mincome + food_price + age_female + age_male + educ_female + 
                educ_male + female_earners + male_earners + area_residence.f +
                ln_hhsize + region_residence.f + hh_children + hh_composition.f,
              data = emnv14_master_data,
              method = "MM",
              control = lmrob.control(efficiency = 0.95)
)
summary(reg4)
