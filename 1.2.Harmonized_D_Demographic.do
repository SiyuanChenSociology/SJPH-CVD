cd "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/Raw_Data"
dir 

use H_CHARLS_D_Data.dta, clear

count


keep ID ID_w1 householdID communityID ///
     ragender ///
     r1hukou r2hukou r3hukou ///
     h1rural h2rural h3rural ///
     raeduc_c  ///
     hh1cperc hh2cperc hh3cperc ///
     rabyear ///
     r1mstat r2mstat r3mstat ///

sort ID


drop ID_w1

codebook ragender
	drop if ragender == .m
gen gender = .
	replace gender = 0 if ragender == 2
	replace gender = 1 if ragender == 1
		label define gender_label 1 "1 male" 0 "0 female"
		label value gender gender_label
codebook gender






codebook rabyear
	tab rabyear, missing	
	drop if rabyear == .x | rabyear == .m | rabyear == .d | rabyear == .i | rabyear == .r
gen birth_year = rabyear
	tab birth_year
		drop if birth_year > 1970



tab birth_year
gen age = 2011 - birth_year
	sum age 
tab age 
	drop if age < 45
gen age_group = .
	replace age_group = 0 if age >= 45 & age <=49
	replace age_group = 1 if age >= 50 & age <=54
	replace age_group = 2 if age >= 55 & age <=59
	replace age_group = 3 if age >= 60 & age <=64
	replace age_group = 4 if age >= 65 & age <=69
	replace age_group = 5 if age >= 70 & age <=74
	replace age_group = 6 if age >= 75 
codebook age_group







codebook raeduc_c
	tab raeduc_c, missing	
		drop if raeduc_c == .d | raeduc_c == .m | raeduc_c == .q

gen education = .
	replace education = 0 if raeduc_c == 1
	replace education = 1 if raeduc_c >= 2 & raeduc_c <= 3
	replace education = 2 if raeduc_c == 4 
	replace education = 3 if raeduc_c == 5
	replace education = 4 if raeduc_c >= 6 & raeduc_c <=10
		label define edu_lable  0 "0 illiterate" ///
							    1 "1 literate" ///
							    2 "2 Elementary school" ///
							    3 "3 Middle school" ///
							    4 "4 High school or above"
		label value education edu_lable
codebook education



* Assuming hukou won't change at age >= 45
codebook r1hukou
	tab r1hukou, missing
		drop if r1hukou == 3 | r1hukou == 4 | r1hukou == . | r1hukou == .d | r1hukou == .m | r1hukou == .r

gen hukou = .
	replace hukou = 0 if r1hukou == 1
	replace hukou = 1 if r1hukou == 2
		label define hukou_label  0 "0 rural hukou" ///
								  1 "1 urban hukou"	
		label value hukou hukou_label
tab hukou

/*
codebook r3hukou
	tab r3hukou, missing
		drop if r3hukou == . | r3hukou == .m
		drop if r3hukou == 3 | r3hukou == 4
gen hukou = .
	replace hukou = 0 if r3hukou == 1
	replace hukou = 1 if r3hukou == 2
		label define hukou_label  0 "0 rural hukou" ///
								  1 "1 urban hukou"	
		label value hukou hukou_label
codebook hukou 
*/



* Time-varying predictors

codebook h1rural
gen residency = .
	replace residency = 0 if h1rural == 1
	replace residency = 1 if h1rural == 0
		label define residency_label 0 "0 lives in rural" ///
									 1 "1 lives in urban"
		label value residency residency_label
codebook residency


/*
codebook h3rural
gen residency = .
	replace residency = 0 if h3rural == 1
	replace residency = 1 if h3rural == 0
		label define residency_label 0 "0 lives in rural" ///
									 1 "1 lives in urban"
		label value residency residency_label
codebook residency
*/


codebook r1mstat
tab r1mstat 
gen marital = .
	replace marital = 0 if r1mstat >= 4 & r1mstat <= 8
	replace marital = 1 if r1mstat == 1 | r1mstat == 3
		label define marital_label 0 "0 single" ///
								   1 "1 married or partnered"
		label value marital marital_label
codebook marital



codebook hh1cperc
	browse hh1cperc hh2cperc hh3cperc
	summarize hh1cperc, detail 
	tab hh1cperc, missing 
	//	drop if hh1cperc == .d | hh1cperc == .m | hh1cperc == .r

egen pche_quartile = xtile(hh1cperc) if !missing(hh1cperc), nq(4)

tab pche_quartile
codebook pche_quartile
	replace pche_quartile = 5 if pche_quartile == .
tabstat hh1cperc, by(pche_quartile) stat(min max mean count)
	label define qlabel 1 "Q1(0-25%)" 2 "Q2(25-50%)" 3 "Q3(50-75%)" 4 "Q4(75-100%)" 5 "5 Missing"
	label values pche_quartile qlabel
codebook pche_quartile



keep ID communityID hukou residency gender age age_group marital pche_quartile education

sort ID 
sort communityID

save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_cleaning/demo_2011.dta", replace


* =======================
cd "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning"
dir 

use "demo_2011.dta", clear
sort communityID

merge  m:1 communityID  using "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/Raw_Data/psu.dta"   
drop _merge
drop areatype

sort ID 


save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_cleaning/demo_2011_final.dta", replace


use "out_final.dta", clear
merge m:1 ID using "demo_2011_final.dta"

browse if _merge == 1
	drop if _merge == 1

browse if _merge == 2
	drop if _merge == 2

drop _merge

save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_cleaning/final.dta", replace






* ==================================
* Demographic based on 2015
cd "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/Raw_Data"
dir 
use "H_CHARLS_D_Data.dta", clear


count

codebook inw3
keep if inw3 == 1

count

keep ID communityID ///
     ragender ///
     r1hukou r2hukou r3hukou ///
     h1rural h2rural h3rural ///
     raeduc_c  ///
     hh1cperc hh2cperc hh3cperc ///
     rabyear ///
     r1mstat r2mstat r3mstat ///

sort ID



codebook ragender
	drop if ragender == .m
gen gender = .
	replace gender = 0 if ragender == 2
	replace gender = 1 if ragender == 1
		label define gender_label 1 "1 male" 0 "0 female"
		label value gender gender_label
codebook gender




codebook rabyear
	tab rabyear, missing	
	drop if rabyear == .x | rabyear == .m | rabyear == .d | rabyear == .i | rabyear == .r
gen birth_year = rabyear
	tab birth_year
		drop if birth_year > 1970



tab birth_year
gen age = 2015 - birth_year
	sum age 
tab age 
	drop if age < 45
gen age_group = .
	replace age_group = 0 if age >= 45 & age <=49
	replace age_group = 1 if age >= 50 & age <=54
	replace age_group = 2 if age >= 55 & age <=59
	replace age_group = 3 if age >= 60 & age <=64
	replace age_group = 4 if age >= 65 & age <=69
	replace age_group = 5 if age >= 70 & age <=74
	replace age_group = 6 if age >= 75 
codebook age_group







codebook raeduc_c
	tab raeduc_c, missing	
		drop if raeduc_c == .d | raeduc_c == .m | raeduc_c == .q

gen education = .
	replace education = 0 if raeduc_c == 1
	replace education = 1 if raeduc_c >= 2 & raeduc_c <= 3
	replace education = 2 if raeduc_c == 4 
	replace education = 3 if raeduc_c == 5
	replace education = 4 if raeduc_c >= 6 & raeduc_c <=10
		label define edu_lable  0 "0 illiterate" ///
							    1 "1 literate" ///
							    2 "2 Elementary school" ///
							    3 "3 Middle school" ///
							    4 "4 High school or above"
		label value education edu_lable
codebook education



* Assuming hukou won't change at age >= 45
codebook r3hukou
	tab r3hukou, missing
		browse r1hukou r2hukou r3hukou if r3hukou == . | r3hukou == .d | r3hukou == .m
	replace r3hukou = r1hukou if (r3hukou == . | r3hukou == .d | r3hukou == .m) & (r2hukou == . | r2hukou == .m)
	replace r3hukou = r2hukou if (r3hukou == . | r3hukou == .d | r3hukou == .m) & r1hukou == .
	replace r3hukou = r2hukou if (r3hukou == . | r3hukou == .d | r3hukou == .m) 


gen hukou = .
	replace hukou = 0 if r3hukou == 1
	replace hukou = 1 if r3hukou == 2
		label define hukou_label  0 "0 rural hukou" ///
								  1 "1 urban hukou"	
		label value hukou hukou_label
tab hukou

/*
codebook r3hukou
	tab r3hukou, missing
		drop if r3hukou == . | r3hukou == .m
		drop if r3hukou == 3 | r3hukou == 4
gen hukou = .
	replace hukou = 0 if r3hukou == 1
	replace hukou = 1 if r3hukou == 2
		label define hukou_label  0 "0 rural hukou" ///
								  1 "1 urban hukou"	
		label value hukou hukou_label
codebook hukou 
*/



* Time-varying predictors

codebook h3rural
gen residency = .
	replace residency = 0 if h3rural == 1
	replace residency = 1 if h3rural == 0
		label define residency_label 0 "0 lives in rural" ///
									 1 "1 lives in urban"
		label value residency residency_label
codebook residency


tab huk residency, missing


/*
codebook h3rural
gen residency = .
	replace residency = 0 if h3rural == 1
	replace residency = 1 if h3rural == 0
		label define residency_label 0 "0 lives in rural" ///
									 1 "1 lives in urban"
		label value residency residency_label
codebook residency
*/


codebook r3mstat
tab r3mstat 
gen marital = .
	replace marital = 0 if r3mstat >= 4 & r3mstat <= 8
	replace marital = 1 if r3mstat == 1 | r3mstat == 3
		label define marital_label 0 "0 single" ///
								   1 "1 married or partnered"
		label value marital marital_label
codebook marital



codebook hh3cperc
	browse hh1cperc hh2cperc hh3cperc
	summarize hh3cperc, detail 
	tab hh3cperc, missing 
	//	drop if hh1cperc == .d | hh1cperc == .m | hh1cperc == .r

egen pche_quartile = xtile(hh3cperc) if !missing(hh3cperc), nq(4)

tab pche_quartile
codebook pche_quartile
	replace pche_quartile = 5 if pche_quartile == .
tabstat hh1cperc, by(pche_quartile) stat(min max mean count)
	label define qlabel 1 "Q1(0-25%)" 2 "Q2(25-50%)" 3 "Q3(50-75%)" 4 "Q4(75-100%)" 5 "5 Missing"
	label values pche_quartile qlabel
codebook pche_quartile



keep ID communityID hukou residency gender age age_group marital pche_quartile education

sort ID 
sort communityID

count 

save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_cleaning/demo_2015.dta", replace


* =======================
cd "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning"
dir 

use "demo_2015.dta", clear
sort communityID

merge  m:1 communityID  using "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/Raw_Data/psu.dta"   
drop _merge
drop areatype

sort ID 


save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_cleaning/demo_2015_final.dta", replace


use "out_final.dta", clear
merge m:1 ID using "demo_2015_final.dta"

browse if _merge == 1
	drop if _merge == 1

browse if _merge == 2
	drop if _merge == 2

drop _merge





save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_cleaning/final_2015.dta", replace


use "final_2015.dta", clear
count

sort ID 


* wide to long
reshape long pre_hypertension_ sr_hypertension_ pre_diabetes_ sr_diabetes_ pre_dyslipidemia_ sr_dyslipidemia_, i(ID) j(year) 

tab hukou residency
gen urban_rural = .
	replace urban_rural = 0 if hukou == 0 & residency == 0
	replace urban_rural = 1 if hukou == 0 & residency == 1
	replace urban_rural = 2 if hukou == 1 
tab urban_rural

destring ID, gen(numeric_ID)

save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning/final_2015_long.dta", replace 


drop if year != 2015

sum pre_hypertension_ pre_diabetes_ pre_dyslipidemia_
sum sr_hypertension_ if pre_hypertension_ == 1
sum sr_diabetes_ if pre_diabetes_ == 1
sum sr_dyslipidemia_ if pre_dyslipidemia_ == 1
