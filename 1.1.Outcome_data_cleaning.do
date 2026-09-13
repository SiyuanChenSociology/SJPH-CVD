* Re do data analysis after 8th desk reject
* 17th April 2025
* At UGent

cd "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/Raw_Data"
dir


* =========================================================================================
* Biomarker / Blood DATA
* Outcome 2011
/* Hypertension in 2011 */
* data: Biomarker_2011
*  Data cleaning for the Charls_2011, Biomarkers for Hypertension
	use Biomarker_2011.dta, clear
		count   // 13,974
 
	// 1. Biomarker-Hypertension
		// 1.1. Systolic Reading
			codebook qa003
				browse qa003 qa007 qa011 qa004 qa008 qa012 if qa003 == 993
					// drop if qa003 == 993
					replace qa003 = . if qa003 == 993
				sum qa003

			codebook qa007
				browse qa003 qa007 qa011 qa004 qa008 qa012 if qa007 == 993
					// drop if qa007 == 993
					replace qa007 =. if qa007 == 993
				sum qa007
			browse qa003 qa004 qa007	

			codebook qa011
				browse qa003 qa007 qa011 qa004 qa008 qa012 if qa011 == 993

					//	drop if qa011 == 993 & qa012 == .e
				sum qa011
					replace qa011 = . if qa011 == 993 
			count

			browse qa003 qa007 qa011 if qa003 ==. | qa007 ==. | qa011 ==. 
			browse qa003 qa007 qa011 if qa003 !=. & qa007 !=. & qa011 !=. 
			browse qa003 qa007 qa011 if qa003 ==. & qa007 ==.  & qa011 ==. 
				count if qa003 ==. & qa007 ==.  & qa011 ==. 
				count if qa003 ==. | qa007 ==. | qa011 ==. 
			
			gen sbp = (qa003 + qa007 + qa011) / 3 
			codebook sbp
				count if sbp == .
				sum sbp
				browse qa003 qa007 qa011 sbp if sbp == . | (qa003 ==. | qa007 ==. | qa011 ==. ) // some missing using .d, .r


		// 1.2. Diastolic Reading
			codebook qa004
				sum qa004
			codebook qa008
				sum qa008
			codebook qa012
				sum qa012
			
			
			gen dbp = (qa004 + qa008 + qa012) / 3
				sum dbp
			


		browse sbp dbp if sbp == . | dbp == .

		//	drop if sbp == . & dbp == .
		sum sbp dbp

	
	gen hypertension_b = .

		replace hypertension_b = 0 if (sbp < 140 & sbp > 0) & ///
												 (dbp < 90  & dbp > 0)
		
		replace hypertension_b = 1 if (sbp >= 140 & sbp <= 240) | ///
												 (dbp >= 90  & dbp <= 150)
	codebook hypertension_b

		browse sbp dbp hypertension_b if sbp == . | dbp == .
			replace hypertension_b = 0 if hypertension_b == .

	sum sbp dbp hypertension_b

	sort ID
	
/*	
    replace householdID = householdID + "0"
    replace ID = householdID + substr(ID,-2,2)
*/
keep ID householdID hypertension_b
	
save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning/bio_hypertension_2011.dta", replace



/* --------------------------------------------------------------------------------------------- */
/* Diabetes */
use Blood_2011.dta, clear
		count // 11,847

	// 1.Diabetes	
		// Fasting Plasma Glucose 
		// in fact, in the survey may be not all the respondents followed fasting Plasma
		// codebook newglu-8% individuals did not follow the fast!
		codebook qc1_va003  // fasting ?
			tab qc1_va003

		codebook newglu
				sum newglu
				codebook newglu if qc1_va003 == 1	
				codebook newglu if qc1_va003 == 2	
			tab newglu

		gen glucose_b = .
			replace glucose_b = 0 if  (newglu > 0 & newglu < 126) // even if not fasting, still healthy
			replace glucose_b = 1 if qc1_va003 == 1	& (newglu >=126 & newglu <= 999)
		tab glucose_b   // total 11,354
		
		codebook glucose_b

		browse qc1_va003 newglu glucose_b if glucose_b == .


		// HbA1c # better and with less missing value
		codebook newhba1c
		gen hba1c_b = .
			replace hba1c_b = 0 if newhba1c > 0 & newhba1c < 6.5
			replace hba1c_b = 1 if newhba1c >= 6.5 & newhba1c < 99 // or missing included
		tab hba1c_b
			
		codebook hba1c_b


		
		// Diabetes
		gen diabetes_b = .
			replace diabetes_b = 1 if glucose_b == 1 | hba1c_b == 1
			replace diabetes_b = 0 if glucose_b == 0 & hba1c_b == 0 
		
		tab diabetes_b
		codebook diabetes_b
		
		browse glucose_b hba1c_b diabetes_b if diabetes_b == .
		sum glucose_b hba1c_b diabetes_b

	sort ID
	/*
    replace householdID = householdID + "0"
    replace ID = householdID + substr(ID,-2,2)
	*/
	keep ID diabetes_b

	save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning/bio_diabetes_2011.dta", replace




/* --------------------------------------------------------------------------------------------- */
/* Dyslipidemia */
use Blood_2011.dta, clear
		count // 11,847

	// 1. total cholesterol (TC) ≥ 240 mg/dL (6.22 mmol/L) 
	codebook newcho
		browse newcho if newcho >= 240 
		browse newcho if newcho >= 240 & newcho != .
			browse newcho if newcho >= 240 

	gen TC = .
		replace TC = 0 if newcho > 0 & newcho < 240
		replace TC = 1 if newcho >= 240 & newcho < 999 // or missing included
	codebook TC
		browse newcho TC if newcho == .

	// 2. LDL-C ≥ 160mg/dL (4.14 mmol/L)
	codebook newldl
		browse newldl if newldl >= 160 & newldl != .
		sum newldl
	gen LDL = .
		replace LDL = 0 if newldl > 0 & newldl < 160
		replace LDL = 1 if newldl >= 160 & newldl < 999 // or missing included
	tab LDL
		
	// 3. HDL-C <40 mg/dL (1.04 mmol/L)
	codebook newhdl
		tab newhdl
	gen HDL = .
		replace HDL = 0 if newhdl >= 40  & newhdl < 999
		replace HDL = 1 if newhdl > 0 & newhdl <40     // or missing included
	tab HDL
	
	// 4. Triglyceride (TG) ≥ 200 mg/dL (2.26 mmol/L)
	codebook newtg
		sum newtg
			browse newtg TC if newtg >= 1500
		gen TG = .
			replace TG = 0 if newtg > 0 & newtg < 200 
			replace TG = 1 if newtg >= 200 & newtg < 2000
		sum TG

	sum TC LDL HDL TG

	browse TC LDL HDL TG



	gen dyslipidemia_b = .
		replace dyslipidemia_b = 1 if TC == 1 | LDL == 1 | HDL == 1 | TG == 1 
		replace dyslipidemia_b = 0 if TC == 0 & LDL == 0 & HDL == 0 & TG == 0
		
	codebook dyslipidemia_b
		browse TC LDL HDL TG if dyslipidemia_b == .
			/* replace dyslipidemia_1 = 0 if dyslipidemia_1 == . & ///
										   (TC == 0 | LDL == 0 | HDL == 0 | TG == 0) */
	browse TC LDL HDL TG dyslipidemia_b
	sum TC LDL HDL TG dyslipidemia_b

sort ID
/*
    replace householdID = householdID + "0"
    replace ID = householdID + substr(ID,-2,2)
*/

keep ID dyslipidemia_b

save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning/bio_dyslipidemia_2011.dta", replace



/* --------------------------------------------------------------------------------------------- */
* merge bio index 2011
cd "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning"
dir 

use bio_hypertension_2011.dta, clear
merge m:1 ID using "bio_diabetes_2011.dta"
	browse if _merge == 1| _merge == 2
drop _merge

sort ID
merge m:1 ID using "bio_dyslipidemia_2011.dta"
drop _merge

sort ID


gen wave = 2011
count  // 15,657
browse if hypertension_b == . & diabetes_b == . & dyslipidemia_b == .
	drop if hypertension_b == . & diabetes_b == . & dyslipidemia_b == .
count // 15,637

sort ID

save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning/bio_2011.dta", replace

/* --------------------------------------------------------------------------------------------- */





/* --------------------------------------------------------------------------------------------- */
* Self-reported & Awareness
cd "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/Raw_Data"
dir 

* Anti-CVD risk factors medication & awareness
use Health_Status_and_Functioning_2011.dta, clear
count
sort ID

/*
    replace householdID = householdID + "0"
    replace ID = householdID + substr(ID,-2,2)
*/


// 1. Hypertension
	codebook da007_1_
	gen sr_hypertension = .
		replace sr_hypertension = 0  if da007_1_ == 2
		replace sr_hypertension = 1  if da007_1_ == 1
	codebook sr_hypertension

		codebook da011s1
		codebook da011s2
		codebook da011s3
			gen hypertension_medicine = .
				replace hypertension_medicine = 0  if sr_hypertension == 1 & da011s3 == 3
				replace hypertension_medicine = 1  if sr_hypertension == 1 & (da011s1 == 1 | da011s2 == 2) 
			tab hypertension_medicine

		codebook hypertension_medicine

	
	sort ID



// 2. anti-diabetes medication & awareness

	codebook da007_3_
	gen sr_diabetes = .
		replace sr_diabetes = 0  if da007_3_ == 2
		replace sr_diabetes = 1  if da007_3_ == 1
	tab sr_diabetes

		codebook da014s1
		codebook da014s2
		codebook da014s3
		codebook da014s4
			gen diabetes_medicine = .
				replace diabetes_medicine = 0  if sr_diabetes == 1 & da014s4 == 4
				replace diabetes_medicine = 1  if sr_diabetes == 1 & (da014s1 == 1 | da014s2 == 2 | da014s3 == 3)
			tab diabetes_medicine
	
sort ID
	/*
    replace householdID = householdID + "0"
    replace ID = householdID + substr(ID,-2,2)
	*/



// 3. anti-dyslipidemia medication & awareness

	codebook da007_2_
	gen sr_dyslipidemia = .
		replace sr_dyslipidemia = 0  if da007_2_ == 2
		replace sr_dyslipidemia = 1  if da007_2_ == 1
	tab sr_dyslipidemia

		codebook da010_2_s1	
		codebook da010_2_s2
		codebook da010_2_s3	
		codebook da010_2_s4
			gen dyslipidemia_medicine = .
				replace dyslipidemia_medicine = 0 if sr_dyslipidemia == 1 & da010_2_s4 == 4
				replace dyslipidemia_medicine = 1 if sr_dyslipidemia == 1 & (da010_2_s1 == 1 | da010_2_s2 == 2 | da010_2_s3 == 3)
			tab dyslipidemia_medicine



keep ID householdID sr_hypertension hypertension_medicine sr_diabetes diabetes_medicine sr_dyslipidemia dyslipidemia_medicine
	count

sort ID

save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning/medi_sr_2011.dta", replace

/* --------------------------------------------------------------------------------------------- */



/* --------------------------------------------------------------------------------------------- */
* Merge 2011

cd "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning"
dir 

use medi_sr_2011.dta, clear
count

sort ID 
merge m:1 ID using "bio_2011.dta"
	browse if _merge == 2
	* household has several missing, N = 30
gen householdID_2011 = householdID


gen ID_w1 = ID  // to merge Harmonized data
gen ID_2011_ori = ID // keep the original


replace householdID = householdID + "0"
replace ID = householdID + substr(ID,-2,2)
	

drop wave 
gen wave = 2011

drop _merge

count
sort ID
save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning/outcome_2011.dta", replace





* =========================================================================================
* Outcome 2013

* ----------------------------------------------------------------------
* Hypertension
cd "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/Raw_Data"
dir 

use Biomarker_2013.dta, clear
count

// 1. Biomarker-Hypertension
		// 1.1. Systolic Reading
			codebook qa003
				
					
				sum qa003
			codebook qa007
				
				sum qa007
			browse qa003 qa004 qa007	
			codebook qa011
				
				sum qa011
					
			count

			browse qa003 qa007 qa011 if qa003 ==. | qa007 ==. | qa011 ==. 
			browse qa003 qa007 qa011 if qa003 !=. & qa007 !=. & qa011 !=. & qa011 != .
			
		
			
			gen sbp = (qa003 + qa007 + qa011) / 3
				sum sbp

				browse qa003 qa007 qa011 sbp if sbp == .	
			codebook sbp

			browse qa003 qa007 qa011 sbp if qa003 ==. | qa007 ==. | qa011 ==. 


		// 1.2. Diastolic Reading
			codebook qa004
				sum qa004
			codebook qa008
				sum qa008
			codebook qa012
				sum qa012
			
			
			gen dbp = (qa004 + qa008 + qa012) / 3
				sum dbp
			

		browse sbp dbp if sbp == . | dbp == .

			// drop if sbp == . & dbp == .
		sum sbp dbp

	browse sbp dbp if sbp == . & dbp == .

	gen hypertension_b = .

		replace hypertension_b = 0 if (sbp < 140 & sbp > 0) & ///
												 (dbp < 90  & dbp > 0)
		
		replace hypertension_b = 1 if (sbp >= 140 & sbp <= 240) | ///
												 (dbp >= 90  & dbp <= 200)
	codebook hypertension_b

		browse sbp dbp hypertension_b if hypertension_b == . 
			// replace prevalence_hypertension_b = 0 if prevalence_hypertension_b == .

	sort ID
		// drop householdID
/*	
    replace householdID = householdID + "0"
    replace ID = householdID + substr(ID,-2,2)
*/
keep ID hypertension_b
	
save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning/bio_hypertension_2013.dta", replace




* anti-hypertension medication 2013
cd "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/Raw_Data"
dir 
use "Health_Status_and_Functioning_2013.dta", clear
count
sort ID


	// Hypertension
	codebook da007_w2_2_1_
	gen sr_hypertension = .
		replace sr_hypertension = 0  if da007_w2_2_1_ == 2
		replace sr_hypertension = 1  if da007_w2_2_1_ == 1
	codebook sr_hypertension


		codebook da011s1
		codebook da011s2
		codebook da011s3
			gen hypertension_medicine = .
				replace hypertension_medicine = 0  if sr_hypertension == 1 & da011s3 == 3
				replace hypertension_medicine = 1  if sr_hypertension == 1 & (da011s1 == 1 | da011s2 == 2)
			tab hypertension_medicine

		codebook hypertension_medicine

	keep ID sr_hypertension hypertension_medicine
	sort ID 
	gen wave = 2013

save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning/medi_sr_2013.dta", replace



* -------------------------------------------------------------------------
* Merge

cd "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning"
dir 

use medi_sr_2013.dta, clear
count

merge m:1 ID using bio_hypertension_2013.dta 
	browse if _merge == 1 | _merge == 2

drop _merge
sort ID 

save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning/outcome_2013.dta", replace
















* =========================================================================================
* Outcome 2015


* --------------------------------------------------------------------------------
* Hypertension ~ 2015
cd "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/Raw_Data"
dir 

use Biomarker_2015.dta, clear
count

	// 1. Biomarker-Hypertension
		// 1.1. Systolic Reading
			codebook qa003
				browse qa003 qa007 qa011 qa004 qa008 qa012 if qa003 == 993
				//	drop if qa003 == 993
				sum qa003
			codebook qa007
				browse qa003 qa007 qa011 qa004 qa008 qa012 if qa007 == 993
				//drop if qa007 == 993
				sum qa007
			browse qa003 qa004 qa007	
			codebook qa011
				browse qa003 qa007 qa011 qa004 qa008 qa012 if qa011 == 993
				//	drop if qa011 == 993 & qa012 == .e
				sum qa011
					
			count

			browse qa003 qa007 qa011 if qa003 ==. | qa007 ==. | qa011 ==. 
			browse qa003 qa007 qa011 if qa003 !=. & qa007 !=. & qa011 !=. & qa011 != .
			
		
			
			gen sbp = (qa003 + qa007 + qa011) / 3
				sum sbp
				browse qa003 qa007 qa011 sbp if sbp == .	


		// 1.2. Diastolic Reading
			codebook qa004
				sum qa004
			codebook qa008
				sum qa008
			codebook qa012
				sum qa012
			
			
			gen dbp = (qa004 + qa008 + qa012) / 3
				sum dbp
			

		browse sbp dbp if sbp == . | dbp == .

		//	drop if sbp == . & dbp == .
		sum sbp dbp

	
	gen hypertension_b = .

		replace hypertension_b = 0 if (sbp < 140 & sbp > 0) & ///
												 (dbp < 90  & dbp > 0)
		
		replace hypertension_b = 1 if (sbp >= 140 & sbp <= 500) | ///
												 (dbp >= 90  & dbp <= 200)
	codebook hypertension_b
		sum hypertension_b
		browse sbp dbp hypertension_b if sbp == . | dbp == .
		//	replace prevalence_hypertension_b = 0 if prevalence_hypertension_b == .

	sort ID
		drop householdID
/*	
    replace householdID = householdID + "0"
    replace ID = householdID + substr(ID,-2,2)
*/
keep ID  hypertension_b
gen wave == 2015
	
save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning/bio_hypertension_2015.dta", replace



* --------------------------------------------------------------------------------

cd "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/Raw_Data"
dir 

use Blood_2015.dta, clear


// 1.Diabetes	
		// Fasting Plasma Glucose 
		// in fact, in the survey may be not all the respondents followed fasting Plasma
		// codebook newglu-8% individuals did not follow the fast!
		codebook bl_fasting

		codebook bl_glu
				sum bl_glu
				codebook bl_glu if bl_fasting == 1	
				codebook bl_glu if bl_fasting == 2	
			tab bl_glu

		gen glucose_b = .
			replace glucose_b = 0 if  (bl_glu > 0 & bl_glu < 126)
			replace glucose_b = 1 if bl_fasting == 1 & (bl_glu >=126 & bl_glu <= 999)
		tab glucose_b
		
		codebook glucose_b

		browse bl_fasting bl_glu glucose_b if glucose_b == .


		//HbA1c # better and with less missing value
		codebook bl_hbalc
			tab bl_hbalc
		gen hba1c_b = .
			replace hba1c_b = 0 if bl_hbalc > 0 & bl_hbalc < 6.5
			replace hba1c_b = 1 if bl_hbalc >= 6.5 & bl_hbalc < 99 // or missing included
		tab hba1c_b
			
		codebook hba1c_b
			tab hba1c_b

		
		// Diabetes
		gen diabetes_b = .
			replace diabetes_b = 1 if glucose_b == 1 | hba1c_b == 1
			replace diabetes_b = 0 if glucose_b == 0 & hba1c_b == 0 
		
		tab diabetes_b
		codebook diabetes_b
		
		browse glucose_b hba1c_b diabetes_b if diabetes_b == .
		

	sort ID
	/*
    replace householdID = householdID + "0"
    replace ID = householdID + substr(ID,-2,2)
	*/
	keep ID  diabetes_b


save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning/bio_diabetes_2015.dta", replace






* --------------------------------------------------------------------------------
* Outcome: Dyslipidemia 2015
cd "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/Raw_Data"
dir 

use Blood_2015.dta, clear


	// 2.1 total cholesterol (TC) ≥ 240 mg/dL (6.22 mmol/L) 
	codebook bl_cho 
		browse bl_cho  if bl_cho  >= 240 
		browse bl_cho  if bl_cho  >= 240 & bl_cho  != .
			browse bl_cho  if bl_cho  >= 240 

	gen TC = .
		replace TC = 0 if bl_cho  > 0 & bl_cho  < 240
		replace TC = 1 if bl_cho  >= 240 & bl_cho  < 999 // or missing included
	codebook TC
		browse bl_cho  TC if bl_cho  == .

	// 2.2 LDL-C ≥ 160mg/dL (4.14 mmol/L)
	codebook bl_ldl 
		browse bl_ldl  if bl_ldl  >= 160 & bl_ldl  != .
		sum bl_ldl 
	gen LDL = .
		replace LDL = 0 if bl_ldl  > 0 & bl_ldl  < 160
		replace LDL = 1 if bl_ldl  >= 160 & bl_ldl  < 999 // or missing included
	tab LDL
		
	// 2.3 HDL-C <40 mg/dL (1.04 mmol/L)
	codebook bl_hdl 
		tab bl_hdl 
	gen HDL = .
		replace HDL = 0 if bl_hdl  >= 40  & bl_hdl  < 999
		replace HDL = 1 if bl_hdl  <40    & bl_hdl  > 0 // or missing included
	tab HDL
	
	// 2.4 Triglyceride (TG) ≥ 200 mg/dL (2.26 mmol/L)
	codebook bl_tg 
		sum bl_tg 
			browse bl_tg  TC if bl_tg  >= 1500
		gen TG = .
			replace TG = 0 if bl_tg  < 200 & bl_tg  > 0
			replace TG = 1 if bl_tg  >= 200 & bl_tg  < 2000
		sum TG

	browse TC LDL HDL TG



	gen dyslipidemia_b = .
		replace dyslipidemia_b = 1 if TC == 1 | LDL == 1 | HDL == 1 | TG == 1 
		replace dyslipidemia_b = 0 if TC == 0 & LDL == 0 & HDL == 0 & TG == 0
		
	codebook dyslipidemia_b
		browse TC LDL HDL TG if dyslipidemia_b == .
			/* replace dyslipidemia_1 = 0 if dyslipidemia_1 == . & ///
										   (TC == 0 | LDL == 0 | HDL == 0 | TG == 0) */
	sum dyslipidemia_b
sort ID
/*
    replace householdID = householdID + "0"
    replace ID = householdID + substr(ID,-2,2)
*/

keep ID dyslipidemia_b

save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning/bio_dyslipidemia_2015.dta", replace












* --------------------------------------------------------------------------------
* anti-CVD risk factors medication & awareness

cd "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/Raw_Data"
dir 

* anti-hypertension medication
use "Health_Status_and_Functioning_2015.dta", clear
count
sort ID


	// 1. Hypertension
	codebook da007_w2_2_1_
	gen sr_hypertension= .
		replace sr_hypertension = 0  if da007_w2_2_1_ == 2
		replace sr_hypertension = 1  if da007_w2_2_1_ == 1
	codebook sr_hypertension


		codebook da011s1
		codebook da011s2
		codebook da011s3
			gen hypertension_medicine = .
				replace hypertension_medicine = 0  if sr_hypertension == 1 & da011s3 == 3
				replace hypertension_medicine = 1  if sr_hypertension == 1 & (da011s1 == 1 | da011s2 == 2)
			tab hypertension_medicine

		codebook hypertension_medicine

	sort ID


	// 2. Diabetes
	codebook da007_w2_2_3_
	gen sr_diabetes = .
		replace sr_diabetes = 0  if da007_w2_2_3_ == 2
		replace sr_diabetes = 1  if da007_w2_2_3_ == 1
	codebook sr_diabetes


		codebook da014s1
		codebook da014s2
		codebook da014s3
			gen diabetes_medicine = .
				replace diabetes_medicine = 0  if sr_diabetes == 1 & da014s3 == 3
				replace diabetes_medicine = 1  if sr_diabetes == 1 & (da014s1 == 1 | da014s2 == 2)
			tab diabetes_medicine

		codebook diabetes_medicine



	// 3. Dyslipidemia
	codebook da007_w2_2_2_
	gen sr_dyslipidemia = .
		replace sr_dyslipidemia = 0  if da007_w2_2_2_ == 2
		replace sr_dyslipidemia = 1  if da007_w2_2_2_ == 1
	codebook sr_dyslipidemia


		codebook da010_2_s1
		codebook da010_2_s2
		codebook da010_2_s3
		codebook da010_2_s4
			gen dyslipidemia_medicine = .
				replace dyslipidemia_medicine = 0  if sr_dyslipidemia == 1 & da010_2_s4 == 4
				replace dyslipidemia_medicine = 1  if sr_dyslipidemia == 1 & (da010_2_s1 == 1 | da010_2_s2 == 2)
			tab dyslipidemia_medicine

		codebook dyslipidemia

keep ID sr_hypertension hypertension_medicine sr_diabetes diabetes_medicine sr_dyslipidemia dyslipidemia_medicine


gen wave = 2015
sort ID
save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning/medi_sr_2015.dta", replace






* -------------------------------------------------------------------------
* Merge
cd "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning"
dir 
use "bio_hypertension_2015.dta", clear
count
sort ID 

merge m:1 ID using "bio_diabetes_2015.dta"
drop _merge
sort ID 

merge m:1 ID using "bio_dyslipidemia_2015.dta"
drop _merge
sort ID 

save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning/bio_2015.dta", replace



use "medi_sr_2015.dta", clear
count
sort ID

merge m:1 ID using "bio_2015.dta"
	browse if _merge != 3

drop _merge

save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning/outcome_2015.dta", replace

* -------------------------------------------------------------------------








* -------------------------------------------------------------------------
* Merge outcome 2011, 2013, 2015
cd "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning"
dir 

use "outcome_2011.dta", clear 
drop householdID householdID_2011
drop ID_w1 ID_2011_ori


// hypertension_final
codebook hypertension_b
codebook hypertension_medicine


gen pre_hypertension_2011 = .
	replace pre_hypertension_2011 = 1 if hypertension_b == 1 | hypertension_medicine == 1
	replace pre_hypertension_2011 = 0 if (hypertension_medicine == 0 & hypertension_b == .) | (hypertension_medicine == . & hypertension_b == 0) | (hypertension_medicine == 0 & hypertension_b == 0)
codebook pre_hypertension_2011
	browse hypertension_b hypertension_medicine if pre_hypertension_2011 == .


// diabetes_final
codebook diabetes_b
codebook diabetes_medicine


gen pre_diabetes_2011 = .
	replace pre_diabetes_2011 = 1 if diabetes_b == 1 | diabetes_medicine == 1
	replace pre_diabetes_2011 = 0 if (diabetes_medicine == 0 & diabetes_b == .) | (diabetes_medicine == . & diabetes_b == 0) | (diabetes_medicine == 0 & diabetes_b == 0)
codebook pre_diabetes_2011
	browse diabetes_b diabetes_medicine if pre_diabetes_2011 == .



// dyslipidemia_final
codebook dyslipidemia_b
codebook dyslipidemia_medicine


gen pre_dyslipidemia_2011 = .
	replace pre_dyslipidemia_2011 = 1 if dyslipidemia_b == 1 | dyslipidemia_medicine == 1
	replace pre_dyslipidemia_2011 = 0 if (dyslipidemia_medicine == 0 & dyslipidemia_b == .) | (dyslipidemia_medicine == . & dyslipidemia_b == 0) | (dyslipidemia_medicine == 0 & dyslipidemia_b == 0)
codebook pre_dyslipidemia_2011
	browse dyslipidemia_b dyslipidemia_medicine if pre_dyslipidemia_2011 == .



drop hypertension_b hypertension_medicine diabetes_b diabetes_medicine dyslipidemia_b dyslipidemia_medicine
drop wave 
gen sr_hypertension_2011 = sr_hypertension
gen sr_diabetes_2011 	 = sr_diabetes
gen sr_dyslipidemia_2011 = sr_dyslipidemia

drop sr_hypertension sr_diabetes sr_dyslipidemia
sort ID 



// merge outcome 2013
merge m:1 ID using "outcome_2013.dta"


	// hypertension_final
codebook hypertension_b
codebook hypertension_medicine


gen pre_hypertension_2013 = .
	replace pre_hypertension_2013 = 1 if hypertension_b == 1 | hypertension_medicine == 1
	replace pre_hypertension_2013 = 0 if (hypertension_medicine == 0 & hypertension_b == .) | (hypertension_medicine == . & hypertension_b == 0) | (hypertension_medicine == 0 & hypertension_b == 0)
codebook pre_hypertension_2013
	browse hypertension_b hypertension_medicine if pre_hypertension_2013 == .

drop _merge hypertension_b hypertension_medicine 
drop wave

gen sr_hypertension_2013 = sr_hypertension
drop sr_hypertension

sort ID 


// merge outcome 2015

merge m:1 ID using "outcome_2015.dta"

	// hypertension_final
codebook hypertension_b
codebook hypertension_medicine


gen pre_hypertension_2015 = .
	replace pre_hypertension_2015 = 1 if hypertension_b == 1 | hypertension_medicine == 1
	replace pre_hypertension_2015 = 0 if (hypertension_medicine == 0 & hypertension_b == .) | (hypertension_medicine == . & hypertension_b == 0) | (hypertension_medicine == 0 & hypertension_b == 0)
codebook pre_hypertension_2015
	browse hypertension_b hypertension_medicine if pre_hypertension_2015 == .

drop _merge hypertension_b hypertension_medicine 
drop wave


gen sr_hypertension_2015 = sr_hypertension
drop sr_hypertension




// diabetes_final
codebook diabetes_b
codebook diabetes_medicine


gen pre_diabetes_2015 = .
	replace pre_diabetes_2015 = 1 if diabetes_b == 1 | diabetes_medicine == 1
	replace pre_diabetes_2015 = 0 if (diabetes_medicine == 0 & diabetes_b == .) | (diabetes_medicine == . & diabetes_b == 0) | (diabetes_medicine == 0 & diabetes_b == 0)
codebook pre_diabetes_2015
	browse diabetes_b diabetes_medicine if pre_diabetes_2015 == .

drop diabetes_b diabetes_medicine

gen sr_diabetes_2015 = sr_diabetes
drop sr_diabetes




// dyslipidemia_final
codebook dyslipidemia_b
codebook dyslipidemia_medicine


gen pre_dyslipidemia_2015 = .
	replace pre_dyslipidemia_2015 = 1 if dyslipidemia_b == 1 | dyslipidemia_medicine == 1
	replace pre_dyslipidemia_2015 = 0 if (dyslipidemia_medicine == 0 & dyslipidemia_b == .) | (dyslipidemia_medicine == . & dyslipidemia_b == 0) | (dyslipidemia_medicine == 0 & dyslipidemia_b == 0)
codebook pre_dyslipidemia_2015
	browse dyslipidemia_b dyslipidemia_medicine if pre_dyslipidemia_2015 == .


drop dyslipidemia_b dyslipidemia_medicine

gen sr_dyslipidemia_2015 = sr_dyslipidemia
drop sr_dyslipidemia



sort ID 
save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning/merged_outcome.dta", replace
* -------------------------------------------------------------------------


* -------------------------------------------------------------------------
cd "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning"
dir 
use "merged_outcome.dta", clear
	count


// prevalence
* Hypertension
browse pre_hypertension_2011 pre_hypertension_2013 pre_hypertension_2015
	browse pre_hypertension_2011 pre_hypertension_2013 pre_hypertension_2015 if pre_hypertension_2013 == .
		count if pre_hypertension_2013 == .
	replace pre_hypertension_2013 = 1 if pre_hypertension_2013 == . & pre_hypertension_2011 == 1
		count if pre_hypertension_2013 == .
			browse pre_hypertension_2011 pre_hypertension_2013 pre_hypertension_2015 if pre_hypertension_2013 == .

	replace pre_hypertension_2013 = 1 if pre_hypertension_2013 == 0 & pre_hypertension_2011 == 1


	browse pre_hypertension_2011 pre_hypertension_2013 pre_hypertension_2015 if pre_hypertension_2015 == .
		count if pre_hypertension_2015 == .
			replace pre_hypertension_2015 = 1 if pre_hypertension_2015 == . & (pre_hypertension_2011 == 1 | pre_hypertension_2013 == 1)
				count if pre_hypertension_2015 == .
					browse pre_hypertension_2011 pre_hypertension_2013 pre_hypertension_2015 if pre_hypertension_2015 == .

	replace pre_hypertension_2015 = 1 if pre_hypertension_2015 == 0 & (pre_hypertension_2011 == 1 | pre_hypertension_2013 == 1)

browse ID pre_hypertension_2011 pre_hypertension_2013 pre_hypertension_2015	
browse pre_hypertension_2011 pre_hypertension_2013 pre_hypertension_2015 if pre_hypertension_2011 != . & pre_hypertension_2013 !=. & pre_hypertension_2015 != .

sum pre_hypertension_2011 pre_hypertension_2013 pre_hypertension_2015
sum pre_hypertension_2011 pre_hypertension_2013 pre_hypertension_2015 if pre_hypertension_2011 != . & pre_hypertension_2013 !=. & pre_hypertension_2015 != .




* Diabetes
browse pre_diabetes_2011  pre_diabetes_2015
	browse pre_diabetes_2011  pre_diabetes_2015 if pre_diabetes_2015 == .
		count if pre_diabetes_2015 == .
	replace pre_diabetes_2015 = 1 if pre_diabetes_2015 == . & pre_diabetes_2011 == 1
		count if pre_diabetes_2015 == .
			browse pre_diabetes_2011 pre_diabetes_2015 if pre_diabetes_2015 == .

	replace pre_diabetes_2015 = 1 if pre_diabetes_2015 == 0 & pre_diabetes_2011 == 1



browse ID pre_diabetes_2011 pre_diabetes_2015

sum pre_diabetes_2011 pre_diabetes_2015
sum pre_diabetes_2011 pre_diabetes_2015 if pre_diabetes_2011 !=. & pre_diabetes_2015 !=.





* Dyslipidemia
browse pre_dyslipidemia_2011  pre_dyslipidemia_2015
	browse pre_dyslipidemia_2011  pre_dyslipidemia_2015 if pre_dyslipidemia_2015 == .
		count if pre_dyslipidemia_2015 == .
	replace pre_dyslipidemia_2015 = 1 if pre_dyslipidemia_2015 == . & pre_dyslipidemia_2011 == 1
		count if pre_dyslipidemia_2015 == .
			browse pre_dyslipidemia_2011 pre_dyslipidemia_2015 if pre_dyslipidemia_2015 == .

	replace pre_dyslipidemia_2015 = 1 if pre_dyslipidemia_2015== 0 & pre_dyslipidemia_2011 == 1



browse ID pre_dyslipidemia_2011 pre_dyslipidemia_2015

sum pre_dyslipidemia_2011 pre_dyslipidemia_2015
sum pre_dyslipidemia_2011 pre_dyslipidemia_2015 if pre_dyslipidemia_2011 !=. & pre_dyslipidemia_2015 !=.


* summarize

sum pre_hypertension_2011 pre_hypertension_2013 pre_hypertension_2015
sum pre_hypertension_2011 pre_hypertension_2013 pre_hypertension_2015 if pre_hypertension_2011 != . & pre_hypertension_2013 !=. & pre_hypertension_2015 != .

sum pre_diabetes_2011 pre_diabetes_2015
sum pre_diabetes_2011 pre_diabetes_2015 if pre_diabetes_2011 !=. & pre_diabetes_2015 !=.

sum pre_dyslipidemia_2011 pre_dyslipidemia_2015
sum pre_dyslipidemia_2011 pre_dyslipidemia_2015 if pre_dyslipidemia_2011 !=. & pre_dyslipidemia_2015 !=.







// awareness
* Hypertension
browse sr_hypertension_2011 sr_hypertension_2013 sr_hypertension_2015
	browse sr_hypertension_2011 sr_hypertension_2013 sr_hypertension_2015 if sr_hypertension_2013 == .
		count if sr_hypertension_2013 == .

	replace sr_hypertension_2013 = 1 if sr_hypertension_2013 == . & sr_hypertension_2011 == 1
		count if sr_hypertension_2013 == .
			browse sr_hypertension_2011 sr_hypertension_2013 sr_hypertension_2015 if sr_hypertension_2013 == 0

	replace sr_hypertension_2013 = 1 if sr_hypertension_2013 == 0 & sr_hypertension_2011 == 1



browse sr_hypertension_2011 sr_hypertension_2013 sr_hypertension_2015 if sr_hypertension_2013 == .
		count if sr_hypertension_2015 == .
			replace sr_hypertension_2015 = 1 if sr_hypertension_2015 == . & (sr_hypertension_2011 == 1 | sr_hypertension_2013 == 1)
				count if sr_hypertension_2015 == .
					browse sr_hypertension_2011 sr_hypertension_2013 sr_hypertension_2015 if sr_hypertension_2015 == .

	replace sr_hypertension_2015 = 1 if sr_hypertension_2015 == 0 & (sr_hypertension_2011 == 1 | sr_hypertension_2013 == 1)

sum sr_hypertension_2011 sr_hypertension_2013 sr_hypertension_2015
sum sr_hypertension_2011 sr_hypertension_2013 sr_hypertension_2015 if sr_hypertension_2011 != . & sr_hypertension_2013 !=. & sr_hypertension_2015 != .



* Diabetes
browse sr_diabetes_2011  sr_diabetes_2015
	browse sr_diabetes_2011  sr_diabetes_2015 if sr_diabetes_2015 == .
		count if sr_diabetes_2015 == .
	replace sr_diabetes_2015 = 1 if sr_diabetes_2015 == . & sr_diabetes_2011 == 1
		count if sr_diabetes_2015 == .
			browse sr_diabetes_2011 sr_diabetes_2015 if sr_diabetes_2015 == .

	replace sr_diabetes_2015 = 1 if sr_diabetes_2015 == 0 & sr_diabetes_2011 == 1



browse ID sr_diabetes_2011 sr_diabetes_2015

sum sr_diabetes_2011 sr_diabetes_2015
sum sr_diabetes_2011 sr_diabetes_2015 if sr_diabetes_2011 !=. & sr_diabetes_2015  !=.





* Dyslipidemia
browse sr_dyslipidemia_2011  sr_dyslipidemia_2015
	browse sr_dyslipidemia_2011  sr_dyslipidemia_2015 if sr_dyslipidemia_2015 == .
		count if sr_dyslipidemia_2015 == .
	replace sr_dyslipidemia_2015 = 1 if sr_dyslipidemia_2015 == . & sr_dyslipidemia_2011 == 1
		count if sr_dyslipidemia_2015 == .
			browse sr_dyslipidemia_2011 sr_dyslipidemia_2015 if sr_dyslipidemia_2015 == .

	replace sr_dyslipidemia_2015 = 1 if sr_dyslipidemia_2015== 0 & sr_dyslipidemia_2011 == 1



browse ID sr_dyslipidemia_2011 sr_dyslipidemia_2015

sum sr_dyslipidemia_2011 sr_dyslipidemia_2015
sum sr_dyslipidemia_2011 sr_dyslipidemia_2015 if sr_dyslipidemia_2011 !=. & sr_dyslipidemia_2015 !=.



* summarize

sum sr_hypertension_2011 sr_hypertension_2013 sr_hypertension_2015
sum sr_hypertension_2011 sr_hypertension_2013 sr_hypertension_2015 if sr_hypertension_2011 != . & sr_hypertension_2013 !=. & sr_hypertension_2015 != .

sum sr_diabetes_2011 sr_diabetes_2015
sum sr_diabetes_2011 sr_diabetes_2015 if sr_diabetes_2011 !=. & sr_diabetes_2015  !=.

sum sr_dyslipidemia_2011 sr_dyslipidemia_2015
sum sr_dyslipidemia_2011 sr_dyslipidemia_2015 if sr_dyslipidemia_2011 !=. & sr_dyslipidemia_2015 !=.



sort ID
save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Redo/Data_Analysis/After_Cleaning/out_final.dta", replace



* summarize

sum pre_hypertension_2011 pre_hypertension_2013 pre_hypertension_2015
sum pre_hypertension_2011 pre_hypertension_2013 pre_hypertension_2015 if pre_hypertension_2011 != . 

sum pre_diabetes_2011 pre_diabetes_2015
sum pre_diabetes_2011 pre_diabetes_2015 if pre_diabetes_2011 !=. 

sum pre_dyslipidemia_2011 pre_dyslipidemia_2015
sum pre_dyslipidemia_2011 pre_dyslipidemia_2015 if pre_dyslipidemia_2011 !=. 



sum sr_hypertension_2011 sr_hypertension_2013 sr_hypertension_2015
sum sr_hypertension_2011 sr_hypertension_2013 sr_hypertension_2015 if sr_hypertension_2011 != .
sum sr_hypertension_2011 sr_hypertension_2013 sr_hypertension_2015 if sr_hypertension_2011 != . & pre_hypertension_2011 == 1  

sum sr_diabetes_2011 sr_diabetes_2015
sum sr_diabetes_2011 sr_diabetes_2015 if sr_diabetes_2011 !=. 
sum sr_diabetes_2011 sr_diabetes_2015 if sr_diabetes_2011 !=. & pre_diabetes_2011 == 1 

sum sr_dyslipidemia_2011 sr_dyslipidemia_2015
sum sr_dyslipidemia_2011 sr_dyslipidemia_2015 if sr_dyslipidemia_2011 !=. 
sum sr_dyslipidemia_2011 sr_dyslipidemia_2015 if sr_dyslipidemia_2011 !=. & pre_dyslipidemia_2011 == 1







 





































sum age
	drop if age < 45

gen age_group = .
	replace age_group = 0 if age >= 45 & age <=49
	replace age_group = 1 if age >= 50 & age <=54
	replace age_group = 2 if age >= 55 & age <=59
	replace age_group = 3 if age >= 60 & age <=64
	replace age_group = 4 if age >= 65 & age <=69
	replace age_group = 5 if age >= 70 & age <=96
codebook age_group

codebook education
codebook gender

save hypertension_wide.dta, replace


use hypertension_wide.dta, clear




reshape long hypertension_ sr_hypertension_, i(ID) j(wave)
save hypertension_long.dta, replace


gen after_baseline = wave - 2011

codebook residency
	tab hukou residency
gen urban_rural = .
	replace urban_rural = 0 if hukou == 0 & residency == 0
	replace urban_rural = 1 if hukou == 0 & residency == 1
	replace urban_rural = 2 if hukou == 1
codebook urban_rural
save hypertension_long.dta, replace



logit hypertension_ i.urban_rural i.education i.pche_quartile i.gender i.marital i.age_group after_baseline, cluster(ID) or

encode ID, gen(numeric_ID)

xtset numeric_ID
xtgee hypertension_ i.urban_rural i.education i.pche_quartile i.gender i.marital i.age_group after_baseline, ///
     family(binomial) link(logit) corr(exchangeable) eform

xtgee hypertension_ i.urban_rural i.education i.pche_quartile i.gender i.marital i.age_group after_baseline, ///
     family(binomial) link(logit) corr(exchangeable) eform

xtgee sr_hypertension_ i.urban_rural i.education i.pche_quartile i.gender i.marital i.age_group after_baseline if (hypertension_ == 1 & wave >=2011 & wave <= 2015), ///
     family(binomial) link(logit) corr(exchangeable) eform
tab hypertension_ wave 

gen hy_1 = .
	replace hy_1 = 1 if hypertension_ == 1
		browse ID sr_hypertension_ hy_1 hypertension_ wave if hy_1 == 1 & sr_hypertension_ !=.

tab sr_hypertension wave if hy_1 == 1 & sr_hypertension_ !=. , col

xtgee sr_hypertension_ i.urban_rural i.education i.pche_quartile i.gender i.marital i.age_group after_baseline if hy_1 == 1 & sr_hypertension_ !=. , ///
     family(binomial) link(logit) corr(exchangeable) eform





* =================================================================
* Outcome: Diabetes













* ====================================================================================
* merge
cd "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Writing/8. Submit/Data/raw data_outcome/outcomes_after cleaning"
dir 

use diabetes_bio_2015.dta, clear
count
sort ID

merge  m:1 ID  using "diabetes_sr_medi_2015.dta"   

browse diabetes_b diabetes_medicine if (diabetes_medicine == . & diabetes_b ==.)
	drop if (diabetes_medicine == . & diabetes_b ==.)


gen diabetes_2015 = .
	replace diabetes_2015 = 1 if diabetes_b == 1 | diabetes_medicine == 1
	replace diabetes_2015 = 0 if diabetes_b == 0 
	
codebook diabetes_2015


drop if diabetes_2015 == .
drop _merge
codebook diabetes_2015


codebook sr_diabetes_2015

keep ID diabetes_2015 sr_diabetes_2015
tab diabetes_2015 sr_diabetes_2015

sort ID
save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Writing/8. Submit/Data/raw data_outcome/outcomes_after cleaning/diabetes_2015.dta", replace





use dyslipidemia_bio_2015.dta, clear
count
sort ID

merge  m:1 ID  using "dyslipidemia_sr_medi_2015.dta"   

browse dyslipidemia_b dyslipidemia_medicine if (dyslipidemia_medicine == . & dyslipidemia_b ==.)
	drop if (dyslipidemia_medicine == . & dyslipidemia_b ==.)


gen dyslipidemia_2015 = .
	replace dyslipidemia_2015 = 1 if dyslipidemia_b == 1 | dyslipidemia_medicine == 1
	replace dyslipidemia_2015 = 0 if dyslipidemia_b == 0 
	
codebook dyslipidemia_2015


drop if dyslipidemia_2015 == .
drop _merge
codebook dyslipidemia_2015


codebook sr_dyslipidemia_2015

keep ID dyslipidemia_2015 sr_dyslipidemia_2015
tab dyslipidemia_2015 sr_dyslipidemia_2015

sort ID
save "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Writing/8. Submit/Data/raw data_outcome/outcomes_after cleaning/dyslipidemia_2015.dta", replace






* ------------------------------------------------------------------
* merge

cd "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Writing/8. Submit/Data/Merging for DA"
dir 

use "diabetes_wide_2011.dta", clear
sort ID

merge m:1 ID using "diabetes_2015.dta"
count



drop hhe loghhe hhe_quartile

drop if hukou == .
	tab _merge
		drop _merge

sum age
	drop if age < 45

gen age_group = .
	replace age_group = 0 if age >= 45 & age <=49
	replace age_group = 1 if age >= 50 & age <=54
	replace age_group = 2 if age >= 55 & age <=59
	replace age_group = 3 if age >= 60 & age <=64
	replace age_group = 4 if age >= 65 & age <=69
	replace age_group = 5 if age >= 70 & age <=96
codebook age_group

codebook education
codebook gender


browse diabetes_2011 diabetes_2015 sr_diabetes_2011 sr_diabetes_2015
	replace diabetes_2015 = 1 if diabetes_2011 == 1 | diabetes_2015 == 1
	replace sr_diabetes_2015 = 1 if sr_diabetes_2011 ==1  | sr_diabetes_2015 == 1
sort ID


save diabetes_wide.dta, replace


use diabetes_wide.dta, clear




reshape long diabetes_ sr_diabetes_, i(ID) j(wave)



gen after_baseline = wave - 2011

codebook residency
	tab hukou residency
gen urban_rural = .
	replace urban_rural = 0 if hukou == 0 & residency == 0
	replace urban_rural = 1 if hukou == 0 & residency == 1
	replace urban_rural = 2 if hukou == 1
codebook urban_rural



save diabetes_long.dta, replace




* -------------------------------------------------------
* Dyslipidemia
cd "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Writing/8. Submit/Data/Merging for DA"
dir 

use "dyslipidemia_wide_2011.dta", clear
sort ID

merge m:1 ID using "dyslipidemia_2015.dta"
count



drop hhe loghhe hhe_quartile

drop if hukou == .
	tab _merge
		drop _merge

sum age
	drop if age < 45

gen age_group = .
	replace age_group = 0 if age >= 45 & age <=49
	replace age_group = 1 if age >= 50 & age <=54
	replace age_group = 2 if age >= 55 & age <=59
	replace age_group = 3 if age >= 60 & age <=64
	replace age_group = 4 if age >= 65 & age <=69
	replace age_group = 5 if age >= 70 & age <=96
codebook age_group

codebook education
codebook gender


browse dyslipidemia_2011 dyslipidemia_2015 sr_dyslipidemia_2011 sr_dyslipidemia_2015
	replace dyslipidemia_2015 = 1 if dyslipidemia_2011 == 1 | dyslipidemia_2015 == 1
	replace sr_dyslipidemia_2015 = 1 if sr_dyslipidemia_2011 ==1  | sr_dyslipidemia_2015 == 1
sort ID

save dyslipidemia_wide.dta, replace


use dyslipidemia_wide.dta, clear




reshape long dyslipidemia_ sr_dyslipidemia_, i(ID) j(wave)

gen after_baseline = wave - 2011

codebook residency
	tab hukou residency
gen urban_rural = .
	replace urban_rural = 0 if hukou == 0 & residency == 0
	replace urban_rural = 1 if hukou == 0 & residency == 1
	replace urban_rural = 2 if hukou == 1
codebook urban_rural


save dyslipidemia_long.dta, replace




* ---------------------------------------------
* Test
cd "/Users/siyuanchen/Documents/PhD Thesis/Empirical Chapter 1/Writing/8. Submit/Final_data"
dir 

/*
drop if wave == 2018 | wave == 2020
save hypertension_long.dta, replace
*/

// hypertension
use hypertension_long.dta, clear
encode ID, gen(numeric_ID)
tab urban_rural

tab hypertension_ wave 
logit hypertension_ i.urban_rural i.education i.pche_quartile i.gender i.marital i.age_group if wave == 2011, or
logit hypertension_ i.urban_rural i.education i.pche_quartile i.gender i.marital i.age_group i.after_baseline, cluster(ID) or

xtset numeric_ID wave
xtgee hypertension_ i.urban_rural i.education i.pche_quartile i.gender i.marital i.age_group i.wave, ///
     family(binomial) link(logit) corr(ar1) 

xtgee sr_hypertension_ i.urban_rural i.education i.pche_quartile i.gender i.marital i.age_group i.after_baseline if hypertension_ == 1 , ///
     family(binomial) link(logit) corr(exchangeable) eform





// diabetes
use diabetes_long.dta, clear
encode ID, gen(numeric_ID)


tab diabetes_ wave 
logit diabetes_ i.urban_rural i.education i.pche_quartile i.gender i.marital i.age_group if wave == 2011, or
logit diabetes_ i.urban_rural i.education i.pche_quartile i.gender i.marital i.age_group i.after_baseline, cluster(ID) or

xtset numeric_ID
xtgee diabetes_ i.urban_rural i.education i.pche_quartile i.gender i.marital i.age_group i.after_baseline, ///
     family(binomial) link(logit) corr(exchangeable) eform


xtgee sr_diabetes_ i.urban_rural i.education i.pche_quartile i.gender i.marital i.age_group i.after_baseline if diabetes_ == 1  , ///
     family(binomial) link(logit) corr(independen) eform

logit sr_diabetes_ i.urban_rural i.education i.pche_quartile i.gender i.marital i.age_group i.after_baseline if diabetes_ == 1, cluster(ID) or




// dyslipidemia
use dyslipidemia_long.dta, clear
encode ID, gen(numeric_ID)


tab dyslipidemia_ wave 
logit dyslipidemia_ i.urban_rural i.education i.pche_quartile i.gender i.marital i.age_group if wave == 2011, or
logit dyslipidemia_ i.urban_rural i.education i.pche_quartile i.gender i.marital i.age_group i.after_baseline, cluster(ID) or

xtset numeric_ID
xtgee dyslipidemia_ i.urban_rural i.education i.pche_quartile i.gender i.marital i.age_group i.after_baseline, ///
     family(binomial) link(logit) corr(independen) eform


xtgee sr_dyslipidemia_ i.urban_rural i.education i.pche_quartile i.gender i.marital i.age_group i.after_baseline if dyslipidemia_ == 1  , ///
     family(binomial) link(logit) corr(independen) eform

logit sr_diabetes_ i.urban_rural i.education i.pche_quartile i.gender i.marital i.age_group i.after_baseline if dyslipidemia_ == 1, cluster(ID) or





