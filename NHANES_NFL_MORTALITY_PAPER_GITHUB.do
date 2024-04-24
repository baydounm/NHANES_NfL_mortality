cd "E:\NHANES_NFL_MORTALITY_PAPER\DATA"   


capture log close

log using "E:\NHANES_NFL_MORTALITY_PAPER\OUTPUT\DATA_MANAGEMENT.smcl", replace

************************************STEP 1: GENERATE DTA FILE FOR LINKED MORTALITY DATA*************************


*******************************************************************************
* May 2022

**	PUBLIC-USE LINKED MORTALITY FOLLOW-UP THROUGH DECEMBER 31, 2019 **

* 	The following Stata code can be used to read the fixed-width format ASCII 
*	public-use Linked Mortality Files (LMFs) from a stored location into a 
*	Stata dataset.  Basic frequencies are also produced.  
 
	
**NOTE:	The format definitions given below will result in procedure output 
	**	showing values that have been grouped as they are shown in the file layout
	**	documentation.

		
**To download and save the public-use LMFs to your hard drive, follow these steps:

*Step 1: Designate a folder on your hard drive to download the public-use LMF. 
	**	 In this example, the data will be saved to: 'C:\PUBLIC USE DATA'

*Step 2: To download the public-use LMF, go to the web site: 
	  **   https://ftp.cdc.gov/pub/health_statistics/nchs/datalinkage/linked_mortality/.

      **   Right click on the desired survey link and select "Save target as...".  
     **	 A "Save As" screen will appear where you will need to select and input 
		** a location where to save the data file on your hard drive.  

        ** Also note that the "Save as type:" box should read "DAT File (*.dat)".  
		** This will ensure that the data file is saved to your hard drive in the 
		** correct format.  

        ** In this example, the data file is saved in the folder, "C:\PUBLIC USE DATA", 
		** and the data file is saved as "<SURVEY>_MORT_2019_PUBLIC.DAT". 
*/

global SURVEY NHANES_2013_2014     // REPLACE <SURVEY> WITH RELEVANT SURVEY NAME (IN ALL CAPS)

clear all

****************
*NHANES VERSION*
****************
clear all

// DEFINE VALUE LABELS FOR REPORTS
label define premiss .z "Missing"
label define eligfmt 1 "Eligible" 2 "Under age 18, not available for public release" 3 "Ineligible" 
label define mortfmt 0 "Assumed alive" 1 "Assumed deceased" .z "Ineligible or under age 18"
label define flagfmt 0 "No - Condition not listed as a multiple cause of death" 1 "Yes - Condition listed as a multiple cause of death"	.z "Assumed alive, under age 18, ineligible for mortality follow-up, or MCOD not available"
label define qtrfmt 1 "January-March" 2 "April-June" 3 "July-September" 4 "October-December" .z "Ineligible, under age 18, or assumed alive"
label define dodyfmt .z "Ineligible, under age 18, or assumed alive"
label define ucodfmt 1 "Diseases of heart (I00-I09, I11, I13, I20-I51)"                           
label define ucodfmt 2 "Malignant neoplasms (C00-C97)"                                            , add
label define ucodfmt 3 "Chronic lower respiratory diseases (J40-J47)"                             , add
label define ucodfmt 4 "Accidents (unintentional injuries) (V01-X59, Y85-Y86)"                    , add
label define ucodfmt 5 "Cerebrovascular diseases (I60-I69)"                                       , add
label define ucodfmt 6 "Alzheimer's disease (G30)"                                                , add
label define ucodfmt 7 "Diabetes mellitus (E10-E14)"                                              , add
label define ucodfmt 8 "Influenza and pneumonia (J09-J18)"                                        , add
label define ucodfmt 9 "Nephritis, nephrotic syndrome and nephrosis (N00-N07, N17-N19, N25-N27)"  , add
label define ucodfmt 10 "All other causes (residual)"                                             , add
label define ucodfmt .z "Ineligible, under age 18, assumed alive, or no cause of death data"      , add

// READ IN THE FIXED-WIDTH FORMAT ASCII PUBLIC-USE LMF
infix seqn 1-6 eligstat 15 mortstat 16 ucod_leading 17-19 diabetes 20 hyperten 21 permth_int 43-45 permth_exm 46-48 using ${SURVEY}_MORT_2019_PUBLIC.dat	


// REPLACE MISSING VALUES TO .z FOR LABELING
replace mortstat = .z if mortstat >=.
replace ucod_leading = .z if ucod_leading >=.
replace diabetes = .z if diabetes >=.
replace hyperten = .z if hyperten >=.
replace permth_int = .z if permth_int >=.
replace permth_exm = .z if permth_exm >=.


// DEFINE VARIABLE LABELS 
label var seqn "NHANES Respondent Sequence Number"
label var eligstat "Eligibility Status for Mortality Follow-up"
label var mortstat "Final Mortality Status"
label var ucod_leading "Underlying Cause of Death: Recode"
label var diabetes "Diabetes flag from Multiple Cause of Death"
label var hyperten "Hypertension flag from Multiple Cause of Death"
label var permth_int "Person-Months of Follow-up from NHANES Interview date"
label var permth_exm "Person-Months of Follow-up from NHANES Mobile Examination Center (MEC) Date"


// ASSOCIATE VARIABLES WITH FORMAT VALUES 
label values eligstat eligfmt
label values mortstat mortfmt
label values ucod_leading ucodfmt
label values diabetes flagfmt
label values hyperten flagfmt
label value permth_int premiss
label value permth_exm premiss


// DISPLAY OVERALL DESCRIPTION OF FILE 
describe


// ONE-WAY FREQUENCIES (UNWEIGHTED)
tab1 eligstat mortstat ucod_leading diabetes hyperten, missing
tab permth_int if permth_int==.z, missing
tab permth_exm if permth_exm==.z, missing

// SAVE DATA FILE IN DIRECTORY DESIGNATED AT TOP OF PROGRAM AS **SURVEY**_PUF.DTA
// replace option allows Stata to overwrite an existing .dta file
save ${SURVEY}_PUF, replace


************************************STEP 2: MERGE MORTALITY DATASET WITH NFL DATASET*********************************

use NHANES_2013_2014_PUF,clear
sort seqn
save, replace

use SSSNFL_H,clear
sort seqn
save, replace

merge seqn using NHANES_2013_2014_PUF

save NHANES_2013_2014_PUF_NFL, replace
tab _merge
capture drop _merge
sort seqn
save, replace

*********************************STEP 3: MERGE DEMOGRAPHICS FILE WITH THE NFL-MORTALITY FILE******************************

use NHANES_2013_2014_PUF_NFL,clear
sort seqn
save, replace



use NHANES2013DEMO,clear
sort seqn
save, replace


merge seqn using NHANES_2013_2014_PUF_NFL

save NHANES_NFL_MORTALITY_PAPER, replace


*********************************STEP 4: LN TRANSFORM THE NFL VARIABLES, CREATE Z-SCORES AND TERTILES************************

use NHANES_NFL_MORTALITY_PAPER,clear

capture drop LnNFL
gen LnNFL=.
replace LnNFL=ln(sssnfl)

capture drop zLnNFL
egen zLnNFL=std(LnNFL)

capture drop LnNFLmedian
xtile LnNFLmedian=LnNFL, nq(2)


save, replace


*********************************STEP 5: STSET DATASET USING STUDYTIME AND DEATH OF ALL CAUSES*******************************


stset permth_exm [pweight=wtssnh2y], failure(mortstat)

save, replace




*********************************STEP 6: RUN PRELIMINARY COX MODELS WITH BASIC DEMOGRAPHICS: AGE, SEX, RACE, PIR*************



stcox zLnNFL riagendr ridageyr i.ridreth3 dmdeduc2 i.dmdmartl dmdhhsiz indfmpir 


stcox LnNFLmedian riagendr ridageyr i.ridreth3 dmdeduc2 i.dmdmartl dmdhhsiz indfmpir 

save, replace


*********************************STEP 7-10: INCLUDE ADDITIONAL COVARIATES: INSURANCE STATUS, SMOKING, DRUG USE, PHYSICAL ACTIVITY, ALCOHOL USE, DIET (create MAR), SOCIAL SUPPORT, DEPRESSIVE SYMPTOMS, SELECTED NUTRITIONAL BIOMARKERS (FOLATE, B-12 and vitamin D), measures of BMI, blood pressure, dyslipidemia, hyperglycemia, eGFR, CO-MORBIDITY INDEX, SELF-RATED HEALTH**************************

use NHANES_NFL_MORTALITY_PAPER,clear
capture drop _merge
sort seqn
save, replace

**Health insurance**
use HIQ_H,clear
capture drop _merge
sort seqn
save, replace


**Income measures**
use INQ_H,clear
capture drop _merge
sort seqn
save, replace

**Smoking**
use SMQ_H,clear
capture drop _merge
sort seqn
save, replace

**Alcohol use**
use ALQ_H,clear
capture drop _merge
sort seqn
save, replace


**Drug use**
use DUQ_H,clear
capture drop _merge
sort seqn
save, replace


**DIET**
use DR1TOT_H,clear
capture drop _merge
sort seqn
save, replace


use DR2TOT_H,clear
capture drop _merge
sort seqn
save, replace


use DR1TOT_H,clear
merge seqn using DR2TOT_H
save DR12TOT_H,replace
capture drop _merge
sort seqn
save, replace


**BODY MEASURES**
use BMX_H,clear
capture drop _merge
sort seqn
save, replace


**Blood pressure**
use BPX_H,clear
capture drop _merge
sort seqn
save, replace

**Albumin and creatinine**
use ALB_CR_H,clear
capture drop _merge
sort seqn
save, replace

**BP and Cholesterol medication**
use BPQ_H,clear
capture drop _merge
sort seqn
save, replace

**Depressive symptoms**
use DPQ_H,clear
capture drop _merge
sort seqn
save, replace

**Self-rated health**
use HSQ_H,clear
capture drop _merge
sort seqn
save, replace

**Co-morbidity index**
use MCQ_H,clear
capture drop _merge
sort seqn
save, replace

**Physical activity**
use PAQ_H,clear
capture drop _merge
sort seqn
save, replace

**Sleep**
use SLQ_H,clear
capture drop _merge
sort seqn
save, replace

**Vitamin B-12**
use VITB12_H,clear
capture drop _merge
sort seqn
save, replace


**Folate**
use FOLATE_H,clear
capture drop _merge
sort seqn
save, replace


**Vitamin D**
use VID_H,clear
capture drop _merge
sort seqn
save, replace


**Lipids and glycated hemoglobin**

use TRIGLY_H,clear
capture drop _merge
sort seqn
save, replace


use TCHOL_H,clear
capture drop _merge
sort seqn
save, replace


use INS_H,clear
capture drop _merge
sort seqn
save, replace


use GHB_H,clear
capture drop _merge
sort seqn
save, replace


use GLU_H,clear
capture drop _merge
sort seqn
save, replace

**HIQ_H INQ_H SMQ_H ALQ_H DUQ_H DR12TOT_H BMX_H BPX_H ALB_CR_H BPQ_H DPQ_H HSQ_H MCQ_H PAQ_H SLQ_H FOLATE_H VITB12_H VID_H 
**TRIGLY_H TCHOL_H INS_H GHB_H GLU_H 


use NHANES_NFL_MORTALITY_PAPER,clear
merge seqn using HIQ_H
capture drop _merge
sort seqn
save, replace
merge seqn using INQ_H
capture drop _merge
sort seqn
save, replace
merge seqn using SMQ_H
capture drop _merge
sort seqn
save, replace
merge seqn using ALQ_H
capture drop _merge
sort seqn
save, replace
merge seqn using DUQ_H
capture drop _merge
sort seqn
save, replace
merge seqn using DR12TOT_H
capture drop _merge
sort seqn
save, replace
merge seqn using BMX_H
capture drop _merge
sort seqn
save, replace
merge seqn using BPX_H
capture drop _merge
sort seqn
save, replace
merge seqn using ALB_CR_H
capture drop _merge
sort seqn
save, replace
merge seqn using BPQ_H
capture drop _merge
sort seqn
save, replace
merge seqn using DPQ_H
capture drop _merge
sort seqn
save, replace
merge seqn using HSQ_H
capture drop _merge
sort seqn
save, replace
merge seqn using MCQ_H
capture drop _merge
sort seqn
save, replace
merge seqn using PAQ_H
capture drop _merge
sort seqn
save, replace
merge seqn using SLQ_H
capture drop _merge
sort seqn
save, replace
merge seqn using FOLATE_H
capture drop _merge
sort seqn
save, replace
merge seqn using VITB12_H
capture drop _merge
sort seqn
save, replace
merge seqn using VID_H
capture drop _merge
sort seqn
save, replace
merge seqn using TRIGLY_H
capture drop _merge
sort seqn
save, replace
merge seqn using TCHOL_H
capture drop _merge
sort seqn
save, replace
merge seqn using INS_H
capture drop _merge
sort seqn
save, replace
merge seqn using GHB_H
capture drop _merge
sort seqn
save, replace
merge seqn using GLU_H
capture drop _merge
sort seqn
save, replace

capture rename seqn-lbdglusi,upper

save, replace

***************************SOCIO-DEMOGRAPHIC VARIABLES****************************************************

**AGE*

capture drop AGE
gen AGE=RIDAGEYR

su AGE, det
histogram AGE

**SEX**

capture drop SEX
gen SEX=RIAGENDR

tab SEX


**RACE/ETHNICITY**

**0: Non-Hispanic White, 1:Non-Hispanic Black, 2:Mexican American or other Hispanic, and 3:Other race/ethnicities


capture drop RACE_ETHN
gen RACE_ETHN=.
replace RACE_ETHN=0 if RIDRETH3==3
replace RACE_ETHN=1 if RIDRETH3==4
replace RACE_ETHN=2 if RIDRETH3==1 | RIDRETH3==2
replace RACE_ETHN=3 if RIDRETH3~=. & RACE_ETHN==.

tab RACE_ETHN

save, replace


**MARITAL STATUS**
tab DMDMARTL, missing

capture drop MARRIED_LIVP
gen MARRIED_LIVP=.
replace MARRIED_LIVP=1 if DMDMARTL == 1 | DMDMARTL == 6
replace MARRIED_LIVP=2 if DMDMARTL == 2 | DMDMARTL == 3 | DMDMARTL == 4 | DMDMARTL == 5

tab MARRIED_LIVP, missing


**HOUSEHOLD SIZE**

capture drop HOUSEHOLDSIZE
gen HOUSEHOLDSIZE=DMDHHSIZ
su HOUSEHOLDSIZE


**POVERTY INCOME RATIO**

tab INDFMPIR, missing

capture drop PIR
gen PIR=.
replace PIR=1 if INDFMPIR <1 
replace PIR=2 if INDFMPIR>=1 & INDFMPIR<2
replace PIR=3 if INDFMPIR>=2 & INDFMPIR ~=.

tab PIR, missing


**EDUCATION, YEARS**

**Less than 9th grade 		
**9-11th grade (Includes 12th grade with no diploma) 		
**High school graduate/GED or equivalent 	 	
**Some college or AA degree 	 	
**College graduate or above 	


tab DMDEDUC2, missing

capture drop EDUCATION
gen EDUCATION=DMDEDUC2
replace EDUCATION=. if (DMDEDUC2==9 | DMDEDUC2==7) 

tab EDUCATION, missing
su EDUCATION

save, replace


***************************SMOKING***********************************************************************

tab SMQ020, missing
tab SMQ040, missing

capture drop SMOKE
gen SMOKE=.
replace SMOKE=1 if SMQ020==2
replace SMOKE=2 if SMQ020==1 & SMQ040==3
replace SMOKE=3 if SMQ020==1 & (SMQ040==1|SMQ040==2)
 
tab SMOKE, missing


save, replace



***************************ALCOHOL, GRAMS *******************************************************************

tab ALQ101, missing

capture drop ALCOHOL
gen ALCOHOL=.
replace ALCOHOL=1 if (ALQ101==1)
replace ALCOHOL=2 if (ALQ101==2)

tab ALCOHOL, missing


*****************************EVER DRUG USE********************************************************************
tab1 DUQ200 DUQ240 DUQ290 DUQ330

capture drop DUQ200r DUQ240r DUQ290r DUQ330r


foreach x of varlist DUQ200 DUQ240 DUQ290 DUQ330 {
	gen `x'r=`x' if `x'~=7 & `x'~=9
}

tab1  DUQ200r DUQ240r DUQ290r DUQ330r



capture drop DRUG_USER_EVER
gen DRUG_USER_EVER=.
replace DRUG_USER_EVER=1 if DUQ200r==1 | DUQ240r==1 | DUQ290r==1 | DUQ330r==1
replace DRUG_USER_EVER=0 if DRUG_USER_EVER~=1

tab DRUG_USER_EVER

save, replace


**************************** ENERGEY INTAKE*******************************************

*****************************DASH DIET SCORE*************************************************************

/*DASH component 1-8 scores*/

/*use "H:\MANUSCRIPTS_2018_2019\MANSCRIPT_12_DASH_MORTALITY\DATA\FULL_DASH_DIET_DATASET_MORTALITY_LINKED_B_G.dta"*/

/*Energy -- kcal*/

summ DR1TKCAL 
summ DR2TKCAL 

capture drop DR12TKCAL
gen DR12TKCAL=(DR1TKCAL+DR2TKCAL)/2

su DR12TKCAL

save, replace


/*Saturated fat -- grams*/

summ DR1TSFAT 
summ DR2TSFAT 


capture drop DR12TSFAT
gen DR12TSFAT=(DR1TSFAT+DR2TSFAT)/2

su DR12TSFAT

save, replace


capture drop SAT_FAT_DASH
gen SAT_FAT_DASH=.
replace SAT_FAT_DASH=(DR12TSFAT*9)/DR12TKCAL*100

summ SAT_FAT_DASH

capture drop SAT_FAT_DASH_BR
gen SAT_FAT_DASH_BR=.
replace SAT_FAT_DASH_BR=1 if SAT_FAT_DASH <=6 
replace SAT_FAT_DASH_BR=0.5 if (SAT_FAT_DASH >6 & SAT_FAT_DASH<=11)
replace SAT_FAT_DASH_BR=0 if (SAT_FAT_DASH>11 & SAT_FAT_DASH~=.)

tab SAT_FAT_DASH_BR, missing


/*Total fat -- grams*/

summ DR1TTFAT 
summ DR2TTFAT 

capture drop DR12TTFAT
gen DR12TTFAT=(DR1TTFAT+DR2TTFAT)/2

save, replace


capture drop TOT_FAT_DASH
gen TOT_FAT_DASH=.
replace TOT_FAT_DASH=(DR12TTFAT*9)/DR12TKCAL*100

summ TOT_FAT_DASH

capture drop TOT_FAT_DASH_BR
gen TOT_FAT_DASH_BR=.
replace TOT_FAT_DASH_BR=1 if TOT_FAT_DASH <=27 
replace TOT_FAT_DASH_BR=0.5 if (TOT_FAT_DASH >27 & TOT_FAT_DASH<=32)
replace TOT_FAT_DASH_BR=0 if (TOT_FAT_DASH >32 & TOT_FAT_DASH ~=.)

tab TOT_FAT_DASH_BR, missing


/*Protein -- grams*/

summ DR1TPROT
summ DR2TPROT 

capture drop DR12TPROT
gen DR12TPROT=(DR1TPROT+DR2TPROT)/2

save, replace

capture drop PROT_DASH
gen PROT_DASH=.
replace PROT_DASH=(DR12TPROT*4)/DR12TKCAL*100

summ PROT_DASH

capture drop PROT_DASH_BR
gen PROT_DASH_BR=.
replace PROT_DASH_BR=1 if PROT_DASH >=18 & PROT_DASH~=.
replace PROT_DASH_BR=0.5 if (PROT_DASH <18 & PROT_DASH>=16.5)
replace PROT_DASH_BR=0 if (PROT_DASH <16.5 & PROT_DASH ~=.)

tab PROT_DASH_BR, missing


save, replace

/*Cholesterol -- mg*/

summ DR1TCHOL 
summ DR2TCHOL 

capture drop DR12TCHOL
gen DR12TCHOL=(DR1TCHOL+DR2TCHOL)/2



capture drop CHOL_DASH
gen CHOL_DASH=.
replace CHOL_DASH=(DR12TCHOL/DR12TKCAL)*1000

summ CHOL_DASH

capture drop CHOL_DASH_BR
gen CHOL_DASH_BR=.
replace CHOL_DASH_BR=1 if CHOL_DASH <=71.4 
replace CHOL_DASH_BR=0.5 if (CHOL_DASH >71.4 & CHOL_DASH<=107.1)
replace CHOL_DASH_BR=0 if (CHOL_DASH >107.1) & CHOL_DASH ~=.

tab CHOL_DASH_BR, missing

/*Fiber -- g*/

summ DR1TFIBE 
summ DR2TFIBE 


capture drop DR12TFIBE
gen DR12TFIBE=(DR1TFIBE+DR2TFIBE)/2


capture drop FIB_DASH
gen FIB_DASH=.
replace FIB_DASH=(DR12TFIBE/DR12TKCAL)*1000

summ FIB_DASH

capture drop FIB_DASH_BR
gen FIB_DASH_BR=.
replace FIB_DASH_BR=1 if FIB_DASH >=14.8 & FIB_DASH ~=.
replace FIB_DASH_BR=0.5 if (FIB_DASH <14.8 & FIB_DASH>=9.5)
replace FIB_DASH_BR=0 if (FIB_DASH <9.5 & FIB_DASH ~=.)

tab FIB_DASH_BR, missing

/*Magnesium -- mg*/

summ DR1TMAGN 
summ DR2TMAGN


capture drop DR12TMAGN
gen DR12TMAGN=(DR1TMAGN+DR2TMAGN)/2



capture drop MAG_DASH
gen MAG_DASH=.
replace MAG_DASH=(DR12TMAGN/DR12TKCAL)*1000

summ MAG_DASH

capture drop MAG_DASH_BR
gen MAG_DASH_BR=.
replace MAG_DASH_BR=1 if MAG_DASH >=238 & MAG_DASH ~=. 
replace MAG_DASH_BR=0.5 if (MAG_DASH <238 & MAG_DASH>=158)
replace MAG_DASH_BR=0 if (MAG_DASH <158) 

tab MAG_DASH_BR, missing

save, replace

/*Calcium -- mg*/

summ DR1TCALC 
summ DR2TCALC


capture drop DR12TCALC
gen DR12TCALC=(DR1TCALC+DR2TCALC)/2

capture drop CAL_DASH
gen CAL_DASH=.
replace CAL_DASH=(DR2TCALC/DR12TKCAL)*1000


capture drop CAL_DASH
gen CAL_DASH=.
replace CAL_DASH=(DR12TCALC/DR12TKCAL)*1000

summ CAL_DASH

capture drop CAL_DASH_BR
gen CAL_DASH_BR=.
replace CAL_DASH_BR=1 if CAL_DASH >=590 & CAL_DASH ~=.
replace CAL_DASH_BR=0.5 if (CAL_DASH <590 & CAL_DASH>=402)
replace CAL_DASH_BR=0 if (CAL_DASH <402) 

tab CAL_DASH_BR, missing


save, replace

/*Potassium -- mg*/

summ DR1TPOTA 
summ DR2TPOTA


capture drop DR12TPOTA
gen DR12TPOTA=(DR1TPOTA+DR2TPOTA)/2



capture drop POT_DASH
gen POT_DASH=.
replace POT_DASH=(DR12TPOTA*1000/DR12TKCAL)

summ POT_DASH

capture drop POT_DASH_BR
gen POT_DASH_BR=.
replace POT_DASH_BR=1 if POT_DASH >=2238 & POT_DASH ~=.
replace POT_DASH_BR=0.5 if (POT_DASH <2238 & POT_DASH>=1534)
replace POT_DASH_BR=0 if (POT_DASH <1534) 

tab POT_DASH_BR, missing
 

/*Sodium*/

summ DR1TSODI 
summ DR2TSODI 



capture drop DR12TSODI
gen DR12TSODI=(DR1TSODI+DR2TSODI)/2


capture drop SOD_DASH
gen SOD_DASH=.
replace SOD_DASH=(DR12TSODI/DR12TKCAL)*1000


summ SOD_DASH

capture drop SOD_DASH_BR
gen SOD_DASH_BR=.
replace SOD_DASH_BR=1 if SOD_DASH <=1143 
replace SOD_DASH_BR=0.5 if (SOD_DASH >1143 & SOD_DASH<=1286)
replace SOD_DASH_BR=0 if (SOD_DASH >1286) &  SOD_DASH ~=.  

tab SOD_DASH_BR, missing

/*DASH total score*/

capture drop DASH_TOTAL_SCORE
gen DASH_TOTAL_SCORE = SAT_FAT_DASH_BR+TOT_FAT_DASH_BR+PROT_DASH_BR+CHOL_DASH_BR+FIB_DASH_BR+MAG_DASH_BR+CAL_DASH_BR+POT_DASH_BR+SOD_DASH_BR

tab DASH_TOTAL_SCORE

capture drop DASH_TOTAL_SCORE_BR
gen DASH_TOTAL_SCORE_BR=.
replace DASH_TOTAL_SCORE_BR=1 if DASH_TOTAL_SCORE<4.5 
replace DASH_TOTAL_SCORE_BR=0 if DASH_TOTAL_SCORE>=4.5 & DASH_TOTAL_SCORE ~=.

tab DASH_TOTAL_SCORE_BR

save, replace 



****************************PHYSICAL ACTIVITY: MODERATE OR VIGOROUS, WORK OR RECREATIONAL*************************************************************

capture rename paq605-paq772c, upper

save, replace


tab PAQ605, missing
tab PAQ620, missing
tab PAQ635, missing
tab PAQ650, missing
tab PAQ665, missing

capture drop PHYSICAL
gen PHYSICAL=.
replace PHYSICAL=1 if (PAQ605==1 | PAQ620==1 | PAQ650==1 | PAQ665==1 | PAQ635) 
replace PHYSICAL=0 if PAQ605==2 & PAQ620==2 & PAQ650==2 & PAQ665==2 & PAQ635==2

tab1 PAQ670 PAQ655 PAQ640 PAQ625 PAQ610

foreach x of varlist PAQ670 PAQ655 PAQ640 PAQ625 PAQ610 {
	gen `x'r=`x' if `x'~=77 & `x'~=99
}

save, replace


describe PAQ* PAD*


**PAD615          double  %10.0g                Minutes vigorous-intensity work
**PAD630          double  %10.0g                Minutes moderate-intensity work
**PAD645          double  %10.0g                Minutes walk/bicycle for transportation
**PAD660          double  %10.0g                Minutes vigorous recreational activities
**PAD675          double  %10.0g                Minutes moderate recreational activities
**PAD680          double  %10.0g                Minutes sedentary activity
**PAD733          double  %10.0g                Minutes play active video games

su PAD615 PAD630 PAD645 PAD660 PAD675 PAD680 PAD733

tab1 PAD615 PAD630 PAD645 PAD660 PAD675 PAD680 PAD733


foreach x of varlist PAD615 PAD630 PAD645 PAD660 PAD675 PAD680 PAD733 {
	capture drop `x'r
}


foreach x of varlist PAD615 PAD630 PAD645 PAD660 PAD675 PAD680 PAD733 {
	gen `x'r=`x' if `x'~=9999
}

save, replace

**PAQ605          double  %10.0g                Vigorous work activity
**PAQ610          double  %10.0g                Number of days vigorous work
**PAQ620          double  %10.0g                Moderate work activity
**PAQ625          double  %10.0g                Number of days moderate work
**PAQ635          double  %10.0g                Walk or bicycle
**PAQ640          double  %10.0g                Number of days walk or bicycle
**PAQ650          double  %10.0g                Vigorous recreational activities
**PAQ655          double  %10.0g                Days vigorous recreational activities
**PAQ665          double  %10.0g                Moderate recreational activities
**PAQ670          double  %10.0g                Days moderate recreational activities
**PAQ706          double  %10.0g                Days physically active at least 60 min.
**PAQ710          double  %10.0g                Hours watch TV or videos past 30 days
**PAQ715          double  %10.0g                Hours use computer past 30 days
**PAQ722          double  %10.0g                Any physical activities past 7 days


su PAQ605 PAQ610 PAQ620 PAQ625 PAQ635 PAQ640 PAQ650 PAQ655 PAQ665 PAQ670 PAQ706 PAQ710 PAQ715 PAQ722

foreach x of varlist PAQ605 PAQ610 PAQ620 PAQ625 PAQ635 PAQ640 PAQ650 PAQ655 PAQ665 PAQ670 PAQ706 PAQ710 PAQ715 PAQ722 {
	capture drop `x'r
}





foreach x of varlist PAQ605 PAQ610 PAQ620 PAQ625 PAQ635 PAQ640 PAQ650 PAQ655 PAQ665 PAQ670 PAQ706 PAQ710 PAQ715 PAQ722 {
	gen `x'r=`x' if `x'~=9 | `x'~=99
}

save, replace

*Walking MET-min/week - use median time of each category*
capture drop walkperday
gen walkperday = PAD645r
label var walkperday "walking minutes per day"

*Frequency of Walking (no days per week) - use median time of each category* 
capture drop walkperweek
gen walkperweek = PAQ640r
label var walkperweek "walking days per week"

*Calculate walking MET-min/week*
capture drop walkMETmin
gen walkMETmin = 3.3 * walkperday * walkperweek
label var walkMETmin "MET-min per week walking"

*Moderate exercise MET-min/week*
capture drop modperday
gen modperday = (PAD630r+PAD675r)
label var modperday "moderate exercise minutes per day"

*Frequency of moderate exercise (no days per week)*
capture drop modperweek
gen modperweek =  (PAQ625r+PAQ670r)
label var modperweek "moderate exercise days per week"

*Calculate moderate exercise MET-min/week*
capture drop modMETmin
gen modMETmin = 4.0 * modperday * modperweek
label var modMETmin "MET-min per week moderate exercise"

*Vigorous exercise MET-min/week*
capture drop vigperday
gen vigperday = (PAD615r+PAD660r)
label var vigperday "vigorous exercise minutes per day"

*Frequency of vigorous exercise (no days per week)*
capture drop vigperweek
gen vigperweek = (PAQ610r+PAQ655r)
label var vigperweek "vigorous exercise days per week"

*Calculate vigorous exercise MET-min/week*
capture drop vigMETmin
gen vigMETmin = 8.0 * vigperday * vigperweek
label var vigMETmin "MET-min per week vigorous exercise"


****Calculate TOTAL MET-min per week*******
capture drop METmin
order modMETmin, before(vigMETmin)
order walkMETmin, before(modMETmin)
egen METmin = rowtotal(walkMETmin-vigMETmin)
label var METmin "MET-min per week total exercise" // missing values exist - consider imputation


capture drop PHYSICAL_days_average
gen PHYSICAL_days_average=METmin

save, replace


****SELF-RATED HEALTH**
tab HSD010, missing 

capture drop SELF_RATED_HEALTH
gen SELF_RATED_HEALTH=. 
replace SELF_RATED_HEALTH=1 if HSD010>=1 & HSD010<=3
replace SELF_RATED_HEALTH=2 if HSD010>=4 & HSD010<=5

tab SELF_RATED_HEALTH, missing

**CO-MORBIDITY INDEX**
tab MCQ160B 
tab MCQ160C 
tab MCQ160D  
tab MCQ160E
tab MCQ160F 
tab MCQ220  

describe MCQ160B MCQ160C MCQ160D MCQ160E MCQ160F MCQ220

**MCQ160B         double  %10.0g                Ever told had congestive heart failure
**MCQ160C         double  %10.0g                Ever told you had coronary heart disease
**MCQ160D         double  %10.0g                Ever told you had angina/angina pectoris
**MCQ160E         double  %10.0g                Ever told you had heart attack
**MCQ160F         double  %10.0g                Ever told you had a stroke
**MCQ220          double  %10.0g                Ever told you had cancer or malignancy


capture drop CVD_CANCER_HISTORY
gen CVD_CANCER_HISTORY=.
replace CVD_CANCER_HISTORY=1 if (MCQ160B==1 | MCQ160C==1 | MCQ160D==1 | MCQ160E==1 | MCQ160F==1 | MCQ220==1) 
replace CVD_CANCER_HISTORY=0 if CVD_CANCER_HISTORY==.

tab CVD_CANCER_HISTORY


****************************BODY MASS INDEX***************************************************************


capture drop BMI
gen BMI=BMXBMI

su BMI
histogram BMI

save, replace


****************************SBP and DBP**********************************************************

capture drop SBP
gen SBP=(BPXSY1+BPXSY2+BPXSY3)/3 

capture drop DBP
gen DBP=(BPXDI1+BPXDI2+BPXDI3)/3

su SBP DBP

histogram SBP
histogram DBP

save, replace

**************************** TOTAL CHOLESTEROL, MMOL/L**************************************************

capture drop TOTALCHOLESTEROLSI
gen TOTALCHOLESTEROLSI=LBDTCSI


su TOTALCHOLESTEROLSI
histogram TOTALCHOLESTEROLSI

save, replace


***************************HBA1C**************************************************************

capture drop HBA1C
gen HBA1C=LBXGH

histogram HBA1C

save,replace

**************************URINARY ALBUMIN:CREATININE RATIO*************************************************************


capture drop ACR
gen ACR=URDACT if URDACT<1000

capture drop LnACR
gen LnACR=ln(ACR)

su LnACR
histogram LnACR

save, replace


*****************************NUTRITIONAL BIOMARKERS*******************************************************

**PLASMA 25(OH)D3, NMOL/L**

capture drop VitaminD_serum
gen VitaminD_serum=LBXVD3MS

save, replace

**RBC FOLATE, NMOL/L**

capture drop folate_RBCSI
gen folate_RBCSI=LBDRFOSI

su folate_RBCSI
histogram folate_RBCSI

save, replace


**PLASMA B-12, PMOL/L**
capture drop vitaminb12_serumsi
gen vitaminb12_serumsi=LBDB12

save, replace

capture log close
log using "E:\NHANES_NFL_MORTALITY_PAPER\OUTPUT\FIGURE1.smcl", replace



******************************STEP 11: SAMPLE SELECTION FLOWCHART AND 2-STAGE HECKMAN SELECTION: MONDAY************


**////////////GENERATE FINAL SAMPLE SELECTION VARIABLE/////////////////////
use  NHANES_NFL_MORTALITY_PAPER,clear

*************SAMPLE 20+********************

capture drop sample20plus
gen sample20plus=.
replace sample20plus=1 if AGE>=20 & AGE~=.
replace sample20plus=0 if sample20plus~=1 & AGE~=.


*************FINAL SAMPLE*****************
capture drop SAMPLE_FINAL
gen SAMPLE_FINAL=.
replace SAMPLE_FINAL=1 if LNNFL~=. 
replace SAMPLE_FINAL=0 if SAMPLE_FINAL~=1


tab SAMPLE_FINAL

save, replace


*************FINAL SAMPLE2, WITH COMPLETE COVARIATES*****


**GET FROM MED4WAY ANALYSIS**

save, replace



**///////////////CREATE INVERSE MILLS RATIO///////////////////////////////


xi:probit SAMPLE_FINAL AGE SEX i.RACE_ETHN if  RIDAGEYR>=20
capture drop p1
predict p1, xb

capture drop phi
capture drop caphi
capture drop invmills

gen phi=(1/sqrt(2*_pi))*exp(-(p1^2/2))

egen caphi=std(p1)

capture drop invmills
gen invmills=phi/caphi

save, replace


/*SURVEY COMMANDS*/

sum WTSSNH2Y 

svyset [pweight=WTSSNH2Y], strata(SDMVSTRA) psu(SDMVPSU)


save, replace

*******************AGE AT ALL-CAUSE DEATH****

capture drop AGE_DEATH
gen AGE_DEATH=AGE+PERMTH_EXM/12

save, replace


capture log close
log using "E:\NHANES_NFL_MORTALITY_PAPER\OUTPUT\IMPUTATION.smcl", replace


*****************************STEP 12: MULTIPLE IMPUTATION*************************************************


///////////////MI IMPUTE STEP/////////////////////////


**design variables, continuous: WTSSNH2Y

**design variables, multinomial: SDMVSTRA SDMVPSU

**Key effect modifier, binary: SEX

**Exogenous covariates, continuous: AGE 

**Exogenous variables, binary or multinomial: SEX  

**Exogenous covariates, ordinal: @@@@@

**Potential mediators/moderators, continuous: @@@@@

**Potential mediators/moderators, binary/multinomial: @@@@@@@ 

**Mortality, binary: MORTSTAT

**Time to event, continuous: permth_exm

**Exposure variables, continuous: LNNFL


use  NHANES_NFL_MORTALITY_PAPER, clear


sort SEQN

save, replace

sort SEQN


save finaldata_unimputed, replace

set matsize 11000

capture mi set, clear

mi set flong

capture mi svyset, clear

capture mi stset,clear


mi svyset SDMVPSU [pweight=WTSSNH2Y], strata(SDMVSTRA) vce(linearized) singleunit(missing)

mi stset AGE_DEATH  [pweight = WTSSNH2Y], failure(MORTSTAT==1) enter(AGE) id(SEQN) scale(1)




mi unregister AGE SEX RACE_ETHN MARRIED_LIVP HOUSEHOLDSIZE PIR EDUCATION SMOKE ALCOHOL DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average SELF_RATED_HEALTH CVD_CANCER_HISTORY BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi SEQN 

mi register imputed    MARRIED_LIVP HOUSEHOLDSIZE PIR EDUCATION SMOKE ALCOHOL DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average SELF_RATED_HEALTH CVD_CANCER_HISTORY BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi  

mi unregister  AGE SEX RACE_ETHN 

mi impute chained (mlogit) MARRIED_LIVP SMOKE ALCOHOL DRUG_USER_EVER CVD_CANCER_HISTORY (ologit) PIR  EDUCATION  HOUSEHOLDSIZE SELF_RATED_HEALTH (regress)    DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average  BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi=MORTSTAT AGE SEX i.RACE_ETHN  if AGE>=20 ,force augment noisily  add(5) rseed(1234) savetrace(tracefile, replace) 



save finaldata_imputed, replace

capture log close

log using "E:\NHANES_NFL_MORTALITY_PAPER\OUTPUT\TABLE1.smcl", replace


*****************************STEP 13: MAIN ANALYSES*******************************************************

use finaldata_imputed,clear


**************************TABLE 1**********************************

mi svyset SDMVPSU [pweight=WTSSNH2Y], strata(SDMVSTRA) vce(linearized) singleunit(missing)

**AGE SEX RACE_ETHN PIR MARRIED_LIVP HOUSEHOLDSIZE  EDUCATION SMOKE ALCOHOL DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average SELF_RATED_HEALTH CVD_CANCER_HISTORY BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi  

**LNNFL  LNNFLMEDIAN

**MORTSTAT

**AGE_DEATH**


******************OVERALL*************************

mi estimate: svy, subpop(SAMPLE_FINAL): prop SEX 
mi estimate: svy, subpop(SAMPLE_FINAL): mean AGE 
mi estimate: svy, subpop(SAMPLE_FINAL): prop RACE_ETHN
mi estimate: svy, subpop(SAMPLE_FINAL): prop MARRIED_LIVP
mi estimate: svy, subpop(SAMPLE_FINAL): mean HOUSEHOLDSIZE
mi estimate: svy, subpop(SAMPLE_FINAL): prop PIR
mi estimate: svy, subpop(SAMPLE_FINAL): prop EDUCATION
mi estimate: svy, subpop(SAMPLE_FINAL): prop SMOKE
mi estimate: svy, subpop(SAMPLE_FINAL): prop ALCOHOL
mi estimate: svy, subpop(SAMPLE_FINAL): prop DRUG_USER_EVER
mi estimate: svy, subpop(SAMPLE_FINAL): mean DR12TKCAL
mi estimate: svy, subpop(SAMPLE_FINAL): mean DASH_TOTAL_SCORE
mi estimate: svy, subpop(SAMPLE_FINAL): mean PHYSICAL_days_average
mi estimate: svy, subpop(SAMPLE_FINAL): prop SELF_RATED_HEALTH
mi estimate: svy, subpop(SAMPLE_FINAL): prop CVD_CANCER_HISTORY

foreach x of varlist BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi  LNNFL {
	mi estimate: svy, subpop(SAMPLE_FINAL): mean `x'
} 

mi estimate: svy, subpop(SAMPLE_FINAL): prop LNNFLMEDIAN

mi estimate: svy, subpop(SAMPLE_FINAL): prop MORTSTAT

mi estimate: svy, subpop(SAMPLE_FINAL): mean AGE_DEATH

save, replace


****************MEN*******************************

capture drop MEN_FINAL
gen MEN_FINAL=.
replace MEN_FINAL=1 if SAMPLE_FINAL==1 & SEX==1
replace MEN_FINAL=0 if MEN_FINAL~=1

save, replace

mi estimate: svy, subpop(MEN_FINAL): mean AGE 
mi estimate: svy, subpop(MEN_FINAL): prop RACE_ETHN
mi estimate: svy, subpop(MEN_FINAL): prop MARRIED_LIVP
mi estimate: svy, subpop(MEN_FINAL): mean HOUSEHOLDSIZE
mi estimate: svy, subpop(MEN_FINAL): prop PIR
mi estimate: svy, subpop(MEN_FINAL): prop EDUCATION
mi estimate: svy, subpop(MEN_FINAL): prop SMOKE
mi estimate: svy, subpop(MEN_FINAL): prop ALCOHOL
mi estimate: svy, subpop(MEN_FINAL): prop DRUG_USER_EVER
mi estimate: svy, subpop(MEN_FINAL): mean DR12TKCAL
mi estimate: svy, subpop(MEN_FINAL): mean DASH_TOTAL_SCORE
mi estimate: svy, subpop(MEN_FINAL): mean PHYSICAL_days_average
mi estimate: svy, subpop(MEN_FINAL): prop SELF_RATED_HEALTH
mi estimate: svy, subpop(MEN_FINAL): prop CVD_CANCER_HISTORY

foreach x of varlist BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi  LNNFL {
	mi estimate: svy, subpop(MEN_FINAL): mean `x'
} 

mi estimate: svy, subpop(MEN_FINAL): prop LNNFLMEDIAN

mi estimate: svy, subpop(MEN_FINAL): prop MORTSTAT

mi estimate: svy, subpop(MEN_FINAL): mean AGE_DEATH

save, replace



***************WOMEN*******************************


capture drop WOMEN_FINAL
gen WOMEN_FINAL=.
replace WOMEN_FINAL=1 if SAMPLE_FINAL==1 & SEX==2
replace WOMEN_FINAL=0 if WOMEN_FINAL~=1

save, replace

mi estimate: svy, subpop(WOMEN_FINAL): mean AGE 
mi estimate: svy, subpop(WOMEN_FINAL): prop RACE_ETHN
mi estimate: svy, subpop(WOMEN_FINAL): prop MARRIED_LIVP
mi estimate: svy, subpop(WOMEN_FINAL): mean HOUSEHOLDSIZE
mi estimate: svy, subpop(WOMEN_FINAL): prop PIR
mi estimate: svy, subpop(WOMEN_FINAL): prop EDUCATION
mi estimate: svy, subpop(WOMEN_FINAL): prop SMOKE
mi estimate: svy, subpop(WOMEN_FINAL): prop ALCOHOL
mi estimate: svy, subpop(WOMEN_FINAL): prop DRUG_USER_EVER
mi estimate: svy, subpop(WOMEN_FINAL): mean DR12TKCAL
mi estimate: svy, subpop(WOMEN_FINAL): mean DASH_TOTAL_SCORE
mi estimate: svy, subpop(WOMEN_FINAL): mean PHYSICAL_days_average
mi estimate: svy, subpop(WOMEN_FINAL): prop SELF_RATED_HEALTH
mi estimate: svy, subpop(WOMEN_FINAL): prop CVD_CANCER_HISTORY

foreach x of varlist BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi  LNNFL {
	mi estimate: svy, subpop(WOMEN_FINAL): mean `x'
} 

mi estimate: svy, subpop(WOMEN_FINAL): prop LNNFLMEDIAN

mi estimate: svy, subpop(WOMEN_FINAL): prop MORTSTAT

mi estimate: svy, subpop(WOMEN_FINAL): mean AGE_DEATH

save, replace



**************DIFFERENCE BY SEX********************
mi estimate: svy, subpop(SAMPLE_FINAL): reg AGE SEX
mi estimate: svy, subpop(SAMPLE_FINAL): mlogit RACE_ETHN SEX
mi estimate: svy, subpop(SAMPLE_FINAL): mlogit MARRIED_LIVP SEX
mi estimate: svy, subpop(SAMPLE_FINAL): reg HOUSEHOLDSIZE SEX
mi estimate: svy, subpop(SAMPLE_FINAL): mlogit PIR SEX
mi estimate: svy, subpop(SAMPLE_FINAL): mlogit EDUCATION SEX
mi estimate: svy, subpop(SAMPLE_FINAL): mlogit SMOKE SEX
mi estimate: svy, subpop(SAMPLE_FINAL): mlogit ALCOHOL SEX
mi estimate: svy, subpop(SAMPLE_FINAL): mlogit DRUG_USER_EVER SEX
mi estimate: svy, subpop(SAMPLE_FINAL): reg DR12TKCAL SEX
mi estimate: svy, subpop(SAMPLE_FINAL): reg DASH_TOTAL_SCORE SEX
mi estimate: svy, subpop(SAMPLE_FINAL): reg PHYSICAL_days_average SEX
mi estimate: svy, subpop(SAMPLE_FINAL): mlogit SELF_RATED_HEALTH SEX
mi estimate: svy, subpop(SAMPLE_FINAL): mlogit CVD_CANCER_HISTORY SEX

foreach x of varlist BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi  LNNFL {
	mi estimate: svy, subpop(SAMPLE_FINAL): reg `x' SEX
} 

mi estimate: svy, subpop(SAMPLE_FINAL): mlogit LNNFLMEDIAN SEX

mi estimate: svy, subpop(SAMPLE_FINAL): mlogit MORTSTAT SEX

mi estimate: svy, subpop(SAMPLE_FINAL): reg AGE_DEATH SEX

save, replace



capture log close

log using "E:\NHANES_NFL_MORTALITY_PAPER\OUTPUT\TABLE2.smcl", replace


use finaldata_imputed,clear


mi stset AGE_DEATH  [pweight = WTSSNH2Y], failure(MORTSTAT==1) enter(AGE) id(SEQN) scale(1)


su AGE if SAMPLE_FINAL==1 & _mi_m==0

capture drop AGEcenter
gen AGEcenter=.
replace AGEcenter=AGE-47


**************************TABLE 2**********************************

**********OVERALL*************************

**MARRIED_LIVP HOUSEHOLDSIZE  EDUCATION SMOKE ALCOHOL DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average SELF_RATED_HEALTH CVD_CANCER_HISTORY BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi  


//MODEL 1//

mi estimate, esampvaryok: svy, subpop(SAMPLE_FINAL): stcox ZLNNFL  SEX i.RACE_ETHN i.PIR invmills



//MODEL 2//
mi estimate, esampvaryok: svy, subpop(SAMPLE_FINAL): stcox ZLNNFL  SEX  i.RACE_ETHN i.PIR MARRIED_LIVP HOUSEHOLDSIZE  i.EDUCATION i.SMOKE i.ALCOHOL i.DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average i.SELF_RATED_HEALTH i.CVD_CANCER_HISTORY BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi invmills 


*********MEN*******************************


//MODEL 1//
mi estimate, esampvaryok: svy, subpop(MEN_FINAL): stcox ZLNNFL SEX i.RACE_ETHN i.PIR invmills



//MODEL 2//
mi estimate, esampvaryok: svy, subpop(MEN_FINAL): stcox ZLNNFL SEX  i.RACE_ETHN i.PIR MARRIED_LIVP HOUSEHOLDSIZE  i.EDUCATION i.SMOKE i.ALCOHOL i.DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average i.SELF_RATED_HEALTH i.CVD_CANCER_HISTORY BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi  invmills



*********WOMEN*******************************
//MODEL 1//
mi estimate, esampvaryok: svy, subpop(WOMEN_FINAL): stcox ZLNNFL SEX i.RACE_ETHN i.PIR invmills



//MODEL 2//
mi estimate, esampvaryok: svy, subpop(WOMEN_FINAL): stcox ZLNNFL SEX  i.RACE_ETHN i.PIR MARRIED_LIVP HOUSEHOLDSIZE  i.EDUCATION i.SMOKE i.ALCOHOL i.DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average i.SELF_RATED_HEALTH i.CVD_CANCER_HISTORY BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi  invmills






*******BY SEX********************************


//MODEL 1//

mi estimate, esampvaryok: svy, subpop(SAMPLE_FINAL): stcox c.ZLNNFL##SEX i.RACE_ETHN i.PIR invmills



//MODEL 2//
mi estimate, esampvaryok: svy, subpop(SAMPLE_FINAL): stcox c.ZLNNFL##SEX  i.RACE_ETHN i.PIR MARRIED_LIVP HOUSEHOLDSIZE  i.EDUCATION i.SMOKE i.ALCOHOL i.DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average i.SELF_RATED_HEALTH i.CVD_CANCER_HISTORY BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi  invmills


*******BY SEX AND AGE********************************


//MODEL 1//

mi estimate, esampvaryok: svy, subpop(SAMPLE_FINAL): stcox c.ZLNNFL##SEX##c.AGEcenter i.RACE_ETHN i.PIR invmills



//MODEL 2//
mi estimate, esampvaryok: svy, subpop(SAMPLE_FINAL): stcox c.ZLNNFL##SEX##c.AGEcenter  i.RACE_ETHN i.PIR MARRIED_LIVP HOUSEHOLDSIZE  i.EDUCATION i.SMOKE i.ALCOHOL i.DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average i.SELF_RATED_HEALTH i.CVD_CANCER_HISTORY BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi  invmills


*******BY AGE********************************


//MODEL 1//

mi estimate, esampvaryok: svy, subpop(SAMPLE_FINAL): stcox c.ZLNNFL##c.AGEcenter i.RACE_ETHN SEX i.PIR invmills



//MODEL 2//
mi estimate, esampvaryok: svy, subpop(SAMPLE_FINAL): stcox c.ZLNNFL##c.AGEcenter  i.RACE_ETHN SEX i.PIR MARRIED_LIVP HOUSEHOLDSIZE  i.EDUCATION i.SMOKE i.ALCOHOL i.DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average i.SELF_RATED_HEALTH i.CVD_CANCER_HISTORY BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi  invmills

*******BY AGE AMONG MEN********************************


//MODEL 1//

mi estimate, esampvaryok: svy, subpop(SAMPLE_FINAL): stcox c.ZLNNFL##c.AGEcenter i.RACE_ETHN i.PIR invmills if SEX==1



//MODEL 2//
mi estimate, esampvaryok: svy, subpop(SAMPLE_FINAL): stcox c.ZLNNFL##c.AGEcenter  i.RACE_ETHN i.PIR MARRIED_LIVP HOUSEHOLDSIZE  i.EDUCATION i.SMOKE i.ALCOHOL i.DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average i.SELF_RATED_HEALTH i.CVD_CANCER_HISTORY BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi  invmills if SEX==1


*******BY AGE AMONG WOMEN********************************


//MODEL 1//

mi estimate, esampvaryok: svy, subpop(SAMPLE_FINAL): stcox c.ZLNNFL##c.AGEcenter i.RACE_ETHN i.PIR invmills if SEX==2



//MODEL 2//
mi estimate, esampvaryok: svy, subpop(SAMPLE_FINAL): stcox c.ZLNNFL##c.AGEcenter  i.RACE_ETHN i.PIR MARRIED_LIVP HOUSEHOLDSIZE  i.EDUCATION i.SMOKE i.ALCOHOL i.DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average i.SELF_RATED_HEALTH i.CVD_CANCER_HISTORY BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi  invmills if SEX==2



capture log close

log using "E:\NHANES_NFL_MORTALITY_PAPER\OUTPUT\TABLE3.smcl", replace


use finaldata_imputed,clear


mi stset AGE_DEATH  [pweight = WTSSNH2Y], failure(MORTSTAT==1) enter(AGE) id(SEQN) scale(1)

capture drop RACE_ETHNg* PIRg* EDUCATIONg* SMOKEg* ALCOHOLg* 

tab RACE_ETHN,generate(RACE_ETHNg)

tab PIR, generate(PIRg)

tab EDUCATION, generate(EDUCATIONg)

tab SMOKE, generate(SMOKEg)

tab ALCOHOL, generate(ALCOHOLg)


save, replace

***************************TABLE 3: MODEL 2*********************************

*****************************OVERALL*******************************


*********BMI***********
capture drop zBMI
mi passive: egen zBMI=std(BMI) if SAMPLE_FINAL==1


mi estimate, cmdok esampvaryok: med4way ZLNNFL zBMI  SEX  RACE_ETHNg2 RACE_ETHNg3 RACE_ETHNg4 PIRg2 PIRg3 MARRIED_LIVP HOUSEHOLDSIZE EDUCATIONg2 EDUCATIONg3 EDUCATIONg4 EDUCATIONg5 SMOKEg2 SMOKEg3 ALCOHOLg2 DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average SELF_RATED_HEALTH CVD_CANCER_HISTORY SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi invmills if SAMPLE_FINAL==1 , a0(0) a1(1) m(0) yreg(cox) mreg(linear) 


*********SBP***********
capture drop zSBP
mi passive: egen zSBP=std(SBP) if SAMPLE_FINAL==1

mi estimate, cmdok: med4way ZLNNFL zSBP  SEX  RACE_ETHNg2 RACE_ETHNg3 RACE_ETHNg4 PIRg2 PIRg3 MARRIED_LIVP HOUSEHOLDSIZE  EDUCATIONg2 EDUCATIONg3 EDUCATIONg4 EDUCATIONg5 SMOKEg2 SMOKEg3 ALCOHOLg2 DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average SELF_RATED_HEALTH CVD_CANCER_HISTORY BMI DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi invmills if SAMPLE_FINAL==1 , a0(0) a1(1) m(0) yreg(cox) mreg(linear) 


********DBP***************
capture drop zDBP
mi passive: egen zDBP=std(DBP) if SAMPLE_FINAL==1

mi estimate, cmdok: med4way ZLNNFL zDBP  SEX  RACE_ETHNg2 RACE_ETHNg3 RACE_ETHNg4 PIRg2 PIRg3 MARRIED_LIVP HOUSEHOLDSIZE  EDUCATIONg2 EDUCATIONg3 EDUCATIONg4 EDUCATIONg5 SMOKEg2 SMOKEg3 ALCOHOLg2 DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average SELF_RATED_HEALTH CVD_CANCER_HISTORY BMI SBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi  invmills if SAMPLE_FINAL==1 , a0(0) a1(1) m(0) yreg(cox) mreg(linear) 



*******TOTAL CHOLESTEROL************


capture drop zTOTALCHOLESTEROLSI
mi passive: egen zTOTALCHOLESTEROLSIP=std(TOTALCHOLESTEROLSI) if SAMPLE_FINAL==1

mi estimate, cmdok: med4way ZLNNFL zTOTALCHOLESTEROLSI  SEX  RACE_ETHNg2 RACE_ETHNg3 RACE_ETHNg4 PIRg2 PIRg3 MARRIED_LIVP HOUSEHOLDSIZE  EDUCATIONg2 EDUCATIONg3 EDUCATIONg4 EDUCATIONg5 SMOKEg2 SMOKEg3 ALCOHOLg2 DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average SELF_RATED_HEALTH CVD_CANCER_HISTORY BMI SBP DBP HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi invmills if SAMPLE_FINAL==1 , a0(0) a1(1) m(0) yreg(cox) mreg(linear) 


**********HBA1C************************

capture drop zHBA1C
mi passive: egen zHBA1C=std(HBA1C) if SAMPLE_FINAL==1

mi estimate, cmdok: med4way ZLNNFL zHBA1C  SEX  RACE_ETHNg2 RACE_ETHNg3 RACE_ETHNg4  PIRg2 PIRg3 MARRIED_LIVP HOUSEHOLDSIZE  EDUCATIONg2 EDUCATIONg3 EDUCATIONg4 EDUCATIONg5 SMOKEg2 SMOKEg3 ALCOHOLg2 DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average SELF_RATED_HEALTH CVD_CANCER_HISTORY BMI SBP DBP TOTALCHOLESTEROLSI LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi invmills  if SAMPLE_FINAL==1 , a0(0) a1(1) m(0) yreg(cox) mreg(linear) 


**********ACR*************************


capture drop zLnACR
mi passive: egen zLnACR=std(LnACR) if SAMPLE_FINAL==1

mi estimate, cmdok: med4way ZLNNFL zLnACR  SEX  RACE_ETHNg2 RACE_ETHNg3 RACE_ETHNg4  PIRg2 PIRg3 MARRIED_LIVP HOUSEHOLDSIZE  EDUCATIONg2 EDUCATIONg3 EDUCATIONg4 EDUCATIONg5 SMOKEg2 SMOKEg3 ALCOHOLg2 DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average SELF_RATED_HEALTH CVD_CANCER_HISTORY BMI SBP DBP TOTALCHOLESTEROLSI HBA1C VitaminD_serum folate_RBCSI vitaminb12_serumsi invmills  if SAMPLE_FINAL==1 , a0(0) a1(1) m(0) yreg(cox) mreg(linear) 



***********VITAMIN D*******************


capture drop zVitaminD_serum
mi passive: egen zVitaminD_serum=std(VitaminD_serum) if SAMPLE_FINAL==1

mi estimate, cmdok: med4way ZLNNFL zVitaminD_serum  SEX  RACE_ETHNg2 RACE_ETHNg3 RACE_ETHNg4  PIRg2 PIRg3 MARRIED_LIVP HOUSEHOLDSIZE  EDUCATIONg2 EDUCATIONg3 EDUCATIONg4 EDUCATIONg5 SMOKEg2 SMOKEg3 ALCOHOLg2 DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average SELF_RATED_HEALTH CVD_CANCER_HISTORY BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR folate_RBCSI vitaminb12_serumsi invmills  if SAMPLE_FINAL==1 , a0(0) a1(1) m(0) yreg(cox) mreg(linear) 



************RBC folate**************


capture drop zfolate_RBCSI
mi passive: egen zfolate_RBCSI=std(folate_RBCSI) if SAMPLE_FINAL==1

mi estimate, cmdok: med4way ZLNNFL zfolate_RBCSI  SEX  RACE_ETHNg2 RACE_ETHNg3 RACE_ETHNg4  PIRg2 PIRg3 MARRIED_LIVP HOUSEHOLDSIZE  EDUCATIONg2 EDUCATIONg3 EDUCATIONg4 EDUCATIONg5 SMOKEg2 SMOKEg3 ALCOHOLg2 DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average SELF_RATED_HEALTH CVD_CANCER_HISTORY BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum vitaminb12_serumsi invmills  if SAMPLE_FINAL==1 , a0(0) a1(1) m(0) yreg(cox) mreg(linear) 


***********Vitamin B-12****************

capture drop zvitaminb12_serumsi
mi passive: egen zvitaminb12_serumsi=std(vitaminb12_serumsi) if SAMPLE_FINAL==1

mi estimate, cmdok: med4way ZLNNFL zvitaminb12_serumsi  SEX  RACE_ETHNg2 RACE_ETHNg3 RACE_ETHNg4  PIRg2 PIRg3 MARRIED_LIVP HOUSEHOLDSIZE  EDUCATIONg2 EDUCATIONg3 EDUCATIONg4 EDUCATIONg5 SMOKEg2 SMOKEg3 ALCOHOLg2 DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average SELF_RATED_HEALTH CVD_CANCER_HISTORY BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI  invmills  if SAMPLE_FINAL==1 , a0(0) a1(1) m(0) yreg(cox) mreg(linear) 


*************CO-MORBIDITY****************

mi estimate, cmdok: med4way ZLNNFL CVD_CANCER_HISTORY  SEX  RACE_ETHNg2 RACE_ETHNg3 RACE_ETHNg4  PIRg2 PIRg3 MARRIED_LIVP HOUSEHOLDSIZE  EDUCATIONg2 EDUCATIONg3 EDUCATIONg4 EDUCATIONg5 SMOKEg2 SMOKEg3 ALCOHOLg2 DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average SELF_RATED_HEALTH  BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi invmills  if SAMPLE_FINAL==1 , a0(0) a1(1) m(0) yreg(cox) mreg(logistic) 


************SELF-RATED HEALTH****************
capture drop SELF_RATED_HEALTHg
gen SELF_RATED_HEALTHg=SELF_RATED_HEALTH
replace SELF_RATED_HEALTHg=0 if SELF_RATED_HEALTH==1
replace SELF_RATED_HEALTHg=1 if SELF_RATED_HEALTH==2



mi estimate, cmdok: med4way ZLNNFL SELF_RATED_HEALTHg   SEX  RACE_ETHNg2 RACE_ETHNg3 RACE_ETHNg4  PIRg2 PIRg3 MARRIED_LIVP HOUSEHOLDSIZE  EDUCATIONg2 EDUCATIONg3 EDUCATIONg4 EDUCATIONg5 SMOKEg2 SMOKEg3 ALCOHOLg2 DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average CVD_CANCER_HISTORY  BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi invmills  if SAMPLE_FINAL==1 , a0(0) a1(1) m(0) yreg(cox) mreg(logistic) 


save finaldata_imputed, replace

capture log close

log using "E:\NHANES_NFL_MORTALITY_PAPER\OUTPUT\FIGURE2.smcl", replace


*****************************FIGURE 2: KAPLAN-MEIER CURVES, ABOVE AND BELOW MEDIAN NFL*********************************************************


use finaldata_imputed,clear

mi extract 0

save finaldata_unimputed, replace



svyset SDMVPSU [pweight=WTSSNH2Y], strata(SDMVSTRA) vce(linearized) singleunit(missing)

stset AGE_DEATH  [pweight = WTSSNH2Y] , failure(MORTSTAT==1) enter(AGE) id(SEQN) scale(1)


**OVERALL**
sts test LNNFLMEDIAN if SAMPLE_FINAL==1



sts graph if SAMPLE_FINAL==1, by(LNNFLMEDIAN) scheme(sj)
graph save "E:\NHANES_NFL_MORTALITY_PAPER\OUTPUT\FIGURE2A.gph",replace

**MEN*******************************

sts test LNNFLMEDIAN if SAMPLE_FINAL==1 & SEX==1



sts graph if SAMPLE_FINAL==1 & SEX==1, by(LNNFLMEDIAN) scheme(sj)
graph save "E:\NHANES_NFL_MORTALITY_PAPER\OUTPUT\FIGURE2B.gph",replace


**WOMEN*******************************


sts test LNNFLMEDIAN if SAMPLE_FINAL==1 & SEX==2



sts graph if SAMPLE_FINAL==1 & SEX==2, by(LNNFLMEDIAN) scheme(sj)
graph save "E:\NHANES_NFL_MORTALITY_PAPER\OUTPUT\FIGURE2C.gph",replace


***BAR GRAPH****

graph box LNNFL [pweight = WTSSNH2Y], over(LNNFLMEDIAN) over(SEX) scheme(sj)
graph save "E:\NHANES_NFL_MORTALITY_PAPER\OUTPUT\FIGURE2D.gph",replace

****GRAPH COMBINE***

graph combine "E:\NHANES_NFL_MORTALITY_PAPER\OUTPUT\FIGURE2A.gph"  "E:\NHANES_NFL_MORTALITY_PAPER\OUTPUT\FIGURE2B.gph"  "E:\NHANES_NFL_MORTALITY_PAPER\OUTPUT\FIGURE2C.gph"  ///
"E:\NHANES_NFL_MORTALITY_PAPER\OUTPUT\FIGURE2D.gph" 


graph save "E:\NHANES_NFL_MORTALITY_PAPER\OUTPUT\FIGURE2.gph",replace

save, replace

capture log close


***********************************************SUPPLEMENTARY TABLES******************************************************************

log using "E:\NHANES_NFL_MORTALITY_PAPER\OUTPUT\TABLES1.smcl", replace


use finaldata_imputed,clear


**************************TABLE S1**********************************

mi svyset SDMVPSU [pweight=WTSSNH2Y], strata(SDMVSTRA) vce(linearized) singleunit(missing)

**AGE SEX RACE_ETHN PIR MARRIED_LIVP HOUSEHOLDSIZE  EDUCATION SMOKE ALCOHOL DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average SELF_RATED_HEALTH CVD_CANCER_HISTORY BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi  

**LNNFL  LNNFLMEDIAN

**MORTSTAT

**AGE_DEATH**


capture drop LNNFLMEDIANBELOW
gen LNNFLMEDIANBELOW=.
replace LNNFLMEDIANBELOW=0 if LNNFLMEDIAN==2 & SAMPLE_FINAL==1
replace LNNFLMEDIANBELOW=1 if LNNFLMEDIAN==1 & SAMPLE_FINAL==1


capture drop LNNFLMEDIANABOVE
gen LNNFLMEDIANABOVE=.
replace LNNFLMEDIANABOVE=0 if LNNFLMEDIAN==1 & SAMPLE_FINAL==1
replace LNNFLMEDIANABOVE=1 if LNNFLMEDIAN==2 & SAMPLE_FINAL==1


****************BELOW MEDIAN LN NFL*******************************
save, replace

mi estimate: svy, subpop(LNNFLMEDIANBELOW): mean AGE 
mi estimate: svy, subpop(LNNFLMEDIANBELOW): prop SEX
mi estimate: svy, subpop(LNNFLMEDIANBELOW): prop RACE_ETHN
mi estimate: svy, subpop(LNNFLMEDIANBELOW): prop MARRIED_LIVP
mi estimate: svy, subpop(LNNFLMEDIANBELOW): mean HOUSEHOLDSIZE
mi estimate: svy, subpop(LNNFLMEDIANBELOW): prop PIR
mi estimate: svy, subpop(LNNFLMEDIANBELOW): prop EDUCATION
mi estimate: svy, subpop(LNNFLMEDIANBELOW): prop SMOKE
mi estimate: svy, subpop(LNNFLMEDIANBELOW): prop ALCOHOL
mi estimate: svy, subpop(LNNFLMEDIANBELOW): prop DRUG_USER_EVER
mi estimate: svy, subpop(LNNFLMEDIANBELOW): mean DR12TKCAL
mi estimate: svy, subpop(LNNFLMEDIANBELOW): mean DASH_TOTAL_SCORE
mi estimate: svy, subpop(LNNFLMEDIANBELOW): mean PHYSICAL_days_average
mi estimate: svy, subpop(LNNFLMEDIANBELOW): prop SELF_RATED_HEALTH
mi estimate: svy, subpop(LNNFLMEDIANBELOW): prop CVD_CANCER_HISTORY

foreach x of varlist BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi  LNNFL {
	mi estimate: svy, subpop(LNNFLMEDIANBELOW): mean `x'
} 

mi estimate: svy, subpop(LNNFLMEDIANBELOW): prop LNNFLMEDIAN

mi estimate: svy, subpop(LNNFLMEDIANBELOW): prop MORTSTAT


save, replace



***************ABOVE MEDIAN LNNFL*******************************

mi estimate: svy, subpop(LNNFLMEDIANABOVE): mean AGE
mi estimate: svy, subpop(LNNFLMEDIANABOVE): prop SEX
mi estimate: svy, subpop(LNNFLMEDIANABOVE): prop RACE_ETHN
mi estimate: svy, subpop(LNNFLMEDIANABOVE): prop MARRIED_LIVP
mi estimate: svy, subpop(LNNFLMEDIANABOVE): mean HOUSEHOLDSIZE
mi estimate: svy, subpop(LNNFLMEDIANABOVE): prop PIR
mi estimate: svy, subpop(LNNFLMEDIANABOVE): prop EDUCATION
mi estimate: svy, subpop(LNNFLMEDIANABOVE): prop SMOKE
mi estimate: svy, subpop(LNNFLMEDIANABOVE): prop ALCOHOL
mi estimate: svy, subpop(LNNFLMEDIANABOVE): prop DRUG_USER_EVER
mi estimate: svy, subpop(LNNFLMEDIANABOVE): mean DR12TKCAL
mi estimate: svy, subpop(LNNFLMEDIANABOVE): mean DASH_TOTAL_SCORE
mi estimate: svy, subpop(LNNFLMEDIANABOVE): mean PHYSICAL_days_average
mi estimate: svy, subpop(LNNFLMEDIANABOVE): prop SELF_RATED_HEALTH
mi estimate: svy, subpop(LNNFLMEDIANABOVE): prop CVD_CANCER_HISTORY

foreach x of varlist BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi  LNNFL {
	mi estimate: svy, subpop(LNNFLMEDIANABOVE): mean `x'
} 

mi estimate: svy, subpop(LNNFLMEDIANABOVE): prop LNNFLMEDIAN

mi estimate: svy, subpop(LNNFLMEDIANABOVE): prop MORTSTAT

save, replace



**************DIFFERENCE BY LNNFLMEDIAN********************
mi estimate: svy, subpop(SAMPLE_FINAL): reg AGE LNNFLMEDIAN
mi estimate: svy, subpop(SAMPLE_FINAL): mlogit SEX LNNFLMEDIAN
mi estimate: svy, subpop(SAMPLE_FINAL): mlogit RACE_ETHN LNNFLMEDIAN
mi estimate: svy, subpop(SAMPLE_FINAL): mlogit MARRIED_LIVP LNNFLMEDIAN
mi estimate: svy, subpop(SAMPLE_FINAL): reg HOUSEHOLDSIZE LNNFLMEDIAN
mi estimate: svy, subpop(SAMPLE_FINAL): mlogit PIR LNNFLMEDIAN
mi estimate: svy, subpop(SAMPLE_FINAL): mlogit EDUCATION LNNFLMEDIAN
mi estimate: svy, subpop(SAMPLE_FINAL): mlogit SMOKE LNNFLMEDIAN
mi estimate: svy, subpop(SAMPLE_FINAL): mlogit ALCOHOL LNNFLMEDIAN
mi estimate: svy, subpop(SAMPLE_FINAL): mlogit DRUG_USER_EVER LNNFLMEDIAN
mi estimate: svy, subpop(SAMPLE_FINAL): reg DR12TKCAL LNNFLMEDIAN
mi estimate: svy, subpop(SAMPLE_FINAL): reg DASH_TOTAL_SCORE LNNFLMEDIAN
mi estimate: svy, subpop(SAMPLE_FINAL): reg PHYSICAL_days_average LNNFLMEDIAN
mi estimate: svy, subpop(SAMPLE_FINAL): mlogit SELF_RATED_HEALTH LNNFLMEDIAN
mi estimate: svy, subpop(SAMPLE_FINAL): mlogit CVD_CANCER_HISTORY LNNFLMEDIAN

foreach x of varlist BMI SBP DBP TOTALCHOLESTEROLSI HBA1C LnACR VitaminD_serum folate_RBCSI vitaminb12_serumsi  LNNFL {
	mi estimate: svy, subpop(SAMPLE_FINAL): reg `x' LNNFLMEDIAN
} 

mi estimate: svy, subpop(SAMPLE_FINAL): mlogit MORTSTAT LNNFLMEDIAN

save, replace



capture log close





capture log close

log using "E:\NHANES_NFL_MORTALITY_PAPER\OUTPUT\TABLES2.smcl", replace

******************************SUPPLEMENTARY TABLE 2************************************

use finaldata_imputed,clear


**MODEL A**

foreach x of varlist zBMI zSBP zDBP zTOTALCHOLESTEROLSIP zHBA1C zLnACR zVitaminD_serum zfolate_RBCSI zvitaminb12_serumsi CVD_CANCER_HISTORY SELF_RATED_HEALTHg {
	mi estimate: stcox `x' SEX  AGE RACE_ETHNg2 RACE_ETHNg3 RACE_ETHNg4  PIRg2 PIRg3 MARRIED_LIVP HOUSEHOLDSIZE  EDUCATIONg2 EDUCATIONg3 EDUCATIONg4 EDUCATIONg5 SMOKEg2 SMOKEg3 ALCOHOLg2 DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average  invmills
}



**MODEL B**
foreach x of varlist zBMI zSBP zDBP zTOTALCHOLESTEROLSIP zHBA1C zLnACR zVitaminD_serum zfolate_RBCSI zvitaminb12_serumsi CVD_CANCER_HISTORY SELF_RATED_HEALTHg {
	mi estimate: stcox c.`x'##c.ZLNNFL AGE SEX  RACE_ETHNg2 RACE_ETHNg3 RACE_ETHNg4  PIRg2 PIRg3 MARRIED_LIVP HOUSEHOLDSIZE  EDUCATIONg2 EDUCATIONg3 EDUCATIONg4 EDUCATIONg5 SMOKEg2 SMOKEg3 ALCOHOLg2 DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average  invmills
}



capture drop *bin
foreach x of varlist zBMI zSBP zDBP zTOTALCHOLESTEROLSIP zHBA1C zLnACR zVitaminD_serum zfolate_RBCSI zvitaminb12_serumsi ZLNNFL {
	xtile `x'bin=`x', nq(2)
}





keep if ZLNNFLbin~=.
save finaldata_imputed_full, replace



foreach x of varlist zBMIbin zSBPbin zDBPbin zTOTALCHOLESTEROLSIPbin zHBA1Cbin zLnACRbin zVitaminD_serumbin zfolate_RBCSIbin zvitaminb12_serumsibin CVD_CANCER_HISTORY SELF_RATED_HEALTHg {
	mi xeq 1: reri stcox  ZLNNFLbin `x' SEX  AGE RACE_ETHNg2 RACE_ETHNg3 RACE_ETHNg4 PIRg2 PIRg3 MARRIED_LIVP HOUSEHOLDSIZE  EDUCATIONg2 EDUCATIONg3 EDUCATIONg4 EDUCATIONg5 SMOKEg2 SMOKEg3 ALCOHOLg2 DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average invmills 
}


foreach x of varlist zBMIbin zSBPbin zDBPbin zTOTALCHOLESTEROLSIPbin zHBA1Cbin zLnACRbin zVitaminD_serumbin zfolate_RBCSIbin zvitaminb12_serumsibin CVD_CANCER_HISTORY SELF_RATED_HEALTHg {
	mi xeq 2: reri stcox  ZLNNFLbin `x' SEX  AGE RACE_ETHNg2 RACE_ETHNg3 RACE_ETHNg4 PIRg2 PIRg3 MARRIED_LIVP HOUSEHOLDSIZE  EDUCATIONg2 EDUCATIONg3 EDUCATIONg4 EDUCATIONg5 SMOKEg2 SMOKEg3 ALCOHOLg2 DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average invmills 
}


foreach x of varlist zBMIbin zSBPbin zDBPbin zTOTALCHOLESTEROLSIPbin zHBA1Cbin zLnACRbin zVitaminD_serumbin zfolate_RBCSIbin zvitaminb12_serumsibin CVD_CANCER_HISTORY SELF_RATED_HEALTHg {
	mi xeq 3: reri stcox  ZLNNFLbin `x' SEX  AGE RACE_ETHNg2 RACE_ETHNg3 RACE_ETHNg4 PIRg2 PIRg3 MARRIED_LIVP HOUSEHOLDSIZE  EDUCATIONg2 EDUCATIONg3 EDUCATIONg4 EDUCATIONg5 SMOKEg2 SMOKEg3 ALCOHOLg2 DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average invmills 
}



foreach x of varlist zBMIbin zSBPbin zDBPbin zTOTALCHOLESTEROLSIPbin zHBA1Cbin zLnACRbin zVitaminD_serumbin zfolate_RBCSIbin zvitaminb12_serumsibin CVD_CANCER_HISTORY SELF_RATED_HEALTHg {
	mi xeq 4: reri stcox  ZLNNFLbin `x' SEX  AGE RACE_ETHNg2 RACE_ETHNg3 RACE_ETHNg4 PIRg2 PIRg3 MARRIED_LIVP HOUSEHOLDSIZE  EDUCATIONg2 EDUCATIONg3 EDUCATIONg4 EDUCATIONg5 SMOKEg2 SMOKEg3 ALCOHOLg2 DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average invmills 
}


foreach x of varlist zBMIbin zSBPbin zDBPbin zTOTALCHOLESTEROLSIPbin zHBA1Cbin zLnACRbin zVitaminD_serumbin zfolate_RBCSIbin zvitaminb12_serumsibin CVD_CANCER_HISTORY SELF_RATED_HEALTHg {
	mi xeq 5: reri stcox  ZLNNFLbin `x' SEX  AGE RACE_ETHNg2 RACE_ETHNg3 RACE_ETHNg4 PIRg2 PIRg3 MARRIED_LIVP HOUSEHOLDSIZE  EDUCATIONg2 EDUCATIONg3 EDUCATIONg4 EDUCATIONg5 SMOKEg2 SMOKEg3 ALCOHOLg2 DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average invmills 
}



save, replace


**MODEL C**

use finaldata_imputed,clear


foreach x of varlist zBMI zSBP zDBP zTOTALCHOLESTEROLSIP zHBA1C zLnACR zVitaminD_serum zfolate_RBCSI zvitaminb12_serumsi CVD_CANCER_HISTORY SELF_RATED_HEALTHg {
	mi estimate: reg ZLNNFL `x' AGE SEX  RACE_ETHNg2 RACE_ETHNg3 RACE_ETHNg4  PIRg2 PIRg3 MARRIED_LIVP HOUSEHOLDSIZE  EDUCATIONg2 EDUCATIONg3 EDUCATIONg4 EDUCATIONg5 SMOKEg2 SMOKEg3 ALCOHOLg2 DRUG_USER_EVER DR12TKCAL DASH_TOTAL_SCORE PHYSICAL_days_average  invmills
}


capture log close

