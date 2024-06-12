clear all
 cd "\\studfiles.campus.uvt.nl\files\home\home01\u271201\Desktop\"
 use "ZA7500_v5-0-0.dta(2)"
ssc install fre
ssc install outreg2 
ssc install mgen
ssc install estout
ssc install coefplot

/*
In this project, which will span both Multilevel Analysis, you will look into the subject of opinions about abortion. This is a topic that polarizes groups in society such as different generations, different professions (doctors versus psychologists), and different religious groups. With this assignment, you will study `which individual and contextual factors are associated with the justification of abortion?' You will use the European Values Study (2017)—using the same dataset as before. Use as many countries as possible (so, have a good look at the missing data, also you may want to include a missing indicator for any of the independent variables) . The central item for this assignment is the item (v154): "Do you justify: abortion?" 

Start your research by looking into the country variation in this variable in a table or figure and compute the intraclass correlation.
Subsequently, estimate random intercept models.
First look at differences explained by individual-level variables.
Start with including the variable v63 ("How important is God in your life?").
Then, include the following relevant control variables: education (v243_r, decide yourself whether you will use this variable as interval or categorical), socio-economic status (v246_ISEI), age (treat 82 and above as missing), gender (v225).
Secondly, turn your attention to the explanation in individual differences by country characteristics.
Include two country characteristics: post-materialism (v111_4) and secularization (v63, this is the same variable as the individual level indicator!). For post-materialism compute the percentage post-materialists in a country and for secularization the mean value of v63. Add these aggregated variables to your dataset, they will be used as country indicators.
So far, you have estimated random intercept models only. However, because you are interested if the effect of being religious differs across countries, employ a random slope model, allowing the effect of believing in God on abortion to vary over countries. You might expect that in more secularized countries, believing in God may influence your attitudes more compared to less secularized countries.
Finally, investigate whether the potential differences in the religiosity effect can be explained by the level of secularization in the country (a cross-level interaction).
 */
 
 fre country
* Define missing values for numeric variables
qui ds , has(type numeric)
foreach var in `r(varlist)' {
recode `var' (-1 = .a ) ///
(-2 = .b ) ///
(-3 = .c ) ///
(-4 = .d ) ///
(-5 = .e )
}

*  Define missing values for string variables
ds , has(type string)
foreach var in `r(varlist)' {
replace `var' = ".a" if `var' == "-1"
replace `var' = ".b" if `var' == "-2"
replace `var' = ".c" if `var' == "-3" 
replace `var' = ".d" if `var' == "-4" 
replace `var' = ".e" if `var' == "-5" 
}


* Attach missing value labels
qui ds , has(type numeric)
foreach var in `r(varlist)'  {
lab def `var' .a "Don't know" , modify
lab def `var' .b "No answer" , modify
lab def `var' .c "Not applicable" , modify
lab def `var' .d "Not asked in survey" , modify
lab def `var' .e "Missing: other" , modify
}
* Define missing values for numeric variables
qui ds , has(type numeric)
foreach var in `r(varlist)' {
recode `var' (-1 = .a ) ///
(-2 = .b ) ///
(-3 = .c ) ///
(-4 = .d ) ///
(-5 = .e ) 
}

*  Define missing values for string variables
ds , has(type string)
foreach var in `r(varlist)' {
replace `var' = ".a" if `var' == "-1"
replace `var' = ".b" if `var' == "-2"
replace `var' = ".c" if `var' == "-3" 
replace `var' = ".d" if `var' == "-4" 
replace `var' = ".e" if `var' == "-5" 
}


* Attach missing value labels
qui ds , has(type numeric)
foreach var in `r(varlist)'  {
lab def `var' .a "Don't know" , modify
lab def `var' .b "No answer" , modify
lab def `var' .c "Not applicable" , modify
lab def `var' .d "Not asked in survey" , modify
lab def `var' .e "Missing: other" , modify
}

/********************************************************************************
****************************Assignment 1****************************************
********************************************************************************/


************************* Variables*********************************************
fre country
fre v154
fre v63
fre v243_r
fre v246_ISEI
fre v225
fre v111_4
fre age

********************************************************************************
********************************************************************************
/********************************************************************************
///////////////////////////// IMPORTANCE OF GOD (V63) ///////////////////////////
********************************************************************************/

// Frequency of the original variable
fre v63

gen missing1=.
	replace missing1=0 if v63!=.a|v63!=.b
	replace missing1=1 if v63==.a|v63==.b
graph bar (mean) missing1, over(country, sort(1) descending label(angle(90) labsize(tiny))) tit("% Missing in believe in God") ytit("% missing") bar(1, color(blue)) 

// Creating a new variable for How important is God
gen imp_god = v63
label values  imp_god V63
// Display the frequency table for the importance of God variable
fre imp_god

******************************Secularization************************************
gen secu = 11 - v63
label define secu 1 "very important" 2 "9" 3 "8" 4 "7" 5 "6" 6 "5" 7 "4" 8 "3" 9 "2" 10 "not at all important"
label values  secu secu
tab secu imp_god
fre secu 
/********************************************************************************
//////////////////////////// EDUCATION (V243_R) /////////////////////////////////
********************************************************************************/

// Frequency of the original variable
bys country: fre v243_r 
	replace v243_r=.a if v243_r==66
gen missing2=.
	replace missing2=0 if v243_r!=.a|v243_r!=.b
	replace missing2=1 if v243_r==.a|v243_r==.b
graph bar (mean) missing2, over(country, sort(1) descending label(angle(90) labsize(tiny))) tit("% Missing in education") ytit("% missing") bar(1, color(red)) 

// Creating a new variable for education
gen edu = v243_r

// Recoding '66' (other) as missing
recode edu (66 = .)

// Labeling the new education variable
label values edu LABN

// Display the frequency table for the recoded education variable
fre edu

/********************************************************************************
////////////////// SOCIO-ECONOMIC STATUS (V246_ISEI) ////////////////////////////
********************************************************************************/

// Frequency of the original variable
fre v246_ISEI
bys country: fre v246_ISEI 
// Creating a new variable for socio-economic status
gen ses = v246_ISEI 
recode ses (-10 = .)

// Display the frequency table for the SES variable
bys ses: fre  country
tab  country ses
gen missing3=.
	replace missing3=0 if v246_ISEI!=.a|v246_ISEI!=.b | v246_ISEI==.c
	replace missing3=1 if v246_ISEI==.a|v246_ISEI==.b| v246_ISEI==.c
graph bar (mean) missing3, over(country, sort(1) descending label(angle(90) labsize(tiny))) tit("% Missing in ISEI") ytit("% missing") bar(1, color(purple)) yline(0.20, lc(red)) 

// There are too many missings values in these countries: Bosnia and Herzegovina , Azerbaijan, Albania, Armenia, Italy, North Macedonia, Montenegro, Romania and Serbia. But we will keep them to avoid the bias in the random effects by country. 

/********************************************************************************
///////////////////////////////// AGE ///////////////////////////////////////////
********************************************************************************/

// Frequency of the original variable
fre age

// Creating a new variable for age
gen Age = age

// Recoding age 82 and above as missing
recode Age (82/999 = .)

// Display the frequency table for the recoded age variable
fre Age
sum Age

/********************************************************************************
//////////////////////////////// GENDER (V225) //////////////////////////////////
********************************************************************************/

// Frequency of the original variable
fre v225

// Creating a new variable for gender
gen gend = v225

// Labeling the new gender variable
label values gend V225

// Display the frequency table for the gender variable
fre gend

/********************************************************************************
////////////////////////// DO YOU JUSTIFY ABORTION (V154) ///////////////////////
********************************************************************************/

// Frequency of the original variable
fre v154

gen missing=.
	replace missing=0 if v154!=.a|v154!=.b
	replace missing=1 if v154==.a|v154==.b
graph bar (mean) missing, over(country, sort(1) descending label(angle(90) labsize(tiny))) tit("% Missing in justify abortion") ytit("% missing") bar(1, color(green)) 

// Creating a new variable for opinions on abortion
gen abrt_justify = v154
label values abrt_justify V154
// Display the frequency table for the abortion justification variable
fre abrt_justify

/********************************************************************************
////////////////// POST-MATERIALIST INDEX (V111_4) //////////////////////////////
********************************************************************************/

// Frequency of the original variable
fre v111_4

// Creating a new variable for the post-materialist index
gen pmi = v111_4
label values pmi V111_4 
// Display the frequency table for the post-materialist index variable
fre pmi

/********************************************************************************
//////////////////  Country  //////////////////////////////////////////////////
********************************************************************************/
label define countrylabel 8 "AL" 31 "AZ" 40 "AT" 51 "AM" 70 "BA" 100 "BG" 112 "BY" 191 "HR" 203 "CZ" 208 "DK" 233 "EE" 246 "FI" 250 "FR" 268 "GE" 276 "DE" 348 "HU" 352 "IS" 380 "IT" 428 "LV" 440 "LT" 499 "ME" 528 "NL" 578 "NO" 616 "PL" 620 "PT" 642 "RO" 643 "RU" 688 "RS" 703 "SK" 705 "SI" 724 "ES" 752 "SE" 756 "CH" 804 "UA" 807 "MK" 826 "GB"
						  
label values country countrylabel

 ///////////////////////////////////////////////////////////////////////////////
 
egen nmiss=rowmiss(imp_god edu ses Age gend abrt_justify pmi country)
	fre nmiss
	drop if nmiss!=0

fre imp_god edu ses Age gend abrt_justify pmi country 
sum imp_god edu ses Age gend abrt_justify 
fre   abrt_justify


********************************************************************************
*****************************Centered Age***************************************
********************************************************************************
egen Age_mean = mean(Age)
gen cage = Age - Age_mean
sum Age_mean
fre cage 
/********************************************************************************
/////////////////////////COUNTRY VARIATION EXPLORATION///////////////////////////
********************************************************************************/

// Explore country variation for key variables
table  (country) (abrt_justify)
tabstat abrt_justify, by(country)


* Preparing the data
bysort country: egen mean_abrt_justify = mean(abrt_justify)
bysort country: egen mean_imp_god  = mean(imp_god )
bysort country: egen mean_secu = mean(secu)
*Calculate the percentage of post-materialists (v111_4) 
egen pct_postmat = mean(pmi == 3), by(country)

*Investigating the Interaction Between Religiosity and Country-Level Secularization

gen imp_god_sec = imp_god * mean_secu
gen imp_god_sec1 = imp_god * mean_imp_god

/********************************************************************************
////////////////////////////INTRACLASS CORRELATION///////////////////////////////
********************************************************************************/

// Calculate Intraclass Correlation for v63
xtset country 
mixed abrt_justify || country:
estat icc

*by hand
*Based on the numbers in the bottom table:.
*the residual (individual) variance =  7.621755 . This number represents the variation at the individual level.
*the country variance (var _cons) = 2.586529 . This number represents the variation at the country level.
*the ICC is calculated by: country variation (sigma2 u0) / (residual variation (sigma2 e0) + country variation (sigma2 u0)). 
dis 2.586529  / (2.586529  + 7.621755)
*ICC =  2.093291 / (2.093291 + 7.617326) = .25337549.
* The ICC is approximately 0.2533, which means that around 25.33% of the variation in attitudes towards abortion can be attributed to country-level differences, while the remaining 74.67% is due to individual-level variation.

/********************************************************************************
//////////////////////////RANDOM INTERCEPT MODELS////////////////////////////////
********************************************************************************/
//Null model 
mixed abrt_justify || country: , cov(un)
estat ic
est store m1
outreg2 using my.doc, replace ctitle(Model Null)
// Adding control variables one by one
mixed abrt_justify imp_god || country:, cov(un)
estat ic
est store m2
lrtest m1 m2 // LR chi2(1) = 4124.72 Prob > chi2 =  0.0000
outreg2 using my.doc, append ctitle(Model 1)
// Including education (treated as categorical)
mixed abrt_justify imp_god i.edu || country:, cov(un)
estat ic
est store m3
lrtest  m2 m3 // LR chi2(2) = 796.76  Prob > chi2 = 0.0000
outreg2 using my.doc, append ctitle(Model 2)
// Including socio-economic status
mixed abrt_justify imp_god i.edu c.ses || country:, cov(un)
estat ic
est store m4
lrtest  m3 m4 //  LR chi2(1) = 208.25 Prob > chi2 = 0.0000
outreg2 using my.doc, append ctitle(Model 3)
// Including age centered (with 82 and above treated as missing)
mixed abrt_justify imp_god i.edu c.ses c.cage || country:, cov(un)
estat ic
est store m5
lrtest  m4 m5  // LR chi2(1) =  36.39 Prob > chi2 = 0.0000
outreg2 using my.doc, append ctitle(Model 4)
// Including gender
mixed abrt_justify c.imp_god i.edu c.ses c.cage i.gend || country:, cov(un)
estat ic
est store m6
lrtest m5 m6 // LR chi2(1) = 204.73 Prob > chi2 = 0.0000
outreg2 using my.doc, append ctitle(Model 5)
/********************************************************************************
/////////////////////////INDIVIDUAL-LEVEL VARIABLES ANALYSIS/////////////////////
********************************************************************************/
//Aggregating Country-Level Variables:


*Random Intercept Model Including Country Characteristics (percentage of post-materialists):
mixed abrt_justify imp_god i.edu c.ses c.cage i.gend pct_postmat || country:, cov(un)
estat ic
est store m7
lrtest m6 m7 // LR chi2(1) =  13.75 Prob > chi2 = 0.0002
outreg2 using my.doc, append ctitle(Model 6)
*Random Intercept Model Including Country Characteristics (Secularization):
mixed abrt_justify imp_god i.edu c.ses c.cage i.gend pct_postmat mean_secu || country:, mle cov(un)
estat ic
est store m8
lrtest m7 m8 // LR chi2(1) =  30.95 Prob > chi2 = 0.0000
outreg2 using my.doc, append ctitle(Model 7)
*Random Slope Model for the Effect of Religiosity and percentage of post-materialists :
mixed abrt_justify imp_god i.edu c.ses c.cage i.gend pct_postmat mean_secu || country: imp_god, cov(un)
estat ic
est store m9
lrtest m8 m9 // LR chi2(1) =  360.07 Prob > chi2 = 0.0000
outreg2 using my.doc, append ctitle(Model 8)
********************************************************************************
//////////Considering Secularization's Influence on Religiosity Effect//////////
********************************************************************************

 
estat ic
est store m10
lrtest m9 m10 // LR chi2(1) =  6.55 Prob > chi2 = 0.0.0105
outreg2 using my.doc, append ctitle(Model 9)
esttab m1 m2 m6 m8 m9 m10, se ar2 

********************************************************************************
////////////////////////////Deviance////////////////////////////////////////////
********************************************************************************

*Model Null is already fitted
dis "Deviance for Model Null: " -2 * (-110651.33) //221302.66
*Model 1 is already fitted
dis "Deviance for Model 1: " -2 * (-108588.97) //217177.94
*Model 2 is already fitted
dis "Deviance for Model 2: " -2 * (-108190.59) //216381.18
*Model 3 is already fitted
dis "Deviance for Model 3: " -2 * (-108086.47) //216172.94
*Model 4 is already fitted
dis "Deviance for Model 4: " -2 * (-108068.27) //216136.54
*Model 5 is already fitted
dis "Deviance for Model 5: " -2 * (-107965.91) //215931.82
*Model 6 is already fitted
dis "Deviance for Model 6: " -2 * (-107959.03) //215918.06
*Model 7 is already fitted
dis "Deviance for Model 7: " -2 * (-107943.56) //215887.12
*Model 8 is already fitted
dis "Deviance for Model 8: " -2 * (-107763.52) //215527.04
*Model 9 is already fitted
dis"Deviance for Model 9: " -2 * (-107760.25) //215520.5

////////////////////////////////////////////////////////////////////////////////
////////////////////////////Visualizations//////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

/// Visualization between models. 
coefplot m1 m2 m6 m8 m9 m10, drop(_cons) coefl(imp_god="Believe in God" 2.edu="Medium educated" 3.edu="High educated" ses=SES gend=Female cage=Age c.pct_postmat=Post-materialism c.mean_secu=Secularization c.imp_god_sec="Cross-level interaction")


///Impotance of god
twoway (scatter  mean_abrt_justify mean_imp_god, mlabel(country) mlabposition(2) mlabcolor(gs10)  mlabposition(6) mlabsize(vsmall) mlabangle() msymbol(o) mcolor(gs10)) ///
       (lfit mean_imp_god mean_abrt_justify, lw(medium) lc(gs1)), legend(off) ytitle("Attitude towards Abortion") xtitle("Importance of God in Life")
	   
//Secularization level 	   
twoway (scatter mean_abrt_justify mean_secu, mlabel(country) mlabposition(2) mlabcolor(gs10)  mlabposition(6) mlabsize(vsmall) mlabangle() msymbol(o) mcolor(gs10)) ///
       (lfit mean_abrt_justify mean_secu, lw(medium) lc(gs1)), legend(off) ytitle("Attitude towards Abortion") xtitle("Level of secularization")


