clear all
 cd "\\studfiles.campus.uvt.nl\files\home\home01\u271201\Desktop\"
 use "ZA7500_v5-0-0.dta(2)"
ssc install fre
ssc install outreg2 
ssc install mgen
net install spost13_ado, from(http://www.indiana.edu/~jslsoc/stata)
/*
Binomial Logistic Regression

We will be looking at determinants of being in paid employment.

We will use the following variables:

v244: Our dependent variable, measuring whether a person is in paid employment (consider 1-3 as paid and all else and unpaid; consider paid as a "success").
v243_r: Respondent's education.
v262_r: Father's education.
v225: Respondent's gender identity (binary).
Age: Respondent's age (again, 82 or older should be treated as missing).
We want you to:

Inspect all variables and recode them if it provides a better interpretation (up to you, but you should be consequent in your interpretation) and drop all observations which contain a missing value.
Regress the recoded v244 (whether a person has paid work) on v243_r (respondent's education), v262_r (father's education), age, and v225 (gender). Age should be treated as a continuous variable. Your analysis should be done step-by-step:
Model 1: v244 = v243_r
Model 2: v244 = v243_r + v262_r
Model 3: v244 = v243_r + v262_r + age + v225
Report only relevant parameters in tables. Do not forget to interpret the fit measures. Additionally, provide a substantive interpretation of your results in terms of odds.
Provide two tables where you show the effects of respondent's education and father's education on the probability of having paid work. You should use the last model for your calculations (also use non-significant coefficients):

Respondent's education: fix your other independent variables to their mean (for the categorical variables, just use the category with the biggest proportions). Show the probability of having paid work for a respondent of low, medium, and high education.
Father's education: proceed as above and show the probability of having paid work for a respondent whose father is of low, medium, and high education.
*/
fre country
keep if country==380

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

// Step 1: Load required datasets
fre v244
fre v243_r
fre v262_r
fre v225
fre age

/********************************************************************************
*****************************Recodification*************************************/
//v244: Our dependent variable, measuring whether a person is in paid employment (consider 1-3 as paid and all else and unpaid; consider paid as a "success").
fre v244
gen dep = v244
recode dep (1 2 3 = 1) (4 5 6 7 8 9 10 =0) (-10 =.)
label define dep 1 "Paid" 0 "Unpaid"
label values dep dep

// Display the frequency table for the recoded variable
fre dep

/********************************************************************************
/////////////////////////////EDUCATION/////////////////////////////////////////
********************************************************************************/

fre v243_r
gen edu = v243_r
recode edu (66 =.  )
label values edu LABN
fre edu

// Display the frequency table for the education variable
fre edu

/********************************************************************************
//////////////////////////Father's education////////////////////////////////////
********************************************************************************/

fre v262_r
gen edu_fa = v262_r
recode edu_fa (66 =.  )
label values edu_fa LABN
fre edu_fa

// Display the frequency table for father's education variable
fre edu_fa

/********************************************************************************
////////////////////////////GENDER//////////////////////////////////////////////
********************************************************************************/

fre v225
gen gend = v225
label values gend V225
fre gend

// Display the frequency table for gender variable
fre gend

/********************************************************************************
///////////////////////// Age //////////////////////////////////////////////////
********************************************************************************/

fre age
gen Age = age
recode Age (82 =.  )

egen nmiss=rowmiss(dep edu edu_fa gend Age)
	fre nmiss
	drop if nmiss!=0
	
	
* Recode Age into age_cent
egen Age_mean = mean(Age)
gen age_centered = Age - Age_mean
fre age_centered
fre Age_mean // 49.93756 years old
kdensity age_centered, normal
fre Age
sum  dep edu edu_fa gend age_centered
outreg2 using x.doc, replace 
corr dep edu edu_fa gend age_centered
/********************************************************************************
///////////////////////////Models///////////////////////////////////////////////
********************************************************************************/
// Logistic regression models
fre  dep edu edu_fa gend age_centered

logistic dep 
outreg2 using my.doc, replace ctitle(Model Null)  addstat(Pseudo R-squared, `e(r2_p)', Log-Likelihood Full Model, e(ll),  Chi-square test, e(chi2)) eform label
logistic dep ib2.edu
outreg2 using my.doc, append ctitle(Model 1)  addstat(Pseudo R-squared, `e(r2_p)', Log-Likelihood Full Model, e(ll),  Chi-square test, e(chi2)) eform label
logistic dep ib2.edu ib1.edu_fa
outreg2 using my.doc, append  ctitle(Model 2)  addstat(Pseudo R-squared, `e(r2_p)', Log-Likelihood Full Model, e(ll),  Chi-square test, e(chi2)) eform label
logistic dep ib2.edu ib1.edu_fa b1.gend c.age_centered
outreg2 using my.doc, append ctitle(Model 3)  addstat(Pseudo R-squared, `e(r2_p)', Log-Likelihood Full Model, e(ll),  Chi-square test, e(chi2)) eform label

/********************************************************************************
//////////////Probabilities/////////////////////////////////////////////////////
********************************************************************************/

// Extract probabilities using margins (It estimates the average probability for each level of the predictor variable, taking into consideration the distribution of other predictor variables in your dataset.)
ta dep edu, col chi2
logistic dep ib2.edu ib1.edu_fa b1.gend c.age_centered

margins b2.edu b1.edu_fa, post 
estimates store herearemargins
outreg2  herearemargins using awsd.doc, label
estimates store probit
margins, at(edu=(1(1)3)) 
margins, at(edu_fa=(1(1)3)) 


// Extracting the probabilities by hand (Using the logistic function to transform the linear combination of coefficients into probabilities) 
logit dep b2.edu b1.edu_fa b1.gend c.age_centered, coeflegend
	* Odds
	di exp((_b[_cons]) + (_b[1.edu])) // lower educated
	di exp((_b[_cons]) + (_b[1.edu] *0)) // medium educated
	di exp((_b[_cons]) + (_b[3.edu] *1)) // higher educated

	di exp((_b[_cons]) + (_b[2.edu_fa]*0)) // lower educated (father)
	di exp((_b[_cons]) + (_b[2.edu_fa])) // medium educated (father)
	di exp((_b[_cons]) + (_b[3.edu_fa])) // higher educated (father)


* Probabilities by hand. 
di "Probability (Lower Educated): " (exp(_b[_cons] + _b[1.edu]) / (1 + exp(_b[_cons] + _b[1.edu])))
di "Probability (Medium Educated): " (exp(_b[_cons] + _b[1.edu] * 0) / (1 + exp(_b[_cons] + _b[1.edu] * 0)))
di "Probability (Higher Educated): " (exp(_b[_cons] + _b[3.edu] * 1) / (1 + exp(_b[_cons] + _b[3.edu] * 1)))

di "Probability (Lower Educated Father): " (exp(_b[_cons] + _b[2.edu_fa] * 0) / (1 + exp(_b[_cons] + _b[2.edu_fa] * 0)))
di "Probability (Medium Educated Father): " (exp(_b[_cons] + _b[2.edu_fa]) / (1 + exp(_b[_cons] + _b[2.edu_fa])))
di "Probability (Higher Educated Father): " (exp(_b[_cons] + _b[3.edu_fa]) / (1 + exp(_b[_cons] + _b[3.edu_fa])))

* Probabilities
gen prob_low_edu = exp(_b[_cons] + _b[1.edu]) / (1 + exp(_b[_cons] + _b[1.edu]))
gen prob_med_edu = exp(_b[_cons] + _b[1.edu] * 0) / (1 + exp(_b[_cons] + _b[1.edu] * 0))
gen prob_high_edu = exp(_b[_cons] + _b[3.edu] * 1) / (1 + exp(_b[_cons] + _b[3.edu] * 1))

gen prob_low_edu_father = exp(_b[_cons] + _b[2.edu_fa] * 0) / (1 + exp(_b[_cons] + _b[2.edu_fa] * 0))
gen prob_med_edu_father = exp(_b[_cons] + _b[2.edu_fa]) / (1 + exp(_b[_cons] + _b[2.edu_fa]))
gen prob_high_edu_father = exp(_b[_cons] + _b[3.edu_fa]) / (1 + exp(_b[_cons] + _b[3.edu_fa]))

 *Create a table using outreg2 in Excel format
outreg2 using results_table.xls, replace excel


// Create and save probability plots
logistic dep ib2.edu ib1.edu_fa b1.gend c.age_centered
margins ib2.edu ib1.edu_fa

margins, at(edu=(1(1)3)) 
marginsplot, recast(line) recastci(rarea) title("Respondents' level of education, Predictive Margins with 95% CIs") plot1opts(lcolor(gs8)) ciopt(color(black%20)) xtitle("Level of education")  ytitle("Pr(Paid employment = 1)")  name(g1)


margins, at(edu_fa=(1(1)3))
marginsplot, recast(line) recastci(rarea) title("Respondents' father level of education, Predictive Margins with 95% CIs") plot1opts(lcolor(gs8)) ciopt(color(black%20)) xtitle("Level of education")  ytitle("Pr(Paid employment = 1)")  name(g2)

/*****************************************************************************////////////////////////////Logistic function/////////////////////////////////////////////

logit dep b2.edu b1.edu_fa b1.gend c.age_centered
margins, dydx(edu) at(edu=(1 2 3)) 
predict yhat
sum yhat
twoway function y=ln(x/(1-x)), range( .1332128    .8849657) xtitle("Probability") ytitle("Log odds")title("Logistic function")

////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////Plot differences in Probabilities/////////////////////////
///////////////////////////////////////////////////////////////////////////////////////
clear
input str20 category double calc_prob double hand_prob double std_err
"Individual Lower" 0.3023 0.3801 0.0171
"Individual Medium" 0.5338 0.6320 0.0157
"Individual Higher" 0.7136 0.7962 0.0250
"Father's Lower" 0.5017 0.6320 0.0115
"Father's Medium" 0.4335 0.5530 0.0246
"Father's Higher" 0.3443 0.4419 0.0408
end
encode category, generate(category_num)
gen lower_bound = calc_prob - std_err
gen upper_bound = calc_prob + std_err
gen category_num = _n


twoway  (scatter calc_prob category_num, mcolor(blue) msymbol(T)) ///
        (scatter hand_prob category_num, mcolor(red) msymbol(S)), ///
        legend(label(1 "Calculated Probabilities") label(2 "Manual Probabilities")) ///
        ytitle("Probability") xtitle("Category") ///
        xtick(1/6, valuelabel angle(45)) ///
        xlabel(1 "Individual Lower" 2 "Individual Medium" 3 "Individual Higher" ///
              4 "Father's Lower" 5 "Father's Medium" 6 "Father's Higher", angle(45)) ///
        graphregion(color(white)) plotregion(color(white))
/********************************************************************************
/////////////////////////////Second Part////////////////////////////////////////
********************************************************************************/
clear all
 use "G:\Mi unidad\Tilburg\Block 2\From theory to\Assignment 4\Part 1\ZA7500_v5-0-0.dta"


/*

*/
fre country
keep if country==380

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



* Recode v172 into vote
gen vote = .
replace vote = 1 if v172 == 3
replace vote = 2 if v172 == 2
replace vote = 3 if v172 == 1
label define vote 1 "never" 2 "usually" 3 "always"
label values vote vote
fre vote
 
* Recode v178 into tv
gen tv = .
replace tv = 3 if v178 == 1
replace tv = 2 if v178 == 2
replace tv = 1 if v178 == 3
replace tv = 0 if v178 == 4
label define tv11 0 "not at all often" 1 "not often" 2 "fairly often" 3 "very often"
label values tv tv11
fre tv
/********************************************************************************
/////////////////////////////EDUCATION/////////////////////////////////////////
********************************************************************************/

fre v243_r
gen edu = v243_r
recode edu (66 =.  )
label values edu LABN
fre edu
// Display the frequency table for the education variable
fre edu
/********************************************************************************
////////////////////////////GENDER//////////////////////////////////////////////
********************************************************************************/

fre v225
fre v225
	recode v225 (1=0 "Male") ///
				(2=1 "Female") ///
				(else=.) ///
				, gen(gndr)

// Display the frequency table for gender variable
fre gndr

/********************************************************************************
///////////////////////// Age //////////////////////////////////////////////////
********************************************************************************/

fre age
gen Age = age
recode Age (82 =.  )

egen nmiss=rowmiss(vote edu tv gndr Age)
	fre nmiss
	drop if nmiss!=0
	
	
* Recode ge into age_cent
egen Age_mean = mean(Age)
gen age_centered = Age - Age_mean
fre age_centered
fre Age_mean // 50.50336 years old
egen age_centered_mean = mean(age_centered)
fre age_centered_mean
fre vote edu tv gndr age_centered
sum vote edu tv gndr age_centered, detail
/********************************************************************************
////////////////////////Models//////////////////////////////////////////////////
********************************************************************************/
fre age_centered_mean
// Multinomial logistic regression
mlogit vote ib3.edu c.tv ib1.gndr c.age_centered, rrr baseoutcome(1) 

// Ordinal logistic regression
ologit vote , or 
outreg2 using my1.doc, replace ctitle(Null Model)  addstat(Pseudo R-squared, `e(r2_p)', Log-Likelihood Full Model, e(ll),  Chi-square test, e(chi2)) eform label
ologit vote ib3.edu , or
outreg2 using my1.doc,  replace ctitle(Model 1)  addstat(Pseudo R-squared, `e(r2_p)', Log-Likelihood Full Model, e(ll),  Chi-square test, e(chi2)) eform label
ologit vote ib3.edu c.tv , or
outreg2 using my1.doc, append ctitle(Model 2)  addstat(Pseudo R-squared, `e(r2_p)', Log-Likelihood Full Model, e(ll),  Chi-square test, e(chi2)) eform label
ologit vote ib3.edu c.tv ib1.gndr c.age_centered, or
outreg2 using my1.doc, append ctitle(Model 3)  addstat(Pseudo R-squared, `e(r2_p)', Log-Likelihood Full Model, e(ll),  Chi-square test, e(chi2)) eform label

// Marginal effects
ologit vote ib3.edu i.tv ib1.gndr c.age_centered, or
forvalues i = 1/3 {
  margins tv , at(edu = 3 gndr = 1 age_centered =  -1.23e-06) predict(outcome(`i'))
}
estimates store margins
outreg2 margins using awsad.doc, label

ologit vote ib3.edu i.tv ib1.gndr c.age_centered, or
forvalues i = 1/3 {
  margins tv , at(edu = 3 gndr = 1 age_centered =  -1.23e-06) predict(outcome(`i'))
}


////////////////////////////////////////////////////////////////////////////////
/////////////////////Compare Models/////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

ologit vote ib3.edu i.tv ib1.gndr Age_mean, coeflegend


// Fit the ordered logit model
ologit vote i.edu c.tv i.gndr c.age_centered, 
scalar m1 = e(ll)
mlogit vote i.edu c.tv i.gndr c.age_centered, baseoutcome(1) 
scalar m2 = e(ll)
di "chi2(2) = " 2*(m2-m1)
di "Prob > chi2 = "chi2tail(2, 2*(m2-m1))


ologit vote i.edu c.tv i.gndr c.age_centered, nolog
scalar ll_ologit = e(ll)
scalar minus2LL_ologit = -2 * ll_ologit
display "The -2 Log Likelihood (-2LL) for ologit is " minus2LL_ologit


mlogit vote i.edu c.tv i.gndr c.age_centered, baseoutcome(1) nolog
scalar ll_mlogit = e(ll)
scalar minus2LL_mlogit = -2 * ll_mlogit
display "The -2 Log Likelihood (-2LL) for mlogit is " minus2LL_mlogit

scalar chi2 = minus2LL_ologit - minus2LL_mlogit
display "Chi-square statistic for the difference between the models is " chi2

/*The -2 Log Likelihood (-2LL) for the Ordered Logistic Regression (ologit) model is 2397.401.
The -2 Log Likelihood (-2LL) for the Multinomial Logistic Regression (mlogit) model is 2392.1934.
The chi-square statistic for the difference between the models is 5.2076.
The critical value for a chi-square distribution with 5 degrees of freedom at the 0.05 significance level is approximately 11.0705.
Since the chi-square statistic (5.2076) is below the critical value (11.0705), we cannot reject the null hypothesis that the simpler model (ologit) is adequate. This indicates that the additional complexity of the multinomial model does not significantly improve the fit of the model to the data.

Therefore, based on these calculations and the principle of parsimony (preferring simpler models when possible), it would be advisable to choose the ordinal model over the multinomial model for this analysis*/


/********************************************************************************
//////////////Probabilities/////////////////////////////////////////////////////
********************************************************************************/

ologit vote ib3.edu c.tv ib1.gndr c.age_centered

//1 "never" 

* Calculating probabilities for Pr(Y=1|x)
dis "Probability for 'tv=0': " (1 / (1 + exp(-(-3.377961 - (.1283456 * 0))))) // Probability for 'not at all often'//.03299138
dis "Probability for 'tv=1': " (1 / (1 + exp(-(-3.377961 - (.1283456 * 1))))) // Probability for 'not often' .02913332
dis "Probability for 'tv=2': " (1 / (1 + exp(-(-3.377961 - (.1283456 * 2))))) // Probability for 'fairly often'
dis "Probability for 'tv=3': " (1 / (1 + exp(-(-3.377961 - (.1283456 * 3))))) // Probability for 'very often'

 //2 "usually"
* Calculating odds for Pr(Y<2|x)/Pr(Y=2|x) for each level of TV news influence
local cut1 -3.377961
local cut2 -1.583227
foreach tv of numlist 0/3 {
    local prob_y1 = 1 / (1 + exp(-(`cut1' - (.1283456 * `tv'))))
    local prob_y2 = 1 / (1 + exp(-(`cut2' - (.1283456 * `tv')))) - `prob_y1'
    dis "Odds for 'tv=`tv'': " `prob_y1' / `prob_y2'
}
* Calculating probabilities for Pr(Y=2|x)
foreach tv of numlist 0/3 {
    local prob_y1 = 1 / (1 + exp(-(`cut1' - (.1283456 * `tv'))))
    local prob_y2 = 1 / (1 + exp(-(`cut2' - (.1283456 * `tv')))) - `prob_y1'
    dis "Probability for 'tv=`tv'': " `prob_y2'
}


//3 "always"
*Calculating Odds (Pr(Y<3|x)/Pr(Y=3|x)) for each level of TV news influence
* Calculating odds for each level of TV news influence
foreach tv of numlist 0/3 {
    local prob_y1 = 1 / (1 + exp(-(`cut1' - (.1283456 * `tv'))))
    local prob_y2 = 1 / (1 + exp(-(`cut2' - (.1283456 * `tv')))) - `prob_y1'
    local prob_y3 = 1 - `prob_y1' - `prob_y2'
    dis "Odds for 'tv=`tv'': " (`prob_y1' + `prob_y2') / `prob_y3'
}

* Calculating probabilities (Pr(y=3|x)) for each level of TV news influence
* Here y=3 represents 'always vote'
foreach tv of numlist 0/3 {
    local prob_y1 = 1 / (1 + exp(-(`cut1' - (.1283456 * `tv'))))
    local prob_y2 = 1 / (1 + exp(-(`cut2' - (.1283456 * `tv')))) - `prob_y1'
    local prob_y3 = 1 - `prob_y1' - `prob_y2'
    dis "Probability for 'tv=`tv'': " `prob_y3'
}




////////////////////////////////////////////////////////////////////////////////
//////////////////////////Plot differences in Probabilities ////////////////////
////////////////////////////////////////////////////////////////////////////////
clear
input str20 outcome str20 tv_frequency calc_prob hand_prob category_num
"Never vote" "Not at All Often" 0.024084 0.03299138 1
"Never vote" "Not Often" .0347252 0.02913332 2
"Never vote" "Fairly Often" .0248965 0.02571443 3
"Never vote" "Very Often" .0212533 0.02268738 4
"Usually vote" "Not at All Often" .1056977 0.13734756 5
"Usually vote" "Not Often" .1438537 0.12382653 6
"Usually vote" "Fairly Often" .1087755 0.11134654 7
"Usually vote" "Very Often" .0947518 0.09988812 8
"Always vote" "Not at All Often" .8702183 0.82966105 9
"Always vote" "Not Often" .8214211 0.84704015 10
"Always vote" "Fairly Often" .8663279 0.86293903 11
"Always vote" "Very Often" .8839949 0.87742451 12
end

label define category 1 "Not at All Often (Never)" 2 "Not Often (Never)" 3 "Fairly Often (Never)" 4 "Very Often (Never)" ///
                    5 "Not at All Often (Usually)" 6 "Not Often (Usually)" 7 "Fairly Often (Usually)" 8 "Very Often (Usually)" ///
                    9 "Not at All Often (Always)" 10 "Not Often (Always)" 11 "Fairly Often (Always)" 12 "Very Often (Always)"
label values category_num category


twoway  (scatter calc_prob category_num, mcolor(blue) msymbol(Oh)) ///
        (scatter hand_prob category_num, mcolor(red) msymbol(Ox)), ///
        legend(label(1 "Calculated Probabilities") label(2 "Manual Probabilities")) ///
        ytitle("Probability") xtitle("Category") ///
        xtick(1/12, valuelabel angle(45)) ///
        xlabel(1 "Not at All Often (Never)" 2 "Not Often (Never)" 3 "Fairly Often (Never)" 4 "Very Often (Never)" ///
               5 "Not at All Often (Usually)" 6 "Not Often (Usually)" 7 "Fairly Often (Usually)" 8 "Very Often (Usually)" ///
               9 "Not at All Often (Always)" 10 "Not Often (Always)" 11 "Fairly Often (Always)" 12 "Very Often (Always)", angle(45))


//////////////////////////////////////////////////////////////////////////////
clear
input str20 TV_Category Outcome Margin Std_Err Lower_CI Upper_CI
"Not at all" 1 0.0240839 0.0060788 0.0121697 0.0359981
"Not often" 1 0.0347252 0.0069292 0.0211443 0.0483061
"Fairly often" 1 0.0248965 0.0048656 0.0153601 0.0344329
"Very often" 1 0.0212533 0.0048905 0.0116681 0.0308386
"Not at all" 2 0.1056977 0.0215263 0.0635069 0.1478885
"Not often" 2 0.1438537 0.0210757 0.1025461 0.1851613
"Fairly often" 2 0.1087755 0.0161705 0.0770819 0.1404692
"Very often" 2 0.0947518 0.0175214 0.0604104 0.1290931
"Not at all" 3 0.8702183 0.0269906 0.8173178 0.9231189
"Not often" 3 0.8214211 0.0268476 0.7688009 0.8740414
"Fairly often" 3 0.8663279 0.0201855 0.8267651 0.9058908
"Very often" 3 0.8839949 0.0218107 0.8412468 0.926743
end
label values category_num category

* Convert TV_Category to a numeric variable
encode TV_Category, gen(TV_Category_num)

* Verify the conversion
tabulate TV_Category_num, nolabel
tabulate TV_Category_num

* Update the plot command with the new numeric variable
twoway (scatter Margin TV_Category_num if Outcome==1, mcolor(blue) m(O)) ///
       (rcap Upper_CI Lower_CI TV_Category_num if Outcome== 1, color(blue)) ///
       (scatter Margin TV_Category_num if Outcome==2, mcolor(green) m(s)) ///
       (rcap Upper_CI Lower_CI TV_Category_num if Outcome== 2, color(green)) ///
       (scatter Margin TV_Category_num if Outcome== 3, mcolor(red) m(^)) ///
       (rcap Upper_CI Lower_CI TV_Category_num if Outcome==3, color(red)), ///
       legend(order( 2 "Never"  4 "Usually"  6 "Always")) ///
       ylabel(, format(%9.2f)) ///
       xlabel(, valuelabel) ///
       title("Predicted Probability by TV Watching Category and Outcome") ///
       ytitle("Predicted Probability (Margin)") ///
       xtitle("TV Watching Category")


