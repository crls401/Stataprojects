use "C:\Users\FX505\Dropbox\Mi PC (LAPTOP-ALE4TP00)\Desktop\Nueva carpeta\EPA_2022T4(1).dta"
append using "C:\Users\FX505\Dropbox\Mi PC (LAPTOP-ALE4TP00)\Desktop\Nueva carpeta\EPA_2022T3.dta", force 
append using "C:\Users\FX505\Dropbox\Mi PC (LAPTOP-ALE4TP00)\Desktop\Nueva carpeta\EPA_2022T2.dta", force 
append using "C:\Users\FX505\Dropbox\Mi PC (LAPTOP-ALE4TP00)\Desktop\Nueva carpeta\EPA_2022T1.dta", force 
append using "C:\Users\FX505\Dropbox\Mi PC (LAPTOP-ALE4TP00)\Desktop\Nueva carpeta\EPA_2021T4.dta", force 
append using "C:\Users\FX505\Dropbox\Mi PC (LAPTOP-ALE4TP00)\Desktop\Nueva carpeta\EPA_2021T3.dta", force 
append using "C:\Users\FX505\Dropbox\Mi PC (LAPTOP-ALE4TP00)\Desktop\Nueva carpeta\EPA_2021T2.dta", force 
append using "C:\Users\FX505\Dropbox\Mi PC (LAPTOP-ALE4TP00)\Desktop\Nueva carpeta\EPA_2021T1.dta", force 
append using "C:\Users\FX505\Dropbox\Mi PC (LAPTOP-ALE4TP00)\Desktop\Nueva carpeta\EPA_2020T4.dta", force 
append using "C:\Users\FX505\Dropbox\Mi PC (LAPTOP-ALE4TP00)\Desktop\Nueva carpeta\EPA_2020T3.dta", force 
append using "C:\Users\FX505\Dropbox\Mi PC (LAPTOP-ALE4TP00)\Desktop\Nueva carpeta\EPA_2020T2.dta", force 
append using "C:\Users\FX505\Dropbox\Mi PC (LAPTOP-ALE4TP00)\Desktop\Nueva carpeta\EPA_2020T1.dta", force 
append using "C:\Users\FX505\Dropbox\Mi PC (LAPTOP-ALE4TP00)\Desktop\Nueva carpeta\EPA_2019T4.dta", force 
append using "C:\Users\FX505\Dropbox\Mi PC (LAPTOP-ALE4TP00)\Desktop\Nueva carpeta\EPA_2019T3.dta", force 
append using "C:\Users\FX505\Dropbox\Mi PC (LAPTOP-ALE4TP00)\Desktop\Nueva carpeta\EPA_2019T2.dta", force 
append using "C:\Users\FX505\Dropbox\Mi PC (LAPTOP-ALE4TP00)\Desktop\Nueva carpeta\EPA_2019T1.dta", force 


//////////////////////////////////////////////////////////////////////////
/*change the label of CICLO*/
destring _all, replace force
//Generate ID variable//
egen id = seq(), from(1) to ()
 label define CICLO1 201 "2022Q4" 200 "2022Q3" 199 "2022Q2" 198 "2022Q1" 197 "2021Q4" 196 "2021Q3" 195 "2021Q2" 194 "2021Q1" 193 "2020Q4" 192 "2020Q3" 191 "2020Q2" 190 "2020Q1" 189 "2019Q4" 188 "2019Q3" 187 "2019Q2" 186 "2019Q1"
 
 label define DUCON1 1 "Indefinido" 6 "Temporal"
 label define NACIO 1 "Española" 2 "Española y doble nacionalidad" 3 "Extranjera" 
recode SEXO1 (6 = 0)
label define SEXO 1 "Male" 0 "Female"
label define ECIV1 1 "Soltero" 2 "Casado" 3	"Viudo" 4 "Separado o divorciado"
label define EDAD1 00	"0 a 4 años" 05	"5 a 9 años" 10	"10 a 15 años" 16	"16 a 19 años"  20	"20 a 24 años"  25	"25 a 29 años"  30	"30 a 34 años"  35	"35 a 39 años"  40	"40 a 44 años"  45	"45 a 49 años"  50	"50 a 54 años"  55	"55 a 59 años"  60	"60 a 64 años"  65	"65 o más años"     


label define CCAA 01	"Andalucía" 02	"Aragón" 03	"Asturias, Principado de" 04	"Balears, Illes" 05	"Canarias" 06	"Cantabria" 07	"Castilla y León" 08	"Castilla-La Mancha" 09	"Cataluña" 10	"Comunitat Valenciana" 11	"Extremadura" 12	"Galicia" 13	"Madrid, Comunidad de" 14	"Murcia, Región de" 15	"Navarra, Comunidad Foral de" 16	"País Vasco" 17	"Rioja, La" 51	"Ceuta" 52	"Melilla"

label define OCUP 0 "Desempleados"  1	"Ocupaciones militares"	2	"Directores y gerentes Y Dirección de las empresas y de las  Administraciones Públicas" 3	"Técnicos y Profesionales científicos e intelectuales" 	4 "Técnicos y Profesionales de apoyo" 	5	"Empleados de tipo administrativo" 6	"Trabajadores de servicios de restauración, personales, protección y vendedores de comercio" 7	"Trabajadores cualificados en el sector agrícola, ganadero, forestal y pesquero" 8	"Artesanos y trabajadores cualificados de las industrias manufactureras y la construcción"  9	"Operadores de instalaciones y maquinaria, y montadores" 10	"Ocupaciones elementales" 
label values CICLO CICLO // TIEMPO//
label values DUCON1 DUCON1 //Tiene contrato indefinido o temporal//
label values NAC1 NACIO    //Nacionalidad//
label values SEXO1 SEXO1 //"Sexo Todas las personas"//
label values ECIV1 ECIV1 //Estado civil legal//
label values EDAD1 EDAD1 //"Edad, grupos quinquenales de años cumplidos//
label values CCAA CCAA //Comunidades//
label values OCUP1 OCUP1 //Tipo de ocupación//


///////////////////////////////////////////////////////////////////////////////
////////////////////////// Difference in Difference////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/////////////////////////////Variables//////////////////////////////////////// 
 


///OUTCOME///
recode DUCON1(.=0) 
gen temporary = DUCON1
recode  temporary (1=.) 
gen permanent = DUCON1
recode  permanent (6=.) 
gen workers = DUCON1
recode  workers( 1 6 = 1)( 0 =.) 
////
recode OCUP1 0 = 1		1 = 2 	 2 = 3		3 = 4 	4 = 5	 5 = 6	 6 = 7	 7 = 8   8 = 9	 9 = 10 	
recode OCUP1 (.=0)
gen treated = inlist(OCUP1,  1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
br treated

gen time = CICLO
gen time1 = CICLO
recode time1 (197 196  195  194  193 192  191  190 189  188  187  186 = 0) (201  200  199  198 = 1), generate(post) 

gen policy = post*treated
br policy CICLO


egen MISS = rowmiss (temporary  treated post policy CICLO OCUP1 SEXO1 NAC1)

egen MISS1 = rowmiss ( permanent treated post policy CICLO OCUP1 SEXO1 NAC1)

br temporary permanent treated post policy CICLO OCUP1 SEXO1 NAC1 EDAD1
sum temporary permanent treated post policy CICLO OCUP1 SEXO1 NAC1 EDAD1
/////////////////////////////////////////////////////////////////////////////////////////////////////////Temporary////////////////////////////////////////////////////////////////////////////////////////////////////////////
reg temporary i.treated i.post i.policy i.CICLO 
margins i.CICLO, noestimcheck
marginsplot,  xlabel(, angle(45)) name (gr5) 

reg temporary i.policy i.CICLO i.OCUP1 , vce(cluster OCUP1)
bysort CICLO treated: egen mean_temporary = mean(temporary)


twoway line mean_temporary CICLO if treated == 1, sort lpattern(dash) ///
xline(197)


twoway line mean_temporary  CICLO if treated == 0, sort || ///
line mean_temporary  CICLO if treated == 1, sort lpattern(dash) ///
legend(label(1 "Control") label(2 "Treated")) ///
xline(197)

twoway line mean_temporary  CICLO if treated == 1, sort || ///
line mean_permanent  CICLO if treated == 1, sort lpattern(dash) ///
legend(label(1 "Temporary") label(2 "Permanent")) ///
xline(197)

didregress (temporary) (policy), group(OCUP1) time(CICLO)
estat trendplots, name(gr6)
estat  granger
estat grangerplot, 

didregress (temporary) (policy), group(OCUP1) time(CICLO)
////////////////////////////////////////////////////////////////////////////////////////////////////////Permanent///////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
reg permanent i.treated i.post i.policy i.CICLO 
margins CICLO#policy, noestimcheck
marginsplot,  xlabel(, angle(45)) name (gr4)


reg permanent i.treated i.post i.policy i.CICLO, vce(cluster OCUP1)

bysort CICLO treated: egen mean_permanent = mean(permanent)
graph twoway line mean_permanent CICLO if treated == 1, sort lpattern(dash) ///
xline(197)
graph twoway line mean_temporary CICLO if treated == 1, sort lpattern(dash) ///
xline(197)



twoway line mean_permanent CICLO if treated == 0, sort || 
line mean_permanent CICLO if treated == 1, sort lpattern(dash) ///
legend(label(1 "Control") label(2 "Treated")) ///
xline(197)


graph combine  gr5 gr4, 
xtset OCUP1
xtdidregress (permanent) (policy), group(OCUP1) time(CICLO)
estat trendplots, legendfrom(1)
estat ptrends
estat  granger
**# Bookmark #1
estat grangerplot, baseline(197)




///////////////////////////////////////////////////////////////////////////////
//////////////////Dinamic Difference in Difference/////////////////////////////
///////////////////////////////////////////////////////////////////////////////
xtreg permanent  i.treated i.post policy i.CICLO if MISS1==0 , fe i(OCUP1)
eststo model1 
xtreg temporary  i.treated i.post policy i.CICLO if MISS==0, fe i(OCUP1)
eststo model2
esttab, 
outreg2 [model1 model2 ] using mymod.doc, replace 
 
eststo clear


didregress (permanent ) (policy), group(OCUP1) time(CICLO) 
estat ptrends
estat  granger

didregress (temporary) (policy), group(OCUP1) time(CICLO) 
estat ptrends
estat  granger


didregress (permanent ) (policy), group(OCUP1) time(CICLO) 
estat grangerplot, name (gr10)
estat trendplots, name ( gr18)
didregress (temporary) (policy), group(OCUP1) time(CICLO)
estat grangerplot, name (gr9)
estat trendplots, name(gr6)




xtreg permanent  i.treated i.post policy i.CICLO if MISS1==0 , fe i(OCUP1)
eststo model1 
xtreg temporary  i.treated i.post policy i.CICLO if MISS==0, fe i(OCUP1)
eststo model2
xtreg permanent i.treated i.post  policy i.CICLO 0.SEXO1 25.EDAD1 30.EDAD1 35.EDAD1 3.NAC1 if MISS1==0, fe i(OCUP1)
eststo model3 
xtreg temporary i.treated i.post  policy i.CICLO 0.SEXO1 25.EDAD1 30.EDAD1 35.EDAD1 3.NAC1 if MISS==0, fe i(OCUP1)
eststo model4
esttab,
outreg2 [model1 model2 model3 model4 ] using mymod.doc, replace
eststo clear


fre SEXO1
recode SEXO1 (6 = 0)
0.SEXO1 6.OCUP1 25.EDAD1 30.EDAD1 35.EDAD1 3.NAC1



reg permanent i.CICLO 
estimates store D 
reg temporary   i.CICLO 
estimates store F
coefplot D F, vertical drop(_cons) xline (197)   xlabel(, angle(45)) 




reg permanent   i.CICLO 
estimates store D 
reg temporary   i.CICLO 
estimates store F
coefplot D, bylabel(Permanent) ///
|| F, bylabel(Temporary)  ///
||, drop(_cons) xline(0) yline (197) 




reg permanent   i.CICLO#6.OCUP1 
estimates store D 
reg temporary   i.CICLO#6.OCUP1 
estimates store F
coefplot D, bylabel(Permanent) ///
|| F, bylabel(Temporary)  ///
||, drop(_cons) xline(0)
drop _est_D _est_F 

reg permanent   i.CICLO#25.EDAD1 
estimates store D 
reg temporary   i.CICLO#25.EDAD1
estimates store F
coefplot D, bylabel(Permanent) ///
|| F, bylabel(Temporary)  ///
||, drop(_cons) xline(0) name(gr94)
drop _est_D _est_F 

reg permanent   i.CICLO#30.EDAD1 
estimates store D 
reg temporary   i.CICLO#30.EDAD1
estimates store F
coefplot D, bylabel(Permanent) ///
|| F, bylabel(Temporary)  ///
||, drop(_cons) xline(0) name(gr95)
drop _est_D _est_F  
 
reg permanent   i.CICLO#35.EDAD1 
estimates store D 
reg temporary   i.CICLO#35.EDAD1
estimates store F
coefplot D, bylabel(Permanent) ///
|| F, bylabel(Temporary)  ///
||, drop(_cons) xline(0)  name(gr96) 
drop _est_D _est_F 
Graph combine gr94 gr95 gr96





reg permanent   i.CICLO if EDAD1 == 25
estimates store A 
reg permanent   i.CICLO if EDAD1 == 30
estimates store B 
reg permanent   i.CICLO if EDAD1 == 35
estimates store C 
reg temporary   i.CICLO if EDAD1 == 25
estimates store D 
reg temporary   i.CICLO if EDAD1 == 30
estimates store E
reg temporary   i.CICLO if EDAD1 == 35
estimates store F
coefplot (A, label(EDAD1 = 25)) (B , label(EDAD1 = 30)) (C , label(EDAD1 == 35)), bylabel(Permanent)  ///
|| (D, label(EDAD1 = 25)) (E , label(EDAD1 = 30)) (F , label(EDAD1 == 35)), bylabel(Temporary) ///
||, vertical drop(_cons) xlabel(, angle(45))  norecycle legend(colfirst)


********************************************************************************
regress permanent i.CICLO if NAC1 == 3 
estimates store D 
regress temporary   i.CICLO if NAC1 == 3
estimates store F
coefplot (D, label(permanent) pstyle(p3))  ///
(F, label(temporary)  pstyle(p4))  ///
, drop(_cons) xline(0) msymbol(S) name (grz)
drop _est_D _est_F 


reg permanent   i.CICLO if NAC1 == 1
estimates store D 
reg permanent   i.CICLO if NAC1 == 3
estimates store C 
reg temporary   i.CICLO if NAC1 == 1
estimates store F
reg temporary   i.CICLO if NAC1 == 3
estimates store B
coefplot (D, label(NAC1 = 1)) (C , label(NAC1 = 3)), bylabel(Permanent)  ///
|| (F, label(NAC1 = 1)) (B , label(NAC1 = 3)), bylabel(Temporary) ///
||, vertical drop(_cons) xlabel(, angle(45))  norecycle legend(colfirst)

*********************************************************

reg permanent   i.CICLO if SEXO1 == 1
estimates store D 
reg permanent   i.CICLO if SEXO1 == 0
estimates store C 
reg temporary   i.CICLO if SEXO1 == 1
estimates store F
reg temporary   i.CICLO if SEXO1 == 0
estimates store B
coefplot (D, label(SEXO1 = 1)) (C , label(SEXO1 = 0)), bylabel(Permanent)  ///
|| (F, label(SEXO1 = 1)) (B , label(SEXO1 = 0)), bylabel(Temporary) ///
||, vertical drop(_cons) xlabel(, angle(45))  norecycle legend(colfirst)
 
drop _est_D _est_F 

/////////////////////////////////////////////////////////////////////////////////






