/*Replicates the tables and figures in the main text of "Brexit Uncertainty and its (Dis)Service Effects" 
Note that table 4 is presented at the end of the dofile because it uses a different data sample*/

clear 
set more off
capture log close

*Set directory paths
global path "/Users/florianburnat/Library/CloudStorage/Dropbox-Personal/Studies & Research/Universities/UK/Warwick/MRes Business & Management/My Modules/IB9ND Causal Inference/Assessment/Essay/173961-V1/AEJ repo revised/Results - Replication"
global main_output "/Users/florianburnat/Library/CloudStorage/Dropbox-Personal/Studies & Research/Universities/UK/Warwick/MRes Business & Management/My Modules/IB9ND Causal Inference/Assessment/Essay/173961-V1/AEJ repo revised/Results - Replication/main_tables_figures"

cd "$path"
log using main.log, replace
log using main.txt, replace text name(textlog)
use brexit_services_main_sample, clear 
gen all=1 if eu_minus_gb==1  /*EU*/ 
gen all_but_air=1 if all==1 & servicet~="Air transportation" /*EU w/out air*/

*Figure 1: UK Foreign Entry STRI: MFN, Preferential and Risk Measures
label define service 2 "Architect., engineer., scientific", modify
label define service 3 "Audiovisual and related", modify
label define service 2 "Architecture, engineering, scientific", modify
label define service 6 "Financial", modify
label define service 8 "Legal, accounting, consulting", modify
label define service 9 "Other transportation", modify
label define service 10 "Postal and courier", modify
label define service 12 "Telecommunications", modify

graph hbar (mean) mfn_risk_c1 (mean) stri_c1 (mean) eea_stri_c1 if importer=="GBR" & quarter==1 & all==1, over(S, sort(mfn_risk_c1) label(angle(horizontal) labsize(small))) bar(1, fintensity(inten40) lcolor(red)) bar(2, fcolor(dknavy)) bar(3, fcolor(none)) ytitle("") legend(order(1 "Risk" 2 "MFN TRI" 3 "Preferential TRI") nostack cols(1) position(1) ring(0)) 
graph export "$main_output/fig1.pdf", replace

*Figure 2: EU Foreign Entry STRI: Risk Measures
graph hbar (mean) mfn_risk_c1 (sd) mfn_risk_c1 if importer~="GBR" & quarter==1 & all==1, over(S, sort(mfn_risk_c1) label(angle(horizontal) labsize(small))) bar(1, fintensity(inten40) lcolor(red)) bar(2, lcolor(black) fcolor(none)) ytitle("") legend(order(1 "Risk Mean" 2 "Risk Standard Deviation") nostack cols(1) position(1) ring(0)) 
graph export "$main_output/fig2.pdf", replace

**Use only TABLE 2 sample for Figures 3 and 4 (OLS, STRI 1, me for brexit probability and all_but_air EU sample)
foreach y in log_value_cont pos_tr_non_d{
	foreach q in 12 {
		qui reghdfe `y' ln_mfn_risk_c1_x_me if quarter<=`q' & all_but_air==1, absorb(J#I#quarter J#I#S) vce(cluster J#S#quarter)  nocons
		gen `y'_sample=e(sample)
	}
}
save test.dta, replace

**A single loop is used to generate both Figure 3 and Figure 4
local k=3 
foreach y in log_value_cont pos_tr_non_d { 
**Figure 3 generated when y is log_value_cont in the loop 
**Figure 4 generated when y is pos_tr_non_d in the loop 

**Defining hi vs low based on continuing subsample
***VALUES***
	use test, clear
	keep if `y'_sample==1
	save del, replace
	keep J S mfn_risk_c1
	duplicates drop
	bysort J: egen med=median(mfn_risk_c1)
	gen hi=1 if mfn_risk_c1>med & mfn_risk_c1~=.
	replace hi=0 if mfn_risk_c1<=med & mfn_risk_c1~=.
	drop mfn_risk_c1
	sort J S 
	save class_hi, replace
	use del, clear
	sort J S
	merge J S using class_hi 
	gen initial= `y' if quarter ==1
	bysort I J S: egen dif=min(initial)
	replace dif= `y'-dif
	sum dif if quarter==1

**graphs 
	sum brexit_me
	gen brexit=brexit_me
	replace brexit=1 if brexit_me==. & quarter>5
	bysort quarter: egen mean_hi=mean(dif) if hi==1
	bysort quarter: egen mean_low=mean(dif) if hi==0
	sort quarter
		
	twoway  (line brexit quarter, lcolor(maroon)) (line mean_low quarter,lcolor(blue) lpattern(dash) yaxis(2)) (line mean_hi quarter if hi==1, lcolor(blue) yaxis(2)), ytitle("") xtitle("") xlabel(1 "16Q1" 2 "16Q2" 3 "16Q3" 4 "16Q4" 5 "17Q1" 6 "17Q2" 7 "17Q3" 8 "17Q4" 9 "18Q1" 10 "18Q2" 11 "18Q3" 12 "18Q4") xmtick(, valuelabel) legend(order(2 "Low" 3 "High") rows(2) rowgap(large) region(fcolor(%0) lcolor(none)) position(4) ring(0)) ytitle(Probability, color(maroon)) ytitle(Change vs. 2016Q1, color(blue) axis(2)) ylab(,nogrid) xline(2.5 5.5, lwidth(thin)) text(.9 3.5 "Referendum") text(.9 6 "Art. 50") 
	graph export "$main_output/fig`k'.pdf",replace

	local ++k
	erase del.dta
	erase class_hi.dta
	
	}
erase test.dta
drop _all
graph close 

**Main Tables
*Table 1: Regression Data Summary Statistics Bilateral-Industry in Quarters 2016Q1-2018Q4  and Table 2: Services Risk and EU-UK (OLS)
use brexit_services_main_sample, clear 

gen all=1 if eu_minus_gb==1  /*EU*/ 
gen all_but_air=1 if all==1 & servicet~="Air transportation" /*EU w/out air*/

*Table 1: Summary Statistics Bilateral-Industry in Quarters 2016Q1-2018Q4   
//Continously Traded Exports Sample
//Label Variables to match Table Output
label var log_value_cont "Exports (ln)" 
label var stri_c1 "STRI Category 1"
label var eea_stri_c1 "EEA STRI Category 1"
label var ln_mfn_risk_c1_x_me "Pr(Brexit) x Risk (ln)"
label var ln_brexit_me "Pr(Brexit) (ln)"
label var mfn_risk_c1 "Risk"
label var pos_tr_non_d "Exported" 

eststo clear
qui reghdfe log_value_cont ln_mfn_risk_c1_x_me if all_but_air==1, absorb(J#I#quarter J#I#S) vce(cluster J#S#quarter)  nocons
estpost tabstat log_value_cont stri_c1 eea_stri_c1 ln_mfn_risk_c1_x_me ln_brexit_me mfn_risk_c1 if e(sample),st(mean sd min max N) columns(statistics)
esttab using "$main_output/table1.txt", cells("mean(fmt(3)) sd(fmt(3)) min(fmt(3)) max(fmt(3)) count(fmt(0))") replace noobs label varwidth(25) nonumbers   
	
// Export Participation Sample
qui reghdfe pos_tr_non_d ln_mfn_risk_c1_x_me if all_but_air==1, absorb(J#I#quarter J#I#S) vce(cluster J#S#quarter)  nocons
estpost tabstat pos_tr_non_d stri_c1 eea_stri_c1 ln_mfn_risk_c1_x_me if e(sample),st(mean sd min max N) columns(statistics)
esttab using "$main_output/table1.txt", cells("mean(fmt(3)) sd(fmt(3)) min(fmt(3)) max(fmt(3)) count(fmt(0))") noobs label append varwidth(25) nonumbers
	
* Table 2: Services Risk and EU-UK (OLS)
foreach y in log_value_cont pos_tr_non_d {
	foreach q in 8 12 {
		reghdfe `y' ln_mfn_risk_c1_x_me if quarter<=`q' & all_but_air==1, absorb(J#I#quarter J#I#S) vce(cluster J#S#quarter)  nocons
		outreg2 using "$main_output/table2", se  excel ctitle(Sample: `y'_eu_q1_q`q', SE: cluster J#S#quarter, FE:J#I#quarter J#I#S ) addstat(R2, e(r2_a)) append dec(3) nocons nor2 
	}
}

*Table 3: Services Risk and EU-UK (PPML)
gen trade_value_ols=trade_value if log_value_cont!=. //Continously-traded sample used in OLS in Table 2

foreach y in trade_value_ols trade_value trade_imp  {
	foreach q in 8 12 {
		ppmlhdfe `y' ln_mfn_risk_c1_x_me  /*
					*/ if quarter<=`q' & all_but_air==1, absorb(J#I#quarter J#I#S) vce(cluster J#S#quarter)  nocons
		outreg2 using "$main_output/table3", se  excel ctitle(Sample: `y'_eu_q1_q`q', SE: cluster J#S#quarter, FE:J#I#quarter J#I#S ) addstat(R2, e(r2_p)) nocons
	}
}

*Table 5: Services Risk and EU-UK (IV)
foreach x in c1 total c2 c3 c4 c5 {
	bysort S: egen stri_`x'_iv=median(stri_`x') if eu_minus_gb~=1 & importer~="GBR" & year==2016 /*
*/	& (importer=="USA"|importer=="CAN"|importer=="AUS"|importer=="JPN")  
	
*	bysort S: egen stri_`x'_iv=median(stri_`x') if eu_minus_gb~=1 & importer~="GBR" & year==2016  
	bysort S: egen stri_`x'_iv_imp=median(stri_`x'_iv)
			
	
	reg stri_`x' stri_`x'_iv_imp if eu_minus_gb==1 & year==2016 & quarter==1
			 
	**first stage of relevant part of variable: high for c1 and c2, low for others so use only c1 as IV (c2 as robustness?)
	reg mfn_risk_`x' stri_`x'_iv_imp if eu_minus_gb==1 & year==2016 & quarter==1
}
			
** Construct the interactions with IV measure
foreach x in  c1 c2 /* total  c3 c4 c5*/ { 
	foreach br in  me  ma {
		gen pre_ln_mfn_risk_`x'_x_`br'_iv=stri_`x'_iv_imp*ln(pre_`br')
		gen post_ln_mfn_risk_`x'_x_`br'_iv=stri_`x'_iv_imp*ln(post_`br')
		gen ln_mfn_risk_`x'_x_`br'_iv=pre_ln_mfn_risk_`x'_x_`br'_iv+post_ln_mfn_risk_`x'_x_`br'_iv
	}
}

* Construct the instrument: STRI IV x ln(Brexit probability)
foreach var in ln_mfn_risk_c1_x_me_iv {
    capture drop `var'
}
gen ln_mfn_risk_c1_x_me_iv = stri_c1_iv_imp * ln_brexit_me


* Create cluster variable
egen cluster_var = group(J S quarter)

foreach y in log_value_cont pos_tr_non_d {  
    foreach q in 8 12 {
        capture noisily ivreghdfe `y' (ln_mfn_risk_c1_x_me=ln_mfn_risk_c1_x_me_iv) ///
            if quarter<=`q' & all_but_air==1, absorb(J#I#quarter J#I#S) cluster(cluster_var)
        
        * Only export results if estimation was successful
        if _rc == 0 {
            outreg2 using "$main_output/table5", se excel ctitle(Sample: `y' iv all_but_air==1 q`q') ///
                addstat(F Stat, e(widstat)) nocons dec(3) nor2
        }
    }
}

//Table 6: Brexit Impact at Average Risk
// (1) Collect the OLS and IV estimates for exports and participation, mean risk by sample and growth in Brexit prob
scalar drop _all
qui reghdfe log_value_cont ln_mfn_risk_c1_x_me if all_but_air==1, absorb(J#I#quarter J#I#S) vce(cluster J#S#quarter) nocons
scalar ols_export_coeff=e(b)[1,1]
sum mfn_risk_c1 if e(sample)==1
scalar export_risk=r(mean)

qui reghdfe pos_tr_non_d ln_mfn_risk_c1_x_me if all_but_air==1, absorb(J#I#quarter J#I#S) vce(cluster J#S#quarter)  nocons
scalar ols_part_coeff=e(b)[1,1]
sum mfn_risk_c1 if e(sample)==1
scalar part_risk=r(mean)
sum pos_tr_non_d if e(sample)==1
scalar part_mean=r(mean)

foreach q in 8 12 {
    count if quarter <= `q' & all_but_air == 1 & ln_mfn_risk_c1_x_me != . & ln_mfn_risk_c1_x_me_iv != .
}

// Robust safeguard to ensure IV regression didn't fail silently

capture noisily ivreghdfe log_value_cont (ln_mfn_risk_c1_x_me = ln_mfn_risk_c1_x_me_iv) ///
    if all_but_air == 1, absorb(J#I#quarter J#I#S) cluster(cluster_var)

if _rc == 0 {
    scalar iv_export_coeff = e(b)[1,1]
} 
else {
    di as error "⚠️ IV regression for exports failed — scalar not created."
}


// Robust safeguard to ensure IV regression didn't fail silently


capture noisily ivreghdfe pos_tr_non_d (ln_mfn_risk_c1_x_me = ln_mfn_risk_c1_x_me_iv) ///
    if all_but_air == 1, absorb(J#I#quarter J#I#S) cluster(cluster_var)

if _rc == 0 {
    scalar iv_part_coeff = e(b)[1,1]
} 
else {
    di as error "⚠️ IV regression for participation failed — scalar not created."
}





sum brexit_mean_q if quarter==1
scalar brexit_probq1=r(mean)

sum brexit_mean_q if quarter==3
scalar brexit_probq3=r(mean)

sum brexit_mean_q if quarter==6
scalar brexit_probq6=r(mean)

scalar ref_prob=ln(brexit_probq3/brexit_probq1)
scalar art50_prob=ln(brexit_probq6/brexit_probq1)

scalar list

// (2) Compute the uncertainty elasticty and Brexit effects
//Export
scalar ols_export_elas=ols_export_coeff*export_risk

scalar ols_export_ref=ols_export_elas*ref_prob*100

scalar ols_export_art50=ols_export_elas*art50_prob*100



capture confirm scalar iv_export_coeff
if _rc == 0 {
    scalar iv_export_elas = iv_export_coeff * export_risk
    scalar iv_export_ref = iv_export_elas * ref_prob * 100
    scalar iv_export_art50 = iv_export_elas * art50_prob * 100
} 
else {
    di as error "⚠️ iv_export_coeff not found — skipping IV export effects."
}



//Participation
scalar ols_part_elas=ols_part_coeff*part_risk/part_mean

scalar ols_part_ref=ols_part_elas*ref_prob*100

scalar ols_part_art50=ols_part_elas*art50_prob*100

capture confirm scalar iv_part_coeff
if _rc == 0 {
    scalar iv_part_elas = iv_part_coeff * part_risk / part_mean
    scalar iv_part_ref = iv_part_elas * ref_prob * 100
    scalar iv_part_art50 = iv_part_elas * art50_prob * 100
} 
else {
    di as error "⚠️ iv_part_coeff not found — skipping IV participation effects."
}


//Put values in a matrix
mat t6 = J(3, 4, .) 
mat t6[1,1] = ols_export_elas
mat t6[2,1] = ols_export_ref
mat t6[3,1] = ols_export_art50
mat t6[1,3] = ols_part_elas
mat t6[2,3] = ols_part_ref
mat t6[3,3] = ols_part_art50

* Check before inserting IV export values
capture confirm scalar iv_export_elas
if _rc == 0 {
    mat t6[1,2] = iv_export_elas
    mat t6[2,2] = iv_export_ref
    mat t6[3,2] = iv_export_art50
}

* Check before inserting IV participation values
capture confirm scalar iv_part_elas
if _rc == 0 {
    mat t6[1,4] = iv_part_elas
    mat t6[2,4] = iv_part_ref
    mat t6[3,4] = iv_part_art50
}


mat rownames t6="Elasticity" "Referendum" "Referendum + Art50" 
mat colnames t6="OLS Export" "IV Export" "OLS Participation" "IV Participation" 

mat list t6
mat2txt, matrix(t6) saving("$main_output/table6") title(Table 6: Brexit Uncertainty Impacts at Average Risk) format(%12.3f) replace

*Table 4: UK and EU Risk - Robustness to Industry Shocks in Extended Country Sample
use brexit_services_extended_sample, clear
keep exporter_country importer_country ln_mfn_risk_c1_x_me ebops quarter
generate exporter_paired=""
generate importer_paired=""
rename ln_mfn_risk_c1_x_me placebo_a_mfn_risk

*Pairs, no FTA agreements in 2016-Canada, Japan have since signed FTAs but need them to have sufficient observations to match
replace exporter_paired="USA" if exporter_country=="DEU"
replace exporter_paired="JPN" if exporter_country=="FRA"
replace exporter_paired="AUS" if exporter_country=="NLD"
replace exporter_paired="SAU" if exporter_country=="IRL"
replace exporter_paired="CAN" if exporter_country=="ITA"
replace exporter_paired="CHN" if exporter_country=="ESP"
replace exporter_paired="HKG" if exporter_country=="SWE"
replace exporter_paired="RUS" if exporter_country=="BEL"
replace exporter_paired="IND" if exporter_country=="LUX"
replace exporter_paired="BRA" if exporter_country=="DNK"
replace exporter_paired="MYS" if exporter_country=="POL"
replace exporter_paired="TWN" if exporter_country=="GRC"
replace exporter_paired="NZL" if exporter_country=="PRT"
replace exporter_paired="THA" if exporter_country=="FIN"
replace exporter_paired="PAK" if exporter_country=="AUT"
replace exporter_paired="IDN" if exporter_country=="CZE"
replace exporter_paired="ARG" if exporter_country=="HUN"
replace exporter_paired="PHL" if exporter_country=="SVK"
replace exporter_paired="IRN" if exporter_country=="LTU"
replace exporter_paired="VEN" if exporter_country=="LVA"
replace exporter_paired="URY" if exporter_country=="SVN"
replace exporter_paired="BLR" if exporter_country=="EST"

replace importer_paired="USA" if importer_country=="DEU"
replace importer_paired="JPN" if importer_country=="FRA"
replace importer_paired="AUS" if importer_country=="NLD"
replace importer_paired="SAU" if importer_country=="IRL"
replace importer_paired="CAN" if importer_country=="ITA"
replace importer_paired="CHN" if importer_country=="ESP"
replace importer_paired="HKG" if importer_country=="SWE"
replace importer_paired="RUS" if importer_country=="BEL"
replace importer_paired="IND" if importer_country=="LUX"
replace importer_paired="BRA" if importer_country=="DNK"
replace importer_paired="MYS" if importer_country=="POL"
replace importer_paired="TWN" if importer_country=="GRC"
replace importer_paired="NZL" if importer_country=="PRT"
replace importer_paired="THA" if importer_country=="FIN"
replace importer_paired="PAK" if importer_country=="AUT"
replace importer_paired="IDN" if importer_country=="CZE"
replace importer_paired="ARG" if importer_country=="HUN"
replace importer_paired="PHL" if importer_country=="SVK"
replace importer_paired="IRN" if importer_country=="LTU"
replace importer_paired="VEN" if importer_country=="LVA"
replace importer_paired="URY" if importer_country=="SVN"
replace importer_paired="BLR" if importer_country=="EST"

drop exporter_country importer_country
rename exporter_paired exporter_country
rename importer_paired importer_country
drop if importer_country=="" & exporter_country==""
save "placebo_a_noFTA.dta", replace
keep if exporter_country!=""
rename placebo_a_mfn_risk placebo_exp
save "placebo_a_noFTA_exp.dta", replace
use "placebo_a_noFTA.dta", clear
keep if importer_country!=""
 rename placebo_a_mfn_risk placebo_imp
save "placebo_a_noFTA_imp.dta", replace


use brexit_services_extended_sample, clear

gen all=1 if eu_minus_gb==1  /*EU*/ 
gen all_but_air=1 if all==1 & servicet~="Air transportation" /*EU w/out air*/
merge m:1 exporter_country ebops quarter using "placebo_a_noFTA_exp.dta"
drop _m
merge m:1 importer_country ebops quarter using "placebo_a_noFTA_imp.dta"

generate placebo_country= placebo_exp if placebo_exp!=.
replace placebo_country=placebo_imp if placebo_imp!=.

*extended sample, no industry x quarter FE
local methods "reghdfe reghdfe ppmlhdfe"
local trade_var "log_value_cont pos_tr_non_d trade_imp"
local r2_adj "a a p"

forval i=1/3{
	local hdfe `: word `i' of `methods''
	local y `:word `i' of `trade_var''
	local k `:word `i' of `r2_adj''


	`hdfe' `y' ln_mfn_risk_c1_x_me if quarter<=12 &  ebops!="3.2" & (placebo_country!=. | all==1), absorb(J#I#quarter J#I#S ) vce(cluster J#S#quarter)  nocons //service quarter fixed effects
	outreg2 using "$main_output/table4", se  excel ctitle(Sample: `y'_eu_q1_q12, SE: cluster J#S#quarter, FE:J#I#quarter J#I#S   ) dec(3) addstat(R2, e(r2_`k')) nocons nor2
	
	`hdfe' `y' ln_mfn_risk_c1_x_me if quarter<=12 &  ebops!="3.2" & (placebo_country!=. | all==1), absorb(J#I#quarter J#I#S S#quarter) vce(cluster J#S#quarter)  nocons //service quarter fixed effects
	outreg2 using "$main_output/table4", se  excel ctitle(Sample: `y'_eu_q1_q12, SE: cluster J#S#quarter, FE:J#I#quarter J#I#S  S*quarter ) dec(3) addstat(R2, e(r2_`k')) nocons nor2

}	

erase placebo_a_noFTA.dta
erase placebo_a_noFTA_exp.dta
erase placebo_a_noFTA_imp.dta

log close textlog
log close
