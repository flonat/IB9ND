*Replicates the tables and figures in the appendix of "Brexit Uncertainty and its (Dis)Service Effects" 
*Note: A1 and A2 tables describe characteristics of the STRI. Table A9, A14 and A15 are presented at the end of dofile as they use different data samples than the main specifications.

clear 
set more off
capture log close

*Set directory paths
*global path "...\AEJrepo\Results"
*global appendix_output "...\AEJrepo\Results\appendix_tables_figures"

cd "$path"
log using appendix.log, replace
use brexit_services_main_sample, clear 
gen all=1 if eu_minus_gb==1  /*EU*/ 
gen all_but_air=1 if all==1 & servicet~="Air transportation" /*EU w/out air*/

*Table A3: Risk by Industry-UK and EU Services Trade 
bysort S: egen tot=sum( trade_value ) if all==1 & quarter==1
egen tot_a=sum(trade_value ) if all==1 & quarter==1
gen share_16Q1=tot/tot_a
label var mfn_risk_c1 "Risk"
label var share_16Q1 "Fraction of Trade (16Q1)" 

eststo clear
estpost tabstat share_16Q1 mfn_risk_c1 if all==1 & quarter==1, by(S) st(mean sd cv) nototal columns(statistics)
esttab using "$appendix_output/tableA3.txt", cells("mean sd cv") noobs varwidth(25) replace label

*Table A4: Services Risk and UK-EU Robustness Timing (Moving Average): 2016Q1-2018Q4
local methods "reghdfe reghdfe ppmlhdfe"
local trade_var "log_value_cont pos_tr_non_d trade_imp"
local r2_adj "a a p"
forval i=1/3{
	local hdfe `: word `i' of `methods''
	local y `:word `i' of `trade_var''
	local k `:word `i' of `r2_adj''

	`hdfe' `y' ln_mfn_risk_c1_x_ma if quarter<=12 & all_but_air==1, absorb(J#I#quarter J#I#S) vce(cluster J#S#quarter) nocons
	outreg2 using "$appendix_output/tableA4", se  excel ctitle(Sample: `y'_eu_q1_q12, SE: cluster J#S#quarter, FE:J#I#quarter J#I#S ) dec(3) addstat(R2, e(r2_`k')) nocons nor2
}

*Table A5: Services Risk and UK-EU Robustness to Other Barriers: 2016Q1-2018Q4
local methods "reghdfe reghdfe ppmlhdfe"
local trade_var "log_value_cont pos_tr_non_d trade_imp"
local r2_adj "a a p"
forval i=1/3{
	local x 
	local hdfe `: word `i' of `methods''
	local y `:word `i' of `trade_var''
	local k `:word `i' of `r2_adj''
	foreach risk in ln_mfn_risk_c1_x_me ln_mfn_risk_total_x_me { 
	    local x `x' `risk' 
		`hdfe' `y' `x' if quarter<=12 & all_but_air==1, absorb(J#I#quarter J#I#S) vce(cluster J#S#quarter)
		outreg2 using "$appendix_output/tableA5", se  excel ctitle(Sample: `y'_eu_q1_q12, SE: cluster J#S#quarter, FE:J#I#quarter J#I#S ) dec(3) addstat(R2, e(r2_`k')) nocons nor2
	}
}

*Table A6: Services Risk and UK-EU Robustness to Passporting Risk: 2016Q1-2018Q4
generate risk_passport=ln_brexit_me*passport_d
**about 60 percent of observations have no passport risk

local methods "reghdfe reghdfe ppmlhdfe"
local trade_var "log_value_cont pos_tr_non_d trade_imp"
local r2_adj "a a p"
forval i=1/3{
	local x 
	local hdfe `: word `i' of `methods''
	local y `:word `i' of `trade_var''
	local k `:word `i' of `r2_adj''

	`hdfe' `y' ln_mfn_risk_c1_x_me risk_passport if all_but_air==1, absorb(J#I#quarter J#I#S) vce(cluster J#S#quarter) nocons
	outreg2 using "$appendix_output/tableA6", se  excel ctitle(Sample: `y'_eu_q1_q12, SE: cluster J#S#quarter, FE:J#I#quarter J#I#S) append dec(3) addstat(R2, e(r2_`k')) nocons nor2 
}

*Table A7: UK and EU Risk - Robustness to Unobserved Correlation and Industry Trends: 2016Q1-2018Q4
local methods "reghdfe reghdfe ppmlhdfe"
local trade_var "log_value_cont pos_tr_non_d trade_imp"
local r2_adj "a a p"

forval i=1/3{
	local hdfe `: word `i' of `methods''
	local y `:word `i' of `trade_var''
	local k `:word `i' of `r2_adj''
	 `hdfe' `y' ln_mfn_risk_c1_x_me if quarter<=12 & all_but_air==1, absorb(J#I#quarter J#I#S) vce(cluster J#S#quarter S#quarter) nocons //2-way clustering
	outreg2 using "$appendix_output/tableA7", se  excel ctitle(Sample: `y'_eu_q1_q12, SE: cluster J#S#quarter S#quarter, FE:J#I#quarter J#I#S ) dec(3) addstat(R2, e(r2_`k')) nocons nor2	

	`hdfe' `y' ln_mfn_risk_c1_x_me if quarter<=12 & all_but_air==1, absorb(J#I#quarter J#I#S S#c.quarter) vce(cluster J#S#quarter)  nocons //service trend fixed effects
	outreg2 using "$appendix_output/tableA7", se  excel ctitle(Sample: `y'_eu_q1_q12, SE: cluster J#S#quarter, FE:J#I#quarter J#I#S  S*quarter trend) dec(3) addstat(R2, e(r2_`k')) nocons nor2
}		

*Table A8: UK and EU Risk - Robustness to Unobserved Sector-time Effects: 2016Q1-2018Q4
**Define broad sector**
gen broad_sec="Financial" if ebops=="6" | ebops=="7.1" 
replace broad_sec="Transport" if ebops=="3.1" | ebops=="3.2" | ebops=="3.3" | ebops=="3.4"
replace broad_sec="Professional" if ebops=="10.2.1" | ebops=="10.3.1" | ebops=="5"
replace broad_sec="Electronic" if ebops=="9.1" | ebops=="9.2" | ebops=="11.1"
egen G=group(broad_sec)

gen imp_eu_uk=importer_country if all==1 
replace imp_eu_uk="EU" if importer_country!="GBR"
egen imp_quarter=group(imp_eu_uk quarters) //Time effect with importer either EU or UK
gen uk_exp=1 if all==1 & exporter_country=="GBR" //UK exports
gen eu_exp=1 if all==1 & exporter_country!="GBR" //EU exports

local methods "reghdfe reghdfe ppmlhdfe"
local trade_var "log_value_cont pos_tr_non_d trade_imp"
local r2_adj "a a p"

forval i=1/3{
	local hdfe `: word `i' of `methods''
	local y `:word `i' of `trade_var''
	local k `:word `i' of `r2_adj''
	
	`hdfe' `y' ln_mfn_risk_c1_x_me if quarter<=12 & all_but_air==1, absorb(J#I#quarter J#I#S) vce(cluster J#S#quarter G#quarter) nocons //2-way clustering
	outreg2 using "$appendix_output/tableA8", se  excel ctitle(Sample: `y'_eu_q1_q12, SE: cluster J#S#quarter  G#quarter, FE:J#I#quarter J#I#S) dec(3) addstat(R2, e(r2_`k')) nocons nor2	
	
	`hdfe' `y' ln_mfn_risk_c1_x_me if quarter<=12 & all_but_air==1, absorb(J#I#quarter J#I#S G#quarter) vce(cluster J#S#quarter)  nocons //service trend fixed effects
	outreg2 using "$appendix_output/tableA8", se excel ctitle(Sample: `y'_eu_q1_q12, SE: cluster J#S#quarter, FE:J#I#quarter J#I#S G#quarter) dec(3) addstat(R2, e(r2_`k')) nocons nor2
}		

**Table A10: Baseline Robustness to Changes in Industry Sample: 2016Q1-2018Q4**
**Shorten service descriptions
replace servicetype="Arch Eng Other" if ebops=="10.3.1"
replace servicetype="Banking and Financial" if ebops=="7.1"
replace servicetype="Legal Acct Mgmt" if ebops=="10.2.1"
replace servicetype="Audiovisual" if ebops=="11.1"
replace servicetype="Other Transport" if ebops=="3.3"

levelsof servicetype if all_but_air==1, local(sec) 
foreach s of local sec { 
		reghdfe log_value_cont ln_mfn_risk_c1_x_me if servicetype!="`s'" & all_but_air==1, absorb(J#I#quarter J#I#S) vce(cluster J#S#quarter) nocons 
		outreg2 using "$appendix_output/tableA10", se  excel ctitle(Sector:"Remove:" "`s'") addstat(R2, e(r2_a))dec(3) nocons nor2
	}

**Add Air Sector to Main Sample
reghdfe log_value_cont ln_mfn_risk_c1_x_me if all==1, absorb(J#I#quarter J#I#S) vce(cluster J#S#quarter) nocons 
outreg2 using "$appendix_output/tableA10", se  excel ctitle(Sector:"Add: Air Transport") dec(3) nocons addstat(R2, e(r2_a)) nor2


** Table A11: Services Risk by Broad Sector: 2016Q1-2018Q4
local methods "reghdfe reghdfe ppmlhdfe"
local trade_var "log_value_cont pos_tr_non_d trade_imp"
local r2_adj "a a p"

foreach s in Electronic Financial Transport Professional {
   		forval i=1/3{
			local hdfe `: word `i' of `methods''
			local y `:word `i' of `trade_var''
			local k `:word `i' of `r2_adj''

			`hdfe' `y' ln_mfn_risk_c1_x_me if all_but_air==1 & broad_sec=="`s'", absorb(J#I#quarter J#I#S) vce(cluster J#S#quarter)  nocons
			sum ln_mfn_risk_c1_x_me if e(sample)==1
			outreg2 using "$appendix_output/tableA11", se  excel ctitle(Sector: `s', Sample: `y'_eu_q1_q`q', SE: cluster J#S#quarter, FE:J#I#quarter J#I#S) append dec(3) addstat(R2, e(r2_`k'), Std Dev (Risk), r(sd)) nocons nor2  
			}
	}

**Table A12: Services Risk and UK-EU Heterogeneity: 2016Q1-2018Q4**
foreach x in ln_mfn_risk_c1_x_me {
	gen `x'_gb=`x' if importer=="GBR" & eu_minus_gb==1
	replace `x'_gb=0 if importer~="GBR" & eu_minus_gb==1
}
		
**estimate differential effects for GB: not present in OLS, weak in IV, dependent on instruments? . 
local methods "reghdfe reghdfe ppmlhdfe"
local trade_var "log_value_cont pos_tr_non_d trade_imp"
local r2_adj "a a p"

forval i=1/3{
	local hdfe `: word `i' of `methods''
	local y `:word `i' of `trade_var''
	local k `:word `i' of `r2_adj''
	`hdfe' `y' ln_mfn_risk_c1_x_me ln_mfn_risk_c1_x_me_gb if quarter<=12 & all_but_air==1, absorb(J#I#quarter J#I#S) vce(cluster J#S#quarter) nocons 
	outreg2 using "$appendix_output/tableA12", se  excel ctitle(Sample: `y'_eu_q1_q12, SE: cluster J#S#quarter, FE:J#I#quarter J#I#S ) dec(3) addstat(R2, e(r2_`k')) nocons nor2
}		
	
*Figure A1: Sterling Depreciation 2016Q1-2018Q4
gen qdate = quarterly(quarters, "YQ")
replace brexit_me=1 if brexit_me==. & importer=="AUT" & ebops=="10.2.1"
corr exch brexit_me if importer=="AUT" & ebops=="10.2.1"
sort qdate
twoway (line exch qdate if importer_country=="AUT" & ebops=="10.2.1", lpattern(dash) lcolor(blue)) /*
		*/ (line brexit_me qdate if importer=="AUT" & ebops=="10.2.1", yaxis(2) lcolor(maroon)) /*
		*/, ytitle("Euro/Pound exchange rate", color(blue)) ylabel(,format(%5.2f) labcolor(blue)) ylabel(, labcolor(maroon) axis(2)) ytitle("Brexit Probablity", color(maroon) axis(2))/* 
		*/ xlabel(225 227 229 231 233 235,format(%tqYY!qq)) xtitle("") title("") legend(off)
graph export "$appendix_output/figA1.pdf",replace
graph close 

*Table A13: Robustness to Exchange Rate Changes with Heterogeneous Industry Passthrough
local methods "reghdfe reghdfe ppmlhdfe"
local trade_var "log_value_cont pos_tr_non_d trade_imp"
local r2_adj "a a p"

forval i=1/3{
	local x
	local hdfe `: word `i' of `methods''
	local y `:word `i' of `trade_var''
	local k `:word `i' of `r2_adj''
	
	foreach risk in ln_mfn_risk_c1_x_me ln_mfn_risk_c1_x_me_gb {
		local x `x' `risk' 


		`hdfe' `y' `x' if quarter<=12 & all_but_air==1, absorb(J#I#quarter J#I#S S#c.log_exch) vce(cluster J#S#quarter) nocons 
		outreg2 using "$appendix_output/tableA13", se  excel ctitle(Sample: `y'_eu_q1_q12, SE: cluster J#S#quarter, FE:J#I#quarter J#I#S S*log_exch) dec(3) addstat(R2, e(r2_`k')) append nocons nor2	
	}	
}

****Table A9 and A14 use broader sample of countries***
*Table A9: Services Risk Country Placebo Test: 2016Q1-2018Q4
use brexit_services_extended_sample.dta, clear
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


use brexit_services_extended_sample.dta, clear
gen all=1 if eu_minus_gb==1  /*EU*/ 
gen all_but_air=1 if all==1 & servicet~="Air transportation" /*EU w/out air*/
merge m:1 exporter_country ebops quarter using "placebo_a_noFTA_exp.dta"
drop _m
merge m:1 importer_country ebops quarter using "placebo_a_noFTA_imp.dta"

generate placebo_country= placebo_exp if placebo_exp!=.
replace placebo_country=placebo_imp if placebo_imp!=.
*Get rid of sea transport observations for countries where they're matched to landlocked countries-Placebo only*
replace placebo_country=. if ebops=="3.1" & (importer=="PAK" | importer=="IDN" | importer=="ARG" | importer=="IND" | importer=="PHL") 

foreach y in log_value_cont pos_tr_non_d {
		reghdfe `y'  placebo_country  if ebops!="3.2", absorb(J#I#quarter J#I#S) vce(cluster J#S#quarter)  nocons
		outreg2 using "$appendix_output/tableA9", se  excel ctitle(Sample: `y'_eu_q1_q`q', SE: cluster J#S#quarter, FE:J#I#quarter J#I#S ) append dec(3) addstat(R2, e(r2_a)) nocons nor2 
}
	ppmlhdfe log_value_cont placebo_country  if ebops!="3.2", absorb(J#I#quarter J#I#S) vce(cluster J#S#quarter)  nocons
		outreg2 using "$appendix_output/tableA9", se  excel ctitle(Sample: `y'_eu_q1_q`q', SE: cluster J#S#quarter, FE:J#I#quarter J#I#S, risk_`p') addstat(R2, e(r2_p)) nocons nor2

		
*Table A14: UK and EU Risk - IV Robustness to Industry-Time Effects in Extended Sample: 2016Q1-2018Q4

use brexit_services_extended_sample.dta, clear
gen all=1 if eu_minus_gb==1  /*EU*/ 
gen all_but_air=1 if all==1 & servicet~="Air transportation" /*EU w/out air*/
merge m:1 exporter_country ebops quarter using "placebo_a_noFTA_exp.dta"
drop _m
merge m:1 importer_country ebops quarter using "placebo_a_noFTA_imp.dta"

generate placebo_country= placebo_exp if placebo_exp!=.
replace placebo_country=placebo_imp if placebo_imp!=.

foreach x in c1 total c2 c3 c4 c5 {
	bysort S: egen stri_`x'_iv=median(stri_`x') if eu_minus_gb~=1 & importer~="GBR" & year==2016 /*
*/	& (importer=="USA"|importer=="CAN"|importer=="AUS"|importer=="JPN")


	bysort S: egen stri_`x'_iv_imp=median(stri_`x'_iv)
			
}

** Construct the interactions with IV measure
foreach x in  c1 c2 /* total  c3 c4 c5*/ { 
	foreach br in  me  ma {
		gen pre_ln_mfn_risk_`x'_x_`br'_iv=stri_`x'_iv_imp*ln(pre_`br')
		gen post_ln_mfn_risk_`x'_x_`br'_iv=stri_`x'_iv_imp*ln(post_`br')
		gen ln_mfn_risk_`x'_x_`br'_iv=pre_ln_mfn_risk_`x'_x_`br'_iv+post_ln_mfn_risk_`x'_x_`br'_iv
	}
}

replace ln_mfn_risk_c1_x_me_iv=0 if eu_minus_gb!=1
**IV regressions (IV using only c1 for stri, me for brexit probability and all_but_air EU sample)
egen cluster_var=group(J S quarter)
foreach y in log_value_cont pos_tr_non_d {  
	foreach q in  12 {
		ivreghdfe  `y' (ln_mfn_risk_c1_x_me=ln_mfn_risk_c1_x_me_iv) /*
		*/ if quarter<=`q' & servicet~="Air transportation" & (placebo_country!=. | all==1), first absorb(J#I#quarter J#I#S) cluster(cluster_var)
		outreg2 using "$appendix_output/tableA14", se  excel ctitle(Sample: `y' iv all_but_air==1 q`q') addstat(F Stat, e(widstat)) nor2
		ivreghdfe  `y' (ln_mfn_risk_c1_x_me=ln_mfn_risk_c1_x_me_iv) /*
		**/ if quarter<=`q' & servicet~="Air transportation" & (placebo_country!=. | all==1), first absorb(J#I#quarter J#I#S S#quarter) cluster(cluster_var)
		outreg2 using "$appendix_output/tableA14", se  excel ctitle(Sample: `y' iv all_but_air==1 q`q' sq FE) addstat(F Stat, e(widstat)) nor2
		}
} 

erase placebo_a_noFTA.dta
erase placebo_a_noFTA_exp.dta
erase placebo_a_noFTA_imp.dta

*Table A15: Gravity estimates with STRI (2014-16) 
* Estimated using USITC's ITPD-E data merged with OECD STRI
use itpd_gravity_stri,clear 
gen ln_trade=ln(trade)
gen ln_dist=ln(distance)
gen colony =colony_of_destination_ever+colony_of_origin_ever
gen gdp_o=ln(gdp_wdi_const_o)
gen gdp_d=ln(gdp_wdi_const_d)
rename (common_language agree_pta_services colony contiguity member_eu_joint) (lang rta_s clny cntg eu)

egen exp=group(exporter_iso3)
egen imp=group(importer_iso3)
rename prod_id prod

//EEA country dummy
foreach k in NOR ISL {
 replace eu=1 if exporter_iso3=="`k'" & member_eu_d==1
 replace eu=1 if importer_iso3=="`k'" & member_eu_o==1
}
 
//Use EEA STRIs for EU trade
replace stri_o=eea_stri_o if eu==1
replace stri_d=eea_stri_d if eu==1

forvalues i =1/3{
	replace stri`i'_d=eea_stri`i'_d if eu==1
}

foreach x in 1 2 3 4 5  {
	ren   stri`x'_d stri_`x'
}

//Add domestic production
gen brdr=1 if exporter_iso3!=importer_iso3
replace brdr=0 if brdr==.
gen brdr_stri=brdr*stri_d

forvalues i =1/3{
	gen brdr_stri`i'=brdr*stri_`i'
}

//EU cross-border trade dummy
gen eubrdr=0
replace eubrdr=1 if eu==1 & brdr==1

//Generate Table A15 estimates (OLS with no EU flows)
* Column 1 (No STRI and Intra-Trade)
qui reghdfe ln_trade ln_dist cntg lang clny rta_s brdr brdr_stri brdr_stri1 if eubrdr==0,a(exp#prod#year imp#prod#year) cluster(exp#imp)
gen stri_sample = 1 if e(sample) 

eststo clear
reghdfe ln_trade ln_dist cntg lang clny rta_s if importer_iso3!=exporter_iso3 & stri_sample==1 & eubrdr==0,a(exp#prod#year imp#prod#year) cluster(exp#imp)
estadd local impsecyr "Yes"
estadd local expsecyr "Yes"
estadd local intra "No"
eststo d_a

*Column 2 (STRI1 and Intra-Trade)
reghdfe ln_trade ln_dist cntg lang clny rta_s brdr brdr_stri1 if eubrdr==0,a(exp#prod#year imp#prod#year) cluster(exp#imp) 
estadd local impsecyr "Yes"
estadd local expsecyr "Yes"
estadd local intra "Yes"
eststo d_b
		
* Column 3 (STRI, STRI1 and Intra-Trade)	
reghdfe ln_trade ln_dist cntg lang clny rta_s brdr brdr_stri brdr_stri1 if eubrdr==0,a(exp#prod#year imp#prod#year) cluster(exp#imp)
estadd local impsecyr "Yes"
estadd local expsecyr "Yes"
estadd local intra "Yes"
eststo d_c

esttab using "$appendix_output/tableA15.txt", b(3) se(2) star(* 0.1 ** 0.05 *** 0.01) drop(_cons) replace title("Table A15: Gravity Estimates with STRI (2014-16)") scalars("impsecyr Imp-Sec-Year" "expsecyr Exp-Sec-Year" "intra Intra-Trade")

log close




