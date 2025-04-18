*Replicates the steps to construct all datasets used in the estimation of "Brexit Uncertainty and its (Dis)Service Effects" 
*Note: User needs first to download ITPD-E and Dynamic Gravity Version 2.0 from the USITC Gravity Portal 

** REPLICATION 


*Set directory paths
global data_input "."  // Current directory
global data_output "../Results - Replication" 

cd "$data_input"

*Services trade data from UK ONS*
import excel "UK quarterly imports by services type.xlsx", sheet("Time Series") firstrow clear

rename Q1 value2016Q1
rename Q2 value2016Q2
rename Q3 value2016Q3
rename Q4 value2016Q4

rename M value2017Q1
rename N value2017Q2
rename O value2017Q3
rename P value2017Q4

rename Q value2018Q1
rename R value2018Q2
rename S value2018Q3
rename T value2018Q4

rename U value2019Q1
rename V value2019Q2
rename W value2019Q3

drop F G H

reshape long value, i( Direction Servicetypecode Servicetype Countrycode Country) j(year) string
export delimited using "quarterlyimports.csv", replace

*OECD MFN STRI data*
import delimited "OECD STRI detailed.csv", clear
generate ebops=""
replace ebops="10.2.1" if sector=="Accounting"
replace ebops="3.2" if sector=="Air transport"
replace ebops="10.3.1" if sector=="Architecture"
replace ebops="9.1" if sector=="Broadcasting"
replace ebops="7.1" if sector=="Commercial banking"
replace ebops="9.2" if sector=="Computer"
replace ebops="5" if sector=="Construction"
replace ebops="3.4" if sector=="Courier"

replace ebops="10.3.1" if sector=="Engineering"
replace ebops="6" if sector=="Insurance"
replace ebops="10.2.1" if sector=="Legal"
replace ebops="3.3" if sector=="Logistics cargo-handling"
replace ebops="3.3" if sector=="Logistics customs brokerage"
replace ebops="3.3" if sector=="Logistics freight forwarding"
replace ebops="3.3" if sector=="Logistics storage and warehouse"
replace ebops="3.1" if sector=="Maritime transport"
replace ebops="11.1" if sector=="Motion pictures"
replace ebops="3.3" if sector=="Rail freight transport"
replace ebops="3.3" if sector=="Road freight transport"
replace ebops="11.1" if sector=="Sound recording"
replace ebops="9.1" if sector=="Telecom"

keep clas cou year stri ebops 
rename clas stri_type
rename cou importer_country
collapse stri, by(stri_type importer_country year ebops)
drop if ebops==""
reshape wide stri, i(importer_country year ebops) j(stri_type) string
drop striCLAS3_DR_TO- striCLAS5_NON_DISC
save "striwithebops.dta", replace

*OECD EEA STRI data*
import delimited "OECD EEA stri.csv", clear
keep clas cou year eea_stri sector
reshape wide eea_stri, i(clas cou sector) j(year)
replace eea_stri2014= eea_stri2018 if eea_stri2014==.
replace eea_stri2015= eea_stri2018 if eea_stri2015==.
replace eea_stri2016= eea_stri2018 if eea_stri2016==.
replace eea_stri2017= eea_stri2018 if eea_stri2017==.
reshape long eea_stri, i(clas cou sector) j(year)
generate ebops=""
replace ebops="10.2.1" if sector=="Accounting"
replace ebops="3.2" if sector=="Air transport"
replace ebops="10.3.1" if sector=="Architecture"
replace ebops="9.1" if sector=="Broadcasting"
replace ebops="7.1" if sector=="Commercial banking"
replace ebops="9.2" if sector=="Computer"
replace ebops="5" if sector=="Construction"
replace ebops="3.4" if sector=="Courier"

replace ebops="10.3.1" if sector=="Engineering"
replace ebops="6" if sector=="Insurance"
replace ebops="10.2.1" if sector=="Legal"
replace ebops="3.3" if sector=="Logistics cargo-handling"
replace ebops="3.3" if sector=="Logistics customs brokerage"
replace ebops="3.3" if sector=="Logistics freight forwarding"
replace ebops="3.3" if sector=="Logistics storage and warehouse"
replace ebops="3.1" if sector=="Maritime transport"
replace ebops="11.1" if sector=="Motion pictures"
replace ebops="3.3" if sector=="Rail freight transport"
replace ebops="3.3" if sector=="Road freight transport"
replace ebops="11.1" if sector=="Sound recording"
replace ebops="9.1" if sector=="Telecom"

keep clas cou year eea_stri ebops
rename clas stri_type
rename cou importer_country
collapse eea_stri, by(stri_type importer_country year ebops)
drop if ebops==""
reshape wide eea_stri, i(importer_country year ebops) j(stri_type) string

save "eeastriwithebops.dta", replace

*exchange rate data*
import excel quarters=A euro=G using "BoE exch rates.xlsx", sheet("brexit exch rates") clear cellrange(A2)
save euro_pound, replace 

import excel quarters=A CZE=B HUN=C POL=D DNK=E SWE=F using "BoE exch rates.xlsx", sheet("brexit exch rates") cellrange(A2) clear
rename (CZE-SWE) non_euro=
reshape long non_euro, i(quarters) j(iso3) string
sort iso3 quarters
save non_euro_pound,replace

*Brexit Probabilty data*

*As noted in the readme, raw data is not included in our data repository. If the replicator obtains the raw data described in the readme and reads it into .dta files, this commented out code will clean and merge the PredictIT and PredictWise data.  
/*PredictIT-Daily averages for Brexit
use predictIT_brexit_daily.dta, clear
gen month=month(daydate)
gen year=year(daydate)
collapse mbv, by(month year)
save brexit_monthly.dta, replace

*PredictWise-Daily averages for Article 50
use predictwise_art50_daily_format.dta, clear
gen month=month(date_d)
gen year=year(date_d)
collapse art50_1S2017, by(month year)
replace art50_1S2017=art50_1S2017/100

merge 1:1 month year using brexit_monthly.dta

*replace values for article 50 with average value for July 2016, the first month with complete article 50 data. 
replace art50_1S2017=.39709678 if (year==2016 & month<7) | year==2015

drop _m
replace mbv=1 if mbv==.
replace art50_1S2017=1 if art50_1S2017==.

*combined probability measure

gen brexit_comb_prob=mbv*art50_1S2017 

*/

*This code formats the monthly data available in the data repository into quarterly data
use art_50_cond_ref.dta, clear
sort year month 
gen brexit_L1=brexit_comb_prob[_n-1]
gen brexit_L2=brexit_comb_prob[_n-2]
gen mov_avg= (brexit_comb_prob+ brexit_L1+ brexit_L2)/3

generate quarter=0
replace quarter=8 if year==2017 & month>=10 & month<=12
replace quarter=7 if year==2017 & month>=7 & month<=9
replace quarter=6 if year==2017 & month>=4 & month<=6
replace quarter=5 if year==2017 & month>=1 & month<=3
replace quarter=4 if year==2016 & month>=10 & month<=12
replace quarter=3 if year==2016 & month>=7 & month<=9
replace quarter=2 if year==2016 & month>=4 & month<=6
replace quarter=1 if year==2016 & month>=1 & month<=3


drop if quarter==0
collapse brexit_comb_prob mov_avg, by(quarter)
rename brexit_comb_prob brexit_mean_q
rename mov_avg brexit_ma_q
label variable brexit_mean_q "Simple mean of current combined ref and art 50 prob. Avg over months in quarter"
label variable brexit_ma_q "Moving average of current and up to two lags of combined ref and art 50 prob. Avg."

save art_50_cond_ref_quarterly.dta, replace

*merging trade data with stri data*
import delimited "quarterlyimports.csv", varnames(1) clear 

rename servicetypecode ebops
rename countrycode countryisocode
*adding 3 digit ISO-data
merge m:1 countryisocode using "twotothreeISO.dta" 
rename countrycode countryiso3
drop _m
rename year quarters
generate year = substr(quarters,1, 4)
drop if countryiso3==""
replace value="" if value==".."
destring value, replace
destring year, replace
replace ebops="9.2" if ebops=="9.199999999999999"
generate exporter_country = "GBR" if direction=="Exports"
replace exporter_country = countryiso3 if direction=="Imports"
generate importer_country = "GBR" if direction =="Imports"
replace importer_country = countryiso3 if direction == "Exports"

merge m:1 importer_country year ebops using "striwithebops.dta"
drop _m 
merge m:1 importer_country year ebops using "eeastriwithebops.dta"
drop _m

 generate eea_exporter= 1 if exporter_country=="ISL" | exporter_country=="LIE" | exporter_country=="AUT" | exporter_country=="BEL" | exporter_country=="BGR" | exporter_country=="HRV" | exporter_country=="CYP" | exporter_country=="CZE" | exporter_country=="DNK" | exporter_country=="EST" | exporter_country=="FIN" | exporter_country=="FRA" | exporter_country=="DEU" | exporter_country=="GRC" | exporter_country=="HUN" | exporter_country=="IRL" | exporter_country=="ITA" | exporter_country=="LVA" | exporter_country=="LTU" | exporter_country=="LUX" | exporter_country=="MLT" | exporter_country=="NLD" | exporter_country=="POL" | exporter_country=="PRT" | exporter_country=="ROM" | exporter_country=="SVK" | exporter_country=="SVN" | exporter_country=="ESP" | exporter_country=="SWE" | exporter_country=="NOR" | exporter_country=="GBR" 	 

 generate eea_importer =1 if importer_country == "ISL" | importer_country == "LIE" | importer_country == "AUT" |importer_country == "BEL" | importer_country == "BGR" | importer_country == "HRV" | importer_country == "CYP" | importer_country == "CZE" | importer_country == "DNK" | importer_country == "EST" | importer_country == "FIN" | importer_country == "FRA" | importer_country == "DEU" | importer_country == "GRC" | importer_country == "HUN" | importer_country == "IRL" | importer_country == "ITA" | importer_country == "LVA" | importer_country == "LTU" | importer_country == "LUX" | importer_country == "MLT" | importer_country == "NLD" | importer_country == "POL" | importer_country == "PRT" | importer_country == "ROM" | importer_country == "SVK" | importer_country == "SVN" | importer_country == "ESP" | importer_country == "SWE" | importer_country == "NOR" | importer_country == "GBR" 

 generate eea_pair=1 if eea_exporter!=. & eea_importer!=.
 
 foreach v of var eea_striSTRI eea_striCLAS1_1 eea_striCLAS1_2 eea_striCLAS1_3 eea_striCLAS1_4 eea_striCLAS1_5{
 replace `v' = . if eea_pair!=1    
 }

drop if direction==""

keep ebops servicetype quarters value year exporter_country importer_country striCLAS1_1 striCLAS1_2 striCLAS1_3 striCLAS1_4 striCLAS1_5 striSTRI eea_striCLAS1_1 eea_striCLAS1_2 eea_striCLAS1_3 eea_striCLAS1_4 eea_striCLAS1_5 eea_striSTRI eea_pair 
rename value trade_value
label variable trade_value "millions pounds"
label variable quarters "quarterly time variable"
label variable year "annual time variable"
rename striCLAS1_1 stri_category1
label variable stri_category1 "Restrictions on foreign ownership and other market entry conditions"
rename striCLAS1_2 stri_category2
rename striCLAS1_3 stri_category3
rename striCLAS1_4 stri_category4
rename striCLAS1_5 stri_category5

rename striSTRI stri_total
label variable stri_category2 "Restrictions on the movement of people"
label variable stri_category3 "Other discriminatory measures and international standards"
label variable stri_category4 "Barriers to competition and public ownership"
label variable stri_category5 "Regulatory transparency and administrative requirements"

rename eea_striCLAS1_1 eea_stri_category1
label variable eea_stri_category1 "Restrictions on foreign ownership and other market entry conditions"
rename eea_striCLAS1_2 eea_stri_category2
rename eea_striCLAS1_3 eea_stri_category3
rename eea_striCLAS1_4 eea_stri_category4
rename eea_striCLAS1_5 eea_stri_category5
rename eea_striSTRI eea_stri_total
label variable eea_stri_category2 "Restrictions on the movement of people"
label variable eea_stri_category3 "Other discriminatory measures and international standards"
label variable eea_stri_category4 "Barriers to competition and public ownership"
label variable eea_stri_category5 "Regulatory transparency and administrative requirements"

*add passport risk data-compiled by the authors using data from OECD STRI Data Explorer tool*
merge m:1 importer_country ebops using "passport_dummy.dta"
replace passport_d=0 if passport_d==.
drop _merge
keep if ebops=="10.2.1" | ebops=="3.2" | ebops=="10.3.1" | ebops=="9.1" | ebops=="7.1" | ebops=="9.2" | ebops=="5" | ebops=="3.4" | ebops=="6" | ebops=="3.3" | ebops=="3.1" | ebops=="11.1" | ebops=="3.3" 
save "brexit_services_fullstri.dta", replace
keep if stri_total!=.
drop eea_pair 
save "brexit_services_substri.dta", replace

//(1) EU and EEA sample countries
**missing 5 eu countries Bulgaria, Croatia, Cyprus, Malta, Romania
**missing Liechenstein in eea, so only ISL and NOR
use brexit_services_substri, clear
keep eea_stri_total importer_c
keep if eea_stri_total~=. 
keep importer_c
duplicates drop 

gen eea_m=1 
gen eu_minus_gb_m=1 if eea_m==1 & importer~="ISL" & importer~="GBR" & importer~="NOR"
gen gb_m=1 if importer=="GBR"
sort importer_c
save test_m, replace

ren  importer_c exporter_country
ren  eea_m eea_x
ren  eu_minus_gb_m eu_minus_gb_x 
ren  gb_m gb_x 
sort exporter_c
save test_x, replace
	
use brexit_services_substri, clear
sort importer_c
merge importer_c using test_m, nokeep
sum _merge
drop _merge
 
sort exporter_c
merge exporter_c using test_x, nokeep
sum _merge
drop _merge
 
**EU and EEA dummies 
replace eu_minus_gb_m=0 if eu_minus_gb_m==.
replace eu_minus_gb_x=0 if eu_minus_gb_x==.
gen eu_minus_gb=1 if eu_minus_gb_m==1 | eu_minus_gb_x==1
replace eu_minus_gb=0 if eu_minus_gb==.
//replace euro=. if eu_minus_gb!=1
	
replace eea_m=0 if eea_m==.
replace eea_x=0 if eea_x==.
gen eea=1 if eea_m==eea_x==1
replace  eea=0 if eea==.

//(2) Gravity dummies and trade variables
sort quarters
egen quarter=group(quarters) , lname(quarters)
egen I=group(exporter) , lname(exporter)
egen J=group(importer), lname(importer)
egen S=group(service) , lname(service)
egen id=group(exporter importer service)
compress

gen log_value=ln(trade_value)
gen pos_tr_non_d=1 	//note that we include the non-reported confidential since these are positive values of trade 
replace pos_tr_non_d=0 if trade_v==0 //True zeros
label var pos_tr_non_d "positive trade indicator, also 1 if missing due to confidentiality"

//Continuously traded services excluding confidential values
gen pos_tr_exc_conf=pos_tr_non_d 
replace pos_tr_exc_conf=0 if trade_value ==. //positive trade excluding confidential values
bysort J I S: egen q_traded_all=min(pos_tr_exc_conf)  
gen log_value_cont=log_value if q_traded_all==1 //Captures trade bw ctry pairs with no missing obs for a sector in sample  
label var log_value_cont "Continuously traded excluding any confidential values"

//Impute missing confidential trade values with min of relevant exporter or importer within EU (used in PPML)
bysort  S quarter : egen test1=min(log_value) if expo=="GBR" & eu_minus_gb==1 
bysort  S quarter : egen test2=min(log_value) if impo=="GBR" & eu_minus_gb==1 

gen log_val_imp_conf=log_value
replace log_val_imp_conf=test1 if trade_value==. & expo=="GBR" & eu_minus_gb==1 
replace log_val_imp_conf=test2 if trade_value==. & impo=="GBR" & eu_minus_gb==1 

gen trade_imp=exp(log_val_imp_conf)
replace trade_imp=0 if trade_value==0
label var trade_imp "Imputed values for confidential trade flows"

//(3) Merge with Brexit probablity data
sort quarter
capture drop _merge
merge quarter using art_50_cond_ref_quarterly.dta, nokeep

sum _merge if year<2018, det
drop _merge
list quarters year br* if y<2018 & im=="GBR" & ebo=="3.4" & ex=="DEU", clean noobs //testcase
	
//(4) Generate STRI risk variables
**shorten var names
foreach x in 1 2 3 4 5  {
	ren   stri_category`x' stri_c`x'
	ren   eea_stri_category`x' eea_stri_c`x'
}

**combine the protection levels when pooling eu and row, which are initially in different variables. 
foreach x in total c1 c2 c3 c4 c5 {
	gen stri_`x'_comb=eea_stri_`x' if eea==1
	replace stri_`x'_comb=stri_`x' if eea==0
}	

**use only the initial year stri b/c it does not vary much (don't want spurious coeficients from that variation)
foreach y in total c1 c2 c3 c4 c5 {
	foreach x in  stri_`y'_comb eea_stri_`y' stri_`y' {
		gen `x'_base=`x' if year==2016
		bysort J I S: egen test=mean(`x'_base)
		compare test `x'_base
		replace `x'_base=test
		drop test
		}
}

**compute mfn_risk as percent change of stri factor ((1+stri_`x'_base)/(1+eea_stri_`x'_base))-1
foreach x in total c1 c2 c3 c4 c5 {
	gen mfn_risk_`x'=max(0,((1+stri_`x'_base)/(1+eea_stri_`x'_base))-1)
	foreach y in brexit_me brexit_ma {
		gen mfn_risk_`x'_x_`y'=mfn_risk_`x'*`y'
		gen stri_`x'_base_x_`y'=stri_`x'_base*`y'
	}
}
	
//(5) Interact STRI risk with Brexit probablity	(allow for different effects pre and post referendum)
	*1. use log probability and J#I#S effects to follow GHL more closely. 
	*2. Try different coefficients for pre and post. 
	*3. Extend by imputing the probability to be same from end of 2017 to end of 2018
	
	**note that the approach below using logs yields exactly the same as if we just take the log of the combined brexit probability (As used) or separating out the pre and post (allows us to try different coefficient estimation at some point). Must be careful in computing impact and summary stats for probability based on ln_brexit. 

foreach br in  me  ma {
	gen pre_`br'=(brexit_`br') if quarter<3
	replace pre_`br'=1 if quarter>2

	gen post_`br'= (brexit_`br') if quarter>2
	replace post_`br'=1 if quarter>8
	replace post_`br'=1 if quarter<3

	gen ln_brexit_`br'=ln(brexit_`br')
	replace ln_brexit_`br'=0 if quarter>8  /*imputes 2018, 2019 to 1*/
}

foreach x in total c1 c2 c3 c4 c5{ 
	foreach br in  me  ma {
		gen pre_ln_mfn_risk_`x'_x_`br'=mfn_risk_`x'*ln(pre_`br')
		gen post_ln_mfn_risk_`x'_x_`br'=mfn_risk_`x'*ln(post_`br')
*		gen ln_mfn_risk_`x'_x_`br'=pre_ln_mfn_risk_`x'_x_`br'+post_ln_mfn_risk_`x'_x_`br'
		gen ln_mfn_risk_`x'_x_`br'=mfn_risk_`x'*ln_brexit_`br'
	}
}




order exporter_c importer_c ebops servicetype quarters trade_value pos_tr* log_value* trade_imp //arrange vars  
sort exporter_country importer_country ebops quarters

*Merge exchange rate data*
merge m:1 quarters using euro_pound 
sort exporter_country importer_country ebops quarters
replace euro=. if eu_minus_gb!=1 //Remove exch rates for non EU countries
drop _merge

gen iso3=exporter_country
merge m:1 iso3 quarters using non_euro_pound
rename non_euro non_euro_exp

replace iso3=importer_country
merge m:1 iso3 quarters using non_euro_pound, gen(_merge2)
rename non_euro non_euro_imp
replace euro=non_euro_imp if non_euro_imp!=.
replace euro=non_euro_exp if non_euro_exp!=.
rename euro exch
drop iso3 _merge* non_euro*
sort exporter_country importer_country ebops quarters

*replace exch=1/exch if importer_country=="GBR" //all exch rates now referenced as price of exporter's currency in importer's
*gen log_exch=ln(exch)

** REPLICATION 1
* Create quarterly average measures first
preserve
    collapse (mean) brexit_mean_q, by(quarter)
    sort quarter
    gen brexit_L1_q = brexit_mean_q[_n-1]
    gen brexit_L2_q = brexit_mean_q[_n-2]
    gen brexit_L3_q = brexit_mean_q[_n-3]
    gen brexit_L4_q = brexit_mean_q[_n-4]
    gen brexit_L5_q = brexit_mean_q[_n-5]
    
    gen brexit_ma3_q = (brexit_mean_q + brexit_L1_q + brexit_L2_q)/3
    gen brexit_ma6_q = (brexit_mean_q + brexit_L1_q + brexit_L2_q + brexit_L3_q + brexit_L4_q + brexit_L5_q)/6
    
    keep quarter brexit_ma3_q brexit_ma6_q
    save temp_brexit_lags.dta, replace
restore

* Merge the quarterly measures back
merge m:1 quarter using temp_brexit_lags.dta, nogenerate

* Generate required variables for analysis
gen ln_brexit_ma3 = ln(brexit_ma3_q)
gen ln_brexit_ma6 = ln(brexit_ma6_q)

* Create interaction terms with risk measures
foreach x in total c1 c2 c3 c4 c5 {
    gen ln_mfn_risk_`x'_x_ma3 = mfn_risk_`x'*ln_brexit_ma3
    gen ln_mfn_risk_`x'_x_ma6 = mfn_risk_`x'*ln_brexit_ma6
}

** REPLICATION 2
* Create continuous measure of risk instead of binary high/low
* Use the actual STRI values rather than just the binary categorization
xtile stri_quintiles = stri_c1_base, nq(5)
gen stri_continuous = stri_c1_base

* Create alternative risk measures using different STRI categories
gen alt_risk1_x_mean = stri_continuous*brexit_mean_q
gen alt_risk1_x_ma = stri_continuous*brexit_ma_q
    
* Use other STRI categories (not just category 1)
gen alt_risk2_x_mean = stri_c2_base*brexit_mean_q
gen alt_risk2_x_ma = stri_c2_base*brexit_ma_q
gen alt_risk3_x_mean = stri_c3_base*brexit_mean_q
gen alt_risk3_x_ma = stri_c3_base*brexit_ma_q
    
* Create a composite risk measure across categories
gen comp_risk_x_mean = (stri_c1_base + stri_c2_base + stri_c3_base)/3*brexit_mean_q
gen comp_risk_x_ma = (stri_c1_base + stri_c2_base + stri_c3_base)/3*brexit_ma_q


** REPLICATION 3
* Create subsamples for sensitivity analysis
* Pre-referendum sample
gen pre_referendum = (quarter < 3)

* Post-Article 50 sample
gen post_article50 = (quarter > 5)

* Excluding specific countries or outliers
gen exclude_outliers = !(importer_country=="IRL" & exporter_country=="GBR") 

* Create a sample without financial services
gen non_financial = (ebops != "7.1" & ebops != "6")

** REPLICATION 4
replace exch=1/exch if importer_country=="GBR" 
gen log_exch=ln(exch)

save "$data_output/brexit_services_main_sample", replace


save "$data_output/brexit_services_main_sample", replace
//erase del.dta
//erase class_hi.dta

**Add additional variables for extended sample**
use "brexit_services_fullstri.dta", clear
//(1) EU and EEA sample countries
**missing 5 eu countries Bulgaria, Croatia, Cyprus, Malta, Romania
**missing Liechenstein in eea, so only ISL and NOR
keep eea_stri_total importer_c
keep if eea_stri_total~=. 
keep importer_c
duplicates drop 

gen eea_m=1 
gen eu_minus_gb_m=1 if eea_m==1 & importer~="ISL" & importer~="GBR" & importer~="NOR"
gen gb_m=1 if importer=="GBR"
sort importer_c
save test_m, replace

ren  importer_c exporter_country
ren  eea_m eea_x
ren  eu_minus_gb_m eu_minus_gb_x 
ren  gb_m gb_x 
sort exporter_c
save test_x, replace
	
use "brexit_services_fullstri.dta", replace
drop if quarters=="2019Q1" | quarters=="2019Q2" | quarters=="2019Q3"
sort importer_c
merge importer_c using test_m, nokeep
sum _merge
drop _merge
 
sort exporter_c
merge exporter_c using test_x, nokeep
sum _merge
drop _merge
 
**EU and EEA dummies 
replace eu_minus_gb_m=0 if eu_minus_gb_m==.
replace eu_minus_gb_x=0 if eu_minus_gb_x==.
gen eu_minus_gb=1 if eu_minus_gb_m==1 | eu_minus_gb_x==1
replace eu_minus_gb=0 if eu_minus_gb==.
//replace euro=. if eu_minus_gb!=1
	
replace eea_m=0 if eea_m==.
replace eea_x=0 if eea_x==.
gen eea=1 if eea_m==eea_x==1
replace  eea=0 if eea==.

//(2) Gravity dummies and trade variables
sort quarters
egen quarter=group(quarters) , lname(quarters)
egen I=group(exporter) , lname(exporter)
egen J=group(importer), lname(importer)
egen S=group(service) , lname(service)
egen id=group(exporter importer service)
compress

gen log_value=ln(trade_value)
gen pos_tr_non_d=1 	//note that we include the non-reported confidential since these are positive values of trade 
replace pos_tr_non_d=0 if trade_v==0 //True zeros
label var pos_tr_non_d "positive trade indicator, also 1 if missing due to confidentiality"

//Continuously traded services excluding confidential values
gen pos_tr_exc_conf=pos_tr_non_d 
replace pos_tr_exc_conf=0 if trade_value ==. //positive trade excluding confidential values
bysort J I S: egen q_traded_all=min(pos_tr_exc_conf)  
gen log_value_cont=log_value if q_traded_all==1 //Captures trade bw ctry pairs with no missing obs for a sector in sample  
label var log_value_cont "Continuously traded excluding any confidential values"

//Impute missing confidential trade values with min of relevant exporter or importer within EU (used in PPML)
bysort  S quarter : egen test1=min(log_value) if expo=="GBR" & eu_minus_gb==1 
bysort  S quarter : egen test2=min(log_value) if impo=="GBR" & eu_minus_gb==1 

gen log_val_imp_conf=log_value
replace log_val_imp_conf=test1 if trade_value==. & expo=="GBR" & eu_minus_gb==1 
replace log_val_imp_conf=test2 if trade_value==. & impo=="GBR" & eu_minus_gb==1 

gen trade_imp=exp(log_val_imp_conf)
replace trade_imp=0 if trade_value==0
label var trade_imp "Imputed values for confidential trade flows"

//(3) Merge with Brexit probablity data
sort quarter
capture drop _merge
merge quarter using art_50_cond_ref_quarterly.dta, nokeep

sum _merge if year<2018, det
drop _merge
list quarters year br* if y<2018 & im=="GBR" & ebo=="3.4" & ex=="DEU", clean noobs //testcase
	
//(4) Generate STRI risk variables
**shorten var names
foreach x in 1 2 3 4 5  {
	ren   stri_category`x' stri_c`x'
	ren   eea_stri_category`x' eea_stri_c`x'
}

**combine the protection levels when pooling eu and row, which are initially in different variables. 
foreach x in total c1 c2 c3 c4 c5 {
	gen stri_`x'_comb=eea_stri_`x' if eea==1
	replace stri_`x'_comb=stri_`x' if eea==0
}	

**use only the initial year stri b/c it does not vary much (don't want spurious coeficients from that variation)
foreach y in total c1 c2 c3 c4 c5 {
	foreach x in  stri_`y'_comb eea_stri_`y' stri_`y' {
		gen `x'_base=`x' if year==2016
		bysort J I S: egen test=mean(`x'_base)
		compare test `x'_base
		replace `x'_base=test
		drop test
		}
}

**compute mfn_risk as percent change of stri factor ((1+stri_`x'_base)/(1+eea_stri_`x'_base))-1
foreach x in total c1 c2 c3 c4 c5 {
	gen mfn_risk_`x'=max(0,((1+stri_`x'_base)/(1+eea_stri_`x'_base))-1)
	foreach y in brexit_me brexit_ma {
		gen mfn_risk_`x'_x_`y'=mfn_risk_`x'*`y'
		gen stri_`x'_base_x_`y'=stri_`x'_base*`y'
	}
}
	
//(5) Interact STRI risk with Brexit probablity	(allow for different effects pre and post referendum)
	*1. use log probability and J#I#S effects to follow GHL more closely. 
	*2. Try different coefficients for pre and post. 
	*3. Extend by imputing the probability to be same from end of 2017 to end of 2018
	
	**note that the approach below using logs yields exactly the same as if we just take the log of the combined brexit probability (As used) or separating out the pre and post (allows us to try different coefficient estimation at some point). Must be careful in computing impact and summary stats for probability based on ln_brexit. 

foreach br in  me  ma {
	gen pre_`br'=(brexit_`br') if quarter<3
	replace pre_`br'=1 if quarter>2

	gen post_`br'= (brexit_`br') if quarter>2
	replace post_`br'=1 if quarter>8
	replace post_`br'=1 if quarter<3

	gen ln_brexit_`br'=ln(brexit_`br')
	replace ln_brexit_`br'=0 if quarter>8  /*imputes 2018, 2019 to 1*/
}

foreach x in total c1 c2 c3 c4 c5{ 
	foreach br in  me  ma {
		gen pre_ln_mfn_risk_`x'_x_`br'=mfn_risk_`x'*ln(pre_`br')
		gen post_ln_mfn_risk_`x'_x_`br'=mfn_risk_`x'*ln(post_`br')
*		gen ln_mfn_risk_`x'_x_`br'=pre_ln_mfn_risk_`x'_x_`br'+post_ln_mfn_risk_`x'_x_`br'
		gen ln_mfn_risk_`x'_x_`br'=mfn_risk_`x'*ln_brexit_`br'
	}
}

order exporter_c importer_c ebops servicetype quarters trade_value pos_tr* log_value* trade_imp //arrange vars  
sort exporter_country importer_country ebops quarters

erase brexit_services_fullstri.dta
erase brexit_services_substri.dta
erase quarterlyimports.csv
erase art_50_cond_ref_quarterly.dta
erase striwithebops.dta
erase eeastriwithebops.dta
erase euro_pound.dta
erase non_euro_pound.dta
erase test_m.dta
erase test_x.dta
save "$data_output/brexit_services_extended_sample.dta", replace


*****************Data for Gravity Model-Table A15*****

/*Download v1 of ITPD-E data from this link and save to working directory: https://www.usitc.gov/data/gravity/itpd_e_r01.zip 

Download Dynamic Gravity Version 2.0 from this link and save to working directory:  https://www.usitc.gov/data/gravity/dgd.htm 
*/
import delimited ITPD_E_R01.csv, clear 
keep exporter_iso3 importer_iso3 year industry_id industry_descr trade flag_zero
keep if industry_id>=154 //Only services sectors
save itpd_services_full, replace

// (2) Clean Dynamic Gravity 2.0 
clear
import delimited release_2.0_2000_2016.csv //Dynamic Gravity Version 2.0 downloaded from https://www.usitc.gov/data/gravity/dgd.htm
rename (iso3_o iso3_d) (exporter_iso3 importer_iso3)
save dgd2, replace

// (3) Merge ITPD services with Dynamic Gravity 2.0
use itpd_services_full,clear
merge m:1 exporter_iso3 importer_iso3 year using dgd2
drop if _merge==2
drop _merge
save itpd_services_gravity

// (4) Create STRI for ITPD sectors
// (4A) STRI for ITPD sectors 
import delimited "OECD STRI detailed.csv", clear //OECD STRI data file provided
generate prod_id=.
replace prod_id=163 if sector=="Accounting"
replace prod_id=163 if sector=="Engineering"
replace prod_id=163 if sector=="Architecture"
replace prod_id=163 if sector=="Legal"
replace prod_id=156 if sector=="Air transport"
replace prod_id=156 if sector=="Courier"
replace prod_id=156 if sector=="Logistics cargo-handling"
replace prod_id=156 if sector=="Logistics customs brokerage"
replace prod_id=156 if sector=="Logistics freight forwarding"
replace prod_id=156 if sector=="Logistics storage and warehouse"
replace prod_id=156 if sector=="Maritime transport"
replace prod_id=156 if sector=="Rail freight transport"
replace prod_id=156 if sector=="Road freight transport"
replace prod_id=162 if sector=="Broadcasting"
replace prod_id=162 if sector=="Computer"
replace prod_id=162 if sector=="Motion pictures"
replace prod_id=162 if sector=="Sound recording"
replace prod_id=162 if sector=="Telecom"
replace prod_id=158 if sector=="Construction"
replace prod_id=160 if sector=="Commercial banking"
replace prod_id=159 if sector=="Insurance"

keep clas cou year stri prod_id 
rename clas stri_type
rename cou importer_iso3
collapse stri, by(stri_type importer_iso3 year prod_id)
drop if prod_id==.
reshape wide stri, i(importer_iso3 year prod_id) j(stri_type) string
drop striCLAS2_AM- striCLAS5_NON_DISC

//Create STRIs by origin and destination
drop if year>2016
gen exporter_iso3=importer_iso3
rename (striCLAS1_1 striCLAS1_2  striCLAS1_3 striCLAS1_4 striCLAS1_5 striSTRI) (stri1_o stri2_o stri3_o stri4_o stri5_o stri_o)

local str stri stri1 stri2 stri3 stri4 stri5
foreach v of local str  {
    generate `v'_d = `v'_o
}
save stri_ITPD, replace

use stri_ITPD, clear
keep importer_i~3 year prod_id stri_d stri2_d stri4_d stri1_d stri3_d stri5_d
save stri_ITPD_imp, replace

use stri_ITPD, clear
keep exporter_i~3 year prod_id stri_o stri2_o stri4_o stri1_o stri3_o stri5_o
save stri_ITPD_exp, replace

//(4B) EEA-STRI for ITPD sectors
import delimited "OECD EEA stri.csv", clear //OECD EEA STRI data file provided
keep clas cou year eea_stri sector
reshape wide eea_stri, i(clas cou sector) j(year)
replace eea_stri2014= eea_stri2018 if eea_stri2014==.
replace eea_stri2015= eea_stri2018 if eea_stri2015==.
replace eea_stri2016= eea_stri2018 if eea_stri2016==.
replace eea_stri2017= eea_stri2018 if eea_stri2017==.
reshape long eea_stri, i(clas cou sector) j(year)

generate prod_id=.
replace prod_id=163 if sector=="Accounting"
replace prod_id=163 if sector=="Engineering"
replace prod_id=163 if sector=="Architecture"
replace prod_id=163 if sector=="Legal"
replace prod_id=156 if sector=="Air transport"
replace prod_id=156 if sector=="Courier"
replace prod_id=156 if sector=="Logistics cargo-handling"
replace prod_id=156 if sector=="Logistics customs brokerage"
replace prod_id=156 if sector=="Logistics freight forwarding"
replace prod_id=156 if sector=="Logistics storage and warehouse"
replace prod_id=156 if sector=="Maritime transport"
replace prod_id=156 if sector=="Rail freight transport"
replace prod_id=156 if sector=="Road freight transport"     
replace prod_id=162 if sector=="Broadcasting"
replace prod_id=162 if sector=="Computer"
replace prod_id=162 if sector=="Motion pictures"
replace prod_id=162 if sector=="Sound recording"
replace prod_id=162 if sector=="Telecom"
replace prod_id=158 if sector=="Construction"
replace prod_id=160 if sector=="Commercial banking"
replace prod_id=159 if sector=="Insurance"

keep clas cou year eea_stri prod_id 
rename clas stri_type
rename cou importer_iso3
collapse eea_stri, by(stri_type importer_iso3 year prod_id)
drop if prod_id==.
reshape wide eea_stri, i(importer_iso3 year prod_id) j(stri_type) string

//Create EEA_STRIs by origin and destination
drop if year>2016
gen exporter_iso3=importer_iso3
rename (eea_striCLAS1_1 eea_striCLAS1_2  eea_striCLAS1_3 eea_striCLAS1_4 eea_striCLAS1_5 eea_striSTRI) (eea_stri1_o eea_stri2_o eea_stri3_o eea_stri4_o eea_stri5_o eea_stri_o)

local estr eea_stri1 eea_stri2 eea_stri3 eea_stri4 eea_stri5 eea_stri
foreach v of local estr  {
    generate `v'_d = `v'_o
}
save eea_stri_ITPD, replace

keep importer_i~3 year prod_id eea_stri_d eea_stri2_d eea_stri4_d eea_stri1_d eea_stri3_d eea_stri5_d
save eea_stri_ITPD_imp

use eea_stri_ITPD, clear
drop importer_i~3 eea_stri_d eea_stri2_d eea_stri4_d eea_stri1_d eea_stri3_d eea_stri5_d
save eea_stri_ITPD_exp 

// (5) Merge ITPD-Dynamic Gravity with STRI for 2014-2016
use itpd_services_gravity, clear
rename industry_id prod_id
keep if prod_id==156 | prod_id==158 | prod_id==159 | prod_id==160 | prod_id==162 | prod_id==163 //Sectors with STRIs
drop if year<2014

merge m:1 importer_iso3 prod_id year using "stri_ITPD_imp.dta"
rename _merge merge1

merge m:1 exporter_iso3 prod_id year using "stri_ITPD_exp.dta"
rename _merge merge2

merge m:1 importer_iso3 prod_id year using "eea_stri_ITPD_imp.dta"
rename _merge merge3

merge m:1 exporter_iso3 prod_id year using "eea_stri_ITPD_exp.dta"
rename _merge merge4

foreach v of varlist merge*  {
    drop if `v' ==2
}

erase itpd_services_full.dta
erase dgd2.dta
erase itpd_services_gravity.dta
erase stri_ITPD.dta
erase stri_ITPD_imp.dta
erase stri_ITPD_exp.dta
erase eea_stri_ITPD.dta
erase eea_stri_ITPD_imp.dta
erase eea_stri_ITPD_exp.dta
save "$data_output/itpd_gravity_stri", replace








