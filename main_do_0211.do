

block

**# Set Directory
cd "C:\Users\danvs\Dropbox (Personal)\fake job posting"
global data "data"
global result "results"

* ************************************************************************* *
* ************************************************************************* *
*       Data preparation                                                    *
* ************************************************************************* *
* ************************************************************************* *
use "$data/bg_msa_qua0520.dta", clear


* Merge revelio data
merge 1:1 gvkey year quarter msa using "$data/revelio_qua_msa.dta"
keep if _merge==3
drop _merge


duplicates drop gvkey year quarter msa, force

**# merge with financial data
merge m:1 gvkey year quarter using "$data/financial_data.dta"

/*

    Result                      Number of obs
    -----------------------------------------
    Not matched                       577,095
        from master                    74,872  (_merge==1)
        from using                    502,223  (_merge==2)

    Matched                         1,073,687  (_merge==3)
    -----------------------------------------

*/


drop if fyear > 2021
drop if fyear < 2009
drop if _merge==2 & fyear >=2010 & fyear <=2020
drop _merge

save "C:\Users\danvs\Dropbox (Personal)\fake job posting\data\main_dataset_0211.dta", replace
use "$data/main_dataset_0211.dta", clear

* ************************************************************************* *
* ************************************************************************* *
*       Data processing                                                     *
* ************************************************************************* *
* ************************************************************************* *
// drop btm lev 
// g btm = (atq - ltq)/mkvaltq
// g lev = (dlcq+dlttq)/atq
// replace xsgaq_rev = 0  if xsgaq_rev==.
sum employment,d
// drop those with less than 5 employment
drop if employment < 5


* Create previous employment level
* 1. Create a unique panel ID for each (gvkey, msa) combination
egen panel_id = group(gvkey msa)

* 2. Declare panel data structure using 'panel_id' as the panel variable
*    and 'yq' as the time variable. 
*    (Assumes 'yq' is numeric and increments by 1 each period.)
duplicates drop panel_id yq, force
xtset panel_id yq
* 3. Generate the lagged variable: 'employment_pre' is the employment from the previous yq.
gen employment_pre = L.employment


sum employment employment_pre,d 
**# Indicators for posting gap


* 1. (posting - inflow)/employment

gen gap = jp - inflow
gen gap_pct = gap/employment_pre
winsor2 gap_pct
// sum  gap_pct_w,d 

winsor2 gap_pct, cuts(5 95) suffix(_5)
// sum gap_pct_5,d
// hist gap_pct_5
* 1. IHS and Log transformation

gen log_gap_pct = log(1+gap_pct_w)
sum log_gap_pct,d


* 3. above industry median/mean
bys year quarter msa gind: egen gap_pct_median = median(gap_pct_w)
gen gap_median_msa_dummy = gap_pct_w>gap_pct_median

bys year quarter msa gind: egen gap_pct_mean = mean(gap_pct_w)
gen gap_mean_msa_dummy = gap_pct_w>gap_pct_mean

* Keep the consistent sample across specifications
foreach var in gap_pct inst_pct atq btm lev revtq roa xsgaq_rev analyst_follow { 
    drop if missing(`var')
}

winsor2  log_gap_pct jp inflow outflow employment atq revtq xsgaq mkvaltq btm lev roa xsgaq_rev emp inst_pct 

**# Variables

* log transformation
gen ln_emp = log(employment_w)
gen ln_emp_y = log(emp_w)
gen ln_atq = log(atq_w)
gen ln_revtq = log(revtq_w)

* number of competitors within a MSA
egen group_id = group(year quarter msa)
egen gvkey_tag = tag(group_id gvkey)
egen com_num = total(gvkey_tag), by(group_id)
gen log_comp_num = log(com_num+1)

gen log_jp = log(jp_w+1)
gen log_inflow = log(inflow_w+1)
gen log_outflow = log(outflow_w+1)

// drop inst_pct_w
// replace inst_pct=0 if inst_pct==.
// winsor2 inst_pct




**# MSA Level: IO and gap
reghdfe gap_pct_w inst_pct_w , abs(msa gind#yq gvkey) vce(cl gvkey)
outreg2 using result/IO_msa.doc, replace tstat bdec(3) addtext(MSA FE, YES, Ind*Year-Quarter FE, YES)
reghdfe gap_pct_w inst_pct_w log_comp_num ln_atq  btm_w lev_w roa_w xsgaq_rev_w analyst_follow, abs(msa gind#yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_msa.doc, append tstat bdec(3) addtext(MSA FE, YES, Ind*Year-Quarter FE, YES, Firm FE, YES)
reghdfe gap_pct_5 inst_pct_w log_comp_num ln_atq  btm_w lev_w roa_w xsgaq_rev_w analyst_follow, abs(msa gind#yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_msa.doc, append tstat bdec(3) addtext(MSA FE, YES, Ind*Year-Quarter FE, YES, Firm FE, YES)
reghdfe gap_median_msa_dummy inst_pct_w log_comp_num ln_atq btm_w lev_w roa_w xsgaq_rev_w analyst_follow if gap_pct_w!=., abs(msa gind#yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_msa.doc, append tstat bdec(3) addtext(MSA FE, YES, Ind*Year-Quarter FE, YES, Firm FE, YES)


********************** ***
*** Firm-Quarter Level ***
********************** ***

bys gvkey year quarter: egen jp_q = sum(jp)
bys gvkey year quarter: egen inflow_q = sum(inflow)
bys gvkey year quarter: egen outflow_q = sum(outflow)
bys gvkey year quarter: egen competitor_q = mean(log_comp_num)
bys gvkey year quarter: egen employment_q = sum(employment)
bys gvkey year quarter: egen gap_pct_mean_q = mean(gap_pct)

drop log_jp* log_inflow* log_outflow* ln_emp

duplicates drop gvkey year quarter, force

sum employment_q,d
// drop if employment_q < 10

xtset gvkey yq
gen employment_pre_q = L.employment_q



save "C:\Users\danvs\Dropbox (Personal)\fake job posting\data\main_dataset_yq_0215.dta", replace

use "C:\Users\danvs\Dropbox (Personal)\fake job posting\data\main_dataset_yq_0215.dta", clear



****************************************
*** Gap measure at Quarter Level ***
****************************************
* 1. gap_qua
gen gap_q = jp_q - inflow_q
gen gap_pct_q = gap_q / employment_pre_q


sum gap_pct_q , d
winsor2 gap_pct_q
winsor2 gap_pct_q, cuts(5 95) suffix(_5)
sum gap_pct_q_w gap_pct_q_5 , d


* 2. log transformation
gen log_gap_pct_q = log(1+gap_pct_q)
winsor2 log_gap_pct_q
sum log_gap_pct_q_w if inst_pct!=.,d

* 3. Benchmark with the industry
bys year quarter gind: egen gap_pct_median_q = median(gap_pct_q_w)
bys year quarter gind: egen gap_pct_p75 = pctile(gap_pct_q_w), p(75)
bys year quarter gind: egen gap_cpt_mean = mean(gap_pct_q_w)


gen gap_dum_median = (gap_pct_q_w>=gap_pct_median_q)
gen gap_dum_mean = (gap_pct_q_w>=gap_cpt_mean)
gen gap_dum_75p = (gap_pct_q_w>=gap_pct_p75)

replace gap_dum_median = .  if gap_pct_q_w==.

* Global variable lists
global var_labor competitor_q
global firm_contrl ln_atq xsgaq_rev_w btm_w lev_w roa_w analyst_follow
global firm_contrl1 ln_revtq xsgaq_rev_w btm_w lev_w roa_w analyst_follow

* Keep the consistent sample across specifications
foreach var in $var_labor $firm_contrl $firm_contrl1 { 
    drop if missing(`var')
}

**# IO and gap_pct

* Binscatter Plot
binscatter  inst_pct_w gap_pct_q_w
binscatter  inst_pct_w gap_pct_q_w, absorb(gvkey)
binscatter  inst_pct_w gap_pct_q_5
binscatter  inst_pct_w gap_pct_q_5, absorb(gvkey)


* Regression
reghdfe gap_pct_q_w inst_pct_w , abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_yq.doc, replace tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe gap_pct_q_w inst_pct_w $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_yq.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe gap_pct_q_5 inst_pct_w , abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_yq.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe gap_pct_q_5 inst_pct_w $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_yq.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe gap_dum_median inst_pct_w $var_labor $firm_contrl , abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_yq.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

** Lead and Lag Analysis

xtset gvkey yq
reghdfe gap_pct_q_w l.inst_pct_w $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_leadlag.doc, replace tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe gap_pct_q_5 l.inst_pct_w $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_leadlag.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe gap_dum_median l.inst_pct_w $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_leadlag.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe inst_pct_w l.gap_pct_q_w  $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_leadlag1.doc, replace tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe inst_pct_w l.gap_pct_q_5  $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_leadlag1.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe inst_pct_w l.gap_dum_median  $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_leadlag1.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

**# Different types of IO

binscatter ded_pct gap_pct_q_w , absorb(gvkey)
binscatter tra_pct gap_pct_q_w , absorb(gvkey)

winsor2 ded_pct qix_pct tra_pct, replace

reghdfe gap_pct_q_w ded_pct qix_pct tra_pct, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_type.doc, replace tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe gap_pct_q_w ded_pct qix_pct tra_pct $var_labor  $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_type.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe gap_pct_q_5 ded_pct qix_pct tra_pct, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_type.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe gap_pct_q_5 ded_pct qix_pct tra_pct $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_type.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe gap_dum_median ded_pct qix_pct tra_pct $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_type.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)


reghdfe gap_pct_q_w l.ded_pct $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_type_leadlag.doc, replace tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe gap_pct_q_w l.tra_pct $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_type_leadlag.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe gap_pct_q_5 l.ded_pct $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_type_leadlag.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe gap_pct_q_5 l.tra_pct $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_type_leadlag.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe gap_dum_median l.ded_pct $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_type_leadlag.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe gap_dum_median l.tra_pct $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_type_leadlag.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

xtset gvkey yq
reghdfe ded_pct l.gap_pct_q_w $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_type_leadlag1.doc, replace tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe tra_pct l.gap_pct_q_w $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey)
outreg2 using result/IO_type_leadlag1.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES) 
reghdfe ded_pct l.gap_pct_q_5 $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey)
outreg2 using result/IO_type_leadlag1.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES) 
reghdfe tra_pct l.gap_pct_q_5 $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_type_leadlag1.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe ded_pct l.gap_dum_median $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_type_leadlag1.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe tra_pct l.gap_dum_median $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey)
outreg2 using result/IO_type_leadlag1.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)


************************************
******* Cross-sectional Test *******
************************************


**# Revenue Growth
xtset gvkey yq
gen rev_growth  = (f.revtq_w - revtq_w) / revtq_w
sum rev_growth if gap_pct_q_w!=.,d

egen revenue_50p = median(rev_growth)
gen high_rev = cond(rev_growth>=revenue_50p, 1,0)


est clear
qui{

eststo:reghdfe gap_pct_q_w inst_pct_w  $var_labor $firm_contrl if high_rev==0, abs(yq gvkey) vce(cl gvkey) 
eststo:reghdfe gap_pct_q_w inst_pct_w  $var_labor $firm_contrl if  high_rev==1, abs(yq gvkey) vce(cl gvkey) 
eststo:reghdfe gap_pct_q_5 inst_pct_w  $var_labor $firm_contrl if  high_rev==0, abs(yq gvkey) vce(cl gvkey) 
eststo:reghdfe gap_pct_q_5 inst_pct_w  $var_labor $firm_contrl if  high_rev==1, abs(yq gvkey) vce(cl gvkey) 
}
esttab est*, replace legend b(3) t(2) ar2 label nogap star(* 0.10 ** .05 *** 0.01) ///
    stats( N r2_a, fmt( %15.0g %9.4f) labels("Observations" "Adjusted R2")) ///
    nonumber dep  


reghdfe gap_pct_q_w inst_pct_w  $var_labor $firm_contrl if high_rev==0, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_rev_g.doc, replace tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe gap_pct_q_w inst_pct_w  $var_labor $firm_contrl if high_rev==1, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_rev_g.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe gap_pct_q_5 inst_pct_w  $var_labor $firm_contrl if high_rev==0, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_rev_g.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe gap_pct_q_5 inst_pct_w  $var_labor $firm_contrl if high_rev==1, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_rev_g.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)


**# BTM
sum btm if gap_pct_q_w!=.,d

egen btm_50p = median(btm)
gen high_btm = cond(btm>=btm_50p, 1,0)


est clear
qui{
eststo:reghdfe gap_pct_q_w inst_pct_w  $var_labor $firm_contrl if high_btm==0, abs(yq gvkey) vce(cl gvkey) 
eststo:reghdfe gap_pct_q_w inst_pct_w  $var_labor $firm_contrl if high_btm==1, abs(yq gvkey) vce(cl gvkey) 
eststo:reghdfe gap_pct_q_5 inst_pct_w  $var_labor $firm_contrl if high_btm==0, abs(yq gvkey) vce(cl gvkey) 
eststo:reghdfe gap_pct_q_5 inst_pct_w  $var_labor $firm_contrl if high_btm==1, abs(yq gvkey) vce(cl gvkey) 
}
esttab est*, replace legend b(3) t(2) ar2 label nogap star(* 0.10 ** .05 *** 0.01) ///
    stats( N r2_a, fmt( %15.0g %9.4f) labels("Observations" "Adjusted R2")) ///
    nonumber dep  

reghdfe gap_pct_q_w inst_pct_w  $var_labor $firm_contrl if high_btm==0, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_btm.doc, replace tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe gap_pct_q_w inst_pct_w  $var_labor $firm_contrl if high_btm==1, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_btm.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe gap_pct_q_5 inst_pct_w  $var_labor $firm_contrl if high_btm==0, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_btm.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe gap_pct_q_5 inst_pct_w  $var_labor $firm_contrl if high_btm==1, abs(yq gvkey) vce(cl gvkey) 
outreg2 using result/IO_btm.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)



************************************
	*** Market Reaction ***
************************************

xtset gvkey yq

reghdfe ex_ret_eql1_w gap_pct_q_w inst_pct_w $var_labor $firm_contrl Amihud_q baspread_q, abs(gvkey yq) cluster(gvkey)
outreg2 using result/car.doc, replace tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe f.ex_ret_eql1_w gap_pct_q_w inst_pct_w $var_labor $firm_contrl Amihud_q baspread_q, abs(gvkey yq) cluster(gvkey)
outreg2 using result/car.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe f2.ex_ret_eql1_w gap_pct_q_w inst_pct_w $var_labor $firm_contrl Amihud_q baspread_q, abs(gvkey yq) cluster(gvkey)
outreg2 using result/car.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe f3.ex_ret_eql1_w gap_pct_q_w inst_pct_w $var_labor $firm_contrl Amihud_q baspread_q, abs(gvkey yq) cluster(gvkey)
outreg2 using result/car.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe f4.ex_ret_eql1_w gap_pct_q_w inst_pct_w $var_labor $firm_contrl Amihud_q baspread_q, abs(gvkey yq) cluster(gvkey)
outreg2 using result/car.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe ex_ret_eql1_w gap_pct_q_5 inst_pct_w $var_labor $firm_contrl Amihud_q baspread_q, abs(gvkey yq) cluster(gvkey)
outreg2 using result/car1.doc, replace tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe f.ex_ret_eql1_w gap_pct_q_5 inst_pct_w $var_labor $firm_contrl Amihud_q baspread_q, abs(gvkey yq) cluster(gvkey)
outreg2 using result/car1.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe f2.ex_ret_eql1_w gap_pct_q_5 inst_pct_w $var_labor $firm_contrl Amihud_q baspread_q, abs(gvkey yq) cluster(gvkey)
outreg2 using result/car1.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe f3.ex_ret_eql1_w gap_pct_q_5 inst_pct_w $var_labor $firm_contrl Amihud_q baspread_q, abs(gvkey yq) cluster(gvkey)
outreg2 using result/car1.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe f4.ex_ret_eql1_w gap_pct_q_5 inst_pct_w $var_labor $firm_contrl Amihud_q baspread_q, abs(gvkey yq) cluster(gvkey)
outreg2 using result/car1.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)



************************************
	*** Algorithic trading ***
************************************
winsor2 at
reghdfe  gap_pct_q_w at_w, abs(gvkey yq) cluster(gvkey)
outreg2 using result/at.doc, replace tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe  gap_pct_q_w at_w  $var_labor $firm_contrl inst_pct_w ln_mktval ln_price ln_ret_vol, abs(gvkey yq) cluster(gvkey)
outreg2 using result/at.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe  gap_pct_q_5 at_w, abs(gvkey yq) cluster(gvkey)
outreg2 using result/at.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe  gap_pct_q_5 at_w  $var_labor $firm_contrl inst_pct_w ln_mktval ln_price ln_ret_vo, abs(gvkey yq) cluster(gvkey)
outreg2 using result/at.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)


xtset gvkey yq

reghdfe gap_pct_q_w l.at $var_labor $firm_contrl  inst_pct_w , abs(gvkey yq) cluster(gvkey)
outreg2 using result/at1.doc, replace tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe gap_pct_q_5 l.at $var_labor $firm_contrl  inst_pct_w , abs(gvkey yq) cluster(gvkey)
outreg2 using result/at1.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe at l.gap_pct_q_w  $var_labor $firm_contrl  inst_pct_w , abs(gvkey yq) cluster(gvkey)
outreg2 using result/at1.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)
reghdfe  at l.gap_pct_q_5  $var_labor $firm_contrl  inst_pct_w , abs(gvkey yq) cluster(gvkey)
outreg2 using result/at1.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)



reghdfe gap_pct_q_w trade_order $var_labor $firm_contrl  inst_pct_w , abs(gvkey yq) cluster(gvkey)
reghdfe gap_pct_q_w odd_lot $var_labor $firm_contrl  inst_pct_w , abs(gvkey yq) cluster(gvkey)
reghdfe gap_pct_q_w cancel_trade $var_labor $firm_contrl  inst_pct_w , abs(gvkey yq) cluster(gvkey)
reghdfe gap_pct_q_w trade_size_avg $var_labor $firm_contrl  inst_pct_w , abs(gvkey yq) cluster(gvkey)




