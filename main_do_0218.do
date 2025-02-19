block


/* ============================================================================
   GHOST JOB ANALYSIS - MAIN SCRIPT
   
   This script performs data preparation, processing, and analysis for ghost job 
   research examining the relationship between institutional ownership (IO) and 
   job posting gaps, using both MSA (Metropolitan Statistical Area) and 
   firm-quarter level analyses.
   
   Structure:
   1. Environment setup and directory specification
   2. Data preparation (merging relevant datasets)
   3. Data processing (cleaning, variable creation)
   4. MSA-level analysis of IO and posting gaps
   5. Firm-quarter level analysis of IO and posting gaps
   6. Cross-sectional tests
   7. Market reaction analysis
   ============================================================================ */

/* ============================================================================
   1. ENVIRONMENT SETUP
   ============================================================================ */
cd "C:\Users\danvs\Dropbox (Personal)\ghost_job"  // Change to your directory 
global data "data"
global result "results"


/* ============================================================================
   2. DATA PREPARATION
   ============================================================================ */
use "$data/bg_msa_qua0520.dta", clear

// Merge revelio data at MSA level
merge 1:1 gvkey year quarter msa using "$data/revelio_qua_msa.dta"
keep if _merge==3
drop _merge

// Remove duplicates at the gvkey-year-quarter-msa level
duplicates drop gvkey year quarter msa, force

// Merge with financial data
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

// Keep data within our study period (2009-2021)
drop if fyear > 2021
drop if fyear < 2009
drop if _merge==2 & fyear >=2010 & fyear <=2020
drop _merge

// Save the merged dataset
save "$data/main_dataset_0211.dta", replace
use "$data/main_dataset_0211.dta", clear



/* ============================================================================
   3. DATA PROCESSING
   ============================================================================ */
// Display employment statistics
sum employment, d
// Remove small firms or branches (less than 5 employees in a msa)
drop if employment < 5

// Create panel structure for tracking employment over time
// 1. Create a unique panel ID for each (gvkey, msa) combination
egen panel_id = group(gvkey msa)

// 2. Declare panel data structure using 'panel_id' as the panel variable
//    and 'yq' as the time variable
duplicates drop panel_id yq, force
xtset panel_id yq

// 3. Generate lagged employment variable to track changes
gen employment_pre = L.employment

// Summarize employment variables
sum employment employment_pre, d



/* ----------------------------------------------------------------------------
   CREATING POSTING GAP INDICATORS
   ----------------------------------------------------------------------------*/
// 1. Calculate posting gap and normalize by previous employment
gen gap = jp - inflow
gen gap_pct = gap/employment_pre
winsor2 gap_pct  // Winsorize to handle outliers
winsor2 gap_pct, cuts(5 95) suffix(_5)  // More conservative winsorization

// 3. Create indicators for gaps above industry median/mean
bys year quarter msa gind: egen gap_pct_median = median(gap_pct_w)
gen gap_median_msa_dummy = gap_pct_w > gap_pct_median

bys year quarter msa gind: egen gap_pct_mean = mean(gap_pct_w)
gen gap_mean_msa_dummy = gap_pct_w > gap_pct_mean

// Keep consistent sample by dropping observations with missing variables
foreach var in gap_pct inst_pct atq btm lev revtq roa xsgaq_rev analyst_follow { 
    drop if missing(`var')
}

// Winsorize key variables to address outliers
winsor2 jp inflow outflow employment atq revtq xsgaq mkvaltq btm lev roa xsgaq_rev emp inst_pct 


/* ----------------------------------------------------------------------------
   VARIABLE TRANSFORMATIONS
   ----------------------------------------------------------------------------*/
// Log transformations for better distributional properties
gen ln_emp = log(employment_w)
gen ln_emp_y = log(emp_w)
gen ln_atq = log(atq_w)
gen ln_revtq = log(revtq_w)

// Calculate number of competitors within each MSA
egen group_id = group(year quarter msa)
egen gvkey_tag = tag(group_id gvkey)
egen com_num = total(gvkey_tag), by(group_id)
gen log_comp_num = log(com_num+1)


/* ============================================================================
   4. MSA-LEVEL ANALYSIS: IO AND POSTING GAPS
   ============================================================================ */
// Base model with firm, MSA, and industry-time fixed effects
reghdfe gap_pct_w inst_pct_w, abs(msa gind#yq gvkey) vce(cl gvkey)
outreg2 using $result/IO_msa.doc, replace tstat bdec(3) addtext(MSA FE, YES, Ind*Year-Quarter FE, YES)

// Full model with controls
reghdfe gap_pct_w inst_pct_w log_comp_num ln_atq btm_w lev_w roa_w xsgaq_rev_w analyst_follow, abs(msa gind#yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_msa.doc, append tstat bdec(3) addtext(MSA FE, YES, Ind*Year-Quarter FE, YES, Firm FE, YES)

// Alternative gap measure (winsorized at 5-95)
reghdfe gap_pct_5 inst_pct_w log_comp_num ln_atq btm_w lev_w roa_w xsgaq_rev_w analyst_follow, abs(msa gind#yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_msa.doc, append tstat bdec(3) addtext(MSA FE, YES, Ind*Year-Quarter FE, YES, Firm FE, YES)

// Binary gap measure (above/below median)
reghdfe gap_median_msa_dummy inst_pct_w log_comp_num ln_atq btm_w lev_w roa_w xsgaq_rev_w analyst_follow if gap_pct_w!=., abs(msa gind#yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_msa.doc, append tstat bdec(3) addtext(MSA FE, YES, Ind*Year-Quarter FE, YES, Firm FE, YES)





/* ============================================================================
   5. FIRM-QUARTER LEVEL ANALYSIS
   ============================================================================ */

/* ----------------------------------------------------------------------------
   5A. DATA AGGREGATION TO FIRM-QUARTER LEVEL
   ----------------------------------------------------------------------------*/
// Aggregate job metrics to firm-quarter level
bys gvkey year quarter: egen jp_q = sum(jp)
bys gvkey year quarter: egen inflow_q = sum(inflow)
bys gvkey year quarter: egen outflow_q = sum(outflow)
bys gvkey year quarter: egen competitor_q = mean(log_comp_num)
bys gvkey year quarter: egen employment_q = sum(employment)
bys gvkey year quarter: egen gap_pct_mean_q = mean(gap_pct)

// Keep one observation per firm-quarter
duplicates drop gvkey year quarter, force

// Summarize firm-quarter employment and create lagged variable
sum employment_q, d
xtset gvkey yq
gen employment_pre_q = L.employment_q

// Save firm-quarter dataset
save "$data/main_dataset_yq_0215.dta", replace
use "$data/main_dataset_yq_0215.dta", clear



/* ----------------------------------------------------------------------------
   5B. FIRM-QUARTER GAP MEASURES
   ----------------------------------------------------------------------------*/
// 1. Absolute and percentage gap measures
gen gap_q = jp_q - inflow_q
gen gap_pct_q = gap_q / employment_pre_q

// Winsorize to handle outliers
sum gap_pct_q, d
winsor2 gap_pct_q
winsor2 gap_pct_q, cuts(5 95) suffix(_5)
sum gap_pct_q_w gap_pct_q_5, d

// 2. Log transformation of gap percentage
gen log_gap_pct_q = log(1+gap_pct_q)
winsor2 log_gap_pct_q
sum log_gap_pct_q_w if inst_pct!=., d

// 3. Industry benchmarking measures
bys year quarter gind: egen gap_pct_median_q = median(gap_pct_q_w)
bys year quarter gind: egen gap_pct_p75 = pctile(gap_pct_q_w), p(75)
bys year quarter gind: egen gap_cpt_mean = mean(gap_pct_q_w)

// Binary indicators for gap position relative to industry
gen gap_dum_median = (gap_pct_q_w >= gap_pct_median_q)
gen gap_dum_mean = (gap_pct_q_w >= gap_cpt_mean)
gen gap_dum_75p = (gap_pct_q_w >= gap_pct_p75)
replace gap_dum_median = . if gap_pct_q_w == .

// Define global variables for regression models
global var_labor competitor_q
global firm_contrl ln_atq xsgaq_rev_w btm_w lev_w roa_w analyst_follow

// Keep consistent sample across specifications
foreach var in $var_labor $firm_contrl { 
    drop if missing(`var')
}

/* ----------------------------------------------------------------------------
   5C. RELATIONSHIP BETWEEN IO AND POSTING GAPS
   ----------------------------------------------------------------------------*/
// Visual analysis with binscatter plots
binscatter inst_pct_w gap_pct_q_w
binscatter inst_pct_w gap_pct_q_w, absorb(gvkey)
binscatter inst_pct_w gap_pct_q_5
binscatter inst_pct_w gap_pct_q_5, absorb(gvkey)

// Main regression models with different gap measures
reghdfe gap_pct_q_w inst_pct_w, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_yq.doc, replace tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe gap_pct_q_w inst_pct_w $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_yq.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe gap_pct_q_5 inst_pct_w, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_yq.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe gap_pct_q_5 inst_pct_w $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_yq.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe gap_dum_median inst_pct_w $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_yq.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)


/* ----------------------------------------------------------------------------
   5D. LEAD-LAG ANALYSIS
   ----------------------------------------------------------------------------*/
// Effect of lagged IO on current posting gaps
xtset gvkey yq
reghdfe gap_pct_q_w l.inst_pct_w $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_leadlag.doc, replace tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe gap_pct_q_5 l.inst_pct_w $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_leadlag.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe gap_dum_median l.inst_pct_w $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_leadlag.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

// Effect of lagged posting gaps on current IO
reghdfe inst_pct_w l.gap_pct_q_w $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_leadlag1.doc, replace tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe inst_pct_w l.gap_pct_q_5 $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_leadlag1.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe inst_pct_w l.gap_dum_median $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_leadlag1.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

/* ----------------------------------------------------------------------------
   5E. ANALYSIS BY IO TYPE
   ----------------------------------------------------------------------------*/
// Visual analysis by IO type
binscatter ded_pct gap_pct_q_w, absorb(gvkey)  // Dedicated IO
binscatter tra_pct gap_pct_q_w, absorb(gvkey)  // Transient IO

// Winsorize IO type variables
winsor2 ded_pct qix_pct tra_pct, replace

// Regression models by IO type
reghdfe gap_pct_q_w ded_pct qix_pct tra_pct, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_type.doc, replace tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe gap_pct_q_w ded_pct qix_pct tra_pct $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_type.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe gap_pct_q_5 ded_pct qix_pct tra_pct, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_type.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe gap_pct_q_5 ded_pct qix_pct tra_pct $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_type.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe gap_dum_median ded_pct qix_pct tra_pct $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_type.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

// Lead-lag analysis by IO type: Dedicated or Transient IO to ghost postings
reghdfe gap_pct_q_w l.ded_pct $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_type_leadlag.doc, replace tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe gap_pct_q_w l.tra_pct $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_type_leadlag.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe gap_pct_q_5 l.ded_pct $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_type_leadlag.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe gap_pct_q_5 l.tra_pct $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_type_leadlag.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe gap_dum_median l.ded_pct $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_type_leadlag.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe gap_dum_median l.tra_pct $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_type_leadlag.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

// Lead-lag analysis by IO type: ghost postings to Dedicated or Transient IO
xtset gvkey yq
reghdfe ded_pct l.gap_pct_q_w $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_type_leadlag1.doc, replace tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe tra_pct l.gap_pct_q_w $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey)
outreg2 using $result/IO_type_leadlag1.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES) 

reghdfe ded_pct l.gap_pct_q_5 $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey)
outreg2 using $result/IO_type_leadlag1.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES) 

reghdfe tra_pct l.gap_pct_q_5 $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_type_leadlag1.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe ded_pct l.gap_dum_median $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_type_leadlag1.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe tra_pct l.gap_dum_median $var_labor $firm_contrl, abs(yq gvkey) vce(cl gvkey)
outreg2 using $result/IO_type_leadlag1.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)



/* ============================================================================
   6. CROSS-SECTIONAL TESTS
   ============================================================================ */

/* ----------------------------------------------------------------------------
   6A. TESTS BY REVENUE GROWTH
   ----------------------------------------------------------------------------*/
// Generate revenue growth variable and median split
xtset gvkey yq
gen rev_growth = (f.revtq_w - revtq_w) / revtq_w
sum rev_growth if gap_pct_q_w!=., d

egen revenue_50p = median(rev_growth)
gen high_rev = cond(rev_growth>=revenue_50p, 1, 0)

// Run models separately for high and low growth firms
est clear
qui{
eststo: reghdfe gap_pct_q_w inst_pct_w $var_labor $firm_contrl if high_rev==0, abs(yq gvkey) vce(cl gvkey) 
eststo: reghdfe gap_pct_q_w inst_pct_w $var_labor $firm_contrl if high_rev==1, abs(yq gvkey) vce(cl gvkey) 
eststo: reghdfe gap_pct_q_5 inst_pct_w $var_labor $firm_contrl if high_rev==0, abs(yq gvkey) vce(cl gvkey) 
eststo: reghdfe gap_pct_q_5 inst_pct_w $var_labor $firm_contrl if high_rev==1, abs(yq gvkey) vce(cl gvkey) 
}
esttab est*, replace legend b(3) t(2) ar2 label nogap star(* 0.10 ** .05 *** 0.01) ///
    stats(N r2_a, fmt(%15.0g %9.4f) labels("Observations" "Adjusted R2")) ///
    nonumber dep  

// Export results with outreg2
reghdfe gap_pct_q_w inst_pct_w $var_labor $firm_contrl if high_rev==0, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_rev_g.doc, replace tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe gap_pct_q_w inst_pct_w $var_labor $firm_contrl if high_rev==1, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_rev_g.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe gap_pct_q_5 inst_pct_w $var_labor $firm_contrl if high_rev==0, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_rev_g.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe gap_pct_q_5 inst_pct_w $var_labor $firm_contrl if high_rev==1, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_rev_g.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)



/* ----------------------------------------------------------------------------
   6B. TESTS BY BOOK-TO-MARKET RATIO
   ----------------------------------------------------------------------------*/
// Generate BTM median split
sum btm if gap_pct_q_w!=., d
egen btm_50p = median(btm)
gen high_btm = cond(btm>=btm_50p, 1, 0)

// Run models separately for high and low BTM firms
est clear
qui{
eststo: reghdfe gap_pct_q_w inst_pct_w $var_labor $firm_contrl if high_btm==0, abs(yq gvkey) vce(cl gvkey) 
eststo: reghdfe gap_pct_q_w inst_pct_w $var_labor $firm_contrl if high_btm==1, abs(yq gvkey) vce(cl gvkey) 
eststo: reghdfe gap_pct_q_5 inst_pct_w $var_labor $firm_contrl if high_btm==0, abs(yq gvkey) vce(cl gvkey) 
eststo: reghdfe gap_pct_q_5 inst_pct_w $var_labor $firm_contrl if high_btm==1, abs(yq gvkey) vce(cl gvkey) 
}
esttab est*, replace legend b(3) t(2) ar2 label nogap star(* 0.10 ** .05 *** 0.01) ///
    stats(N r2_a, fmt(%15.0g %9.4f) labels("Observations" "Adjusted R2")) ///
    nonumber dep  

// Export results with outreg2
reghdfe gap_pct_q_w inst_pct_w $var_labor $firm_contrl if high_btm==0, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_btm.doc, replace tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe gap_pct_q_w inst_pct_w $var_labor $firm_contrl if high_btm==1, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_btm.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe gap_pct_q_5 inst_pct_w $var_labor $firm_contrl if high_btm==0, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_btm.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe gap_pct_q_5 inst_pct_w $var_labor $firm_contrl if high_btm==1, abs(yq gvkey) vce(cl gvkey) 
outreg2 using $result/IO_btm.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)


/* ============================================================================
   7. MARKET REACTION ANALYSIS
   ============================================================================ */
xtset gvkey yq

// Market reaction to posting gaps (concurrent quarter)
reghdfe ex_ret_eql1_w gap_pct_q_w inst_pct_w $var_labor $firm_contrl Amihud_q baspread_q, abs(gvkey yq) cluster(gvkey)
outreg2 using $result/car.doc, replace tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

// Market reaction over subsequent quarters (t+1 to t+4)
reghdfe f.ex_ret_eql1_w gap_pct_q_w inst_pct_w $var_labor $firm_contrl Amihud_q baspread_q, abs(gvkey yq) cluster(gvkey)
outreg2 using $result/car.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe f2.ex_ret_eql1_w gap_pct_q_w inst_pct_w $var_labor $firm_contrl Amihud_q baspread_q, abs(gvkey yq) cluster(gvkey)
outreg2 using $result/car.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe f3.ex_ret_eql1_w gap_pct_q_w inst_pct_w $var_labor $firm_contrl Amihud_q baspread_q, abs(gvkey yq) cluster(gvkey)
outreg2 using $result/car.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe f4.ex_ret_eql1_w gap_pct_q_w inst_pct_w $var_labor $firm_contrl Amihud_q baspread_q, abs(gvkey yq) cluster(gvkey)
outreg2 using $result/car.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

// Alternative gap measure (winsorized at 5-95)
reghdfe ex_ret_eql1_w gap_pct_q_5 inst_pct_w $var_labor $firm_contrl Amihud_q baspread_q, abs(gvkey yq) cluster(gvkey)
outreg2 using $result/car1.doc, replace tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe f.ex_ret_eql1_w gap_pct_q_5 inst_pct_w $var_labor $firm_contrl Amihud_q baspread_q, abs(gvkey yq) cluster(gvkey)
outreg2 using $result/car1.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe f2.ex_ret_eql1_w gap_pct_q_5 inst_pct_w $var_labor $firm_contrl Amihud_q baspread_q, abs(gvkey yq) cluster(gvkey)
outreg2 using $result/car1.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe f3.ex_ret_eql1_w gap_pct_q_5 inst_pct_w $var_labor $firm_contrl Amihud_q baspread_q, abs(gvkey yq) cluster(gvkey)
outreg2 using $result/car1.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)

reghdfe f4.ex_ret_eql1_w gap_pct_q_5 inst_pct_w $var_labor $firm_contrl Amihud_q baspread_q, abs(gvkey yq) cluster(gvkey)
outreg2 using $result/car1.doc, append tstat bdec(3) addtext(Firm FE, YES, Year-Quarter FE, YES)


/* ============================================================================
   8. Algorithic trading
   ============================================================================ */
   
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
   