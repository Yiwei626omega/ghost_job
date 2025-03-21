/*===========================================================================*/
/* DESCRIPTIVE AND QUINTILE ANALYSIS                                         */
/* Author: Research Team                                                     */
/* Created: March 3, 2025                                                    */
/* Updated: March 4, 2025                                                    */              
/*===========================================================================*/

/*===========================================================================*/
/* DATA PREPARATION                                                          */
/*===========================================================================*/

* Set working directory and define global paths
cd "C:\Users\danvs\Dropbox (Personal)\ghost_job"  // Change according to your dropbox folder
global data "data"
global result "results"

* Load dataset and handle outliers
use "$data/yq_0303.dta", clear

merge 1:1 gvkey year quarter using "$data/msa_count.dta"
drop if _merge==2
drop _merge

// measures are winsorized at 1%,99%, with suffix _w


/*===========================================================================*/
/* SECTION 0: General trend graph of gap_pct_w over time                     */
/*===========================================================================*/

* This code aggregates data to quarter level and plots the trend

/*---------------------------------------------------------------------------*/
/* Trend of Gap                                							     */
/*---------------------------------------------------------------------------*/
* Collapse data to quarter level, summing the key variables
preserve
collapse (sum) jp_q_w inflow_q_w employment_pre_q_w, by(yq)

* Generate gap_pct_w at the aggregate level for each quarter
gen gap_pct = (jp_q_w - inflow_q_w) / employment_pre_q_w // Multiply by 100 to get percentage

* Generate a date variable for better x-axis formatting
gen date = qofd(dofq(yq))
format date %tq

* Create line graph of the trend
twoway (line gap_pct date if yq>=201 & yq < 244, lcolor(navy) lwidth(medium)), ///
       title("Trend in Gap % Over Time", size(medium)) ///
       subtitle("Calculated as (Job Postings - Inflows)/Pre-period Employment") ///
       xtitle("Quarter") ///
       ytitle("Gap %") ///
       ylabel(, angle(horizontal)) ///
       tlabel(, labsize(small)) ///
       legend(off)

* Export graph if needed
graph export gap_trend.png, replace width(1200) height(800)

* Restore original dataset
restore

/*---------------------------------------------------------------------------*/
/* Trend of Job Postings                                					 */
/*---------------------------------------------------------------------------*/

* General trend graph of gap_pct_w over time
* This code aggregates data to quarter level and plots the trend

* Collapse data to quarter level, summing the key variables
preserve
collapse (sum) jp_q_w, by(yq)

* Generate a date variable for better x-axis formatting
gen date = qofd(dofq(yq))
format date %tq

* Create line graph of the trend
twoway (line jp_q_w date if yq>=201 & yq < 244, lcolor(navy) lwidth(medium)), ///
       title("Trend in Job Postings Over Time", size(medium)) ///
       xtitle("Quarter") ///
       ytitle("Total Job Postings") ///
       ylabel(, angle(horizontal)) ///
       tlabel(, labsize(small)) ///
       legend(off)

* Export graph if needed
graph export jp_trend.png, replace width(1200) height(800)

* Restore original dataset
restore

* General trend graph of gap_pct_w and industry over time
* This code aggregates data to quarter level and plots the trend

**# By Industries

* Collapse data to quarter level, summing the key variables
preserve
collapse (sum) jp_q_w inflow_q_w employment_pre_q_w, by(gsector)

* Generate gap_pct_w at the aggregate level for each quarter
gen gap_pct = (jp_q_w - inflow_q_w) / employment_pre_q_w * 100 // Multiply by 100 to get percentage

* Define value labels for gsector based on GICS sector codes
label define gsector_lbl ///
    10 "Energy" ///
    15 "Materials" ///
    20 "Industrials" ///
    25 "Consumer Discretionary" ///
    30 "Consumer Staples" ///
    35 "Health Care" ///
    40 "Financials" ///
    45 "Information Technology" ///
    50 "Communication Services" ///
    55 "Utilities" ///
    60 "Real Estate"

* Assign the label to the gsector variable
label values gsector gsector_lbl
graph bar gap_pct, over(gsector, label(angle(45))) ///
    title("Average Ghost Job Rate by Industry") ytitle("Gap Percentage")
* Export graph if needed
graph export gap_industry_trend.png, replace width(1200) height(800)
* Restore original dataset
restore


/*===========================================================================*/
/* SECTION 1: QUINTILE ANALYSIS                                              */
/*===========================================================================*/

/*---------------------------------------------------------------------------*/
/* 1.1 Time-Varying Quintile Classification                                  */
/*---------------------------------------------------------------------------*/

* Ensure data is properly sorted
sort gvkey yq

/* Create quintiles for each year-quarter
   - This allows quintile boundaries to change over time
   - Captures relative position within each period */
bysort yq: egen gap_quintile = xtile(gap_pct_q_w), nq(5)

* Add descriptive labels to quintiles
label define quintile_lbl 1 "Q1 (Lowest)" 2 "Q2" 3 "Q3" 4 "Q4" 5 "Q5 (Highest)"
label values gap_quintile quintile_lbl

* Calculate mean gap within each year-quarter and quintile
preserve
collapse (mean) avg_gap=gap_pct_q_w (count) n_firms=gvkey, by(yq gap_quintile)

twoway (line avg_gap yq if gap_quintile==1, sort lcolor(navy) lpattern(solid) lwidth(medthick)) ///
       (line avg_gap yq if gap_quintile==2, sort lcolor(blue) lpattern(solid) lwidth(medthick)) ///
       (line avg_gap yq if gap_quintile==3, sort lcolor(green) lpattern(solid) lwidth(medthick)) ///
       (line avg_gap yq if gap_quintile==4, sort lcolor(orange) lpattern(solid) lwidth(medthick)) ///
       (line avg_gap yq if gap_quintile==5, sort lcolor(purple) lpattern(solid) lwidth(medthick)), ///
       title("Gap Measure by Quintile Over Time") ///
       subtitle("Quarterly Means") ///
       xtitle("Year-Quarter") ytitle("Average Gap Measure") ///
       legend(order(1 "Q1 (Lowest)" 2 "Q2" 3 "Q3" 4 "Q4" 5 "Q5 (Highest)")) ///
       xlabel(#20, angle(45) valuelabel)
graph export quintile_progression.png, replace width(2000) height(1200)
restore

/*---------------------------------------------------------------------------*/
/* 1.2 Quintile Persistence Analysis                                         */
/*---------------------------------------------------------------------------*/

/* Analyze how firms move between quintiles over time
   - What percentage of firms stay in the same quintile?
   - How many firms move up or down in quintile ranking?
   - Are certain quintiles more "sticky" than others? */

* Generate lagged quintile to track quarter-to-quarter movement
tsset gvkey yq
gen lag_quintile = l.gap_quintile

* Create transition matrix showing movement between quintiles
* Row percentages show probability of moving from one quintile to another
tab gap_quintile lag_quintile, row

* Calculate persistence and movement metrics for each firm-quarter
gen same_quintile = (gap_quintile == lag_quintile) if !missing(lag_quintile)
gen moved_up = (gap_quintile > lag_quintile) if !missing(lag_quintile)
gen moved_down = (gap_quintile < lag_quintile) if !missing(lag_quintile)

* Summarize persistence metrics by quintile
* This shows if certain quintiles are more stable than others
tabstat same_quintile moved_up moved_down, by(gap_quintile) stat(mean n) nototal

* Create time series of quintile persistence
preserve
    * Collapse to quarterly level to track overall persistence over time
    collapse (mean) pct_same=same_quintile (count) n_firms=gvkey, by(yq)
    
    * Filter to relevant analysis period
    keep if yq>=200 & yq < 244
    
    * Create a variable to identify first quarter of each year for x-axis labeling
    gen q1_label = mod(quarter(dofq(yq)),4) == 1
    
    * Visualize quintile persistence over time
    twoway (line pct_same yq, sort lcolor(navy) lwidth(medthick)), ///
           title("Quintile Persistence Over Time", size(large) color(black)) ///
           subtitle("Percentage of Firms Remaining in Same Quintile", size(medium)) ///
           xtitle("Year-Quarter", size(medium)) ytitle("Percentage", size(medium)) ///
           ylabel(0(0.1)1, format(%3.1f)) ///
           xlabel(#20, angle(45) valuelabel) ///
           scheme(s1color) name(persistence, replace) ///
           note("Source: Analysis Data", size(small))
           
    * Export chart as high-resolution image
    graph export "quintile_persistence.png", replace width(2000) height(1200)
restore

* Create time series of quintile persistence
preserve
collapse (mean) pct_same=same_quintile (count) n_firms=gvkey, by(yq)

* Create a variable to identify first quarter of each year for x-axis labeling
gen q1_label = mod(quarter(dofq(yq)),4) == 1

* Graph quintile persistence over time
twoway (line pct_same yq if yq>=200 & yq < 244, sort), ///
       title("Quintile Persistence Over Time") ///
       subtitle("Percentage of Firms Remaining in Same Quintile") ///
       xtitle("Year-Quarter") ytitle("Percentage") ///
       ylabel(0(0.1)1, format(%3.1f)) ///
       xlabel(#20, angle(45) valuelabel)
graph export quintile_persistence.png, replace width(2000) height(1200)
restore

/*===========================================================================*/
/* SECTION 2: FIXED-PERIOD QUINTILE ANALYSIS                                 */
/*===========================================================================*/

/*---------------------------------------------------------------------------*/
/* 2.1 2010 Q2 Base Period Classification                                    */
/*---------------------------------------------------------------------------*/

preserve    
    * Ensure data is sorted
    sort gvkey yq
      
    * Keep only the starting quarter (2010Q2) for quintile assignment
    keep if year == 2010 & quarter == 2
    
    * Create quintiles based on gap_pct_q_w in the starting quarter
    xtile quintile_2010q2 = gap_pct_q_w, nq(5)
    
    * Keep only necessary variables for merging
    keep gvkey quintile_2010q2
    
    * Save temporary file with quintile assignments
    tempfile quintiles
    save `quintiles'
restore

* Merge quintile assignments back to main dataset
merge m:1 gvkey using `quintiles'

* Keep matched observations and unmatched from master
keep if _merge == 3 | _merge == 1
drop _merge

* Apply quintile labels
label values quintile_2010q2 quintile_lbl

* Calculate average gap by quintile and quarter
preserve
    * Collapse to quintile-quarter level
    collapse (mean) avg_gap_pct_q_w=gap_pct_q_w, by(quintile_2010q2 yq)
    
    * Keep only relevant time period
    keep if yq>=200 & yq < 244
    
    * Create formatted date variable for improved labeling
    gen date_str = string(yq, "%tq")
    
    * Create trend chart by 2010Q2 quintile
    twoway (line avg_gap_pct_q_w yq if quintile_2010q2==1, lcolor(blue)) ///
           (line avg_gap_pct_q_w yq if quintile_2010q2==2, lcolor(green)) ///
           (line avg_gap_pct_q_w yq if quintile_2010q2==3, lcolor(orange)) ///
           (line avg_gap_pct_q_w yq if quintile_2010q2==4, lcolor(red)) ///
           (line avg_gap_pct_q_w yq if quintile_2010q2==5, lcolor(purple)), ///
           title("Trends in Gap Posting by Initial 2010Q2 Quintile") ///
           subtitle("Firms categorized based on 2010Q2 gap_pct_q_w values") ///          
           xtitle("Year-Quarter") ytitle("Average gap_pct_q_w") ///
           legend(order(1 "Q1 (Lowest)" 2 "Q2" 3 "Q3" 4 "Q4" 5 "Q5 (Highest)")) ///
           xlabel(#8, angle(45)) ///
           xline(201, lcolor("64 64 64") lpattern(dash) lwidth(medium)) ///
           scheme(s1color)
           
    graph export "gap_pct_q_w_quintile_trends_2010q2.png", replace width(1200) height(800)
restore

/*---------------------------------------------------------------------------*/
/* 2.2 2014 Q1 Base Period Classification                                    */
/*---------------------------------------------------------------------------*/

preserve    
    * Ensure data is sorted
    sort gvkey yq
      
    * Keep only firms present in the base period (2014Q1)
    keep if year == 2014 & quarter == 1
    
    * Create quintiles based on base period values
    xtile quintile_2014q1 = gap_pct_q_w, nq(5)
    
    * Keep only necessary variables for merging
    keep gvkey quintile_2014q1
    
    * Save temporary file with quintile assignments
    tempfile quintiles
    save `quintiles'
restore

* Merge quintile assignments back to main dataset
merge m:1 gvkey using `quintiles'

* Keep matched observations and unmatched from master
keep if _merge == 3 | _merge == 1
drop _merge

* Apply quintile labels
label values quintile_2014q1 quintile_lbl

* Analyze trends by quintile
preserve
    * Collapse to quintile-quarter level
    collapse (mean) avg_gap_pct_q_w=gap_pct_q_w, by(quintile_2014q1 yq)
    
    * Keep only relevant time period
    keep if yq>=200 & yq < 244
    
    * Create formatted date variable for improved labeling
    gen date_str = string(yq, "%tq")
 
    
* Analyze trends by quintile
preserve
    * Collapse to quintile-quarter level
    collapse (mean) avg_gap_pct_q_w=gap_pct_q_w, by(quintile_2014q1 yq)
    
    * Keep only relevant time period
    keep if yq>=200 & yq < 244
    
    * Create formatted date variable for improved labeling
    gen date_str = string(yq, "%tq")
    
    * Create trend chart by 2014Q1 quintile
    twoway (line avg_gap_pct_q_w yq if quintile_2014q1==1, lcolor(blue)) ///
           (line avg_gap_pct_q_w yq if quintile_2014q1==2, lcolor(green)) ///
           (line avg_gap_pct_q_w yq if quintile_2014q1==3, lcolor(orange)) ///
           (line avg_gap_pct_q_w yq if quintile_2014q1==4, lcolor(red)) ///
           (line avg_gap_pct_q_w yq if quintile_2014q1==5, lcolor(purple)), ///
           title("Trends in Gap Posting by Initial 2014Q1 Quintile") ///
           subtitle("Firms categorized based on 2014Q1 gap_pct_q_w values") ///
           xtitle("Year-Quarter") ytitle("Average gap_pct_q_w") ///
           legend(order(1 "Q1 (Lowest)" 2 "Q2" 3 "Q3" 4 "Q4" 5 "Q5 (Highest)")) ///
           xlabel(#8, angle(45)) ///
           xline(216, lcolor("64 64 64") lpattern(dash) lwidth(medium)) ///
           scheme(s1color)
           
    * Export chart as high-resolution image
    graph export "gap_pct_q_w_quintile_trends_quintile_2014q1.png", replace width(1200) height(800)
restore

/*---------------------------------------------------------------------------*/
/* 2.3 2018 Q1 Base Period Classification                                    */
/*---------------------------------------------------------------------------*/

preserve    
    * Ensure data is sorted
    sort gvkey yq
      
    * Keep only firms present in the base period (2018Q1)
    keep if year == 2018 & quarter == 1
    
    * Create quintiles based on base period values
    xtile quintile_2018q1 = gap_pct_q_w, nq(5)
    
    * Keep only necessary variables for merging
    keep gvkey quintile_2018q1
    
    * Save temporary file with quintile assignments
    tempfile quintiles
    save `quintiles'
restore

* Merge quintile assignments back to main dataset
merge m:1 gvkey using `quintiles'

* Keep matched observations and unmatched from master
keep if _merge == 3 | _merge == 1
drop _merge

* Apply quintile labels
label values quintile_2018q1 quint_lbl

* Analyze trends by quintile
preserve
    * Collapse to quintile-quarter level
    collapse (mean) avg_gap_pct_q_w=gap_pct_q_w, by(quintile_2018q1 yq)
    
    * Keep only relevant time period
    keep if yq>=200 & yq < 244
    
    * Create formatted date variable for improved labeling
    gen date_str = string(yq, "%tq")
    
    * Create trend chart by 2018Q1 quintile
    twoway (line avg_gap_pct_q_w yq if quintile_2018q1==1, lcolor(blue)) ///
           (line avg_gap_pct_q_w yq if quintile_2018q1==2, lcolor(green)) ///
           (line avg_gap_pct_q_w yq if quintile_2018q1==3, lcolor(orange)) ///
           (line avg_gap_pct_q_w yq if quintile_2018q1==4, lcolor(red)) ///
           (line avg_gap_pct_q_w yq if quintile_2018q1==5, lcolor(purple)), ///
			title("Trends in Gap Posting by Initial 2018Q1 Quintile") ///
           subtitle("Firms categorized based on 2018Q1 gap_pct_q_w values") ///
           xtitle("Year-Quarter") ytitle("Average gap_pct_q_w") ///
           legend(order(1 "Q1 (Lowest)" 2 "Q2" 3 "Q3" 4 "Q4" 5 "Q5 (Highest)")) ///
           xlabel(#8, angle(45)) ///
           xline(232, lcolor("64 64 64") lpattern(dash) lwidth(medium)) ///
           scheme(s1color)
           
    * Export chart as high-resolution image
    graph export "$gap_pct_q_w_quintile_trends_quintile_2018q1.png", replace width(1200) height(800)
restore

/*===========================================================================*/
/* SECTION 3: PERSISTENCE METRICS                                            */
/*===========================================================================*/

/*---------------------------------------------------------------------------*/
/* 3.1 AR(1) Time Trend Models                                               */
/*---------------------------------------------------------------------------*/

preserve
    * Collapse to firm-quarter level for time series analysis
    collapse (mean) gap_pct_w, by(gvkey yq)
    
    * Set panel structure with quarterly time intervals
    xtset gvkey yq
    
    * Generate lagged variables and time trend
    gen gap_lag1 = L1.gap_pct_w
    gen gap_lag4 = L4.gap_pct_w
    
    * Create a time trend variable for each firm
    bys gvkey: egen min_yq = min(yq)
    gen time_trend = yq-min_yq
    
    * Get list of unique firm identifiers
    levelsof gvkey, local(gvkeys)
    
    * Create file to store results and observations
    tempfile ar1_trend_results
    tempname memhold
    postfile `memhold' gvkey ar1_coef_trend trend_coef ar1_pval_trend trend_pval r2_trend n_obs using `ar1_trend_results', replace
    
    foreach g of local gvkeys {
        * Check for sufficient observations
        quietly count if !missing(gap_pct_w) & !missing(gap_lag1) & !missing(time_trend) & gvkey==`g'
        local n_obs = r(N)
        
        * Initialize model coefficients
        local ar1_coef = .
        local trend_coef = .
        local ar1_pval = .
        local trend_pval = .
        local r2 = .
        
        * Run regression if sufficient observations
        if `n_obs' >= 4 {
            capture quietly reg gap_pct_w gap_lag1 time_trend if gvkey==`g'
            if _rc == 0 {
                local ar1_coef = _b[gap_lag1]
                local trend_coef = _b[time_trend]
                local ar1_pval = 2*ttail(e(df_r),abs(_b[gap_lag1]/_se[gap_lag1]))
                local trend_pval = 2*ttail(e(df_r),abs(_b[time_trend]/_se[time_trend]))
                local r2 = e(r2)
            }
        }
        
        post `memhold' (`g') (`ar1_coef') (`trend_coef') (`ar1_pval') (`trend_pval') (`r2') (`n_obs')
    }
    postclose `memhold'
    
    * Load and save results
    use `ar1_trend_results', clear
    save "ar1_coefficients_with_obs.dta", replace
    
restore

* Merge trend model results to main dataset
merge m:1 gvkey using "ar1_coefficients_with_obs.dta", nogen

replace ar1_coef_trend = . if n_obs<20
sum ar1_coef_trend,d

/*---------------------------------------------------------------------------*/
/* 3.2 Correlation                                                           */
/*---------------------------------------------------------------------------*/

* Autocorrelation analysis - properly handling multiple MSAs per gvkey-quarter
* First collapse to gvkey-quarter level to handle the time series analysis properly
preserve
collapse (mean) gap_pct_w, by(gvkey yq)

* Generate lags at the gvkey-quarter level
xtset gvkey yq
by gvkey: gen gap_lag1 = gap_pct_w[_n-1] if gvkey==gvkey[_n-1]
by gvkey: gen gap_lag4 = gap_pct_w[_n-4] if gvkey==gvkey[_n-4]

* Calculate autocorrelations by gvkey
* Create a temporary file to store correlation results
tempfile corr_results
tempname memhold
postfile `memhold' gvkey corr_lag1 corr_lag4 n_obs_corr using `corr_results', replace

levelsof gvkey, local(gvkeys)
foreach g of local gvkeys {
    * Check if there are sufficient observations for correlation
    quietly count if !missing(gap_pct_w) & !missing(gap_lag1) & gvkey==`g'
    local n_obs1 = r(N)
    
    quietly count if !missing(gap_pct_w) & !missing(gap_lag4) & gvkey==`g'
    local n_obs4 = r(N)
    
    * Only calculate correlation if sufficient observations (at least 3)
    local cor1 = .
    local cor4 = .
    local n_obs_corr = max(`n_obs1', `n_obs4')
    
    if `n_obs1' >= 3 {
        capture quietly correlate gap_pct_w gap_lag1 if gvkey==`g'
        if _rc == 0 {
            local cor1 = r(rho)
        }
    }
    
    if `n_obs4' >= 3 {
        capture quietly correlate gap_pct_w gap_lag4 if gvkey==`g'
        if _rc == 0 {
            local cor4 = r(rho)
        }
    }
    
    post `memhold' (`g') (`cor1') (`cor4') (`n_obs_corr')
}
postclose `memhold'

* Use the correlation results
use `corr_results', clear
save corr_results.dta, replace

* Restore the original dataset with all MSAs
restore

* Merge correlation results back to the main dataset at gvkey level
merge m:1 gvkey using corr_results.dta, keep(1 3) nogen

replace corr_lag1 = . if n_obs_corr<20
sum corr_lag1,d


/*---------------------------------------------------------------------------*/
/* 3.3 Spike %                                                           */
/*---------------------------------------------------------------------------*/
// Spike is defined as the change larger than 2 std compared to the mean gap of a firm

preserve
* Step 1: Collapse to firm-quarter level
collapse (mean) gap_pct_w , by(gvkey yq)

* Step 2: Calculate firm-level mean and standard deviation
bysort gvkey: egen firm_mean_gap = mean(gap_pct_w)
bysort gvkey: egen firm_sd_gap = sd(gap_pct_w)

* Step 3: Calculate standardized measure (std_gap)
gen std_gap = (gap_pct_w - firm_mean_gap) / firm_sd_gap if firm_sd_gap > 0 & !missing(firm_sd_gap)

* Step 4: Detect spikes (std_gap > 2)
gen spike = (std_gap > 2) if !missing(std_gap)


* Step 5: Count spikes and calculate percentage of quarters with spikes
bysort gvkey: egen num_spikes = sum(spike)
bysort gvkey: egen total_quarters = count(yq)

replace spike = 0 if missing(spike) | total_quarters < 20

gen spike_pct = (num_spikes / total_quarters) * 100 if total_quarters > 0

* Step 6: Keep only necessary variables for merging back
keep gvkey num_spikes total_quarters spike_pct

* Step 7: Remove duplicates at the firm level (since we need one record per firm)
duplicates drop gvkey, force

* Step 8: Save results
save "spike.dta", replace
restore

* Step 9: Merge the firm-level spike statistics back to the original dataset
merge m:1 gvkey using "spike.dta", nogen


sum spike_pct,d

/*===========================================================================*/
/* SECTION 4: CATEGORICAL CLASSIFICATION                                     */
/*===========================================================================*/

/*---------------------------------------------------------------------------*/
/* 4.1 Quartile-Based Classification -- Using ar1_coef_trend                 */
/*---------------------------------------------------------------------------*/

* Create absolute value of AR coefficient for persistence measure
gen ar1 = abs(ar1_coef_trend)

preserve
    * Collapse to firm level for classification
    collapse (mean) firm_mean_gap=gap_pct_q_w (mean) ar1 (mean)n_obs, by(gvkey)

    * Calculate quartiles for classification boundaries
    sum firm_mean_gap, detail
    local q3_mean_gap = r(p75)  // Top quartile (75th percentile)
    local q1_mean_gap = r(p25)  // Bottom quartile (25th percentile)

    * Calculate quartiles for persistence measure
    sum ar1, detail
    local q3_abs_corr = r(p75)  // Top quartile for absolute persistence
    local q1_abs_corr = r(p25)  // Bottom quartile for absolute persistence

    * Document threshold values in log
    di "75th percentile (High) of firm_mean_gap: `q3_mean_gap'"
    di "25th percentile (Low) of firm_mean_gap: `q1_mean_gap'"
    di "75th percentile (Persistent) of abs_corr_lag1: `q3_abs_corr'"
    di "25th percentile (Non-persistent) of abs_corr_lag1: `q1_abs_corr'"


    /* Create binary classification variables
       - High/low gap: based on quartiles of average gap posting rate
       - Persistent/non-persistent: based on quartiles of AR(1) coefficient */
    gen high_gap = (firm_mean_gap >= `q3_mean_gap') & (firm_mean_gap != .)
    gen low_gap = (firm_mean_gap <= `q1_mean_gap') & (firm_mean_gap != .)
    gen persistent = (ar1 >= `q3_abs_corr') & (ar1 != .)
    gen non_persistent = (ar1 <= `q1_abs_corr') & (ar1 != .)

    /* Create categorical variable combining both dimensions
       - Creates 5 distinct firm types based on gap level and persistence */
    gen gap_category = .
    replace gap_category = 1 if high_gap == 1 & persistent == 1      // High & Persistent
    replace gap_category = 2 if high_gap == 1 & non_persistent == 1  // High & Non-Persistent
    replace gap_category = 3 if low_gap == 1 & persistent == 1       // Low & Persistent
    replace gap_category = 4 if low_gap == 1 & non_persistent == 1   // Low & Non-Persistent
    replace gap_category = 5 if (high_gap == 0 & low_gap == 0) | (persistent == 0 & non_persistent == 0)  // Middle Group

    * Save firm-level classifications
    keep gvkey high_gap low_gap persistent non_persistent gap_category
    tempfile firm_classifications
    save `firm_classifications'
restore

* Merge firm classifications to main dataset
merge m:1 gvkey using `firm_classifications', keep(3) nogen


label define gap_cat 1 "High & Persistent" 2 "High & Non-Persistent" 3 "Low & Persistent" 4 "Low & Non-Persistent" 5 "Middle Group"
label values gap_category gap_cat

* Examine distribution of firms across categories
tab gap_category, missing

/*---------------------------------------------------------------------------*/
/* 4.2 Quartile-Based Classification -- Using Correlation                    */
/*---------------------------------------------------------------------------*/

* Create absolute value of AR coefficient for persistence measure
drop ar1
gen ar1 = abs(corr_lag1)

preserve
    * Collapse to firm level for classification
    collapse (mean) firm_mean_gap=gap_pct_q_w (mean) ar1 (mean)n_obs_corr, by(gvkey)

    * Calculate quartiles for classification boundaries
    sum firm_mean_gap, detail
    local q3_mean_gap = r(p75)  // Top quartile (75th percentile)
    local q1_mean_gap = r(p25)  // Bottom quartile (25th percentile)

    * Calculate quartiles for persistence measure
    sum ar1, detail
    local q3_abs_corr = r(p75)  // Top quartile for absolute persistence
    local q1_abs_corr = r(p25)  // Bottom quartile for absolute persistence

    * Document threshold values in log
    di "75th percentile (High) of firm_mean_gap: `q3_mean_gap'"
    di "25th percentile (Low) of firm_mean_gap: `q1_mean_gap'"
    di "75th percentile (Persistent) of abs_corr_lag1: `q3_abs_corr'"
    di "25th percentile (Non-persistent) of abs_corr_lag1: `q1_abs_corr'"

    /* Create binary classification variables
       - High/low gap: based on quartiles of average gap posting rate
       - Persistent/non-persistent: based on quartiles of AR(1) coefficient */
    gen high_gap = (firm_mean_gap >= `q3_mean_gap') & (firm_mean_gap != .)
    gen low_gap = (firm_mean_gap <= `q1_mean_gap') & (firm_mean_gap != .)
    gen persistent = (ar1 >= `q3_abs_corr') & (ar1 != .)
    gen non_persistent = (ar1 <= `q1_abs_corr') & (ar1 != .)

    /* Create categorical variable combining both dimensions
       - Creates 5 distinct firm types based on gap level and persistence */
    gen gap_category_corr = .
    replace gap_category_corr = 1 if high_gap == 1 & persistent == 1      // High & Persistent
    replace gap_category_corr = 2 if high_gap == 1 & non_persistent == 1  // High & Non-Persistent
    replace gap_category_corr = 3 if low_gap == 1 & persistent == 1       // Low & Persistent
    replace gap_category_corr = 4 if low_gap == 1 & non_persistent == 1   // Low & Non-Persistent
    replace gap_category_corr = 5 if (high_gap == 0 & low_gap == 0) | (persistent == 0 & non_persistent == 0)  // Middle Group

    * Save firm-level classifications
    keep gvkey high_gap low_gap persistent non_persistent gap_category_corr
    tempfile firm_classifications_corr
    save `firm_classifications_corr'
restore


* Merge firm classifications to main dataset
merge m:1 gvkey using `firm_classifications_corr', keep(3) nogen

label values gap_category_corr gap_cat

* Examine distribution of firms across categories
tab gap_category_corr, missing

/*---------------------------------------------------------------------------*/
/* 4.3 Percentage of Spikes                                                  */
/*---------------------------------------------------------------------------*/

preserve
    * Collapse to firm level for classification
    collapse (mean) firm_mean_gap=gap_pct_q_w (mean) spike_pct, by(gvkey)

    * Calculate quartiles for classification boundaries
    sum firm_mean_gap, detail
    local q3_mean_gap = r(p75)  // Top quartile (75th percentile)
    local q1_mean_gap = r(p25)  // Bottom quartile (25th percentile)

    * Calculate quartiles for persistence measure
    sum spike_pct, detail
    local q3_spike_pct = r(p75)  // Top quartile for absolute persistence
    local q1_spike_pct = r(p25)  // Bottom quartile for absolute persistence

    * Document threshold values in log
    di "75th percentile (High) of firm_mean_gap: `q3_mean_gap'"
    di "25th percentile (Low) of firm_mean_gap: `q1_mean_gap'"
    di "75th percentile (Persistent) of spike_pct: `q3_abs_corr'"
    di "25th percentile (Non-persistent) of spike_pct: `q1_abs_corr'"

    /* Create binary classification variables
       - High/low gap: based on quartiles of average gap posting rate
       - Persistent/non-persistent: based on quartiles of spike_pct */
    gen high_gap = (firm_mean_gap >= `q3_mean_gap') & (firm_mean_gap != .)
    gen low_gap = (firm_mean_gap <= `q1_mean_gap') & (firm_mean_gap != .)
    gen persistent = (spike_pct >= `q3_spike_pct') & (spike_pct != .)
    gen non_persistent = (spike_pct <= `q1_spike_pct') & (spike_pct != .)

    /* Create categorical variable combining both dimensions
       - Creates 5 distinct firm types based on gap level and persistence */
    gen gap_category_spike = .
    replace gap_category_spike = 1 if high_gap == 1 & persistent == 1      // High & Persistent
    replace gap_category_spike = 2 if high_gap == 1 & non_persistent == 1  // High & Non-Persistent
    replace gap_category_spike = 3 if low_gap == 1 & persistent == 1       // Low & Persistent
    replace gap_category_spike = 4 if low_gap == 1 & non_persistent == 1   // Low & Non-Persistent
    replace gap_category_spike = 5 if (high_gap == 0 & low_gap == 0) | (persistent == 0 & non_persistent == 0)  // Middle Group

    * Save firm-level classifications
    keep gvkey high_gap low_gap persistent non_persistent gap_category_spike
    tempfile firm_classifications_spike
    save `firm_classifications_spike'
restore


* Merge firm classifications to main dataset
merge m:1 gvkey using `firm_classifications_spike', keep(3) nogen

label values gap_category_spike gap_cat

* Examine distribution of firms across categories
tab gap_category_spike, missing


/*===========================================================================*/
/* SECTION 5: ANALYSIS BY FIRM CATEGORY                                      */
/*===========================================================================*/

/*---------------------------------------------------------------------------*/
/* 5.1 Firm Characteristics by Category                                      */
/*---------------------------------------------------------------------------*/

/* Analyze differences in firm characteristics across categories
   - Compares size, profitability, leverage, institutional ownership, etc.
   - Helps identify unique features of each gap posting pattern type */
tabstat atq_w revtq_w mkvaltq_w btm_w lev_w roa_w inst_pct_w analyst_follow, ///
    by(gap_category) statistics(mean) format(%9.3f) 

* Create standardized metrics for comparison across variables
foreach var of varlist atq_w revtq_w mkvaltq_w btm_w lev_w roa_w inst_pct_w analyst_follow {
    egen `var'_z = std(`var')
}

* Create horizontal bar chart comparing standardized metrics
graph hbar (mean) mkvaltq_w_z roa_w_z inst_pct_w_z analyst_follow_z, ///
    over(gap_category, sort(1) descending label(labsize(small))) ///
    title("Key Financial Metrics by gap Posting Type", size(medium)) ///
    subtitle("Standardized Values (Z-scores), Quartile-Based Classification", size(small)) ///
    legend(label(1 "Market Value") label(2 "ROA") label(3 "Institutional %") ///
           label(4 "Analyst Coverage") size(small)) ///
    blabel(bar, format(%5.2f) size(vsmall)) ///
    ysize(6) xsize(8) ///
    scheme(s1color)
	
* Create horizontal bar chart comparing standardized metrics
graph hbar (mean) mkvaltq_w_z roa_w_z inst_pct_w_z analyst_follow_z, ///
    over(gap_category_corr, sort(1) descending label(labsize(small))) ///
    title("Key Financial Metrics by gap Posting Type", size(medium)) ///
    subtitle("Standardized Values (Z-scores), Quartile-Based Classification", size(small)) ///
    legend(label(1 "Market Value") label(2 "ROA") label(3 "Institutional %") ///
           label(4 "Analyst Coverage") size(small)) ///
    blabel(bar, format(%5.2f) size(vsmall)) ///
    ysize(6) xsize(8) ///
    scheme(s1color)
	
* Create horizontal bar chart comparing standardized metrics
graph hbar (mean) mkvaltq_w_z roa_w_z inst_pct_w_z analyst_follow_z, ///
    over(gap_category_spike, sort(1) descending label(labsize(small))) ///
    title("Key Financial Metrics by gap Posting Type", size(medium)) ///
    subtitle("Standardized Values (Z-scores), Quartile-Based Classification", size(small)) ///
    legend(label(1 "Market Value") label(2 "ROA") label(3 "Institutional %") ///
           label(4 "Analyst Coverage") size(small)) ///
    blabel(bar, format(%5.2f) size(vsmall)) ///
    ysize(6) xsize(8) ///
    scheme(s1color)	
	

/*---------------------------------------------------------------------------*/
/* 5.2 Time Series Trends by Category                                        */
/*---------------------------------------------------------------------------*/

**# Using AR1

preserve
    * Collapse data to category-quarter level
    collapse (mean) gap_pct_q_w, by(gap_category yq)
    
    * Keep only relevant time period
    keep if yq >= 200 & yq < 244
    
    * Ensure yq is formatted properly for time axis
    format yq %tq

    * Create combined trend chart for all categories
    twoway ///
      (line gap_pct_q_w yq if gap_category==1, lcolor(purple)) ///
      (line gap_pct_q_w yq if gap_category==2, lcolor(red)) ///
      (line gap_pct_q_w yq if gap_category==3, lcolor(green)) ///
      (line gap_pct_q_w yq if gap_category==4, lcolor(navy)) ///
      (line gap_pct_q_w yq if gap_category==5, lcolor(gray)), ///
      title("Gap Posting Trends by Category", size(medium)) ///
      subtitle("Average gap_pct_q_w over Time") ///
      xtitle("Year-Quarter") ytitle("gap Posting Rate (gap_pct_q_w)") ///
      xlabel(, angle(45) labsize(small)) ///
      ylabel(, grid) ///
      legend(order(1 "High & Persistent" 2 "High & Non-Persistent" ///
                   3 "Low & Persistent" 4 "Low & Non-Persistent" 5 "Middle Group") ///
             cols(2) position(6) size(small)) ///
      scheme(s1color) name(category_trends, replace)
      
    * Export high-resolution image
    graph export "gap_posting_category_trends.png", replace width(1200)
restore

**# Using Correlation

preserve
    * Collapse data to category-quarter level
    collapse (mean) gap_pct_q_w, by(gap_category_corr yq)
    
    * Keep only relevant time period
    keep if yq >= 200 & yq < 244
    
    * Ensure yq is formatted properly for time axis
    format yq %tq

    * Create combined trend chart for all categories
    twoway ///
      (line gap_pct_q_w yq if gap_category_corr==1, lcolor(purple)) ///
      (line gap_pct_q_w yq if gap_category_corr==2, lcolor(red)) ///
      (line gap_pct_q_w yq if gap_category_corr==3, lcolor(green)) ///
      (line gap_pct_q_w yq if gap_category_corr==4, lcolor(navy)) ///
      (line gap_pct_q_w yq if gap_category_corr==5, lcolor(gray)), ///
      title("Gap Posting Trends by Category", size(medium)) ///
      subtitle("Average gap_pct_q_w over Time") ///
      xtitle("Year-Quarter") ytitle("gap Posting Rate (gap_pct_q_w)") ///
      xlabel(, angle(45) labsize(small)) ///
      ylabel(, grid) ///
      legend(order(1 "High & Persistent" 2 "High & Non-Persistent" ///
                   3 "Low & Persistent" 4 "Low & Non-Persistent" 5 "Middle Group") ///
             cols(2) position(6) size(small)) ///
      scheme(s1color) name(category_trends, replace)
      
    * Export high-resolution image
    graph export "gap_posting_category_corr_trends.png", replace width(1200)
restore



**# Using Spike_pct

preserve
    * Collapse data to category-quarter level
    collapse (mean) gap_pct_q_w, by(gap_category_spike yq)
    
    * Keep only relevant time period
    keep if yq >= 200 & yq < 244
    
    * Ensure yq is formatted properly for time axis
    format yq %tq

    * Create combined trend chart for all categories
    twoway ///
      (line gap_pct_q_w yq if gap_category_spike==1, lcolor(purple)) ///
      (line gap_pct_q_w yq if gap_category_spike==2, lcolor(red)) ///
      (line gap_pct_q_w yq if gap_category_spike==3, lcolor(green)) ///
      (line gap_pct_q_w yq if gap_category_spike==4, lcolor(navy)) ///
      (line gap_pct_q_w yq if gap_category_spike==5, lcolor(gray)), ///
      title("Gap Posting Trends by Category", size(medium)) ///
      subtitle("Average gap_pct_q_w over Time") ///
      xtitle("Year-Quarter") ytitle("gap Posting Rate (gap_pct_q_w)") ///
      xlabel(, angle(45) labsize(small)) ///
      ylabel(, grid) ///
      legend(order(1 "High & Persistent" 2 "High & Non-Persistent" ///
                   3 "Low & Persistent" 4 "Low & Non-Persistent" 5 "Middle Group") ///
             cols(2) position(6) size(small)) ///
      scheme(s1color) name(category_trends, replace)
      
    * Export high-resolution image
    graph export "gap_posting_category_spike_trends.png", replace width(1200)
restore
/*---------------------------------------------------------------------------*/
/* 5.3 Industry Distribution by Category                                     */
/*---------------------------------------------------------------------------*/

preserve
    * Exclude middle category for clearer analysis
    drop if gap_category == 5
    
    * Collapse to get counts by sector and category
    collapse (count) n=gap_pct_q_w, by(gsector gap_category)
    
    * Calculate percentage within each sector
    bysort gsector: egen total = sum(n)
    gen percentage = 100 * n / total

    * Define descriptive labels for GICS sectors
    label define gsector_lbl ///
        10 "Energy" ///
        15 "Materials" ///
        20 "Industrials" ///
        25 "Consumer Discretionary" ///
        30 "Consumer Staples" ///
        35 "Health Care" ///
        40 "Financials" ///
        45 "Information Technology" ///
        50 "Communication Services" ///
        55 "Utilities" ///
        60 "Real Estate"
    label values gsector gsector_lbl

    * Create stacked bar chart showing category distribution by sector
    graph bar percentage, over(gap_category, label(labsize(small))) ///
          over(gsector, label(angle(45) labsize(small))) ///
          title("Distribution of gap Posting Categories by Sector", size(medium)) ///
          ytitle("Percentage within Sector") ///
          ylabel(0(20)100, grid) ///
          asyvars stack ///
          legend(order(1 "High & Persistent" 2 "High & Non-Persistent" ///
                       3 "Low & Persistent" 4 "Low & Non-Persistent") ///
                 cols(2) position(6) ring(1) size(small)) ///
          bar(1, color(purple)) ///
          bar(2, color(red)) ///
          bar(3, color(green)) ///
          bar(4, color(blue)) ///
          scheme(s1color) name(sector_distribution, replace)

    * Export high-resolution image
    graph export "gap_category_by_sector.png", replace width(1200)
restore

preserve
    * Exclude middle category for clearer analysis
    drop if gap_category_corr == 5
    
    * Collapse to get counts by sector and category
    collapse (count) n=gap_pct_q_w, by(gsector gap_category_corr)
    
    * Calculate percentage within each sector
    bysort gsector: egen total = sum(n)
    gen percentage = 100 * n / total

    * Define descriptive labels for GICS sectors
    label define gsector_lbl ///
        10 "Energy" ///
        15 "Materials" ///
        20 "Industrials" ///
        25 "Consumer Discretionary" ///
        30 "Consumer Staples" ///
        35 "Health Care" ///
        40 "Financials" ///
        45 "Information Technology" ///
        50 "Communication Services" ///
        55 "Utilities" ///
        60 "Real Estate"
    label values gsector gsector_lbl

    * Create stacked bar chart showing category distribution by sector
    graph bar percentage, over(gap_category_corr, label(labsize(small))) ///
          over(gsector, label(angle(45) labsize(small))) ///
          title("Distribution of Gap Posting Categories by Sector", size(medium)) ///
          ytitle("Percentage within Sector") ///
          ylabel(0(20)100, grid) ///
          asyvars stack ///
          legend(order(1 "High & Persistent" 2 "High & Non-Persistent" ///
                       3 "Low & Persistent" 4 "Low & Non-Persistent") ///
                 cols(2) position(6) ring(1) size(small)) ///
          bar(1, color(purple)) ///
          bar(2, color(red)) ///
          bar(3, color(green)) ///
          bar(4, color(blue)) ///
          scheme(s1color) name(sector_distribution, replace)

    * Export high-resolution image
    graph export "gap_category_corr_by_sector.png", replace width(1200)
restore

preserve
    * Exclude middle category for clearer analysis
    drop if gap_category_spike == 5
    
    * Collapse to get counts by sector and category
    collapse (count) n=gap_pct_q_w, by(gsector gap_category_spike)
    
    * Calculate percentage within each sector
    bysort gsector: egen total = sum(n)
    gen percentage = 100 * n / total

    * Define descriptive labels for GICS sectors
    label define gsector_lbl ///
        10 "Energy" ///
        15 "Materials" ///
        20 "Industrials" ///
        25 "Consumer Discretionary" ///
        30 "Consumer Staples" ///
        35 "Health Care" ///
        40 "Financials" ///
        45 "Information Technology" ///
        50 "Communication Services" ///
        55 "Utilities" ///
        60 "Real Estate"
    label values gsector gsector_lbl

    * Create stacked bar chart showing category distribution by sector
    graph bar percentage, over(gap_category_spike, label(labsize(small))) ///
          over(gsector, label(angle(45) labsize(small))) ///
          title("Distribution of Gap Posting Categories by Sector", size(medium)) ///
          ytitle("Percentage within Sector") ///
          ylabel(0(20)100, grid) ///
          asyvars stack ///
          legend(order(1 "High & Persistent" 2 "High & Non-Persistent" ///
                       3 "Low & Persistent" 4 "Low & Non-Persistent") ///
                 cols(2) position(6) ring(1) size(small)) ///
          bar(1, color(purple)) ///
          bar(2, color(red)) ///
          bar(3, color(green)) ///
          bar(4, color(blue)) ///
          scheme(s1color) name(sector_distribution, replace)

    * Export high-resolution image
    graph export "gap_category_corr_by_sector.png", replace width(1200)
restore


/*===========================================================================*/
/* SECTION 6: ANALYSIS BY FIRM CATEGORY                                      */
/*===========================================================================*/

bys gvkey: egen firm_mean_gap = mean(gap_pct_q_w)
bys gvkey: egen firm_sd_gap = sd(gap_pct_q_w)

gen ar1_coef_abs =  abs(ar1_coef_trend)
gen corr1_abs =  abs(corr_lag1)

preserve
duplicates drop gvkey, force
pwcorr firm_mean_gap firm_sd_gap ar1_coef_abs corr1_abs, sig
restore

pwcorr gap_pct_q_w firm_sd_gap ar1_coef_abs corr1_abs atq_w mkvaltq_w btm_w lev_w roa_w inst_pct_w analyst_follow msa_count,sig
