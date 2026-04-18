////////////////////////////////////////////////////////////////////////////////
// 02_aggregate_analysis.do
// Section 2: Aggregate Employment Rate and Federal Funds Rate Analysis
//
// Inputs  : $proc/cps_fedfunds_mth.dta
// Outputs : $tables/table2_1_employment_lagged_fedfunds_correlations.xlsx
//           $figures/figure2_2_1_employment_fedfunds_hike_periods.png
//           $figures/figure2_2_2_employment_fedfunds_hike_periods.png
//
// Run via : 00_master.do  (globals must already be set)
////////////////////////////////////////////////////////////////////////////////

di ""
di "--- 02_aggregate_analysis.do: starting ---"

// --------------------------------------------------------------------------
// 2.1 Correlation Analysis
//     Compute correlations between the aggregate employment rate and
//     Federal Funds Rate lagged 1–30 months.
// --------------------------------------------------------------------------

use "$proc/cps_fedfunds_mth.dta", clear

// Build monthly employment rate
gen time = ym(year, month)
format time %tm

bysort time: egen emp_tot = total(wtfinl * (employment == 2))
bysort time: egen lf_tot  = total(wtfinl * (lf_status == 1))
gen erate = (emp_tot / lf_tot) * 100

// Collapse to one observation per month
collapse (mean) erate fedfunds, by(time)
sort time

// Generate 30 lagged Federal Funds Rate variables
forvalues i = 1/30 {
    gen fedfunds_lag`i' = fedfunds[_n-`i']
    label variable fedfunds_lag`i' "Federal Funds Rate (t-`i' months)"
}

// Compute and store lag-correlation pairs
tempfile results
tempname memhold
postfile `memhold' lag correlation using `results'

forvalues i = 1/30 {
    qui corr erate fedfunds_lag`i' if !missing(erate) & !missing(fedfunds_lag`i')
    post `memhold' (`i') (r(rho))
}
postclose `memhold'

use `results', clear
export excel using "$tables/table2_1_employment_lagged_fedfunds_correlations.xlsx", ///
    replace firstrow(variables)

di "  Table 2.1 saved."

// --------------------------------------------------------------------------
// 2.2 Historical Trends: Employment Rate vs. Federal Funds Rate
//     Two figures:
//       Fig 2.2.1 — time-series dual-axis chart with shaded hike periods
//       Fig 2.2.2 — bar chart of employment before/after each hike cycle
// --------------------------------------------------------------------------

use "$proc/cps_fedfunds_mth.dta", clear

gen time = ym(year, month)
format time %tm

bysort time: egen emp_tot = total(wtfinl * (employment == 2))
bysort time: egen lf_tot  = total(wtfinl * (lf_status == 1))
gen erate = (emp_tot / lf_tot) * 100

collapse (mean) erate fedfunds, by(time)
sort time

// 1-month lead of Federal Funds Rate (shows leading relationship with employment)
gen fedfunds_lead1 = fedfunds[_n+1] if _n <= _N - 1
label variable fedfunds_lead1 "Federal Funds Rate (t+1 month)"

// --- Identify five major tightening cycles ---
gen hike = 0
replace hike = 1 if time >= ym(1994, 2)  & time <= ym(1995, 2)   // Pre-Tech Boom
replace hike = 1 if time >= ym(1999, 6)  & time <= ym(2000, 5)   // Dot-Com Bubble
replace hike = 1 if time >= ym(2004, 6)  & time <= ym(2006, 6)   // Housing Boom
replace hike = 1 if time >= ym(2015, 12) & time <= ym(2018, 12)  // Zero-Rate Exit
replace hike = 1 if time >= ym(2022, 3)  & time <= ym(2023, 7)   // Post-COVID
label variable hike "Rate Hike Period"

// Shading bounds
local min_y = 84
local max_y = 100
gen min_value = `min_y'
gen max_value = `max_y'

// --- Figure 2.2.1: Dual-axis time series ---
twoway ///
    (rbar min_value max_value time if hike == 1, color(gs14)) ///
    (line erate time,                            yaxis(1) lcolor(blue) lwidth(medthick)) ///
    (line fedfunds_lead1 time if !missing(fedfunds_lead1), ///
                                                 yaxis(2) lcolor(red)  lwidth(medthick)), ///
    title("Employment Rate and One-Month-Ahead Federal Funds Rate" ///
          "During Monetary Tightening Cycles", size(medium) margin(b=2)) ///
    ytitle("Employment Rate (%)",          size(small) margin(r=2)) ///
    ytitle("Federal Funds Rate (%)", axis(2) size(small) margin(l=2)) ///
    xtitle("Year",                         size(small) margin(t=2)) ///
    ylabel(84(4)100, angle(0) grid labsize(small)) ///
    ylabel(0(2)10,   axis(2) angle(0) labsize(small)) ///
    xlabel(360(60)780, angle(45) labsize(small)) xscale(r(360 780)) ///
    legend(order(2 "Employment Rate" ///
                 3 "Future Federal Funds Rate (1-month Lead)" ///
                 1 "Rate Hike Period") ///
           rows(1) position(6) size(small) symxsize(6) symysize(2) keygap(2)) ///
    note("Note: Shaded areas represent major Federal Reserve tightening cycles." ///
         "The Federal Funds Rate is shifted forward by one month to show its" ///
         "lead relationship with the employment rate.", ///
         size(vsmall) margin(t=2)) ///
    graphregion(margin(l=2 r=2 t=2 b=2)) ///
    ysize(5) xsize(8)

graph export "$figures/figure2_2_1_employment_fedfunds_hike_periods.png", ///
    replace width(2400) height(1500)
di "  Figure 2.2.1 saved."

// --- Figure 2.2.2: Employment before vs. after each hike cycle ---
gen hike_start = (hike == 0 & hike[_n+1] == 1)
gen hike_end   = (hike == 1 & hike[_n+1] == 0)

gen hike_cycle = sum(hike_start)
replace hike_cycle = . if hike_start == 0 & hike_end == 0 & hike == 0

gen period = .
replace period = 0 if hike_start == 1   // Month before hike begins
replace period = 1 if hike_end   == 1   // Month hike ends

label define period_lbl 0 "Before Hike" 1 "After Hike"
label values period period_lbl

label define hike_cycle_lbl         ///
    1 "Pre-Tech Boom (1994-1995)"   ///
    2 "Dot-Com Bubble (1999-2000)"  ///
    3 "Housing Boom (2004-2006)"    ///
    4 "Zero-Rate Exit (2015-2018)"  ///
    5 "Post-COVID Tightening (2022-2023)"
label values hike_cycle hike_cycle_lbl

graph bar erate, over(period, label(angle(45) labsize(small))) ///
    over(hike_cycle, label(labsize(vsmall))) ///
    title("Employment Rates Before and After Rate Hike Periods") ///
    ytitle("Employment Rate (%)") ///
    ylabel(0(20)100, angle(0)) ///
    blabel(bar, format(%9.1f)) ///
    bar(1, color("59 76 132")) ///
    note("Note: Shaded areas represent major Federal Reserve tightening cycles.")

graph export "$figures/figure2_2_2_employment_fedfunds_hike_periods.png", ///
    replace width(2400) height(1500)
di "  Figure 2.2.2 saved."

di "--- 02_aggregate_analysis.do: done ---"
