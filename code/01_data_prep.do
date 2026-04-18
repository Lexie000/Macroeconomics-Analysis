////////////////////////////////////////////////////////////////////////////////
// 01_data_prep.do
// Section 1: Data Preparation
//
// Inputs  : $raw/FEDFUNDS.csv   (or FEDFUNDS.dta if already converted)
//           $raw/cps_raw.dta    (CPS microdata — must be sourced separately;
//                                see README for download instructions)
// Outputs : $proc/fedfunds_bymth.dta
//           $proc/cps_fedfunds_mth.dta   ← master analysis dataset
//
// Run via : 00_master.do  (globals $raw and $proc must already be set)
////////////////////////////////////////////////////////////////////////////////

di ""
di "--- 01_data_prep.do: starting ---"

// --------------------------------------------------------------------------
// 1a. Convert Federal Funds Rate CSV → Stata .dta
// --------------------------------------------------------------------------

// If the .dta already exists skip the import step
cap confirm file "$raw/FEDFUNDS.dta"
if _rc {
    import delimited "$raw/FEDFUNDS.csv", varnames(1) clear
    save "$raw/FEDFUNDS.dta", replace
}

// Keep only the monthly time series
use "$raw/FEDFUNDS.dta", clear

gen daily_date = date(date, "YMD")
gen year        = year(daily_date)
gen month       = month(daily_date)
gen time        = ym(year, month)
format time %tm

keep time fedfunds
save "$proc/fedfunds_bymth.dta", replace

di "  fedfunds_bymth.dta saved."

// --------------------------------------------------------------------------
// 1b. Load CPS microdata and merge with Federal Funds Rate
// --------------------------------------------------------------------------

use "$raw/cps_raw.dta", clear
keep if ind1990 > 0                   // drop invalid industry codes

gen time = ym(year, month)
format time %tm

merge m:1 time using "$proc/fedfunds_bymth.dta"
drop _merge time

save "$proc/cps_fedfunds_mth_raw.dta", replace
di "  cps_fedfunds_mth_raw.dta saved."

// --------------------------------------------------------------------------
// 1c. Variable coding and cleaning
// --------------------------------------------------------------------------

use "$proc/cps_fedfunds_mth_raw.dta", clear

// --- Industry categories (IND1990 codes) ---
gen industry = .
replace industry = 1  if inrange(ind1990, 10, 32)    // Agriculture
replace industry = 2  if inrange(ind1990, 40, 50)    // Mining
replace industry = 3  if ind1990 == 60               // Construction
replace industry = 4  if inrange(ind1990, 100, 392)  // Manufacturing
replace industry = 5  if inrange(ind1990, 400, 472)  // Transport & Utilities
replace industry = 6  if inrange(ind1990, 500, 571)  // Wholesale
replace industry = 7  if inrange(ind1990, 580, 691)  // Retail
replace industry = 8  if inrange(ind1990, 700, 712)  // Finance & Real Estate
replace industry = 9  if inrange(ind1990, 721, 893)  // Services
replace industry = 10 if inrange(ind1990, 900, 960)  // Public Admin

label define industry_lbl ///
    1 "Agriculture"         ///
    2 "Mining"              ///
    3 "Construction"        ///
    4 "Manufacturing"       ///
    5 "Transport & Utilities" ///
    6 "Wholesale"           ///
    7 "Retail"              ///
    8 "Finance & Real Estate" ///
    9 "Services"            ///
    10 "Public Admin"
label values industry industry_lbl

// --- Top-code income as missing ---
// Wage income
replace incwage  = . if incwage  >= 9999999
// Non-wage income
foreach var of varlist incint incdivid incrent {
    replace `var' = . if `var' >= 9999999
}

// --- Employment status ---
gen employment = .
replace employment = 2 if empstat == 10 | empstat == 12                   // Employed
replace employment = 1 if inlist(empstat, 20, 21, 22)                    // Unemployed
replace employment = 0 if empstat == 0 | empstat == 1 | inrange(empstat, 30, 36) // NILF

label define emp_lbl 0 "Not in Labor Force" 1 "Unemployed" 2 "Employed"
label values employment emp_lbl

// --- Labor force participation ---
gen lf_status = .
replace lf_status = 1 if labforce == 2   // In labor force
replace lf_status = 0 if labforce == 1   // Not in labor force

label define lf_lbl 0 "Not in Labor Force" 1 "In Labor Force"
label values lf_status lf_lbl

// --- Save master analysis dataset ---
save "$proc/cps_fedfunds_mth.dta", replace
di "  cps_fedfunds_mth.dta saved — data prep complete."
di "--- 01_data_prep.do: done ---"
