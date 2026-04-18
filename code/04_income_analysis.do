////////////////////////////////////////////////////////////////////////////////
// 04_income_analysis.do
// Section 4: Income Structure and Industry Characteristics
//
// Inputs  : $proc/cps_fedfunds_mth.dta
// Outputs : $tables/table4_1_industry_income_composition.xlsx
//           $tables/table4_2_industry_income_levels.xlsx
//           $tables/table4_3_industry_income_sensitivity.xlsx
//           $figures/figure4_1_income_distribution.png
//
// Run via : 00_master.do  (globals must already be set)
//
// Notes on CPI adjustment
//   The dataset's cpi99 deflates to 1999 dollars.
//   A further scalar (314.4 / 166.6) converts to 2024 dollars using the
//   ratio of the 2024 to 1999 annual CPI-U (BLS series CUUR0000SA0).
////////////////////////////////////////////////////////////////////////////////

di ""
di "--- 04_income_analysis.do: starting ---"

local cpi_ratio = 314.4 / 166.6    // 1999→2024 CPI adjustment

// --------------------------------------------------------------------------
// 4.1 Income Composition by Industry
//     Shares of wage, interest, dividend, and rental income in total income
// --------------------------------------------------------------------------

use "$proc/cps_fedfunds_mth.dta", clear

keep industry year incwage incint incdivid incrent wtfinl cpi99
keep if !missing(incwage) & !missing(incint) & !missing(incdivid) & !missing(incrent)

// Deflate to real 1999 dollars
gen real_incwage  = incwage  * cpi99
gen real_incint   = incint   * cpi99
gen real_incdivid = incdivid * cpi99
gen real_incrent  = incrent  * cpi99

// Total real income per person
egen real_total = rowtotal(real_incwage real_incint real_incdivid real_incrent)

// Industry-year weighted sums
collapse (sum) real_inc* real_total wtfinl, by(industry year)

// Weighted average per person
gen avg_wage  = real_incwage  / wtfinl
gen avg_int   = real_incint   / wtfinl
gen avg_divid = real_incdivid / wtfinl
gen avg_rent  = real_incrent  / wtfinl
gen avg_total = real_total    / wtfinl

// Income shares (%)
gen share_wage  = (real_incwage  / real_total) * 100
gen share_int   = (real_incint   / real_total) * 100
gen share_divid = (real_incdivid / real_total) * 100
gen share_rent  = (real_incrent  / real_total) * 100

// Collapse to industry level: mean shares and volatility
collapse (mean) share_* avg_* (sd) sd_total = avg_total, by(industry)

gen income_vol = (sd_total / avg_total) * 100   // coefficient of variation (%)
decode industry, gen(industry_name)

keep industry_name share_* avg_total income_vol
export excel using "$tables/table4_1_industry_income_composition.xlsx", ///
    replace firstrow(variables)
di "  Table 4.1 saved."

// --- Figure 4.1: Stacked bar — income shares by industry ---
gsort -share_wage

graph bar share_wage share_int share_divid share_rent, ///
    over(industry_name, label(angle(45) labsize(small))) ///
    title("Income Distribution by Industry", size(large)) ///
    ytitle("Percent of Total Income", size(small)) ///
    ylabel(0(20)100, angle(0) labsize(small)) ///
    legend(label(1 "Wage") label(2 "Interest") ///
           label(3 "Dividend") label(4 "Rent") ///
           size(small) position(12) rows(1)) ///
    bar(1, color("59 76 132")) bar(2, color("128 133 156")) ///
    bar(3, color("157 159 170")) bar(4, color("189 189 189")) ///
    bgcolor(white) graphregion(color(white)) ///
    bargap(30) ///
    blabel(bar, format(%3.1f) size(vsmall)) ///
    note("Note: All values are expressed as percentages of total income.", size(vsmall)) ///
    ysize(7) xsize(13)

graph export "$figures/figure4_1_income_distribution.png", replace width(3200) height(2000)
di "  Figure 4.1 saved."

// --------------------------------------------------------------------------
// 4.2 Income Levels and Volatility
//     Real income levels (in 2024 dollars) and coefficient of variation
// --------------------------------------------------------------------------

use "$proc/cps_fedfunds_mth.dta", clear

keep industry year month incwage incint incdivid incrent cpi99 wtfinl
keep if !missing(incwage) & !missing(incint) & !missing(incdivid) & !missing(incrent)

// Real 2024 dollars
gen real_wage     = incwage  * cpi99 * `cpi_ratio'
gen real_interest = incint   * cpi99 * `cpi_ratio'
gen real_dividend = incdivid * cpi99 * `cpi_ratio'
gen real_rent     = incrent  * cpi99 * `cpi_ratio'

label var real_wage     "Real Wage Income (2024 USD)"
label var real_interest "Real Interest Income (2024 USD)"
label var real_dividend "Real Dividend Income (2024 USD)"
label var real_rent     "Real Rental Income (2024 USD)"

// Total real income
egen real_total_income = rowtotal(real_wage real_interest real_dividend real_rent)
label var real_total_income "Real Total Income (2024 USD)"

// Income shares
gen share_wage     = (real_wage     / real_total_income) * 100
gen share_interest = (real_interest / real_total_income) * 100
gen share_dividend = (real_dividend / real_total_income) * 100
gen share_rent     = (real_rent     / real_total_income) * 100

// Industry-level income volatility
bysort industry: egen sd_income   = sd(real_total_income)
bysort industry: egen mean_income = mean(real_total_income)
gen cv_income = (sd_income / mean_income) * 100

// Collapse to industry
collapse (mean) real_wage real_total_income share_* cv_income, by(industry)

label var real_wage          "Real Wage Level — Mean (2024 USD)"
label var real_total_income  "Real Total Income — Mean (2024 USD)"
label var share_wage         "Wage Share — Mean (%)"
label var share_interest     "Interest Share — Mean (%)"
label var share_dividend     "Dividend Share — Mean (%)"
label var share_rent         "Rent Share — Mean (%)"
label var cv_income          "Income Volatility — CV (%)"

gsort -real_total_income
export excel using "$tables/table4_2_industry_income_levels.xlsx", ///
    replace firstrow(variables)
di "  Table 4.2 saved."

// --------------------------------------------------------------------------
// 4.3 Industry Income Sensitivity to Monetary Policy
//     OLS: avg_income = α + β·fedfunds + ε  (annual, by industry)
//     Robust standard errors. Reports β, SE, t, p, significance stars.
// --------------------------------------------------------------------------

use "$proc/cps_fedfunds_mth.dta", clear

keep year industry incwage incint incdivid incrent cpi99 wtfinl fedfunds
keep if !missing(incwage)

// Real income in thousands of 2024 dollars
gen real_wage     = incwage  * cpi99 * `cpi_ratio'
gen real_interest = incint   * cpi99 * `cpi_ratio'
gen real_dividend = incdivid * cpi99 * `cpi_ratio'
gen real_rent     = incrent  * cpi99 * `cpi_ratio'
egen total_income = rowtotal(real_wage real_interest real_dividend real_rent)
replace total_income = total_income / 1000    // convert to thousands

// Annual industry averages
collapse (mean) avg_income = total_income fedfunds, by(year industry)

// Run OLS per industry, collect results
tempfile results
tempname memhold
postfile `memhold' str50 industry_name ///
    double(coefficient standard_error t_stat p_value) long(n_obs) using `results'

levelsof industry, local(industries)
foreach ind of local industries {
    local ind_name : label industry_lbl `ind'
    quietly {
        reg avg_income fedfunds if industry == `ind', robust
        local coef = _b[fedfunds]
        local se   = _se[fedfunds]
        local t    = `coef' / `se'
        local p    = 2 * ttail(e(df_r), abs(`t'))
        local n    = e(N)
        post `memhold' ("`ind_name'") (`coef') (`se') (`t') (`p') (`n')
    }
}
postclose `memhold'

use `results', clear

// Significance stars
gen sig_stars = ""
replace sig_stars = "*"   if p_value < 0.05
replace sig_stars = "**"  if p_value < 0.01
replace sig_stars = "***" if p_value < 0.001

format coefficient standard_error %9.2f
format t_stat p_value              %9.3f

gsort coefficient
export excel using "$tables/table4_3_industry_income_sensitivity.xlsx", ///
    replace firstrow(variables)
di "  Table 4.3 saved."

di "--- 04_income_analysis.do: done ---"
