////////////////////////////////////////////////////////////////////////////////
// 03_industry_analysis.do
// Section 3: Industry-Specific Employment Analysis
//
// Inputs  : $proc/cps_fedfunds_mth.dta
// Outputs : $tables/table3_1_yearly_industry_employment.xlsx
//           $tables/table3_2_industry_sensitivity.xlsx
//           $figures/figure3_1_employment_trends.png
//           $figures/figure3_1_supply_employment_trends_separate.png
//
// Run via : 00_master.do  (globals must already be set)
////////////////////////////////////////////////////////////////////////////////

di ""
di "--- 03_industry_analysis.do: starting ---"

// --------------------------------------------------------------------------
// 3.1 Evolution of Industry-Specific Employment Rates (annual averages)
// --------------------------------------------------------------------------

use "$proc/cps_fedfunds_mth.dta", clear

gen time = ym(year, month)
format time %tm

// Monthly employment rate by industry
bysort time industry: egen emp_tot       = total(wtfinl * (employment == 2))
bysort time industry: egen lf_tot        = total(wtfinl * (lf_status == 1))
gen erate_industry = (emp_tot / lf_tot) * 100

// Collapse: monthly → annual averages
collapse (mean) erate_industry, by(year month industry)
collapse (mean) erate_industry, by(year industry)

// Reshape wide for export
reshape wide erate_industry, i(year) j(industry)
forvalues i = 1/10 {
    rename erate_industry`i' industry_`i'
}

label variable industry_1  "Agriculture"
label variable industry_2  "Mining"
label variable industry_3  "Construction"
label variable industry_4  "Manufacturing"
label variable industry_5  "Transport & Utilities"
label variable industry_6  "Wholesale"
label variable industry_7  "Retail"
label variable industry_8  "Finance & Real Estate"
label variable industry_9  "Services"
label variable industry_10 "Public Admin"

export excel using "$tables/table3_1_yearly_industry_employment.xlsx", ///
    replace firstrow(variables)
di "  Table 3.1 saved."

// Reshape back to long for plotting
reshape long industry_, i(year) j(industry)
rename industry_ erate_industry

// Shade tightening cycle years
gen hike = 0
replace hike = 1 if inrange(year, 1994, 1995)
replace hike = 1 if inrange(year, 1999, 2000)
replace hike = 1 if inrange(year, 2004, 2006)
replace hike = 1 if inrange(year, 2015, 2018)
replace hike = 1 if inrange(year, 2022, 2023)

sum erate_industry
local min_y = 80
local max_y = 100
gen min_value = `min_y'
gen max_value = `max_y'

// --- Figure 3.1: All industries on one chart ---
twoway ///
    (rbar min_value max_value year if hike == 1, color(gs14)) ///
    (line erate_industry year if industry == 1,  lcolor(red)    lpattern(solid)   lwidth(medthick)) ///
    (line erate_industry year if industry == 2,  lcolor(gold)   lpattern(solid)   lwidth(medthick)) ///
    (line erate_industry year if industry == 3,  lcolor(blue)   lpattern(solid)   lwidth(medthick)) ///
    (line erate_industry year if industry == 4,  lcolor(green)  lpattern(solid)   lwidth(medthick)) ///
    (line erate_industry year if industry == 5,  lcolor(black)  lpattern(solid)   lwidth(medthick)) ///
    (line erate_industry year if industry == 6,  lcolor(red)    lpattern(dash)    lwidth(medthick)) ///
    (line erate_industry year if industry == 7,  lcolor(gold)   lpattern(dash)    lwidth(medthick)) ///
    (line erate_industry year if industry == 8,  lcolor(blue)   lpattern(dash)    lwidth(medthick)) ///
    (line erate_industry year if industry == 9,  lcolor(green)  lpattern(dash)    lwidth(medthick)) ///
    (line erate_industry year if industry == 10, lcolor(black)  lpattern(dash)    lwidth(medthick)), ///
    title("Industry-Specific Employment Rates in the United States, 1990-2024") ///
    subtitle("Annual Averages") ///
    xtitle("Year") ytitle("Employment Rate (%)") ///
    ylabel(80(5)100, angle(0)) yscale(range(80 100)) ///
    xlabel(1990(5)2025) ///
    legend(order(2 "Agriculture" 3 "Mining" 4 "Construction" 5 "Manufacturing" ///
                 6 "Transport & Utilities" 7 "Wholesale" 8 "Retail" ///
                 9 "Finance & Real Estate" 10 "Services" 11 "Public Admin" ///
                 1 "Rate Hike Period") rows(3) position(6) size(small)) ///
    note("Note: Shaded areas represent major Federal Reserve tightening cycles")

graph export "$figures/figure3_1_employment_trends.png", replace width(2000) height(1200)
di "  Figure 3.1 (combined) saved."

// --- Figure 3.1b: Panel chart — one facet per industry ---
twoway ///
    (rbar min_value max_value year if hike == 1, color(gs14)) ///
    (line erate_industry year if industry == 1,  lcolor(red)    lpattern(solid)) ///
    (line erate_industry year if industry == 2,  lcolor(gold)   lpattern(solid)) ///
    (line erate_industry year if industry == 3,  lcolor(blue)   lpattern(solid)) ///
    (line erate_industry year if industry == 4,  lcolor(green)  lpattern(solid)) ///
    (line erate_industry year if industry == 5,  lcolor(black)  lpattern(solid)) ///
    (line erate_industry year if industry == 6,  lcolor(purple) lpattern(solid)) ///
    (line erate_industry year if industry == 7,  lcolor(orange) lpattern(solid)) ///
    (line erate_industry year if industry == 8,  lcolor(pink)   lpattern(solid)) ///
    (line erate_industry year if industry == 9,  lcolor(brown)  lpattern(solid)) ///
    (line erate_industry year if industry == 10, lcolor(cyan)   lpattern(solid)), ///
    by(industry, col(2) compact ///
        title("Industry-Specific Employment Trends During Tightening Cycles", ///
              size(medlarge)) ///
        subtitle("United States, 1990-2024")) ///
    ytitle("Employment Rate (%)") xtitle("Year") ///
    ylabel(80(5)100, angle(0)) xlabel(1990(5)2027) ///
    legend(order(2 "Agriculture" 3 "Mining" 4 "Construction" ///
                 5 "Manufacturing" 6 "Transport & Utilities" ///
                 7 "Wholesale" 8 "Retail" 9 "Finance & Real Estate" ///
                 10 "Services" 11 "Public Admin" 1 "Rate Hike Period") ///
           size(small))

graph export "$figures/figure3_1_supply_employment_trends_separate.png", ///
    replace width(2400) height(1400)
di "  Figure 3.1 (panel) saved."

// --------------------------------------------------------------------------
// 3.2 Industry Sensitivity to Interest Rate Changes
//     Correlations between industry employment rate and FFR at leads/lags
//     ±1–5 years plus contemporaneous.
// --------------------------------------------------------------------------

use "$proc/cps_fedfunds_mth.dta", clear

bysort year month industry: egen emp_tot       = total(wtfinl * (employment == 2))
bysort year month industry: egen lf_tot        = total(wtfinl * (lf_status == 1))
gen erate_industry = (emp_tot / lf_tot) * 100

// Annual averages by industry
collapse (mean) erate_industry fedfunds, by(year month industry)
collapse (mean) erate_industry fedfunds, by(year industry)
sort industry year

// Generate 5-year lags and leads within each industry
forvalues i = 1/5 {
    bysort industry: gen fedfunds_prev`i' = fedfunds[_n-`i']   // lags
    bysort industry: gen fedfunds_next`i' = fedfunds[_n+`i']   // leads
}

// Store correlation table
decode industry, gen(industry_name)
tempfile results
tempname memhold
postfile `memhold' str32 industry_name ///
    lead5 lead4 lead3 lead2 lead1 contemporaneous lag1 lag2 lag3 lag4 lag5 ///
    maxcorr bestlag using `results'

levelsof industry, local(industries)
foreach ind of local industries {
    local ind_name : label industry_lbl `ind'
    quietly {
        // Lead correlations (FFR leads employment)
        forvalues i = 1/5 {
            corr erate_industry fedfunds_next`i' if industry == `ind'
            local lead`i' = r(rho)
        }
        // Contemporaneous
        corr erate_industry fedfunds if industry == `ind'
        local contemp = r(rho)
        // Lag correlations (FFR lags employment)
        forvalues i = 1/5 {
            corr erate_industry fedfunds_prev`i' if industry == `ind'
            local lag`i' = r(rho)
        }

        // Find highest-magnitude correlation
        local maxcorr = abs(`contemp')
        local bestlag = 0
        forvalues i = 1/5 {
            if abs(`lead`i'') > `maxcorr' {
                local maxcorr = abs(`lead`i'')
                local bestlag = -`i'
            }
        }
        forvalues i = 1/5 {
            if abs(`lag`i'') > `maxcorr' {
                local maxcorr = abs(`lag`i'')
                local bestlag = `i'
            }
        }

        post `memhold' ("`ind_name'") ///
            (`lead5') (`lead4') (`lead3') (`lead2') (`lead1') ///
            (`contemp') (`lag1') (`lag2') (`lag3') (`lag4') (`lag5') ///
            (`maxcorr') (`bestlag')
    }
}
postclose `memhold'

use `results', clear

foreach var of varlist lead* contemporaneous lag* maxcorr {
    format `var' %9.3f
}
label variable maxcorr "Maximum |Correlation|"
label variable bestlag "Best Lag (negative = FFR leads)"
label variable contemporaneous "Contemporaneous Correlation"

sort maxcorr
export excel using "$tables/table3_2_industry_sensitivity.xlsx", ///
    replace firstrow(variables)
di "  Table 3.2 saved."

di "--- 03_industry_analysis.do: done ---"
