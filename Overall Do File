clear all
global folder "/Users/tianhuiqi_chen/Desktop/eco481_final" 
cd "$folder"

///// Preparing Data ///// 

***** FEDFUNDS to .dta ***** 
* Import FEDFUNDS CSV files and convert to DTA format
local filelist: dir . files "FEDFUNDS.csv"

foreach file of local filelist {
    clear
    import delimited "`file'", varnames(1) clear
    local dtafile = subinstr("`file'", ".csv", ".dta", .)
    save "`dtafile'", replace
}

* Generate time variables and filter for 1990-2024
gen daily_date = date(date, "YMD")
gen year = year(daily_date)
gen month = month(daily_date)
gen time = ym(year, month)
format time %tm
keep time fedfunds
save fedfunds_bymth.dta, replace

***** Merging FEDFUNDS ***** 
* Load CPS data and retain valid industry codes
use cps_raw.dta, clear
keep if ind1990 > 0

* Create time variable for merging
gen time = ym(year, month)
format time %tm

* Merge with FEDFUNDS data
merge m:1 time using fedfunds_bymth.dta
drop _merge time
save "cps_fedfunds_mth_raw.dta", replace

***** Label and Variable Cleanup ***** 
* Load merged data
use cps_fedfunds_mth_raw.dta, clear

* Create industry categories
gen industry = .
replace industry = 1 if inrange(ind1990, 10, 32)  // Agriculture
replace industry = 2 if inrange(ind1990, 40, 50)  // Mining
replace industry = 3 if ind1990 == 60             // Construction
replace industry = 4 if inrange(ind1990, 100, 392) // Manufacturing
replace industry = 5 if inrange(ind1990, 400, 472) // Transport & Utilities
replace industry = 6 if inrange(ind1990, 500, 571) // Wholesale
replace industry = 7 if inrange(ind1990, 580, 691) // Retail
replace industry = 8 if inrange(ind1990, 700, 712) // Finance & Real Estate
replace industry = 9 if inrange(ind1990, 721, 893) // Services
replace industry = 10 if inrange(ind1990, 900, 960) // Public Admin

* Label industry categories
label define industry_lbl ///
    1 "Agriculture" ///
    2 "Mining" ///
    3 "Construction" ///
    4 "Manufacturing" ///
    5 "Transport & Utilities" ///
    6 "Wholesale" ///
    7 "Retail" ///
    8 "Finance & Real Estate" ///
    9 "Services" ///
    10 "Public Admin"
label values industry industry_lbl

* Clean top-coded income variables
foreach var of varlist incwage {
    replace `var' = . if `var' >= 9999999
}

foreach var of varlist incint incdivid incrent {
    replace `var' = . if `var' >= 9999999
}

* Create employment status variable
gen employment = .
replace employment = 2 if empstat == 10 | empstat == 12 // Employed
replace employment = 1 if empstat == 20 | empstat == 21 | empstat == 22 // Unemployed
replace employment = 0 if empstat == 00 | empstat == 01 | inrange(empstat, 30, 36) // Not in labor force

* Label employment status
label define emp_lbl 0 "Not in Labor Force" 1 "Unemployed" 2 "Employed"
label values employment emp_lbl

* Create labor force participation variable
gen lf_status = .
replace lf_status = 1 if labforce == 2 // In labor force
replace lf_status = 0 if labforce == 1 // Not in labor force

* Label labor force participation
label define lf_lbl 0 "Not in Labor Force" 1 "In Labor Force"
label values lf_status lf_lbl

* Save cleaned data
save "cps_fedfunds_mth.dta", replace





////// 2. Aggregate Employment Rate and Federal Funds Rate Analysis //////

***** 2.1 Correlation Analysis *****
use cps_fedfunds_mth.dta, clear

* Calculate employment rate
gen time = ym(year, month)
format time %tm
bysort time: egen emp_tot = total(wtfinl * (employment == 2))
bysort time: egen lf_tot = total(wtfinl * (lf_status == 1))
gen erate = (emp_tot / lf_tot) * 100

* Collapse to monthly data and sort by time
collapse (mean) erate fedfunds, by(time)
sort time

* Generate lagged Federal Funds Rate variables
forvalues i = 1/30 {
    gen fedfunds_lag`i' = fedfunds[_n-`i']
    label variable fedfunds_lag`i' "Federal Funds Rate (t-`i' months)"
}

* Store correlation results
tempfile results
tempname memhold
postfile `memhold' lag correlation using `results'

* Compute correlations between employment rate and lagged rates
forvalues i = 1/30 {
    qui corr erate fedfunds_lag`i' if !missing(erate) & !missing(fedfunds_lag`i')
    post `memhold' (`i') (r(rho))
}
postclose `memhold'

* Save correlation results
use `results', clear
export excel using "table2_1_employment_lagged_fedfunds_correlations.xlsx", replace firstrow(variables)


***** 2.2 Historical Trends and Patterns *****
use cps_fedfunds_mth.dta, clear

* Create time variable and calculate employment rate
gen time = ym(year, month)
format time %tm
bysort time: egen emp_tot = total(wtfinl * (employment == 2))
bysort time: egen lf_tot = total(wtfinl * (lf_status == 1))
gen erate = (emp_tot / lf_tot) * 100 // Employment rate (%)

* Collapse to monthly data and sort
collapse (mean) erate fedfunds, by(time)
sort time

* Generate 1-month lead for the Federal Funds Rate
gen fedfunds_lead1 = fedfunds[_n+1] if _n <= _N-1
label variable fedfunds_lead1 "Federal Funds Rate (t+1 month)"

* Define rate hike periods
gen hike = 0
replace hike = 1 if time >= ym(1994,2) & time <= ym(1995,2)
replace hike = 1 if time >= ym(1999,6) & time <= ym(2000,5)
replace hike = 1 if time >= ym(2004,6) & time <= ym(2006,6)
replace hike = 1 if time >= ym(2015,12) & time <= ym(2018,12)
replace hike = 1 if time >= ym(2022,3) & time <= ym(2023,7)

label variable hike "Rate Hike Periods"

* Define y-axis bounds for shaded areas
sum erate
local min_y = 84
local max_y = 100
gen min_value = `min_y'
gen max_value = `max_y'

* Plot employment and Federal Funds Rate trends
twoway ///
    (rbar min_value max_value time if hike == 1, color(gs14)) ///
    (line erate time, yaxis(1) lcolor(blue) lwidth(medthick)) ///
    (line fedfunds_lead1 time if !missing(fedfunds_lead1), yaxis(2) lcolor(red) lwidth(medthick)), ///
    title("Employment Rate and One-Month-Ahead Federal Funds Rate" "During Monetary Tightening Cycles", size(medium) margin(b=2)) ///
    ytitle("Employment Rate (%)", size(small) margin(r=2)) ytitle("Federal Funds Rate (%)", axis(2) size(small) margin(l=2)) ///
    xtitle("Year", size(small) margin(t=2)) ///
    ylabel(84(4)100, angle(0) grid labsize(small)) ylabel(0(2)10, axis(2) angle(0) labsize(small)) ///
    xlabel(360(60)780, angle(45) labsize(small)) xscale(r(360 780)) ///
    legend(order(2 "Employment Rate" 3 "Future Federal Funds Rate (1-month Lead)" 1 "Rate Hike Period") ///
           rows(1) position(6) size(small) symxsize(6) symysize(2) keygap(2)) ///
    note("Note: Shaded areas represent major Federal Reserve tightening cycles." ///
         "The Federal Funds Rate is shifted forward by one month to show its lead relationship with employment rate.", ///
         size(vsmall) margin(t=2)) ///
    graphregion(margin(l=2 r=2 t=2 b=2)) ///
    ysize(5) xsize(8)

* Save the graph
graph export "figure2_2_1_employment_fedfunds_hike_periods.png", replace width(2400) height(1500)


* Plot employment rates before and after hike periods

* Identify start and end points of hike periods
gen hike_start = .
replace hike_start = 1 if hike == 0 & hike[_n+1] == 1  // Start of hike
gen hike_end = .
replace hike_end = 1 if hike == 1 & hike[_n+1] == 0  // End of hike

* Generate hike cycle identifier
gen hike_cycle = .
replace hike_cycle = sum(hike_start) if hike_start == 1
replace hike_cycle = sum(hike_end) if hike_end == 1

* Define periods relative to hikes
gen period = .
replace period = 0 if hike_start == 1  // Before hike
replace period = 1 if hike_end == 1  // After hike

* Label periods for visualization
label define period_lbl 0 "Before Hike" 1 "After Hike"
label values period period_lbl

* Create labels for hike cycles
label define hike_cycle_lbl ///
    1 "Pre-Tech Boom (1994-1995)" ///
    2 "Dot-Com Bubble (1999-2000)" ///
    3 "Housing Boom (2004-2006)" ///
    4 "Zero-Rate Exit (2015-2018)" ///
    5 "Post-COVID Tightening (2022-2023)"
label values hike_cycle hike_cycle_lbl

* Generate bar chart for employment rates before and after hikes
graph bar erate, over(period, label(angle(45) labsize(small))) ///
    over(hike_cycle, label(labsize(vsmall))) ///  // Changed labsize to tiny for period names
    title("Employment Rates Before and After Hike Periods") ///
    ytitle("Employment Rate (%)") ///
    ylabel(0(20)100, angle(0)) ///
    blabel(bar, format(%9.1f)) ///
    bar(1, color("59 76 132")) ///
    note("Note: Shaded areas represent major Federal Reserve tightening cycles.")

* Save the graph
graph export "figure2_2_2_employment_fedfunds_hike_periods.png", replace width(2400) height(1500)





////// 3. Industry-Specific Analysis //////

***** 3.1 The Evolution of Industry-Specific Employment Rate *****
* Load data
use cps_fedfunds_mth.dta, clear

* Generate year-month variable for aggregation
gen time = ym(year, month)
format time %tm

* Calculate employment rates by industry using survey weights
bysort time industry: egen emp_tot = total(wtfinl * (employment == 2))
bysort time industry: egen lf_tot = total(wtfinl * (lf_status == 1))
gen erate_industry = (emp_tot / lf_tot) * 100

* Aggregate data to monthly level
collapse (mean) erate_industry, by(year month industry)

* Further aggregate to annual averages
collapse (mean) erate_industry, by(year industry)

* Reshape to wide format for clarity
reshape wide erate_industry, i(year) j(industry)

* Rename variables for industry categories
forvalues i = 1/10 {
    rename erate_industry`i' industry_`i'
}

* Apply descriptive labels to variables
label variable industry_1 "Agriculture"
label variable industry_2 "Mining"
label variable industry_3 "Construction"
label variable industry_4 "Manufacturing"
label variable industry_5 "Transport & Utilities"
label variable industry_6 "Wholesale"
label variable industry_7 "Retail"
label variable industry_8 "Finance & Real Estate"
label variable industry_9 "Services"
label variable industry_10 "Public Admin"

* Export the data to xlsx for reference
export excel using "table3_1_yearly_industry_employment.xlsx", replace firstrow(variables)

* Reshape back to long format for plotting
reshape long industry_, i(year) j(industry)
rename industry_ erate_industry

* Define Federal Reserve tightening periods
gen hike = 0
replace hike = 1 if year >= 1994 & year <= 1995
replace hike = 1 if year >= 1999 & year <= 2000
replace hike = 1 if year >= 2004 & year <= 2006
replace hike = 1 if year >= 2015 & year <= 2018
replace hike = 1 if year >= 2022 & year <= 2023

* Define y-axis limits for shaded areas
sum erate_industry
local min_y = 80
local max_y = 100
gen min_value = `min_y'
gen max_value = `max_y'

* Plot industry-specific employment rates with shaded hike periods
twoway (rbar min_value max_value year if hike == 1, color(gs14)) ///
       (line erate_industry year if industry == 1, lcolor(red) lpattern(solid) lwidth(medthick)) ///
       (line erate_industry year if industry == 2, lcolor(gold) lpattern(solid) lwidth(medthick)) ///
       (line erate_industry year if industry == 3, lcolor(blue) lpattern(solid) lwidth(medthick)) ///
       (line erate_industry year if industry == 4, lcolor(green) lpattern(solid) lwidth(medthick)) ///
       (line erate_industry year if industry == 5, lcolor(black) lpattern(solid) lwidth(medthick)) ///
       (line erate_industry year if industry == 6, lcolor(red) lpattern(dash) lwidth(medthick)) ///
       (line erate_industry year if industry == 7, lcolor(gold) lpattern(dash) lwidth(medthick)) ///
       (line erate_industry year if industry == 8, lcolor(blue) lpattern(dash) lwidth(medthick)) ///
       (line erate_industry year if industry == 9, lcolor(green) lpattern(dash) lwidth(medthick)) ///
       (line erate_industry year if industry == 10, lcolor(black) lpattern(dash) lwidth(medthick)), ///
       title("Industry-Specific Employment Rates in the United States, 1990-2024") ///
       subtitle("Annual Averages") ///
       xtitle("Year") ///
       ytitle("Employment Rate (%)") ///
       ylabel(80(5)100, angle(0)) ///
       yscale(range(80 100)) ///
       xlabel(1990(5)2025) ///
       legend(order(2 "Agriculture" 3 "Mining" 4 "Construction" 5 "Manufacturing" ///
              6 "Transport & Utilities" 7 "Wholesale" 8 "Retail" ///
              9 "Finance & Real Estate" 10 "Services" 11 "Public Admin" 1 "Rate Hike Period") ///
              rows(3) position(6) size(small)) ///
       note("Note: Shaded areas represent major Federal Reserve tightening cycles")
              
graph export "figure3_1_employment_trends.png", replace width(2000) height(1200)

* Generate panel graphs by industry
twoway ///
    (rbar min_value max_value year if hike == 1, color(gs14)) ///
    (line erate_industry year if industry == 1, lcolor(red) lpattern(solid)) ///
    (line erate_industry year if industry == 2, lcolor(gold) lpattern(solid)) ///
    (line erate_industry year if industry == 3, lcolor(blue) lpattern(solid)) ///
    (line erate_industry year if industry == 4, lcolor(green) lpattern(solid)) ///
    (line erate_industry year if industry == 5, lcolor(black) lpattern(solid)) ///
    (line erate_industry year if industry == 6, lcolor(purple) lpattern(solid)) ///
    (line erate_industry year if industry == 7, lcolor(orange) lpattern(solid)) ///
    (line erate_industry year if industry == 8, lcolor(pink) lpattern(solid)) ///
    (line erate_industry year if industry == 9, lcolor(brown) lpattern(solid)) ///
    (line erate_industry year if industry == 10, lcolor(cyan) lpattern(solid)), ///
    by(industry, col(2) compact ///
        title("Industry-Specific Employment Trends During Tightening Cycles", size(medlarge)) ///
        subtitle("United States, 1990-2024")) ///
    ytitle("Employment Rate (%)") ///
    xtitle("Year") ///
    ylabel(80(5)100, angle(0)) ///
    xlabel(1990(5)2027) ///
    legend(order(2 "Agriculture" 3 "Mining" 4 "Construction" ///
                 5 "Manufacturing" 6 "Transport & Utilities" ///
                 7 "Wholesale" 8 "Retail" 9 "Finance & Real Estate" ///
                 10 "Services" 11 "Public Admin" 1 "Rate Hike Period") size(small))

graph export "figure3_1_supply_employment_trends_seperate.png", replace width(2400) height(1400)



***** 3.2 Industry Sensitivity to Interest Rate Changes *****
use "cps_fedfunds_mth.dta", clear

* Calculate employment rates by industry using survey weights
bysort year month industry: egen emp_tot = total(wtfinl * (employment == 2))
bysort year month industry: egen lf_tot = total(wtfinl * (lf_status == 1))
gen erate_industry = (emp_tot / lf_tot) * 100

* Aggregate to monthly and then annual rates
collapse (mean) erate_industry fedfunds, by(year month industry)
collapse (mean) erate_industry fedfunds, by(year industry)

* Prepare data for time series analysis
sort industry year

* Generate lagged and lead variables for interest rates
forvalues i = 1/5 {
    bysort industry: gen fedfunds_prev`i' = fedfunds[_n-`i'] // Lags
    bysort industry: gen fedfunds_next`i' = fedfunds[_n+`i'] // Leads
}

* Initialize temporary files for storing correlation results
tempfile results
tempname memhold

* Set up postfile for saving results
decode industry, gen(industry_name)
postfile `memhold' str32 industry_name lead5 lead4 lead3 lead2 lead1 contemporaneous lag1 lag2 lag3 lag4 lag5 maxcorr bestlag using `results'

* Calculate correlations for each industry
levelsof industry, local(industries)
foreach ind of local industries {
    local ind_name: label industry_lbl `ind'
    quietly {
        * Correlations with leads
        forvalues i = 1/5 {
            corr erate_industry fedfunds_next`i' if industry == `ind'
            local lead`i' = r(rho)
        }
        
        * Contemporaneous correlation
        corr erate_industry fedfunds if industry == `ind'
        local contemp = r(rho)
        
        * Correlations with lags
        forvalues i = 1/5 {
            corr erate_industry fedfunds_prev`i' if industry == `ind'
            local lag`i' = r(rho)
        }
        
        * Identify maximum correlation and corresponding lag/lead
        local maxcorr = abs(`contemp')
        local bestlag = 0
        local bestcorr = `contemp'
        
        forvalues i = 1/5 {
            if abs(`lead`i'') > `maxcorr' {
                local maxcorr = abs(`lead`i'')
                local bestlag = -`i'
                local bestcorr = `lead`i''
            }
        }
        forvalues i = 1/5 {
            if abs(`lag`i'') > `maxcorr' {
                local maxcorr = abs(`lag`i'')
                local bestlag = `i'
                local bestcorr = `lag`i''
            }
        }
        
        * Save results for the current industry
        post `memhold' ("`ind_name'") (`lead5') (`lead4') (`lead3') (`lead2') (`lead1') ///
                          (`contemp') (`lag1') (`lag2') (`lag3') (`lag4') (`lag5') ///
                          (`bestcorr') (`bestlag')
    }
}
postclose `memhold'

* Load and prepare results
use `results', clear

* Format correlation coefficients
foreach var of varlist lead* contemp* lag* maxcorr {
    format `var' %9.3f
}

* Add variable labels
label variable maxcorr "Maximum Correlation"
label variable bestlag "Best Lag (negative=lead)"
label variable contemporaneous "Contemporaneous Correlation"

* Sort results by maximum correlation
sort maxcorr

* Export results to Excel
export excel using "table3_2_industry_sensitivity.xlsx", replace firstrow(variables)






///// 4. Income Structure and Industry Characteristics /////

***** 4.1 Income Composition Analysis *****
use "cps_fedfunds_mth.dta", clear

* Retain relevant variables and observations
keep industry year incwage incint incdivid incrent wtfinl cpi99
keep if !missing(incwage) & !missing(incint) & !missing(incdivid) & !missing(incrent)

* Compute real income values
gen real_incwage = incwage * cpi99
gen real_incint = incint * cpi99
gen real_incdivid = incdivid * cpi99
gen real_incrent = incrent * cpi99

* Calculate total real income
egen real_total = rowtotal(real_incwage real_incint real_incdivid real_incrent)

* Aggregate to industry-year level with weighted sums
collapse (sum) real_inc* real_total wtfinl, by(industry year)

* Calculate weighted average incomes
gen avg_wage = real_incwage / wtfinl
gen avg_int = real_incint / wtfinl
gen avg_divid = real_incdivid / wtfinl
gen avg_rent = real_incrent / wtfinl
gen avg_total = real_total / wtfinl

* Compute income shares as percentages
gen share_wage = (real_incwage / real_total) * 100
gen share_int = (real_incint / real_total) * 100
gen share_divid = (real_incdivid / real_total) * 100
gen share_rent = (real_incrent / real_total) * 100

* Collapse to industry level with averages and standard deviations
collapse (mean) share_* avg_* (sd) sd_total = avg_total, by(industry)

* Calculate coefficient of variation for total income
gen income_vol = (sd_total / avg_total) * 100

* Decode industry variable for clarity
decode industry, gen(industry_name)

* Retain necessary variables for output
keep industry_name share_* avg_total income_vol

* Export results to Excel
export excel using "table4_1_industry_income_analysis.xlsx", replace firstrow(variables)

* Sort data for visualization
gsort -share_wage

* Create bar chart for income distribution
graph bar share_wage share_int share_divid share_rent, ///
    over(industry_name, label(angle(45) labsize(small))) ///
    title("Income Distribution by Industry", size(large)) ///
    ytitle("Percent of Total Income", size(small)) ///
    ylabel(0(20)100, angle(0) labsize(small)) ///
    legend(label(1 "Wage") label(2 "Interest") ///
           label(3 "Dividend") label(4 "Rent") ///
           size(small) position(12) rows(1)) ///
    bar(1, color("59 76 132")) ///
    bar(2, color("128 133 156")) ///
    bar(3, color("157 159 170")) ///
    bar(4, color("189 189 189")) ///
    bgcolor(white) graphregion(color(white)) ///
    bargap(30) ///
    blabel(bar, format(%3.1f) size(vsmall)) /// 
    note("Note: All values are expressed as percentages of total income", size(vsmall)) ///
    ysize(7) xsize(13)

* Save the graph as a PNG
graph export "figure4_1_income_distribution.png", replace ///
    width(3200) height(2000)


	
***** 4.2 Income Levels and Volatility to Interest Rate *****
use "cps_fedfunds_mth.dta", clear

* Retain essential variables and valid observations
keep industry year month incwage incint incdivid incrent cpi99 wtfinl
keep if !missing(incwage) & !missing(incint) & !missing(incdivid) & !missing(incrent)

* Adjust income to 1999 dollars
gen real_wage_1999 = incwage * cpi99
gen real_interest_1999 = incint * cpi99
gen real_dividend_1999 = incdivid * cpi99
gen real_rent_1999 = incrent * cpi99

* Adjust for 2024 dollars using CPI ratio
local ratio = 314.4/166.6
gen real_wage = real_wage_1999 * `ratio'
gen real_interest = real_interest_1999 * `ratio'
gen real_dividend = real_dividend_1999 * `ratio'
gen real_rent = real_rent_1999 * `ratio'

* Label variables
label var real_wage "Real Wage Level"
label var real_interest "Real Interest Income Level"
label var real_dividend "Real Dividend Income Level"
label var real_rent "Real Rental Income Level"

* Compute total real income
egen real_total_income = rowtotal(real_wage real_interest real_dividend real_rent)
label var real_total_income "Real Total Income Level"

* Calculate income shares
gen share_wage = (real_wage / real_total_income) * 100
gen share_interest = (real_interest / real_total_income) * 100
gen share_dividend = (real_dividend / real_total_income) * 100
gen share_rent = (real_rent / real_total_income) * 100

* Industry-level income statistics (mean, standard deviation, CV)
bysort industry: egen sd_income = sd(real_total_income)
bysort industry: egen mean_income = mean(real_total_income)
gen cv_income = (sd_income / mean_income) * 100

* Collapse data to industry-level
collapse (mean) real_wage real_total_income share_wage share_interest share_dividend share_rent cv_income, by(industry)

* Final variable labels
label var real_wage "Real Wage Level (Mean)"
label var real_total_income "Real Total Income Level (Mean)"
label var share_wage "Wage Share (Mean, %)"
label var share_interest "Interest Share (Mean, %)"
label var share_dividend "Dividend Share (Mean, %)"
label var share_rent "Rent Share (Mean, %)"
label var cv_income "Income Volatility (CV, %)"

* Sort by total income and export results
gsort -real_total_income
export excel using "table4_2_industry_income_analysis.xlsx", replace firstrow(variables)


***** 4.3 Industry-Specific Income Sensitivity to Monetary Policy Changes *****
use "cps_fedfunds_mth.dta", clear

* Retain valid observations and key variables
keep year industry incwage incint incdivid incrent cpi99 wtfinl fedfunds
keep if !missing(incwage)

* Calculate real income values and total income
gen real_wage = incwage * cpi99
gen real_interest = incint * cpi99
gen real_dividend = incdivid * cpi99
gen real_rent = incrent * cpi99
egen total_income = rowtotal(real_wage real_interest real_dividend real_rent)
replace total_income = total_income / 1000

* Aggregate income and Federal Funds Rate to annual industry-level
collapse (mean) avg_income = total_income fedfunds, by(year industry)

* Run regressions by industry
tempfile results
tempname memhold
postfile `memhold' str50 industry_name double(coefficient standard_error t_stat p_value) long(n_obs) using `results'

levelsof industry, local(industries)
foreach ind of local industries {
    local ind_name: label industry_lbl `ind'
    qui {
        reg avg_income fedfunds if industry == `ind', robust
        local coef = _b[fedfunds]
        local se = _se[fedfunds]
        local t = `coef' / `se'
        local p = 2 * ttail(e(df_r), abs(`t'))
        local n = e(N)
        
        post `memhold' ("`ind_name'") (`coef') (`se') (`t') (`p') (`n')
    }
}
postclose `memhold'

* Load and process results
use `results', clear

* Add significance stars
gen sig_stars = ""
replace sig_stars = "*" if p_value < 0.05
replace sig_stars = "**" if p_value < 0.01
replace sig_stars = "***" if p_value < 0.001

* Format variables
format coefficient standard_error %9.2f
format t_stat p_value %9.3f

* Sort results and export to Excel
gsort coefficient
export excel using "table4_3_industry_income_sensitivity.xlsx", replace firstrow(variables)
