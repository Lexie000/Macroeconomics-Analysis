////////////////////////////////////////////////////////////////////////////////
// 00_master.do
// The Temporal and Sectoral Effects of Federal Funds Rate Changes on
// U.S. Employment: A Multi-Industry Analysis (1990-2024)
//
// Authors : Tianhuiqi Chen, Jinmin (Lexie) Li
// Course  : ECO 481
// Updated : 2024-12-20
//
// PURPOSE
//   Single entry point for full replication. Run this file from the repo root
//   and all four analysis modules will execute in order.
//
// HOW TO RUN
//   1. Open Stata and cd to the repo root:
//        cd "/path/to/Macroeconomics-Analysis"
//   2. Run:
//        do code/00_master.do
//
// OUTPUTS
//   Tables  → output/tables/
//   Figures → output/figures/
//   Cleaned data → data/processed/
////////////////////////////////////////////////////////////////////////////////

clear all
set more off
version 16

// --------------------------------------------------------------------------
// 1. Portable root: set to wherever this repo lives on your machine.
//    By default we use the current working directory, so just cd to the
//    repo root before running. You can also hard-code the path here:
//
//    global root "/absolute/path/to/Macroeconomics-Analysis"
// --------------------------------------------------------------------------
if "$root" == "" global root "`c(pwd)'"

// Subdirectory globals — do NOT edit these
global raw     "$root/data/raw"
global proc    "$root/data/processed"
global code    "$root/code"
global tables  "$root/output/tables"
global figures "$root/output/figures"

di "Root   : $root"
di "Raw    : $raw"
di "Proc   : $proc"
di "Tables : $tables"
di "Figures: $figures"

// --------------------------------------------------------------------------
// 2. Install required user-written packages (skip if already installed)
// --------------------------------------------------------------------------
foreach pkg in reghdfe ftools estout outreg2 {
    cap which `pkg'
    if _rc {
        di "Installing `pkg'..."
        ssc install `pkg', replace
    }
}

// --------------------------------------------------------------------------
// 3. Run analysis modules in order
// --------------------------------------------------------------------------
do "$code/01_data_prep.do"
do "$code/02_aggregate_analysis.do"
do "$code/03_industry_analysis.do"
do "$code/04_income_analysis.do"

di ""
di "================================================================"
di "  Replication complete."
di "  Tables  → $tables"
di "  Figures → $figures"
di "================================================================"
