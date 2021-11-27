*** Bruhn et al "The Impact of High School Financial Education: Evidence from a Large-Scale Evaluation in Brazil" forthcoming in the American Economic Journal: Applied Economics
*** This do-file generates the tables shown in the text and appendix

*** List of Tables :
***   * Table 1: Summary Statistics and Test of Balance of the Randomization [Maybe change list of variables to reflect new outcome tables]
***   * Table 2: Balance on time-invariant variables for students present in follow-up 1 and for students present in follow-up 2
***   * Table 3: School Intervention effect on STUDENT FINANCIAL PROFICIENCY
***   * Table 4: School Intervention effect on graduation rates (admin data)

***   Cross-cutting themes:
***   * Table 5: School Intervention effect on STUDENT SAVINGS ATTITUDES AND BEHAVIOR
***   * Table 6: School Intervention effect on STUDENT Money Management ATTITUDES AND BEHAVIOR

***   Specific topics:
***   * Table 7: School Intervention effect on Purchasing Behavior
***   * Table 8: School Intervention effect on Borrowing Behavior
***   * Table 9: School Intervention effect on Student Work & Entrepreneurship

***   Impact on parents:
***   * Table 10: School Intervention Trickle Up Impact on Parent Financial Knowledge & Behavior
***   * Table 11: IMPACT OF PARENT FINANCIAL EDUCATION WORKSHOP

***   Appendix:
***   * Ap Table 2: Balance for students present only in baseline and for students present only in baseline and follow-up 1
***   * Ap Table 3: Quantile Regressions for Student Financial Proficiency
***   * Ap Table 4: Student Participation in Household Finance


clear
clear matrix
set mem 200m
set matsize 1000
set varabbrev off
set more off

capture cd "C:\Users\wb317778\Box Sync\Brazil\AEJ Revision\Data"

*************************    TABLE 1: Sum Stats and Balance    *****************************

* Data
use "school_intervention_panel_final.dta", clear
keep if round==1
keep if treatment ~= .	// We will report summ stats on the observations we can test the balance of randomization and treatment effects

sort cd_escola id_geral
egen tag_school = tag(cd_escola)
egen tag_school_test = tag(cd_escola) if bl_test == 1
egen tag_school_aluno = tag(cd_escola) if bl_aluno == 1

* Outcomes
local test female dumm_rp_08_bl dumm_rp_09_bl dumm_rp_24_bl dumm_rp_14_bl dumm_rp_23_bl vl_proficiencia_bl  
local aluno dumm_rp_49_bl dumm_rp_50_bl dumm_rp_65A_bl poupar_final2_bl dumm_rp_64A_bl dumm_negotiates_bl autonomia_final2_bl 
local school matriculas docentes abandonona1sriemdio aprovaona1sriemdio 
local xvars `test' `aluno' `school' 

* Matrix for output
local z: word count `xvars' 

matrix T = J(`z', 7, . ) 
matrix rownames T = `xvars' 
matrix colnames T = ns n Control sdC Treatment sdT pv 

* Estimation
foreach var in `test' { 
	global `var'label: var label `var' 
	
	reg `var' treatment, clu(cd_escola) 
		g sample_`var' = (e(sample)==1) 
		local pr = 2*ttail(e(df_r),abs(_b[treatment]/_se[treatment])) 
		mat T[rownumb(T, "`var'"), colnumb(T,"pv")] = `pr' 

	sum `var' if sample_`var' == 1, d
	mat T[rownumb(T, "`var'"), colnumb(T,"n")] = `r(N)' 
	
	sum `var' if sample_`var' == 1 & treatment == 1 
		mat T[rownumb(T, "`var'"), colnumb(T,"sdT")] = `r(sd)' 
		mat T[rownumb(T, "`var'"), colnumb(T,"Treatment")] = `r(mean)'
	sum `var' if sample_`var' == 1 & treatment == 0 
		mat T[rownumb(T, "`var'"), colnumb(T,"sdC")] = `r(sd)' 
		mat T[rownumb(T, "`var'"), colnumb(T,"Control")] = `r(mean)'

	egen max_`var' = max(`var'), by(cd_escola)
	count if max_`var' ~= . & tag_school_test == 1 
	mat T[rownumb(T, "`var'"), colnumb(T,"ns")] = `r(N)' 
} 
foreach var in `aluno' { 
	global `var'label: var label `var' 
	
	reg `var' treatment, clu(cd_escola) 
		g sample_`var' = (e(sample)==1) 
		local pr = 2*ttail(e(df_r),abs(_b[treatment]/_se[treatment])) 
		mat T[rownumb(T, "`var'"), colnumb(T,"pv")] = `pr'

	sum `var' if sample_`var' == 1, d
	mat T[rownumb(T, "`var'"), colnumb(T,"n")] = `r(N)' 
	
	sum `var' if sample_`var' == 1 & treatment == 1 
		mat T[rownumb(T, "`var'"), colnumb(T,"sdT")] = `r(sd)' 
		mat T[rownumb(T, "`var'"), colnumb(T,"Treatment")] = `r(mean)' 
	sum `var' if sample_`var' == 1 & treatment == 0 
		mat T[rownumb(T, "`var'"), colnumb(T,"sdC")] = `r(sd)' 
		mat T[rownumb(T, "`var'"), colnumb(T,"Control")] = `r(mean)'

	egen max_`var' = max(`var'), by(cd_escola)
	count if max_`var' ~= . & tag_school_aluno == 1 
	mat T[rownumb(T, "`var'"), colnumb(T,"ns")] = `r(N)' 
} 
foreach var in `school' { 
	global `var'label: var label `var' 

	reg `var' treatment if tag_school == 1, clu(cd_escola)
		g sample_`var' = (e(sample)==1) 
		local pr = 2*ttail(e(df_r),abs(_b[treatment]/_se[treatment])) 
		mat T[rownumb(T, "`var'"), colnumb(T,"pv")] = `pr' 
	
	sum `var' if tag_school == 1 & sample_`var' == 1, d
	mat T[rownumb(T, "`var'"), colnumb(T,"ns")] = `r(N)' 
	
	sum `var' if tag_school == 1 & sample_`var' == 1 & treatment == 1 
		mat T[rownumb(T, "`var'"), colnumb(T,"sdT")] = `r(sd)' 
		mat T[rownumb(T, "`var'"), colnumb(T,"Treatment")] = `r(mean)' 
	sum `var' if tag_school == 1 & sample_`var' == 1 & treatment == 0 
		mat T[rownumb(T, "`var'"), colnumb(T,"sdC")] = `r(sd)' 
		mat T[rownumb(T, "`var'"), colnumb(T,"Control")] = `r(mean)' 
} 

* Formating Matrix
matrix list T

clear 
svmat T 
rename T1 ns
rename T2 n 
rename T3 Control
rename T4 sdC
rename T5 Treatment
rename T6 sdT
rename T7 pv

gen var_name = "" 
gen var = ""
order var_name 

local i = 1 
foreach var in `xvars' { 
	replace var_name = "$`var'label" in `i' 
	replace var = "`var'" in `i' 
	local i = `i' + 1 
} 

label var var_name "Variable" 
label var ns "N (School)" 
label var n "N (Students)" 
label var Control "Control" 
label var sdC "SD" 
label var Treatment "Treatment" 
label var sdT "SD" 
label var pv "p-value" 

foreach var of varlist n ns { 
	replace `var' = round(`var', 1) 
	format `var' %9.0f 
} 

foreach var of varlist Treatment Control sdT sdC { 
	replace `var' = round(`var', 0.01) 
	format `var' %9.2f 
} 

foreach var of varlist pv { 
	replace `var' = round(`var', 0.001) 
	format `var' %9.3f 
} 

gen stars = "" 
replace stars = "*" if pv <= 0.10 
replace stars = "**" if pv <= 0.05 
replace stars = "***" if pv <= 0.01 

foreach x in female dumm_rp_08_bl dumm_rp_09_bl dumm_rp_24_bl dumm_rp_14_bl dumm_rp_23_bl dumm_rp_49_bl dumm_rp_50_bl dumm_rp_65A_bl dumm_rp_64A_bl dumm_negotiates_bl {
	foreach y in sdC sdT {
		replace `y' = . if var == "`x'" 
	}
}
drop var

* Output
capture erase "table1.csv"
outsheet using "table1.csv", replace comma

***********************************************************************


************************    Table 2 - Panel A: Balance on time-invariant variables for students present in follow-up 1  ***********************

* Data
use "school_intervention_panel_final.dta", clear

keep if round==0
keep if fu1_test==1 | fu1_aluno==1
keep if treatment ~= .	// We will report summ stats on the observations we can test the balance of randomization and treatment effects

sort cd_escola id_geral
egen tag_school_coded = tag(cd_escola) if female_coded ~= .
egen tag_school_responsavel = tag(cd_escola) if fu1_responsavel == 1

* Outcomes
local coded female_coded 
local responsavel p_employee_fup p_selfempl_fup p_other_fup 
local xvars `coded' `responsavel' 

* Matrix for output
local z: word count `xvars' 

matrix T = J(`z', 7, . ) 
matrix rownames T = `xvars' 
matrix colnames T = ns n Control sdC Treatment sdT pv

* Estimation
foreach var in `coded' { 
	global `var'label: var label `var' 
	
	reg `var' treatment, r clu(cd_escola) 
		g sample_`var' = (e(sample)==1) 
		local pr = 2*ttail(e(df_r),abs(_b[treatment]/_se[treatment])) 
		mat T[rownumb(T, "`var'"), colnumb(T,"pv")] = `pr' 

	sum `var' if sample_`var' == 1, d
	mat T[rownumb(T, "`var'"), colnumb(T,"n")] = `r(N)' 
	
	sum `var' if sample_`var' == 1 & treatment == 1 
		mat T[rownumb(T, "`var'"), colnumb(T,"sdT")] = `r(sd)' 
		mat T[rownumb(T, "`var'"), colnumb(T,"Treatment")] = `r(mean)' 
	sum `var' if sample_`var' == 1 & treatment == 0 
		mat T[rownumb(T, "`var'"), colnumb(T,"sdC")] = `r(sd)' 
		mat T[rownumb(T, "`var'"), colnumb(T,"Control")] = `r(mean)'

	egen max_`var' = max(`var'), by(cd_escola)
	count if max_`var' ~= . & tag_school_coded == 1 
	mat T[rownumb(T, "`var'"), colnumb(T,"ns")] = `r(N)' 
}
foreach var in `responsavel' { 
	global `var'label: var label `var' 
	
	reg `var' treatment, r clu(cd_escola)
		g sample_`var' = (e(sample)==1) 
		local pr = 2*ttail(e(df_r),abs(_b[treatment]/_se[treatment])) 
		mat T[rownumb(T, "`var'"), colnumb(T,"pv")] = `pr' 

	sum `var' if sample_`var' == 1, d
	mat T[rownumb(T, "`var'"), colnumb(T,"n")] = `r(N)' 
	
	sum `var' if sample_`var' == 1 & treatment == 1 
		mat T[rownumb(T, "`var'"), colnumb(T,"sdT")] = `r(sd)' 
		mat T[rownumb(T, "`var'"), colnumb(T,"Treatment")] = `r(mean)' 
	sum `var' if sample_`var' == 1 & treatment == 0 
		mat T[rownumb(T, "`var'"), colnumb(T,"sdC")] = `r(sd)' 
		mat T[rownumb(T, "`var'"), colnumb(T,"Control")] = `r(mean)'

	egen max_`var' = max(`var'), by(cd_escola)
	count if max_`var' ~= . & tag_school_responsavel == 1 
	mat T[rownumb(T, "`var'"), colnumb(T,"ns")] = `r(N)' 
} 

* Formating Matrix
matrix list T

clear 
svmat T 
rename T1 ns
rename T2 n 
rename T3 Control
rename T4 sdC
rename T5 Treatment
rename T6 sdT
rename T7 pv

gen var_name = "" 
gen var = ""
order var_name 

local i = 1 
foreach var in `xvars' { 
	replace var_name = "$`var'label" in `i' 
	replace var = "`var'" in `i' 
	local i = `i' + 1 
} 

label var var_name "Variable" 
label var ns "N (School)" 
label var n "N (Students)" 
label var Control "Control" 
label var sdC "SD" 
label var Treatment "Treatment" 
label var sdT "SD" 
label var pv "p-value" 

foreach var of varlist n ns { 
	replace `var' = round(`var', 1) 
	format `var' %9.0f 
} 

foreach var of varlist Treatment Control sdT sdC { 
	replace `var' = round(`var', 0.01) 
	format `var' %9.2f 
} 

foreach var of varlist pv { 
	replace `var' = round(`var', 0.001) 
	format `var' %9.3f 
} 

gen stars = "" 
replace stars = "*" if pv <= 0.10 
replace stars = "**" if pv <= 0.05 
replace stars = "***" if pv <= 0.01 

foreach x in female p_female_fup p_employee_fup p_selfempl_fup p_other_fup {
	foreach y in sdC sdT {
		replace `y' = . if var == "`x'" 
	}
}
drop var

* Output
capture erase "table2_panelA.csv"
outsheet using "table2_panelA.csv", replace comma

************************   Table 2 - Panel B: Balance on time-invariant variables for students present in follow-up 2  ***********************

* Data
use "school_intervention_panel_final.dta", clear

keep if round==1
keep if fu2_test==1 | fu2_aluno==1
keep if treatment ~= .	// We will report summ stats on the observations we can test the balance of randomization and treatment effects

sort cd_escola id_geral
egen tag_school_coded = tag(cd_escola) if female_coded ~= .
egen tag_school_responsavel = tag(cd_escola) if fu2_responsavel == 1

* Outcomes
local coded female_coded 
local responsavel dumm_rp_08p_fup p_employee_fup p_selfempl_fup p_other_fup
local xvars `coded' `responsavel' 

* Matrix for output
local z: word count `xvars' 

matrix T = J(`z', 7, . ) 
matrix rownames T = `xvars' 
matrix colnames T = ns n Control sdC Treatment sdT pv

* Estimation
foreach var in `coded' { 
	global `var'label: var label `var' 
	
	reg `var' treatment, r clu(cd_escola)
		g sample_`var' = (e(sample)==1) 
		local pr = 2*ttail(e(df_r),abs(_b[treatment]/_se[treatment])) 
		mat T[rownumb(T, "`var'"), colnumb(T,"pv")] = `pr' 

	sum `var' if sample_`var' == 1, d
	mat T[rownumb(T, "`var'"), colnumb(T,"n")] = `r(N)' 
	
	sum `var' if sample_`var' == 1 & treatment == 1 
		mat T[rownumb(T, "`var'"), colnumb(T,"sdT")] = `r(sd)' 
		mat T[rownumb(T, "`var'"), colnumb(T,"Treatment")] = `r(mean)' 
	sum `var' if sample_`var' == 1 & treatment == 0 
		mat T[rownumb(T, "`var'"), colnumb(T,"sdC")] = `r(sd)' 
		mat T[rownumb(T, "`var'"), colnumb(T,"Control")] = `r(mean)'

	egen max_`var' = max(`var'), by(cd_escola)
	count if max_`var' ~= . & tag_school_coded == 1 
	mat T[rownumb(T, "`var'"), colnumb(T,"ns")] = `r(N)' 
}
foreach var in `responsavel' { 
	global `var'label: var label `var' 
	
	reg `var' treatment, r clu(cd_escola)
		g sample_`var' = (e(sample)==1) 
		local pr = 2*ttail(e(df_r),abs(_b[treatment]/_se[treatment])) 
		mat T[rownumb(T, "`var'"), colnumb(T,"pv")] = `pr' 

	sum `var' if sample_`var' == 1, d
	mat T[rownumb(T, "`var'"), colnumb(T,"n")] = `r(N)' 
	
	sum `var' if sample_`var' == 1 & treatment == 1 
		mat T[rownumb(T, "`var'"), colnumb(T,"sdT")] = `r(sd)' 
		mat T[rownumb(T, "`var'"), colnumb(T,"Treatment")] = `r(mean)' 
	sum `var' if sample_`var' == 1 & treatment == 0 
		mat T[rownumb(T, "`var'"), colnumb(T,"sdC")] = `r(sd)' 
		mat T[rownumb(T, "`var'"), colnumb(T,"Control")] = `r(mean)'

	egen max_`var' = max(`var'), by(cd_escola)
	count if max_`var' ~= . & tag_school_responsavel == 1 
	mat T[rownumb(T, "`var'"), colnumb(T,"ns")] = `r(N)' 
} 

* Formating Matrix
matrix list T

clear 
svmat T 
rename T1 ns
rename T2 n 
rename T3 Control
rename T4 sdC
rename T5 Treatment
rename T6 sdT
rename T7 pv

gen var_name = "" 
gen var = ""
order var_name 

local i = 1 
foreach var in `xvars' { 
	replace var_name = "$`var'label" in `i' 
	replace var = "`var'" in `i' 
	local i = `i' + 1 
} 

label var var_name "Variable" 
label var ns "N (School)" 
label var n "N (Students)" 
label var Control "Control" 
label var sdC "SD" 
label var Treatment "Treatment" 
label var sdT "SD" 
label var pv "p-value" 

foreach var of varlist n ns { 
	replace `var' = round(`var', 1) 
	format `var' %9.0f 
} 

foreach var of varlist Treatment Control sdT sdC { 
	replace `var' = round(`var', 0.01) 
	format `var' %9.2f 
} 

foreach var of varlist pv { 
	replace `var' = round(`var', 0.001) 
	format `var' %9.3f 
} 

gen stars = "" 
replace stars = "*" if pv <= 0.10 
replace stars = "**" if pv <= 0.05 
replace stars = "***" if pv <= 0.01 

foreach x in female p_female_fup dumm_rp_08p_fup p_employee_fup p_selfempl_fup p_other_fup {
	foreach y in sdC sdT {
		replace `y' = . if var == "`x'" 
	}
}
drop var

* Output
capture erase "table2_panelB.csv"
outsheet using "table2_panelB.csv", replace comma
***********************************************************************


**************************    TABLE 3: Financial Proficiency    ******************************

* Data
use "school_intervention_panel_final.dta", clear

* Outcomes
local outcomes vl_proficiencia_fup

* Estimation: Panel A
est drop _all
local i = 1
foreach var of varlist `outcomes' { 

	reg `var' treatment if round==0, r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
	
	reg `var' treatment if round==1, r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
} 

* Output: Panel A
capture erase "table3_panelA.csv"
#delimit ;
esttab table1_* using "table3_panelA.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N N_clust controlmean controlsd f_test, fmt(%9.3f %9.0g %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Number of Clusters" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr

* Estimation: Panel B
est drop _all
local i = 1
foreach var of varlist `outcomes' { 
	
	bys pair_all: egen flag_`var'0=mean(treatment) if `var'~=. & round==0
	gen pair_`var'0=pair_all
	replace pair_`var'0=0 if flag_`var'0==0 | flag_`var'0==1
	
	areg `var' treatment if round==0, a(pair_`var'0) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
	
	bys pair_all: egen flag_`var'1=mean(treatment) if `var'~=. & round==1
	gen pair_`var'1=pair_all
	replace pair_`var'1=0 if flag_`var'1==0 | flag_`var'1==1
	
	areg `var' treatment if round==1,  a(pair_`var'1)r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
} 

* Output: Panel B
capture erase "table3_panelB.csv"
#delimit ;
esttab table1_* using "table3_panelB.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N N_clust controlmean controlsd f_test, fmt(%9.3f %9.0g %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Number of Clusters" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr

* Estimation: Panel C
est drop _all
local i = 1
foreach var of varlist `outcomes' { 

	local bl "" 
	local bl = subinstr("`var'","_fup","_bl",.) 
	gen miss_`bl' = 0 
	replace miss_`bl' = 1 if `bl' == . 
	replace `bl' = 0 if `bl' == . 
	
	areg `var' treatment `bl' miss_`bl' female_coded miss_f_coded if round==0, a(pair_`var'0) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
	
	areg `var' treatment `bl' miss_`bl' female_coded miss_f_coded if round==1, a(pair_`var'1) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
} 

* Output: Panel C
capture erase "table3_panelC.csv"
#delimit ;
esttab table1_* using "table3_panelC.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N N_clust controlmean controlsd f_test, fmt(%9.3f %9.0g %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Number of Clusters" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr
***********************************************************************


**************************  Table 4: School Intervention effect on graduation rates (admin data)  **************************

* Data
use "school_admin_data_final.dta", clear

* Outcomes
local outcomes aprovao2011 reprovao2011 abandono2011

* Estimation
est drop _all
local i = 1

gen missingBL_aprovao2011 = (aprovao2009==.)
gen missingBL_reprovao2011 = (reprovao2009==.)
gen missingBL_abandono2011 = (abandono2009==.)

replace aprovao2009=1 if aprovao2009==.
replace reprovao2009=1 if reprovao2009==.
replace abandono2009=1 if abandono2009==.

gen aprovao2011_baseline=aprovao2009
gen reprovao2011_baseline=reprovao2009
gen abandono2011_baseline=abandono2009


foreach var of varlist `outcomes' {
	reg `var' treatment, r 
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	local ++ i 
	
	bys pair_all: egen flag_`var'=mean(treatment) if `var'~=.
	gen pair_`var'=pair_all
	replace pair_`var'=0 if flag_`var'==0 | flag_`var'==1
	
	areg `var' treatment , a(pair_`var') r
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	local ++ i 
	
	areg `var' treatment `var'_baseline missingBL_`var', a(pair_`var') r
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	local ++ i 

	
} 

* Output
capture erase "table4.csv"
#delimit ;
esttab table1_* using "table4.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N N_clust controlmean controlsd, fmt(%9.3f %9.0g %9.0g %9.3f %9.3f) labels("R-squared" "N" "Number of Clusters" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group")) ;
#delimit cr
***********************************************************************


**************************    TABLE 5: SAVINGS   ******************************

* Data
use "school_intervention_panel_final.dta", clear

* Outcomes
local outcomes1 dumm_rp_59_fup dumm_rp_53B_fup dumm_rp_65A_fup dumm_rp_61_fup poupar_final2_fup 
local outcomes2 dumm_rp_20C_fup 

* Estimation: Panel A
est drop _all
local i = 1
foreach var of varlist `outcomes1' { 

	reg `var' treatment if round==0, r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		if "`var'"=="dumm_rp_20C_fup" estadd scalar controlsd = r(sd)
	est sto table2_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table2_`i'
	local ++ i 
	
	reg `var' treatment if round==1, r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		if "`var'"=="dumm_rp_20C_fup" estadd scalar controlsd = r(sd)
	est sto table2_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table2_`i'
	local ++ i 
} 
foreach var of varlist `outcomes2' { 
	
	reg `var' treatment if round==1, r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		if "`var'"=="dumm_rp_20C_fup" estadd scalar controlsd = r(sd)
	est sto table2_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table2_`i'
	local ++ i 
} 

* Output: Panel A
capture erase "table5_PanelA.csv"
#delimit ;
esttab table2_* using "table5_PanelA.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N N_clust controlmean controlsd f_test, fmt(%9.3f %9.0g %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Number of Clusters" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr

* Estimation: Panel B
est drop _all
local i = 1
foreach var of varlist `outcomes1' { 

	bys pair_all: egen flag_`var'0=mean(treatment) if `var'~=. & round==0
	gen pair_`var'0=pair_all
	replace pair_`var'0=0 if flag_`var'0==0 | flag_`var'0==1

	areg `var' treatment if round==0, a(pair_`var'0) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		if "`var'"=="dumm_rp_20C_fup" estadd scalar controlsd = r(sd)
	est sto table2_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table2_`i'
	local ++ i 
	
	bys pair_all: egen flag_`var'1=mean(treatment) if `var'~=. & round==1
	gen pair_`var'1=pair_all
	replace pair_`var'1=0 if flag_`var'1==0 | flag_`var'1==1
	
	areg `var' treatment if round==1, a(pair_`var'1) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		if "`var'"=="dumm_rp_20C_fup" estadd scalar controlsd = r(sd)
	est sto table2_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table2_`i'
	local ++ i 
} 
foreach var of varlist `outcomes2' { 

	bys pair_all: egen flag_`var'1=mean(treatment) if `var'~=. & round==1
	gen pair_`var'1=pair_all
	replace pair_`var'1=0 if flag_`var'1==0 | flag_`var'1==1
	
	areg `var' treatment if round==1, a(pair_`var'1) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		if "`var'"=="dumm_rp_20C_fup" estadd scalar controlsd = r(sd)
	est sto table2_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table2_`i'
	local ++ i 
} 

* Output: Panel B
capture erase "table5_PanelB.csv"
#delimit ;
esttab table2_* using "table5_PanelB.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N N_clust controlmean controlsd f_test, fmt(%9.3f %9.0g %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Number of Clusters" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr

* Estimation: Panel C
est drop _all
local i = 1
foreach var of varlist `outcomes1' { 

	local bl "" 
	local bl = subinstr("`var'","_fup","_bl",.) 
	gen miss_`bl' = 0 
	replace miss_`bl' = 1 if `bl' == . 
	replace `bl' = 0 if `bl' == . 

	areg `var' treatment `bl' miss_`bl' female_coded miss_f_coded if round==0, a(pair_`var'0) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		if "`var'"=="dumm_rp_20C_fup" estadd scalar controlsd = r(sd)
	est sto table2_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table2_`i'
	local ++ i 
	
	areg `var' treatment `bl' miss_`bl' female_coded miss_f_coded if round==1, a(pair_`var'1) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		if "`var'"=="dumm_rp_20C_fup" estadd scalar controlsd = r(sd)
	est sto table2_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table2_`i'
	local ++ i 
} 

* Output: Panel C
capture erase "table5_PanelC.csv"
#delimit ;
esttab table2_* using "table5_PanelC.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N N_clust controlmean controlsd f_test, fmt(%9.3f %9.0g %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Number of Clusters" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr

***********************************************************************


**************************    TABLE 6: Money Management    ******************************

* Data
use "school_intervention_panel_final.dta", clear

* Outcomes
local outcomes dumm_rp_64A_fup dumm_negotiates_fup dumm_search_fup autonomia_final2_fup 

* Estimation: Panel A
est drop _all
local i = 1
foreach var of varlist `outcomes' { 

	reg `var' treatment if round==0, r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
	
	reg `var' treatment if round==1, r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
} 

* Output: Panel A
capture erase "table6_panelA.csv"
#delimit ;
esttab table1_* using "table6_panelA.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N N_clust controlmean controlsd f_test, fmt(%9.3f %9.0g %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Number of Clusters" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr

* Estimation: Panel B
est drop _all
local i = 1
foreach var of varlist `outcomes' { 
	
	bys pair_all: egen flag_`var'0=mean(treatment) if `var'~=. & round==0
	gen pair_`var'0=pair_all
	replace pair_`var'0=0 if flag_`var'0==0 | flag_`var'0==1
	
	areg `var' treatment if round==0, a(pair_`var'0) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
	
	bys pair_all: egen flag_`var'1=mean(treatment) if `var'~=. & round==1
	gen pair_`var'1=pair_all
	replace pair_`var'1=0 if flag_`var'1==0 | flag_`var'1==1
	
	areg `var' treatment if round==1,  a(pair_`var'1)r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
} 

* Output: Panel B
capture erase "table6_panelB.csv"
#delimit ;
esttab table1_* using "table6_panelB.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N N_clust controlmean controlsd f_test, fmt(%9.3f %9.0g %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Number of Clusters" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr

* Estimation: Panel C
est drop _all
local i = 1
foreach var of varlist `outcomes' { 

	local bl "" 
	local bl = subinstr("`var'","_fup","_bl",.) 
	gen miss_`bl' = 0 
	replace miss_`bl' = 1 if `bl' == . 
	replace `bl' = 0 if `bl' == . 
	
	areg `var' treatment `bl' miss_`bl' female_coded miss_f_coded if round==0, a(pair_`var'0) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
	
	areg `var' treatment `bl' miss_`bl' female_coded miss_f_coded if round==1, a(pair_`var'1) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
} 

* Output: Panel C
capture erase "table6_panelC.csv"
#delimit ;
esttab table1_* using "table6_panelC.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N N_clust controlmean controlsd f_test, fmt(%9.3f %9.0g %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Number of Clusters" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr

***********************************************************************


**************************    TABLE 7: Purchasing Behavior   ******************************

* Data
use "school_intervention_panel_final.dta", clear

* Outcomes
local outcomes dumm_rp88__92AB_fup dumm_rp88__92C_fup dumm_rp88__92D_fup  
 
* Estimation: Panel A
est drop _all
local i = 1
foreach var of varlist `outcomes' { 

	reg `var' treatment if round==0, r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
	
	reg `var' treatment if round==1, r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
} 

* Output: Panel A
capture erase "table7_panelA.csv"
#delimit ;
esttab table1_* using "table7_panelA.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N N_clust controlmean controlsd f_test, fmt(%9.3f %9.0g %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Number of Clusters" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr

* Estimation: Panel B
est drop _all
local i = 1
foreach var of varlist `outcomes' { 
	
	bys pair_all: egen flag_`var'0=mean(treatment) if `var'~=. & round==0
	gen pair_`var'0=pair_all
	replace pair_`var'0=0 if flag_`var'0==0 | flag_`var'0==1
	
	areg `var' treatment if round==0, a(pair_`var'0) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
	
	bys pair_all: egen flag_`var'1=mean(treatment) if `var'~=. & round==1
	gen pair_`var'1=pair_all
	replace pair_`var'1=0 if flag_`var'1==0 | flag_`var'1==1
	
	areg `var' treatment if round==1,  a(pair_`var'1)r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
} 

* Output: Panel B
capture erase "table7_panelB.csv"
#delimit ;
esttab table1_* using "table7_panelB.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N N_clust controlmean controlsd f_test, fmt(%9.3f %9.0g %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Number of Clusters" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr

* Estimation: Panel C
est drop _all
local i = 1
foreach var of varlist `outcomes' { 

	local bl "" 
	local bl = subinstr("`var'","_fup","_bl",.) 
	gen miss_`bl' = 0 
	replace miss_`bl' = 1 if `bl' == . 
	replace `bl' = 0 if `bl' == . 
	
	areg `var' treatment `bl' miss_`bl' female_coded miss_f_coded if round==0, a(pair_`var'0) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
	
	areg `var' treatment `bl' miss_`bl' female_coded miss_f_coded if round==1, a(pair_`var'1) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
} 

* Output: Panel C
capture erase "table7_panelC.csv"
#delimit ;
esttab table1_* using "table7_panelC.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N N_clust controlmean controlsd f_test, fmt(%9.3f %9.0g %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Number of Clusters" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr
***********************************************************************


**************************    TABLE 8: Borrowing Behavior   ******************************

* Data
use "school_intervention_panel_final.dta", clear

* Outcomes
local outcomes1 dumm_rp_55_fup dumm_rp_56_fup  
local outcomes2 dumm_rp_57f_fup dumm_rp_57s_fup dumm_rp_57i_fup 
 
* Estimation: Panel A
est drop _all
local i = 1
foreach var of varlist `outcomes1' { 

	reg `var' treatment if round==0, r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
	
	reg `var' treatment if round==1, r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
} 

foreach var of varlist `outcomes2' { 

	reg `var' treatment if round==0, r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
} 

* Output: Panel A
capture erase "table8_panelA.csv"
#delimit ;
esttab table1_* using "table8_panelA.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N N_clust controlmean controlsd f_test, fmt(%9.3f %9.0g %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Number of Clusters" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr

* Estimation: Panel B
est drop _all
local i = 1
foreach var of varlist `outcomes1' { 
	
	bys pair_all: egen flag_`var'0=mean(treatment) if `var'~=. & round==0
	gen pair_`var'0=pair_all
	replace pair_`var'0=0 if flag_`var'0==0 | flag_`var'0==1
	
	areg `var' treatment if round==0, a(pair_`var'0) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
	
	bys pair_all: egen flag_`var'1=mean(treatment) if `var'~=. & round==1
	gen pair_`var'1=pair_all
	replace pair_`var'1=0 if flag_`var'1==0 | flag_`var'1==1
	
	areg `var' treatment if round==1,  a(pair_`var'1)r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
} 

foreach var of varlist `outcomes2' { 
	
	bys pair_all: egen flag_`var'0=mean(treatment) if `var'~=. & round==0
	gen pair_`var'0=pair_all
	replace pair_`var'0=0 if flag_`var'0==0 | flag_`var'0==1
	
	areg `var' treatment if round==0, a(pair_`var'0) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
} 

* Output: Panel B
capture erase "table8_panelB.csv"
#delimit ;
esttab table1_* using "table8_panelB.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N N_clust controlmean controlsd f_test, fmt(%9.3f %9.0g %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Number of Clusters" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr

* Estimation: Panel C
est drop _all
local i = 1
foreach var of varlist `outcomes1' { 

	local bl "" 
	local bl = subinstr("`var'","_fup","_bl",.) 
	gen miss_`bl' = 0 
	replace miss_`bl' = 1 if `bl' == . 
	replace `bl' = 0 if `bl' == . 
	
	areg `var' treatment `bl' miss_`bl' female_coded miss_f_coded if round==0, a(pair_`var'0) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
	
	areg `var' treatment `bl' miss_`bl' female_coded miss_f_coded if round==1, a(pair_`var'1) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
} 

foreach var of varlist `outcomes2' { 

	local bl "" 
	local bl = subinstr("`var'","_fup","_bl",.) 
	gen miss_`bl' = 0 
	replace miss_`bl' = 1 if `bl' == . 
	replace `bl' = 0 if `bl' == . 
	
	areg `var' treatment `bl' miss_`bl' female_coded miss_f_coded if round==0, a(pair_`var'0) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
} 

* Output: Panel C
capture erase "table8_panelC.csv"
#delimit ;
esttab table1_* using "table8_panelC.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N N_clust controlmean controlsd f_test, fmt(%9.3f %9.0g %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Number of Clusters" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr
***********************************************************************


**************************    TABLE 9: Work & Enterpreneurship    ******************************

* Data
use "school_intervention_panel_final.dta", clear

* Outcomes
local outcomes business_fup employee_fup dumm_rp_50_fup 

* Estimation: Panel A
est drop _all
local i = 1
foreach var of varlist `outcomes' { 

	reg `var' treatment if round==0, r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
	
	reg `var' treatment if round==1, r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
} 

* Output: Panel A
capture erase "table9_panelA.csv"
#delimit ;
esttab table1_* using "table9_panelA.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N N_clust controlmean controlsd f_test, fmt(%9.3f %9.0g %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Number of Clusters" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr

* Estimation: Panel B
est drop _all
local i = 1
foreach var of varlist `outcomes' { 
	
	bys pair_all: egen flag_`var'0=mean(treatment) if `var'~=. & round==0
	gen pair_`var'0=pair_all
	replace pair_`var'0=0 if flag_`var'0==0 | flag_`var'0==1
	
	areg `var' treatment if round==0, a(pair_`var'0) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
	
	bys pair_all: egen flag_`var'1=mean(treatment) if `var'~=. & round==1
	gen pair_`var'1=pair_all
	replace pair_`var'1=0 if flag_`var'1==0 | flag_`var'1==1
	
	areg `var' treatment if round==1,  a(pair_`var'1)r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
} 

* Output: Panel B
capture erase "table9_panelB.csv"
#delimit ;
esttab table1_* using "table9_panelB.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N N_clust controlmean controlsd f_test, fmt(%9.3f %9.0g %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Number of Clusters" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr

* Estimation: Panel C
est drop _all
local i = 1
foreach var of varlist `outcomes' { 

	local bl "" 
	local bl = subinstr("`var'","_fup","_bl",.) 
	gen miss_`bl' = 0 
	replace miss_`bl' = 1 if `bl' == . 
	replace `bl' = 0 if `bl' == . 
	
	areg `var' treatment `bl' miss_`bl' female_coded miss_f_coded if round==0, a(pair_`var'0) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
	
	areg `var' treatment `bl' miss_`bl' female_coded miss_f_coded if round==1, a(pair_`var'1) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
} 

* Output: Panel C
capture erase "table9_panelC.csv"
#delimit ;
esttab table1_* using "table9_panelC.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N N_clust controlmean controlsd f_test, fmt(%9.3f %9.0g %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Number of Clusters" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr

***********************************************************************


**************************    TABLE 10: Parent Financial Knowledge & Behavior   ******************************

* Data
use "school_intervention_panel_final.dta", clear

* Outcomes
local outcomes1 dumm_rp_36p_fup dumm_rp_37p_fup dumm_rp_14p_fup dumm_formal_saving_fup 
local outcomes2 dumm_rp_41p_fup dumm_rp_12Cp_fup 

* Estimation: Panel A
est drop _all
local i = 1
foreach var of varlist `outcomes1' { 

	reg `var' treatment if round==0, r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
	est sto table2_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table2_`i'
	local ++ i 
	
	reg `var' treatment if round==1, r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
	est sto table2_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table2_`i'
	local ++ i 
} 
foreach var of varlist `outcomes2' { 
	
	reg `var' treatment if round==1, r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
	est sto table2_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table2_`i'
	local ++ i 
} 

* Output: Panel A
capture erase "table10_PanelA.csv"
#delimit ;
esttab table2_* using "table10_PanelA.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N N_clust controlmean controlsd f_test, fmt(%9.3f %9.0g %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Number of Clusters" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr

* Estimation: Panel B
est drop _all
local i = 1
foreach var of varlist `outcomes1' { 

	bys pair_all: egen flag_`var'0=mean(treatment) if `var'~=. & round==0
	gen pair_`var'0=pair_all
	replace pair_`var'0=0 if flag_`var'0==0 | flag_`var'0==1

	areg `var' treatment if round==0, a(pair_`var'0) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
	est sto table2_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table2_`i'
	local ++ i 
	
	bys pair_all: egen flag_`var'1=mean(treatment) if `var'~=. & round==1
	gen pair_`var'1=pair_all
	replace pair_`var'1=0 if flag_`var'1==0 | flag_`var'1==1
	
	areg `var' treatment if round==1, a(pair_`var'1) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
	est sto table2_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table2_`i'
	local ++ i 
} 
foreach var of varlist `outcomes2' { 

	bys pair_all: egen flag_`var'1=mean(treatment) if `var'~=. & round==1
	gen pair_`var'1=pair_all
	replace pair_`var'1=0 if flag_`var'1==0 | flag_`var'1==1
	
	areg `var' treatment if round==1, a(pair_`var'1) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
	est sto table2_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table2_`i'
	local ++ i 
} 

* Output: Panel B
capture erase "table10_PanelB.csv"
#delimit ;
esttab table2_* using "table10_PanelB.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N N_clust controlmean controlsd f_test, fmt(%9.3f %9.0g %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Number of Clusters" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr

* Estimation: Panel C
est drop _all
local i = 1
foreach var of varlist `outcomes1' { 

	local bl "" 
	local bl = subinstr("`var'","_fup","_bl",.) 
	gen miss_`bl' = 0 
	replace miss_`bl' = 1 if `bl' == . 
	replace `bl' = 0 if `bl' == . 

	areg `var' treatment `bl' miss_`bl' female_coded miss_f_coded if round==0, a(pair_`var'0) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
	est sto table2_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table2_`i'
	local ++ i 
	
	areg `var' treatment `bl' miss_`bl' female_coded miss_f_coded if round==1, a(pair_`var'1) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
	est sto table2_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table2_`i'
	local ++ i 
} 

* Output: Panel C
capture erase "table10_PanelC.csv"
#delimit ;
esttab table2_* using "table10_PanelC.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N N_clust controlmean controlsd f_test, fmt(%9.3f %9.0g %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Number of Clusters" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr

***********************************************************************


**************************    TABLE 11: Parent Financial Education Workshop  ******************************

* Data
use "school_intervention_panel_final.dta", clear
	
* Outcomes
local outcomes1 dumm_rp_14p_fup dumm_formal_saving_fup dumm_rp_53B_fup dumm_rp88__92AB_fup dumm_rp88__92C_fup dumm_rp88__92D_fup dumm_rp_55_fup dumm_rp_56_fup 
local outcomes2 dumm_rp_12Cp_fup dumm_rp_20C_fup dumm_rp_41p_fup 

* Labels
foreach var in dumm_rp_53B_fup dumm_rp_20C_fup dumm_rp88__92AB_fup dumm_rp88__92C_fup dumm_rp88__92D_fup dumm_rp_55_fup dumm_rp_56_fup {
	global `var'label: var label `var'
	label var `var' "Student: $`var'label"
}
foreach var in dumm_rp_41p_fup dumm_rp_14p_fup dumm_formal_saving_fup dumm_rp_12Cp_fup {
	global `var'label: var label `var'
	label var `var' "Parent: $`var'label"
}

* Estimation: Panel A
est drop _all
local i = 1
foreach var of varlist `outcomes1' `outcomes2' { 
	
	reg `var' treatment_workshop if round==1 & treatment_workshop~=., r
	summ `var' if treatment_workshop == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		if "`var'"=="dumm_rp_20C_fup" | "`var'"=="dumm_rp_12Cp_fup" {
			estadd scalar controlsd = r(sd)
		}
	est sto table7_`i' 
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r  
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table7_`i'
	local ++ i 
} 

* Output: Panel A
capture erase "table11_PanelA.csv"
#delimit ;
esttab table7_* using "table11_PanelA.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment_workshop) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N controlmean controlsd f_test, fmt(%9.3f %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr


* Estimation: Panel B
est drop _all
local i = 1
foreach var of varlist `outcomes1' `outcomes2' { 
	
	areg `var' treatment_workshop if round==1 & treatment_workshop~=., a(strata) r
	summ `var' if treatment_workshop == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		if "`var'"=="dumm_rp_20C_fup" | "`var'"=="dumm_rp_12Cp_fup" {
			estadd scalar controlsd = r(sd)
		}
	est sto table7_`i' 
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r  
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table7_`i'
	local ++ i 
} 

* Output: Panel B
capture erase "table11_PanelB.csv"
#delimit ;
esttab table7_* using "table11_PanelB.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment_workshop) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N controlmean controlsd f_test, fmt(%9.3f %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr

* Estimation: Panel C
est drop _all
local i = 1
foreach var of varlist `outcomes1' { 

	local bl "" 
	local bl = subinstr("`var'","_fup","_bl",.) 
	gen miss_`bl' = 0 
	replace miss_`bl' = 1 if `bl' == . 
	replace `bl' = 0 if `bl' == .  
	
	areg `var' treatment_workshop `bl' miss_`bl' female_coded miss_f_coded if round==1 & treatment_workshop~=., a(strata) r
	summ `var' if treatment_workshop == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		if "`var'"=="dumm_rp_20C_fup" | "`var'"=="dumm_rp_12Cp_fup" {
			estadd scalar controlsd = r(sd)
		}
	est sto table7_`i' 
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r  
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table7_`i'
	local ++ i 
}

* Output: Panel C
capture erase "table11_PanelC.csv"
#delimit ;
esttab table7_* using "table11_PanelC.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment_workshop) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N controlmean controlsd f_test, fmt(%9.3f %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr

***********************************************************************


************************    APPENDIX TABLE 2 - Panel A: Balance for students only present in baseline   ***********************

* Data
use "school_intervention_panel_final.dta", clear

foreach wave in bl fu1 fu2 {
	foreach survey in test aluno {
		egen i_`wave'_`survey' = max(`wave'_`survey'), by(id_geral)
		replace i_`wave'_`survey' = 0 if i_`wave'_`survey' == .
	}
}

keep if round==1
drop if i_fu1_test==1 | i_fu1_aluno==1 | i_fu2_test==1 | i_fu2_aluno==1
drop if i_bl_test==0 & i_bl_aluno==0
keep if treatment ~= .	// We will report summ stats on the observations we can test the balance of randomization and treatment effects

sort cd_escola id_geral
egen tag_school_test = tag(cd_escola) if bl_test == 1
egen tag_school_aluno = tag(cd_escola) if bl_aluno == 1

* Outcomes
local test female dumm_rp_08_bl dumm_rp_09_bl dumm_rp_24_bl dumm_rp_14_bl dumm_rp_23_bl vl_proficiencia_bl  
local aluno dumm_rp_49_bl dumm_rp_50_bl dumm_rp_64A_bl dumm_negotiates_bl autonomia_final2_bl poupar_final2_bl
local xvars `test' `aluno' 

* Matrix for output
local z: word count `xvars' 

matrix T = J(`z', 7, . ) 
matrix rownames T = `xvars' 
matrix colnames T = ns n Control sdC Treatment sdT pv

* Estimation
foreach var in `test' { 
	global `var'label: var label `var' 
	
	reg `var' treatment, r clu(cd_escola) 
		g sample_`var' = (e(sample)==1) 
		local pr = 2*ttail(e(df_r),abs(_b[treatment]/_se[treatment])) 
		mat T[rownumb(T, "`var'"), colnumb(T,"pv")] = `pr' 

	sum `var' if sample_`var' == 1, d
	mat T[rownumb(T, "`var'"), colnumb(T,"n")] = `r(N)' 
	
	sum `var' if sample_`var' == 1 & treatment == 1 
		mat T[rownumb(T, "`var'"), colnumb(T,"sdT")] = `r(sd)' 
		mat T[rownumb(T, "`var'"), colnumb(T,"Treatment")] = `r(mean)' 
	sum `var' if sample_`var' == 1 & treatment == 0 
		mat T[rownumb(T, "`var'"), colnumb(T,"sdC")] = `r(sd)' 
		mat T[rownumb(T, "`var'"), colnumb(T,"Control")] = `r(mean)'

	egen max_`var' = max(`var'), by(cd_escola)
	count if max_`var' ~= . & tag_school_test == 1 
	mat T[rownumb(T, "`var'"), colnumb(T,"ns")] = `r(N)' 
} 
foreach var in `aluno' { 
	global `var'label: var label `var' 
	
	reg `var' treatment, r clu(cd_escola)
		g sample_`var' = (e(sample)==1) 
		local pr = 2*ttail(e(df_r),abs(_b[treatment]/_se[treatment])) 
		mat T[rownumb(T, "`var'"), colnumb(T,"pv")] = `pr' 

	sum `var' if sample_`var' == 1, d
	mat T[rownumb(T, "`var'"), colnumb(T,"n")] = `r(N)' 
	
	sum `var' if sample_`var' == 1 & treatment == 1 
		mat T[rownumb(T, "`var'"), colnumb(T,"sdT")] = `r(sd)' 
		mat T[rownumb(T, "`var'"), colnumb(T,"Treatment")] = `r(mean)' 
	sum `var' if sample_`var' == 1 & treatment == 0 
		mat T[rownumb(T, "`var'"), colnumb(T,"sdC")] = `r(sd)' 
		mat T[rownumb(T, "`var'"), colnumb(T,"Control")] = `r(mean)'

	egen max_`var' = max(`var'), by(cd_escola)
	count if max_`var' ~= . & tag_school_aluno == 1 
	mat T[rownumb(T, "`var'"), colnumb(T,"ns")] = `r(N)' 
} 

* Formating Matrix
matrix list T

clear 
svmat T 
rename T1 ns
rename T2 n 
rename T3 Control
rename T4 sdC
rename T5 Treatment
rename T6 sdT
rename T7 pv

gen var_name = "" 
gen var = ""
order var_name 

local i = 1 
foreach var in `xvars' { 
	replace var_name = "$`var'label" in `i' 
	replace var = "`var'" in `i' 
	local i = `i' + 1 
} 

label var var_name "Variable" 
label var ns "N (School)" 
label var n "N (Students)" 
label var Control "Control" 
label var sdC "SD" 
label var Treatment "Treatment" 
label var sdT "SD" 
label var pv "p-value" 

foreach var of varlist n ns { 
	replace `var' = round(`var', 1) 
	format `var' %9.0f 
} 

foreach var of varlist Treatment Control sdT sdC { 
	replace `var' = round(`var', 0.01) 
	format `var' %9.2f 
} 

foreach var of varlist pv { 
	replace `var' = round(`var', 0.001) 
	format `var' %9.3f 
} 

gen stars = "" 
replace stars = "*" if pv <= 0.10 
replace stars = "**" if pv <= 0.05 
replace stars = "***" if pv <= 0.01 

foreach x in female dumm_rp_08_bl dumm_rp_09_bl dumm_rp_24_bl dumm_rp_14_bl dumm_rp_23_bl dumm_rp_49_bl dumm_rp_50_bl dumm_rp_64A_bl dumm_negotiates_bl {
	foreach y in sdC sdT {
		replace `y' = . if var == "`x'" 
	}
}
drop var

* Output
capture erase "tableA2a.csv"
outsheet using "tableA2a.csv", replace comma

***********************************************************************


************************    APPENDIX TABLE 2 - Panel B: Balance for students present only in baseline and follow-up 1  ***********************

* Data
use "school_intervention_panel_final.dta", clear

foreach wave in bl fu1 fu2 {
	foreach survey in test aluno {
		egen i_`wave'_`survey' = max(`wave'_`survey'), by(id_geral)
		replace i_`wave'_`survey' = 0 if i_`wave'_`survey' == .
	}
}

keep if round==1
keep if i_fu1_test==1 | i_fu1_aluno==1 
drop if i_fu2_test==1 | i_fu2_aluno==1
drop if i_bl_test==0 & i_bl_aluno==0
keep if treatment ~= .	// We will report summ stats on the observations we can test the balance of randomization and treatment effects

sort cd_escola id_geral
egen tag_school_test = tag(cd_escola) if bl_test == 1
egen tag_school_aluno = tag(cd_escola) if bl_aluno == 1

* Outcomes
local test female dumm_rp_08_bl dumm_rp_09_bl dumm_rp_24_bl dumm_rp_14_bl dumm_rp_23_bl vl_proficiencia_bl  
local aluno dumm_rp_49_bl dumm_rp_50_bl dumm_rp_64A_bl dumm_negotiates_bl autonomia_final2_bl poupar_final2_bl
local xvars `test' `aluno' 

* Matrix for output
local z: word count `xvars' 

matrix T = J(`z', 7, . ) 
matrix rownames T = `xvars' 
matrix colnames T = ns n Control sdC Treatment sdT pv

* Estimation
foreach var in `test' { 
	global `var'label: var label `var' 
	
	reg `var' treatment, r clu(cd_escola) 
		g sample_`var' = (e(sample)==1) 
		local pr = 2*ttail(e(df_r),abs(_b[treatment]/_se[treatment])) 
		mat T[rownumb(T, "`var'"), colnumb(T,"pv")] = `pr' 

	sum `var' if sample_`var' == 1, d
	mat T[rownumb(T, "`var'"), colnumb(T,"n")] = `r(N)' 
	
	sum `var' if sample_`var' == 1 & treatment == 1 
		mat T[rownumb(T, "`var'"), colnumb(T,"sdT")] = `r(sd)' 
		mat T[rownumb(T, "`var'"), colnumb(T,"Treatment")] = `r(mean)' 
	sum `var' if sample_`var' == 1 & treatment == 0 
		mat T[rownumb(T, "`var'"), colnumb(T,"sdC")] = `r(sd)' 
		mat T[rownumb(T, "`var'"), colnumb(T,"Control")] = `r(mean)'

	egen max_`var' = max(`var'), by(cd_escola)
	count if max_`var' ~= . & tag_school_test == 1 
	mat T[rownumb(T, "`var'"), colnumb(T,"ns")] = `r(N)' 
} 
foreach var in `aluno' { 
	global `var'label: var label `var' 
	
	reg `var' treatment, r clu(cd_escola) 
		g sample_`var' = (e(sample)==1) 
		local pr = 2*ttail(e(df_r),abs(_b[treatment]/_se[treatment])) 
		mat T[rownumb(T, "`var'"), colnumb(T,"pv")] = `pr' 

	sum `var' if sample_`var' == 1, d
	mat T[rownumb(T, "`var'"), colnumb(T,"n")] = `r(N)' 
	
	sum `var' if sample_`var' == 1 & treatment == 1 
		mat T[rownumb(T, "`var'"), colnumb(T,"sdT")] = `r(sd)' 
		mat T[rownumb(T, "`var'"), colnumb(T,"Treatment")] = `r(mean)' 
	sum `var' if sample_`var' == 1 & treatment == 0 
		mat T[rownumb(T, "`var'"), colnumb(T,"sdC")] = `r(sd)' 
		mat T[rownumb(T, "`var'"), colnumb(T,"Control")] = `r(mean)'

	egen max_`var' = max(`var'), by(cd_escola)
	count if max_`var' ~= . & tag_school_aluno == 1 
	mat T[rownumb(T, "`var'"), colnumb(T,"ns")] = `r(N)' 
} 

* Formating Matrix
matrix list T

clear 
svmat T 
rename T1 ns
rename T2 n 
rename T3 Control
rename T4 sdC
rename T5 Treatment
rename T6 sdT
rename T7 pv

gen var_name = "" 
gen var = ""
order var_name 

local i = 1 
foreach var in `xvars' { 
	replace var_name = "$`var'label" in `i' 
	replace var = "`var'" in `i' 
	local i = `i' + 1 
} 

label var var_name "Variable" 
label var ns "N (School)" 
label var n "N (Students)" 
label var Control "Control" 
label var sdC "SD" 
label var Treatment "Treatment" 
label var sdT "SD" 
label var pv "p-value" 

foreach var of varlist n ns { 
	replace `var' = round(`var', 1) 
	format `var' %9.0f 
} 

foreach var of varlist Treatment Control sdT sdC { 
	replace `var' = round(`var', 0.01) 
	format `var' %9.2f 
} 

foreach var of varlist pv { 
	replace `var' = round(`var', 0.001) 
	format `var' %9.3f 
} 

gen stars = "" 
replace stars = "*" if pv <= 0.10 
replace stars = "**" if pv <= 0.05 
replace stars = "***" if pv <= 0.01 

foreach x in female dumm_rp_08_bl dumm_rp_09_bl dumm_rp_24_bl dumm_rp_14_bl dumm_rp_23_bl dumm_rp_49_bl dumm_rp_50_bl dumm_rp_64A_bl dumm_negotiates_bl {
	foreach y in sdC sdT {
		replace `y' = . if var == "`x'" 
	}
}
drop var

* Output
capture erase "tableA2b.csv"
outsheet using "tableA2b.csv", replace comma

***********************************************************************


***********      APPENDIX TABLE 3: QUANTILE REGRESSION FOR FINANCIAL PROFICIENCY with BOOTSTRAPPED SEs
*1,000 bootstrap replications to obtain SEs (this will take a while in stata)

* Data
use "school_intervention_panel_final.dta", clear

* Outcomes
local outcomes vl_proficiencia_fup

* Estimation
est drop _all
local i = 1
foreach var of varlist `outcomes' { 

	sqreg `var' treatment if round==0, quantiles (0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1) reps (1000)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	local ++ i 
	
	sqreg `var' treatment if round==1, quantiles (0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1) reps (1000)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	local ++ i 
} 

* Output
capture erase "tableA3.csv"
#delimit ;
esttab table1_* using "tableA3.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N controlmean controlsd, fmt(%9.3f %9.0g %9.3f %9.3f) labels("R-squared" "N" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group")) ;
#delimit cr


***********************************************************************


**************************    APPENDIX TABLE 4:  Student Participation in Household Finance   ******************************

* Data
use "school_intervention_panel_final.dta", clear

* Outcomes
local outcomes dumm_rp_33p_fup dumm_rp_34p_fup

* Estimation: Panel A
est drop _all
local i = 1
foreach var of varlist `outcomes' { 

	reg `var' treatment if round==0, r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
	
	reg `var' treatment if round==1, r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
} 

* Output: Panel A
capture erase "tableA4_panelA.csv"
#delimit ;
esttab table1_* using "tableA4_panelA.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N N_clust controlmean controlsd f_test, fmt(%9.3f %9.0g %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Number of Clusters" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr

* Estimation: Panel B
est drop _all
local i = 1
foreach var of varlist `outcomes' { 
	
	bys pair_all: egen flag_`var'0=mean(treatment) if `var'~=. & round==0
	gen pair_`var'0=pair_all
	replace pair_`var'0=0 if flag_`var'0==0 | flag_`var'0==1
	
	areg `var' treatment if round==0, a(pair_`var'0) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
	
	bys pair_all: egen flag_`var'1=mean(treatment) if `var'~=. & round==1
	gen pair_`var'1=pair_all
	replace pair_`var'1=0 if flag_`var'1==0 | flag_`var'1==1
	
	areg `var' treatment if round==1,  a(pair_`var'1)r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
} 

* Output: Panel B
capture erase "tableA4_panelB.csv"
#delimit ;
esttab table1_* using "tableA4_panelB.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N N_clust controlmean controlsd f_test, fmt(%9.3f %9.0g %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Number of Clusters" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr

* Estimation: Panel C
est drop _all
local i = 1
foreach var of varlist `outcomes' { 

	local bl "" 
	local bl = subinstr("`var'","_fup","_bl",.) 
	gen miss_`bl' = 0 
	replace miss_`bl' = 1 if `bl' == . 
	replace `bl' = 0 if `bl' == . 
	
	areg `var' treatment `bl' miss_`bl' female_coded miss_f_coded if round==0, a(pair_`var'0) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
	
	areg `var' treatment `bl' miss_`bl' female_coded miss_f_coded if round==1, a(pair_`var'1) r clu(cd_escola)
	summ `var' if treatment == 0 & e(sample) == 1 
		estadd scalar controlmean = r(mean)
		estadd scalar controlsd = r(sd)
	est sto table1_`i'
	reg treatment female_coded dumm_rp_08p_fup p_employee_fup p_selfempl_fup if e(sample) == 1, r clu(cd_escola) 
	test female_coded=dumm_rp_08p_fup=p_employee_fup=p_selfempl_fup=0
		estadd scalar f_test = r(p) : table1_`i'
	local ++ i 
} 

* Output: Panel C
capture erase "tableA4_panelC.csv"
#delimit ;
esttab table1_* using "tableA4_panelC.csv", replace modelwidth(16) varwidth(30) depvar legend label 
	keep(treatment) cells(b(star fmt(%9.3f)) se(par)) star(* 0.10 ** 0.05 *** 0.01)
	stats(r2 N N_clust controlmean controlsd f_test, fmt(%9.3f %9.0g %9.0g %9.3f %9.3f %9.3f) labels("R-squared" "N" "Number of Clusters" "Dependent Variable Mean in Control Group" "Dependent Variable SD in Control Group" "F-test p-value")) ;
#delimit cr

***********************************************************************
