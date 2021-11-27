*** Bruhn et al "The Impact of High School Financial Education: Evidence from a Large-Scale Evaluation in Brazil" forthcoming in the American Economic Journal: Applied Economics
*** This do-file documents how dummy variables for the analysis were generated based on the underlying data

gen miss_f_coded=1 if female_coded==9
replace miss_f_coded=0 if female_coded==0 | female_coded==1
label var miss_f_coded "Gender could not be coded based on name"

g female = 1 if rp_01_bl == 2 & rp_01_bl ~= .
replace female = 0 if rp_01_bl == 1 & rp_01_bl ~= .
label var female "Student is female"

g dumm_rp_08_bl = 0 if rp_08_bl <= 5 & rp_08_bl ~= .
replace dumm_rp_08_bl = 1 if dumm_rp_08_bl == . & rp_08_bl ~= .
label var dumm_rp_08_bl "Education of student's mother: At least some secondary"

g dumm_rp_09_bl = 0 if rp_09_bl <= 5 & rp_09_bl ~= .
replace dumm_rp_09_bl = 1 if dumm_rp_09_bl == . & rp_09_bl ~= .
label var dumm_rp_09_bl "Education of student's father: At least some secondary"

g dumm_rp_14_bl = (rp_14_bl==1) if rp_14_bl != .
label var dumm_rp_14_bl "Student's family receives Bolsa Familia cash transfer"

g dumm_rp_23_bl = 0 if rp_23_bl == 1 & rp_23_bl ~= .
replace dumm_rp_23_bl = 1 if rp_23_bl >= 2 & rp_23_bl ~= .
label var dumm_rp_23_bl "Student has computer with internet at home"

g dumm_rp_24_bl = 1 if rp_24_bl ~= 4 & rp_24_bl ~= .
replace dumm_rp_24_bl = 0 if dumm_rp_24_bl == . & rp_24_bl ~= .
label var dumm_rp_24_bl "Student has failed at least one school year"

foreach survey in bl fup {

	gen dumm_rp_49_`survey' = 1 if rp_49_`survey' ~= 5 & rp_49_`survey' ~= .
	replace dumm_rp_49_`survey' = 0 if rp_49_`survey' == 5
	label var dumm_rp_49_`survey' "Student is not working at the moment"
	
	gen business_`survey'=0 if rp_49_`survey' ~= .
	replace business_`survey'=1 if rp_49_`survey'==1 | rp_49_`survey'==2
	label var business_`survey' "Q49 Student works in own or family business"
	gen employee_`survey'=0 if rp_49_`survey' ~= .
	replace employee_`survey'=1 if rp_49_`survey'==3 | rp_49_`survey'==4 | rp_49_`survey'==6
	label var employee_`survey' "Q49 Student works as employee or other"
	
	gen dumm_rp_50_`survey' = 1 if rp_50_`survey' ~= 8 & rp_50_`survey' ~= .
	replace dumm_rp_50_`survey' = 0 if rp_50_`survey' == 8
	label var dumm_rp_50_`survey' "Q50 Receives income"
	
	gen dumm_rp_53B_`survey' = (rp_53_`survey' >= 2 & rp_53_`survey' <= 5) if rp_53_`survey' != . 
	label var dumm_rp_53B_`survey' "Q53 Pct of income saved is non-zero" 
	
	gen dumm_rp_55_`survey' = 1 if rp_55_`survey' ~= "F" & rp_55_`survey' ~= ""
	replace dumm_rp_55_`survey' = 0 if rp_55_`survey' == "F"
	cap replace dumm_rp_55_fup = 1 if rp_55_fup2 >= 1 & rp_55_fup2 <= 5 & round == 1
	cap replace dumm_rp_55_fup = 0 if rp_55_fup2 == 6 & round == 1
	label var dumm_rp_55_`survey' "Q55 Has borrowed money (any source)" //Question was multiple answer, but in follow-up2 only one was recorded by mistake
	
	gen dumm_rp_56_`survey' = 1 if rp_56_`survey' == 1  
	replace dumm_rp_56_`survey' = 0 if rp_56_`survey' == 2 
	label var dumm_rp_56_`survey' "Q56 Is behind on payments (unconditional)"
	
	# delimit ;
	*For rp_57 "With whom do you have outstanding loans?", answers are A=banks, B=MFIs, C=Stores, D=Family, E=Friends, F=Other people, G=Nobody;
	gen dumm_rp_57f_`survey' = 1 if rp_57_`survey' ~= "";
	replace dumm_rp_57f_`survey' = 0 if rp_57_`survey' == "C" | rp_57_`survey' == "CD" | rp_57_`survey' == "CDE" | rp_57_`survey' == "CDEF" | rp_57_`survey' == "CDF" | rp_57_`survey' == "CE" | rp_57_`survey' == "CEF" | rp_57_`survey' == "CF" | 
	rp_57_`survey' == "D" | rp_57_`survey' == "DE" | rp_57_`survey' == "DEF" | rp_57_`survey' == "DF" | rp_57_`survey' == "E" | rp_57_`survey' == "EF" | rp_57_`survey' == "F" | rp_57_`survey' == "G"; 
	replace dumm_rp_57f_`survey' = . if rp_57_`survey' == "ABCDEFG" | rp_57_`survey' == "ABCDG" | rp_57_`survey' == "ACG" | rp_57_`survey' == "AG" | rp_57_`survey' == "BDCEFG" | rp_57_`survey' == "BG" | 
	rp_57_`survey' == "CDG" | rp_57_`survey' == "CG" | rp_57_`survey' == "DEG" | rp_57_`survey' == "DG" | rp_57_`survey' == "EG" | rp_57_`survey' == "FG"; 
	cap replace dumm_rp_57f_fup = . if round == 1; //Question was multiple answer, but in follow-up2 only one was recorded by mistake;
	label var dumm_rp_57f_`survey' "Q57 Is behind on payments to bank or FI"; 
	
	gen dumm_rp_57s_`survey' = 1 if rp_57_`survey' ~= "";
	replace dumm_rp_57s_`survey' = 0 if rp_57_`survey' == "A" | rp_57_`survey' == "AB" | rp_57_`survey' == "ABDF" | rp_57_`survey' == "AD" | rp_57_`survey' == "ADE" | rp_57_`survey' == "AE" | rp_57_`survey' == "AF" | 
	rp_57_`survey' == "B" | rp_57_`survey' == "BD" | rp_57_`survey' == "BDE" | rp_57_`survey' == "BE" | rp_57_`survey' == "BF" | rp_57_`survey' == "D" | rp_57_`survey' == "DE" | rp_57_`survey' == "DEF" | rp_57_`survey' == "DF" | 
	rp_57_`survey' == "E" | rp_57_`survey' == "EF" | rp_57_`survey' == "F" | rp_57_`survey' == "G"; 
	replace dumm_rp_57s_`survey' = . if rp_57_`survey' == "ABCDEFG" | rp_57_`survey' == "ABCDG" | rp_57_`survey' == "ACG" | rp_57_`survey' == "AG" | rp_57_`survey' == "BDCEFG" | rp_57_`survey' == "BG" | 
	rp_57_`survey' == "CDG" | rp_57_`survey' == "CG" | rp_57_`survey' == "DEG" | rp_57_`survey' == "DG" | rp_57_`survey' == "EG" | rp_57_`survey' == "FG"; 
	cap replace dumm_rp_57s_fup = . if round == 1; //Question was multiple answer, but in follow-up2 only one was recorded by mistake;
	label var dumm_rp_57s_`survey' "Q57 Is behind on payments to store";
	
	gen dumm_rp_57i_`survey' = 1 if rp_57_`survey' ~= "";
	replace dumm_rp_57i_`survey' = 0 if rp_57_`survey' == "A" | rp_57_`survey' == "AB" | rp_57_`survey' == "ABC" | rp_57_`survey' == "AC" | 
    rp_57_`survey' == "B" | rp_57_`survey' == "BC" | rp_57_`survey' == "C" | rp_57_`survey' == "G"; 
	replace dumm_rp_57i_`survey' = . if rp_57_`survey' == "ABCDEFG" | rp_57_`survey' == "ABCDG" | rp_57_`survey' == "ACG" | rp_57_`survey' == "AG" | rp_57_`survey' == "BDCEFG" | rp_57_`survey' == "BG" | 
	rp_57_`survey' == "CDG" | rp_57_`survey' == "CG" | rp_57_`survey' == "DEG" | rp_57_`survey' == "DG" | rp_57_`survey' == "EG" | rp_57_`survey' == "FG"; 
	cap replace dumm_rp_57i_fup = . if round == 1; //Question was multiple answer, but in follow-up2 only one was recorded by mistake;
	label var dumm_rp_57i_`survey' "Q57 Is behind on payments to family friends or other people";
	
	#delimit cr
	 
	gen dumm_rp_59_`survey' = 1 if rp_59_`survey' == 4 | rp_59_`survey' == 5
	replace dumm_rp_59_`survey' = 0 if rp_59_`survey' == 1 | rp_59_`survey' == 2 | rp_59_`survey' == 3
	label var dumm_rp_59_`survey' "Q59 Says they are a saver"
	
	gen dumm_rp_61_`survey' = (rp_61_`survey' == 1) if rp_61_`survey' ~= . 
	label var dumm_rp_61_`survey' "Q61 Has formal savings"
	
	foreach i in 64 65 { 
		gen dumm_rp_`i'A_`survey' = (rp_`i'_`survey' == 1) if rp_`i'_`survey' != . 
	} 
	label var dumm_rp_64A_`survey' "Q64 I make a list of all monthly expenses" 
	label var dumm_rp_65A_`survey' "Q65 Saves money for future purchases" 
	
	*Note: For questions 88 through 92, if the student has bought the item more than once, answer refers to most recent purchase
	
	forvalues i = 88/92 {
		gen dumm_rp_`i'C_`survey' = (rp_`i'_`survey' == 3) if rp_`i'_`survey' != . 
		gen dumm_rp_`i'D_`survey' = (rp_`i'_`survey' == 4) if rp_`i'_`survey' != . 
		gen dumm_rp_`i'AB_`survey' = (rp_`i'_`survey' == 1 | rp_`i'_`survey' == 2) if rp_`i'_`survey' != . 
	}
	
    gen dumm_rp88__92C_`survey' = 0 if dumm_rp_88C_`survey'==0 & dumm_rp_89C_`survey'==0 & dumm_rp_90C_`survey'==0 & dumm_rp_91C_`survey'==0 & dumm_rp_92C_`survey'==0 
	replace dumm_rp88__92C_`survey' = 1 if dumm_rp_88C_`survey'==1 | dumm_rp_89C_`survey'==1 | dumm_rp_90C_`survey'==1 | dumm_rp_91C_`survey'==1 | dumm_rp_92C_`survey'==1
	
	gen dumm_rp88__92D_`survey' = 0 if dumm_rp_88D_`survey'==0 & dumm_rp_89D_`survey'==0 & dumm_rp_90D_`survey'==0 & dumm_rp_91D_`survey'==0 & dumm_rp_92D_`survey'==0 
	replace dumm_rp88__92D_`survey' = 1 if dumm_rp_88D_`survey'==1 | dumm_rp_89D_`survey'==1 | dumm_rp_90D_`survey'==1 | dumm_rp_91D_`survey'==1 | dumm_rp_92D_`survey'==1
	
	gen dumm_rp88__92AB_`survey' = 0 if dumm_rp_88AB_`survey'==0 & dumm_rp_89AB_`survey'==0 & dumm_rp_90AB_`survey'==0 & dumm_rp_91AB_`survey'==0 & dumm_rp_92AB_`survey'==0 
	replace dumm_rp88__92AB_`survey' = 1 if dumm_rp_88AB_`survey'==1 | dumm_rp_89AB_`survey'==1 | dumm_rp_90AB_`survey'==1 | dumm_rp_91AB_`survey'==1 | dumm_rp_92AB_`survey'==1
	
	label var dumm_rp_88C_`survey' "Q88 I have bought cell phone with credit card" 
	label var dumm_rp_88D_`survey' "Q88 I have bought cell phone on installments" 
	label var dumm_rp_88AB_`survey' "Q88 I have bought a cell phone with cash/debit card" 
	label var dumm_rp_89C_`survey' "Q89 I have bought computer with credit card" 
	label var dumm_rp_89D_`survey' "Q89 I have bought computer on installments" 
	label var dumm_rp_89AB_`survey' "Q89 I have bought a computer with cash/debit card" 
	label var dumm_rp_90C_`survey' "Q90 I have bought an electronic device with credit card" 
	label var dumm_rp_90D_`survey' "Q90 I have bought an electronic device on installments" 
	label var dumm_rp_90AB_`survey' "Q90 I have bought an electronic device with cash/debit card" 
	label var dumm_rp_91C_`survey' "Q91 I have bought shoes with credit card" 
	label var dumm_rp_91D_`survey' "Q91 I have bought shoes on installments" 
	label var dumm_rp_91AB_`survey' "Q91 I have bought shoes with cash/debit card"
	label var dumm_rp_92C_`survey' "Q92 I have bought clothing with credit card" 
	label var dumm_rp_92D_`survey' "Q92 I have bought clothing on installments" 
	label var dumm_rp_92AB_`survey' "Q92 I have bought clothing with cash/debit card"
	label var dumm_rp88__92C_`survey' "Q88 - Q92 I have bought electronics shoes or clothing with credit card"
	label var dumm_rp88__92D_`survey' "Q88 - Q92 I have bought electronics shoes or clothing on installments"
	label var dumm_rp88__92AB_`survey' "Q88 - Q92 I have bought electronics shoes or clothing with cash/debit card"
		
	forvalues i = 93/96 {
		gen dumm_rp_`i'_`survey' = (rp_`i'_`survey' == 1) if rp_`i'_`survey' != . 
	}
	label var dumm_rp_93_`survey' "Q93 I negotiate the price" 
	label var dumm_rp_94_`survey' "Q94 I search price in different stores" 
	label var dumm_rp_95_`survey' "Q95 I negotiate the payment method" 
	label var dumm_rp_96_`survey' "Q96 I search similar models/brands" 
	
	g dumm_negotiates_`survey' = 1 if (dumm_rp_93_`survey'==1 | dumm_rp_95_`survey'==1)
	replace dumm_negotiates_`survey' = 0 if (dumm_rp_93_`survey'==0 & dumm_rp_95_`survey'==0)
	g dumm_search_`survey' = 1 if (dumm_rp_94_`survey'==1 | dumm_rp_96_`survey'==1)
	replace dumm_search_`survey' = 0 if (dumm_rp_94_`survey'==0 & dumm_rp_96_`survey'==0)
	label var dumm_negotiates_`survey' "Q93 Q95: Negotiates prices or payment methods"
	label var dumm_search_`survey' "Q94 Q96: Comparison shops before making purchase"
	
	g dumm_rp_08p_`survey' = 0 if rp_08p_`survey' <= 5 & rp_08p_`survey' ~= .
	replace dumm_rp_08p_`survey' = 1 if dumm_rp_08p_`survey' == . & rp_08p_`survey' ~= .
	label var dumm_rp_08p_`survey' "Q8 Parent has at least some secondary education"
	
	foreach var in employee selfempl other {
	g p_`var'_`survey' = 0 if rp_09p_`survey' ~= .
	}
	replace p_employee_`survey' = 1 if rp_09p_`survey' == 1 | rp_09p_`survey' == 2 | rp_09p_`survey' == 6 
	label var p_employee_`survey' "Q9 Parent is an employee"
	replace p_selfempl_`survey' = 1 if rp_09p_`survey' == 3 | rp_09p_`survey' == 4 | rp_09p_`survey' == 5 
	label var p_selfempl_`survey' "Q9 Parent is self-employeed"
	replace p_other_`survey' = 1 if rp_09p_`survey' == 7 | rp_09p_`survey' == 8 | rp_09p_`survey' == 9 | rp_09p_`survey' == 10 | rp_09p_`survey' == 11 
	label var p_other_`survey' "Q9 Parent's occupation is other (homemaker, retired, unemployed, other)"
	
	foreach i in 14 { 
		gen dumm_rp_`i'p_`survey' = (rp_`i'p_`survey' == 1) if rp_`i'p_`survey' != . 
	} 
	label var dumm_rp_14p_`survey' "Parent: Q14: makes a list of all monthly expenses" 
	
	foreach i in 18 19 21 23 { 
		gen dumm_rp_`i'p_`survey' = (rp_`i'p_`survey' == 1) if rp_`i'p_`survey' != . 
	} 	
	label var dumm_rp_18p_`survey' "Q18 Parent has checking account" 
	label var dumm_rp_19p_`survey' "Q19 Parent has savings account" 
	label var dumm_rp_21p_`survey' "Q21 Parent has debit card" 
	label var dumm_rp_23p_`survey' "Q23 Parent has checks" 
	
	g dumm_formal_saving_`survey' = .
	replace dumm_formal_saving_`survey' = 1 if dumm_rp_18p_`survey' == 1 | dumm_rp_19p_`survey' == 1 | dumm_rp_21p_`survey' == 1 | dumm_rp_23p_`survey' == 1
	replace dumm_formal_saving_`survey' = 0 if dumm_rp_18p_`survey' == 0 & dumm_rp_19p_`survey' == 0 & dumm_rp_21p_`survey' == 0 & dumm_rp_23p_`survey' == 0
	label var dumm_formal_saving_`survey' "Parent has formal savings" 
	
	gen dumm_rp_33p_`survey' = (rp_33p_`survey' == 1) if rp_33p_`survey' != . 
	label var dumm_rp_33p_`survey' "Q33 Student talks to you about finances" 
	gen dumm_rp_34p_`survey' = (rp_34p_`survey' == 1) if rp_34p_`survey' != . 
	label var dumm_rp_34p_`survey' "Q34 Student helps organize HH budget" 
	gen dumm_rp_36p_`survey' = (rp_36p_`survey' == 1) if rp_36p_`survey' != . 
	label var dumm_rp_36p_`survey' "Parent: Q36 Prefers R50K plus 15 percent interest" 
	gen dumm_rp_37p_`survey' = (rp_37p_`survey' == 3) if rp_37p_`survey' != . 
	label var dumm_rp_37p_`survey' "Parent: Q37 Inflation question dummy for correct" 
}

destring rp_20_fup, g(dumm_rp_20C_fup)
replace dumm_rp_20C_fup = 0 if dumm_rp_20C_fup == . & rp_53_fup==1 
replace dumm_rp_20C_fup = . if round == 0		// info is only from FU2 (not asked in FU1)
label var dumm_rp_20C_fup "Student: Q20: Pct of income saved - filled in with zero if missing and rp_53 says nothing"

destring rp_12p_fup, g(dumm_rp_12Cp_fup)
replace dumm_rp_12Cp_fup = 0 if dumm_rp_12Cp_fup == . & rp_13p_fup == 1 
replace dumm_rp_12Cp_fup = . if round == 0		// info is only from FU2 (not asked in FU1)
label var dumm_rp_12Cp_fup "Parent: Q12: Pct income saved - filled in with zero if missing and rp_13 says nothing"

g dumm_rp_41p_fup = 1 if rp_41p_fup == 3 & rp_41p_fup~=.
replace dumm_rp_41p_fup = 0 if (rp_41p_fup == 1 | rp_41p_fup == 2 | rp_41p_fup ==4) & rp_41p_fup~=.
label var dumm_rp_41p_fup "Parent: Q41: budget must have income and expenses"
