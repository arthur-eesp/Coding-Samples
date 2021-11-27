*** Bruhn et al "The Impact of High School Financial Education: Evidence from a Large-Scale Evaluation in Brazil" forthcoming in the American Economic Journal: Applied Economics
*** This file describes the do and dta files that allow readers to replicate the analysis in Bruhn et al.

***Data files

school_intervention_panel_final.dta
This file contains the underlying variables used in the analysis, as well as dummy variables generated based on the underlying data. 
The questionnaires for the survey data are in Portuguese, but all variables and their values in the data are labeled in English.
Variables with "_bl" at the end are from the baseline survey. Variables with "_fu" at the end are from the follow-up survey.
The data is stacked for the first and second follow-up surveys, with variable round==0 denoting the first follow-up and round==1 the second follow-up.
Variables with a "p" after the number in the variables name, e.g. "rp_09p_bl" come from the parent questionnaire.

school_admin_data_final.dta
This file contains grade level passing, failing, and dropout rates from administrative school data (used in Table 4).


***Do files

creating_final.do:
This do-file documents how dummy variables for the analysis were generated based on the underlying data.

analysis_final.do:
This do-file generates the tables shown in the text and appendix.