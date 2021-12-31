### Question 4 - Part 1: further preparing the data ###

# Clean environment
rm(list = ls())

# Load library
library(data.table)
library(haven)

# Set percentage of threads for data.table to use
setDTthreads(percent = 75)

# Read data
dt <- readRDS('Data/dt_full_period1.RDS')

# Exclude NJ from the analysis because it passed a PFL program in 2008
dt <- dt[STATEFIP != 34]

# Manually construct some aggregate state-year variables

# Share of married (among adults)
dt[, married_rate := sum(marst_group == 'Married' & AGE >= 15) / sum(AGE >= 15), 
   by = c('STATEFIP', 'YEAR')]

# Share of college educated
dt[, college_rate := sum(educ_group == 'College' & AGE >= 15) / sum(AGE >= 15), 
   by = c('STATEFIP', 'YEAR')]

# Share of non-white population
dt[, non_white_share := sum(race_group != 'White') / .N,
   by = c('STATEFIP', 'YEAR')]

# Poverty rate
dt[, pov_rate := sum(POVERTY == 10) / .N, by = c('STATEFIP', 'YEAR')]

# Replace some NIU variables by NA
dt[FTOTVAL == 9999999999, FTOTVAL := NA]
dt[INCTOT == 9999999999, INCTOT := NA]
dt[INCWAGE == 9999999999, INCWAGE := NA]

# Median family income and non-wage income
dt[, med_inc := median(FTOTVAL, na.rm = TRUE), by = c('STATEFIP', 'YEAR')]
dt[, non_wage := weighted.mean(INCTOT - INCWAGE, w = ASECWT, na.rm = TRUE),
   by = c('STATEFIP', 'YEAR')]

# Share of manufacturing, sales, and professional and related services jobs
dt[, mnf_jobs := sum(100 <= IND1990 & IND1990 < 400) / sum(IND1990 != 0), 
   by = c('STATEFIP', 'YEAR')]
dt[, sales_jobs := sum(500 <= IND1990 & IND1990 < 700) / sum(IND1990 != 0), 
   by = c('STATEFIP', 'YEAR')]
dt[, prof_jobs := sum(812 <= IND1990 & IND1990 < 900) / sum(IND1990 != 0), 
   by = c('STATEFIP', 'YEAR')]

# Restrict to women above age 14 (database does not include over 65 already)
dt <- dt[AGE >= 15 & SEX == 2]

# Redefine treatment dummy as mothers in CA  (I'll restrict to samples with 
# mothers of children of a given age later, so no need to include that now)
dt[, treat := ifelse(STATEFIP == 6, 1, 0)]

# Save prepared data.table
saveRDS(dt, file = 'Data/dt_full_period2.RDS')