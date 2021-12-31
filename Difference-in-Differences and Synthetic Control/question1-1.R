### Question 1 - part 1: preparing the data ###

# Clean environment
rm(list = ls())

# Load library 
library(ipumsr)
library(data.table)

# Set percentage of threads for data.table to use
setDTthreads(percent = 75)

# Read data and save it for faster loading later
# ddi <- read_ipums_ddi("Data/cps_00010.xml")
# dt <- read_ipums_micro(ddi)
# 
# setDT(dt)
# 
# saveRDS(ddi, file = 'Data/ddi.RDS')
# saveRDS(dt, file = 'Data/base.RDS')

# Read saved data
dt <- readRDS('Data/base.RDS')

# Remove people with zero ASEC weights (it's like 30 people)
dt <- dt[ASECWT != 0]

### Creating covariates

# Age groups
age_breaks <- c(15, seq(20, 60, by = 10), 65)
dt[, age_group := cut(AGE, breaks = age_breaks, right = FALSE)]

# Race categories
dt[RACE == 100 & HISPAN == 0, race_group := 'White']
dt[RACE == 200 & HISPAN == 0, race_group := 'Black']
#dt[RACE == 200, race_group := 'Black']
dt[is.na(race_group) & HISPAN > 0 & HISPAN < 901, race_group := 'Hispanic']
dt[is.na(race_group), race_group := 'Other']

dt[, race_group := as.factor(race_group)]

# Marital status
dt[MARST == 1 | MARST == 2, marst_group := 'Married']
dt[MARST == 3, marst_group := 'Separated']
dt[MARST == 4, marst_group := 'Divorced']
dt[MARST == 5, marst_group := 'Widowed']
dt[MARST == 6, marst_group := 'Single']
dt[MARST > 6, marst_group := 'Other']

dt[, marst_group := as.factor(marst_group)]

# Education level
dt[EDUC >= 2 & EDUC <= 72, educ_group := '< high school']
dt[EDUC == 73, educ_group := 'High school']
dt[EDUC == 81, educ_group := 'Some college']
dt[EDUC == 91 | EDUC == 92, educ_group := 'Associate degree']
dt[EDUC >= 111, educ_group := 'College']
dt[is.na(educ_group), educ_group := 'Other']

dt[, educ_group := as.factor(educ_group)]

# Foreign-born
dt[, foreign_born := ifelse(BPL <= 12090, 0, 1)]

# State-year unemployment rate, manually calculated from EMPSTAT and LABFORCE
dt[, unemployment := sum(EMPSTAT %in% c(20, 21, 22)) / sum(LABFORCE==2),
   by = c('STATEFIP', 'YEAR')]


### Creating treatment dummies

# Eventually treated parent
dt[, treat := ifelse(STATEFIP == 6 & YNGCH == 0, 1, 0)]

# Treatment period
dt[, post := ifelse(YEAR >= 2005, 1, 0)]


### Creating outcome variables

# Maternity leave
dt[, maternity_leave := ifelse(WHYABSNT == 9, 1, 0)]

# Family leave
dt[, family_leave := ifelse(WHYABSNT %in% c(9, 5, 7, 8), 1, 0)]

# Other leave
dt[, other_leave := ifelse(WHYABSNT %in% c(5, 7, 8), 1, 0)]

# Any leave
dt[, any_leave := ifelse(WHYABSNT != 0, 1, 0)]

# Any hours worked last week
#dt[, any_work := ifelse(EMPSTAT == 10, 1, 0)] these are equivalent
dt[, work_week := ifelse(AHRSWORKT != 999, 1, 0)]

# Log hours worked last week
dt[AHRSWORKT != 999, log_hrs_week := log(AHRSWORKT)]

# Any usual hours worked last year
#dt[, any_work := ifelse(WORKLY == 2, 1, 0)] these are equivalent
dt[, work_year := ifelse(UHRSWORKLY != 999, 1, 0)]

# Log usual hours worked last year
dt[UHRSWORKLY != 999, log_hrs_year := log(UHRSWORKLY)]

# Log wage income last year
dt[INCWAGE != 99999999 & INCWAGE != 0, log_wage_year := log(INCWAGE)]

# Replace NIU values by NA, to make it easier to run regressions later
dt[AHRSWORKT == 999, AHRSWORKT := NA]
dt[UHRSWORKLY == 999, UHRSWORKLY := NA]
dt[INCWAGE == 99999999, INCWAGE := NA]


### Creating dummies for specifications in table 3

# Treatment group is always mothers with infants in California (aged 15-64).
# For each alternative control group, create two specifications: one for 
# eligible mothers (for TOT estimate), one for all mothers (for ITT estimate)

# Comparison group 1: mothers with youngest child aged 5 to 17 in CA
dt[SEX == 2 & STATEFIP == 6 & ((YNGCH >= 5 & YNGCH <= 17) | YNGCH == 0) & 
     WORKLY==2 & AGE >= 15, group1_TOT := 1]
dt[is.na(group1_TOT), group1_TOT := 0]

dt[SEX == 2 & STATEFIP == 6 & ((YNGCH >= 5 & YNGCH <= 17) | YNGCH == 0) & 
     AGE >= 15, group1_ITT := 1]
dt[is.na(group1_ITT), group1_ITT := 0]

# Comparison group 2: women with no children in CA
dt[SEX == 2 & STATEFIP == 6 & (NCHILD == 0 | YNGCH == 0) & WORKLY==2 & 
     AGE >= 15, group2_TOT := 1]
dt[is.na(group2_TOT), group2_TOT := 0]

dt[SEX == 2 & STATEFIP == 6 & (NCHILD == 0 | YNGCH == 0) & AGE >= 15,
   group2_ITT := 1]
dt[is.na(group2_ITT), group2_ITT := 0]

# Comparison group 3: mothers with infants in FL, NY and TX
dt[SEX == 2 & (STATEFIP %in% c(6, 12, 36, 48)) & YNGCH == 0 & WORKLY==2 & 
     AGE >= 15, group3_TOT := 1]
dt[is.na(group3_TOT), group3_TOT := 0]

dt[SEX == 2 & (STATEFIP %in% c(6, 12, 36, 48)) & YNGCH == 0 & AGE >= 15,
   group3_ITT := 1]
dt[is.na(group3_ITT), group3_ITT := 0]

# Comparison group 4: mothers with infants in all states excluding CA
dt[SEX == 2 & YNGCH == 0 & WORKLY==2 & AGE >= 15,
   group4_TOT := 1]
dt[is.na(group4_TOT), group4_TOT := 0]

dt[SEX == 2 & YNGCH == 0 & AGE >= 15,
   group4_ITT := 1]
dt[is.na(group4_ITT), group4_ITT := 0]


### Creating dummies for specifications in table 7

# Control group is always women (aged 15-64) with youngest child aged 7-17 in CA
# Treatment is women with child 

# To find women with children aged 0, 1, 2 or 3 years old, first create dummies
# to indicate if a child is of one of those ages
dt[, ':=' (
  age0 = ifelse(AGE == 0, 1, 0),
  age1 = ifelse(AGE == 1, 1, 0),
  age2 = ifelse(AGE == 2, 1, 0),
  age3 = ifelse(AGE == 3, 1, 0))
]

# Create new data.table with number of children of each age in each household-year
tmp <- dt[, lapply(.SD, sum), by =c('SERIAL', 'YEAR'),
               .SDcols = c('age0', 'age1', 'age2', 'age3')]

# Remove age dummies
dt[, c('age0', 'age1', 'age2', 'age3') := NULL]

# Merge to include number of children of each age in each household-year for each
# observation in the original data.table
dt <- merge(dt, tmp, by = c("SERIAL","YEAR"))

# Transform those variables into dummies indicating if a parent has an own child
# of each given age (<1, 1, 2, or 3 years old)
dt[, ':=' (
  age0 = ifelse(NCHILD * age0 > 0 & YNGCH == 0, 1, 0),
  age1 = ifelse(NCHILD * age1 > 0 & YNGCH <= 1, 1, 0),
  age2 = ifelse(NCHILD * age2 > 0 & YNGCH <= 2, 1, 0),
  age3 = ifelse(NCHILD * age3 > 0 & YNGCH <= 3, 1, 0))
]


# Treatment group 1: mothers of children aged 1
dt[SEX == 2 & STATEFIP == 6 & (YNGCH>6 & YNGCH<18 | age1 == 1) & WORKLY != 0 &
     AGE >= 15 , treat1 := 1]
dt[is.na(treat1), treat1 := 0]

# Treatment group 2: mothers of children aged 2
dt[SEX == 2 & STATEFIP == 6 & (YNGCH>6 & YNGCH<18 | age2 == 1) & WORKLY != 0 & 
     AGE >= 15, treat2 := 1]
dt[is.na(treat2), treat2 := 0]

# Treatment group 3: mothers of children aged 3
dt[SEX == 2 & STATEFIP == 6 & (YNGCH>6 & YNGCH<18 | age3 == 1) & WORKLY != 0 & 
     AGE >= 15, treat3 := 1]
dt[is.na(treat3), treat3 := 0]

# Set key to YEAR variable (makes things easier when organizing coefficients later)
setkey(dt, YEAR)

# Save prepared data
saveRDS(dt, file = 'Data/dt_full_period1.RDS')
