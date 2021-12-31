### Question 1 - part 3: replicating table 7 ###

# Clean environment
rm(list = ls())

# Load library
library(data.table)
library(fixest)

# Change some defaults for convenience
setFixest_nthreads(0.75)

setFixest_etable(
  signifCode = c("***"=0.01, "**"=0.05, "*"=0.10),
  fitstat = NA,
  drop.section = c('fixef'),
  drop = 'Intercept',
  digits = 'r4'
)

# Read data
dt <- readRDS('Data/dt_full_period1.RDS')

# Keep only same years used in the paper and set key (to order by YEAR)
dt <- dt[YEAR >= 1999 & YEAR <= 2010]

# Get pre-PFL treatment group fraction in previous year employment
ITT_scale <- readRDS('Data/ITT_scale.RDS')

# Create list to store first-stage regressions (to run placebo later)
t7_fs_list <- list()

### Replicate estimates for treatment group 1

# Run first-stage regression
fml <- as.formula(
  'c(work_week, log_hrs_week, work_year, log_hrs_year, log_wage_year) ~ 
  as.factor(YEAR):age1 + age_group + race_group + age0 + 
  educ_group | YEAR'
)

model <- feols(fml,
               data = dt[treat1 == 1 & YEAR != 2005],
               se = 'hetero',
               lean = TRUE)

# Get number of observations
num_obs <- sapply(model, nobs)

# Get coefficients (as a matrix, and transpose it to join with data.table later)
num_cols <- ncol(coef(model))
coefs <- t(coef(model)[, seq(num_cols-10, num_cols)])
colnames(coefs) <- c('work_week', 'log_hrs_week', 'work_year', 
                     'log_hrs_year', 'log_wage_year')

# Create data.table collapsing into 11 survey year cells excluding year 2005
# (summing ASEC weights) then combine with matrix of coefficients
dt_tmp <- dt[group1_TOT == 1 & YEAR != 2005, c(lapply(.SD, mean), weight = sum(ASECWT)),
             by = YEAR, .SDcols = 'post']
dt_tmp <- cbind(dt_tmp, coefs)

# Save this table on the list of first-stage coefficients
t7_fs_list[['t1']] <- dt_tmp

# Run second-stage regression
model_t1 <- feols(
  c(work_week, log_hrs_week, work_year, log_hrs_year, log_wage_year) ~ post,
  data = dt_tmp,
  weights = ~weight,
  lean = TRUE
)

# Now get the implied TOT coefficients implied by ITT estimates using the 
# ITT scaling factor
t1_ITT_coefs <- coef(model_t1)[,2] / ITT_scale

# Create and save table
etable(model_t1,
       dict = c('post' = 'Estimated PFL effect'), 
       extralines = list('Implied TOT from ITT estimate' = t1_ITT_coefs,
                         'Number of observations' = num_obs),
       file = 'Output/tab7-1.tex',
       replace = TRUE,
       title = 'Replication of table 7 - treatment group 1.',
       label = 'tab7-1'
)


### Treatment group 2

# Run first-stage regression
fml <- as.formula(
  'c(work_week, log_hrs_week, work_year, log_hrs_year, log_wage_year) ~ 
  as.factor(YEAR):age2 + age_group + race_group + age0 + age1 + 
  educ_group | YEAR'
)

model <- feols(fml,
               data = dt[treat2 == 1 & YEAR != 2006],
               se = 'hetero',
               lean = TRUE)

# Get number of observations
num_obs <- sapply(model, nobs)

# Get coefficients (as a matrix, and transpose it to join with data.table later)
num_cols <- ncol(coef(model))
coefs <- t(coef(model)[, seq(num_cols-10, num_cols)])
colnames(coefs) <- c('work_week', 'log_hrs_week', 'work_year', 
                     'log_hrs_year', 'log_wage_year')

# Create new post dummy
dt[, post2 := ifelse(YEAR >= 2007, 1, 0)]

# Create data.table collapsing into 11 survey year cells excluding year 2006
# (summing ASEC weights) then combine with matrix of coefficients
dt_tmp <- dt[group1_TOT == 1 & YEAR != 2006, c(lapply(.SD, mean), weight = sum(ASECWT)),
             by = YEAR, .SDcols = 'post2']
dt_tmp <- cbind(dt_tmp, coefs)

# Save this table on the list of first-stage coefficients
t7_fs_list[['t2']] <- dt_tmp

# Run second-stage regression
model_t2 <- feols(
  c(work_week, log_hrs_week, work_year, log_hrs_year, log_wage_year) ~ post2,
  data = dt_tmp,
  weights = ~weight,
  lean = TRUE
)

# Now get the implied TOT coefficients implied by ITT estimates using the 
# ITT scaling factor
t2_ITT_coefs <- coef(model_t2)[,2] / ITT_scale

# Create and save table
etable(model_t2,
       dict = c('post2' = 'Estimated PFL effect'), 
       extralines = list('Implied TOT from ITT estimate' = t2_ITT_coefs,
                         'Number of observations' = num_obs),
       file = 'Output/tab7-2.tex',
       replace = TRUE,
       title = 'Replication of table 7 - treatment group 2.',
       label = 'tab7-2'
)


### Treatment group 3

# Run first-stage regression
fml <- as.formula(
  'c(work_week, log_hrs_week, work_year, log_hrs_year, log_wage_year) ~ 
  as.factor(YEAR):age3 + age_group + race_group + age0 + age1 + age2 +
  educ_group | YEAR'
)

model <- feols(fml,
               data = dt[treat3 == 1 & YEAR != 2007],
               se = 'hetero',
               lean = TRUE)

# Get number of observations
num_obs <- sapply(model, nobs)

# Get coefficients (as a matrix, and transpose it to join with data.table later)
num_cols <- ncol(coef(model))
coefs <- t(coef(model)[, seq(num_cols-10, num_cols)])
colnames(coefs) <- c('work_week', 'log_hrs_week', 'work_year', 
                     'log_hrs_year', 'log_wage_year')

# Create new post dummy
dt[, post3 := ifelse(YEAR >= 2008, 1, 0)]

# Create data.table collapsing into 11 survey year cells excluding year 2006
# (summing ASEC weights) then combine with matrix of coefficients
dt_tmp <- dt[group1_TOT == 1 & YEAR != 2007, c(lapply(.SD, mean), weight = sum(ASECWT)),
             by = YEAR, .SDcols = 'post3']
dt_tmp <- cbind(dt_tmp, coefs)

# Save this table on the list of first-stage coefficients
t7_fs_list[['t3']] <- dt_tmp

# Run second-stage regression
model_t3 <- feols(
  c(work_week, log_hrs_week, work_year, log_hrs_year, log_wage_year) ~ post3,
  data = dt_tmp,
  weights = ~weight,
  lean = TRUE
)

# Now get the implied TOT coefficients implied by ITT estimates using the 
# ITT scaling factor
t3_ITT_coefs <- coef(model_t3)[,2] / ITT_scale

# Create and save table
etable(model_t3,
       dict = c('post3' = 'Estimated PFL effect'), 
       extralines = list('Implied TOT from ITT estimate' = t3_ITT_coefs,
                         'Number of observations' = num_obs),
       file = 'Output/tab7-3.tex',
       replace = TRUE,
       title = 'Replication of table 7 - treatment group 3.',
       label = 'tab7-3'
)


### Save list of first-stage coefficient tables
saveRDS(t7_fs_list, 'Data/t7_fs_list.RDS')


### Extra treatment group 1 including more covariates

# Run first-stage regression
fml <- as.formula(
  'c(work_week, log_hrs_week, work_year, log_hrs_year, log_wage_year) ~ 
  as.factor(YEAR):age1 + age_group + race_group + age0 + 
  marst_group + educ_group | YEAR'
)

model <- feols(fml,
               data = dt[treat1 == 1 & YEAR != 2005],
               se = 'hetero',
               lean = TRUE)

# Get number of observations
num_obs <- sapply(model, nobs)

# Get coefficients (as a matrix, and transpose it to join with data.table later)
num_cols <- ncol(coef(model))
coefs <- t(coef(model)[, seq(num_cols-10, num_cols)])
colnames(coefs) <- c('work_week', 'log_hrs_week', 'work_year', 
                     'log_hrs_year', 'log_wage_year')

# Create data.table collapsing into 11 survey year cells excluding year 2005
# (summing ASEC weights) then combine with matrix of coefficients
dt_tmp <- dt[group1_TOT == 1 & YEAR != 2005, c(lapply(.SD, mean), weight = sum(ASECWT)),
             by = YEAR, .SDcols = 'post']
dt_tmp <- cbind(dt_tmp, coefs)

# Run second-stage regression
model_t4 <- feols(
  c(work_week, log_hrs_week, work_year, log_hrs_year, log_wage_year) ~ post,
  data = dt_tmp,
  weights = ~weight,
  lean = TRUE
)

# Now get the implied TOT coefficients implied by ITT estimates using the 
# ITT scaling factor
t4_ITT_coefs <- coef(model_t4)[,2] / ITT_scale

# Create and save table
etable(model_t4,
       dict = c('post' = 'Estimated PFL effect'), 
       extralines = list('Implied TOT from ITT estimate' = t4_ITT_coefs,
                         'Number of observations' = num_obs),
       file = 'Output/tab7-4.tex',
       replace = TRUE,
       title = 'Treatment group 1 in table 7 controlling for marital status.',
       label = 'tab7-4'
)


### Extra treatment group 2 including more covariates

# Run first-stage regression
fml <- as.formula(
  'c(work_week, log_hrs_week, work_year, log_hrs_year, log_wage_year) ~ 
  as.factor(YEAR):age2 + age_group + race_group + age0 + age1 + 
  marst_group + educ_group | YEAR'
)

model <- feols(fml,
               data = dt[treat2 == 1 & YEAR != 2006],
               se = 'hetero',
               lean = TRUE)

# Get number of observations
num_obs <- sapply(model, nobs)

# Get coefficients (as a matrix, and transpose it to join with data.table later)
num_cols <- ncol(coef(model))
coefs <- t(coef(model)[, seq(num_cols-10, num_cols)])
colnames(coefs) <- c('work_week', 'log_hrs_week', 'work_year', 
                     'log_hrs_year', 'log_wage_year')

# Create data.table collapsing into 11 survey year cells excluding year 2006
# (summing ASEC weights) then combine with matrix of coefficients
dt_tmp <- dt[group1_TOT == 1 & YEAR != 2006, c(lapply(.SD, mean), weight = sum(ASECWT)),
             by = YEAR, .SDcols = 'post2']
dt_tmp <- cbind(dt_tmp, coefs)

# Run second-stage regression
model_t5 <- feols(
  c(work_week, log_hrs_week, work_year, log_hrs_year, log_wage_year) ~ post2,
  data = dt_tmp,
  weights = ~weight,
  lean = TRUE
)

# Now get the implied TOT coefficients implied by ITT estimates using the 
# ITT scaling factor
t5_ITT_coefs <- coef(model_t5)[,2] / ITT_scale

# Create and save table
etable(model_t5,
       dict = c('post2' = 'Estimated PFL effect'), 
       extralines = list('Implied TOT from ITT estimate' = t5_ITT_coefs,
                         'Number of observations' = num_obs),
       file = 'Output/tab7-5.tex',
       replace = TRUE,
       title = 'Treatment group 2 in table 7 controlling for marital status.',
       label = 'tab7-5'
)


### Extra treatment group 3 including more covariates

# Run first-stage regression
fml <- as.formula(
  'c(work_week, log_hrs_week, work_year, log_hrs_year, log_wage_year) ~ 
  as.factor(YEAR):age3 + age_group + race_group + age0 + age1 + age2 +
  marst_group + educ_group | YEAR'
)

model <- feols(fml,
               data = dt[treat3 == 1 & YEAR != 2007],
               se = 'hetero',
               lean = TRUE)

# Get number of observations
num_obs <- sapply(model, nobs)

# Get coefficients (as a matrix, and transpose it to join with data.table later)
num_cols <- ncol(coef(model))
coefs <- t(coef(model)[, seq(num_cols-10, num_cols)])
colnames(coefs) <- c('work_week', 'log_hrs_week', 'work_year', 
                     'log_hrs_year', 'log_wage_year')

# Create data.table collapsing into 11 survey year cells excluding year 2006
# (summing ASEC weights) then combine with matrix of coefficients
dt_tmp <- dt[group1_TOT == 1 & YEAR != 2007, c(lapply(.SD, mean), weight = sum(ASECWT)),
             by = YEAR, .SDcols = 'post3']
dt_tmp <- cbind(dt_tmp, coefs)

# Run second-stage regression
model_t6 <- feols(
  c(work_week, log_hrs_week, work_year, log_hrs_year, log_wage_year) ~ post3,
  data = dt_tmp,
  weights = ~weight,
  lean = TRUE
)

# Now get the implied TOT coefficients implied by ITT estimates using the 
# ITT scaling factor
t6_ITT_coefs <- coef(model_t6)[,2] / ITT_scale

# Create and save table
etable(model_t6,
       dict = c('post3' = 'Estimated PFL effect'), 
       extralines = list('Implied TOT from ITT estimate' = t6_ITT_coefs,
                         'Number of observations' = num_obs),
       file = 'Output/tab7-6.tex',
       replace = TRUE,
       title = 'Treatment group 3 in table 7 controlling for marital status.',
       label = 'tab7-6'
)