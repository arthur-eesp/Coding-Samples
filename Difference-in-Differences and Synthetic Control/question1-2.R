### Question 1 - part 2: replicating table 3 ###

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
ITT_scale <- mean(dt[SEX == 2 & post==0 & treat==1, WORKLY == 2])
saveRDS(ITT_scale, file = 'Data/ITT_scale.RDS')
cat(round(ITT_scale, 3), file = 'Output/ITT_scale.tex')

# Create list to store first-stage regressions (to run placebo later)
t3_fs_list <- list()

### Replicate estimates for comparison group 1

# Run first-stage regression
fml <- as.formula('c(maternity_leave, family_leave, other_leave, any_leave) ~ 
                  as.factor(YEAR):treat + age_group + race_group + 
                  marst_group + educ_group + foreign_born | YEAR')

model <- feols(fml,
               data = dt[group1_TOT == 1],
               se = 'hetero',
               lean = TRUE)

# Get coefficients (as a matrix, and transpose it to join with data.table later)
coefs <- t(coef(model)[,18:29])
colnames(coefs) <- c('maternity_leave', 'family_leave', 'other_leave', 'any_leave')

# Create data.table collapsing into 12 survey year cells (summing ASEC weights)
# then combine with matrix of coefficients
dt_tmp <- dt[group1_TOT == 1, c(lapply(.SD, mean), weight = sum(ASECWT)),
             by = YEAR, .SDcols = 'post']
dt_tmp <- cbind(dt_tmp, coefs)

# Save this table on the list of first-stage coefficients
t3_fs_list[['g1_TOT']] <- dt_tmp

# Run second-stage regression
model_g1_TOT <- feols(c(maternity_leave, family_leave, other_leave, any_leave) ~ post,
                      data = dt_tmp,
                      weights = ~weight,
                      lean = TRUE)

# Repeat the procedure to find the ITT for comparison group 1
model <- feols(fml,
               data = dt[group1_ITT == 1],
               se = 'hetero',
               lean = TRUE)

coefs <- t(coef(model)[,18:29])
colnames(coefs) <- c('maternity_leave', 'family_leave', 'other_leave', 'any_leave')

dt_tmp <- dt[group1_ITT == 1, c(lapply(.SD, mean), weight = sum(ASECWT)),
             by = YEAR, .SDcols = 'post']
dt_tmp <- cbind(dt_tmp, coefs)

t3_fs_list[['g1_ITT']] <- dt_tmp

model_g1_ITT <- feols(c(maternity_leave, family_leave, other_leave, any_leave) ~ post,
                      data = dt_tmp,
                      weights = ~weight,
                      lean = TRUE)

# Now get the implied TOT coefficients implied by ITT estimates using the 
# ITT scaling factor
g1_ITT_coefs <- coef(model_g1_ITT)[,2] / ITT_scale

# Create and save table
etable(model_g1_TOT,
       dict = c('post' = 'Estimated PFL effect'), 
       extralines = list('Implied TOT from ITT estimate' = g1_ITT_coefs),
       file = 'Output/tab3-1.tex',
       replace = TRUE,
       title = 'Replication of table 3 - comparison group 1',
       label = 'tab3-1'
       )

# Repeat that for the other comparison groups

### Comparison group 2
model <- feols(fml,
               data = dt[group2_TOT == 1],
               se = 'hetero',
               lean = TRUE)

coefs <- t(coef(model)[,18:29])
colnames(coefs) <- c('maternity_leave', 'family_leave', 'other_leave', 'any_leave')

dt_tmp <- dt[group2_TOT == 1, c(lapply(.SD, mean), weight = sum(ASECWT)),
             by = YEAR, .SDcols = 'post']
dt_tmp <- cbind(dt_tmp, coefs)

t3_fs_list[['g2_TOT']] <- dt_tmp

model_g2_TOT <- feols(c(maternity_leave, family_leave, other_leave, any_leave) ~ post,
                      data = dt_tmp,
                      weights = ~weight,
                      lean = TRUE)

model <- feols(fml,
               data = dt[group2_ITT == 1],
               se = 'hetero',
               lean = TRUE)

coefs <- t(coef(model)[,18:29])
colnames(coefs) <- c('maternity_leave', 'family_leave', 'other_leave', 'any_leave')

dt_tmp <- dt[group2_ITT == 1, c(lapply(.SD, mean), weight = sum(ASECWT)),
             by = YEAR, .SDcols = 'post']
dt_tmp <- cbind(dt_tmp, coefs)

t3_fs_list[['g2_ITT']] <- dt_tmp

model_g2_ITT <- feols(c(maternity_leave, family_leave, other_leave, any_leave) ~ post,
                      data = dt_tmp,
                      weights = ~weight,
                      lean = TRUE)

g2_ITT_coefs <- coef(model_g2_ITT)[,2] / ITT_scale

etable(model_g2_TOT,
       dict = c('post' = 'Estimated PFL effect'), 
       extralines = list('Implied TOT from ITT estimate' = g2_ITT_coefs),
       file = 'Output/tab3-2.tex',
       replace = TRUE,
       title = 'Replication of table 3 - comparison group 2',
       label = 'tab3-2'
)

### Comparison group 3

# Include state fixed effects and control for state-year unemployment rate
fml <- as.formula('c(maternity_leave, family_leave, other_leave, any_leave) ~ 
                  as.factor(YEAR):treat + age_group + race_group + unemployment +
                  marst_group + educ_group + foreign_born | YEAR + STATEFIP')

model <- feols(fml,
               data = dt[group3_TOT == 1],
               se = 'hetero',
               lean = TRUE)

coefs <- t(coef(model)[, 19:29])
colnames(coefs) <- c('maternity_leave', 'family_leave', 'other_leave', 'any_leave')

# Remove 2010 because of collinearity
dt_tmp <- dt[group3_TOT == 1 & YEAR != 2010, c(lapply(.SD, mean), weight = sum(ASECWT)),
             by = YEAR, .SDcols = 'post']
dt_tmp <- cbind(dt_tmp, coefs)

t3_fs_list[['g3_TOT']] <- dt_tmp

model_g3_TOT <- feols(c(maternity_leave, family_leave, other_leave, any_leave) ~ post,
                      data = dt_tmp,
                      weights = ~weight,
                      lean = TRUE)

model <- feols(fml,
               data = dt[group3_ITT == 1],
               se = 'hetero',
               lean = TRUE)

coefs <- t(coef(model)[, 19:29])
colnames(coefs) <- c('maternity_leave', 'family_leave', 'other_leave', 'any_leave')

# Remove 2010 because of collinearity
dt_tmp <- dt[group3_TOT == 1 & YEAR != 2010, c(lapply(.SD, mean), weight = sum(ASECWT)),
             by = YEAR, .SDcols = 'post']
dt_tmp <- cbind(dt_tmp, coefs)

t3_fs_list[['g3_ITT']] <- dt_tmp

model_g3_ITT <- feols(c(maternity_leave, family_leave, other_leave, any_leave) ~ post,
                      data = dt_tmp,
                      weights = ~weight,
                      lean = TRUE)

g3_ITT_coefs <- coef(model_g3_ITT)[,2] / ITT_scale

etable(model_g3_TOT,
       dict = c('post' = 'Estimated PFL effect'), 
       extralines = list('Implied TOT from ITT estimate' = g3_ITT_coefs),
       file = 'Output/tab3-3.tex',
       replace = TRUE,
       title = 'Replication of table 3 - comparison group 3',
       label = 'tab3-3'
)



### Comparison group 4

model <- feols(fml,
               data = dt[group4_TOT == 1],
               se = 'hetero',
               lean = TRUE)

coefs <- t(coef(model)[, 19:29])
colnames(coefs) <- c('maternity_leave', 'family_leave', 'other_leave', 'any_leave')

# Remove 2010 because of collinearity
dt_tmp <- dt[group3_TOT == 1 & YEAR != 2010, c(lapply(.SD, mean), weight = sum(ASECWT)),
             by = YEAR, .SDcols = 'post']
dt_tmp <- cbind(dt_tmp, coefs)

t3_fs_list[['g4_TOT']] <- dt_tmp

model_g4_TOT <- feols(c(maternity_leave, family_leave, other_leave, any_leave) ~ post,
                      data = dt_tmp,
                      weights = ~weight,
                      lean = TRUE)

model <- feols(fml,
               data = dt[group4_ITT == 1],
               se = 'hetero',
               lean = TRUE)

coefs <- t(coef(model)[, 19:29])
colnames(coefs) <- c('maternity_leave', 'family_leave', 'other_leave', 'any_leave')

# Remove 2010 because of collinearity
dt_tmp <- dt[group3_TOT == 1 & YEAR != 2010, c(lapply(.SD, mean), weight = sum(ASECWT)),
             by = YEAR, .SDcols = 'post']
dt_tmp <- cbind(dt_tmp, coefs)

t3_fs_list[['g4_ITT']] <- dt_tmp

model_g4_ITT <- feols(c(maternity_leave, family_leave, other_leave, any_leave) ~ post,
                      data = dt_tmp,
                      weights = ~weight,
                      lean = TRUE)

g4_ITT_coefs <- coef(model_g4_ITT)[,2] / ITT_scale

etable(model_g4_TOT,
       dict = c('post' = 'Estimated PFL effect'), 
       extralines = list('Implied TOT from ITT estimate' = g4_ITT_coefs),
       file = 'Output/tab3-4.tex',
       replace = TRUE,
       title = 'Replication of table 3 - comparison group 4',
       label = 'tab3-4'
)


### Save list of first-stage coefficient tables
saveRDS(t3_fs_list, 'Data/t3_fs_list.RDS')


### Extra comparison group 3, without controlling for state-year unemployment
fml <- as.formula('c(maternity_leave, family_leave, other_leave, any_leave) ~ 
                  as.factor(YEAR):treat + age_group + race_group + 
                  marst_group + educ_group + foreign_born | YEAR + STATEFIP')

model <- feols(fml,
               data = dt[group3_TOT == 1],
               se = 'hetero',
               lean = TRUE)

coefs <- t(coef(model)[, 18:28])
colnames(coefs) <- c('maternity_leave', 'family_leave', 'other_leave', 'any_leave')

# Remove 2010 because of collinearity
dt_tmp <- dt[group3_TOT == 1 & YEAR != 2010, c(lapply(.SD, mean), weight = sum(ASECWT)),
             by = YEAR, .SDcols = 'post']
dt_tmp <- cbind(dt_tmp, coefs)

model_g5_TOT <- feols(c(maternity_leave, family_leave, other_leave, any_leave) ~ post,
                      data = dt_tmp,
                      weights = ~weight,
                      lean = TRUE)

model <- feols(fml,
               data = dt[group3_ITT == 1],
               se = 'hetero',
               lean = TRUE)

coefs <- t(coef(model)[, 18:28])
colnames(coefs) <- c('maternity_leave', 'family_leave', 'other_leave', 'any_leave')

# Remove 2010 because of collinearity
dt_tmp <- dt[group3_TOT == 1 & YEAR != 2010, c(lapply(.SD, mean), weight = sum(ASECWT)),
             by = YEAR, .SDcols = 'post']
dt_tmp <- cbind(dt_tmp, coefs)

model_g5_ITT <- feols(c(maternity_leave, family_leave, other_leave, any_leave) ~ post,
                      data = dt_tmp,
                      weights = ~weight,
                      lean = TRUE)

g5_ITT_coefs <- coef(model_g5_ITT)[,2] / ITT_scale

etable(model_g5_TOT,
       dict = c('post' = 'Estimated PFL effect'), 
       extralines = list('Implied TOT from ITT estimate' = g5_ITT_coefs),
       file = 'Output/tab3-5.tex',
       replace = TRUE,
       title = 'Comparison group 4 in table 3 but without controlling 
                for state-year unemployment.',
       label = 'tab3-5'
)


### Extra comparison group 4, without controlling for state-year unemployment

model <- feols(fml,
               data = dt[group4_TOT == 1],
               se = 'hetero',
               lean = TRUE)

coefs <- t(coef(model)[, 18:28])
colnames(coefs) <- c('maternity_leave', 'family_leave', 'other_leave', 'any_leave')

# Remove 2010 because of collinearity
dt_tmp <- dt[group3_TOT == 1 & YEAR != 2010, c(lapply(.SD, mean), weight = sum(ASECWT)),
             by = YEAR, .SDcols = 'post']
dt_tmp <- cbind(dt_tmp, coefs)

model_g6_TOT <- feols(c(maternity_leave, family_leave, other_leave, any_leave) ~ post,
                      data = dt_tmp,
                      weights = ~weight,
                      lean = TRUE)

model <- feols(fml,
               data = dt[group4_ITT == 1],
               se = 'hetero',
               lean = TRUE)

coefs <- t(coef(model)[, 18:28])
colnames(coefs) <- c('maternity_leave', 'family_leave', 'other_leave', 'any_leave')

# Remove 2010 because of collinearity
dt_tmp <- dt[group3_TOT == 1 & YEAR != 2010, c(lapply(.SD, mean), weight = sum(ASECWT)),
             by = YEAR, .SDcols = 'post']
dt_tmp <- cbind(dt_tmp, coefs)

model_g6_ITT <- feols(c(maternity_leave, family_leave, other_leave, any_leave) ~ post,
                      data = dt_tmp,
                      weights = ~weight,
                      lean = TRUE)

g6_ITT_coefs <- coef(model_g6_ITT)[,2] / ITT_scale

etable(model_g6_TOT,
       dict = c('post' = 'Estimated PFL effect'), 
       extralines = list('Implied TOT from ITT estimate' = g6_ITT_coefs),
       file = 'Output/tab3-6.tex',
       replace = TRUE,
       title = 'Comparison group 4 in table 3 but without controlling 
                for state-year unemployment.',
       label = 'tab3-6'
)
