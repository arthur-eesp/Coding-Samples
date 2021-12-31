### Question 4 - Part 4: placebo outcomes ###

# Clean environment
rm(list = ls())

# Load library
library(data.table)
library(haven)
library(fixest)
library(kableExtra)

# Set percentage of threads for data.table to use
setDTthreads(percent = 75)

# Change some defaults for convenience
setFixest_nthreads(0.75)

setFixest_etable(
  signifCode = c("***"=0.01, "**"=0.05, "*"=0.10),
  fitstat = ~n,
  drop.section = c('fixef'),
  drop = 'Intercept',
  digits = 'r4'
)

# Read data
dt <- readRDS('Data/dt_full_period2.RDS')

# Source function to implement Ferman & Pinto (2019) from other R script
source('FP_estimation.R')

# Select vector of placebo outcomes
outcomes <- c('non_white_share', 'pov_rate', 'med_inc', 'unemployment')

# Group 1: mother's of 1 year olds
g1_list <- list()

for (var in outcomes){
  g1_list[[var]] <- FP_estimation(
    outcome = var,
    treatdum = 'treat',
    postdum = 'post', 
    timevar = 'YEAR',
    groupvar = 'STATEFIP',
    data = dt[!is.na(get(var)) & age1 == 1 & YEAR != 2005],
    controls = c('age_group', 'age0', 'educ_group'), 
    weightvar = 'ASECWT',
    return_lean = TRUE
  )
}

# Group 2: mother's of 2 year olds
g2_list <- list()

for (var in outcomes){
  g2_list[[var]] <- FP_estimation(
    outcome = var,
    treatdum = 'treat',
    postdum = 'post', 
    timevar = 'YEAR',
    groupvar = 'STATEFIP',
    data = dt[!is.na(get(var)) & age2 == 1 & YEAR != 2006],
    controls = c('age_group', 'race_group', 'age0', 'age1', 'educ_group'), 
    weightvar = 'ASECWT',
    return_lean = TRUE
  )
}

# Group 3: mother's of 3 year olds
g3_list <- list()

for (var in outcomes){
  g3_list[[var]] <- FP_estimation(
    outcome = var,
    treatdum = 'treat',
    postdum = 'post', 
    timevar = 'YEAR',
    groupvar = 'STATEFIP',
    data = dt[!is.na(get(var)) & age3 == 1 & YEAR != 2007],
    controls = c('age_group', 'race_group', 'age0', 'age1', 'age2', 'educ_group'), 
    weightvar = 'ASECWT',
    return_lean = TRUE
  )
}

# Construct list with coefficient and inference data

group <- rep(c('DiD estimate', '95% CI', 'p-value', ' '), 3)

out <- list()

for (var in outcomes){
  out[[var]] <- list(
    round(g1_list[[var]]$coefficient, 3), 
    paste0('[', round(g1_list[[var]]$ci.low, 3), '; ',
           round(g1_list[[var]]$ci.high, 3), ']'),
    round(g1_list[[var]]$pvalue, 3),
    paste0('N = ', g1_list[[var]]$model$nobs),
    round(g2_list[[var]]$coefficient, 3), 
    paste0('[', round(g2_list[[var]]$ci.low, 3), '; ',
           round(g2_list[[var]]$ci.high, 3), ']'),
    round(g2_list[[var]]$pvalue, 3),
    paste0('N = ', g2_list[[var]]$model$nobs),
    round(g3_list[[var]]$coefficient, 3), 
    paste0('[', round(g3_list[[var]]$ci.low, 3), '; ',
           round(g3_list[[var]]$ci.high, 3), ']'),
    round(g3_list[[var]]$pvalue, 3),
    paste0('N = ', g3_list[[var]]$model$nobs)
  )
}

# Create and export table to a .tex file
kbl(data.table(group, out[[1]], out[[2]], out[[3]], out[[4]]), 
    format = 'latex',
    booktabs = TRUE,
    digits = 3,
    caption = 'DiD regressions on placebo outcomes: non-white share of population,
               poverty rate, median income and unemployment.',
    label = 'placebo-outcomes',
    col.names = c('', 'Share of non-whites', 'Poverty rate', 'Median income',
                  'Unemployment')) %>%
  pack_rows('Group 1', 1, 4) %>%
  pack_rows('Group 2', 5, 8) %>%
  pack_rows('Group 3', 9, 12) %>%
  save_kable('Output/placebo-outcomes.tex')