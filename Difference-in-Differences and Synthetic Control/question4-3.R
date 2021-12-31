### Question 4 - Part 3: creating table of descriptive statistics ###

# Clean environment
rm(list = ls())

# Load library
library(data.table)
library(haven)
library(Hmisc)
library(kableExtra)

# Set percentage of threads for data.table to use
setDTthreads(percent = 75)

# Read data
dt <- readRDS('Data/dt_full_period2.RDS')

# Choose variables
vars <- c('married_rate', 'college_rate', 'non_white_share', 'pov_rate', 'med_inc',
          'non_wage', 'unemployment', 'mnf_jobs', 'sales_jobs', 'prof_jobs')

# Get weighted means
dt_desc <- dt[, lapply(.SD, weighted.mean, w = ASECWT, na.rm=TRUE),
              by = c("treat","post"), .SDcols = vars]
dt_desc <- dt_desc[order(-treat,post)]

# Get weighted std.dev.
dt_sd <- dt[, lapply(.SD, wtd.var, w = ASECWT, na.rm=TRUE),
            by = c("treat","post"), .SDcols = vars]
dt_sd <- dt_sd[, sqrt(.SD), .SDcols = vars, by = c("treat","post")]
dt_sd <- dt_sd[order(-treat,post)]

# Join both data.tables
dt_desc <- rbind(dt_desc, dt_sd)

# Keep only the important variables
dt_desc <- dt_desc[, ..vars]

# Create vector of variable names
var_names <- c('Married share', 'Share of college educated', 'Share of non-whites',
               'Poverty rate', 'Median income', 'Avg. non-income wage',
               'Unemployment', 'Share of manucturing jobs', 
               'Share of sales jobs', 'Share of professional jobs')

# Transpose
dt_desc <- data.table(variable = var_names, transpose(dt_desc))

# Calculate percentage difference
dt_desc[, ':=' (Delta1 = (V2/V1-1)*100, Delta0 = (V4/V3-1)*100)]

# Set order of columns
setcolorder(dt_desc, c('variable', 'V1', 'V5', 'V2', 'V6', 'Delta1', 'V3', 'V7',
                       'V4', 'V8', 'Delta0'))

# Create and export table to a .tex file
kbl(dt_desc, 
    format = 'latex',
    booktabs = TRUE,
    digits = 3,
    valign = 'c',
    caption = 'Descriptive statistics.',
    label = 'desc-stat',
    col.names = c('', 'Mean', 'Std.Dev.', 'Mean', 'Std.Dev.', '% Change', 'Mean', 
                'Std.Dev.', 'Mean', 'Std.Dev.', '% Change')) %>%
  add_header_above(c(' ' = 1, 'Before' = 2, 'After' = 2, ' ' = 1, 'Before' = 2, 
                     'After' = 2, ' ' = 1))  %>%
  add_header_above(c(' ' = 1, 'California' = 5, 'Other states' = 5))  %>%
  landscape() %>%
  save_kable('Output/desc-stat.tex')