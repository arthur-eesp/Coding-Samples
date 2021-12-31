### Question 2 - Part 1: evolution plots ###

# Clean environment
rm(list = ls())

# Load library
library(data.table)
library(haven)
library(ggplot2)
library(patchwork)

# Set percentage of threads for data.table to use
setDTthreads(percent = 75)

# Read data
dt <- readRDS('Data/dt_full_period1.RDS')

# Keep only same years used in the paper and set key (to order by YEAR)
dt <- dt[YEAR >= 1999 & YEAR <= 2010]

# Define function that gets default ggplot colors for later
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

### Plot evolution of outcome variables for treatment and control across time

### First for TOT outcomes of table 3
outcomes <- c('maternity_leave', 'family_leave', 'other_leave', 'any_leave')

out_names <- list('Maternity leave', 'Family leave', 'Other leave', 'Any leave')
names(out_names) <- outcomes

# Create one plot for each outcome and store in a list
plots <- list()
for (var in outcomes){
  # Select mean of outcome for each group and combine all into one data.table
  dt_tmp <- rbind(
    dt[group1_TOT == 1 & treat == 1, 
       .(mean = mean(get(var)), group = 'Treatment'),
       by = YEAR],
    dt[group1_TOT == 1 & treat == 0, 
       .(mean = mean(get(var)), group = 'Control 1'),
       by = YEAR],
    dt[group2_TOT == 1 & treat == 0, 
       .(mean = mean(get(var)), group = 'Control 2'),
       by = YEAR],
    dt[group3_TOT == 1 & treat == 0, 
       .(mean = mean(get(var)), group = 'Control 3'),
       by = YEAR],
    dt[group4_TOT == 1 & treat == 0, 
       .(mean = mean(get(var)), group = 'Control 4'),
       by = YEAR]
  )
  
  # Plot evolution of the outcome for this outcome
  plots[[var]] <- ggplot(dt_tmp, aes(x = YEAR, y = mean, colour = group)) + 
    geom_line() +
    geom_vline(xintercept = 2004.5, linetype = 'dashed') +
    xlab('Year') +
    ylab(out_names[[var]]) +
    theme(legend.title=element_blank())
}

# Create and save a plot combining those four plots
ggsave('Output/q3-evol-plot-t3-TOT.pdf',
       plot = (plots[[1]] + plots[[2]]) / (plots[[3]] + plots[[4]]),
       width = 24, height = 16, units = 'cm')


### Now for ITT outcomes of table 3

# Create one plot for each outcome and store in a list
plots <- list()
for (var in outcomes){
  # Select mean of outcome for each group and combine all into one data.table
  dt_tmp <- rbind(
    dt[group1_ITT == 1 & treat == 1, 
       .(mean = mean(get(var)), group = 'Treatment'),
       by = YEAR],
    dt[group1_ITT == 1 & treat == 0, 
       .(mean = mean(get(var)), group = 'Control 1'),
       by = YEAR],
    dt[group2_ITT == 1 & treat == 0, 
       .(mean = mean(get(var)), group = 'Control 2'),
       by = YEAR],
    dt[group3_ITT == 1 & treat == 0, 
       .(mean = mean(get(var)), group = 'Control 3'),
       by = YEAR],
    dt[group4_ITT == 1 & treat == 0, 
       .(mean = mean(get(var)), group = 'Control 4'),
       by = YEAR]
  )
  
  # Plot evolution of the outcome for this outcome
  plots[[var]] <- ggplot(dt_tmp, aes(x = YEAR, y = mean, colour = group)) + 
    geom_line() +
    geom_vline(xintercept = 2004.5, linetype = 'dashed') +
    xlab('Year') +
    ylab(out_names[[var]]) +
    theme(legend.title=element_blank())
}

# Create and save a plot combining those four plots
ggsave('Output/q3-evol-plot-t3-ITT.pdf',
       plot = (plots[[1]] + plots[[2]]) / (plots[[3]] + plots[[4]]),
       width = 24, height = 16, units = 'cm')


### Finally, for outcomes of table 7
outcomes <- c('work_week', 'log_hrs_week', 'work_year', 'log_hrs_year', 
              'log_wage_year')

out_names <- list('Any work hrs. last week', 'log(work hrs. last week)',
                  'Any usual work hrs. last year',
                  'log(usual work hrs. last year)', 'log(wage last year)')
names(out_names) <- outcomes

# Get ggplot default colors
colors <- gg_color_hue(4)

# Create one plot for each outcome and store in a list
plots <- list()
for (var in outcomes){
  # Select mean of outcome for each group and combine all into one data.table
  dt_tmp <- rbind(
    dt[treat1 == 1 & age1 == 0, 
       .(mean = mean(get(var), na.rm = TRUE), group = 'Control'),
       by = YEAR],
    dt[treat1 == 1 & age1 == 1, 
       .(mean = mean(get(var), na.rm = TRUE), group = 'Treatment 1'),
       by = YEAR],
    dt[treat2 == 1 & age2 == 1, 
       .(mean = mean(get(var), na.rm = TRUE), group = 'Treatment 2'),
       by = YEAR],
    dt[treat3 == 1 & age3 == 1, 
       .(mean = mean(get(var), na.rm = TRUE), group = 'Treatment 3'),
       by = YEAR]
  )
  
  # Plot evolution of the outcome for this outcome 
  # And keep legend for only one of them (to look better when combining)
  ifelse(var != outcomes[4], {
         plots[[var]] <- ggplot(dt_tmp, aes(x = YEAR, y = mean, colour = group)) + 
           geom_line() +
           geom_vline(xintercept = 2005, colour = colors[2], linetype = 'dashed') +
           geom_vline(xintercept = 2006, colour = colors[3], linetype = 'dashed') +
           geom_vline(xintercept = 2007, colour = colors[4], linetype = 'dashed') +
           xlab('Year') +
           ylab(out_names[[var]]) +
           guides(colour = 'none')
         }, {
         plots[[var]] <- ggplot(dt_tmp, aes(x = YEAR, y = mean, colour = group)) + 
           geom_line() +
           geom_vline(xintercept = 2005, colour = colors[2], linetype = 'dashed') +
           geom_vline(xintercept = 2006, colour = colors[3], linetype = 'dashed') +
           geom_vline(xintercept = 2007, colour = colors[4], linetype = 'dashed') +
           xlab('Year') +
           ylab(out_names[[var]]) +
           theme(legend.title=element_blank())
         })
}

# Create and save a plot combining those five plots
ggsave('Output/q3-evol-plot-t7.pdf',
       plot = (plots[[1]] + plots[[2]] + plots[[3]]) / (plots[[4]] + plots[[5]]),
       width = 24, height = 16, units = 'cm')