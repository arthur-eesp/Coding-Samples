### Question 4 - Part 2: evolution plots ###

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
dt <- readRDS('Data/dt_full_period2.RDS')

# Define function that gets default ggplot colors for later
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# Get ggplot default colors
colors <- gg_color_hue(2)

### Plot evolution of outcome variables for treatment and control across time

outcomes <- c('work_week', 'log_hrs_week', 'work_year', 'log_hrs_year', 
              'log_wage_year')

out_names <- list('Any work hrs. last week', 'log(work hrs. last week)',
                  'Any usual work hrs. last year',
                  'log(usual work hrs. last year)', 'log(wage last year)')
names(out_names) <- outcomes

## Group 1: mothers of 1 year olds

# Create one plot for each outcome and store in a list
plots <- list()
for (var in outcomes){
  # Select mean of outcome for each group and combine all into one data.table
  dt_tmp <- rbind(
    dt[STATEFIP != 6 & age1 == 1,
       .(mean = mean(get(var), na.rm = TRUE), group = 'Control'),
       by = YEAR],
    dt[STATEFIP == 6 & age1 == 1, 
       .(mean = mean(get(var), na.rm = TRUE), group = 'Creatment'),
       by = YEAR]
  )
  
  # Plot evolution of the outcome for this outcome 
  # And keep legend for only one of them (to look better when combining)
  ifelse(var != outcomes[4], {
    plots[[var]] <- ggplot(dt_tmp, aes(x = YEAR, y = mean, colour = group)) + 
      geom_line() +
      geom_vline(xintercept = 2005, colour = colors[2], linetype = 'dashed') +
      xlab('Year') +
      ylab(out_names[[var]]) +
      guides(colour = 'none')
  }, {
    plots[[var]] <- ggplot(dt_tmp, aes(x = YEAR, y = mean, colour = group)) + 
      geom_line() +
      geom_vline(xintercept = 2005, colour = colors[2], linetype = 'dashed') +
      xlab('Year') +
      ylab(out_names[[var]]) +
      theme(legend.title=element_blank())
  })
}

# Create and save a plot combining those five plots
ggsave('Output/q4-evol-plot-g1.pdf',
       plot = (plots[[1]] + plots[[2]] + plots[[3]]) / (plots[[4]] + plots[[5]]),
       width = 24, height = 16, units = 'cm')


## Group 2: mothers of 2 year olds

# Create one plot for each outcome and store in a list
plots <- list()
for (var in outcomes){
  # Select mean of outcome for each group and combine all into one data.table
  dt_tmp <- rbind(
    dt[STATEFIP != 6 & age2 == 1,
       .(mean = mean(get(var), na.rm = TRUE), group = 'Control'),
       by = YEAR],
    dt[STATEFIP == 6 & age2 == 1, 
       .(mean = mean(get(var), na.rm = TRUE), group = 'Treatment'),
       by = YEAR]
  )
  
  # Plot evolution of the outcome for this outcome 
  # And keep legend for only one of them (to look better when combining)
  ifelse(var != outcomes[4], {
    plots[[var]] <- ggplot(dt_tmp, aes(x = YEAR, y = mean, colour = group)) + 
      geom_line() +
      geom_vline(xintercept = 2006, colour = colors[2], linetype = 'dashed') +
      xlab('Year') +
      ylab(out_names[[var]]) +
      guides(colour = 'none')
  }, {
    plots[[var]] <- ggplot(dt_tmp, aes(x = YEAR, y = mean, colour = group)) + 
      geom_line() +
      geom_vline(xintercept = 2006, colour = colors[2], linetype = 'dashed') +
      xlab('Year') +
      ylab(out_names[[var]]) +
      theme(legend.title=element_blank())
  })
}

# Create and save a plot combining those five plots
ggsave('Output/q4-evol-plot-g2.pdf',
       plot = (plots[[1]] + plots[[2]] + plots[[3]]) / (plots[[4]] + plots[[5]]),
       width = 24, height = 16, units = 'cm')

## Group 3: mothers of 3 year olds

# Create one plot for each outcome and store in a list
plots <- list()
for (var in outcomes){
  # Select mean of outcome for each group and combine all into one data.table
  dt_tmp <- rbind(
    dt[STATEFIP != 6 & age3 == 1,
       .(mean = mean(get(var), na.rm = TRUE), group = 'Control'),
       by = YEAR],
    dt[STATEFIP == 6 & age3 == 1, 
       .(mean = mean(get(var), na.rm = TRUE), group = 'Treatment'),
       by = YEAR]
  )
  
  # Plot evolution of the outcome for this outcome 
  # And keep legend for only one of them (to look better when combining)
  ifelse(var != outcomes[4], {
    plots[[var]] <- ggplot(dt_tmp, aes(x = YEAR, y = mean, colour = group)) + 
      geom_line() +
      geom_vline(xintercept = 2007, colour = colors[2], linetype = 'dashed') +
      xlab('Year') +
      ylab(out_names[[var]]) +
      guides(colour = 'none')
  }, {
    plots[[var]] <- ggplot(dt_tmp, aes(x = YEAR, y = mean, colour = group)) + 
      geom_line() +
      geom_vline(xintercept = 2007, colour = colors[2], linetype = 'dashed') +
      xlab('Year') +
      ylab(out_names[[var]]) +
      theme(legend.title=element_blank())
  })
}

# Create and save a plot combining those five plots
ggsave('Output/q4-evol-plot-g3.pdf',
       plot = (plots[[1]] + plots[[2]] + plots[[3]]) / (plots[[4]] + plots[[5]]),
       width = 24, height = 16, units = 'cm')


## Extra: testing functional form

### Plot evolution of outcome variables for treatment and control across time

outcomes <- c('AHRSWORKT', 'UHRSWORKLY', 'INCWAGE')

out_names <- list('Work hrs. last week', 'Usual work hrs. last year', 
                  'Wage last year')
names(out_names) <- outcomes

# Create one plot for each outcome and store in a list
plots1 <- list()
for (var in outcomes){
  # Select mean of outcome for each group and combine all into one data.table
  dt_tmp <- rbind(
    dt[STATEFIP != 6 & age1 == 1,
       .(mean = mean(get(var), na.rm = TRUE), group = 'Control 1'),
       by = YEAR],
    dt[STATEFIP == 6 & age1 == 1, 
       .(mean = mean(get(var), na.rm = TRUE), group = 'Treatment 1'),
       by = YEAR]
  )
  
  # Plot evolution of the outcome for this outcome 
  # And keep legend for only one of them (to look better when combining)
  ifelse(var != outcomes[3], {
    plots1[[var]] <- ggplot(dt_tmp, aes(x = YEAR, y = mean, colour = group)) + 
      geom_line() +
      geom_vline(xintercept = 2005, colour = colors[2], linetype = 'dashed') +
      xlab('Year') +
      ylab(out_names[[var]]) +
      guides(colour = 'none')
  }, {
    plots1[[var]] <- ggplot(dt_tmp, aes(x = YEAR, y = mean, colour = group)) + 
      geom_line() +
      geom_vline(xintercept = 2005, colour = colors[2], linetype = 'dashed') +
      xlab('Year') +
      ylab(out_names[[var]]) +
      theme(legend.title=element_blank())
  })
}

# Create one plot for each outcome and store in a list
plots2 <- list()
for (var in outcomes){
  # Select mean of outcome for each group and combine all into one data.table
  dt_tmp <- rbind(
    dt[STATEFIP != 6 & age2 == 1,
       .(mean = mean(get(var), na.rm = TRUE), group = 'Control 2'),
       by = YEAR],
    dt[STATEFIP == 6 & age2 == 1, 
       .(mean = mean(get(var), na.rm = TRUE), group = 'Treatment 2'),
       by = YEAR]
  )
  
  # Plot evolution of the outcome for this outcome 
  # And keep legend for only one of them (to look better when combining)
  ifelse(var != outcomes[3], {
    plots2[[var]] <- ggplot(dt_tmp, aes(x = YEAR, y = mean, colour = group)) + 
      geom_line() +
      geom_vline(xintercept = 2006, colour = colors[2], linetype = 'dashed') +
      xlab('Year') +
      ylab(out_names[[var]]) +
      guides(colour = 'none')
  }, {
    plots2[[var]] <- ggplot(dt_tmp, aes(x = YEAR, y = mean, colour = group)) + 
      geom_line() +
      geom_vline(xintercept = 2006, colour = colors[2], linetype = 'dashed') +
      xlab('Year') +
      ylab(out_names[[var]]) +
      theme(legend.title=element_blank())
  })
}

# Create one plot for each outcome and store in a list
plots3 <- list()
for (var in outcomes){
  # Select mean of outcome for each group and combine all into one data.table
  dt_tmp <- rbind(
    dt[STATEFIP != 6 & age3 == 1,
       .(mean = mean(get(var), na.rm = TRUE), group = 'Control 3'),
       by = YEAR],
    dt[STATEFIP == 6 & age3 == 1, 
       .(mean = mean(get(var), na.rm = TRUE), group = 'Treatment 3'),
       by = YEAR]
  )
  
  # Plot evolution of the outcome for this outcome 
  # And keep legend for only one of them (to look better when combining)
  ifelse(var != outcomes[3], {
    plots3[[var]] <- ggplot(dt_tmp, aes(x = YEAR, y = mean, colour = group)) + 
      geom_line() +
      geom_vline(xintercept = 2007, colour = colors[2], linetype = 'dashed') +
      xlab('Year') +
      ylab(out_names[[var]]) +
      guides(colour = 'none')
  }, {
    plots3[[var]] <- ggplot(dt_tmp, aes(x = YEAR, y = mean, colour = group)) + 
      geom_line() +
      geom_vline(xintercept = 2007, colour = colors[2], linetype = 'dashed') +
      xlab('Year') +
      ylab(out_names[[var]]) +
      theme(legend.title=element_blank())
  })
}

# Create and save a plot combining those five plots
ggsave('Output/q4-evol-plot-func.pdf',
       plot = (plots1[[1]] + plots1[[2]] + plots1[[3]]) / 
         (plots2[[1]] + plots2[[2]] + plots2[[3]]) / 
         (plots3[[1]] + plots3[[2]] + plots3[[3]]),
       width = 24, height = 16, units = 'cm')