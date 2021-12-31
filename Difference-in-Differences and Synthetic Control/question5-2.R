### Question 5 - Part 3: sensitivity to exclusion of controls ###

# Clean environment
rm(list = ls())

# devtools::install_github("ebenmichael/augsynth")

# Load library
library(data.table)
library(haven)
library(augsynth)
library(kableExtra)
library(ggplot2)
library(patchwork)

# Set percentage of threads for data.table to use
setDTthreads(percent = 75)

# Read data
dt <- readRDS('Data/dt_full_period2.RDS')

dt <- dt[!(STATEFIP %in% c(45, 46, 55))]

# For each group of mothers, estimate the effect on labor market outcomes
outcomes <- c('work_week', 'log_hrs_week', 'work_year', 'log_hrs_year',
              'log_wage_year')

outcome_names <- c('Any work hrs. last week', 'log(work hrs. last week)', 
                   'Any usual work hrs. last year', 
                   'log(usual work hrs. last year)', 'log(wage last year)')

max_weights <- c()

### SC estimates for group 1
g1_plots <- list()
g1_mat <- matrix(nrow = 2, ncol = length(outcomes))

for (i in seq_along(outcomes)){
  var <- outcomes[i]
  dt_sc <- dt[!is.na(get(var)) & age1 == 1 & YEAR != 2005]
  
  # Aggregate by state-year level
  dt_sc <- dt_sc[, .(outcome = weighted.mean(get(var), w = ASECWT)),
                 by = c('STATEFIP', 'YEAR')]
  
  # Define treatment dummy
  dt_sc[, treatment := ifelse(STATEFIP == 6 & YEAR > 2005, 1, 0)]
  
  # Run SC estimation
  sc_est <- augsynth(outcome ~ treatment,
                     unit = STATEFIP,
                     time = YEAR, 
                     data = dt_sc,
                     progfunc = 'none',
                     scm = TRUE,
                     fixedeff = TRUE)
  
  sum <- summary(sc_est)
  
  g1_mat[1, i] <- sum$average_att$Estimate
  g1_mat[2, i] <- sum$average_att$p_val
  
  g1_plots[[var]] <- (plot(sc_est) + 
                        ggtitle(paste('Group 1:', outcome_names[i]))
  )
  
  # Get state with maximum weight
  max_weights <- c(
    max_weights,
    row.names(sc_est$weights)[which(sc_est$weights == max(sc_est$weights))]
  )
}

### SC estimates for group 1
g2_plots <- list()
g2_mat <- matrix(nrow = 2, ncol = length(outcomes))

for (i in seq_along(outcomes)){
  var <- outcomes[i]
  dt_sc <- dt[!is.na(get(var)) & age2 == 1 & YEAR != 2006]
  
  # Aggregate by state-year level
  dt_sc <- dt_sc[, .(outcome = weighted.mean(get(var), w = ASECWT)),
                 by = c('STATEFIP', 'YEAR')]
  
  # Define treatment dummy
  dt_sc[, treatment := ifelse(STATEFIP == 6 & YEAR > 2006, 1, 0)]
  
  # Run SC estimation
  sc_est <- augsynth(outcome ~ treatment,
                     unit = STATEFIP,
                     time = YEAR, 
                     data = dt_sc,
                     progfunc = 'none',
                     scm = TRUE,
                     fixedeff = TRUE)
  
  sum <- summary(sc_est)
  
  g2_mat[1, i] <- sum$average_att$Estimate
  g2_mat[2, i] <- sum$average_att$p_val
  
  g2_plots[[var]] <- (plot(sc_est) + 
                        ggtitle(paste('Group 2:', outcome_names[i]))
  )
  
  # Get state with maximum weight
  max_weights <- c(
    max_weights,
    row.names(sc_est$weights)[which(sc_est$weights == max(sc_est$weights))]
  )
}


### SC estimates for group 3
g3_plots <- list()
g3_mat <- matrix(nrow = 2, ncol = length(outcomes))

for (i in seq_along(outcomes)){
  var <- outcomes[i]
  dt_sc <- dt[!is.na(get(var)) & age3 == 1 & YEAR != 2007]
  
  # Aggregate by state-year level
  dt_sc <- dt_sc[, .(outcome = weighted.mean(get(var), w = ASECWT)),
                 by = c('STATEFIP', 'YEAR')]
  
  # Define treatment dummy
  dt_sc[, treatment := ifelse(STATEFIP == 6 & YEAR > 2007, 1, 0)]
  
  # Run SC estimation
  sc_est <- augsynth(outcome ~ treatment,
                     unit = STATEFIP,
                     time = YEAR, 
                     data = dt_sc,
                     progfunc = 'none',
                     scm = TRUE,
                     fixedeff = TRUE)
  
  sum <- summary(sc_est)
  
  g3_mat[1, i] <- sum$average_att$Estimate
  g3_mat[2, i] <- sum$average_att$p_val
  
  g3_plots[[var]] <- (plot(sc_est) + 
                        ggtitle(paste('Group 3:', outcome_names[i]))
  )
  # Get state with maximum weight
  max_weights <- c(
    max_weights,
    row.names(sc_est$weights)[which(sc_est$weights == max(sc_est$weights))]
  )
}

# Create table
group <- rep(c('SC estimate', 'p-value'), 3)

tmp <- data.table(group, rbind(g1_mat, g2_mat, g3_mat))

kbl(data.table(group, rbind(g1_mat, g2_mat, g3_mat)),
    format = 'latex',
    booktabs = TRUE,
    digits = 3,
    caption = "SC estimates obtained after excluding North Dakota, South Dakota
               and Wisconsin from control pool.",
    label = 'excl_SC',
    col.names = c('', 'Any work hrs. last week', 'log(work hrs. last week)',
                  'Any usual work hrs. last year',
                  'log(usual work hrs. last year)', 'log(wage last year)')) %>%
  pack_rows('Group 1: mothers with a child aged 1 years old', 1, 2) %>%
  pack_rows('Group 2: mothers with a child aged 2 years old', 3, 4) %>%
  pack_rows('Group 3: mothers with a child aged 3 years old', 5, 6) %>%
  landscape() %>%
  save_kable('Output/excl_SC.tex')

# Create and save a plot combining those plots
ggsave('Output/q5-sc-g1-f.pdf',
       plot = (g1_plots[[1]] + g1_plots[[2]]) / (g1_plots[[3]] + g1_plots[[4]]),
       width = 24, height = 16, units = 'cm')

ggsave('Output/q5-sc-g2-f.pdf',
       plot = (g2_plots[[1]] + g2_plots[[2]]) / (g2_plots[[3]] + g2_plots[[4]]),
       width = 24, height = 16, units = 'cm')

ggsave('Output/q5-sc-g3-f.pdf',
       plot = (g3_plots[[1]] + g3_plots[[2]]) / (g3_plots[[3]] + g3_plots[[4]]),
       width = 24, height = 16, units = 'cm')

ggsave('Output/q5-sc-wageyear-f.pdf',
       plot = g1_plots[[5]] / g2_plots[[5]] / g3_plots[[5]],
       width = 12, height = 21, units = 'cm')

# Save most common max weights
saveRDS(max_weights, file = 'Data/max_weights3.RDS')
