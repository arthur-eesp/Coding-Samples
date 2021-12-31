### Question 2, item 4 ###

# Clean environment
rm(list = ls())

# Load libraries
library(data.table)
library(rdrobust)
library(ggplot2)

# Load the data
dt_rdd <- readRDS("data/dt_rdd.RDS")
dt_covar <- readRDS("data/dt_covar.RDS")

# Define function that does RD estimation for a given cutoff
get_placebocutoff = function(cutoff){
  # Use only values in the same side of the cutoff
  if(cutoff > 0.5){
    data <- dt_rdd[lagdemvoteshare > 0.5]
    data_covar <- dt_covar[dt_rdd$lagdemvoteshare > 0.5]
  }
  
  if(cutoff < 0.5){
    data <- dt_rdd[lagdemvoteshare < 0.5]
    data_covar <- dt_covar[dt_rdd$lagdemvoteshare < 0.5]
  }
  
  if(cutoff == 0.5){
    data <- dt_rdd
    data_covar <- dt_covar
  }
  
  # Run RD estimation
  out = rdrobust(
    data$demvoteshare,
    data$lagdemvoteshare,
    c = cutoff,
    covs = data_covar,
    bwselect = 'mserd'
  )
  
  # Save results on a data.table
  data.table(
    cutoff = cutoff,
    mse.bandwith = out$bws[1,1],
    coeff= out$coef[1,1],
    low.bound = out$ci[3,1],
    up.bound = out$ci[3,2],
    eff.obs = out$N_h[1] + out$N_h[2]
  )
}

# Create a table with the estimates for different cutoff values
placebo_cutoff = purrr::map_df(seq(.3, .7, by=0.04), get_placebocutoff)

# Construct the plot and save it
ggplot(placebo_cutoff, aes(x = cutoff, y = coeff)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = low.bound, ymax = up.bound), width = 0.01) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  ylab("Estimated RD treatment effect") +
  xlab("Cutoff")

ggsave("tables/rdd-cutoff-plot.pdf", width = 15, height = 10, units = "cm")
