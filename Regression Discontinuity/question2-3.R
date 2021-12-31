### Question 2, item 3 ###

# Clean environment
rm(list = ls())

# Load libraries
library(data.table)
library(rdrobust)
library(ggplot2)

# Load the data
dt_rdd <- readRDS("data/dt_rdd.RDS")
dt_covar <- readRDS("data/dt_covar.RDS")

# Define function that does RD estimation for a given band
get_bandtest = function(band){
  # Run RD estimation
  out <- rdrobust(
    dt_rdd$demvoteshare,
    dt_rdd$lagdemvoteshare,
    c = .5,
    covs = dt_covar,
    h = band,
    rho = .5 # Similar to the rho under MSE-optimal bandwidth
  )
  
  # Save results on a data.table
  data.table(
    band = band,
    coeff = out$coef[1,1],
    low.bound = out$ci[3,1],
    up.bound = out$ci[3,2]
  )
}

# Create a table with the estimates for different bands
test = purrr::map_df(seq(.015, .15, by=0.005), get_bandtest)

# Construct the plot and save it
ggplot(test, aes(x = band, y = coeff)) +
  geom_ribbon(aes(ymin = low.bound, ymax = up.bound), alpha = 0.2) +
  geom_line() + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  ylab("Estimated RD treatment effect") +
  xlab("Bandwidth")

ggsave("tables/rdd-bwci-plot.pdf", width = 15, height = 10, units = "cm")