### Question 2, items 5 and 6 ###

# Clean environment
rm(list = ls())

# Load libraries
library(data.table)
library(rdlocrand)

# Load the data
dt_rdd <- readRDS("data/dt_rdd.RDS")
dt_covar <- readRDS("data/dt_covar.RDS")

# Choose window (I'll keep the default seed = 666)
W0 <- rdwinselect(dt_rdd$lagdemvoteshare, dt_covar, cutoff = .5, level = .15,
                  wobs = 1, obsmin = 1)

# Save window for future use and also for including in LaTeX document
saveRDS(W0, "data/W0.RDS")
cat('[', round(W0[[1]], 4), '; ', round(W0[[2]], 4), ']', sep = '',
    file = 'tables/W0.tex')

# Perform the RD estimation using the selected window and save output
# WARNING: the output is different depending on whether you run the entire
# script using `source(..., echo = TRUE)` or `source(..., echo = FALSE)`.
# The one I used is with echo = FALSE, which is the same you'd obtain if
# if you run this code chunk separately
sink("tables/rdd-lr.txt", type=c("output"))
rd_locrand_est <- rdrandinf(
  dt_rdd$demvoteshare,
  dt_rdd$lagdemvoteshare,
  cutoff = .5,
  covariates = dt_covar,
  wl = W0[[1]],
  wr = W0[[2]],
  ci = .05,
  statistic = 'all'
)
sink()