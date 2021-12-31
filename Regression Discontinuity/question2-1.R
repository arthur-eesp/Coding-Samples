### Question 2, item 1 ###

# Clean environment
rm(list = ls())

# Load libraries
library(data.table)
library(rdrobust)
library(ggplot2)

# Read the data
dt_rdd <- fread("data/usaelections1974.csv")

# Remove the one observation with NA at lagdemvoteshare
dt_rdd <- dt_rdd[!is.na(lagdemvoteshare)]

# Transform some character variables into dummies
dt_rdd[, sex := ifelse(sex == '1:  Male', 1, 0)]
dt_rdd[, college_dum := ifelse(collegeattend == '0:  No coll attended', 0, 1)]
dt_rdd[, enlisted_dum := ifelse(militaryservice == '1:  Enlisted, NonCom', 1, 0)]
dt_rdd[, officer_dum := ifelse(militaryservice == '2:  Officer', 1, 0)]

# Select covariates
covar_names <- c('totpop', 'medianincome', 'pctblack', 'mnfcng', 'pcthighschl',
                 'sex', 'college_dum', 'enlisted_dum', 'officer_dum', 'age')
dt_covar <- dt_rdd[, ..covar_names]

# Save these covariates for later
saveRDS(dt_rdd, "data/dt_rdd.RDS")
saveRDS(covar_names, "data/covar_names.RDS")
saveRDS(dt_covar, "data/dt_covar.RDS")

# Perform the RD estimation and save print results
rd_cont_est <- rdrobust(
  dt_rdd$demvoteshare,
  dt_rdd$lagdemvoteshare,
  c = .5,
  covs = dt_covar,
  bwselect = 'mserd'
)
# WARNING: the output is different depending on whether you run the entire
# script using `source(..., echo = TRUE)` or `source(..., echo = FALSE)`.
# The one I used is with echo = FALSE, which is the same you'd obtain if
# if you run this code chunk separately
sink("tables/rdd-cont.txt", type=c("output"))
summary(rd_cont_est)
sink(NULL)

# To plot the results, first save the bandwidth
bw <- rd_cont_est$bws[1,1]

# Then use it to call the rdplot function
rd_cont_plot <- rdplot(
  dt_rdd$demvoteshare[abs(dt_rdd$lagdemvoteshare - 0.5) <= bw],
  dt_rdd$lagdemvoteshare[abs(dt_rdd$lagdemvoteshare - 0.5) <= bw],
  c = .5,
  p = 1,
  kernel = 'triangular',
  x.label = 'Democratic party vote share in previous period',
  y.label = 'Democratic party vote share in current period',
  title = ''
)

# Save the plot
ggsave("tables/rdd-cont-plot.pdf", width = 15, height = 10, units = "cm",
       plot = rd_cont_plot$rdplot)