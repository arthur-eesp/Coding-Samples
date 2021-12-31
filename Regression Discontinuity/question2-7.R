### Question 2, item 7 ###

# Clean environment
rm(list = ls())

# Load libraries
library(data.table)
library(rdlocrand)
library(rlist) # To manage lists
library(kableExtra)
library(ggplot2)

# Load the data
dt_rdd <- readRDS("data/dt_rdd.RDS")
covar_names <- readRDS("data/covar_names.RDS")
dt_covar <- readRDS("data/dt_covar.RDS")
W0 <- readRDS("data/W0.RDS")


#== Placebo outcomes test ==#

# Delete college_dum covariate since it's constant
covar_names <- setdiff(covar_names, 'college_dum')

# Run the RD estimation for each covariate and record on a list
covar_ci <- list()
for (i in 1:(length(covar_names))){
  tmp <- rdrandinf(
    dt_rdd[, get(covar_names[i])],
    dt_rdd$lagdemvoteshare,
    covariates = dt_covar[, -(..i)],
    cutoff = .5,
    wl = W0[[1]],
    wr = W0[[2]],
    ci = .05
  )
  # Add the confidence interval to the list
  covar_ci <- list.append(covar_ci, tmp$ci[1,])
}

# Transform CI list into 2 matrices
ci_matrix1 <- covar_ci[[1]]
for (i in 2:(length(covar_ci) %/% 2)){
  ci_matrix1 <- cbind(ci_matrix1, covar_ci[[i]])
}
j <- (length(covar_ci) %/% 2) + 1

ci_matrix2 <- covar_ci[[j]]
for (i in (j+1):length(covar_ci)){
  ci_matrix2 <- cbind(ci_matrix2, covar_ci[[i]])
}

# Change column names
colnames(ci_matrix1) <- covar_names[1:(j-1)]
colnames(ci_matrix2) <- covar_names[-(1:(j-1))]

# Save as LaTeX tables
kbl(ci_matrix1,
    format = "latex",
    booktabs = TRUE,
    digits = 2,
    longtable = TRUE,
    escape = TRUE,
    caption = '95\\% confidence intervals for fasification tests.',
    label = 'ci-tbl-lr-1'
) |> 
  kable_classic(full_width = FALSE) |>
  save_kable('tables/ci-tbl-lr-1.tex')

kbl(ci_matrix2,
    format = "latex",
    booktabs = TRUE,
    digits = 2,
    longtable = TRUE,
    escape = TRUE,
    caption = '95\\% confidence intervals for fasification tests.',
    label = 'ci-tbl-lr-2'
) |> 
  kable_classic(full_width = FALSE) |>
  save_kable('tables/ci-tbl-lr-2.tex')


# NOTE: I'll sometimes recenter the running variable at 0 rather than set
# the cutoff at 0.5 because some functions are not very smart and assume
# the cutoff is 0 when applying rdrandinf internally...

#== Window sensitivity test ==#

# Run sensitivity test for window
tmp <- rdsensitivity(
  dt_rdd$demvoteshare,
  (dt_rdd$lagdemvoteshare - .5), # recenter cutoff
  cutoff = 0,
  wlist=seq(.01,0.1,by=.005)
)

# Manually reconstruct the plot and save it
xaxis <- tmp$wlist
yaxis <- tmp$tlist
zvalues <- tmp$results

png(filename="tables/rdd-window-plot.png", width = 15, height = 10, units = "cm", res = 192)
filled.contour(xaxis,yaxis,t(zvalues),
               xlab='Window around cutoff',
               ylab='Null treatment effect',
               key.title=title(main = 'p-value',cex.main=.8),
               levels=seq(0,1,by=.01),col=gray.colors(100,1,0))
dev.off()


#== Alternative window sensitivity test ==#

# Define function that does RD estimation for a given band
get_wintest = function(win){
  # Run RD estimation
  out <- rdrandinf(
    dt_rdd$demvoteshare,
    dt_rdd$lagdemvoteshare,
    cutoff = .5,
    covariates = dt_covar,
    wl = .5 - win,
    wr = .5 + win,
    ci = .05
  )
  
  # Save results on a data.table
  data.table(
    window = win,
    coeff = out$obs.stat[1],
    low.bound = out$ci[1],
    up.bound = out$ci[2]
  )
}

# Create a table with the estimates for different bands
test = purrr::map_df(seq(.01,0.1,by=.005), get_wintest)

# Construct the plot and save it
ggplot(test, aes(x = window, y = coeff)) +
  geom_ribbon(aes(ymin = low.bound, ymax = up.bound), alpha = 0.2) +
  geom_line() + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  ylab("Estimated RD treatment effect") +
  xlab("Window")

ggsave("tables/rdd-winci-plot.pdf", width = 15, height = 10, units = "cm")


#== Placebo cutoffs test ==#

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
  out <- rdrandinf(
    dt_rdd$demvoteshare,
    dt_rdd$lagdemvoteshare,
    cutoff = cutoff,
    covariates = dt_covar,
    wl = cutoff - (0.5 - W0[[1]]), # Use same window around cutoff
    wr = cutoff + (W0[[2]] - 0.5),
    ci = .05
  )
  
  # Save results on a data.table
  data.table(
    cutoff = cutoff,
    coeff = out$obs.stat[1],
    low.bound = out$ci[1],
    up.bound = out$ci[2]
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

ggsave("tables/rdd-cutoff-lr-plot.pdf", width = 15, height = 10, units = "cm")



#== Sensitivity to randomization mechanism test ==#

# Run the rdrbounds function
rd_locrand_bounds <- rdrbounds(
  Y = dt_rdd$demvoteshare,
  R = (dt_rdd$lagdemvoteshare - .5), # recenter the cutoff
  wlist = c(.01, .02, .03, .04, .05),
  expgamma = c(1.5,2,2.5,3),
  reps = 1000
)

# Save this object just in case (it takes a while to run)
saveRDS(rd_locrand_bounds, "data/rd_locrand_bounds.RDS")

pvals_upper <- rd_locrand_bounds$upper.bound
row.names(pvals_upper) <- c('$\\Gamma = 1.5$', '$\\Gamma = 2$', 
                            '$\\Gamma = 2.5$', '$\\Gamma = 3$')
colnames(pvals_upper) <- c('$w = 0.01$', '$w = 0.02$', '$w = 0.03$', 
                           '$w = 0.04$', '$w = 0.05$')

kbl(pvals_upper,
    format = "latex",
    booktabs = TRUE,
    digits = 3,
    escape = FALSE,
    caption = 'Upper bounds for $p$-values for varying windows around the 
              cutoff and different values of $\\Gamma$',
    label = 'rd-locrand-bounds'
) |> 
  kable_classic(full_width = FALSE) |>
  save_kable('tables/rd-locrand-bounds.tex')
