### Question 2, item 2 ###

# Clean environment
rm(list = ls())

# Load libraries
library(data.table)
library(rdrobust)
library(rddensity)
library(rlist) # To manage lists
library(kableExtra)
library(ggplot2)

# Load the data
dt_rdd <- readRDS("data/dt_rdd.RDS")
covar_names <- readRDS("data/covar_names.RDS")
dt_covar <- readRDS("data/dt_covar.RDS")

# Run the RD estimation for each covariate and record on a list
covar_ci <- list()
for (i in 1:(length(covar_names))){
  tmp <- rdrobust(
    dt_rdd[, get(covar_names[i])],
    dt_rdd$lagdemvoteshare,
    c = .5,
    covs = dt_covar[, -(..i)],
    bwselect = 'mserd'
  )
  # Add the confidence interval to the list
  covar_ci <- list.append(covar_ci, tmp$ci['Robust', ])
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
    label = 'ci-tbl-1'
) |> 
  kable_classic(full_width = FALSE) |>
  save_kable('tables/ci-tbl-1.tex')

kbl(ci_matrix2,
    format = "latex",
    booktabs = TRUE,
    digits = 2,
    longtable = TRUE,
    escape = TRUE,
    caption = '95\\% confidence intervals for fasification tests.',
    label = 'ci-tbl-2'
) |> 
  kable_classic(full_width = FALSE) |>
  save_kable('tables/ci-tbl-2.tex')

# Run the density test and plot the density
rd_dens <- rddensity(dt_rdd$lagdemvoteshare, c = .5)
rd_dens_plot <- rdplotdensity(
  rd_dens,
  dt_rdd$lagdemvoteshare,
  plotRange = c(.2,.8),
  xlabel = 'Democratic party vote share in previous period',
  ylabel = 'Density'
)

# Save the p-value and the plot
cat(round(rd_dens$test[['p_jk']], 4), file = 'tables/dens-p-val.tex')
ggsave("tables/rdd-dens-plot.pdf", width = 15, height = 10, units = "cm",
       plot = rd_dens_plot$Estplot)
