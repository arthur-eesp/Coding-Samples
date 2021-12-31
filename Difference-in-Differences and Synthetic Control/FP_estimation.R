# This function implements the inference approach of Ferman and Pinto (2019, ReStat)
# "Inference in Differences-in-Differences with Few Treated Groups and Heteroskedasticity"
# Here we follow the steps in appendix A.4, for a model with sampling weights
#
# Arguments are
# outcome = character vector for the variable
# treatdum = dummy variable indicating which group will (eventually) be treated
# postdum = dummy variable indicating which periods are post treatment (1{t > t*})
# timevar = character vector for the time variable. Should be orderable
# groupvar = character vector for the group variable
# (group-averages will be taken before regression model is specified)
# data = data.frame or data.table
# beta_null = the value being tested under the null
# controls = vector of group-level controls to be included. If c() (default), no controls are included.
# weightvar = character vector for the weight variable.
# signif = significance of the test, default is 5% (and 1-signif is size of confidence interval)
# return_lean = Boolean indicating whether returned DiD model is lean fixest obect. Default FALSE.
# Breps = number of bootstrap replications, default is 1000
FP_estimation <- function(outcome, treatdum, postdum, timevar, groupvar, data,
                          beta_null = 0, controls = c(), weightvar, signif = 0.05,
                          return_lean = FALSE, Breps = 1000){
  
  dt <- data.table(data)
  
  dt <- dt[get(weightvar) != 0]
  
  ## Step 1: calculate the DiD estimate
  if(length(controls)>0){
    fml = as.formula(paste0(outcome, '~', treatdum, ':', postdum, '+',
                            paste0(controls, collapse = '+'), '|',
                            groupvar, '+', timevar))
  } else{
    fml = as.formula(paste0(outcome, '~',  treatdum, ':', postdum, '|',
                            groupvar, '+', timevar))
  }
    
  # Cluster at the group level for the sake of comparison (but one should expect
  # very biased results if there are very few control or treated groups)
  DiD_model <- feols(fml, 
                     data = dt,
                     weights = as.formula(paste0('~', weightvar)),
                     
                     cluster = as.formula(paste0('~', groupvar)),
                     lean = return_lean)
  
  # Save coefficient
  alpha_hat <- coef(DiD_model)[[paste0(treatdum, ':', postdum)]]
  
  ## Step 2: estimate the DiD model with the null imposed to get residuals
  dt[, outcome_null := get(outcome) - beta_null * get(treatdum) * get(postdum)]
  
  if(length(controls)>0){
    fml = as.formula(paste0('outcome_null ~',
                            paste0(controls, collapse = '+'), '|',
                            groupvar, '+', timevar))
  } else{
    fml = as.formula(paste0('outcome_null ~ 1 |',
                            groupvar, '+', timevar))
  }
  
  model <- feols(fml,
                 data = dt,
                 weights = as.formula(paste0('~', weightvar)))
  
  # Add vector of residuals to data
  dt[, residual := resid(model)]
  
  # Calculate group x time weights by summing individual weights 
  # (this is the P_{jt} variable in appendix A.4)
  dt[, P := sum(get(weightvar)), by = c(timevar, groupvar)]
  
  # Also get sum of squared weights by group x time (this is used to 
  # calculate the variance -- it is the term \sum_i \omega_{ijt}^2)
  dt[, omega2 := sum(get(weightvar)^2), by = c(timevar, groupvar)]
  
  # Aggregate data at the group x time level, weighting by weightvar
  dt <- dt[, lapply(.SD, weighted.mean, w = get(weightvar)), 
           by = c(timevar, groupvar),
           .SDcols = c(treatdum, postdum, 'residual', 'P', 'omega2')]
  
  # Now to calculate the W_j's using equation (34) in appendix A.4
  # First calculate the denominators (one for pre- and other for post-treatment)
  dt[get(treatdum) == 1 & get(postdum) == 1, P_post := sum(P)]
  dt[get(treatdum) == 1 & get(postdum) == 0, P_pre := sum(P)]
  
  # Calculate the fractions P_{1t} / (\sum_t P_{it'}) for pre- and post-treatment
  dt[get(treatdum) == 1 & get(postdum) == 1, tmp := P / P_post]
  dt[get(treatdum) == 1 & get(postdum) == 0, tmp := P / P_pre]
  
  # Assign those values for every group for each period
  # (note that the fraction doesn't depend on group)
  dt[, tmp := mean(tmp, na.rm = TRUE), by = get(timevar)]
  
  # Multiply the fraction by the residual for each (group, period) pair post-period, 
  # or minus the residual if pre-treatment period
  dt[, W := ifelse(get(postdum) == 1, tmp * residual, - tmp * residual)]
  
  # Calculate the components of q_j in equation (35) of appendix A.4, i.e., the
  # terms (P_{1t} / \sum_t P_{it'})^2 * (\sum_i \omega_{ijt}^2 / P_{jt}^2) for
  # each time period
  dt[, q := tmp^2 * omega2 / P^2]
  
  # Aggregate on the group level by summing
  dt <- dt[, c(lapply(.SD, sum), treatdum = mean(get(treatdum))), 
           by = c(groupvar), .SDcols=c('W', 'q', 'P')]
  
  
  ## Step 3: estimate the variance of the residuals by regressing on A + B q_j
  model <- feols(W^2 ~ q, data = dt, weight = ~P)
  
  # Add the predicted variance and the normalized residuals to the data.table
  dt[, var_W := ifelse(predict(model) > 0, predict(model), 0.0001)]
  dt[, W_norm := W / sqrt(var_W)]
  
  
  ## Step 4: generate bootstrap sample of residuals
  
  # Get total number of groups
  N = length(dt[, get(groupvar)])
  
  # Create matrix to store bootstrap replications
  boot_sample <- matrix(nrow = Breps, ncol = 1)
  
  # Set seed, for reproducibility
  set.seed(0)
  
  for (i in seq_len(Breps)){
    # Sample with replacement the normalized residuals
    dt[, W_resamp := sample(W_norm, N, replace = TRUE)]
    
    # Generate sequence of 1 and -1 to multiply residuals (iid Rademacher)
    dt[, rademach := sample(c(1,-1), N, replace = TRUE)]
    
    # Obtain the \tilde W_{j,b} sequence
    dt[, tilde_W := rademach * W_resamp * sqrt(var_W)]
    
    # Calculate the bootstrap estimate (under the null)
    boot_sample[i,] <- mean(dt[treatdum == 1, tilde_W]) - mean(dt[treatdum == 0, tilde_W])
  }
  
  
  ## Step 5 (kind of): compare alpha_hat to bootstrap distribution
  # (Here I'll use percentile p-values and confidence intervals on a 2-sided test)
  pvalue <- 2 * min(mean(alpha_hat < boot_sample[,1]), mean(alpha_hat > boot_sample[,1]))
  
  ci.low <- alpha_hat - quantile(boot_sample[,1], 1 - signif/2, names = FALSE)
  ci.high <- alpha_hat - quantile(boot_sample[,1], signif/2, names = FALSE)
  
  
  return(list(model = DiD_model,
              coefficient = alpha_hat,
              se = dt[treatdum==1, sqrt(var_W)],
              pvalue = pvalue,
              ci.low = ci.low,
              ci.high = ci.high))
}
