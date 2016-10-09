#takes in lm object as inout and output RSS, number
residual_sum_squares <- function(lm_obj) {
  return(sum(lm_obj$residuals^2))
}

#Takes in lm obj and outputs TSS, number
#sum( (x - mean(x) )^2 )
total_sum_squares <- function(lm_obj) {
  y = lm_obj$residuals + lm_obj$fitted.values
  y_bar = mean(y)
  obs = lm_obj$residuals + lm_obj$fitted.values
  return(sum((obs - y_bar)^2))
}


#Takes in lm object and returns R^2, number
r_squared <- function(lm_obj) {
  RSS = residual_sum_squares(lm_obj)
  TSS = total_sum_squares(lm_obj)
  return(1 - RSS/TSS)
}

#Takes in lm object and returns F-statistic, number
f_statistic <- function(lm_obj) {
  p = lm_obj$rank - 1#number of predictors
  n = length(lm_obj$residuals)
  RSS = residual_sum_squares(lm_obj)
  TSS = total_sum_squares(lm_obj)
  top = (TSS-RSS)/p
  bottom = RSS/(n-p - 1)
  return(top/bottom)
}


#Takes in RSE and outputs RSE,number
residual_std_error <- function(lm_obj) {
  p = lm_obj$rank#number of predictors
  n = length(lm_obj$residuals)
  RSS = residual_sum_squares(lm_obj)
  return(((1/(n-p-1))*RSS)^.5)
}
