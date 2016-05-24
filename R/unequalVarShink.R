#' @title Likelihood function of the James-Stein shrinkage factor.
#' @description To be used in MLE computation of the James-Stein shrinkage factor.
#' @param stat Input statistics to be shrinkage estimated.
#' @param vars Corresponding variances of equal length.
#' @param a_hat Shrinkage intensity to be estimated.
#' @return The likelihood of the function given the parameters.
#' @references http://projecteuclid.org/euclid.ss/1331729986
#' @export
a_hat_mle <- function(stat, vars, a_hat){
  likelihood = 1
  for(i in 1:length(stat)){
    likelihood = likelihood - (((stat[i]^2)/vars[i]) * (vars[i]/(vars[i] + a_hat))) +
      log(vars[i]/(vars[i] + a_hat))/2
  }
  return(likelihood)
}

#' @title Perform James-Stein shrinkage estimation using unequal variances
#' @description Traditional JS shrinkage estimation assumes equal variances for each of the data points, while this algorithm extends JS shrinkage estimation to entries with different variances.
#' @param stat Input statistics to be shrinkage estimated.
#' @param vars Corresponding variances of equal length.
#' @param verbose Whether information about the algorithm should be reported.
#' @return A data frame containing the shrinkage estimated statistics.
#' @references http://projecteuclid.org/euclid.ss/1331729986
#' @export
unequalVarShrink <- function(stat, vars, verbose = TRUE){

  n = length(stat)
  if(!n == length(vars)) stop("stat and vars must be the same length.")

  mle_result = optimize(function(a_hat) {a_hat_mle(stat, vars, a_hat)},
    interval = c(0, 1e6), maximum = TRUE)

  if(verbose) {
    message(paste0("MLE max at A hat of ", round(mle_result$maximum, 4)))
    message(paste0("The MLE objective function has a value of ", round(mle_result$objective, 4), " at this value."))
  }

  B_hat_mle = numeric(length = n)
  for(i in 1:n){
    B_hat_mle[i] = vars[i]/(vars[i] + mle_result$maximum)
  }

  mu_hat_mle = numeric(length = n)
  for(i in 1:n){
    mu_hat_mle[i] = (1 - B_hat_mle[i]) * stat[i]
  }

  res_df = data.frame(stat = stat, variance = vars, b_hat = B_hat_mle, shrink_stat = mu_hat_mle)

  return(res_df)

}
