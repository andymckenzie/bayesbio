#' @title Weighted shrinkage estimation. 
#' @description Shrink values towards the mean (in the sample or the overall cohort) to an inverse degree to the confidence you assign to that observation.
#' @param x Numeric vector of values to be shrunken towards the mean.
#' @param n Numeric vector with corresponding entries to x, specifying the number of observations used to calculate x, or some other confidence weight to associate with x.
#' @param m Number specifying weight of the shrinkage estimation, relative to the number of observations in the input vector n. Defaults to the minimum of n, but this is an arbitrary value and should be explored to find an optimal value for your use case.
#' @param meanVal Number specifying the overall mean towards which the values should be shrunken. Defaults to NULL, in which case it is calculated as the (non-weighted) arithmetic mean of the values in the inputted vector x.
#' @return A numeric vector with shrunken data values.
#' @references http://math.stackexchange.com/a/41513
#' @export
weightedShrink <- function(x, n, m = NULL, meanVal = NULL){
  if(length(x) < 3) warning("There must be more than two data points to perform this form of shrinkage estimation; results will be biased.")
  if(is.null(m)) m = min(n)
  if(is.null(meanVal)) meanVal = mean(x)
  shrunk_vals = (n / (n + m)) * x + (m / (n + m)) * meanVal
  shrunk_vals[is.nan(shrunk_vals)] = 0
  return(shrunk_vals)
}
