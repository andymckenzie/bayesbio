#' @title Find the standard error of the sampling distribution of a statistic.
#' @description Finds the standard error of a numeric vector; by default, removes NAs prior to calculation.
#' @param x The numeric vector whose standard error should be calculated.
#' @param na.rm Logical; TRUE indicates that NAs should be removed from the vector prior to calculating the standard error, and vice versa for FALSE.
#' @return A one-element numeric vector giving the standard error.
#' @export
std_error <- function(x, na.rm = TRUE){

  if(any(is.na(x)) & na.rm == TRUE){
    x = x[!is.na(x)]
  }
  std_error = sd(x)/sqrt(length(x))
  return(std_error)
  
}
