#' @title Identify all duplicates values in a vector.
#' @description By default the base R function duplicated only identifies the duplicated values after the first in a vector as TRUE. This function identifies all of the duplicates as true.
#' @param x The input vector.
#' @return A logical vector.
#' @export
allDups <- function(x){
  dups = duplicated(x) | duplicated(x, fromLast = TRUE)
  return(dups)
}
