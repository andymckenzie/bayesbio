#' @title cbind while converting missing entries to NA.
#' @description cbind usually malfunctions on vector of unequal lengths; this function allows vectors of unequal length to be combined, while filling the missing entries with NAs.
#' @param ... A set of vectors separated by commas.
#' @return A matrix that combines the inputted vectors.
#' @references http://r.789695.n4.nabble.com/How-to-join-matrices-of-different-row-length-from-a-list-td3177212.html; http://stackoverflow.com/a/7962286/560791
cbindFill <- function(...){
  cargs = list(...)
  cargs = lapply(cargs, as.matrix)
  max_nrow = max(sapply(cargs, nrow))
  do.call(cbind, lapply(cargs, function (x)
    rbind(x, matrix(, max_nrow - nrow(x), ncol(x)))))
}
