#' @title Multiple pattern gsub. 
#' @description An extension to gsub that handles vectors of patterns and replacements, avoiding recursion problems associated with overlap at the extense of computation time.
#' @param pattern Character vector of patterns to match.
#' @param replacement Character vector of replacements for each pattern.
#' @param x Character vector in which the gsub should be performed.
#' @param ... Additional arguments to grep.
#' @references http://stackoverflow.com/a/15254254/560791
mgsub <- function(pattern, replacement, x, ...) {
  n = length(pattern)
  if (n != length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result = x
  for (i in 1:n) {
    result[grep(pattern[i], x, ...)] = replacement[i]
  }
  return(result)
}
