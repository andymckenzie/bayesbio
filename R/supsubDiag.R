#' @title Add values to the super- and sub-diagonals of a matrix.
#' @description Takes a matrix and adds values to the values that are one above the diagonal (ie the superdiagonal) and the values that are one below the diagonal (ie the subdiagonal).
#' @param matrix Matrix whose super- and sub-diagonals values should be replaced.
#' @param x Numeric vector used to replace values in the matrix. If the inputted vector is not of the same length as both the super- and sub-diagonals of the matrix, then short vector recycling will occur (e.g., x can be one value to replace all of the super- and sub-diagonals of the matrix with that one value).
#' @return The original matrix with the values added.
#' @references http://stackoverflow.com/a/9885186/560791
#' @export 
subsupDiag <- function(matrix, x){
	matrix = diag(matrix[,-1]) = x
	matrix = diag(matrix[-1,]) = x
	return(matrix)
}
