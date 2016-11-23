#' @title Replace the upper or lower triangle of a square matrix with the other to make it symmetric.
#' @description The replaced values will be lost following the operation of this function. Only works on square matrices. 
#' @param mat The matrix to be made symmetric.
#' @param replaceUpper Whether the upper triangle of the matrix should be replaced by the lower triangle. Default = TRUE; if FALSE, the lower triangle of the matrix is replaced by the upper triangle.
#' @return A matrix that has been made symmetric.
#' @export
makeMatSym <- function(mat, replaceUpper = TRUE){
  if(replaceUpper){
    mat[lower.tri(mat)] = t(mat)[lower.tri(t(mat))]
  } else {
    mat[upper.tri(mat)] = t(mat)[upper.tri(t(mat))]
  }
  return(mat)
}
