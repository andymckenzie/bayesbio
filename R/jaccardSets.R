#' @title Jaccard index of two character vectors.
#' @description This function compares the elements in two character vectors to find the Jaccard index, i.e. the number of intersections divided by the total number of elements in both sets.
#' @param set1 Character vector.
#' @param set2 Character vector.
#' @return A number (one-element numeric vector) specifying the Jaccard index from comparing the two sets.
#' @references https://en.wikipedia.org/wiki/Jaccard_index
#' @export 
jaccardSets <- function(set1, set2){
	in_length = length(intersect(set1, set2))
	un_length = length(union(set1, set2))
	jaccard = in_length/un_length
	return(jaccard)
}
