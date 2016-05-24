#' @title Creates random, unique character strings.
#' @description Makes them unique by randomly choosing the character strings; and, in case it is necessary, adding numbers to the end using make.unique.
#' @param number Specifies the number of character strings that should be created.
#' @param length Specifies the length of each character string in letters.
#' @param upper Binary parameter specifying whether the character strings should be uppercase. Default = FALSE, so the character strings are all lowercase.
#' @references http://stackoverflow.com/a/1439541/560791
#' @export
createStrings <- function(number, length, upper = FALSE){
	strings = make.unique(replicate(number,
		paste(sample(letters, length, replace = TRUE),
		collapse = "")))
	if(upper) toupper(strings)
	return(strings)
}
