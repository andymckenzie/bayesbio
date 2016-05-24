#' @title Perform PubMed queries on 2x2 combinations of term vectors.
#' @description Perform PubMed queries on the intersections of two character vectors. This function is a wrapper to RISmed::EUtilsSummary with type = 'esearch', db = 'pubmed'.
#' @param rowTerms Character vector of terms that should make up the rows of the resulting mention count data frame.
#' @param colTerms Character vector of terms for the columns.
#' @param sleepTime How much time (in seconds) to sleep between successive PubMed queries. If you set this too low, PubMed may shut down your connection to prevent overloading their servers.
#' @return A data frame of the number of mentions for each combination of terms.
#' @export
pubmedQuery <- function(rowTerms, colTerms, sleepTime = 0.01){
  if (!requireNamespace("RISmed", quietly=TRUE)) stop("Please install package 'RISmed' to use this function.")

  disease_gene_mentions = data.frame(matrix(0, nrow = length(rowTerms),
  	 ncol = length(colTerms) + 1))

  for(i in 1:length(colTerms)){
  	for(j in 1:length(rowTerms)){
  		res = RISmed::EUtilsSummary(paste(colTerms[i], "AND", rowTerms[j], sep = " ")
  			, type = 'esearch', db = 'pubmed')
  		disease_gene_mentions[j, i] = RISmed::QueryCount(res)
  		Sys.sleep(sleepTime)
  	}
  }

  total_res = numeric(length(rowTerms))
  for(j in 1:length(rowTerms)){
    res = RISmed::EUtilsSummary(rowTerms[j], type = 'esearch', db = 'pubmed')
    total_res[j] = RISmed::QueryCount(res)
    Sys.sleep(sleepTime)
  }

  rownames(disease_gene_mentions) = rowTerms
  disease_gene_mentions[, length(colTerms) + 1] = total_res
  colnames(disease_gene_mentions) = c(colTerms, "Total_Mentions")
  disease_gene_mentions = disease_gene_mentions[order(disease_gene_mentions[ , length(colTerms) + 1]), ]

  return(disease_gene_mentions)

}
