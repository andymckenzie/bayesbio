#' @title Create a table summarizing covariates segregated by levels of a diagnosis.
#' @description Take a data frame with a diagnosis column and a number of covariate columns and specify the percentage of specified covariate levels in each group and/or the mean +/- sd for quantitative variables for each covariate desired. Although it was designed for generating sample summary tables in the context of bioinformatics experiments and the terminology refers to this, it can be used more generally as well.
#' @param df The data frame containing the columns to be extracted, both diagnosis and covariates.
#' @param dg_col Column specifying the diagnosis column name, which is used to split the table. Levels of this value will be used to generate
#' @param percent_cols Character vector of column names specifying the
#' @param percent_col_cats Character vector specifying the values for which the percentage should be calculated for each percent column.
#' @param group_names Optional character vector specifying the groups within the dg_col, which will be used to order the resulting table.
#' @param row_names Optional character vector specifying what the rownames of the resulting table should be.
#' @return A table summarizing the covariates.
#' @export 
covariatesTable <- function(df, dg_col, percent_cols = NULL, quant_cols = NULL,
  percent_col_cats = NULL, group_names = NULL, row_names = NULL){

  if(!is.null(percent_cols)){
    stopifnot(length(percent_cols) == length(percent_col_cats))
  }

  if(is.null(group_names)){
    group_names = unique(df[ , dg_col])
  }
  n_groups = length(group_names)

  for(i in 1:n_groups){

    tmp_df = df[df[ , dg_col] %in% group_names[i], ]
    tmp_df_n = nrow(tmp_df)
    res = tmp_df_n

    if(!is.null(percent_cols)){
      for(j in 1:length(percent_cols)){
        tmp_percent_vals = tmp_df[tmp_df[ , percent_cols[j]] %in% percent_col_cats[j], ]
        tmp_percent_proportion = nrow(tmp_percent_vals) / tmp_df_n
        res = c(res, round(tmp_percent_cols, 3) * 100)
      }
    }

    if(!is.null(quant_cols)){
      for(j in 1:length(quant_cols)){
        tmp_quant_vals = tmp_df[ , quant_cols[j], ]
        tmp_quant_cols_mean = round(mean(as.numeric(tmp_quant_vals)), 1)
        tmp_quant_cols_sd = round(sd(as.numeric(tmp_quant_vals)), 1)
        tmp_quant_cols_res = paste(tmp_quant_cols_mean, tmp_quant_cols_sd, sep = " +/- ")
        res = c(res, tmp_quant_cols_res)
      }
    }

    if(i == 1){
      res_df = res
    } else {
      res_df = cbind(res_df, res)
    }
  }

  if(is.null(row_names)){
    rownames(res_df) = c("Number", percent_cols, quant_cols)
  } else {
    rownames(res_df) = row_names
  }
  colnames(res_df) = group_names
  return(res_df)

}
