% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createSampleTable.R
\name{covariatesTable}
\alias{covariatesTable}
\title{Create a table summarizing covariates segregated by levels of a diagnosis.}
\usage{
covariatesTable(df, dg_col, percent_cols = NULL, quant_cols = NULL,
  percent_col_cats = NULL, group_names = NULL, row_names = NULL)
}
\arguments{
\item{df}{The data frame containing the columns to be extracted, both diagnosis and covariates.}

\item{dg_col}{Column specifying the diagnosis column name, which is used to split the table. Levels of this value will be used to generate}

\item{percent_cols}{Character vector of column names specifying the}

\item{percent_col_cats}{Character vector specifying the values for which the percentage should be calculated for each percent column.}

\item{group_names}{Optional character vector specifying the groups within the dg_col, which will be used to order the resulting table.}

\item{row_names}{Optional character vector specifying what the rownames of the resulting table should be.}
}
\value{
A table summarizing the covariates.
}
\description{
Take a data frame with a diagnosis column and a number of covariate columns and specify the percentage of specified covariate levels in each group and/or the mean +/- sd for quantitative variables for each covariate desired. Although it was designed for generating sample summary tables in the context of bioinformatics experiments and the terminology refers to this, it can be used more generally as well.
}

