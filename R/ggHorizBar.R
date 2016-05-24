#' @title Create a color-labeled horizontal bar plot in ggplot2.
#' @description This function takes a data frame and creates a horizontal (by default) bar plot from it while ordering the values.
#' @param data_df Data frame with columns to specify the data values, the row names, and the fill colors of each of the bars.
#' @param dataCol The column name that specifies the values to be plotted.
#' @param namesCol The column name that specifies the corresponding names for each of the bar plots to be plotted.
#' @param labelsCol The column name that specifies the groups of the labels.
#' @param decreasing Logical specifying whether the values in dataCol should be in decreasing order.
#' @return A ggplot2 object, which can be plotted via the plot() function or saved via the ggsave() function.
#' @export
ggHorizBar <- function(data_df, dataCol, namesCol, labelsCol, decreasing = TRUE){
  if (!requireNamespace("ggplot2", quietly=TRUE)) stop("Please install package 'ggplot2' to use this function.")
  data_df = data_df[order(data_df[ , dataCol], decreasing = decreasing), ]
  data_df[ , namesCol] = factor(data_df[ , namesCol], levels = data_df[ , namesCol])
  data_df[ , labelsCol] = factor(data_df[ , labelsCol], levels = data_df[ , labelsCol])
  palette = colorRampPalette(c("black", "#56B4E9", "#0072B2"))(n = length(unique(data_df[ , labelsCol])))
  horiz_bar = ggplot2::ggplot(data_df, ggplot2::aes(x = namesCol, y = dataCol, fill = labelsCol)) +
    ggplot2::geom_bar(stat = 'identity')  + ggplot2::ylab("") + ggplot2::xlab("") +
    ggplot2::coord_flip() + ggplot2::theme_bw() +
    ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank()) +
    ggplot2::scale_fill_manual(values = palette, guide = ggplot2::guide_legend(title = "", reverse = TRUE))
  return(horiz_bar)
}
