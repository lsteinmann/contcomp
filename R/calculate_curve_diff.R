#' Calculate the difference of two curves
#'
#'
#' @param model_df a dataframe with vessels / objects / cases as rows and observations as columns, ideally a combination of the shinoto_df$modelcontour-values as returned by the find_shinoto_index() function or possibly a transposed version of shapaardata_df as returned by load_shapaar_contours() / clean_contour_df()
#' @param type either "sd" or "sum", defines if the sum of all y-differences between the curves should be used ("sum"), or the SD of the series of difference values ("sd"). I am still trying to figure out what is more useful, so far both yield workable results.
#'
#' @return A single value expressing the difference of two contours. (The higher the less alike the curves are.)
#'
#' @export calculate_curve_diff
#' @import data.table

calculate_curve_diff <- function(model_df, item1, item2, type = "sd"){
  diff <- NULL
  diff <- abs(model_df[item1,] - model_df[item2,])
  diff <- data.table::transpose(diff)
  if (type == "sd") {
    diff_value <- sd(diff$V1)
  }
  if (type == "sum") {
    diff_value <- sum(diff$V1)
  }
  if (type != "sum" & type != "sd") {
    stop('Invalid type. Choose "sum" or "sd".' )
  }
  return(diff_value)
}
