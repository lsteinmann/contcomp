#' (Custom) Distance Matrix for contour curves
#'
#' Converts a dataframe with vessels as rows and columns as observations into a custom
#' distance matrix (which is not a proper objects of class dist, I would be glad if someone
#' where to fix this) using the distance measure as provided by calculate_curve_diff() from
#' this package.
#'
#' @param model_df a dataframe with vessels / objects / cases as rows and observations as columns, ideally a combination of the shinoto_df$modelcontour-values as returned by the find_shinoto_index() function or possibly a transposed version of shapaardata_df as returned by load_shapaar_contours() / clean_contour_df()
#' @param type either "sd" or "sum", passed on to calculate_curve_diff() (see there)
#'
#' @return a (distance) matrix (**not** of class dist) of model_df
#'
#' @export get_curve_dist

get_curve_dist <- function(model_df, type = "sd") {
  manual_distance <- matrix(0, nrow = nrow(model_df), ncol = nrow(model_df))
  for (row in 1:nrow(model_df)) {
    item1 <- rownames(model_df[row,])
    for (compare_row in 1:nrow(model_df)) {
      diff <- calculate_curve_diff(model_df,
                                   item1,
                                   rownames(model_df[compare_row,]),
                                   type = type)
      manual_distance[row,compare_row] <- diff
    }
  }
  dimnames(manual_distance) <- list(c(rownames(model_df)), c(rownames(model_df)))
  #manual_distance <- convert_to_dist(manual_distance)
  return(manual_distance)
}
