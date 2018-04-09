#' (Custom) Distance Matrix for contour curves
#'
#' Converts a dataframe with vessels as rows and columns as observations into a custom
#' distance matrix (which is not a proper objects of class dist, I would be glad if someone
#' where to fix this) using the distance measure as provided by  shapes::procdist()
#' and returns the resulting matrix.
#'
#' @param model_df a dataframe with vessels / objects / cases as rows and observations as columns, ideally a combination of the shinoto_df$modelcontour-values as returned by the find_shinoto_index() function or possibly a transposed version of shapaardata_df as returned by load_shapaar_contours() / clean_contour_df()
#'
#' @return a (distance) matrix (**not** of class dist) of model_df
#'
#' @export get_procrustes_dist
#' @import shapes
#' @import data.table

get_procrustes_dist <- function(model_df) {
  procrustes_distance <- matrix(0, nrow = nrow(model_df), ncol = nrow(model_df))
  for (row in 1:nrow(model_df)) {
    item1 <- data.frame(x = 1:1000,
                        y = data.table::transpose(model_df[row,]))
    for (compare_row in 1:nrow(model_df)) {
      item2 <- data.frame(x = 1:1000,
                          y = data.table::transpose(model_df[compare_row,]))
      diff <- shapes::procdist(item1, item2, type = "full")
      procrustes_distance[row,compare_row] <- diff
    }
  }
  dimnames(procrustes_distance) <- list(c(rownames(model_df)), c(rownames(model_df)))
  #procrustes_distance <- convert_to_dist(procrustes_distance)
  return(procrustes_distance)
}
